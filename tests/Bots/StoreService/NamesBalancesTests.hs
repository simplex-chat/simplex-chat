{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Bots.StoreService.NamesBalancesTests (namesBalancesTests, cardPaid, credit, creditLate, figures, invariantHolds, lateCredit, payment, placeOrderRow, source) where

import Bots.StoreService.FakeEthNode (plainTerms)
import Bots.StoreService.FakeStripe (FakeStripe (..), stripeObjectEvent, stripeSigHeader, withFakeStripe)
import Bots.StoreService.NamesOrdersTests (Ctx (..), withOrders, withOrdersVia)
import Bots.StoreService.NamesStoreTests (at, btc, hashFixture, newReg, ownerA, paidOrder, reg, t0, write)
import Bots.StoreService.WebTests (agreed, lengthOf, rcv, statusOf, webRequestBody, withServiceStore)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_)
import qualified Data.Aeson as J
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (StripeConfig (..))
import StoreService.Names.Balances
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Chain (Hex32 (..), TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store
import StoreService.Names.Turns (balanceTurnCount, newTurns, turnsCount, withBalanceTurn, withLabelTurn)
import StoreService.Names.View (addressText)
import StoreService.Orders (cancelOpenInvoice, settleOrder)
import StoreService.Service (linkText)
import StoreService.Providers (PaymentSignal (..), Provider (..), Rate (..), Reversal (..))
import StoreService.Providers.Stripe (stripeProvider)
import StoreService.Store.Invoices (getInvoice)
import StoreService.Waiters (newWaiters)
import StoreService.Web.Server (WebEnv (..))
import System.Timeout (timeout)
import Test.Hspec

#if defined(dbPostgres)
import Bots.StoreService.WebTests (withServiceStores)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (modifyTVar', newTVarIO, readTVar)
import Database.PostgreSQL.Simple ((:.) (..))
#else
import Database.SQLite.Simple ((:.) (..))
#endif

namesBalancesTests :: Spec
namesBalancesTests = describe "StoreService names: balances" $ do
  describe "store" $ do
    it "credits a balance, making it on the first credit" testCredit
    it "refuses negative late credits, credited and refunded cents" testNoNegativeCents
    it "refunds a balance only at its amount and above 0" testRefund
    it "keeps the invariant over credit, refund, credit, refund, refunded the sum" testCreditRefundTwice
    it "finds an order and its balance by the order's registration id or its invoice id" testByReference
    it "links a closed order with its late credits and none of its invoice's amount" testLinkOfClosed
    it "links an order its invoice paid with the extra credited to it" testLinkOfOverpaid
    it "returns the 50 newest orders and then every older one holding a balance above 0" testFindBalances
  describe "refunds" $ do
    it "refuses no order, no balance and another amount, then records one with the order's payment" testRecordRefund
    it "words the order with its payment and late or overpaid cents" testLinkText
  describe "late credits" $ do
    it "credits a late payment once per credit number, a taken number changing neither the rows nor the balance" testCreditLatePayment
    it "tells each case of pending money, confirmed money whose cents still read 0 as Monero's can and money with no confirmed figure among them, and selects confirmed money not all credited, by the order whatever the invoice's status" testLateScans
    it "leaves 0.1 and 0.2 BTC credited against 0.3 confirmed neither pending nor selected" testExactUnits
    it "alerts on pending money past the cutoff once, again only when more arrives, claimed by one of two instances" testLateAlerts
    it "alerts again on money that could not be counted in units when more of it arrives" testUncountedAlertRepeats
  describe "turns" $ do
    it "runs two writers of one balance one after the other, and nests in a label turn" testBalanceTurn
  describe "reversals" $ do
    it "cancels a paid order of which no register was sent, releasing its name and crediting nothing, once per event" testReversalCancels
    it "records a reversal of an order not yet paid, and closes the order as reversed when its payment settles" testReversalBeforePayment
    it "keeps an order whose register was sent, naming the transaction" testReversalKeepsSent
    it "cancels an order whose only register is a settled one of another name, as after a move, and keeps one with a register mined or recorded" testReversalCurrentName
    it "takes the reversed cents back from a taken order's balance, as far as it and the invoice's amount hold them, a refund as the rise of its total" testReversalTakesBack
    it "changes nothing for an order whose refund support recorded, and for an early fraud warning" testReversalAlertsOnly
    it "cancels a live order of which no register was sent, also after support refunded part of its balance" testReversalCancelsRefunded
    it "cancels a live order of which no register was sent for a dispute of part of its payment" testReversalCancelsPartial
    it "cancels an order by a signed dispute delivered to the Stripe webhook" testDisputeWebhook
    it "locks an order only at its version" testLockRegistration
#if defined(dbPostgres)
  describe "two connections" $ do
    it "applies a late credit and a refund of one balance, keeping the invariant" testConcurrentWrites
    it "credits nothing that a concurrent dispute leaves in the balance" testConcurrentCreditDispute
#endif

-- fixtures

-- | Every name and commitment its own, so any number of live orders fit.
fieldsN :: Text -> OrderFields
fieldsN label =
  OrderFields
    { ofLabelHash = labelHash label,
      ofLength = lengthOf 6,
      ofDuration = 69292800,
      ofCommitment = Hex32 $ C.sha256Hash $ encodeUtf8 label,
      ofSealed = "sealed-" <> label,
      ofAgreedTerms = agreed plainTerms
    }

-- | A crypto invoice row, or a card one with its client secret in place of an address.
invoiceRow :: DB.Connection -> Text -> Bool -> USDCents -> UTCTime -> IO ()
invoiceRow db invId card cents expiresAt =
  DB.execute
    db
    "INSERT INTO sx_badge_service_invoices (invoice_id, provider, provider_ref, product, price, amount, currency, payment_url, payment_address, payment_crypto_currency, payment_crypto_amount, expires_at, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    ((invId, provider, "ref-" <> invId, "name" :: Text, cents, cents) :. ("usd" :: Text, url, address, currency, cryptoAmount) :. (expiresAt, "open" :: Text, t0, t0))
  where
    destination :: (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
    destination
      | card = ("stripe", Just "pi_secret", Nothing, Nothing, Nothing)
      | otherwise = ("crypto", Nothing, Just "bc1qexampleaddress", Just "btc", Just "0.003")
    (provider, url, address, currency, cryptoAmount) = destination

-- | A paid order, unpaid and live, whose invoice of the cents expires at the given time.
placeOrderRow :: DBStore -> Text -> Bool -> USDCents -> UTCTime -> IO ()
placeOrderRow st r card cents expiresAt = withTransaction st $ \db -> do
  invoiceRow db ("inv-" <> r) card cents expiresAt
  createPaidRegistration db (regFor r (OTInvoice method cents)) (paidOrder ("inv-" <> r)) {poMethod = method, poPrice = cents} t0 `shouldReturn` True
  where
    method = if card then SPMCard CPStripe else btc

regFor :: Text -> OrderTerms -> NewRegistration
regFor r terms = (newReg r ownerA (fieldsN r)) {nrgOrderHash = orderHash ownerA (fieldsN r) terms}

-- | A taken paid order with a balance of its price.
source :: DBStore -> Text -> USDCents -> IO ()
source st r cents = do
  placeOrderRow st r False cents (at 60)
  taken <- reg st r
  write st taken {nrPhase = OTaken} `shouldReturn` Written 2
  credit st r cents

credit :: DBStore -> Text -> USDCents -> IO ()
credit st r cents = withTransaction st $ \db -> creditBalance db (RegistrationId r) cents t0

refund :: DBStore -> Text -> USDCents -> IO Bool
refund st r cents = withTransaction st $ \db -> refundBalance db (RegistrationId r) cents (at 5)

-- | Amount, credited and refunded.
figures :: DBStore -> Text -> IO (Maybe (USDCents, USDCents, USDCents))
figures st r = withTransaction st $ \db -> fmap (\Balance {blAmount, blCredited, blRefunded} -> (blAmount, blCredited, blRefunded)) <$> getBalance db (RegistrationId r)

-- | amount = credited - refunded.
invariantHolds :: DBStore -> Text -> IO ()
invariantHolds st r = do
  Just (amount, credited, refunded) <- figures st r
  (r, amount) `shouldBe` (r, credited - refunded)

-- | A payment row with the figures the provider's reads left: cents received and confirmed, the crypto text, and units received and confirmed.
data Paid = Paid
  { pCents :: Int,
    pConfirmedCents :: Int,
    pCrypto :: Maybe Text,
    pUnits :: Maybe Int64,
    pConfirmedUnits :: Maybe Int64,
    pInFull :: Bool
  }

cryptoPaid :: Int64 -> Int64 -> Paid
cryptoPaid units confirmed = Paid {pCents = 20000, pConfirmedCents = 0, pCrypto = Just "0.003", pUnits = Just units, pConfirmedUnits = Just confirmed, pInFull = False}

cardPaid :: Int -> Int -> Paid
cardPaid cents confirmed = Paid {pCents = cents, pConfirmedCents = confirmed, pCrypto = Nothing, pUnits = Nothing, pConfirmedUnits = Nothing, pInFull = False}

payment :: DBStore -> Text -> Paid -> UTCTime -> IO ()
payment st invId Paid {pCents, pConfirmedCents, pCrypto, pUnits, pConfirmedUnits, pInFull} createdAt =
  withTransaction st $ \db ->
    DB.execute
      db
      "INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, amount, currency, crypto_paid, paid_in_full, status, confirmed_amount, received_units, confirmed_units, invoice_rate, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      ((invId, invId, "crypto" :: Text, pCents, "usd" :: Text, pCrypto, if pInFull then 1 else 0 :: Int, "pending" :: Text, pConfirmedCents, pUnits) :. (pConfirmedUnits, Just ("62500" :: Text), createdAt, createdAt))

-- | An order closed unpaid, by crypto unless card, with a late payment row created at the given time.
closedWith :: DBStore -> Text -> Bool -> Paid -> UTCTime -> IO ()
closedWith st r card paid createdAt = do
  placeOrderRow st r card 20000 (at 60)
  open <- reg st r
  write st open {nrPhase = OClosed ICExpired} `shouldReturn` Written 2
  payment st ("inv-" <> r) paid createdAt

lateCredit :: Text -> Int -> Maybe Int64 -> USDCents -> LateCredit
lateCredit r n units cents =
  LateCredit
    { lcInvoice = InvoiceId ("inv-" <> r),
      lcCreditNo = n,
      lcRegistration = RegistrationId r,
      lcAmount = maybe (CACard cents) crypto units
    }
  where
    crypto caUnits =
      CACrypto
        { caUnits,
          caInvoiceRate = Rate {rateText = "62500", rateValue = 62500},
          caCurrentRate = Rate {rateText = "60000.5", rateValue = 60000.5},
          caRateUsed = Rate {rateText = "61000", rateValue = 61000},
          caCents = cents
        }

creditedUnits :: LateCreditState -> Int64
creditedUnits LateCreditState {lsFigures} = case lsFigures of
  LFCrypto {lfCreditedUnits} -> lfCreditedUnits
  LFCard {} -> 0

creditLate :: DBStore -> LateCredit -> IO Bool
creditLate st c = withTransaction st $ \db -> isJust <$> creditLatePayment db c (at 3)

-- store

testCredit :: IO ()
testCredit = withServiceStore $ \st -> do
  placeOrderRow st "s" False 20000 (at 60)
  figures st "s" `shouldReturn` Nothing
  credit st "s" 20000
  credit st "s" 2500
  figures st "s" `shouldReturn` Just (22500, 22500, 0)

testNoNegativeCents :: IO ()
testNoNegativeCents = withServiceStore $ \st -> do
  source st "s" 10000
  placeOrderRow st "x" False 20000 (at 60)
  credit st "x" 5000
  creditLate st (lateCredit "x" 1 Nothing (-1)) `shouldThrow` anyException
  figures st "x" `shouldReturn` Just (5000, 5000, 0)
  forM_ ["credited", "refunded"] $ \column ->
    withTransaction st (\db -> DB.execute_ db $ "UPDATE sx_badge_service_name_balances SET " <> column <> " = -1 WHERE registration_id = 's'") `shouldThrow` anyException
  figures st "s" `shouldReturn` Just (10000, 10000, 0)

testRefund :: IO ()
testRefund = withServiceStore $ \st -> do
  source st "s" 10000
  mapM (refund st "s") [9999, 10001] `shouldReturn` [False, False]
  figures st "s" `shouldReturn` Just (10000, 10000, 0)
  refund st "s" 10000 `shouldReturn` True
  Just Balance {blAmount, blCredited, blRefunded, blRefundedAt} <- withTransaction st $ \db -> getBalance db (RegistrationId "s")
  (blAmount, blCredited, blRefunded, blRefundedAt) `shouldBe` (0, 10000, 10000, Just (at 5))
  mapM (refund st "s") [0, 10000] `shouldReturn` [False, False]

testCreditRefundTwice :: IO ()
testCreditRefundTwice = withServiceStore $ \st -> do
  source st "s" 10000
  refund st "s" 10000 `shouldReturn` True
  credit st "s" 4000
  refund st "s" 4000 `shouldReturn` True
  figures st "s" `shouldReturn` Just (0, 14000, 14000)
  invariantHolds st "s"

testByReference :: IO ()
testByReference = withServiceStore $ \st -> do
  source st "s" 10000
  placeOrderRow st "plain" False 20000 (at 60)
  let byRef :: Text -> IO (Maybe (RegistrationId, Maybe USDCents))
      byRef ref = withTransaction st $ \db -> fmap (fmap (fmap blAmount)) <$> balanceByReference db ref
  mapM byRef ["s", "inv-s"] `shouldReturn` replicate 2 (Just (RegistrationId "s", Just 10000))
  mapM byRef ["plain", "inv-plain"] `shouldReturn` replicate 2 (Just (RegistrationId "plain", Nothing))
  byRef "nothing" `shouldReturn` Nothing

testLinkOfClosed :: IO ()
testLinkOfClosed = withServiceStore $ \st -> do
  placeOrderRow st "d" False 20000 (at 60)
  waiters <- newWaiters
  cancelOpenInvoice st waiters Nothing (InvoiceId "inv-d") (at 2) `shouldReturn` True
  let late = lateCredit "d" 1 (Just 40000) 2400
  creditLate st late `shouldReturn` True
  withTransaction st (\db -> balanceLink db (RegistrationId "d"))
    `shouldReturn` Just BalanceLink {lkRegistration = RegistrationId "d", lkOwner = ownerA, lkPayment = OPInvoice btc (InvoiceId "inv-d"), lkProviderRef = Just "ref-inv-d", lkPaid = 0, lkLateCredits = [late]}

testLinkOfOverpaid :: IO ()
testLinkOfOverpaid = withServiceStore $ \st -> do
  placeOrderRow st "o" False 20000 t0
  queued <- reg st "o"
  write st queued {nrPhase = OQueued} `shouldReturn` Written (nrVersion queued + 1)
  let late = lateCredit "o" 1 (Just 40000) 2400
  creditLate st late `shouldReturn` True
  withTransaction st (\db -> balanceLink db (RegistrationId "o"))
    `shouldReturn` Just BalanceLink {lkRegistration = RegistrationId "o", lkOwner = ownerA, lkPayment = OPInvoice btc (InvoiceId "inv-o"), lkProviderRef = Just "ref-inv-o", lkPaid = 20000, lkLateCredits = [late]}

testFindBalances :: IO ()
testFindBalances = withServiceStore $ \st -> do
  let rid :: Int -> Text
      rid i = "r" <> T.pack (show i)
  forM_ [1 .. 52 :: Int] $ \i -> withTransaction st $ \db -> do
    invoiceRow db ("inv-" <> rid i) False 20000 (at 60)
    createPaidRegistration db (newReg (rid i) ownerA (fieldsN $ rid i)) (paidOrder ("inv-" <> rid i)) (at i) `shouldReturn` True
  credit st "r1" 5000
  credit st "r2" 3000
  refund st "r2" 3000 `shouldReturn` True
  map nrId <$> withTransaction st (`findRegistrations` ownerA) `shouldReturn` map (RegistrationId . rid) ([52, 51 .. 3] <> [1])

-- refunds

testRecordRefund :: IO ()
testRecordRefund = withServiceStore $ \st -> do
  turns <- newTurns
  source st "s" 10000
  placeOrderRow st "plain" False 20000 (at 60)
  let refundOf :: Text -> USDCents -> UTCTime -> IO (Either RefundRefusal RefundRecord)
      refundOf ref cents now = recordRefund turns st ref cents now
  refundOf "nothing" 10000 t0 `shouldReturn` Left RefundNoOrder
  refundOf "plain" 20000 t0 `shouldReturn` Left RefundNoBalance
  refundOf "s" 9000 t0 `shouldReturn` Left (RefundAmount 10000)
  link <- withTransaction st $ \db -> balanceLink db (RegistrationId "s")
  link `shouldBe` Just BalanceLink {lkRegistration = RegistrationId "s", lkOwner = ownerA, lkPayment = OPInvoice btc (InvoiceId "inv-s"), lkProviderRef = Just "ref-inv-s", lkPaid = 10000, lkLateCredits = []}
  refundOf "inv-s" 10000 t0 `shouldReturn` Right RefundRecord {rdRegistration = RegistrationId "s", rdCents = 10000, rdLink = link}
  figures st "s" `shouldReturn` Just (0, 10000, 10000)
  refundOf "s" 10000 t0 `shouldReturn` Left (RefundAmount 0)
  turnsCount turns `shouldReturn` 0

testLinkText :: IO ()
testLinkText = do
  let link rid (pay, ref_) paid lateCents =
        Just BalanceLink {lkRegistration = RegistrationId rid, lkOwner = ownerA, lkPayment = pay, lkProviderRef = ref_, lkPaid = paid, lkLateCredits = map (lateCredit rid 1 Nothing) lateCents}
      byBtc r = (OPInvoice btc (InvoiceId $ "inv-" <> r), Just $ "ref-" <> r)
      unbilled pay = (pay, Nothing)
      order r = "order " <> r <> " of " <> addressText ownerA <> ": "
  linkText (link "a" (byBtc "a") 20000 []) `shouldBe` order "a" <> "$200.00 by btc on invoice inv-a (provider reference ref-a)"
  linkText (link "d" (byBtc "d") 0 [2400]) `shouldBe` order "d" <> "$24.00 late by btc on invoice inv-d (provider reference ref-d)"
  linkText (link "o" (byBtc "o") 20000 [2400]) `shouldBe` order "o" <> "$200.00 by btc on invoice inv-o (provider reference ref-o), $24.00 overpaid by btc on invoice inv-o (provider reference ref-o)"
  linkText (link "p" (unbilled $ OPCode 1) 0 []) `shouldBe` order "p" <> "a prepaid code"
  linkText (link "x" (byBtc "x") 0 []) `shouldBe` order "x" <> "no payment recorded"

-- late credits

testCreditLatePayment :: IO ()
testCreditLatePayment = withServiceStore $ \st -> do
  closedWith st "x" False (cryptoPaid 400000 400000) t0
  let state :: IO (Maybe (Int64, USDCents, Int))
      state = withTransaction st $ \db -> fmap (\s@LateCreditState {lsCreditedCents, lsCredits} -> (creditedUnits s, lsCreditedCents, lsCredits)) <$> lateCreditState db (InvoiceId "inv-x")
  creditLate st (lateCredit "x" 1 (Just 300000) 3000) `shouldReturn` True
  creditLate st (lateCredit "x" 1 (Just 100000) 1000) `shouldReturn` False
  figures st "x" `shouldReturn` Just (3000, 3000, 0)
  state `shouldReturn` Just (300000, 3000, 1)
  creditLate st (lateCredit "x" 2 (Just 100000) 1000) `shouldReturn` True
  figures st "x" `shouldReturn` Just (4000, 4000, 0)
  state `shouldReturn` Just (400000, 4000, 2)

testLateScans :: IO ()
testLateScans = withServiceStore $ \st -> do
  let cases =
        [ ("uncredited", False, cryptoPaid 300000 300000, Just LPUncredited),
          ("unconfirmed", False, cryptoPaid 300000 0, Just LPUnconfirmed),
          ("monero", False, Paid {pCents = 0, pConfirmedCents = 0, pCrypto = Just "0", pUnits = Just 0, pConfirmedUnits = Just 0, pInFull = True}, Just LPConfirming),
          ("part", False, cryptoPaid 300000 100000, Just LPUnconfirmed),
          ("card", True, cardPaid 5000 5000, Just LPUncredited),
          ("cardpending", True, cardPaid 5000 0, Just LPUnconfirmed),
          ("settledlate", False, cryptoPaid 200000 200000, Just LPUncredited),
          ("uncounted", False, (cryptoPaid 0 0) {pCents = 5000, pUnits = Nothing}, Just LPUncounted),
          ("credited", True, cardPaid 5000 5000, Nothing),
          ("beforecents", False, Paid {pCents = 0, pConfirmedCents = 0, pCrypto = Just "0", pUnits = Just 1000000000000, pConfirmedUnits = Just 1000000000000, pInFull = True}, Just LPUncredited),
          ("noconfirmed", False, (cryptoPaid 300000 0) {pConfirmedUnits = Nothing}, Just LPUnconfirmed)
        ]
  forM_ (zip [1 ..] cases) $ \(i, (r, card, paid, _)) -> closedWith st r card paid (at i)
  creditLate st (lateCredit "part" 1 (Just 100000) 1500) `shouldReturn` True
  creditLate st (lateCredit "credited" 1 Nothing 5000) `shouldReturn` True
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_invoices SET status = ? WHERE invoice_id = ?" ("paid" :: Text, "inv-settledlate" :: Text)
  placeOrderRow st "live" False 20000 (at 60)
  payment st "inv-live" (cryptoPaid 300000 300000) (at 20)
  let pendingOf :: Text -> IO (Text, Maybe LatePending)
      pendingOf r = withTransaction st $ \db -> (r,) <$> latePending db (InvoiceId $ "inv-" <> r)
  forM_ cases $ \(r, _, _, expected) -> pendingOf r `shouldReturn` (r, expected)
  pendingOf "live" `shouldReturn` ("live", Nothing)
  withTransaction st uncreditedLateInvoices `shouldReturn` map (\r -> (InvoiceId $ "inv-" <> r, "ref-inv-" <> r)) ["uncredited", "card", "settledlate", "beforecents"]
  withTransaction st (\db -> lateCreditState db (InvoiceId "inv-part"))
    `shouldReturn` Just
      LateCreditState
        { lsInvoice = InvoiceId "inv-part",
          lsRegistration = RegistrationId "part",
          lsMoney = LMLate,
          lsClosed = True,
          lsSupport = Nothing,
          lsReceivedCents = 20000,
          lsFigures =
            LFCrypto
              { lfCoin = CCBtc,
                lfReceivedUnits = Just 300000,
                lfConfirmedUnits = Just 100000,
                lfInvoicedUnits = Nothing,
                lfInvoiceRate = Just Rate {rateText = "62500", rateValue = 62500},
                lfCreditedUnits = 100000
              },
          lsInvoiceAmount = 20000,
          lsCreditedCents = 1500,
          lsCredits = 1,
          lsPaymentCreatedAt = at 4
        }
  withTransaction st (\db -> lateCreditState db (InvoiceId "inv-plain")) `shouldReturn` Nothing

testExactUnits :: IO ()
testExactUnits = withServiceStore $ \st -> do
  closedWith st "x" False (cryptoPaid 30000000 30000000) t0
  creditLate st (lateCredit "x" 1 (Just 10000000) 6000) `shouldReturn` True
  creditLate st (lateCredit "x" 2 (Just 20000000) 12000) `shouldReturn` True
  withTransaction st (\db -> latePending db (InvoiceId "inv-x")) `shouldReturn` Nothing
  withTransaction st uncreditedLateInvoices `shouldReturn` []
  fmap creditedUnits <$> withTransaction st (\db -> lateCreditState db (InvoiceId "inv-x")) `shouldReturn` Just 30000000

testLateAlerts :: IO ()
testLateAlerts = withServiceStore $ \st -> do
  closedWith st "old" False (cryptoPaid 300000 0) t0
  closedWith st "card" True (cardPaid 5000 0) (at 1)
  closedWith st "done" True (cardPaid 5000 5000) (at 2)
  creditLate st (lateCredit "done" 1 Nothing 5000) `shouldReturn` True
  closedWith st "new" False (cryptoPaid 300000 0) (at 120)
  let due :: IO [LateAlert]
      due = withTransaction st $ \db -> lateAlertDue db (at 60)
      marks :: LateAlert -> (InvoiceId, LatePending, Maybe Int64, Maybe UTCTime, Maybe Int64)
      marks LateAlert {laState = LateCreditState {lsInvoice}, laPending, laReceived, laAlertedAt, laAlertedReceived} = (lsInvoice, laPending, laReceived, laAlertedAt, laAlertedReceived)
      claim :: LateAlert -> UTCTime -> IO Bool
      claim alert t = withTransaction st $ \db -> claimLateAlert db alert Nothing t
  [old, card] <- due
  map marks [old, card]
    `shouldBe` [(InvoiceId "inv-old", LPUnconfirmed, Just 300000, Nothing, Nothing), (InvoiceId "inv-card", LPUnconfirmed, Just 5000, Nothing, Nothing)]
  claim old (at 61) `shouldReturn` True
  claim old (at 61) `shouldReturn` False
  claim card (at 61) `shouldReturn` True
  due `shouldReturn` []
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_payments SET received_units = ? WHERE invoice_id = ?" (400000 :: Int64, "inv-old" :: Text)
  [more] <- due
  marks more `shouldBe` (InvoiceId "inv-old", LPUnconfirmed, Just 400000, Just (at 61), Just 300000)
  claim more (at 62) `shouldReturn` True
  claim more (at 63) `shouldReturn` False
  due `shouldReturn` []
  map marks <$> withTransaction st (\db -> lateAlertDue db (at 180)) `shouldReturn` [(InvoiceId "inv-new", LPUnconfirmed, Just 300000, Nothing, Nothing)]

testUncountedAlertRepeats :: IO ()
testUncountedAlertRepeats = withServiceStore $ \st -> do
  closedWith st "x" False (cryptoPaid 0 0) {pCents = 5000, pUnits = Nothing} t0
  let due :: IO [LateAlert]
      due = withTransaction st $ \db -> lateAlertDue db (at 60)
  [alert] <- due
  (laPending alert, laReceived alert) `shouldBe` (LPUncounted, Just 5000)
  withTransaction st (\db -> claimLateAlert db alert Nothing (at 61)) `shouldReturn` True
  due `shouldReturn` []
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_payments SET amount = ? WHERE invoice_id = ?" (6000 :: Int, "inv-x" :: Text)
  map (\LateAlert {laPending, laReceived, laAlertedReceived} -> (laPending, laReceived, laAlertedReceived)) <$> due `shouldReturn` [(LPUncounted, Just 6000, Just 5000)]

-- turns

testBalanceTurn :: IO ()
testBalanceTurn = withServiceStore $ \st -> do
  turns <- newTurns
  source st "s" 10000
  inside <- newEmptyMVar
  proceed <- newEmptyMVar
  let src = RegistrationId "s"
  firstRefund <- async $ withBalanceTurn turns src $ do
    putMVar inside ()
    takeMVar proceed
    refund st "s" 10000
  takeMVar inside
  secondRefund <- async $ withBalanceTurn turns src $ refund st "s" 10000
  timeout 5000000 (atomically $ balanceTurnCount turns src >>= check . (== 2)) `shouldReturn` Just ()
  putMVar proceed ()
  wait firstRefund `shouldReturn` True
  wait secondRefund `shouldReturn` False
  timeout 5000000 (withLabelTurn turns (labelHash "alicebakery") $ withBalanceTurn turns src $ pure ()) `shouldReturn` Just ()
  turnsCount turns `shouldReturn` 0

-- reversals

withReversals :: (Ctx -> OrdersEnv -> IORef [Text] -> IO a) -> IO a
withReversals action = withOrders $ \ctx@Ctx {cOrders} -> do
  alerts <- newIORef []
  action ctx cOrders {oeReport = \alert -> modifyIORef' alerts (alert :)} alerts

reverseWith :: OrdersEnv -> Text -> Text -> Reversal -> IO ()
reverseWith env r event reversal = do
  Just row <- getInvoice (oeStore env) (InvoiceId $ "inv-" <> r)
  reverseOrder env row (RegistrationId r) event reversal

queuedOrder :: DBStore -> Text -> IO NameRegistration
queuedOrder st r = do
  placeOrderRow st r True 20000 (at 60)
  unpaid <- reg st r
  write st unpaid {nrPhase = OQueued} `shouldReturn` Written 2
  reg st r

takenOrder :: DBStore -> Text -> USDCents -> IO ()
takenOrder st r cents = do
  placeOrderRow st r True 20000 (at 60)
  open <- reg st r
  write st open {nrPhase = OTaken} `shouldReturn` Written 2
  credit st r cents

alertsWith :: IORef [Text] -> [Text] -> Expectation
alertsWith alerts parts = do
  sent <- reverse <$> readIORef alerts
  length sent `shouldBe` length parts
  forM_ (zip parts sent) $ \(part, alert) -> alert `shouldSatisfy` T.isInfixOf part

testReversalCancels :: IO ()
testReversalCancels = withReversals $ \Ctx {cStore = st} env alerts -> do
  queued <- queuedOrder st "a"
  let holderOf = withTransaction st (`labelHolder` nrLabelHash queued)
  holderOf `shouldReturn` Just (RegistrationId "a")
  reverseWith env "a" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 20000
  cancelled <- reg st "a"
  (nrPhase cancelled, nrVersion cancelled, nrReveal cancelled) `shouldBe` (OClosed ICReversed, 3, Nothing)
  holderOf `shouldReturn` Nothing
  alertsWith alerts ["names: card dispute dp_1 of 20000 cents on payment ref-inv-a of order a: it is cancelled, as no register transaction of its current name was sent, and its name is released. Evidence: payment ref-inv-a of 20000 cents"]
  reverseWith env "a" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 20000
  length <$> readIORef alerts `shouldReturn` 1
  nrVersion <$> reg st "a" `shouldReturn` 3
  credit st "a" 20000
  figures st "a" `shouldReturn` Nothing
  payment st "inv-a" (cardPaid 20000 20000) t0
  withTransaction st uncreditedLateInvoices `shouldReturn` []
  withTransaction st (\db -> lateWait db (InvoiceId "inv-a")) `shouldReturn` Nothing

testReversalBeforePayment :: IO ()
testReversalBeforePayment = withReversals $ \Ctx {cStore = st, cWeb} env alerts -> do
  placeOrderRow st "d" False 10000 (at 60)
  reverseWith env "d" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 10000
  nrPhase <$> reg st "d" `shouldReturn` OUnpaid (InvoiceId "inv-d")
  alertsWith alerts ["names: card dispute dp_1 of 10000 cents on payment ref-inv-d of order d: its payment is not recorded yet, so it is closed as reversed when it is. Evidence"]
  settleOrder st (weWaiters cWeb) (Just $ oePhases env) (InvoiceId "inv-d") (SigSettled (rcv 10000 Nothing) t0) t0 `shouldReturn` ISPaid
  d <- reg st "d"
  (nrPhase d, nrPaidAt d) `shouldBe` (OClosed ICReversed, Just t0)
  withTransaction st (`labelHolder` nrLabelHash d) `shouldReturn` Nothing
  figures st "d" `shouldReturn` Nothing

testReversalKeepsSent :: IO ()
testReversalKeepsSent = withReversals $ \Ctx {cStore = st} env alerts -> do
  _ <- queuedOrder st "b"
  _ <- withTransaction st $ \db -> insertTransaction db (TxRegistration RTRegister $ RegistrationId "b") NewTransaction {ntxRaw = "raw", ntxHash = TxHash $ hashFixture 9, ntxNonce = 1} t0
  reverseWith env "b" "evt_1" $ RVRefund "ch_1" $ CurrencyAmount 20000
  nrPhase <$> reg st "b" `shouldReturn` OQueued
  alertsWith alerts ["names: refund of charge ch_1 to 20000 cents in all on payment ref-inv-b of order b: it is not cancelled, as its register transaction was sent. Evidence"]
  readIORef alerts >>= (`shouldSatisfy` all (("no commit transaction sent; register " <> strText (TxHash $ hashFixture 9) <> " pending") `T.isInfixOf`))

testReversalCurrentName :: IO ()
testReversalCurrentName = withReversals $ \Ctx {cStore = st} env alerts -> do
  let sent r n settlement = withTransaction st $ \db -> do
        let h = TxHash $ hashFixture n
        _ <- insertTransaction db (TxRegistration RTRegister $ RegistrationId r) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} t0
        recordTransactionOutcome db h TSPending settlement
  _ <- queuedOrder st "v"
  sent "v" 9 (TSRevertedAt 100) `shouldReturn` True
  _ <- queuedOrder st "n"
  sent "n" 10 (TSMinedAt 100) `shouldReturn` True
  recorded <- queuedOrder st "c"
  sent "c" 11 (TSRevertedAt 100) `shouldReturn` True
  write st recorded {nrHistory = (nrHistory recorded) {thRegisterTx = Just $ TxHash $ hashFixture 11}} `shouldReturn` Written 3
  forM_ ["v", "n", "c"] $ \r -> reverseWith env r "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 20000
  mapM (fmap nrPhase . reg st) ["v", "n", "c"] `shouldReturn` [OClosed ICReversed, OQueued, OQueued]
  alertsWith alerts ["of order v: it is cancelled, as no register transaction of its current name was sent", "of order n: it is not cancelled, as its register transaction was sent", "of order c: it is not cancelled, as its register transaction was sent"]

testReversalTakesBack :: IO ()
testReversalTakesBack = withReversals $ \Ctx {cStore = st} env alerts -> do
  takenOrder st "t" 25000
  let refunded total = RVRefund "ch_1" $ CurrencyAmount total
      amount = fmap (\(a, _, _) -> a) <$> figures st "t"
  reverseWith env "t" "evt_1" $ refunded 500
  amount `shouldReturn` Just 24500
  reverseWith env "t" "evt_1" $ refunded 500
  reverseWith env "t" "evt_2" $ refunded 2000
  amount `shouldReturn` Just 23000
  reverseWith env "t" "evt_3" $ RVDispute "dp_1" $ CurrencyAmount 20000
  amount `shouldReturn` Just 5000
  nrPhase <$> reg st "t" `shouldReturn` OTaken
  alertsWith alerts ["it is not cancelled, as it is taken; 500 cents taken back", "it is not cancelled, as it is taken; 1500 cents taken back", "it is not cancelled, as it is taken; 18000 cents taken back"]

testReversalAlertsOnly :: IO ()
testReversalAlertsOnly = withReversals $ \Ctx {cStore = st} env alerts -> do
  takenOrder st "r" 20000
  refund st "r" 20000 `shouldReturn` True
  reverseWith env "r" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 20000
  figures st "r" `shouldReturn` Just (0, 20000, 20000)
  _ <- queuedOrder st "w"
  Just row <- getInvoice st (InvoiceId "inv-w")
  warnOfFraud env row (RegistrationId "w") "card_never_received"
  nrPhase <$> reg st "w" `shouldReturn` OQueued
  credit st "w" 100
  figures st "w" `shouldReturn` Just (100, 100, 0)
  alertsWith alerts ["of order r: support recorded its refund, so nothing changed. Evidence", "names: early fraud warning, card_never_received on payment ref-inv-w of order w: nothing changed. Evidence"]

testReversalCancelsPartial :: IO ()
testReversalCancelsPartial = withReversals $ \Ctx {cStore = st} env alerts -> do
  _ <- queuedOrder st "p"
  reverseWith env "p" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 5000
  nrPhase <$> reg st "p" `shouldReturn` OClosed ICReversed
  figures st "p" `shouldReturn` Nothing
  alertsWith alerts ["names: card dispute dp_1 of 5000 cents on payment ref-inv-p of order p: it is cancelled, as no register transaction of its current name was sent, and its name is released. Evidence"]

testReversalCancelsRefunded :: IO ()
testReversalCancelsRefunded = withReversals $ \Ctx {cStore = st} env alerts -> do
  _ <- queuedOrder st "m"
  credit st "m" 800
  refund st "m" 800 `shouldReturn` True
  reverseWith env "m" "evt_1" $ RVDispute "dp_1" $ CurrencyAmount 20000
  nrPhase <$> reg st "m" `shouldReturn` OClosed ICReversed
  figures st "m" `shouldReturn` Just (0, 800, 800)
  alertsWith alerts ["of order m: it is cancelled, as no register transaction of its current name was sent, and its name is released. Evidence"]

testDisputeWebhook :: IO ()
testDisputeWebhook = withFakeStripe $ \fake -> do
  real <- stripeProvider (fsConfig fake)
  withOrdersVia (\p -> p {pVerifyWebhook = pVerifyWebhook real}) $ \Ctx {cStore = st, cClient} -> do
    _ <- queuedOrder st "a"
    t <- round . utcTimeToPOSIXSeconds <$> getCurrentTime
    let body = stripeObjectEvent "evt_1" "charge.dispute.created" ["id" J..= ("dp_1" :: Text), "amount" J..= (20000 :: Int), "payment_intent" J..= ("ref-inv-a" :: Text)]
        deliver = statusOf <$> webRequestBody cClient "POST" "/webhooks/stripe" (stripeSigHeader (sWebhookSecret $ fsConfig fake) t body) body
    deliver `shouldReturn` 200
    nrPhase <$> reg st "a" `shouldReturn` OClosed ICReversed
    deliver `shouldReturn` 200
    nrVersion <$> reg st "a" `shouldReturn` 3

testLockRegistration :: IO ()
testLockRegistration = withServiceStore $ \st -> do
  placeOrderRow st "a" True 20000 (at 60)
  r <- reg st "a"
  withTransaction st (`lockRegistration` r) `shouldReturn` True
  write st r {nrPhase = OQueued} `shouldReturn` Written 2
  withTransaction st (`lockRegistration` r) `shouldReturn` False

strText :: StrEncoding a => a -> Text
strText = safeDecodeUtf8 . strEncode

#if defined(dbPostgres)
-- | Both transactions begun before either writes, so the second writer waits for the first's row lock and re-reads its condition.
together :: DBStore -> DBStore -> (DB.Connection -> IO a) -> (DB.Connection -> IO b) -> IO (a, b)
together st1 st2 w1 w2 = do
  begun <- newTVarIO (0 :: Int)
  let run :: DBStore -> (DB.Connection -> IO c) -> IO c
      run st w = withTransaction st $ \db -> do
        atomically $ modifyTVar' begun (+ 1)
        atomically $ readTVar begun >>= check . (== 2)
        w db
  concurrently (run st1 w1) (run st2 w2)

testConcurrentWrites :: IO ()
testConcurrentWrites = withServiceStores $ \open -> do
  st <- open
  other <- open
  forM_ [1 .. 25 :: Int] $ \n -> do
    let lateSrc = "late" <> T.pack (show n)
    source st lateSrc 10000
    (credited, refundedLate) <- together st other (\db -> isJust <$> creditLatePayment db (lateCredit lateSrc 1 (Just 50000) 3000) t0) (\db -> refundBalance db (RegistrationId lateSrc) 10000 t0)
    credited `shouldBe` True
    (n,) <$> figures st lateSrc `shouldReturn` (n, Just $ if refundedLate then (3000, 13000, 10000) else (13000, 13000, 0))
    invariantHolds st lateSrc

testConcurrentCreditDispute :: IO ()
testConcurrentCreditDispute = withServiceStores $ \open -> do
  st <- open
  other <- open
  forM_ [1 .. 25 :: Int] $ \n -> do
    let r = "disputed" <> T.pack (show n)
        rid = RegistrationId r
        inv = InvoiceId $ "inv-" <> r
    placeOrderRow st r True 20000 (at 60)
    order <- reg st r
    _ <-
      together
        st
        other
        (\db -> lockRegistration db order >> recordReversal db ("evt_" <> r) inv (RVDispute "dp_1" $ CurrencyAmount 20000) >>= mapM_ (\cents -> takeBackBalance db inv rid cents t0))
        (\db -> creditLatePayment db (lateCredit r 1 Nothing 3000) t0)
    (n,) . fmap (\(amount, _, _) -> amount) <$> figures st r >>= (`shouldSatisfy` (`elem` [Nothing, Just 0]) . snd)
#endif
