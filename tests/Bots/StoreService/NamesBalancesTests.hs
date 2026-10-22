{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Bots.StoreService.NamesBalancesTests (namesBalancesTests) where

import Bots.StoreService.NamesOrdersTests (Ctx (..), withOrders)
import Bots.StoreService.NamesStoreTests (at, btc, invoiceStatus, newReg, ownerA, ownerB, paidOrder, reg, signature, t0, write)
import Bots.StoreService.WebTests (capturingStderr, rcv, withServiceStore, withServiceStores)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime)
import Simplex.Chat.PaymentService.Types (CardProvider (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.SimplexName (labelHash)
import StoreService.Config (ServiceConfig (..))
import StoreService.Names.Orders (ClosedOrder (..), OrderRefusal (..), OrderRequest (..), OrdersEnv (..))
import qualified StoreService.Names.Orders as Orders
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store
import StoreService.Names.Turns (balanceTurnCount, newTurns, turnsCount, withBalanceTurn, withLabelTurn)
import StoreService.Orders (cancelOpenInvoice, expireOverdue, settleOrder)
import StoreService.Poller (cleanupUnpaid, newPollerEnv, unpaidRetention)
import StoreService.Providers (PaymentSignal (..))
import StoreService.Waiters (newWaiters)
import StoreService.Web.Server (WebEnv (..))
import System.Timeout (timeout)
import Test.Hspec

#if defined(dbPostgres)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (modifyTVar', newTVarIO, readTVar)
import Data.Either (isRight)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
#endif

namesBalancesTests :: Spec
namesBalancesTests = describe "StoreService names: balances" $ do
  describe "store" $ do
    it "credits a balance, making it on the first credit" testCredit
    it "spends a covered price at once from the amount seen, refusing a stale amount, a dearer price or a held balance, changing nothing" testSpendAtOnce
    it "lets one of two difference orders hold a balance, the losing order not written" testTwoHolds
    it "spends a hold only for its holder and while the amount covers it, clearing the hold" testSpendHeld
    it "releases a hold only for its holder" testRelease
    it "refunds a balance only at its amount, above 0 and not held, refused while held and accepted once released" testRefund
    it "keeps amount = credited - spent - refunded over spends at once, a held spend, a released hold and a refund, spent counting orders past unpaid" testInvariant
    it "keeps the invariant over credit, refund, credit, refund, refunded the sum" testCreditRefundTwice
    it "reads a hold and a spend back through a new store connection" testRestart
    it "finds an order and its balance by the order's registration id or its invoice id" testByReference
    it "chains an order back along its balance sources over three orders, with each payment and its cents" testChain
    it "stops the chain of a closed difference order at itself, with its late credits" testChainOfClosed
    it "hashes a balance order over its fields, the paying order, price, balance and method, and stores it for a difference spend" testBalanceOrderHash
    it "returns the 50 newest orders and then every older one holding a balance above 0" testFindBalances
  describe "closes" $ do
    it "releases a hold at a cancel, the sweep's expiry, settlement's expiry and the next order's close" testClosesRelease
    it "reads an overdue holder's hold as none, and closes it in the transaction of the order placed" testOverdueHolderClosed
    it "skips a named holder no longer live, and refuses with the balance when a live one cannot be closed" testNamedHolders
    it "keeps at the cleanup, and logs, an unpaid order still holding a balance" testCleanupKeepsHolder
  describe "late credits" $ do
    it "credits a late payment once per credit number, a taken number changing neither the rows nor the balance" testCreditLatePayment
    it "tells each case of pending money and selects confirmed money not all credited, by the order whatever the invoice's status" testLateScans
    it "leaves 0.1 and 0.2 BTC credited against 0.3 confirmed neither pending nor selected" testExactUnits
    it "alerts on pending money past the cutoff once, again only when more arrives, claimed by one of two instances" testLateAlerts
  describe "turns" $ do
    it "runs two holders of one balance one after the other, and nests in a label turn" testBalanceTurn
#if defined(dbPostgres)
  describe "two connections" $ do
    it "applies one of a spend and a refund, or of a held spend and a refund, and keeps the invariant for a credit and a refund" testConcurrentWrites
#endif

-- fixtures

-- | Every name and commitment its own, so any number of live orders fit.
fieldsN :: Text -> OrderFields
fieldsN label =
  OrderFields
    { ofLabelHash = labelHash label,
      ofLength = T.length label,
      ofDuration = 69292800,
      ofCommitment = C.sha256Hash $ encodeUtf8 label,
      ofSealed = "sealed-" <> label
    }

-- | A crypto invoice row, or a card one with its client secret in place of an address.
invoiceRow :: DB.Connection -> Text -> Bool -> Int -> UTCTime -> IO ()
invoiceRow db invId card cents expiresAt =
  DB.execute
    db
    "INSERT INTO sx_badge_service_invoices (invoice_id, provider, price, amount, currency, payment_url, payment_address, payment_crypto_currency, payment_crypto_amount, expires_at, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
    ((invId, provider, cents, cents, "usd" :: Text, url, address, currency, cryptoAmount, expiresAt) :. ("open" :: Text, t0, t0))
  where
    destination :: (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
    destination
      | card = ("stripe", Just "pi_secret", Nothing, Nothing, Nothing)
      | otherwise = ("crypto", Nothing, Just "bc1qexampleaddress", Just "btc", Just "0.003")
    (provider, url, address, currency, cryptoAmount) = destination

-- | A paid order, unpaid and live, whose invoice of the cents expires at the given time.
placeOrderRow :: DBStore -> Text -> Bool -> Int -> UTCTime -> IO ()
placeOrderRow st r card cents expiresAt = withTransaction st $ \db -> do
  invoiceRow db ("inv-" <> r) card cents expiresAt
  createPaidRegistration db (newReg r ownerA (fieldsN r)) (OTInvoice method cents) (paidOrder ("inv-" <> r)) {poMethod = method, poPrice = cents} Nothing t0 `shouldReturn` Right ()
  where
    method = if card then SPMCard CPStripe else btc

-- | A taken paid order with a balance of its price.
source :: DBStore -> Text -> Int -> IO ()
source st r cents = do
  placeOrderRow st r False cents (at 60)
  taken <- reg st r
  write st taken {nrPhase = PTaken} `shouldReturn` Written 2
  credit st r cents

credit :: DBStore -> Text -> Int -> IO ()
credit st r cents = withTransaction st $ \db -> creditBalance db (RegistrationId r) cents t0

holdPrice :: Int
holdPrice = 40000

holdTerms :: Text -> Int -> OrderTerms
holdTerms src seen = OTBalance (RegistrationId src) holdPrice seen (Just btc)

-- | A difference order holding all of the source's balance seen, its invoice of the difference expiring at the given time.
holder :: DBStore -> Text -> Text -> Int -> UTCTime -> IO (Either RegistrationRefused ())
holder st r src seen expiresAt = withTransaction st $ \db -> do
  invoiceRow db ("inv-" <> r) False (holdPrice - seen) expiresAt
  createPaidRegistration db (newReg r ownerA (fieldsN r)) (holdTerms src seen) (paidOrder ("inv-" <> r)) {poPrice = holdPrice} (Just $ HeldBalance (RegistrationId src) seen) t0

hold :: Text -> UTCTime -> Bool -> BalanceHold
hold h bhUntil bhOverdue = BalanceHold {bhHolder = RegistrationId h, bhUntil, bhOverdue}

coveredSpend :: DBStore -> Text -> Text -> Int -> Int -> IO (Either RegistrationRefused ())
coveredSpend st r src price seen = withTransaction st $ \db -> createBalanceRegistration db (newReg r ownerA (fieldsN r)) (RegistrationId src) price seen t0

-- | The holder queued and its hold spent in one transaction, as settlement does.
settleHolder :: DB.Connection -> NameRegistration -> IO Bool
settleHolder db r = do
  updateRegistration db r {nrPhase = PQueued} t0 `shouldReturn` Written (nrVersion r + 1)
  spendHeldBalance db (nrId r) (nrBalanceUsed r) t0

release :: DBStore -> Text -> IO Bool
release st h = withTransaction st $ \db -> releaseHeldBalance db (RegistrationId h) t0

refund :: DBStore -> Text -> Int -> IO Bool
refund st r cents = withTransaction st $ \db -> refundBalance db (RegistrationId r) cents (at 5)

-- | Amount, credited, refunded and the hold, as they read at the given time.
figures :: DBStore -> Text -> UTCTime -> IO (Maybe (Int, Int, Int, Maybe BalanceHold))
figures st r now = withTransaction st $ \db -> fmap (\Balance {blAmount, blCredited, blRefunded, blHold} -> (blAmount, blCredited, blRefunded, blHold)) <$> getBalance db (RegistrationId r) now

-- | amount = credited - spent - refunded, spent being the balance_used of the orders past unpaid it paid for.
invariantHolds :: DBStore -> Text -> IO ()
invariantHolds st r = do
  Just (amount, credited, refunded, _) <- figures st r t0
  used <- withTransaction st $ \db -> DB.query db "SELECT balance_used FROM sx_badge_service_name_registrations WHERE balance_source = ? AND phase <> ?" (r, PUnpaid)
  (r, amount) `shouldBe` (r, credited - sum (map fromOnly (used :: [Only Int])) - refunded)

absent :: DBStore -> Text -> IO ()
absent st r = withTransaction st (\db -> getRegistration db (RegistrationId r)) `shouldReturn` Nothing

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
  write st open {nrLive = False} `shouldReturn` Written 2
  payment st ("inv-" <> r) paid createdAt

lateCredit :: Text -> Int -> Maybe Int64 -> Int -> LateCredit
lateCredit r n units cents =
  LateCredit
    { lcInvoice = InvoiceId ("inv-" <> r),
      lcCreditNo = n,
      lcRegistration = RegistrationId r,
      lcUnits = units,
      lcInvoiceRate = Just "62500",
      lcCurrentRate = Just "60000.5",
      lcRateUsed = Just "60000.5",
      lcCents = cents
    }

creditLate :: DBStore -> LateCredit -> IO Bool
creditLate st c = withTransaction st $ \db -> creditLatePayment db c (at 3)

orderRequest :: Text -> OrderRequest
orderRequest r = OrderRequest {orOwner = ownerA, orFields = fieldsN r, orTimestamp = 0, orSignature = signature}

spendInsert :: Text -> Text -> Int -> Int -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
spendInsert r src price seen db now = first (const $ ORBalanceChanged Nothing) <$> createBalanceRegistration db (newReg r ownerA (fieldsN r)) (RegistrationId src) price seen now

closedIds :: Either OrderRefusal [ClosedOrder] -> Either OrderRefusal [(RegistrationId, Int64)]
closedIds = fmap $ map $ \ClosedOrder {coRegistration, coVersion} -> (nrId coRegistration, coVersion)

-- store

testCredit :: IO ()
testCredit = withServiceStore $ \st -> do
  placeOrderRow st "s" False 20000 (at 60)
  figures st "s" t0 `shouldReturn` Nothing
  credit st "s" 20000
  credit st "s" 2500
  figures st "s" t0 `shouldReturn` Just (22500, 22500, 0, Nothing)

testSpendAtOnce :: IO ()
testSpendAtOnce = withServiceStore $ \st -> do
  source st "s" 10000
  coveredSpend st "c1" "s" 6000 10000 `shouldReturn` Right ()
  c1 <- reg st "c1"
  (nrPhase c1, nrLive c1, nrPrice c1, nrPayment c1, nrBalanceSource c1, nrBalanceUsed c1) `shouldBe` (PQueued, True, 6000, RPBalance, Just (RegistrationId "s"), 6000)
  nrOrderHash c1 `shouldBe` orderHash ownerA (fieldsN "c1") (OTBalance (RegistrationId "s") 6000 10000 Nothing)
  figures st "s" t0 `shouldReturn` Just (4000, 10000, 0, Nothing)
  coveredSpend st "c2" "s" 3000 10000 `shouldReturn` Left RRBalanceChanged
  coveredSpend st "c3" "s" 5000 4000 `shouldReturn` Left RRBalanceChanged
  mapM_ (absent st) ["c2", "c3"]
  figures st "s" t0 `shouldReturn` Just (4000, 10000, 0, Nothing)
  holder st "h" "s" 4000 (at 60) `shouldReturn` Right ()
  coveredSpend st "c4" "s" 1000 4000 `shouldReturn` Left RRBalanceChanged
  absent st "c4"
  release st "h" `shouldReturn` True
  coveredSpend st "c5" "s" 4000 4000 `shouldReturn` Right ()
  figures st "s" t0 `shouldReturn` Just (0, 10000, 0, Nothing)

testTwoHolds :: IO ()
testTwoHolds = withServiceStore $ \st -> do
  source st "s" 10000
  holder st "h1" "s" 10000 (at 60) `shouldReturn` Right ()
  holder st "h2" "s" 10000 (at 60) `shouldReturn` Left RRBalanceChanged
  absent st "h2"
  h1 <- reg st "h1"
  (nrPhase h1, nrPayment h1, nrPrice h1, nrBalanceSource h1, nrBalanceUsed h1) `shouldBe` (PUnpaid, RPInvoice btc (InvoiceId "inv-h1"), holdPrice, Just (RegistrationId "s"), 10000)
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Just $ hold "h1" (at 60) False)
  release st "h1" `shouldReturn` True
  holder st "h3" "s" 9000 (at 60) `shouldReturn` Left RRBalanceChanged
  absent st "h3"
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

testSpendHeld :: IO ()
testSpendHeld = withServiceStore $ \st -> do
  source st "s" 10000
  source st "other" 5000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  holder st "g" "other" 5000 (at 60) `shouldReturn` Right ()
  let spendBy :: Text -> Int -> IO Bool
      spendBy h used = withTransaction st $ \db -> spendHeldBalance db (RegistrationId h) used t0
  spendBy "nobody" 10000 `shouldReturn` False
  spendBy "h" 10001 `shouldReturn` False
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" (at 60) False)
  spendBy "g" 5000 `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" (at 60) False)
  figures st "other" t0 `shouldReturn` Just (0, 5000, 0, Nothing)
  spendBy "h" 10000 `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (0, 10000, 0, Nothing)
  spendBy "h" 0 `shouldReturn` False

testRelease :: IO ()
testRelease = withServiceStore $ \st -> do
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  release st "g" `shouldReturn` False
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" (at 60) False)
  release st "h" `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  release st "h" `shouldReturn` False

testRefund :: IO ()
testRefund = withServiceStore $ \st -> do
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  refund st "s" 10000 `shouldReturn` False
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" (at 60) False)
  release st "h" `shouldReturn` True
  mapM (refund st "s") [9999, 10001] `shouldReturn` [False, False]
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  refund st "s" 10000 `shouldReturn` True
  Just Balance {blAmount, blCredited, blRefunded, blRefundedAt} <- withTransaction st $ \db -> getBalance db (RegistrationId "s") t0
  (blAmount, blCredited, blRefunded, blRefundedAt) `shouldBe` (0, 10000, 10000, Just (at 5))
  mapM (refund st "s") [0, 10000] `shouldReturn` [False, False]

testInvariant :: IO ()
testInvariant = withServiceStore $ \st -> do
  source st "s" 30000
  invariantHolds st "s"
  coveredSpend st "c1" "s" 5000 30000 `shouldReturn` Right ()
  invariantHolds st "s"
  holder st "h1" "s" 25000 (at 60) `shouldReturn` Right ()
  invariantHolds st "s"
  h1 <- reg st "h1"
  withTransaction st (`settleHolder` h1) `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (0, 30000, 0, Nothing)
  invariantHolds st "s"
  credit st "s" 8000
  holder st "h2" "s" 8000 (at 60) `shouldReturn` Right ()
  h2 <- reg st "h2"
  withTransaction st (\db -> closeOverdue db h2 (at 61)) `shouldReturn` Just (nrVersion h2 + 1)
  figures st "s" t0 `shouldReturn` Just (8000, 38000, 0, Nothing)
  invariantHolds st "s"
  refund st "s" 8000 `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (0, 38000, 8000, Nothing)
  invariantHolds st "s"

testCreditRefundTwice :: IO ()
testCreditRefundTwice = withServiceStore $ \st -> do
  source st "s" 10000
  refund st "s" 10000 `shouldReturn` True
  credit st "s" 4000
  refund st "s" 4000 `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (0, 14000, 14000, Nothing)
  invariantHolds st "s"

testRestart :: IO ()
testRestart = withServiceStores $ \open -> do
  st <- open
  source st "s" 10000
  source st "t" 8000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  coveredSpend st "c" "t" 3000 8000 `shouldReturn` Right ()
  let readBack :: DBStore -> IO (Maybe (Int, Int, Int, Maybe BalanceHold), Maybe (Int, Int, Int, Maybe BalanceHold), NameRegistration, NameRegistration)
      readBack store = (,,,) <$> figures store "s" t0 <*> figures store "t" t0 <*> reg store "h" <*> reg store "c"
  stored@(held, spent, _, c) <- readBack st
  (held, spent, nrBalanceUsed c) `shouldBe` (Just (10000, 10000, 0, Just $ hold "h" (at 60) False), Just (5000, 8000, 0, Nothing), 3000)
  restarted <- open
  readBack restarted `shouldReturn` stored

testByReference :: IO ()
testByReference = withServiceStore $ \st -> do
  source st "s" 10000
  placeOrderRow st "plain" False 20000 (at 60)
  let byRef :: Text -> IO (Maybe (RegistrationId, Maybe Int))
      byRef ref = withTransaction st $ \db -> fmap (fmap (fmap blAmount)) <$> balanceByReference db ref t0
  mapM byRef ["s", "inv-s"] `shouldReturn` replicate 2 (Just (RegistrationId "s", Just 10000))
  mapM byRef ["plain", "inv-plain"] `shouldReturn` replicate 2 (Just (RegistrationId "plain", Nothing))
  byRef "nothing" `shouldReturn` Nothing

testChain :: IO ()
testChain = withServiceStore $ \st -> do
  source st "a" 20000
  holder st "b" "a" 20000 (at 60) `shouldReturn` Right ()
  b <- reg st "b"
  withTransaction st (`settleHolder` b) `shouldReturn` True
  queued <- reg st "b"
  write st queued {nrPhase = PTaken} `shouldReturn` Written (nrVersion queued + 1)
  credit st "b" holdPrice
  coveredSpend st "c" "b" 10000 holdPrice `shouldReturn` Right ()
  withTransaction st (\db -> balanceChain db (RegistrationId "c"))
    `shouldReturn` [ BalanceLink {lkRegistration = RegistrationId "c", lkPayment = RPBalance, lkPaid = 0, lkFromBalance = 10000, lkLateCredits = []},
                     BalanceLink {lkRegistration = RegistrationId "b", lkPayment = RPInvoice btc (InvoiceId "inv-b"), lkPaid = holdPrice - 20000, lkFromBalance = 20000, lkLateCredits = []},
                     BalanceLink {lkRegistration = RegistrationId "a", lkPayment = RPInvoice btc (InvoiceId "inv-a"), lkPaid = 20000, lkFromBalance = 0, lkLateCredits = []}
                   ]

testChainOfClosed :: IO ()
testChainOfClosed = withServiceStore $ \st -> do
  source st "s" 15000
  holder st "d" "s" 15000 (at 60) `shouldReturn` Right ()
  waiters <- newWaiters
  cancelOpenInvoice st waiters Nothing (InvoiceId "inv-d") (at 2) `shouldReturn` True
  figures st "s" t0 `shouldReturn` Just (15000, 15000, 0, Nothing)
  let late = lateCredit "d" 1 (Just 40000) 2400
  creditLate st late `shouldReturn` True
  withTransaction st (\db -> balanceChain db (RegistrationId "d"))
    `shouldReturn` [BalanceLink {lkRegistration = RegistrationId "d", lkPayment = RPInvoice btc (InvoiceId "inv-d"), lkPaid = 0, lkFromBalance = 0, lkLateCredits = [late]}]

testBalanceOrderHash :: IO ()
testBalanceOrderHash = withServiceStore $ \st -> do
  let o = fieldsN "h"
      terms = holdTerms "s" 15000
      base = orderHash ownerA o terms
      src = RegistrationId "s"
      variants =
        [ ("labelHash" :: Text, orderHash ownerA o {ofLabelHash = labelHash "hh"} terms),
          ("length", orderHash ownerA o {ofLength = 2} terms),
          ("duration", orderHash ownerA o {ofDuration = 69292801} terms),
          ("owner", orderHash ownerB o terms),
          ("commitment", orderHash ownerA o {ofCommitment = C.sha256Hash "another"} terms),
          ("sealed", orderHash ownerA o {ofSealed = ofSealed o <> "x"} terms),
          ("paying order", orderHash ownerA o (OTBalance (RegistrationId "t") holdPrice 15000 (Just btc))),
          ("price", orderHash ownerA o (OTBalance src (holdPrice + 1) 15000 (Just btc))),
          ("balance", orderHash ownerA o (OTBalance src holdPrice 15001 (Just btc))),
          ("method", orderHash ownerA o (OTBalance src holdPrice 15000 (Just $ SPMCard CPStripe))),
          ("no method", orderHash ownerA o (OTBalance src holdPrice 15000 Nothing)),
          ("an invoice order", orderHash ownerA o (OTInvoice btc holdPrice))
        ]
  orderHash ownerA (fieldsN "h") (holdTerms "s" 15000) `shouldBe` base
  forM_ variants $ \(field, h) -> (field, h == base) `shouldBe` (field, False)
  source st "s" 15000
  holder st "h" "s" 15000 (at 60) `shouldReturn` Right ()
  fmap nrId <$> withTransaction st (`getRegistrationByOrderHash` base) `shouldReturn` Just (RegistrationId "h")

testFindBalances :: IO ()
testFindBalances = withServiceStore $ \st -> do
  let rid :: Int -> Text
      rid i = "r" <> T.pack (show i)
  forM_ [1 .. 52 :: Int] $ \i -> withTransaction st $ \db -> do
    invoiceRow db ("inv-" <> rid i) False 20000 (at 60)
    createPaidRegistration db (newReg (rid i) ownerA (fieldsN $ rid i)) (OTInvoice btc 20000) (paidOrder ("inv-" <> rid i)) Nothing (at i) `shouldReturn` Right ()
  credit st "r1" 5000
  credit st "r2" 3000
  refund st "r2" 3000 `shouldReturn` True
  map nrId <$> withTransaction st (`findRegistrations` ownerA) `shouldReturn` map (RegistrationId . rid) ([52, 51 .. 3] <> [1])

-- closes

testClosesRelease :: IO ()
testClosesRelease = withServiceStore $ \st -> do
  waiters <- newWaiters
  let closes = ["cancel", "sweep", "settled", "next"]
  forM_ closes $ \c -> do
    source st ("s-" <> c) 10000
    holder st ("h-" <> c) ("s-" <> c) 10000 (at 60) `shouldReturn` Right ()
  cancelOpenInvoice st waiters Nothing (InvoiceId "inv-h-cancel") (at 2) `shouldReturn` True
  expireOverdue st waiters Nothing (at 61) (at 61) [InvoiceId "inv-h-sweep"] `shouldReturn` [InvoiceId "inv-h-sweep"]
  settleOrder st waiters Nothing (InvoiceId "inv-h-settled") (SigClosed (rcv 0 Nothing)) (at 2) `shouldReturn` Right ISExpired
  next <- reg st "h-next"
  withTransaction st (\db -> closeOverdue db next (at 61)) `shouldReturn` Just (nrVersion next + 1)
  forM_ closes $ \c -> do
    (c,) <$> figures st ("s-" <> c) t0 `shouldReturn` (c, Just (10000, 10000, 0, Nothing))
    (c,) . nrLive <$> reg st ("h-" <> c) `shouldReturn` (c, False)
    (c,) <$> invoiceStatus st ("inv-h-" <> c) `shouldReturn` (c, "expired")

testOverdueHolderClosed :: IO ()
testOverdueHolderClosed = withOrders $ \Ctx {cStore = st, cOrders} -> do
  now <- oeNow cOrders
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  Just balance <- withTransaction st $ \db -> getBalance db (RegistrationId "s") now
  (blHold balance, heldBy balance) `shouldBe` (Just $ hold "h" (at 60) True, Nothing)
  Just h <- withTransaction st $ \db -> overdueHolder db (RegistrationId "s") now
  nrId h `shouldBe` RegistrationId "h"
  withTransaction st (\db -> overdueHolder db (RegistrationId "s") t0) `shouldReturn` Nothing
  closedIds <$> Orders.placeOrder cOrders (orderRequest "c") [h] (\_ _ -> pure $ Left ORBadRequest) `shouldReturn` Left ORBadRequest
  invoiceStatus st "inv-h" `shouldReturn` "open"
  nrLive <$> reg st "h" `shouldReturn` True
  figures st "s" now `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" (at 60) True)
  closedIds <$> Orders.placeOrder cOrders (orderRequest "c") [h] (spendInsert "c" "s" 6000 10000) `shouldReturn` Right [(RegistrationId "h", nrVersion h + 1)]
  invoiceStatus st "inv-h" `shouldReturn` "expired"
  nrLive <$> reg st "h" `shouldReturn` False
  figures st "s" now `shouldReturn` Just (4000, 10000, 0, Nothing)
  nrPhase <$> reg st "c" `shouldReturn` PQueued

testNamedHolders :: IO ()
testNamedHolders = withOrders $ \Ctx {cStore = st, cOrders, cWeb} -> do
  now <- oeNow cOrders
  source st "s1" 10000
  holder st "h1" "s1" 10000 (at 60) `shouldReturn` Right ()
  h1 <- reg st "h1"
  cancelOpenInvoice st (weWaiters cWeb) Nothing (InvoiceId "inv-h1") now `shouldReturn` True
  closedIds <$> Orders.placeOrder cOrders (orderRequest "c1") [h1] (spendInsert "c1" "s1" 6000 10000) `shouldReturn` Right []
  figures st "s1" now `shouldReturn` Just (4000, 10000, 0, Nothing)
  source st "s2" 10000
  holder st "h2" "s2" 10000 (at 60) `shouldReturn` Right ()
  h2 <- reg st "h2"
  payment st "inv-h2" (cardPaid 100 0) (at 61)
  Just funded <- withTransaction st $ \db -> getBalance db (RegistrationId "s2") now
  blHold funded `shouldBe` Just (hold "h2" (at 60) False)
  closedIds <$> Orders.placeOrder cOrders (orderRequest "c2") [h2] (spendInsert "c2" "s2" 6000 10000) `shouldReturn` Left (ORBalanceChanged $ Just funded)
  absent st "c2"
  nrLive <$> reg st "h2" `shouldReturn` True
  invoiceStatus st "inv-h2" `shouldReturn` "open"

testCleanupKeepsHolder :: IO ()
testCleanupKeepsHolder = withOrders $ \Ctx {cStore = st, cWeb} -> do
  source st "s" 10000
  holder st "h" "s" 10000 t0 `shouldReturn` Right ()
  let due = addUTCTime (unpaidRetention + 1) t0
  poller <- newPollerEnv st (weWaiters cWeb) Nothing (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
  logs <- capturingStderr $ cleanupUnpaid poller due
  logs `shouldSatisfy` T.isInfixOf "unpaid registration h is kept because it still holds the balance of registration s"
  nrLive <$> reg st "h" `shouldReturn` True
  figures st "s" due `shouldReturn` Just (10000, 10000, 0, Just $ hold "h" t0 True)

-- late credits

testCreditLatePayment :: IO ()
testCreditLatePayment = withServiceStore $ \st -> do
  closedWith st "x" False (cryptoPaid 400000 400000) t0
  let state :: IO (Maybe (Int64, Int, Int))
      state = withTransaction st $ \db -> fmap (\LateCreditState {lsCreditedUnits, lsCreditedCents, lsCredits} -> (lsCreditedUnits, lsCreditedCents, lsCredits)) <$> lateCreditState db (InvoiceId "inv-x")
  creditLate st (lateCredit "x" 1 (Just 300000) 3000) `shouldReturn` True
  creditLate st (lateCredit "x" 1 (Just 100000) 1000) `shouldReturn` False
  figures st "x" t0 `shouldReturn` Just (3000, 3000, 0, Nothing)
  state `shouldReturn` Just (300000, 3000, 1)
  creditLate st (lateCredit "x" 2 (Just 100000) 1000) `shouldReturn` True
  figures st "x" t0 `shouldReturn` Just (4000, 4000, 0, Nothing)
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
          ("credited", True, cardPaid 5000 5000, Nothing)
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
  withTransaction st uncreditedLateInvoices `shouldReturn` map (InvoiceId . ("inv-" <>)) ["uncredited", "card", "settledlate"]
  withTransaction st (\db -> lateCreditState db (InvoiceId "inv-part"))
    `shouldReturn` Just
      LateCreditState
        { lsInvoice = InvoiceId "inv-part",
          lsRegistration = RegistrationId "part",
          lsPhase = PUnpaid,
          lsLive = False,
          lsCard = False,
          lsReceivedCents = 20000,
          lsConfirmedCents = 0,
          lsReceivedUnits = Just 300000,
          lsConfirmedUnits = Just 100000,
          lsInvoiceRate = Just "62500",
          lsCreditedUnits = 100000,
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
  fmap lsCreditedUnits <$> withTransaction st (\db -> lateCreditState db (InvoiceId "inv-x")) `shouldReturn` Just 30000000

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
      claim alert t = withTransaction st $ \db -> claimLateAlert db alert t
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

-- turns

testBalanceTurn :: IO ()
testBalanceTurn = withServiceStore $ \st -> do
  turns <- newTurns
  source st "s" 10000
  inside <- newEmptyMVar
  proceed <- newEmptyMVar
  let src = RegistrationId "s"
  firstHolder <- async $ withBalanceTurn turns src $ do
    putMVar inside ()
    takeMVar proceed
    holder st "h1" "s" 10000 (at 60)
  takeMVar inside
  secondHolder <- async $ withBalanceTurn turns src $ holder st "h2" "s" 10000 (at 60)
  timeout 5000000 (atomically $ balanceTurnCount turns src >>= check . (== 2)) `shouldReturn` Just ()
  putMVar proceed ()
  wait firstHolder `shouldReturn` Right ()
  wait secondHolder `shouldReturn` Left RRBalanceChanged
  timeout 5000000 (withLabelTurn turns (labelHash "alicebakery") $ withBalanceTurn turns src $ pure ()) `shouldReturn` Just ()
  turnsCount turns `shouldReturn` 0

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
    let tag = T.pack (show n)
        spendSrc = "spend" <> tag
        heldSrc = "held" <> tag
        lateSrc = "late" <> tag
    source st spendSrc 10000
    (spent, refundedSpend) <-
      together st other (\db -> createBalanceRegistration db (newReg ("c" <> tag) ownerA (fieldsN $ "c" <> tag)) (RegistrationId spendSrc) 6000 10000 t0) (\db -> refundBalance db (RegistrationId spendSrc) 10000 t0)
    (n, isRight spent, refundedSpend) `shouldSatisfy` (\(_, a, b) -> a /= b)
    fmap (\(amount, _, _, _) -> amount) <$> figures st spendSrc t0 `shouldReturn` Just (if refundedSpend then 0 else 4000)
    invariantHolds st spendSrc
    source st heldSrc 10000
    holder st ("h" <> tag) heldSrc 10000 (at 60) `shouldReturn` Right ()
    h <- reg st ("h" <> tag)
    (n,) <$> together st other (`settleHolder` h) (\db -> refundBalance db (RegistrationId heldSrc) 10000 t0) `shouldReturn` (n, (True, False))
    figures st heldSrc t0 `shouldReturn` Just (0, 10000, 0, Nothing)
    invariantHolds st heldSrc
    source st lateSrc 10000
    (credited, refundedLate) <- together st other (\db -> creditLatePayment db (lateCredit lateSrc 1 (Just 50000) 3000) t0) (\db -> refundBalance db (RegistrationId lateSrc) 10000 t0)
    credited `shouldBe` True
    (n,) <$> figures st lateSrc t0 `shouldReturn` (n, Just $ if refundedLate then (3000, 13000, 10000, Nothing) else (13000, 13000, 0, Nothing))
    invariantHolds st lateSrc
#endif
