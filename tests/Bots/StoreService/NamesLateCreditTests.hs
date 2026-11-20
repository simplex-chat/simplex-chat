{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesLateCreditTests (namesLateCreditTests) where

import Bots.StoreService.NamesBalancesTests (coveredSpend, figures, holder, invariantHolds, placeOrderRow, source)
import Bots.StoreService.NamesStoreTests (at, ownerA, reg, t0)
import Bots.StoreService.WebTests (StubCall (..), StubState (..), capturingStderr, newStubState, renameTable, sampleProviderInvoice, setSignals, stubCalls, stubProvider, stubRate, withServiceStore)
import qualified Control.Exception as E
import Control.Monad (void)
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Names.Record (USDCents)
import StoreService.Config (PollConfig (..))
import StoreService.Names.Balances (recordRefund)
import StoreService.Names.LateCredits (CreditOutcome (..), LateDue (..), creditLateInvoice, lateCreditOf)
import StoreService.Names.Registration (RegistrationId (..))
import StoreService.Names.Store
import StoreService.Names.Turns (newTurns)
import StoreService.Names.View (balanceView)
import StoreService.Orders (cancelOpenInvoice, settleOrder)
import Control.Concurrent.STM (readTVarIO)
import StoreService.Poller (PollerEnv (..), ReadHints, cleanupUnpaid, creditPass, drainHints, forgetRates, newPollerEnv, newReadHints, queueReadHint, runOnePass)
import StoreService.Providers (Confirmation (..), CryptoReceived (..), Funded (..), PaymentSignal (..), Provider (..), ProviderError (..), Rate (..), Received (..), Uncreditable (..), settleWindow)
import StoreService.Store.Invoices (unpaidRefs)
import StoreService.Waiters (Waiters, newWaiters)
import Test.Hspec

#if defined(dbPostgres)
import Bots.StoreService.WebTests (withServiceStores)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar)
import Control.Monad (forM, forM_)
import Data.Maybe (fromMaybe, isJust)
import StoreService.Store.Invoices (lockInvoice)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

namesLateCreditTests :: Spec
namesLateCreditTests = describe "StoreService names: balance life" $ do
  describe "settlement" $ do
    it "queues a difference order funded before its expiry and settled after it, still open in the database, spending its hold, the source's amount down by fromBalance" testFundedThenSettledLate
    it "queues an order whose hold was removed by hand and alerts naming the order and the balance's order" testHoldNotSpent
    it "closes an order meeting another live order at settlement, alerts, and credits its payment after" testLiveOrderExists
    it "releases a difference order's hold in that settlement's transaction and credits its own balance after, by the credit step" testLiveOrderExistsHolder
  describe "late credits" $ do
    it "credits a late payment to the closed order's balance at the lower rate, the released source's balance untouched, logged at info" testLateCredited
    it "credits a payment confirming over two reports by its increase, and a repeated report once" testGrowingReports
    it "credits a confirmed part payment the provider's close meets, and a cancel's at the pass, the cancel reading no rate" testCloseAndCancel
    it "credits nothing for unconfirmed money, logged as pending, then once when it reads Settled with the totals unchanged" testUnconfirmedThenSettled
    it "never credits a replaced or dropped payment, alerting it once past the window from its payment row, never again" testNeverConfirmed
    it "alerts a part credit with an unconfirmed rest once, naming its units" testPartCreditAlerted
    it "logs Monero's zero figure as pending and does not alert it within the window" testMoneroZero
    it "credits nothing while the rate read fails, warning once, then credits at the next pass" testRateFails
    it "logs a credit pass that fails, so a pass failing from the start keeps the start's time past the alert's age, until a pass completes" testCreditPassFails
    it "credits nothing below the rate floor and never after, logging an error once while the price moves below it, and alerts support naming both rates" testRateFloor
    it "alerts confirmed money waiting on the rate read as such, whatever reason was stored before" testWaitingOutranksStoredReason
    it "warns once of an invoice no provider reads a rate for, and names an invoice with no recorded rate" testRateProblemsNamed
    it "reads one rate per coin per pass for several invoices, crediting a late Settled invoice and settlements whose credit step did not run, and none when nothing is due" testOneRateReadPerPass
    it "never credits a late Settled payment on an invoice made under HighSpeed, alerting it past the window" testHighSpeed
    it "refuses to credit an order that is live again by the time it takes the invoice's lock" testLiveAgain
    it "alerts once of two instances, and again when more money arrives" testTwoInstancesAlert
    it "keeps the confirmed figure and the credit when the settled sum falls, crediting nothing on the same sum again" testFallingSum
    it "credits an invoice past the read window after a hinted read" testHintPastWindow
    it "credits Stripe's succeeded amount without reading a rate" testStripeCredited
    it "credits late XMR in piconero when the plugin shows a confirmation, and alerts it to support when it shows none" testXmrLate
  describe "price share" $ do
    it "credits the share of the invoice's amount at the lower rate, so a network fee is never credited, and nothing without invoiced units" testLateCreditOf
  describe "overpayment" $ do
    it "credits the extra of an invoice overpaid in time to its order's balance, logged, once, and nothing for an exact or card payment" testOverpaidInTime
    it "credits the extra at the next pass when its credit step could not read the rate" testOverpaidByPass
    it "alerts the extra of an overpaid HighSpeed invoice to support, naming the invoiced units, and nothing for an exact one" testOverpaidHighSpeed
    it "alerts money a paid invoice receives after its settlement once an hour, crediting none of it" testMoneyAfterPaid
  describe "balances with late credits" $ do
    it "keeps a credit made while the balance is held, the held spend settling" testCreditWhileHeld
    it "sums the refunds over credit, refund, late credit, refund" testCreditRefundTwice
  describe "the view" $ do
    it "shows a funded holder past its expiry held with no end time, and an unfunded one before it with its expiry" testHeldUntil
    it "shows a balance spent to 0 and a refunded one as objects with amount 0" testZeroBalances
    it "waits confirming, crediting, or support for each reason, on nothing once all is credited, and on support for good below the rate floor" testLateWaits
    it "finds the order a balance paid and the order holding a balance, none for an overdue unfunded holder" testFindLinks
#if defined(dbPostgres)
  describe "two connections" $ do
    it "credits once of two credit steps on one invoice, cleanly, where a step reading before the lock is refused" testConcurrentCredits
    it "credits the confirmed units once against a settlement raising them" testCreditAgainstSettlement
#endif

-- fixtures

data Late = Late
  { lStore :: DBStore,
    lStub :: IORef StubState,
    lPoller :: PollerEnv,
    lHints :: ReadHints,
    lWaiters :: Waiters
  }

withLate :: (Late -> IO a) -> IO a
withLate action = withServiceStore (`withLateOn` action)

-- | Another poller over the store, as another instance or a restarted service runs.
withLateOn :: DBStore -> (Late -> IO a) -> IO a
withLateOn lStore action = do
  lStub <- newIORef (newStubState (Right sampleProviderInvoice))
  lWaiters <- newWaiters
  lHints <- newReadHints
  let crypto = stubProvider lStub
      card = crypto {pProvider = PPStripe, pCurrentRate = Nothing}
  lPoller <- newPollerEnv lStore lWaiters Nothing lHints [crypto, card] PollConfig {pWaitingSeconds = 1, pIdleSeconds = 1}
  action Late {lStore, lStub, lPoller, lHints, lWaiters}

-- | The invoices' rate, 62500 dollars a bitcoin, is 0.0625 cents a satoshi.
satCents :: Int64 -> Int
satCents units = fromIntegral (units * 625 `div` 10000)

-- | Bitcoin received and confirmed, in satoshi, at the invoices' rate.
btcPaid :: Int64 -> Int64 -> Received
btcPaid units confirmed =
  RCrypto
    CryptoReceived
      { crAmount = CurrencyAmount (fromIntegral $ satCents units),
        crPaid = Just (T.pack $ show units),
        crDue = Nothing,
        crUnits = Just units,
        crInvoicedUnits = Just lateUnits,
        crRate = rateOf "62500" 62500,
        crConfirmation = CryptoConfirmed confirmed (CurrencyAmount (fromIntegral $ satCents confirmed))
      }

cardPaid :: Int -> Int -> Received
cardPaid cents confirmed = RCard {rcAmount = CurrencyAmount (fromIntegral cents), rcConfirmed = CurrencyAmount (fromIntegral confirmed)}

nothingPaid :: Received
nothingPaid = cardPaid 0 0

-- | Bitcoin received and confirmed on an invoice that asked the given satoshi.
btcPaidOf :: Int64 -> Int64 -> Int64 -> Received
btcPaidOf invoiced units confirmed = withCrypto (\c -> c {crInvoicedUnits = Just invoiced}) (btcPaid units confirmed)

withCrypto :: (CryptoReceived -> CryptoReceived) -> Received -> Received
withCrypto f = \case
  RCrypto c -> RCrypto (f c)
  card -> card

markUncreditable :: Uncreditable -> Received -> Received
markUncreditable reason = withCrypto (\c -> c {crConfirmation = Uncreditable reason})

-- | 320000 satoshi: 20000 cents at the invoice's rate, 19200 at the stub's current rate of 60000.5.
lateUnits :: Int64
lateUnits = 320000

invOf :: Text -> InvoiceId
invOf r = InvoiceId ("inv-" <> r)

refOf :: Text -> Text
refOf r = "ref-inv-" <> r

setRate :: Late -> Either ProviderError Rate -> IO ()
setRate Late {lStub} r = atomicModifyIORef' lStub $ \s -> (s {ssRate = r}, ())

rateOf :: Text -> Rational -> Rate
rateOf t v = Rate {rateText = t, rateValue = fromRational v}

-- | An unpaid order of 20000 cents, crypto unless card, whose invoice the provider closed with nothing received.
closedOrder :: Late -> Text -> Bool -> IO ()
closedOrder l@Late {lStore} r card = do
  placeOrderRow lStore r card 20000 (at 60)
  settleAt l r (SigClosed nothingPaid PaidInPart) (at 61) `shouldReturn` ISExpired
  nrPhase <$> reg lStore r `shouldReturn` OClosed ICExpired

-- | The provider reports the signal for the order's invoice, and the poller reads it on a hint, as a webhook asks; the logs.
readSignal :: Late -> Text -> PaymentSignal -> IO Text
readSignal Late {lStub, lHints, lPoller} r signal = do
  setSignals lStub [(refOf r, signal)]
  capturingStderr $ void (queueReadHint lHints (refOf r)) >> drainHints lPoller

-- | Settled straight in the store at the given time, with no credit step after it.
settleAt :: Late -> Text -> PaymentSignal -> UTCTime -> IO InvoiceStatus
settleAt Late {lStore, lWaiters} r = settleOrder lStore lWaiters Nothing (invOf r)

-- | A new pass's credits; the logs.
passAt :: Late -> UTCTime -> IO Text
passAt Late {lPoller} now = capturingStderr $ forgetRates lPoller >> creditPass lPoller now

-- | The daily alerts of a newly started instance at the given time, with its rates; the logs.
alertsAt :: Late -> Either ProviderError Rate -> UTCTime -> IO Text
alertsAt Late {lStore} r now = withLateOn lStore $ \l@Late {lPoller} -> setRate l r >> capturingStderr (cleanupUnpaid lPoller now)

-- | Amount, credited and refunded of the order's balance.
balanceOf :: DBStore -> Text -> IO (Maybe (USDCents, USDCents, USDCents))
balanceOf st r = fmap (\(amount, credited, refunded, _) -> (amount, credited, refunded)) <$> figures st r t0

-- | Units, the invoice's, current and used rates, and cents of each credit of the order's invoice.
creditsOf :: DBStore -> Text -> IO [(Maybe Int64, Maybe Text, Maybe Text, Maybe Text, Int)]
creditsOf st r =
  withTransaction st $ \db ->
    DB.query db "SELECT units, invoice_rate, current_rate, rate_used, cents FROM sx_badge_service_name_late_credits WHERE invoice_id = ? ORDER BY credit_no" (Only $ "inv-" <> r)

rateCalls :: Late -> IO Int
rateCalls Late {lStub} = length . filter isRate <$> stubCalls lStub
  where
    isRate = \case
      StubRate _ -> True
      _ -> False

lateWaitOf :: DBStore -> Text -> IO (Maybe LateWait)
lateWaitOf st r = withTransaction st $ \db -> lateWait db (invOf r)

-- | A log line of the level holding the text.
logged :: Text -> Text -> Text -> Bool
logged level needle = any (\ln -> ("[" <> level <> " ") `T.isPrefixOf` ln && needle `T.isInfixOf` ln) . T.lines

count :: Text -> Text -> Int
count needle = length . T.breakOnAll needle

-- | The payment rows' created_at, past the read window by the seconds given.
pastWindow :: UTCTime -> Integer -> UTCTime
pastWindow createdAt by = addUTCTime (settleWindow + fromInteger by) createdAt

-- settlement

testFundedThenSettledLate :: IO ()
testFundedThenSettledLate = withLate $ \l@Late {lStore = st} -> do
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  settleAt l "h" (SigFunded (btcPaidOf 480000 480000 0) PaidInFull) (at 50) `shouldReturn` ISOpen
  settleAt l "h" (SigSettled (btcPaidOf 480000 480000 480000) (at 120)) (at 120) `shouldReturn` ISPaid
  NameRegistration {nrPhase, nrPayment} <- reg st "h"
  (nrPhase, buUsed <$> paymentBalance nrPayment) `shouldBe` (OQueued, Just 10000)
  figures st "s" (at 120) `shouldReturn` Just (0, 10000, 0, Nothing)
  invariantHolds st "s"

testHoldNotSpent :: IO ()
testHoldNotSpent = withLate $ \l@Late {lStore = st} -> do
  source st "unspentsrc" 10000
  holder st "unspent" "unspentsrc" 10000 (at 60) `shouldReturn` Right ()
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_balances SET reserved_by = NULL WHERE registration_id = ?" (Only ("unspentsrc" :: Text))
  logs <- capturingStderr $ settleAt l "unspent" (SigSettled (btcPaidOf 480000 480000 480000) (at 10)) (at 10) `shouldReturn` ISPaid
  logs `shouldSatisfy` logged "ERROR" "invoice ref-inv-unspent queued its order as paid, but the balance of registration unspentsrc it held could not be spent"
  nrPhase <$> reg st "unspent" `shouldReturn` OQueued
  balanceOf st "unspentsrc" `shouldReturn` Just (10000, 10000, 0)

-- | The unique index keeping one live order per commitment is dropped, so a second live order can meet the first at settlement.
meetsLiveOrder :: DBStore -> Text -> IO ()
meetsLiveOrder st r = do
  withTransaction st $ \db -> DB.execute_ db "DROP INDEX sx_badge_service_idx_name_registrations_live_commitment"
  NameRegistration {nrCommitment} <- reg st r
  placeOrderRow st "twin" False 20000 (at 60)
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET commitment = ? WHERE registration_id = 'twin'" (Only nrCommitment)

testLiveOrderExists :: IO ()
testLiveOrderExists = withLate $ \l@Late {lStore = st} -> do
  placeOrderRow st "a" False 20000 (at 60)
  meetsLiveOrder st "a"
  logs <- readSignal l "a" (SigSettled (btcPaid lateUnits lateUnits) (at 10))
  logs `shouldSatisfy` logged "ERROR" "invoice ref-inv-a was paid while another live order held its order's commitment, or its owner and name"
  nrPhase <$> reg st "a" `shouldReturn` OClosed ICExpired
  balanceOf st "a" `shouldReturn` Just (19200, 19200, 0)

testLiveOrderExistsHolder :: IO ()
testLiveOrderExistsHolder = withLate $ \l@Late {lStore = st} -> do
  source st "s" 10000
  holder st "aheld" "s" 10000 (at 60) `shouldReturn` Right ()
  meetsLiveOrder st "aheld"
  setRate l (Left $ ProviderError "rates down")
  void $ readSignal l "aheld" (SigSettled (btcPaidOf 480000 lateUnits lateUnits) (at 10))
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  balanceOf st "aheld" `shouldReturn` Nothing
  setRate l (Right stubRate)
  void $ passAt l (at 20)
  balanceOf st "aheld" `shouldReturn` Just (19200, 19200, 0)
  invariantHolds st "s"

-- late credits

testLateCredited :: IO ()
testLateCredited = withLate $ \l@Late {lStore = st} -> do
  source st "paid" 10000
  holder st "credited" "paid" 10000 (at 60) `shouldReturn` Right ()
  settleAt l "credited" (SigClosed nothingPaid PaidInPart) (at 61) `shouldReturn` ISExpired
  figures st "paid" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  logs <- readSignal l "credited" (SigClosed (btcPaidOf 480000 lateUnits lateUnits) PaidInPart)
  logs `shouldSatisfy` logged "NOTE" "invoice ref-inv-credited credited 19200 cents to its order's balance for 320000 units at 60000.5 (invoice 62500, current 60000.5)"
  logs `shouldNotSatisfy` logged "ERROR" "inv-credited"
  balanceOf st "credited" `shouldReturn` Just (19200, 19200, 0)
  creditsOf st "credited" `shouldReturn` [(Just lateUnits, Just "62500", Just "60000.5", Just "60000.5", 19200)]
  figures st "paid" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

testGrowingReports :: IO ()
testGrowingReports = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000) PaidInPart)
  balanceOf st "x" `shouldReturn` Just (9600, 9600, 0)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000) PaidInPart)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  map (\(units, _, _, _, cents) -> (units, cents)) <$> creditsOf st "x" `shouldReturn` [(Just 160000, 9600), (Just 160000, 9600)]
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testCloseAndCancel :: IO ()
testCloseAndCancel = withLate $ \l@Late {lStore = st, lWaiters} -> do
  placeOrderRow st "p" False 20000 (at 60)
  settleAt l "p" (SigFunded (btcPaid lateUnits 160000) PaidInPart) (at 10) `shouldReturn` ISOpen
  void $ readSignal l "p" (SigClosed (btcPaid lateUnits 160000) PaidInPart)
  orderLive <$> reg st "p" `shouldReturn` False
  balanceOf st "p" `shouldReturn` Just (9600, 9600, 0)
  placeOrderRow st "c" False 20000 (at 60)
  settleAt l "c" (SigFunded (btcPaid lateUnits lateUnits) PaidInFull) (at 10) `shouldReturn` ISOpen
  calls <- rateCalls l
  cancelOpenInvoice st lWaiters Nothing (invOf "c") (at 20) `shouldReturn` True
  rateCalls l `shouldReturn` calls
  balanceOf st "c" `shouldReturn` Nothing
  void $ passAt l (at 21)
  balanceOf st "c" `shouldReturn` Just (19200, 19200, 0)

testUnconfirmedThenSettled :: IO ()
testUnconfirmedThenSettled = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "unconfirmed" False
  logs <- readSignal l "unconfirmed" (SigClosed (btcPaid lateUnits 0) PaidInPart)
  logs `shouldSatisfy` logged "NOTE" "invoice ref-inv-unconfirmed holds money of its closed order not yet confirmed"
  balanceOf st "unconfirmed" `shouldReturn` Nothing
  void $ readSignal l "unconfirmed" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  void $ readSignal l "unconfirmed" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  length <$> creditsOf st "unconfirmed" `shouldReturn` 1
  balanceOf st "unconfirmed" `shouldReturn` Just (19200, 19200, 0)

-- | The order was placed long before the payment, whose own row the window counts from.
testNeverConfirmed :: IO ()
testNeverConfirmed = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "never" False
  let seenAt = at (60 * 24 * 5)
      alert = "invoice ref-inv-never: 320000 satoshi received, 0 satoshi confirmed, 0 satoshi credited; refund by hand only a transaction confirmed on chain that BTCPay no longer monitors; one still Processing there waits"
  settleAt l "never" (SigClosed (btcPaid lateUnits 0) PaidInPart) seenAt `shouldReturn` ISExpired
  alertsAt l (Right stubRate) (pastWindow seenAt (-60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice ref-inv-never:")
  lateWaitOf st "never" `shouldReturn` Just LWConfirming
  logs <- alertsAt l (Right stubRate) (pastWindow seenAt 60)
  (count alert logs, logged "ERROR" alert logs) `shouldBe` (1, True)
  lateWaitOf st "never" `shouldReturn` Just LWSupport
  alertsAt l (Right stubRate) (pastWindow seenAt (24 * 3600 + 60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice ref-inv-never:")
  balanceOf st "never" `shouldReturn` Nothing

testPartCreditAlerted :: IO ()
testPartCreditAlerted = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "part" False
  settleAt l "part" (SigClosed (btcPaid lateUnits 160000) PaidInPart) (at 100) `shouldReturn` ISExpired
  void $ passAt l (at 101)
  balanceOf st "part" `shouldReturn` Just (9600, 9600, 0)
  logs <- alertsAt l (Right stubRate) (pastWindow (at 100) 60)
  count "invoice ref-inv-part: 320000 satoshi received, 160000 satoshi confirmed, 160000 satoshi credited; refund by hand only" logs `shouldBe` 1
  alertsAt l (Right stubRate) (pastWindow (at 100) (24 * 3600 + 60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice ref-inv-part:")

testMoneroZero :: IO ()
testMoneroZero = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "monero" False
  logs <- readSignal l "monero" (SigFunded (withCrypto (\c -> c {crPaid = Nothing}) (btcPaid 0 0)) PaidInFull)
  logs `shouldSatisfy` logged "NOTE" "invoice ref-inv-monero holds money of its closed order not yet confirmed"
  withTransaction st (\db -> latePending db (invOf "monero")) `shouldReturn` Just LPConfirming
  now <- getCurrentTime
  alertsAt l (Right stubRate) (addUTCTime 3600 now) >>= (`shouldNotSatisfy` T.isInfixOf "invoice ref-inv-monero:")
  balanceOf st "monero" `shouldReturn` Nothing

testCreditPassFails :: IO ()
testCreditPassFails = withLate $ \Late {lStore = st, lPoller} -> do
  started <- readTVarIO (peCreditPassAt lPoller)
  renameTable st "sx_badge_service_name_invoices" "name_invoices_away"
  passLog <- capturingStderr $ runOnePass lPoller
  passLog `shouldSatisfy` T.isInfixOf "names: the late credit pass failed; the next pass will run: "
  let pastAlert = addUTCTime 601 started
  (E.try (creditPass lPoller pastAlert) :: IO (Either E.SomeException ())) >>= (`shouldSatisfy` either (const True) (const False))
  renameTable st "name_invoices_away" "sx_badge_service_name_invoices"
  readTVarIO (peCreditPassAt lPoller) `shouldReturn` started
  diffUTCTime pastAlert <$> readTVarIO (peCreditPassAt lPoller) >>= (`shouldSatisfy` (> 600))
  let later = addUTCTime 2 pastAlert
  creditPass lPoller later
  readTVarIO (peCreditPassAt lPoller) `shouldReturn` later

testRateFails :: IO ()
testRateFails = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "fail1" False
  closedOrder l "fail2" False
  setRate l (Left $ ProviderError "rates down for fail1")
  logs <- readSignal l "fail1" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  settleAt l "fail2" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  more <- passAt l (at 71)
  count "late credits wait, as the current rate cannot be read; each pass tries again: rates down for fail1" (logs <> more) `shouldBe` 1
  lateWaitOf st "fail1" `shouldReturn` Just LWCrediting
  mapM (balanceOf st) ["fail1", "fail2"] `shouldReturn` [Nothing, Nothing]
  setRate l (Right stubRate)
  void $ passAt l (at 72)
  mapM (balanceOf st) ["fail1", "fail2"] `shouldReturn` replicate 2 (Just (19200, 19200, 0))
  lateWaitOf st "fail1" `shouldReturn` Nothing

testRateFloor :: IO ()
testRateFloor = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "floored" False
  let floored = rateOf "31000" 31000
  setRate l (Right floored)
  settleAt l "floored" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  logs <- passAt l (at 71)
  logs `shouldSatisfy` logged "ERROR" "invoice ref-inv-floored is not credited: the current rate 31000 is below half its rate 62500"
  setRate l (Right $ rateOf "30000" 30000)
  passAt l (at 72) >>= (`shouldNotSatisfy` logged "ERROR" "invoice ref-inv-floored is not credited")
  balanceOf st "floored" `shouldReturn` Nothing
  lateWaitOf st "floored" `shouldReturn` Just LWSupport
  alertsAt l (Right floored) (pastWindow (at 70) 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice ref-inv-floored: 320000 satoshi received, 320000 satoshi confirmed, 0 satoshi credited; confirmed; the coin's price fell by more than half since the invoice (62500, 31000); it is never credited by itself; refund by hand only a transaction confirmed on chain")
  setRate l (Right stubRate)
  void $ passAt l (at 73)
  creditsOf st "floored" `shouldReturn` []
  withTransaction st uncreditedLateInvoices >>= (`shouldNotSatisfy` elem (invOf "floored", refOf "floored"))
  lateWaitOf st "floored" `shouldReturn` Just LWSupport

-- | A reason stored by an earlier read, as one without the invoice's policy would store HighSpeed.
testWaitingOutranksStoredReason :: IO ()
testWaitingOutranksStoredReason = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "outranked" False
  withTransaction st $ \db -> setLateSupport db (invOf "outranked") (LSUncreditable UCHighSpeed)
  settleAt l "outranked" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  alertsAt l (Left $ ProviderError "rates down for outranked") (pastWindow (at 70) 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice ref-inv-outranked: 320000 satoshi received, 320000 satoshi confirmed, 0 satoshi credited; confirmed, the credit waits on the rate read; do not refund; fix the rate read")

testRateProblemsNamed :: IO ()
testRateProblemsNamed = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "noprovider" False
  settleAt l "noprovider" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  withTransaction st $ \db -> DB.execute_ db "UPDATE sx_badge_service_invoices SET provider = 'apple' WHERE invoice_id = 'inv-noprovider'"
  closedOrder l "norate" False
  settleAt l "norate" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  withTransaction st $ \db -> DB.execute_ db "UPDATE sx_badge_service_payments SET invoice_rate = NULL WHERE invoice_id = 'inv-norate'"
  logs <- (<>) <$> passAt l (at 71) <*> passAt l (at 72)
  count "invoice ref-inv-noprovider has late money to credit, but no provider" logs `shouldBe` 1
  logs `shouldSatisfy` logged "ERROR" "invoice ref-inv-norate has confirmed late money, but no invoice rate or invoiced units were recorded to credit it at"

testOneRateReadPerPass :: IO ()
testOneRateReadPerPass = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> closedOrder l r False >> settleAt l r (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70)) ["x", "y"]
  closedOrder l "z" False
  settleAt l "z" (SigSettled (btcPaid lateUnits lateUnits) (at 70)) (at 70) `shouldReturn` ISPaid
  closedOrder l "card" True
  settleAt l "card" (SigSettled (cardPaid 20000 20000) (at 70)) (at 70) `shouldReturn` ISPaid
  mapM (balanceOf st) ["x", "y", "z"] `shouldReturn` replicate 3 Nothing
  void $ passAt l (at 71)
  rateCalls l `shouldReturn` 1
  mapM (balanceOf st) ["x", "y", "z", "card"] `shouldReturn` replicate 3 (Just (19200, 19200, 0)) <> [Just (20000, 20000, 0)]
  void $ passAt l (at 72)
  rateCalls l `shouldReturn` 1

testHighSpeed :: IO ()
testHighSpeed = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "hs" False
  let highSpeed = markUncreditable UCHighSpeed (btcPaid lateUnits 0)
  settleAt l "hs" (SigSettled highSpeed (at 70)) (at 70) `shouldReturn` ISPaid
  void $ passAt l (at 71)
  balanceOf st "hs" `shouldReturn` Nothing
  lateWaitOf st "hs" `shouldReturn` Just LWSupport
  alertsAt l (Right stubRate) (pastWindow (at 70) 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice ref-inv-hs: 320000 satoshi received, 0 satoshi confirmed, 0 satoshi credited; the invoice was made under HighSpeed")

testLiveAgain :: IO ()
testLiveAgain = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  settleAt l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  let liveAgain _ = do
        withTransaction st $ \db -> DB.execute_ db "UPDATE sx_badge_service_name_registrations SET phase = 'unpaid', closed_by = NULL, live = 1 WHERE registration_id = 'x'"
        pure (Right stubRate)
  creditLateInvoice st liveAgain (invOf "x") (at 71) `shouldReturn` CONothingDue
  balanceOf st "x" `shouldReturn` Nothing
  creditsOf st "x" `shouldReturn` []

testTwoInstancesAlert :: IO ()
testTwoInstancesAlert = withLate $ \l -> do
  closedOrder l "twice" False
  settleAt l "twice" (SigClosed (btcPaid lateUnits 0) PaidInPart) (at 70) `shouldReturn` ISExpired
  let first = "invoice ref-inv-twice: 320000 satoshi received"
  logs <- (<>) <$> alertsAt l (Right stubRate) (pastWindow (at 70) 60) <*> alertsAt l (Right stubRate) (pastWindow (at 70) 60)
  count first logs `shouldBe` 1
  settleAt l "twice" (SigClosed (btcPaid 400000 0) PaidInPart) (at 80) `shouldReturn` ISExpired
  alertsAt l (Right stubRate) (pastWindow (at 70) 120) >>= (`shouldSatisfy` logged "ERROR" "invoice ref-inv-twice: 400000 satoshi received, 0 satoshi confirmed")

testFallingSum :: IO ()
testFallingSum = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000) PaidInPart)
  confirmed <- withTransaction st $ \db -> DB.query db "SELECT confirmed_units FROM sx_badge_service_payments WHERE invoice_id = 'inv-x'" ()
  confirmed `shouldBe` [Only (Just lateUnits)]
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  length <$> creditsOf st "x" `shouldReturn` 1
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testHintPastWindow :: IO ()
testHintPastWindow = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  now <- getCurrentTime
  let longAgo = addUTCTime (negate $ settleWindow + 3600) now
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_invoices SET created_at = ? WHERE invoice_id = 'inv-x'" (Only longAgo)
  snd <$> unpaidRefs st (addUTCTime (negate settleWindow) now) >>= (`shouldNotSatisfy` elem (PPCrypto, refOf "x"))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testStripeCredited :: IO ()
testStripeCredited = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "c" True
  void $ readSignal l "c" (SigSettled (cardPaid 20000 20000) (at 70))
  creditsOf st "c" `shouldReturn` [(Nothing, Nothing, Nothing, Nothing, 20000)]
  balanceOf st "c" `shouldReturn` Just (20000, 20000, 0)
  rateCalls l `shouldReturn` 0

-- | 0.32095 XMR at the invoice's rate of 168.25, all of it Settled; uncreditable unless the plugin shows a confirmation.
xmrPaid :: Maybe Uncreditable -> Received
xmrPaid uncreditable =
  RCrypto
    CryptoReceived
      { crAmount = CurrencyAmount 5400,
        crPaid = Just "0.32095000",
        crDue = Nothing,
        crUnits = Just xmrUnits,
        crInvoicedUnits = Just xmrUnits,
        crRate = rateOf "168.25" 168.25,
        crConfirmation = maybe (CryptoConfirmed xmrUnits (CurrencyAmount 5400)) Uncreditable uncreditable
      }
  where
    xmrUnits = 320950000000

testXmrLate :: IO ()
testXmrLate = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> closedOrder l r False) ["xmrshown", "xmrnone"]
  withTransaction st $ \db -> DB.execute_ db "UPDATE sx_badge_service_invoices SET payment_crypto_currency = 'xmr', amount = 5400 WHERE invoice_id IN ('inv-xmrshown', 'inv-xmrnone')"
  setRate l (Right $ rateOf "150" 150)
  void $ readSignal l "xmrshown" (SigSettled (xmrPaid Nothing) (at 70))
  creditsOf st "xmrshown" `shouldReturn` [(Just 320950000000, Just "168.25", Just "150", Just "150", 4814)]
  void $ readSignal l "xmrnone" (SigSettled (xmrPaid $ Just UCNoConfirmationShown) (at 70))
  balanceOf st "xmrnone" `shouldReturn` Nothing
  lateWaitOf st "xmrnone" `shouldReturn` Just LWSupport
  now <- getCurrentTime
  alertsAt l (Right stubRate) (pastWindow now 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice ref-inv-xmrnone: 320950000000 piconero received, 0 piconero confirmed, 0 piconero credited; the Monero plugin shows no confirmation of this money, so it is never credited by itself; refund by hand only a transaction confirmed on chain")

-- price share

-- | The review's $100 Monero invoice is 0.50211283 XMR at a rate of 200, 0.00211283 of it BTCPay's network fee.
testLateCreditOf :: IO ()
testLateCreditOf = do
  let closed coin rate amount invoiced confirmed =
        LateCreditState
          { lsInvoice = invOf "x",
            lsRegistration = RegistrationId "x",
            lsMoney = LMLate,
            lsClosed = True,
            lsSupport = Nothing,
            lsReceivedCents = 0,
            lsFigures =
              LFCrypto
                { lfCoin = coin,
                  lfReceivedUnits = Just confirmed,
                  lfConfirmedUnits = Just confirmed,
                  lfInvoicedUnits = invoiced,
                  lfInvoiceRate = Just rate,
                  lfCreditedUnits = 0
                },
            lsInvoiceAmount = amount,
            lsCreditedCents = 0,
            lsCredits = 0,
            lsPaymentCreatedAt = at 70
          }
      creditOf s rate = case lateCreditOf s (Just rate) of
        LDCredit LateCredit {lcAmount} -> Just (cryptoCredit lcAmount)
        _ -> Nothing
      cryptoCredit = \case
        CACrypto {caUnits, caInvoiceRate, caCurrentRate, caRateUsed, caCents} -> Just (caUnits, rateText caInvoiceRate, rateText caCurrentRate, rateText caRateUsed, caCents)
        CACard _ -> Nothing
      btc = closed CCBtc (rateOf "62500" 62500) 20000
  creditOf (btc (Just lateUnits) lateUnits) (rateOf "70000" 70000) `shouldBe` Just (Just (lateUnits, "62500", "70000", "62500", 20000))
  creditOf (btc (Just 330000) 330000) (rateOf "62500" 62500) `shouldBe` Just (Just (330000, "62500", "62500", "62500", 20000))
  creditOf (btc (Just lateUnits) 480000) stubRate `shouldBe` Just (Just (480000, "62500", "60000.5", "60000.5", 28800))
  creditOf (closed CCXmr (rateOf "200" 200) 10000 (Just 502112830000) 502112830000) (rateOf "200" 200) `shouldBe` Just (Just (502112830000, "200", "200", "200", 10000))
  creditOf (btc Nothing lateUnits) stubRate `shouldBe` Nothing

-- overpayment

-- | 480000 satoshi on an invoice of 320000: the extra 160000 is 10000 cents of the price, 9600 at the stub's current rate.
testOverpaidInTime :: IO ()
testOverpaidInTime = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\(r, card) -> placeOrderRow st r card 20000 (at 60)) [("over", False), ("exact", False), ("card", True)]
  logs <- readSignal l "over" (SigSettled (btcPaidOf lateUnits 480000 480000) (at 10))
  logs `shouldSatisfy` logged "NOTE" "invoice ref-inv-over was overpaid; credited 9600 cents of the extra to its order's balance for 160000 units at 60000.5 (invoice 62500, current 60000.5)"
  nrPhase <$> reg st "over" `shouldReturn` OQueued
  creditsOf st "over" `shouldReturn` [(Just 160000, Just "62500", Just "60000.5", Just "60000.5", 9600)]
  balanceOf st "over" `shouldReturn` Just (9600, 9600, 0)
  void $ passAt l (at 20)
  void $ readSignal l "over" (SigSettled (btcPaidOf lateUnits 480000 480000) (at 10))
  length <$> creditsOf st "over" `shouldReturn` 1
  void $ readSignal l "exact" (SigSettled (btcPaidOf lateUnits lateUnits lateUnits) (at 10))
  void $ readSignal l "card" (SigSettled (cardPaid 20000 20000) (at 10))
  void $ passAt l (at 21)
  mapM (fmap nrPhase . reg st) ["exact", "card"] `shouldReturn` [OQueued, OQueued]
  mapM (balanceOf st) ["exact", "card"] `shouldReturn` [Nothing, Nothing]

testOverpaidByPass :: IO ()
testOverpaidByPass = withLate $ \l@Late {lStore = st} -> do
  placeOrderRow st "over" False 20000 (at 60)
  setRate l (Left $ ProviderError "rates down")
  void $ readSignal l "over" (SigSettled (btcPaidOf lateUnits 480000 480000) (at 10))
  balanceOf st "over" `shouldReturn` Nothing
  withTransaction st uncreditedLateInvoices `shouldReturn` [(invOf "over", refOf "over")]
  setRate l (Right stubRate)
  void $ passAt l (at 20)
  balanceOf st "over" `shouldReturn` Just (9600, 9600, 0)
  withTransaction st uncreditedLateInvoices `shouldReturn` []

testMoneyAfterPaid :: IO ()
testMoneyAfterPaid = withLate $ \l@Late {lStore = st} -> do
  placeOrderRow st "paid" False 20000 (at 60)
  let paid units = SigSettled (btcPaidOf lateUnits units units) (at 10)
      alerted = logged "ERROR" "invoice ref-inv-paid was paid, and its provider now reads 480000 received, more than recorded"
  void $ readSignal l "paid" (paid lateUnits)
  readSignal l "paid" (paid lateUnits) >>= (`shouldNotSatisfy` logged "ERROR" "invoice ref-inv-paid was paid, and its provider")
  readSignal l "paid" (paid 480000) >>= (`shouldSatisfy` alerted)
  readSignal l "paid" (paid 480000) >>= (`shouldNotSatisfy` alerted)
  void $ passAt l (at 20)
  balanceOf st "paid" `shouldReturn` Nothing

testOverpaidHighSpeed :: IO ()
testOverpaidHighSpeed = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> placeOrderRow st r False 20000 (at 60)) ["hsover", "hsexact"]
  let highSpeed units = markUncreditable UCHighSpeed (btcPaidOf lateUnits units 0)
  settleAt l "hsover" (SigSettled (highSpeed 480000) (at 70)) (at 70) `shouldReturn` ISPaid
  settleAt l "hsexact" (SigSettled (highSpeed lateUnits) (at 70)) (at 70) `shouldReturn` ISPaid
  void $ passAt l (at 71)
  mapM (balanceOf st) ["hsover", "hsexact"] `shouldReturn` [Nothing, Nothing]
  logs <- alertsAt l (Right stubRate) (pastWindow (at 70) 60)
  logs `shouldSatisfy` logged "ERROR" "invoice ref-inv-hsover, overpaid: 320000 satoshi invoiced, 480000 satoshi received, 0 satoshi confirmed, 0 satoshi credited; the invoice was made under HighSpeed"
  logs `shouldNotSatisfy` T.isInfixOf "inv-hsexact"

-- balances with late credits

testCreditWhileHeld :: IO ()
testCreditWhileHeld = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  holder st "h" "x" 19200 (at 60) `shouldReturn` Right ()
  void $ readSignal l "x" (SigClosed (btcPaid 400000 400000) PaidInPart)
  figures st "x" (at 10) `shouldReturn` Just (24000, 24000, 0, Just BalanceHold {bhHolder = RegistrationId "h", bhState = HSOpen (at 60)})
  settleAt l "h" (SigSettled (btcPaidOf 332800 330000 330000) (at 10)) (at 10) `shouldReturn` ISPaid
  figures st "x" (at 10) `shouldReturn` Just (4800, 24000, 0, Nothing)
  invariantHolds st "x"

testCreditRefundTwice :: IO ()
testCreditRefundTwice = withLate $ \l@Late {lStore = st} -> do
  turns <- newTurns
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000) PaidInPart)
  void (recordRefund turns st "x" 9600 (at 80) >>= either (fail . show) pure)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  void (recordRefund turns st "inv-x" 9600 (at 81) >>= either (fail . show) pure)
  balanceOf st "x" `shouldReturn` Just (0, 19200, 19200)
  invariantHolds st "x"

-- the view

balanceAt :: DBStore -> Text -> UTCTime -> IO (Maybe J.Value)
balanceAt st r now = fmap (J.toJSON . balanceView) <$> withTransaction st (\db -> getBalance db (RegistrationId r) now)

balanceObject :: Int -> Maybe J.Value -> Int -> J.Value
balanceObject amount hold refunded = J.object ["amount" J..= amount, "hold" J..= hold, "refunded" J..= refunded]

openHold :: UTCTime -> Maybe J.Value
openHold t = Just $ J.object ["type" J..= ("open" :: Text), "heldUntil" J..= t]

fundedHold :: Maybe J.Value
fundedHold = Just $ J.object ["type" J..= ("funded" :: Text)]

testHeldUntil :: IO ()
testHeldUntil = withLate $ \l@Late {lStore = st} -> do
  source st "funded" 10000
  holder st "fh" "funded" 10000 (at 60) `shouldReturn` Right ()
  source st "unfunded" 10000
  holder st "uh" "unfunded" 10000 (at 60) `shouldReturn` Right ()
  balanceAt st "funded" (at 30) `shouldReturn` Just (balanceObject 10000 (openHold $ at 60) 0)
  settleAt l "fh" (SigFunded (btcPaid 100 0) PaidInPart) (at 30) `shouldReturn` ISOpen
  balanceAt st "funded" (at 30) `shouldReturn` Just (balanceObject 10000 fundedHold 0)
  balanceAt st "funded" (at 120) `shouldReturn` Just (balanceObject 10000 fundedHold 0)
  balanceAt st "unfunded" (at 30) `shouldReturn` Just (balanceObject 10000 (openHold $ at 60) 0)
  balanceAt st "unfunded" (at 120) `shouldReturn` Just (balanceObject 10000 Nothing 0)

testZeroBalances :: IO ()
testZeroBalances = withLate $ \Late {lStore = st} -> do
  turns <- newTurns
  source st "spent" 10000
  coveredSpend st "c" "spent" 10000 10000 `shouldReturn` Right ()
  balanceAt st "spent" t0 `shouldReturn` Just (balanceObject 0 Nothing 0)
  source st "refunded" 10000
  void (recordRefund turns st "refunded" 10000 (at 5) >>= either (fail . show) pure)
  balanceAt st "refunded" t0 `shouldReturn` Just (balanceObject 0 Nothing 10000)
  placeOrderRow st "none" False 20000 (at 60)
  balanceAt st "none" t0 `shouldReturn` Nothing

testLateWaits :: IO ()
testLateWaits = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> closedOrder l r False) ["confirming", "crediting", "highspeed", "units", "floor", "alerted"]
  settleAt l "crediting" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
  settleAt l "highspeed" (SigClosed (markUncreditable UCHighSpeed (btcPaid lateUnits 0)) PaidInPart) (at 70) `shouldReturn` ISExpired
  settleAt l "units" (SigClosed (withCrypto (\c -> c {crAmount = CurrencyAmount 20000, crPaid = Just "0.003200001", crUnits = Nothing, crConfirmation = Uncreditable UCUncounted}) (btcPaid 0 0)) PaidInPart) (at 70) `shouldReturn` ISExpired
  settleAt l "alerted" (SigClosed (btcPaid lateUnits 0) PaidInPart) (at 70) `shouldReturn` ISExpired
  void $ alertsAt l (Right stubRate) (pastWindow (at 70) 60)
  settleAt l "confirming" (SigClosed (btcPaid lateUnits 0) PaidInPart) (pastWindow (at 70) 60) `shouldReturn` ISExpired
  setRate l (Right $ rateOf "31000" 31000)
  void $ readSignal l "floor" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  placeOrderRow st "open" False 20000 (at 60)
  settleAt l "open" (SigFunded (btcPaid lateUnits 0) PaidInFull) (at 70) `shouldReturn` ISOpen
  let waits = mapM (\r -> (r,) <$> lateWaitOf st r)
  waits ["confirming", "crediting", "highspeed", "units", "floor", "alerted", "open"]
    `shouldReturn` [ ("confirming", Just LWConfirming),
                     ("crediting", Just LWCrediting),
                     ("highspeed", Just LWSupport),
                     ("units", Just LWSupport),
                     ("floor", Just LWSupport),
                     ("alerted", Just LWSupport),
                     ("open", Nothing)
                   ]
  setRate l (Right stubRate)
  void $ readSignal l "alerted" (SigClosed (btcPaid lateUnits lateUnits) PaidInPart)
  void $ passAt l (at 71)
  waits ["crediting", "floor", "alerted"] `shouldReturn` [("crediting", Nothing), ("floor", Just LWSupport), ("alerted", Nothing)]

testFindLinks :: IO ()
testFindLinks = withLate $ \Late {lStore = st} -> do
  source st "paying" 10000
  coveredSpend st "spend" "paying" 10000 10000 `shouldReturn` Right ()
  source st "held" 10000
  holder st "holder" "held" 10000 (at 60) `shouldReturn` Right ()
  let links now = map (\(NameRegistration {nrId = RegistrationId r, nrPayment}, heldBy') -> (r, buSource <$> paymentBalance nrPayment, heldBy')) <$> withTransaction st (\db -> findRegistrations db ownerA now)
      linked rows = filter (\(_, src, h) -> src /= Nothing || h /= Nothing) rows
  linked <$> links (at 30) `shouldReturn` [("holder", Just (RegistrationId "held"), Nothing), ("held", Nothing, Just (RegistrationId "holder")), ("spend", Just (RegistrationId "paying"), Nothing)]
  linked <$> links (at 120) `shouldReturn` [("holder", Just (RegistrationId "held"), Nothing), ("spend", Just (RegistrationId "paying"), Nothing)]

#if defined(dbPostgres)
-- | Each thread waits here until both arrive, after its unlocked read and before its locked transaction.
meet :: TVar Int -> IO ()
meet arrived = atomically (modifyTVar' arrived (+ 1)) >> atomically (readTVar arrived >>= check . (== 2))

rounds :: Int
rounds = 20

testConcurrentCredits :: IO ()
testConcurrentCredits = withServiceStores $ \open -> do
  st <- open
  other <- open
  withLateOn st $ \l -> do
    let due :: Text -> IO ()
        due r = do
          closedOrder l r False
          settleAt l r (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 70) `shouldReturn` ISExpired
    forM_ [1 .. rounds] $ \n -> do
      let r = "x" <> T.pack (show n)
      due r
      arrived <- newTVarIO 0
      let readRate :: CryptoCurrency -> IO (Either ProviderError Rate)
          readRate _ = Right stubRate <$ meet arrived
      (a, b) <- concurrently (creditLateInvoice st readRate (invOf r) (at 80)) (creditLateInvoice other readRate (invOf r) (at 80))
      (n, length [() | COCredited {} <- [a, b]], length [() | CONothingDue <- [a, b]]) `shouldBe` (n, 1, 1)
      length <$> creditsOf st r `shouldReturn` 1
      balanceOf st r `shouldReturn` Just (19200, 19200, 0)
    refused <- forM [1 .. rounds] $ \n -> do
      let r = "stale" <> T.pack (show n)
      due r
      arrived <- newTVarIO 0
      let staleStep :: DBStore -> IO Bool
          staleStep store = do
            Just s <- withTransaction store (`lateCreditState` invOf r)
            LDCredit c <- pure $ lateCreditOf s (Just stubRate)
            meet arrived
            withTransaction store $ \db -> do
              lockInvoice db (invOf r) (at 80)
              isJust <$> creditLatePayment db c (at 80)
      (a, b) <- concurrently (staleStep st) (staleStep other)
      balanceOf st r `shouldReturn` Just (19200, 19200, 0)
      pure $ length (filter not [a, b])
    sum refused `shouldSatisfy` (> 0)

testCreditAgainstSettlement :: IO ()
testCreditAgainstSettlement = withServiceStores $ \open -> do
  st <- open
  other <- open
  otherWaiters <- newWaiters
  withLateOn st $ \l -> forM_ [1 .. rounds] $ \n -> do
    let r = "x" <> T.pack (show n)
    closedOrder l r False
    settleAt l r (SigClosed (btcPaid lateUnits 160000) PaidInPart) (at 70) `shouldReturn` ISExpired
    arrived <- newTVarIO 0
    let readRate :: CryptoCurrency -> IO (Either ProviderError Rate)
        readRate _ = Right stubRate <$ meet arrived
        raise :: IO InvoiceStatus
        raise = meet arrived >> settleOrder other otherWaiters Nothing (invOf r) (SigClosed (btcPaid lateUnits lateUnits) PaidInPart) (at 80)
    (credited, raised) <- concurrently (creditLateInvoice st readRate (invOf r) (at 80)) raise
    (n, raised) `shouldBe` (n, ISExpired)
    credited `shouldSatisfy` \case COCredited {} -> True; _ -> False
    void $ creditLateInvoice st (const $ pure $ Right stubRate) (invOf r) (at 81)
    units <- map (\(u, _, _, _, _) -> u) <$> creditsOf st r
    (n, sum (map (fromMaybe 0) units)) `shouldBe` (n, lateUnits)
    balanceOf st r `shouldReturn` Just (19200, 19200, 0)
#endif
