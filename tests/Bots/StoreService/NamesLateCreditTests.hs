{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesLateCreditTests (namesLateCreditTests) where

import Bots.StoreService.NamesBalancesTests (fieldsN, figures, holder, invariantHolds, placeOrderRow, source)
import Bots.StoreService.NamesStoreTests (at, newReg, ownerA, reg, t0)
import Bots.StoreService.WebTests (StubCall (..), StubState (..), capturingStderr, newStubState, sampleProviderInvoice, setSignals, stubCalls, stubProvider, withServiceStore)
import Control.Monad (void)
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import StoreService.Config (PollConfig (..))
import StoreService.Names.Balances (RefundRefusal (..), recordRefund)
import StoreService.Names.LateCredits (CreditOutcome (..), creditLateInvoice)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store
import StoreService.Names.Turns (newTurns)
import StoreService.Names.View (balanceJSON)
import StoreService.Orders (cancelOpenInvoice, settleOrder)
import StoreService.Poller (PollerEnv, ReadHints, cleanupUnpaid, creditPass, drainHints, forgetRates, newPollerEnv, newReadHints, queueReadHint)
import StoreService.Providers (Confirmed (..), Funded (..), PaymentSignal (..), Provider (..), ProviderError (..), Rate (..), Received (..), Uncreditable (..), settleWindow)
import StoreService.Store.Invoices (unpaidRefs)
import StoreService.Waiters (Waiters, newWaiters)
import Test.Hspec

#if defined(dbPostgres)
import Bots.StoreService.WebTests (withServiceStores)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar)
import Control.Monad (forM, forM_)
import Data.Maybe (fromJust, fromMaybe)
import Simplex.Chat.PaymentService.Types (CryptoCurrency)
import StoreService.Names.LateCredits (lateCreditOf)
import StoreService.Store.Invoices (lockInvoice)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

namesLateCreditTests :: Spec
namesLateCreditTests = describe "StoreService names: balance life" $ do
  describe "settlement" $ do
    it "queues a held order and spends its hold, the source's amount down by fromBalance" testHeldSettled
    it "queues a difference order funded before its expiry and settled after it, still open in the database, spending its hold" testFundedThenSettledLate
    it "queues an order whose hold was removed by hand and alerts naming the order and the balance's order" testHoldNotSpent
    it "closes an order meeting another live order at settlement, alerts, and credits its payment after" testLiveOrderExists
    it "releases a difference order's hold in that settlement's transaction and credits its own balance after, by the credit step" testLiveOrderExistsHolder
    it "refuses a refund while the hold is open and spends the balance at settlement" testSettlementBeforeRefund
  describe "late credits" $ do
    it "credits a late payment to the closed order's balance at the lower rate, the released source's balance untouched, logged at info" testLateCredited
    it "credits at the invoice's rate when the price rose, storing both rates and the rate used" testRisingPrice
    it "credits a payment confirming over two reports by its increase, and a repeated report once" testGrowingReports
    it "credits a confirmed part payment the provider's close meets, and a cancel's at the pass, the cancel reading no rate" testCloseAndCancel
    it "credits nothing for unconfirmed money, logged as pending, then once when it reads Settled with the totals unchanged" testUnconfirmedThenSettled
    it "never credits a replaced or dropped payment, alerting it once past the window from its payment row, never again" testNeverConfirmed
    it "alerts a part credit with an unconfirmed rest once, naming its units" testPartCreditAlerted
    it "logs Monero's zero figure as pending and does not alert it within the window" testMoneroZero
    it "credits nothing while the rate read fails, warning once, then credits at the next pass" testRateFails
    it "credits nothing below the rate floor, logging an error, and alerts naming both rates and the owner's decision" testRateFloor
    it "reads one rate per coin per pass for several invoices, and none when nothing is due" testOneRateReadPerPass
    it "never credits a late Settled payment on an invoice made under HighSpeed, alerting it past the window" testHighSpeed
    it "credits at the next pass a late Settled invoice and a settlement whose credit step did not run" testCreditedByPass
    it "refuses to credit an order that is live again by the time it takes the invoice's lock" testLiveAgain
    it "alerts once of two instances, and again when more money arrives" testTwoInstancesAlert
    it "keeps the confirmed figure and the credit when the settled sum falls, crediting nothing on the same sum again" testFallingSum
    it "credits an invoice past the read window after a hinted read" testHintPastWindow
    it "credits Stripe's succeeded amount without reading a rate" testStripeCredited
  describe "balances with late credits" $ do
    it "keeps a credit made while the balance is held, the held spend settling" testCreditWhileHeld
    it "refuses a covered spend that read the amount before a credit" testSpendBeforeCredit
    it "sums the refunds over credit, refund, late credit, refund" testCreditRefundTwice
    it "spends a late credit with a covered spend and refunds the rest" testSpendAndRefundLate
  describe "the view" $ do
    it "shows a funded holder past its expiry held with no end time, and an unfunded one before it with its expiry" testHeldUntil
    it "shows a balance spent to 0 and a refunded one as objects with amount 0" testZeroBalances
    it "waits confirming, crediting, or support for each reason, and on nothing once all is credited" testLateWaits
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
  Received
    { rcvAmount = CurrencyAmount (fromIntegral $ satCents units),
      rcvCrypto = Just (T.pack $ show units),
      rcvDue = Nothing,
      rcvUnits = Just units,
      rcvRate = Just "62500",
      rcvConfirmed = Confirmed {cfCents = CurrencyAmount (fromIntegral $ satCents confirmed), cfUnits = Just confirmed},
      rcvUncreditable = Nothing
    }

cardPaid :: Int -> Int -> Received
cardPaid cents confirmed =
  Received
    { rcvAmount = CurrencyAmount (fromIntegral cents),
      rcvCrypto = Nothing,
      rcvDue = Nothing,
      rcvUnits = Nothing,
      rcvRate = Nothing,
      rcvConfirmed = Confirmed {cfCents = CurrencyAmount (fromIntegral confirmed), cfUnits = Nothing},
      rcvUncreditable = Nothing
    }

nothingPaid :: Received
nothingPaid = cardPaid 0 0

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

stubRate :: Rate
stubRate = rateOf "60000.5" 60000.5

-- | An unpaid order of 20000 cents, crypto unless card, whose invoice the provider closed with nothing received.
closedOrder :: Late -> Text -> Bool -> IO ()
closedOrder l@Late {lStore} r card = do
  placeOrderRow lStore r card 20000 (at 60)
  settleAt l r (SigClosed nothingPaid) (at 61) `shouldReturn` Right ISExpired
  NameRegistration {nrPhase, nrLive} <- reg lStore r
  (nrPhase, nrLive) `shouldBe` (PUnpaid, False)

-- | The provider reports the signal for the order's invoice, and the poller reads it on a hint, as a webhook asks; the logs.
readSignal :: Late -> Text -> PaymentSignal -> IO Text
readSignal Late {lStub, lHints, lPoller} r signal = do
  setSignals lStub [(refOf r, signal)]
  capturingStderr $ void (queueReadHint lHints (refOf r)) >> drainHints lPoller

-- | Settled straight in the store at the given time, with no credit step after it.
settleAt :: Late -> Text -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settleAt Late {lStore, lWaiters} r = settleOrder lStore lWaiters Nothing (invOf r)

-- | A new pass's credits; the logs.
passAt :: Late -> UTCTime -> IO Text
passAt Late {lPoller} now = capturingStderr $ forgetRates lPoller >> creditPass lPoller now

-- | The daily alerts of a newly started instance at the given time, with its rates; the logs.
alertsAt :: Late -> Either ProviderError Rate -> UTCTime -> IO Text
alertsAt Late {lStore} r now = withLateOn lStore $ \l@Late {lPoller} -> setRate l r >> capturingStderr (cleanupUnpaid lPoller now)

-- | Amount, credited and refunded of the order's balance.
balanceOf :: DBStore -> Text -> IO (Maybe (Int, Int, Int))
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
logged level needle = any (\ln -> ("[" <> level) `T.isPrefixOf` ln && needle `T.isInfixOf` ln) . T.lines

count :: Text -> Text -> Int
count needle = length . T.breakOnAll needle

-- | The payment rows' created_at, past the read window by the seconds given.
pastWindow :: UTCTime -> Integer -> UTCTime
pastWindow createdAt by = addUTCTime (settleWindow + fromInteger by) createdAt

-- settlement

testHeldSettled :: IO ()
testHeldSettled = withLate $ \l@Late {lStore = st} -> do
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  settleAt l "h" (SigSettled (btcPaid 480000 480000) (at 10)) (at 10) `shouldReturn` Right ISPaid
  NameRegistration {nrPhase, nrLive, nrBalanceUsed} <- reg st "h"
  (nrPhase, nrLive, nrBalanceUsed) `shouldBe` (PQueued, True, 10000)
  figures st "s" (at 10) `shouldReturn` Just (0, 10000, 0, Nothing)
  invariantHolds st "s"

testFundedThenSettledLate :: IO ()
testFundedThenSettledLate = withLate $ \l@Late {lStore = st} -> do
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  settleAt l "h" (SigFunded (btcPaid 480000 0) PaidInFull) (at 50) `shouldReturn` Right ISOpen
  settleAt l "h" (SigSettled (btcPaid 480000 480000) (at 120)) (at 120) `shouldReturn` Right ISPaid
  nrPhase <$> reg st "h" `shouldReturn` PQueued
  figures st "s" (at 120) `shouldReturn` Just (0, 10000, 0, Nothing)

testHoldNotSpent :: IO ()
testHoldNotSpent = withLate $ \l@Late {lStore = st} -> do
  source st "unspentsrc" 10000
  holder st "unspent" "unspentsrc" 10000 (at 60) `shouldReturn` Right ()
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_balances SET reserved_by = NULL WHERE registration_id = ?" (Only ("unspentsrc" :: Text))
  logs <- capturingStderr $ settleAt l "unspent" (SigSettled (btcPaid 480000 480000) (at 10)) (at 10) `shouldReturn` Right ISPaid
  logs `shouldSatisfy` logged "ERROR" "queued registration unspent as paid, but the balance of registration unspentsrc it held could not be spent"
  nrPhase <$> reg st "unspent" `shouldReturn` PQueued
  balanceOf st "unspentsrc" `shouldReturn` Just (10000, 10000, 0)

-- | The unique index keeping one live order per commitment is dropped, so a second live order can meet the first at settlement.
meetsLiveOrder :: DBStore -> Text -> IO ()
meetsLiveOrder st r = do
  withTransaction st $ \db -> DB.execute_ db "DROP INDEX sx_badge_service_idx_name_registrations_live_commitment"
  NameRegistration {nrCommitment} <- reg st r
  placeOrderRow st "twin" False 20000 (at 60)
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET commitment = ? WHERE registration_id = 'twin'" (Only $ DB.Binary nrCommitment)

testLiveOrderExists :: IO ()
testLiveOrderExists = withLate $ \l@Late {lStore = st} -> do
  placeOrderRow st "a" False 20000 (at 60)
  meetsLiveOrder st "a"
  logs <- readSignal l "a" (SigSettled (btcPaid lateUnits lateUnits) (at 10))
  logs `shouldSatisfy` logged "ERROR" "was paid while another live order held the commitment, or the owner and name, of registration a"
  NameRegistration {nrPhase, nrLive} <- reg st "a"
  (nrPhase, nrLive) `shouldBe` (PUnpaid, False)
  balanceOf st "a" `shouldReturn` Just (19200, 19200, 0)

testLiveOrderExistsHolder :: IO ()
testLiveOrderExistsHolder = withLate $ \l@Late {lStore = st} -> do
  source st "s" 10000
  holder st "a" "s" 10000 (at 60) `shouldReturn` Right ()
  meetsLiveOrder st "a"
  setRate l (Left $ ProviderError "rates down")
  void $ readSignal l "a" (SigSettled (btcPaid lateUnits lateUnits) (at 10))
  figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  balanceOf st "a" `shouldReturn` Nothing
  setRate l (Right stubRate)
  void $ passAt l (at 20)
  balanceOf st "a" `shouldReturn` Just (19200, 19200, 0)
  invariantHolds st "s"

testSettlementBeforeRefund :: IO ()
testSettlementBeforeRefund = withLate $ \l@Late {lStore = st} -> do
  turns <- newTurns
  source st "s" 10000
  holder st "h" "s" 10000 (at 60) `shouldReturn` Right ()
  recordRefund turns st "s" 10000 (at 5) >>= (`shouldSatisfy` \case Left (RefundHeld _) -> True; _ -> False)
  settleAt l "h" (SigSettled (btcPaid 480000 480000) (at 10)) (at 10) `shouldReturn` Right ISPaid
  recordRefund turns st "s" 10000 (at 11) >>= (`shouldSatisfy` \case Left _ -> True; Right _ -> False)
  balanceOf st "s" `shouldReturn` Just (0, 10000, 0)

-- late credits

testLateCredited :: IO ()
testLateCredited = withLate $ \l@Late {lStore = st} -> do
  source st "paid" 10000
  holder st "credited" "paid" 10000 (at 60) `shouldReturn` Right ()
  settleAt l "credited" (SigClosed nothingPaid) (at 61) `shouldReturn` Right ISExpired
  figures st "paid" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)
  logs <- readSignal l "credited" (SigClosed (btcPaid lateUnits lateUnits))
  logs `shouldSatisfy` logged "INFO" "invoice inv-credited credited 19200 cents to the balance of registration credited for 320000 units at 60000.5 (invoice 62500, current 60000.5)"
  logs `shouldNotSatisfy` logged "ERROR" "inv-credited"
  balanceOf st "credited" `shouldReturn` Just (19200, 19200, 0)
  creditsOf st "credited" `shouldReturn` [(Just lateUnits, Just "62500", Just "60000.5", Just "60000.5", 19200)]
  figures st "paid" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

testRisingPrice :: IO ()
testRisingPrice = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  setRate l (Right $ rateOf "70000" 70000)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  creditsOf st "x" `shouldReturn` [(Just lateUnits, Just "62500", Just "70000", Just "62500", 20000)]
  balanceOf st "x" `shouldReturn` Just (20000, 20000, 0)

testGrowingReports :: IO ()
testGrowingReports = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000))
  balanceOf st "x" `shouldReturn` Just (9600, 9600, 0)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  map (\(units, _, _, _, cents) -> (units, cents)) <$> creditsOf st "x" `shouldReturn` [(Just 160000, 9600), (Just 160000, 9600)]
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testCloseAndCancel :: IO ()
testCloseAndCancel = withLate $ \l@Late {lStore = st, lWaiters} -> do
  placeOrderRow st "p" False 20000 (at 60)
  settleAt l "p" (SigFunded (btcPaid lateUnits 160000) PaidInPart) (at 10) `shouldReturn` Right ISOpen
  void $ readSignal l "p" (SigClosed (btcPaid lateUnits 160000))
  nrLive <$> reg st "p" `shouldReturn` False
  balanceOf st "p" `shouldReturn` Just (9600, 9600, 0)
  placeOrderRow st "c" False 20000 (at 60)
  settleAt l "c" (SigFunded (btcPaid lateUnits lateUnits) PaidInFull) (at 10) `shouldReturn` Right ISOpen
  calls <- rateCalls l
  cancelOpenInvoice st lWaiters Nothing (invOf "c") (at 20) `shouldReturn` True
  rateCalls l `shouldReturn` calls
  balanceOf st "c" `shouldReturn` Nothing
  void $ passAt l (at 21)
  balanceOf st "c" `shouldReturn` Just (19200, 19200, 0)

testUnconfirmedThenSettled :: IO ()
testUnconfirmedThenSettled = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  logs <- readSignal l "x" (SigClosed (btcPaid lateUnits 0))
  logs `shouldSatisfy` logged "INFO" "invoice inv-x holds money of its closed order not yet confirmed"
  balanceOf st "x" `shouldReturn` Nothing
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  length <$> creditsOf st "x" `shouldReturn` 1
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

-- | The order was placed long before the payment, whose own row the window counts from.
testNeverConfirmed :: IO ()
testNeverConfirmed = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "never" False
  let seenAt = at (60 * 24 * 5)
      alert = "invoice inv-never of order never: 320000 satoshi received, 0 satoshi confirmed, 0 satoshi credited; refund by hand only a transaction confirmed on chain that BTCPay no longer monitors; one still Processing there waits"
  settleAt l "never" (SigClosed (btcPaid lateUnits 0)) seenAt `shouldReturn` Right ISExpired
  alertsAt l (Right stubRate) (pastWindow seenAt (-60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice inv-never of order never")
  lateWaitOf st "never" `shouldReturn` Just LWConfirming
  logs <- alertsAt l (Right stubRate) (pastWindow seenAt 60)
  (count alert logs, logged "ERROR" alert logs) `shouldBe` (1, True)
  lateWaitOf st "never" `shouldReturn` Just LWSupport
  alertsAt l (Right stubRate) (pastWindow seenAt (24 * 3600 + 60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice inv-never of order never")
  balanceOf st "never" `shouldReturn` Nothing

testPartCreditAlerted :: IO ()
testPartCreditAlerted = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "part" False
  settleAt l "part" (SigClosed (btcPaid lateUnits 160000)) (at 100) `shouldReturn` Right ISExpired
  void $ passAt l (at 101)
  balanceOf st "part" `shouldReturn` Just (9600, 9600, 0)
  logs <- alertsAt l (Right stubRate) (pastWindow (at 100) 60)
  count "invoice inv-part of order part: 320000 satoshi received, 160000 satoshi confirmed, 160000 satoshi credited; refund by hand only" logs `shouldBe` 1
  alertsAt l (Right stubRate) (pastWindow (at 100) (24 * 3600 + 60)) >>= (`shouldNotSatisfy` T.isInfixOf "invoice inv-part of order part")

testMoneroZero :: IO ()
testMoneroZero = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "monero" False
  logs <- readSignal l "monero" (SigFunded (btcPaid 0 0) {rcvCrypto = Nothing} PaidInFull)
  logs `shouldSatisfy` logged "INFO" "invoice inv-monero holds money of its closed order not yet confirmed"
  withTransaction st (\db -> latePending db (invOf "monero")) `shouldReturn` Just LPConfirming
  now <- getCurrentTime
  alertsAt l (Right stubRate) (addUTCTime 3600 now) >>= (`shouldNotSatisfy` T.isInfixOf "invoice inv-monero of order monero")
  balanceOf st "monero" `shouldReturn` Nothing

testRateFails :: IO ()
testRateFails = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "fail1" False
  closedOrder l "fail2" False
  setRate l (Left $ ProviderError "rates down for fail1")
  logs <- readSignal l "fail1" (SigClosed (btcPaid lateUnits lateUnits))
  settleAt l "fail2" (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
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
  settleAt l "floored" (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
  logs <- passAt l (at 71)
  logs `shouldSatisfy` logged "ERROR" "invoice inv-floored is not credited: the current rate 31000 is below half its rate 62500"
  balanceOf st "floored" `shouldReturn` Nothing
  lateWaitOf st "floored" `shouldReturn` Just LWSupport
  alertsAt l (Right floored) (pastWindow (at 70) 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice inv-floored of order floored: 320000 satoshi received, 320000 satoshi confirmed, 0 satoshi credited; confirmed; the coin's price fell by more than half since the invoice (62500, 31000); the owner decides: credit at the current rate, or refund by hand")
  setRate l (Right $ rateOf "31250" 31250)
  void $ passAt l (at 72)
  creditsOf st "floored" `shouldReturn` [(Just lateUnits, Just "62500", Just "31250", Just "31250", 10000)]
  lateWaitOf st "floored" `shouldReturn` Nothing

testOneRateReadPerPass :: IO ()
testOneRateReadPerPass = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> closedOrder l r False >> settleAt l r (SigClosed (btcPaid lateUnits lateUnits)) (at 70)) ["x", "y", "z"]
  closedOrder l "card" True
  settleAt l "card" (SigSettled (cardPaid 20000 20000) (at 70)) (at 70) `shouldReturn` Right ISPaid
  void $ passAt l (at 71)
  rateCalls l `shouldReturn` 1
  mapM (balanceOf st) ["x", "y", "z", "card"] `shouldReturn` replicate 3 (Just (19200, 19200, 0)) <> [Just (20000, 20000, 0)]
  void $ passAt l (at 72)
  rateCalls l `shouldReturn` 1

testHighSpeed :: IO ()
testHighSpeed = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "hs" False
  let highSpeed = (btcPaid lateUnits 0) {rcvUncreditable = Just UCHighSpeed}
  settleAt l "hs" (SigSettled highSpeed (at 70)) (at 70) `shouldReturn` Right ISPaid
  void $ passAt l (at 71)
  balanceOf st "hs" `shouldReturn` Nothing
  lateWaitOf st "hs" `shouldReturn` Just LWSupport
  alertsAt l (Right stubRate) (pastWindow (at 70) 60)
    >>= (`shouldSatisfy` logged "ERROR" "invoice inv-hs of order hs: 320000 satoshi received, 0 satoshi confirmed, 0 satoshi credited; the invoice was made under HighSpeed")

testCreditedByPass :: IO ()
testCreditedByPass = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "settled" False
  settleAt l "settled" (SigSettled (btcPaid lateUnits lateUnits) (at 70)) (at 70) `shouldReturn` Right ISPaid
  closedOrder l "crashed" False
  settleAt l "crashed" (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
  mapM (balanceOf st) ["settled", "crashed"] `shouldReturn` [Nothing, Nothing]
  void $ passAt l (at 71)
  mapM (balanceOf st) ["settled", "crashed"] `shouldReturn` replicate 2 (Just (19200, 19200, 0))

testLiveAgain :: IO ()
testLiveAgain = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  settleAt l "x" (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
  let liveAgain _ = do
        withTransaction st $ \db -> DB.execute_ db "UPDATE sx_badge_service_name_registrations SET live = 1 WHERE registration_id = 'x'"
        pure (Right stubRate)
  creditLateInvoice st liveAgain (invOf "x") (at 71) `shouldReturn` CONothingDue
  balanceOf st "x" `shouldReturn` Nothing
  creditsOf st "x" `shouldReturn` []

testTwoInstancesAlert :: IO ()
testTwoInstancesAlert = withLate $ \l -> do
  closedOrder l "twice" False
  settleAt l "twice" (SigClosed (btcPaid lateUnits 0)) (at 70) `shouldReturn` Right ISExpired
  let first = "invoice inv-twice of order twice: 320000 satoshi received"
  logs <- (<>) <$> alertsAt l (Right stubRate) (pastWindow (at 70) 60) <*> alertsAt l (Right stubRate) (pastWindow (at 70) 60)
  count first logs `shouldBe` 1
  settleAt l "twice" (SigClosed (btcPaid 400000 0)) (at 80) `shouldReturn` Right ISExpired
  alertsAt l (Right stubRate) (pastWindow (at 70) 120) >>= (`shouldSatisfy` logged "ERROR" "invoice inv-twice of order twice: 400000 satoshi received, 0 satoshi confirmed")

testFallingSum :: IO ()
testFallingSum = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000))
  confirmed <- withTransaction st $ \db -> DB.query db "SELECT confirmed_units FROM sx_badge_service_payments WHERE invoice_id = 'inv-x'" ()
  confirmed `shouldBe` [Only (Just lateUnits)]
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  length <$> creditsOf st "x" `shouldReturn` 1
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testHintPastWindow :: IO ()
testHintPastWindow = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  now <- getCurrentTime
  let longAgo = addUTCTime (negate $ settleWindow + 3600) now
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_invoices SET created_at = ? WHERE invoice_id = 'inv-x'" (Only longAgo)
  unpaidRefs st (addUTCTime (negate settleWindow) now) >>= (`shouldNotSatisfy` elem ("crypto", refOf "x"))
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  balanceOf st "x" `shouldReturn` Just (19200, 19200, 0)

testStripeCredited :: IO ()
testStripeCredited = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "c" True
  void $ readSignal l "c" (SigSettled (cardPaid 20000 20000) (at 70))
  creditsOf st "c" `shouldReturn` [(Nothing, Nothing, Nothing, Nothing, 20000)]
  balanceOf st "c" `shouldReturn` Just (20000, 20000, 0)
  rateCalls l `shouldReturn` 0

-- balances with late credits

testCreditWhileHeld :: IO ()
testCreditWhileHeld = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  holder st "h" "x" 19200 (at 60) `shouldReturn` Right ()
  void $ readSignal l "x" (SigClosed (btcPaid 400000 400000))
  figures st "x" (at 10) `shouldReturn` Just (24000, 24000, 0, Just BalanceHold {bhHolder = RegistrationId "h", bhUntil = at 60, bhOverdue = False, bhFunded = False})
  settleAt l "h" (SigSettled (btcPaid 330000 330000) (at 10)) (at 10) `shouldReturn` Right ISPaid
  figures st "x" (at 10) `shouldReturn` Just (4800, 24000, 0, Nothing)
  invariantHolds st "x"

coveredSpend :: DBStore -> Text -> Text -> Int -> Int -> IO (Either RegistrationRefused ())
coveredSpend st r src price seen = withTransaction st $ \db -> createBalanceRegistration db (newReg r ownerA (fieldsN r)) (RegistrationId src) price seen t0

testSpendBeforeCredit :: IO ()
testSpendBeforeCredit = withLate $ \l@Late {lStore = st} -> do
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  void $ readSignal l "x" (SigClosed (btcPaid 400000 400000))
  coveredSpend st "c" "x" 10000 19200 `shouldReturn` Left RRBalanceChanged
  coveredSpend st "c" "x" 10000 24000 `shouldReturn` Right ()
  balanceOf st "x" `shouldReturn` Just (14000, 24000, 0)

testCreditRefundTwice :: IO ()
testCreditRefundTwice = withLate $ \l@Late {lStore = st} -> do
  turns <- newTurns
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits 160000))
  void (recordRefund turns st "x" 9600 (at 80) >>= either (fail . show) pure)
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  void (recordRefund turns st "inv-x" 9600 (at 81) >>= either (fail . show) pure)
  balanceOf st "x" `shouldReturn` Just (0, 19200, 19200)
  invariantHolds st "x"

testSpendAndRefundLate :: IO ()
testSpendAndRefundLate = withLate $ \l@Late {lStore = st} -> do
  turns <- newTurns
  closedOrder l "x" False
  void $ readSignal l "x" (SigClosed (btcPaid lateUnits lateUnits))
  coveredSpend st "c" "x" 10000 19200 `shouldReturn` Right ()
  nrBalanceSource <$> reg st "c" `shouldReturn` Just (RegistrationId "x")
  recordRefund turns st "x" 10000 (at 80) >>= (`shouldSatisfy` \case Left (RefundAmount 9200) -> True; _ -> False)
  void (recordRefund turns st "x" 9200 (at 80) >>= either (fail . show) pure)
  balanceOf st "x" `shouldReturn` Just (0, 19200, 9200)

-- the view

balanceAt :: DBStore -> Text -> UTCTime -> IO (Maybe J.Value)
balanceAt st r now = fmap balanceJSON <$> withTransaction st (\db -> getBalance db (RegistrationId r) now)

balanceObject :: Int -> Bool -> Maybe UTCTime -> Int -> J.Value
balanceObject amount held heldUntil refunded = J.object ["amount" J..= amount, "held" J..= held, "heldUntil" J..= heldUntil, "refunded" J..= refunded]

testHeldUntil :: IO ()
testHeldUntil = withLate $ \l@Late {lStore = st} -> do
  source st "funded" 10000
  holder st "fh" "funded" 10000 (at 60) `shouldReturn` Right ()
  source st "unfunded" 10000
  holder st "uh" "unfunded" 10000 (at 60) `shouldReturn` Right ()
  balanceAt st "funded" (at 30) `shouldReturn` Just (balanceObject 10000 True (Just $ at 60) 0)
  settleAt l "fh" (SigFunded (btcPaid 100 0) PaidInPart) (at 30) `shouldReturn` Right ISOpen
  balanceAt st "funded" (at 30) `shouldReturn` Just (balanceObject 10000 True Nothing 0)
  balanceAt st "funded" (at 120) `shouldReturn` Just (balanceObject 10000 True Nothing 0)
  balanceAt st "unfunded" (at 30) `shouldReturn` Just (balanceObject 10000 True (Just $ at 60) 0)
  balanceAt st "unfunded" (at 120) `shouldReturn` Just (balanceObject 10000 False Nothing 0)

testZeroBalances :: IO ()
testZeroBalances = withLate $ \Late {lStore = st} -> do
  turns <- newTurns
  source st "spent" 10000
  coveredSpend st "c" "spent" 10000 10000 `shouldReturn` Right ()
  balanceAt st "spent" t0 `shouldReturn` Just (balanceObject 0 False Nothing 0)
  source st "refunded" 10000
  void (recordRefund turns st "refunded" 10000 (at 5) >>= either (fail . show) pure)
  balanceAt st "refunded" t0 `shouldReturn` Just (balanceObject 0 False Nothing 10000)
  placeOrderRow st "none" False 20000 (at 60)
  balanceAt st "none" t0 `shouldReturn` Nothing

testLateWaits :: IO ()
testLateWaits = withLate $ \l@Late {lStore = st} -> do
  mapM_ (\r -> closedOrder l r False) ["confirming", "crediting", "highspeed", "units", "floor", "alerted"]
  settleAt l "crediting" (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
  settleAt l "highspeed" (SigClosed (btcPaid lateUnits 0) {rcvUncreditable = Just UCHighSpeed}) (at 70) `shouldReturn` Right ISExpired
  settleAt l "units" (SigClosed (btcPaid 0 0) {rcvAmount = CurrencyAmount 20000, rcvCrypto = Just "0.003200001", rcvUnits = Nothing, rcvUncreditable = Just UCUncounted}) (at 70) `shouldReturn` Right ISExpired
  settleAt l "alerted" (SigClosed (btcPaid lateUnits 0)) (at 70) `shouldReturn` Right ISExpired
  void $ alertsAt l (Right stubRate) (pastWindow (at 70) 60)
  settleAt l "confirming" (SigClosed (btcPaid lateUnits 0)) (pastWindow (at 70) 60) `shouldReturn` Right ISExpired
  setRate l (Right $ rateOf "31000" 31000)
  void $ readSignal l "floor" (SigClosed (btcPaid lateUnits lateUnits))
  placeOrderRow st "open" False 20000 (at 60)
  settleAt l "open" (SigFunded (btcPaid lateUnits 0) PaidInFull) (at 70) `shouldReturn` Right ISOpen
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
  void $ readSignal l "alerted" (SigClosed (btcPaid lateUnits lateUnits))
  void $ passAt l (at 71)
  waits ["crediting", "floor", "alerted"] `shouldReturn` [("crediting", Nothing), ("floor", Nothing), ("alerted", Nothing)]

testFindLinks :: IO ()
testFindLinks = withLate $ \Late {lStore = st} -> do
  source st "paying" 10000
  coveredSpend st "spend" "paying" 10000 10000 `shouldReturn` Right ()
  source st "held" 10000
  holder st "holder" "held" 10000 (at 60) `shouldReturn` Right ()
  let links now = map (\(NameRegistration {nrId = RegistrationId r, nrBalanceSource}, heldBy') -> (r, nrBalanceSource, heldBy')) <$> withTransaction st (\db -> findRegistrations db ownerA now)
      linked rows = filter (\(_, src, h) -> src /= Nothing || h /= Nothing) rows
  linked <$> links (at 30) `shouldReturn` [("spend", Just (RegistrationId "paying"), Nothing), ("holder", Just (RegistrationId "held"), Nothing), ("held", Nothing, Just (RegistrationId "holder"))]
  linked <$> links (at 120) `shouldReturn` [("spend", Just (RegistrationId "paying"), Nothing), ("holder", Just (RegistrationId "held"), Nothing)]

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
          settleAt l r (SigClosed (btcPaid lateUnits lateUnits)) (at 70) `shouldReturn` Right ISExpired
    forM_ [1 .. rounds] $ \n -> do
      let r = "x" <> T.pack (show n)
      due r
      arrived <- newTVarIO 0
      let readRate :: CryptoCurrency -> IO (Either ProviderError Rate)
          readRate _ = Right stubRate <$ meet arrived
      (a, b) <- concurrently (creditLateInvoice st readRate (invOf r) (at 80)) (creditLateInvoice other readRate (invOf r) (at 80))
      (n, length [() | COCredited _ <- [a, b]], length [() | CONothingDue <- [a, b]]) `shouldBe` (n, 1, 1)
      length <$> creditsOf st r `shouldReturn` 1
      balanceOf st r `shouldReturn` Just (19200, 19200, 0)
    refused <- forM [1 .. rounds] $ \n -> do
      let r = "stale" <> T.pack (show n)
      due r
      arrived <- newTVarIO 0
      let staleStep :: DBStore -> IO Bool
          staleStep store = do
            Just s <- withTransaction store (`lateCreditState` invOf r)
            meet arrived
            withTransaction store $ \db -> do
              lockInvoice db (invOf r) (at 80)
              creditLatePayment db (fromJust $ lateCreditOf s (Just stubRate)) (at 80)
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
    settleAt l r (SigClosed (btcPaid lateUnits 160000)) (at 70) `shouldReturn` Right ISExpired
    arrived <- newTVarIO 0
    let readRate :: CryptoCurrency -> IO (Either ProviderError Rate)
        readRate _ = Right stubRate <$ meet arrived
        raise :: IO (Either Text InvoiceStatus)
        raise = meet arrived >> settleOrder other otherWaiters Nothing (invOf r) (SigClosed (btcPaid lateUnits lateUnits)) (at 80)
    (credited, raised) <- concurrently (creditLateInvoice st readRate (invOf r) (at 80)) raise
    (n, raised) `shouldBe` (n, Right ISExpired)
    credited `shouldSatisfy` \case COCredited _ -> True; _ -> False
    creditLateInvoice st (const $ pure $ Right stubRate) (invOf r) (at 81) >>= (`shouldSatisfy` (/= CORefused))
    units <- map (\(u, _, _, _, _) -> u) <$> creditsOf st r
    (n, sum (map (fromMaybe 0) units)) `shouldBe` (n, lateUnits)
    balanceOf st r `shouldReturn` Just (19200, 19200, 0)
#endif
