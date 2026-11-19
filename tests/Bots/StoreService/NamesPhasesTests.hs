{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesPhasesTests (namesPhasesTests, change, held, woken, prompt, registrationPath, seenVersion) where

import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (hashFixture, order, ownerA, placePaid, reveal, t0)
import Bots.StoreService.WebTests (bounded, capturingStderr, fieldOf, jsonObject, phaseOf, phaseValue, rcv, statusOf, webGet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait, waitCatchSTM)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, atomically, check, orElse)
import Control.Monad (forM_, void)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import StoreService.Config (ServiceConfig (..))
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (phasesWaiting, publish)
import StoreService.Names.Registration (RegistrationId (..))
import StoreService.Names.Store (Commit (..), NameRegistration (..), OrderPhase (..), VersionWrite (..), getRegistration, updateRegistration)
import StoreService.Poller (cleanupUnpaid, newPollerEnv, unpaidRetention)
import StoreService.Providers (Funded (..), PaymentSignal (..))
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Waiters (publishPayment, waitingCountSTM)
import StoreService.Web.Http (holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesPhasesTests :: Spec
namesPhasesTests = describe "StoreService names: phases" $ do
  describe "changes" $ do
    it "wakes a held invoice poll and a held registration poll on a change" testEachChangeWakes
    it "holds a paid name invoice on its registration's version alone, and answers at once without a valid version" testPaidHoldsOnVersion
    it "answers at once an invoice paid after its order closed" testLatePaidAnswersAtOnce
    it "wakes an open invoice poll by a version change, and holds it on its status without a valid version" testOpenWakesOnVersion
  describe "time" $ do
    it "wakes an unpaid invoice held on its status at its expiry, reading it expired" testExpiryWakes
    it "reads a funded open invoice past its expiry as open, and holds it" testFundedStaysOpen
    it "reads a committed registration with no reveal past revealBefore as lapsed, waking held registration and invoice polls" testLapseReadsFailed
  describe "registration poll" $ do
    it "answers at once without seenVersion, with another version, or with one that is not a number" testAnswersAtOnce
    it "answers unchanged after the hold" testHoldTimesOut
    it "answers an unknown registration 404" testUnknownRegistration
    it "answers 404 at once to a held poll of an order the cleanup deletes" testCleanupWakes

data Polls = Polls
  { pCtx :: Ctx,
    -- | Sets the service's clock, which then runs on in real time, so a hold can end at a moment.
    pSetNow :: UTCTime -> IO ()
  }

-- | The registration route as namesRoutes registers it, beside the invoice routes, since a NamesEnv needs a running chat controller.
withPolls :: Int -> (Polls -> IO a) -> IO a
withPolls hold action = withOrders $ \ctx@Ctx {cClock, cOrders, cWeb} -> do
  started <- getCurrentTime
  let elapsed = (`diffUTCTime` started) <$> getCurrentTime
      running = addUTCTime <$> elapsed <*> readIORef cClock
      orders = cOrders {oeNow = running}
      web = cWeb {weOrders = Just orders}
      routes = \case
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders (weHttp web) hold (RegistrationId rid)}
        _ -> Nothing
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
      pSetNow t = elapsed >>= \e -> writeIORef cClock (addUTCTime (negate e) t)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port ->
    action Polls {pCtx = ctx {cOrders = orders, cWeb = web, cClient = (mgr, "http://127.0.0.1:" <> show port)}, pSetNow}

-- | How long a held poll is watched for a wake that must not come.
parkDelay :: Int
parkDelay = 200000

-- | How long a poll may take to be held or to answer once woken; far below the hold.
prompt :: Int
prompt = 6000000

-- | How far ahead of a moment the clock is set, so a poll is held well before the moment wakes it.
shortOfMoment :: NominalDiffTime
shortOfMoment = 3

shortHold :: Int
shortHold = 500000

get :: Ctx -> String -> IO (Int, J.Object)
get Ctx {cClient} path = webGet cClient path >>= \r -> (statusOf r,) <$> jsonObject r

-- | The holds the server has taken: invoices held on a payment, and waiters on a registration's version.
subscriptions :: Ctx -> STM (Int, Int)
subscriptions Ctx {cWeb, cOrders} = (,) <$> waitingCountSTM (weWaiters cWeb) <*> phasesWaiting (oePhases cOrders)

-- | A poll once the server holds it, having taken these many more holds of each kind.
held :: Ctx -> (Int, Int) -> String -> IO (Async (Int, J.Object))
held ctx (payments, versions) path = do
  (payments0, versions0) <- atomically $ subscriptions ctx
  poll <- async $ get ctx path
  let holding = subscriptions ctx >>= check . (== (payments0 + payments, versions0 + versions))
  timeout prompt (atomically $ (Left <$> waitCatchSTM poll) `orElse` (Right <$> holding)) >>= \case
    Just (Right ()) -> pure poll
    Just (Left answered) -> fail $ path <> " was answered at once: " <> show answered
    Nothing -> cancel poll >> fail (path <> " was not held with " <> show (payments, versions) <> " more holds")

woken :: String -> Async (Int, J.Object) -> IO (Int, J.Object)
woken what poll = timeout prompt (wait poll) >>= maybe (cancel poll >> fail (what <> " was not woken")) pure

answeredAtOnce :: Ctx -> String -> IO (Int, J.Object)
answeredAtOnce ctx path = timeout prompt (get ctx path) >>= maybe (fail (path <> " was held")) pure

invoicePath :: InvoiceId -> String
invoicePath (InvoiceId t) = "/api/invoice/" <> T.unpack t

registrationPath :: RegistrationId -> String
registrationPath (RegistrationId t) = "/api/registration/" <> T.unpack t

-- | The query the page sends to follow an invoice, as it last saw it.
following :: InvoiceStatus -> Int64 -> Text -> Bool -> String
following status version = followingAs status (Just $ show version)

-- | With seenVersion as given, a number or not, or left out.
followingAs :: InvoiceStatus -> Maybe String -> Text -> Bool -> String
followingAs status version paid full =
  "?wait=" <> statusText <> maybe "" ("&seenVersion=" <>) version <> "&seenPaid=" <> T.unpack paid <> "&seenFull=" <> (if full then "1" else "0")
  where
    statusText = case status of
      ISOpen -> "open"
      ISPaid -> "paid"
      ISExpired -> "expired"

seenVersion :: Int64 -> String
seenVersion v = "?seenVersion=" <> show v

-- | A write as the worker or a reveal makes it: a compare-and-set, then a publish after the commit.
change :: Ctx -> RegistrationId -> (NameRegistration -> NameRegistration) -> IO Int64
change Ctx {cStore, cOrders, cClock} rid f = do
  now <- readIORef cClock
  written <- withTransaction cStore $ \db -> getRegistration db rid >>= \r -> updateRegistration db (f $ fromJust r) now
  case written of
    Written v -> v <$ atomically (publish (oePhases cOrders) rid v)
    other -> fail $ "registration not written: " <> show other

-- | The settled payment's marks: its crypto amount and paid in full.
settledPaid :: Text
settledPaid = "0.00156"

-- tests

-- | Paid and queued at version 2; the change is one the worker makes, and every write publishes alike.
testEachChangeWakes :: IO ()
testEachChangeWakes = bounded "each change wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  let changes :: [(String, NameRegistration -> NameRegistration, Text, J.Value)]
      changes =
        [("phase", \r -> r {nrPhase = OCommitting $ TxHash $ hashFixture 8}, "phase", phaseValue "committing" [])]
  forM_ changes $ \(what, f, k, expected) -> do
    v <- nrVersion <$> invoiceRegistration ctx inv
    invoicePoll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid v settledPaid True
    registrationPoll <- held ctx (0, 1) $ registrationPath nrId <> seenVersion v
    v' <- change ctx nrId f
    (status, o) <- woken (what <> ": invoice poll") invoicePoll
    (status, fieldOf o "status", fieldOf (registrationOf o) "version", fieldOf (registrationOf o) k) `shouldBe` (200, Just "paid", Just (J.toJSON v'), Just expected)
    (status', r) <- woken (what <> ": registration poll") registrationPoll
    (status', fieldOf r "version", fieldOf r k) `shouldBe` (200, Just (J.toJSON v'), Just expected)

testPaidHoldsOnVersion :: IO ()
testPaidHoldsOnVersion = bounded "paid holds on version" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cWeb}} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` ISPaid
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  -- without a version there is nothing a paid invoice could wait for
  forM_ [Nothing, Just "two"] $ \version ->
    (fst <$> answeredAtOnce ctx (invoicePath inv <> followingAs ISPaid version settledPaid True)) `shouldReturn` 200
  poll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid nrVersion settledPaid True
  atomically $ publishPayment (weWaiters cWeb) inv
  threadDelay parkDelay
  Async.poll poll >>= (`shouldSatisfy` isNothing)
  v' <- change ctx nrId $ \r -> r {nrPhase = OCommitting $ TxHash $ hashFixture 8}
  (_, o) <- woken "paid invoice poll" poll
  (fieldOf (registrationOf o) "version", phaseOf (registrationOf o)) `shouldBe` (Just (J.toJSON v'), Just "committing")
  -- marks other than the page saw answer at once
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISPaid v' "0.001" True)) `shouldReturn` 200

testLatePaidAnswersAtOnce :: IO ()
testLatePaidAnswersAtOnce = bounded "late paid" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  fst <$> post ctx (invoicePath inv <> "/cancel") "" `shouldReturn` 200
  settleNow ctx inv settled `shouldReturn` ISPaid
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  (status, o) <- answeredAtOnce ctx $ invoicePath inv <> following ISPaid nrVersion settledPaid True
  (status, fieldOf o "status", fieldOf (registrationOf o) "phase") `shouldBe` (200, Just "paid", Just $ phaseValue "closed" ["closedBy" J..= ("cancelled" :: Text)])

testOpenWakesOnVersion :: IO ()
testOpenWakesOnVersion = bounded "open wakes on version" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  -- a version the page has not seen answers at once
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISOpen (nrVersion - 1) "" False)) `shouldReturn` 200
  poll <- held ctx (1, 1) $ invoicePath inv <> following ISOpen nrVersion "" False
  v' <- change ctx nrId $ \r -> r {nrMoves = nrMoves r + 1}
  (_, o) <- woken "open invoice poll" poll
  (fieldOf o "status", fieldOf (registrationOf o) "version") `shouldBe` (Just "open", Just (J.toJSON v'))
  -- a seenVersion that is no version leaves the hold on the status and payment alone
  byStatus <- held ctx (1, 0) $ invoicePath inv <> followingAs ISOpen (Just "two") "" False
  settleNow ctx inv (SigFunded (rcv 5000 (Just "0.00078")) PaidInPart) `shouldReturn` ISOpen
  (_, paidInPart) <- woken "open invoice poll without a version" byStatus
  (fieldOf paidInPart "status", fieldOf paidInPart "cryptoAmountPaid") `shouldBe` (Just "open", Just "0.00078")

-- | The stub's crypto invoice expires 45 minutes after the order; the clock is set just short of it.
testExpiryWakes :: IO ()
testExpiryWakes = bounded "expiry wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  Just InvoiceRow {irExpiresAt} <- getInvoice (cStore ctx) inv
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  pSetNow $ addUTCTime (negate shortOfMoment) irExpiresAt
  poll <- held ctx (1, 0) $ invoicePath inv <> followingAs ISOpen Nothing "" False
  (status, o) <- woken "unpaid invoice poll" poll
  (status, fieldOf o "status", fieldOf (registrationOf o) "version") `shouldBe` (200, Just "expired", Just (J.toJSON nrVersion))
  -- read so, not written: the sweep or the next order closes it
  (irStatus <$>) <$> getInvoice (cStore ctx) inv `shouldReturn` Just ISOpen
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISOpen nrVersion "" False)) `shouldReturn` 200

testFundedStaysOpen :: IO ()
testFundedStaysOpen = bounded "funded stays open" $ withPolls holdMicros $ \Polls {pCtx = ctx, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv (SigFunded (rcv 5000 (Just "0.00078")) PaidInPart) `shouldReturn` ISOpen
  Just InvoiceRow {irExpiresAt} <- getInvoice (cStore ctx) inv
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  pSetNow $ addUTCTime 60 irExpiresAt
  (status, o) <- get ctx (invoicePath inv)
  (status, fieldOf o "status") `shouldBe` (200, Just "open")
  poll <- held ctx (1, 1) $ invoicePath inv <> following ISOpen nrVersion "0.00078" False
  cancel poll

testLapseReadsFailed :: IO ()
testLapseReadsFailed = bounded "lapse" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cOrders}, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  now <- oeNow cOrders
  let revealBefore = seconds now + 600
      lapsedAt = posixSecondsToUTCTime (fromIntegral revealBefore)
  pSetNow $ addUTCTime (negate shortOfMoment) lapsedAt
  let committed = OCommitted Commit {cmTx = Nothing, cmRevealAfter = revealBefore - 3600, cmRevealBefore = revealBefore}
  v <- change ctx nrId $ \r -> r {nrPhase = committed, nrReveal = Nothing}
  (_, r) <- get ctx (registrationPath nrId)
  phaseOf r `shouldBe` Just "committed"
  invoicePoll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid v settledPaid True
  poll <- held ctx (0, 1) $ registrationPath nrId <> seenVersion v
  (status, lapsed) <- woken "registration poll at the lapse" poll
  let lapsedPhase = phaseValue "committed" ["revealAfter" J..= (revealBefore - 3600), "revealBefore" J..= revealBefore, "signature" J..= J.object ["type" J..= ("lapsed" :: Text)]]
  (status, fieldOf lapsed "phase", fieldOf lapsed "version") `shouldBe` (200, Just lapsedPhase, Just (J.toJSON v))
  (_, o) <- woken "invoice poll at the lapse" invoicePoll
  (fieldOf o "status", fieldOf (registrationOf o) "phase") `shouldBe` (Just "paid", Just lapsedPhase)
  (nrPhase <$> invoiceRegistration ctx inv) `shouldReturn` committed
  -- one with a reveal set stays committed past revealBefore
  revealed <- change ctx nrId $ \r' -> r' {nrReveal = Just reveal}
  pSetNow $ addUTCTime 3600 lapsedAt
  (_, r') <- get ctx (registrationPath nrId)
  (phaseOf r', fieldOf r' "label", fieldOf r' "version") `shouldBe` (Just "committed", Just "alicebakery", Just (J.toJSON revealed))

testAnswersAtOnce :: IO ()
testAnswersAtOnce = bounded "at once" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId = rid@(RegistrationId ridText), nrVersion} <- invoiceRegistration ctx inv
  forM_ ["", seenVersion (nrVersion + 1), seenVersion (nrVersion - 1), "?seenVersion=one", "?seenVersion=-1", "?seenVersion=99999999999999999999"] $ \q -> do
    (status, r) <- answeredAtOnce ctx (registrationPath rid <> q)
    (q, status, fieldOf r "id", fieldOf r "version", phaseOf r) `shouldBe` (q, 200, Just (J.String ridText), Just (J.toJSON nrVersion), Just "unpaid")

testHoldTimesOut :: IO ()
testHoldTimesOut = bounded "hold times out" $ withPolls shortHold $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  started <- getCurrentTime
  (status, r) <- get ctx $ registrationPath nrId <> seenVersion nrVersion
  waited <- (`diffUTCTime` started) <$> getCurrentTime
  (status, fieldOf r "version") `shouldBe` (200, Just (J.toJSON nrVersion))
  waited `shouldSatisfy` (>= (fromIntegral shortHold / 1000000 :: NominalDiffTime))

testUnknownRegistration :: IO ()
testUnknownRegistration = bounded "unknown" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (status, o) <- answeredAtOnce ctx (registrationPath (RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g") <> seenVersion 1)
  (status, o) `shouldBe` (404, KM.fromList [("type", "not_found")])

testCleanupWakes :: IO ()
testCleanupWakes = bounded "cleanup wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cStore, cWeb, cOrders}} -> do
  placePaid cStore "gone" ownerA (order "alicebakery" 1) t0 `shouldReturn` Right ()
  Just NameRegistration {nrVersion} <- withTransaction cStore (`getRegistration` RegistrationId "gone")
  deleted <- held ctx (0, 1) $ registrationPath (RegistrationId "gone") <> seenVersion nrVersion
  poller <- newPollerEnv cStore (weWaiters cWeb) (Just $ oePhases cOrders) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
  void $ capturingStderr $ cleanupUnpaid poller $ addUTCTime (unpaidRetention + 1) t0
  woken "the deleted order's poll" deleted `shouldReturn` (404, KM.fromList [("type", "not_found")])
