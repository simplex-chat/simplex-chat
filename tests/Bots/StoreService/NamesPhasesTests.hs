{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesPhasesTests (namesPhasesTests, change, held, woken, prompt, registrationPath, seenVersion) where

import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (reveal)
import Bots.StoreService.WebTests (bounded, jsonObject, rcv, statusOf, webGet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait, waitCatchSTM)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, atomically, check, orElse)
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), phasesWaiting, publish)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, updateRegistration)
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
    it "wakes a held invoice poll and a held registration poll on each change: phase, label set and cleared, reveal times, error" testEachChangeWakes
    it "holds a paid name invoice on its registration's version alone, and answers at once without a valid version" testPaidHoldsOnVersion
    it "answers at once an invoice paid after its order closed" testLatePaidAnswersAtOnce
    it "wakes an open invoice poll by a version change, and holds it on its status without a valid version" testOpenWakesOnVersion
  describe "time" $ do
    it "wakes an unpaid invoice held on its status at its expiry, reading it expired" testExpiryWakes
    it "reads a funded open invoice past its expiry as open, and holds it" testFundedStaysOpen
    it "reads a committed registration with no reveal past revealBefore as failed, waking held registration and invoice polls" testLapseReadsFailed
  describe "registration poll" $ do
    it "answers at once without seenVersion, with another version, or with one that is not a number" testAnswersAtOnce
    it "answers unchanged after the hold" testHoldTimesOut
    it "answers an unknown registration 404" testUnknownRegistration
    it "leaves waitingCount to polls waiting on a payment" testWaitingCount

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
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders hold rid}
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

key :: Text -> J.Object -> Maybe J.Value
key k = KM.lookup (K.fromText k)

seconds :: UTCTime -> Integer
seconds = floor . utcTimeToPOSIXSeconds

-- | The settled payment's marks: its crypto amount and paid in full.
settledPaid :: Text
settledPaid = "0.00156"

-- tests

-- | Paid and queued at version 2; each change below is one the worker, a reveal or a re-commit makes.
testEachChangeWakes :: IO ()
testEachChangeWakes = bounded "each change wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  let day = seconds orderTime + 86400
      changes :: [(String, NameRegistration -> NameRegistration, Text, J.Value)]
      changes =
        [ ("phase", \r -> r {nrPhase = PCommitting}, "phase", "committing"),
          ("committed", \r -> r {nrPhase = PCommitted, nrRevealAfter = Just (fromInteger day), nrRevealBefore = Just (fromInteger $ day + 86400)}, "revealBefore", J.toJSON (day + 86400)),
          ("label set", \r -> r {nrReveal = Just reveal}, "label", "alicebakery"),
          ("label cleared", \r -> r {nrReveal = Nothing}, "label", J.Null),
          ("reveal times after a re-commit", \r -> r {nrRevealAfter = Just (fromInteger $ day + 60), nrRevealBefore = Just (fromInteger $ day + 86460)}, "revealBefore", J.toJSON (day + 86460)),
          ("error", \r -> r {nrPhase = PFailed, nrError = Just "The commit transaction failed"}, "error", "The commit transaction failed")
        ]
  forM_ changes $ \(what, f, k, expected) -> do
    v <- nrVersion <$> invoiceRegistration ctx inv
    invoicePoll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid v settledPaid True
    registrationPoll <- held ctx (0, 1) $ registrationPath nrId <> seenVersion v
    v' <- change ctx nrId f
    (status, o) <- woken (what <> ": invoice poll") invoicePoll
    (status, key "status" o, key "version" (registrationOf o), key k (registrationOf o)) `shouldBe` (200, Just "paid", Just (J.toJSON v'), Just expected)
    (status', r) <- woken (what <> ": registration poll") registrationPoll
    (status', key "version" r, key k r) `shouldBe` (200, Just (J.toJSON v'), Just expected)

testPaidHoldsOnVersion :: IO ()
testPaidHoldsOnVersion = bounded "paid holds on version" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cWeb}} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  -- without a version there is nothing a paid invoice could wait for
  forM_ [Nothing, Just "two"] $ \version ->
    (fst <$> answeredAtOnce ctx (invoicePath inv <> followingAs ISPaid version settledPaid True)) `shouldReturn` 200
  poll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid nrVersion settledPaid True
  atomically $ publishPayment (weWaiters cWeb) inv
  threadDelay parkDelay
  Async.poll poll >>= (`shouldSatisfy` isNothing)
  v' <- change ctx nrId $ \r -> r {nrPhase = PCommitting}
  (_, o) <- woken "paid invoice poll" poll
  (key "version" (registrationOf o), key "phase" (registrationOf o)) `shouldBe` (Just (J.toJSON v'), Just "committing")
  -- marks other than the page saw answer at once
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISPaid v' "0.001" True)) `shouldReturn` 200

testLatePaidAnswersAtOnce :: IO ()
testLatePaidAnswersAtOnce = bounded "late paid" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  fst <$> post ctx (invoicePath inv <> "/cancel") "" `shouldReturn` 200
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  (status, o) <- answeredAtOnce ctx $ invoicePath inv <> following ISPaid nrVersion settledPaid True
  (status, key "status" o, key "phase" (registrationOf o)) `shouldBe` (200, Just "paid", Just "unpaid")

testOpenWakesOnVersion :: IO ()
testOpenWakesOnVersion = bounded "open wakes on version" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  -- a version the page has not seen answers at once
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISOpen (nrVersion - 1) "" False)) `shouldReturn` 200
  poll <- held ctx (1, 1) $ invoicePath inv <> following ISOpen nrVersion "" False
  v' <- change ctx nrId $ \r -> r {nrError = Just "Checking the payment"}
  (_, o) <- woken "open invoice poll" poll
  (key "status" o, key "version" (registrationOf o)) `shouldBe` (Just "open", Just (J.toJSON v'))
  -- a seenVersion that is no version leaves the hold on the status and payment alone
  byStatus <- held ctx (1, 0) $ invoicePath inv <> followingAs ISOpen (Just "two") "" False
  settleNow ctx inv (SigFunded (rcv 5000 (Just "0.00078")) PaidInPart) `shouldReturn` Right ISOpen
  (_, paidInPart) <- woken "open invoice poll without a version" byStatus
  (key "status" paidInPart, key "cryptoAmountPaid" paidInPart) `shouldBe` (Just "open", Just "0.00078")

-- | The stub's crypto invoice expires 45 minutes after the order; the clock is set just short of it.
testExpiryWakes :: IO ()
testExpiryWakes = bounded "expiry wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  Just InvoiceRow {irExpiresAt} <- getInvoice (cStore ctx) inv
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  pSetNow $ addUTCTime (negate shortOfMoment) irExpiresAt
  poll <- held ctx (1, 0) $ invoicePath inv <> followingAs ISOpen Nothing "" False
  (status, o) <- woken "unpaid invoice poll" poll
  (status, key "status" o, key "version" (registrationOf o)) `shouldBe` (200, Just "expired", Just (J.toJSON nrVersion))
  -- read so, not written: the sweep or the next order closes it
  (irStatus <$>) <$> getInvoice (cStore ctx) inv `shouldReturn` Just ISOpen
  (fst <$> answeredAtOnce ctx (invoicePath inv <> following ISOpen nrVersion "" False)) `shouldReturn` 200

testFundedStaysOpen :: IO ()
testFundedStaysOpen = bounded "funded stays open" $ withPolls holdMicros $ \Polls {pCtx = ctx, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv (SigFunded (rcv 5000 (Just "0.00078")) PaidInPart) `shouldReturn` Right ISOpen
  Just InvoiceRow {irExpiresAt} <- getInvoice (cStore ctx) inv
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  pSetNow $ addUTCTime 60 irExpiresAt
  (status, o) <- get ctx (invoicePath inv)
  (status, key "status" o) `shouldBe` (200, Just "open")
  poll <- held ctx (1, 1) $ invoicePath inv <> following ISOpen nrVersion "0.00078" False
  cancel poll

testLapseReadsFailed :: IO ()
testLapseReadsFailed = bounded "lapse" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cOrders}, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  now <- oeNow cOrders
  let revealBefore = seconds now + 600
      lapsedAt = posixSecondsToUTCTime (fromInteger revealBefore)
  pSetNow $ addUTCTime (negate shortOfMoment) lapsedAt
  v <- change ctx nrId $ \r -> r {nrPhase = PCommitted, nrRevealAfter = Just (fromInteger $ revealBefore - 3600), nrRevealBefore = Just (fromInteger revealBefore)}
  (_, r) <- get ctx (registrationPath nrId)
  key "phase" r `shouldBe` Just "committed"
  invoicePoll <- held ctx (0, 1) $ invoicePath inv <> following ISPaid v settledPaid True
  poll <- held ctx (0, 1) $ registrationPath nrId <> seenVersion v
  (status, lapsed) <- woken "registration poll at the lapse" poll
  (status, key "phase" lapsed, key "error" lapsed, key "version" lapsed) `shouldBe` (200, Just "failed", Just "The name was not signed in time", Just (J.toJSON v))
  (_, o) <- woken "invoice poll at the lapse" invoicePoll
  (key "status" o, key "phase" (registrationOf o), key "error" (registrationOf o)) `shouldBe` (Just "paid", Just "failed", Just "The name was not signed in time")
  (nrPhase <$> invoiceRegistration ctx inv) `shouldReturn` PCommitted
  -- one with a reveal set stays committed past revealBefore
  revealed <- change ctx nrId $ \r' -> r' {nrReveal = Just reveal}
  pSetNow $ addUTCTime 3600 lapsedAt
  (_, r') <- get ctx (registrationPath nrId)
  (key "phase" r', key "label" r', key "version" r') `shouldBe` (Just "committed", Just "alicebakery", Just (J.toJSON revealed))

testAnswersAtOnce :: IO ()
testAnswersAtOnce = bounded "at once" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId = rid@(RegistrationId ridText), nrVersion} <- invoiceRegistration ctx inv
  forM_ ["", seenVersion (nrVersion + 1), seenVersion (nrVersion - 1), "?seenVersion=one", "?seenVersion=-1", "?seenVersion=99999999999999999999"] $ \q -> do
    (status, r) <- answeredAtOnce ctx (registrationPath rid <> q)
    (q, status, key "id" r, key "version" r, key "phase" r) `shouldBe` (q, 200, Just (J.String ridText), Just (J.toJSON nrVersion), Just "unpaid")

testHoldTimesOut :: IO ()
testHoldTimesOut = bounded "hold times out" $ withPolls shortHold $ \Polls {pCtx = ctx} -> do
  (inv, _) <- placed ctx aliceOrder
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx inv
  started <- getCurrentTime
  (status, r) <- get ctx $ registrationPath nrId <> seenVersion nrVersion
  waited <- (`diffUTCTime` started) <$> getCurrentTime
  (status, key "version" r) `shouldBe` (200, Just (J.toJSON nrVersion))
  waited `shouldSatisfy` (>= (fromIntegral shortHold / 1000000 :: NominalDiffTime))

testUnknownRegistration :: IO ()
testUnknownRegistration = bounded "unknown" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (status, o) <- answeredAtOnce ctx (registrationPath (RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g") <> seenVersion 1)
  (status, o) `shouldBe` (404, KM.fromList [("error", "not_found")])

-- | The poller reads providers sooner while a page waits on a payment; a hold on a version waits on none.
testWaitingCount :: IO ()
testWaitingCount = bounded "waiting count" $ withPolls holdMicros $ \Polls {pCtx = ctx} -> do
  (paid, _) <- placed ctx aliceOrder
  settleNow ctx paid settled `shouldReturn` Right ISPaid
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx paid
  polls <- mapM (held ctx (0, 1)) [registrationPath nrId <> seenVersion nrVersion, invoicePath paid <> following ISPaid nrVersion settledPaid True]
  atomically (subscriptions ctx) `shouldReturn` (0, 2)
  (open, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
  NameRegistration {nrVersion = openVersion} <- invoiceRegistration ctx open
  openPoll <- held ctx (1, 1) $ invoicePath open <> following ISOpen openVersion "" False
  atomically (subscriptions ctx) `shouldReturn` (1, 3)
  mapM_ cancel (openPoll : polls)
