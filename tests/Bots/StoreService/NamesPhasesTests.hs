{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesPhasesTests (namesPhasesTests) where

import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (reveal)
import Bots.StoreService.WebTests (bounded, jsonObject, rcv, statusOf, webGet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, unless)
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
import StoreService.Names.Phases (RegistrationId (..), phasesCount, publish)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, updateRegistration)
import StoreService.Providers (Funded (..), PaymentSignal (..))
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Waiters (publishPayment, waitingCount)
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
    it "holds a paid name invoice on its registration's version alone" testPaidHoldsOnVersion
    it "answers at once an invoice paid after its order closed" testLatePaidAnswersAtOnce
    it "wakes an invoice poll held on its status and version by a version change" testOpenWakesOnVersion
  describe "time" $ do
    it "wakes an unpaid invoice held on its status at its expiry, reading it expired" testExpiryWakes
    it "reads a funded open invoice past its expiry as open, and holds it" testFundedStaysOpen
    it "reads a committed registration with no reveal past revealBefore as failed, waking a held poll" testLapseReadsFailed
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

-- | A poll still running after this was held, not answered.
parkDelay :: Int
parkDelay = 200000

-- | How long a woken poll may take to answer; far below the hold.
prompt :: Int
prompt = 3000000

shortHold :: Int
shortHold = 500000

get :: Ctx -> String -> IO (Int, J.Object)
get Ctx {cClient} path = webGet cClient path >>= \r -> (statusOf r,) <$> jsonObject r

-- | A poll that is still held after parkDelay.
held :: Ctx -> String -> IO (Async (Int, J.Object))
held ctx path = do
  poll <- async $ get ctx path
  threadDelay parkDelay
  Async.poll poll >>= \answered -> unless (isNothing answered) $ expectationFailure (path <> " was answered at once: " <> show answered)
  pure poll

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
following status version paid full =
  "?wait=" <> statusText <> "&seenVersion=" <> show version <> "&seenPaid=" <> T.unpack paid <> "&seenFull=" <> (if full then "1" else "0")
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
    invoicePoll <- held ctx $ invoicePath inv <> following ISPaid v settledPaid True
    registrationPoll <- held ctx $ registrationPath nrId <> seenVersion v
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
  (fst <$> answeredAtOnce ctx (invoicePath inv <> "?wait=paid&seenPaid=" <> T.unpack settledPaid <> "&seenFull=1")) `shouldReturn` 200
  poll <- held ctx $ invoicePath inv <> following ISPaid nrVersion settledPaid True
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
  poll <- held ctx $ invoicePath inv <> following ISOpen nrVersion "" False
  v' <- change ctx nrId $ \r -> r {nrError = Just "Checking the payment"}
  (_, o) <- woken "open invoice poll" poll
  (key "status" o, key "version" (registrationOf o)) `shouldBe` (Just "open", Just (J.toJSON v'))

-- | The stub's crypto invoice expires 45 minutes after the order; the clock is set just short of it.
testExpiryWakes :: IO ()
testExpiryWakes = bounded "expiry wakes" $ withPolls holdMicros $ \Polls {pCtx = ctx, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  Just InvoiceRow {irExpiresAt} <- getInvoice (cStore ctx) inv
  NameRegistration {nrVersion} <- invoiceRegistration ctx inv
  pSetNow $ addUTCTime (-1) irExpiresAt
  poll <- held ctx $ invoicePath inv <> following ISOpen nrVersion "" False
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
  poll <- held ctx $ invoicePath inv <> following ISOpen nrVersion "0.00078" False
  cancel poll

testLapseReadsFailed :: IO ()
testLapseReadsFailed = bounded "lapse" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cOrders}, pSetNow} -> do
  (inv, _) <- placed ctx aliceOrder
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  now <- oeNow cOrders
  let revealBefore = seconds now + 600
      lapsedAt = posixSecondsToUTCTime (fromInteger revealBefore)
  pSetNow $ addUTCTime (-1) lapsedAt
  v <- change ctx nrId $ \r -> r {nrPhase = PCommitted, nrRevealAfter = Just (fromInteger $ revealBefore - 3600), nrRevealBefore = Just (fromInteger revealBefore)}
  (_, r) <- get ctx (registrationPath nrId)
  key "phase" r `shouldBe` Just "committed"
  poll <- held ctx $ registrationPath nrId <> seenVersion v
  (status, lapsed) <- woken "registration poll at the lapse" poll
  (status, key "phase" lapsed, key "error" lapsed, key "version" lapsed) `shouldBe` (200, Just "failed", Just "The name was not signed in time", Just (J.toJSON v))
  (_, o) <- get ctx (invoicePath inv)
  (key "phase" (registrationOf o), key "error" (registrationOf o)) `shouldBe` (Just "failed", Just "The name was not signed in time")
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
testWaitingCount = bounded "waiting count" $ withPolls holdMicros $ \Polls {pCtx = ctx@Ctx {cWeb, cOrders}} -> do
  (paid, _) <- placed ctx aliceOrder
  settleNow ctx paid settled `shouldReturn` Right ISPaid
  NameRegistration {nrId, nrVersion} <- invoiceRegistration ctx paid
  polls <- mapM (held ctx) [registrationPath nrId <> seenVersion nrVersion, invoicePath paid <> following ISPaid nrVersion settledPaid True]
  (waitingCount (weWaiters cWeb), phasesCount (oePhases cOrders)) `shouldReturnPair` (0, 1)
  (open, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
  NameRegistration {nrVersion = openVersion} <- invoiceRegistration ctx open
  openPoll <- held ctx $ invoicePath open <> following ISOpen openVersion "" False
  (waitingCount (weWaiters cWeb), phasesCount (oePhases cOrders)) `shouldReturnPair` (1, 2)
  mapM_ cancel (openPoll : polls)
  where
    shouldReturnPair (a, b) expected = ((,) <$> a <*> b) `shouldReturn` expected
