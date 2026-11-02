{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesWiringTests (namesWiringTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, concurrently_, replicateConcurrently_, wait)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (replicateM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Client (Manager, Response, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseHeaders, responseStatus)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Method, hCacheControl, hContentType, status200)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.SimplexName (labelHash)
import StoreService.Config (ListenerConfig (..))
import StoreService.Names.Phases
import StoreService.Names.Turns
import StoreService.Web.Http
import StoreService.Web.Names (Route (..), namesApp)
import System.Timeout (timeout)
import Test.Hspec

namesWiringTests :: Spec
namesWiringTests = describe "StoreService names: wiring" $ do
  describe "phases" $ do
    it "wakes a waiter on a newer published version, not on an older or the same one" testPublishWakesOnNewer
    it "answers at once when the version read is not the one seen" testVersionAlreadyMoved
    it "catches a publish that lands between subscribing and reading" testPublishBeforeRead
    it "gives up after the hold when nothing is published" testHoldTimesOut
    it "is not woken by another registration's publish" testOtherRegistration
    it "drops a registration's watch once no waiter is left" testPhasesDropIdleWatches
  describe "router" $ do
    it "answers a known path with another method 405 with Allow, spending no token" testWrongMethod
    it "leaves an unknown path to the caller" testUnknownPath
  describe "turns" $ do
    it "runs two holders of one label hash one after the other" testOneLabelSerialized
    it "runs holders of different label hashes at once" testDifferentLabelsConcurrent
    it "runs two holders of one code hash one after the other" testOneCodeSerialized
    it "holds both label hashes in withLabelTurns, and one when they are equal" testLabelTurnsHoldBoth
    it "never deadlocks withLabelTurns A->B against B->A" testLabelTurnsNoDeadlock
    it "releases a turn whose holder throws" testTurnReleasedOnThrow
    it "drops a lock once nobody holds or waits for it" testTurnsDropIdleLocks

-- | Long enough that only a missed wake-up could make a test wait it out.
longHold :: Int
longHold = 10000000

-- | How long a test waits for something that must happen promptly.
prompt :: Int
prompt = 2000000

parkDelay :: Int
parkDelay = 100000

rid1, rid2 :: RegistrationId
rid1 = RegistrationId "reg-1"
rid2 = RegistrationId "reg-2"

testPublishWakesOnNewer :: IO ()
testPublishWakesOnNewer = do
  phases <- newPhases
  parked <- newEmptyMVar
  waiter <- async $ awaitChange phases rid1 (putMVar parked () >> pure 3) 3 longHold
  takeMVar parked
  atomically $ publish phases rid1 2
  atomically $ publish phases rid1 3
  threadDelay parkDelay
  Async.poll waiter >>= (`shouldSatisfy` isNothing)
  atomically $ publish phases rid1 4
  timeout prompt (wait waiter) `shouldReturn` Just True

testVersionAlreadyMoved :: IO ()
testVersionAlreadyMoved = do
  phases <- newPhases
  timeout prompt (awaitChange phases rid1 (pure 5) 3 longHold) `shouldReturn` Just True

testPublishBeforeRead :: IO ()
testPublishBeforeRead = do
  phases <- newPhases
  -- the row read here is the old one, though version 4 was committed and published while it ran
  let staleRead = atomically (publish phases rid1 4) >> pure 3
  timeout prompt (awaitChange phases rid1 staleRead 3 longHold) `shouldReturn` Just True

testHoldTimesOut :: IO ()
testHoldTimesOut = do
  phases <- newPhases
  timeout prompt (awaitChange phases rid1 (pure 3) 3 parkDelay) `shouldReturn` Just False

testOtherRegistration :: IO ()
testOtherRegistration = do
  phases <- newPhases
  parked <- newEmptyMVar
  waiter <- async $ awaitChange phases rid1 (putMVar parked () >> pure 1) 1 (3 * parkDelay)
  takeMVar parked
  atomically $ publish phases rid2 7
  timeout prompt (wait waiter) `shouldReturn` Just False

testPhasesDropIdleWatches :: IO ()
testPhasesDropIdleWatches = do
  phases <- newPhases
  parked <- newEmptyMVar
  woken <- async $ awaitChange phases rid1 (putMVar parked () >> pure 1) 1 longHold
  cancelled <- async $ awaitChange phases rid2 (putMVar parked () >> pure 1) 1 longHold
  replicateM_ 2 $ takeMVar parked
  phasesCount phases `shouldReturn` 2
  atomically $ publish phases rid1 2
  timeout prompt (wait woken) `shouldReturn` Just True
  Async.cancel cancelled
  awaitChange phases rid1 (pure 2) 2 parkDelay `shouldReturn` False
  phasesCount phases `shouldReturn` 0

type Client = (Manager, String)

probeLimit :: Limit
probeLimit = Limit {lmName = "probe", lmPerMinute = 1}

withProbeRouter :: (Client -> IO a) -> IO a
withProbeRouter action = do
  http <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesStaticDir = "", lServeWebapp = False, lBadgesExportDir = Nothing, lTrustForwardedFor = False, lNamesStaticDir = Nothing, lNamesExportDir = Nothing, lNamesPort = Nothing}
  let routes = \case
        ["api", "probe", _] -> Just Route {rMethod = "GET", rLimit = probeLimit, rHandler = \_ respond -> respond (jsonResponse status200 (J.object []))}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> action (mgr, "http://127.0.0.1:" <> show port)

send :: Client -> Method -> String -> IO (Response LB.ByteString)
send (mgr, base) verb target = do
  req <- parseRequest (base <> target)
  httpLbs req {HTTP.method = verb} mgr

errorTag :: Response LB.ByteString -> Maybe J.Value
errorTag r = case J.decode (responseBody r) of
  Just (J.Object o) -> KM.lookup "error" o
  _ -> Nothing

testWrongMethod :: IO ()
testWrongMethod = withProbeRouter $ \client -> do
  replicateM_ 2 $ do
    r <- send client "POST" "/api/probe/x"
    statusCode (responseStatus r) `shouldBe` 405
    lookup "Allow" (responseHeaders r) `shouldBe` Just "GET"
    lookup hContentType (responseHeaders r) `shouldBe` Just "application/json"
    lookup hCacheControl (responseHeaders r) `shouldBe` Just "no-store"
    errorTag r `shouldBe` Just (J.String "method_not_allowed")
  allowed <- send client "GET" "/api/probe/x"
  statusCode (responseStatus allowed) `shouldBe` 200
  limitedR <- send client "GET" "/api/probe/x"
  statusCode (responseStatus limitedR) `shouldBe` 429
  errorTag limitedR `shouldBe` Just (J.String "rate_limited")
  lookup "Retry-After" (responseHeaders limitedR) `shouldSatisfy` maybe False (BC.all (`elem` ['0' .. '9']))

testUnknownPath :: IO ()
testUnknownPath = withProbeRouter $ \client -> do
  r <- send client "POST" "/api/probe"
  statusCode (responseStatus r) `shouldBe` 404
  errorTag r `shouldBe` Just (J.String "not_found")

-- | Runs the holders concurrently and returns the most that were inside at once.
mostInside :: Int -> ((IO () -> IO ()) -> IO ()) -> IO Int
mostInside n holder = do
  inside <- newTVarIO (0 :: Int)
  most <- newTVarIO 0
  let enter = atomically $ do
        k <- (+ 1) <$> readTVar inside
        writeTVar inside k
        modifyTVar' most (max k)
      leave = atomically $ modifyTVar' inside (subtract 1)
  replicateConcurrently_ n $ holder (\work -> enter >> work >> leave)
  readTVarIO most

testOneLabelSerialized :: IO ()
testOneLabelSerialized = do
  turns <- newTurns
  r <- timeout (5 * prompt) $ mostInside 8 $ \inTurn -> withLabelTurn turns (labelHash "alice1") (inTurn (threadDelay 20000))
  r `shouldBe` Just 1

testOneCodeSerialized :: IO ()
testOneCodeSerialized = do
  turns <- newTurns
  r <- timeout (5 * prompt) $ mostInside 8 $ \inTurn -> withCodeTurn turns (BC.replicate 32 'c') (inTurn (threadDelay 20000))
  r `shouldBe` Just 1

-- | Each holder waits inside its turn for the other to be inside its own, which only concurrent turns allow.
testDifferentLabelsConcurrent :: IO ()
testDifferentLabelsConcurrent = do
  turns <- newTurns
  aInside <- newEmptyMVar
  bInside <- newEmptyMVar
  r <-
    timeout prompt $
      concurrently_
        (withLabelTurn turns (labelHash "alice1") (putMVar aInside () >> takeMVar bInside))
        (withLabelTurn turns (labelHash "bobby2") (putMVar bInside () >> takeMVar aInside))
  r `shouldBe` Just ()

testLabelTurnsHoldBoth :: IO ()
testLabelTurnsHoldBoth = do
  turns <- newTurns
  let a = labelHash "alice1"
      b = labelHash "bobby2"
  withLabelTurns turns a b $ do
    timeout parkDelay (withLabelTurn turns a (pure ())) `shouldReturn` Nothing
    timeout parkDelay (withLabelTurn turns b (pure ())) `shouldReturn` Nothing
  timeout prompt (withLabelTurns turns a a (pure ())) `shouldReturn` Just ()

testLabelTurnsNoDeadlock :: IO ()
testLabelTurnsNoDeadlock = do
  turns <- newTurns
  let a = labelHash "alice1"
      b = labelHash "bobby2"
      rounds = 200
  r <-
    timeout (5 * prompt) $
      concurrently_
        (replicateConcurrently_ rounds (withLabelTurns turns a b (threadDelay 50)))
        (replicateConcurrently_ rounds (withLabelTurns turns b a (threadDelay 50)))
  r `shouldBe` Just ()

data Boom = Boom
  deriving (Show)

instance E.Exception Boom

testTurnReleasedOnThrow :: IO ()
testTurnReleasedOnThrow = do
  turns <- newTurns
  let a = labelHash "alice1"
  E.try (withLabelTurn turns a (E.throwIO Boom)) >>= \case
    Left Boom -> pure ()
    Right () -> expectationFailure "the holder's exception must reach the caller"
  timeout prompt (withLabelTurn turns a (pure ())) `shouldReturn` Just ()

testTurnsDropIdleLocks :: IO ()
testTurnsDropIdleLocks = do
  turns <- newTurns
  let a = labelHash "alice1"
  withLabelTurns turns a (labelHash "bobby2") $ withCodeTurn turns (BC.replicate 32 'c') $ turnsCount turns `shouldReturn` 3
  E.handle (\Boom -> pure ()) $ withLabelTurn turns a (E.throwIO Boom)
  -- the waiter is cancelled by the timeout while the holder keeps the turn
  withLabelTurn turns a $ timeout parkDelay (withLabelTurn turns a (pure ())) `shouldReturn` Nothing
  turnsCount turns `shouldReturn` 0
