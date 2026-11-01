{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesWiringTests (namesWiringTests) where

import Control.Monad (replicateM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Manager, Response, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseHeaders, responseStatus)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Method, hCacheControl, hContentType, status200)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import StoreService.Config (ListenerConfig (..))
import StoreService.Web.Http
import StoreService.Web.Names (Route (..), namesApp)
import Test.Hspec

namesWiringTests :: Spec
namesWiringTests = describe "StoreService names: wiring" $ do
  describe "router" $ do
    it "answers a known path with another method 405 with Allow, spending no token" testWrongMethod
    it "leaves an unknown path to the caller" testUnknownPath

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
