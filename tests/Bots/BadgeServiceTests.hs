{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeServiceTests where

import BadgeService.Options
import BadgeService.Service
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (finally)
import Simplex.Chat.Controller (ChatConfig)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec hiding (it)

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should respond with unsupported_version to redeem" testBadgeServiceRedeemUnsupported

badgeProfile :: Profile
badgeProfile = Profile {displayName = "SimpleX Badges", fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

serviceDbPrefix :: FilePath
serviceDbPrefix = "badge_service"

mkBadgeServiceOpts :: TestParams -> BadgeServiceOpts
mkBadgeServiceOpts TestParams {tmpPath = ps} =
  BadgeServiceOpts
    { coreOptions =
        testCoreOpts
          { dbOptions =
              (dbOptions testCoreOpts)
#if defined(dbPostgres)
                {dbSchemaPrefix = "client_" <> serviceDbPrefix}
#else
                {dbFilePrefix = ps </> serviceDbPrefix}
#endif
          },
      serviceName = "SimpleX Badges",
      clientService = True,
      noAddress = False,
      runCLI = False,
      testing = True
    }

withBadgeService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withBadgeService ps test = do
  let opts = mkBadgeServiceOpts ps
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService testCfg opts (pure ())
  -- Reopen the DB to read the link the service created.
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
  runBadgeService testCfg opts $
    withNewTestChatCfg ps testCfg "client" bobProfile $ \client ->
      test client bsLink

runBadgeService :: ChatConfig -> BadgeServiceOpts -> IO () -> IO ()
runBadgeService cfg opts action = do
  ready <- newEmptyTMVarIO
  t <- forkIO $ badgeService_ (atomically $ putTMVar ready ()) opts cfg
  timeout 10000000 (atomically $ takeTMVar ready) >>= \case
    Nothing -> killThread t >> fail "badge service failed to signal ready within 10s"
    Just () -> action `finally` killThread t

testBadgeServiceRedeemUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemUnsupported ps =
  withBadgeService ps $ \client bsLink -> do
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"
