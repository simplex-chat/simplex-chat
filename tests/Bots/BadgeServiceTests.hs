{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeServiceTests where

import BadgeService.Options
import BadgeService.Service
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Simplex.Chat.Controller (ChatConfig)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import System.FilePath ((</>))
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
  bsLink <-
    withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \bs -> do
      bs ##> "/ad pq_ratchet=on"
      (sLink, _) <- getContactLinks bs True
      pure sLink
  let opts = mkBadgeServiceOpts ps
  runBadgeService testCfg opts $
    withNewTestChatCfg ps testCfg "client" bobProfile $ \client ->
      test client bsLink

runBadgeService :: ChatConfig -> BadgeServiceOpts -> IO () -> IO ()
runBadgeService cfg opts action = do
  t <- forkIO $ badgeService opts cfg
  threadDelay 500000
  action `finally` killThread t

testBadgeServiceRedeemUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemUnsupported ps =
  withBadgeService ps $ \client bsLink -> do
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"
