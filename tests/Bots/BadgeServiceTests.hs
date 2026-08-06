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
import Data.List (isInfixOf)
import Simplex.Chat.Controller (ChatConfig)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import System.FilePath ((</>))
import Test.Hspec hiding (it)

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "creates a DR address when none exists" testBadgeServiceCreatesDRAddress
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

-- Exercises the address-creation branch of initializeBotAddress' that withBadgeService bypasses
-- (it pre-creates the address to keep the RPC round-trip focused). This is the only test that
-- covers what a fresh deployment produces.
--
-- Assertion is on the full link's "rk=" query parameter: simplexmq
-- Simplex/Messaging/Agent/Protocol.hs:1174 appends "rk=" only when the ratchet keys are Just,
-- so its presence proves the address is double-ratchet. A non-DR address would still parse and
-- look valid, but every service request to it would fail with ASENotDRAddress (Agent.hs:1739).
testBadgeServiceCreatesDRAddress :: HasCallStack => TestParams -> IO ()
testBadgeServiceCreatesDRAddress ps = do
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_bs -> pure ()
  let opts = mkBadgeServiceOpts ps
  runBadgeService testCfg opts (pure ())
  withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (_, fullLink) <- getContactLinks bs False
    bs <## "auto_accept off"
    ("rk=" `isInfixOf` fullLink) `shouldBe` True

testBadgeServiceRedeemUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemUnsupported ps =
  withBadgeService ps $ \client bsLink -> do
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    -- The exact serialized string is deterministic today: the aeson fork's KeyMap is Map-backed
    -- with flag `ordered-keymap` defaulted to True (aeson.cabal:52; KeyMap.hs:143), so keys sort
    -- alphabetically. If the future typed handler switches to deriveJSON/enumJSON on the
    -- response, the encoder will emit declaration order and this assertion will need to be
    -- updated (and BadgeServiceErrorCode's snake_case ToJSON in src/Simplex/Chat/Badges/Service.hs
    -- - which this test transitively depends on - reviewed at the same time).
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"
