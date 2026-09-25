{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Members' messages reach a console over separate connections in no fixed order, so gating is asserted from the bot's store.
module Bots.BadgeService.GroupIntegrationTests (badgeGroupIntegrationTests) where

import BadgeService.Config (BadgeIssuerKey (..), GroupConfig (..))
import BadgeService.Group (GroupAction (..), GroupEvent (..), codeInTracker, ensureManagedGroup)
import BadgeService.Group.Command (maxUses)
import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service (ServiceState (..), badgeService, newServiceState)
import BadgeService.Store (ManagedGroup (..), getManagedGroup)
import Bots.BadgeService.BotTests (badgeBotName, badgeProfile, badgeTypeOf, credentialOf, entryOf, issueCode, issueRaw, mkBadgeServiceOpts, newPurchaseKeys, redeemWithQueue, requestObject, revokeRaw, serviceDbPrefix, shouldAnswerError, statementOf, stopBadgeService, testIssuerKeyIdx)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll, waitCatchSTM)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, orElse, readTMVar, readTQueue, readTVarIO, tryReadTMVar, writeTQueue)
import Control.Exception (IOException, bracket, catch, finally, throwIO, try)
import Control.Monad (forM_, guard, mfilter, replicateM_, unless, void, when)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket, tupleToHostAddress)
import Network.Wai.Handler.Warp (openFreePort)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (formatBadgeCode, parseBadgeCode, randomBadgeCode)
import Simplex.Chat.Badges.Service
import Simplex.Chat.Controller (ChatCommand (APIDeleteChatItem), ChatConfig (..), ChatController (..), ChatResponse (CRChatItemsDeleted))
import Simplex.Chat.Core (sendChatCmd, sendChatCmdStr)
import Simplex.Chat.Messages (ChatRef (ChatRef), ChatType (CTGroup))
import Simplex.Chat.Messages.CIContent (CIDeleteMode (CIDMInternal))
import Simplex.Chat.Types (AgentInvId (..))
import Simplex.Chat.Types.Preferences (ChatBotCommand (..), FullDeleteGroupPreference (..), GroupFeatureEnabled (..), GroupFeatureI (..), GroupPreferences (..), HistoryGroupPreference (..), SGroupFeature (..), commands_, emptyGroupPrefs, getGroupPreference)
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Protocol (AConnShortLink (..), ConnShortLink (..), ContactConnType (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BBS (bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Util (tshow)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Timeout (timeout)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (FromRow, Only (..))
#else
import Database.SQLite.Simple (FromRow, Only (..))
#endif
import Test.Hspec hiding (it)

badgeGroupIntegrationTests :: SpecWith TestParams
badgeGroupIntegrationTests = do
  it "creates the group and a join link on first start; on restart reuses it, re-advertises commands and keeps other preferences" testGroupCreateReuse
  it "keeps the group's name and description when the configured ones change" testConfigNotApplied
  it "promotes the first joiner to owner and leaves a later joiner a member" testFirstJoinerPromoted
  it "promotes nobody after the first promotion failed" testFailedPromotionNotRetried
  it "promotes the earliest member on start when their join was never handled" testMissedOwnerPromotedOnStart
  it "promotes the earliest member, not a later joiner, when no owner was set up" testEarliestPromotedOnLaterJoin
  it "skips a member still joining when promoting on start" testJoiningMemberSkippedOnStart
  it "only records an owner made by hand on start, promoting nobody" testHandMadeOwnerKeptOnStart
  it "promotes a member on start when the owner made by hand has left" testLeftOwnerReplacedOnStart
  it "promotes nobody on start once the owner was set up, even with no owner left" testSetUpOwnerNotReplacedOnStart
  it "does not serve a group its owner deleted" testDeletedGroupNotServed
  it "does not serve a group the service itself deleted" testOwnDeletedGroupNotServed
  it "drops the old group's trackers when it creates a new group" testReplacedGroupDropsTrackers
  it "stops rather than create a second group when the group lookup fails" testGroupLookupFailureStops
  it "stops the service's lanes when its runner is cancelled" testCancelStopsLanes
  it "issues on a privileged member's command, ignores a plain member's" testRoleGatedIssue
  it "ignores a command from a privileged member blocked for all" testBlockedMemberCommandIgnored
  it "ignores a command sent in a member support scope" testSupportScopeIgnored
  it "ignores a command sent as a live message" testLiveMessageIgnored
  it "ignores an event for another group" testOtherGroupEventIgnored
  it "issues a batch of redeemable single-use codes of the asked type and months on /bulk" testBulkIssue
  it "issues an /issue code for the months the command asked for" testIssueMonths
  it "answers a failed /issue or //issue without naming what failed in the store" testIssueFailureIsOpaque
  it "lists the codes a /bulk issued before an insert failed" testBulkPartialFailureListsIssued
  it "answers a failed /revoke without naming what failed in the store" testRevokeFailureIsOpaque
  it "tracks a multi-use code across redemptions and refuses it when used up" testMultiUseTracker
  it "keeps the counter and posts no second notice for a stale refresh of a used-up code" testStaleRefreshIgnored
  it "announces a second multi-use code used up on the claim that uses it up" testSecondMultiUseCodeExhausted
  it "refreshes the tracker for a redemption served with no group lane" testLanelessRedeemRefreshesTracker
  it "refreshes the tracker for a redemption from the service's request queue" testQueuedRequestRefreshesTracker
  it "refreshes the tracker inline for a queued redemption after a restart without [group]" testQueuedRequestWithoutGroupConfig
  it "reposts and re-anchors the tracker past the edit window" testTrackerRepost
  it "reposts and re-anchors the tracker when the core refuses the edit" testRefusedEditReposted
  it "keeps the tracker anchored when the repost cannot be sent" testFailedRepostDropped
  it "never reposts a tracker deleted on the service's side, publishing the code no further" testDeletedTrackerNotReposted
  it "never reposts a tracker an owner deleted for everyone" testModeratedTrackerNotReposted
  it "revokes a code whose tracker was deleted on the service's side without reposting it" testDeletedTrackerNotRepostedOnRevoke
  it "posts no notice when a code whose tracker was deleted is used up" testDeletedTrackerNoNotice
  it "revokes a code whose tracker an owner deleted for everyone without reposting it" testModeratedTrackerNotRepostedOnRevoke
  it "revokes a code on /revoke, answers a repeat as already revoked and an unknown code as no such code" testGroupRevoke
  it "retires the tracker message when a multi-use code is revoked" testRevokeRetiresTracker
  it "retires the tracker message when the service command revokes the code" testServiceRevokeRetiresTracker
  it "retires a tracker left behind when the code is revoked again" testGroupRevokeRepairsTracker
  it "retires a tracker left behind when the service command revokes again" testServiceRevokeRepairsTracker
  it "posts one retired tracker however often a revoke past the window repeats" testRevokeRepeatPastWindow
  it "reconciles an exhausted tracker whose refresh was lost, posting no notice" testExhaustedTrackerReconciledOnRestart
  it "reconciles a tracker with uses left whose refresh was lost" testStalledTrackerReconciledOnRestart
  it "retires on restart a tracker whose revoke never reached it" testRevokedTrackerReconciledOnRestart
  it "reconciles every stalled tracker on restart, not only the first" testEveryStalledTrackerReconciled
  it "leaves a tracker it cannot edit alone rather than publishing the code again" testUneditableTrackerLeftAlone

groupName :: String
groupName = "testbadges"

groupDescr :: Text
groupDescr = "badge ops desk"

testGroupConfig :: GroupConfig
testGroupConfig = GroupConfig {gDisplayName = T.pack groupName, gDescription = Just groupDescr}

revokeReply :: Text -> Text -> Text
revokeReply code outcome = code <> ": " <> outcome

revokeCmd :: Text -> String
revokeCmd code = "/revoke " <> T.unpack code

revokeInGroup :: HasCallStack => ChatController -> TestCC -> Text -> Text -> IO ()
revokeInGroup cc member code outcome = do
  sendGroupCmd member (revokeCmd code)
  waitStoredItem cc (revokeReply code outcome)

-- The name is quoted because console output quotes a name with a space.
botName :: String
botName = "'" <> T.unpack badgeBotName <> "'"

data GroupSvc = GroupSvc
  { gsCfg :: ChatConfig,
    gsKey :: BadgeIssuerKey,
    gsStaticDir :: FilePath,
    gsPs :: TestParams
  }

prepareGroupService :: HasCallStack => TestParams -> IO GroupSvc
prepareGroupService ps@TestParams {tmpPath} =
  bbsKeyGen >>= \case
    Left e -> error $ "bbsKeyGen failed: " <> e
    Right (pk, sk) -> do
      let gsKey = BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}
          gsCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
          gsStaticDir = tmpPath </> "badge_group_static"
      createDirectoryIfMissing True gsStaticDir
      withNewTestChatCfg ps gsCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
      pure GroupSvc {gsCfg, gsKey, gsStaticDir, gsPs = ps}

runGroupService :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupService svc = runGroupServiceAs svc (Just testGroupConfig)

runGroupServiceAs :: HasCallStack => GroupSvc -> Maybe GroupConfig -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupServiceAs svc groupCfg action = do
  (t, cc, env, port) <- startGroupService svc groupCfg
  -- The web listener starts after group start-up, so the stop cannot land inside start-up's database calls.
  let started = pollUntilTrue (listening port) >> pollUntil (getStoredGroup cc)
      -- A stop inside a lane's database call leaves a statement open, and closing the store then fails.
      settle gid = when (isJust groupCfg) $ do
        status <- memberStatus cc gid badgeBotName
        forM_ status $ \s -> unless (s `elem` ["removed", "left", "deleted"]) $ settleLane cc env gid
  (started >>= \ManagedGroup {mgGroupId} -> action cc env mgGroupId <* settle mgGroupId) `finally` stopGroupService t cc

-- It returns once chat has started, while group start-up may still run; the Int is its web listener's port.
startGroupService :: HasCallStack => GroupSvc -> Maybe GroupConfig -> IO (Async (), ChatController, ServiceState, Int)
startGroupService svc@GroupSvc {gsCfg} groupCfg = do
  (opts, port) <- groupServiceOpts svc groupCfg
  env <- newServiceState
  t <- async $ badgeService opts gsCfg env
  -- A service that ends before it is ready fails the test with its own error, not the timeout.
  cc <-
    timeout waitLimit (atomically $ (Right <$> readTMVar (serviceCC env)) `orElse` (Left <$> waitCatchSTM t)) >>= \case
      Just (Right cc) -> pure cc
      Just (Left ended) -> either throwIO (\_ -> error "badge service ended before it started") ended
      Nothing -> cancel t >> error "badge service did not start"
  pure (t, cc, env, port)

-- Cancelling ends the service lanes while chat still runs; stopping chat first would leave them running.
stopGroupService :: Async () -> ChatController -> IO ()
stopGroupService t cc = do
  -- cancel discards how the service ended, so a crash is read first and rethrown after cleanup.
  ended <- poll t
  cancel t
  stopChat cc
  forM_ ended $ either throwIO pure

stopChat :: ChatController -> IO ()
stopChat cc = stopBadgeService cc >> disposeAgentClient (smpAgent cc)

-- It writes the service's ini on a free port; Nothing omits the [group] section.
groupServiceOpts :: GroupSvc -> Maybe GroupConfig -> IO (BadgeServiceOpts, Int)
groupServiceOpts GroupSvc {gsKey = BadgeIssuerKey {secretKey}, gsStaticDir, gsPs = ps@TestParams {tmpPath}} groupCfg = do
  (port, sock) <- openFreePort
  close sock
  let iniPath = tmpPath </> "badge_group.ini"
  writeFile iniPath $
    unlines $
      [ "[listener]",
        "host = 127.0.0.1",
        "port = " <> show port,
        "static_dir = " <> gsStaticDir
      ]
        <> maybe [] groupSection groupCfg
  pure ((mkBadgeServiceOpts ps secretKey) {serviceConfigFile = Just iniPath, noAddress = True}, port)
  where
    groupSection GroupConfig {gDisplayName, gDescription} =
      ["", "[group]", "display_name = " <> T.unpack gDisplayName]
        <> maybe [] (\d -> ["description = " <> T.unpack d]) gDescription

withGroupOwner :: HasCallStack => TestParams -> (BadgeIssuerKey -> ChatController -> ServiceState -> TestCC -> IO ()) -> IO ()
withGroupOwner ps action = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  runWithOwner svc $ \cc env _ -> action gsKey cc env

runWithOwner :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> TestCC -> IO a) -> IO a
runWithOwner svc@GroupSvc {gsPs} action = runGroupService svc $ \cc env gid -> withOwnerJoined gsPs cc gid $ action cc env gid

withOwnerJoined :: HasCallStack => TestParams -> ChatController -> Int64 -> (TestCC -> IO a) -> IO a
withOwnerJoined ps cc gid action =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    joinGroup cc alice
    waitMemberRole cc gid "alice" "owner"
    r <- action alice
    drainConsole alice
    pure r

-- Bob joins after the first member and is connected to her.
withMemberJoined :: HasCallStack => TestParams -> ChatController -> TestCC -> (TestCC -> IO a) -> IO a
withMemberJoined ps cc alice action =
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    joinGroup cc bob
    drainUntil alice ["#" <> groupName <> ": new member bob is connected"]
    r <- action bob
    drainConsole bob
    pure r

joinGroup :: HasCallStack => ChatController -> TestCC -> IO ()
joinGroup cc member = groupLink cc >>= send member . ("/c " <>)

sendGroupCmd :: TestCC -> String -> IO ()
sendGroupCmd member cmd = send member ("#" <> groupName <> " " <> cmd)

issueTracked :: HasCallStack => ChatController -> TestCC -> String -> Int -> IO (Int64, Text, Text)
issueTracked cc member badgeType uses = do
  sendGroupCmd member ("/issue " <> badgeType <> " uses " <> show uses)
  (citemId, body) <- waitTrackerItemOf cc uses
  pure (citemId, body, extractCode body)

testGroupCreateReuse :: HasCallStack => TestParams -> IO ()
testGroupCreateReuse ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc _ gid -> do
    ManagedGroup {mgGroupLink} <- storedGroup cc
    shortLinkContactType mgGroupLink `shouldBe` Right CCTGroup
    groupCount cc `shouldReturn` 1
    advertisedCommands cc gid `shouldReturn` Just expectedCommands
    profileDescription cc gid `shouldReturn` Just groupDescr
    profileFullNameShortDescr cc gid `shouldReturn` ("", Nothing)
    groupFeaturePreference cc gid SGFHistory `shouldReturn` HistoryGroupPreference {enable = FEOff}
    -- The first run never rewrites the profile, so only the restart can restore what is blanked here.
    clearCommandsSettingFullDelete cc gid
  advertisedAt <- runGroupService svc $ \cc _ gid -> do
    -- Commands are advertised as the group is resolved, so once they are back this run has found the stored group.
    waitAdvertisedCommands cc gid
    groupCount cc `shouldReturn` 1
    profileDisplayName cc gid `shouldReturn` T.pack groupName
    profileDescription cc gid `shouldReturn` Just groupDescr
    profileFullNameShortDescr cc gid `shouldReturn` ("", Nothing)
    groupFeaturePreference cc gid SGFFullDelete `shouldReturn` FullDeleteGroupPreference {enable = FEOn, role = Nothing}
    profileUpdatedAt cc gid
  -- The lane drains only after the group is resolved, so the promotion proves the start-up sync has run.
  runWithOwner svc $ \cc _ gid _ -> profileUpdatedAt cc gid `shouldReturn` advertisedAt

testConfigNotApplied :: HasCallStack => TestParams -> IO ()
testConfigNotApplied ps = do
  svc <- prepareGroupService ps
  -- Clearing the commands makes the restart rewrite the profile, the only path that could apply the new config.
  runGroupService svc $ \cc _ gid -> clearCommandsSettingFullDelete cc gid
  runGroupServiceAs svc (Just GroupConfig {gDisplayName = "renamedbadges", gDescription = Just "renamed desk"}) $ \cc env gid -> do
    waitAdvertisedCommands cc gid
    awaitLane cc env
    groupCount cc `shouldReturn` 1
    profileDisplayName cc gid `shouldReturn` T.pack groupName
    profileDescription cc gid `shouldReturn` Just groupDescr

testMissedOwnerPromotedOnStart :: HasCallStack => TestParams -> IO ()
testMissedOwnerPromotedOnStart ps = do
  svc <- prepareGroupService ps
  -- A demoted owner with the flag cleared looks like a join the lane never handled.
  withTwoMembersOwnerUnset svc $ \cc gid _ -> setMemberRole cc gid "alice" "member"
  runGroupService svc $ \cc env gid -> do
    waitMemberRole cc gid "alice" "owner"
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]

testEarliestPromotedOnLaterJoin :: HasCallStack => TestParams -> IO ()
testEarliestPromotedOnLaterJoin ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice -> do
    setMemberRole cc gid "alice" "member"
    executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 0"
    withMemberJoined ps cc alice $ \_ -> do
      waitMemberRole cc gid "alice" "owner"
      awaitLane cc env
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]

testJoiningMemberSkippedOnStart :: HasCallStack => TestParams -> IO ()
testJoiningMemberSkippedOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid _ -> do
    setMemberRole cc gid "alice" "member"
    withTransaction (chatStore cc) $ \db ->
      DB.execute db "UPDATE group_members SET member_status = 'accepted' WHERE group_id = ? AND local_display_name = 'alice'" (Only gid)
  runGroupService svc $ \cc env gid -> do
    waitMemberRole cc gid "bob" "owner"
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "owner")]

testHandMadeOwnerKeptOnStart :: HasCallStack => TestParams -> IO ()
testHandMadeOwnerKeptOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid _ -> do
    setMemberRole cc gid "bob" "owner"
    setMemberRole cc gid "alice" "member"
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "owner")]
    fmap mgOwnerBootstrapped <$> getStoredGroup cc `shouldReturn` Just True

testDeletedGroupNotServed :: HasCallStack => TestParams -> IO ()
testDeletedGroupNotServed ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice -> do
    drainConsole alice
    alice ##> ("/d #" <> groupName)
    alice <## ("#" <> groupName <> ": you deleted the group (signed)")
    pollUntilTrue $ (== Just "deleted") <$> memberStatus cc gid badgeBotName
  runGroupService svc $ \cc _ _ ->
    ensureManagedGroup cc testGroupConfig `shouldReturn` Nothing

testOwnDeletedGroupNotServed :: HasCallStack => TestParams -> IO ()
testOwnDeletedGroupNotServed ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    -- The harness cannot settle a deleted group, so the lane must be idle before the delete.
    settleLane cc env gid
    sendChatCmdStr cc ("/d #" <> groupName) >>= (`shouldSatisfy` isRight)
  runGroupService svc $ \cc _ _ ->
    ensureManagedGroup cc testGroupConfig `shouldReturn` Nothing

testReplacedGroupDropsTrackers :: HasCallStack => TestParams -> IO ()
testReplacedGroupDropsTrackers ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ _ alice -> do
    void $ issueTracked cc alice "supporter" 3
    executeSql cc "DELETE FROM sx_badge_service_group"
  runGroupService svc $ \cc _ _ ->
    countRows cc "sx_badge_service_badge_codes WHERE group_item_id IS NOT NULL OR group_item_sent_at IS NOT NULL" `shouldReturn` 0

testGroupLookupFailureStops :: HasCallStack => TestParams -> IO ()
testGroupLookupFailureStops ps = do
  svc@GroupSvc {gsCfg} <- prepareGroupService ps
  runGroupService svc $ \cc _ _ ->
    executeSql cc "ALTER TABLE sx_badge_service_group RENAME TO sx_badge_service_group_hidden"
  (opts, _) <- groupServiceOpts svc (Just testGroupConfig)
  env <- newServiceState
  -- Falling through to create would keep the service running, so the timeout would fire instead.
  ended <- timeout waitLimit (try $ badgeService opts gsCfg env)
  atomically (tryReadTMVar (serviceCC env)) >>= mapM_ stopChat
  ended `shouldBe` Just (Left (ExitFailure 1))

-- The web listener is one of the service's lanes and only the cancel precedes the check,
-- so its port closing shows the core ended the lanes.
testCancelStopsLanes :: HasCallStack => TestParams -> IO ()
testCancelStopsLanes ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc (Just testGroupConfig)
  (pollUntilTrue (listening port) >> cancel t >> pollUntilTrue (not <$> listening port)) `finally` (cancel t >> stopChat cc)

listening :: Int -> IO Bool
listening port =
  bracket (socket AF_INET Stream defaultProtocol) close $ \s ->
    (True <$ connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))) `catch` \(_ :: IOException) -> pure False

testLeftOwnerReplacedOnStart :: HasCallStack => TestParams -> IO ()
testLeftOwnerReplacedOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid bob -> do
    setMemberRole cc gid "bob" "owner"
    setMemberRole cc gid "alice" "member"
    send bob ("/l " <> groupName)
    pollUntilTrue $ (== Just "left") <$> memberStatus cc gid "bob"
  runGroupService svc $ \cc _ gid -> waitMemberRole cc gid "alice" "owner"

testSetUpOwnerNotReplacedOnStart :: HasCallStack => TestParams -> IO ()
testSetUpOwnerNotReplacedOnStart ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid _ -> setMemberRole cc gid "alice" "member"
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member")]

memberStatus :: ChatController -> Int64 -> Text -> IO (Maybe Text)
memberStatus cc gid name =
  queryFirst cc $
    "SELECT member_status FROM group_members WHERE group_id = "
      <> show gid
      <> " AND local_display_name = '"
      <> T.unpack name
      <> "'"

withTwoMembersOwnerUnset :: HasCallStack => GroupSvc -> (ChatController -> Int64 -> TestCC -> IO ()) -> IO ()
withTwoMembersOwnerUnset svc@GroupSvc {gsPs = ps} action =
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      -- The lane must handle bob's join before the flag is cleared, or it would act on the cleared flag in this run.
      awaitLane cc env
      action cc gid bob
      executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 0"

testFirstJoinerPromoted :: HasCallStack => TestParams -> IO ()
testFirstJoinerPromoted ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      waitMemberRole cc gid "bob" "member"
      -- The bot queues bob's join before it introduces him, so alice's code proves the join was handled.
      sendGroupCmd alice "/issue supporter"
      waitCodeOfType cc "supporter"
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]
      let codeReply = "#" <> groupName <> " " <> botName <> "> code SB-"
      drainUntil alice [codeReply]
      drainUntil bob [codeReply, "#" <> groupName <> ": member alice"]

testFailedPromotionNotRetried :: HasCallStack => TestParams -> IO ()
testFailedPromotionNotRetried ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    -- An admin cannot make an owner, so the core refuses the promotion.
    setBotRole cc "admin"
    withNewTestChat ps "alice" aliceProfile $ \alice -> do
      joinGroup cc alice
      pollUntilTrue $ mgOwnerBootstrapped <$> storedGroup cc
      awaitLane cc env
      setBotRole cc "owner"
      withMemberJoined ps cc alice $ \_ -> awaitLane cc env
      memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "member")]
      drainConsole alice

testRoleGatedIssue :: HasCallStack => TestParams -> IO ()
testRoleGatedIssue ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        let announced name = "#" <> groupName <> ": " <> botName <> " added " <> name
            preMember name = "#" <> groupName <> ": member " <> name
            newMember name = "#" <> groupName <> ": new member " <> name <> " is connected"
            usageReply = "#" <> groupName <> " " <> botName <> "> use: /issue <type>"
        joinGroup cc bob
        waitMemberRole cc gid "bob" "member"
        -- cath joins only after alice sees bob announced, or cath's console shows a different line.
        drainUntil alice [announced "bob"]
        joinGroup cc cath
        waitMemberRole cc gid "cath" "member"
        drainUntil cath [preMember "alice", preMember "bob"]
        drainConsole cath
        sendGroupCmd alice "/issue supporter"
        getInAnyOrder
          dropTime
          cath
          [ StartsWith ("#" <> groupName <> " alice> /issue supporter"),
            StartsWith ("#" <> groupName <> " " <> botName <> "> code SB-")
          ]
        codeCount cc `shouldReturn` 1
        sendGroupCmd bob "/issue legend"
        waitStoredItem cc "/issue legend"
        ownerIssuesNext cc alice 2
        sendGroupCmd alice "/issue supporter uses 0"
        waitStoredItem cc "use: /issue <type> [months <M>] [uses <N>]"
        codeCount cc `shouldReturn` 2
        drainUntil alice [usageReply, newMember "bob", newMember "cath"]
        drainUntil bob [usageReply, preMember "alice", newMember "cath"]
        drainUntil cath [usageReply]
        drainConsole bob
        drainConsole cath

testBlockedMemberCommandIgnored :: HasCallStack => TestParams -> IO ()
testBlockedMemberCommandIgnored ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      waitMemberRole cc gid "bob" "member"
      -- bob is made moderator so that only the block can refuse his command.
      send alice ("/mr #" <> groupName <> " bob moderator")
      waitMemberRole cc gid "bob" "moderator"
      -- The block and the command travel over different connections, so the block is awaited first.
      send alice ("/block for all #" <> groupName <> " bob")
      waitMemberBlocked cc gid "bob"
      sendGroupCmd bob "/bulk legend months 255 count 100"
      (deleted, content) <- waitBlockedItem cc "/bulk legend months 255 count 100"
      deleted `shouldBe` blockedByAdminMark
      content `shouldSatisfy` T.isInfixOf "/bulk legend months 255 count 100"
      ownerIssuesNext cc alice 1

-- The lane is a single FIFO drainer, so the owner's next code proves every earlier command was handled.
ownerIssuesNext :: HasCallStack => ChatController -> TestCC -> Int -> IO ()
ownerIssuesNext cc owner total = do
  sendGroupCmd owner "/issue investor"
  waitCodeOfType cc "investor"
  codeCount cc `shouldReturn` total

-- This is the item_deleted value the core writes for a message from a member blocked for all.
blockedByAdminMark :: Int
blockedByAdminMark = 3

testSupportScopeIgnored :: HasCallStack => TestParams -> IO ()
testSupportScopeIgnored ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    send alice "/_send #1(_support) text /issue legend"
    waitStoredItem cc "/issue legend"
    ownerIssuesNext cc alice 1

testLiveMessageIgnored :: HasCallStack => TestParams -> IO ()
testLiveMessageIgnored ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    send alice ("/live #" <> groupName <> " /issue legend")
    waitStoredItem cc "/issue legend"
    ownerIssuesNext cc alice 1

testOtherGroupEventIgnored :: HasCallStack => TestParams -> IO ()
testOtherGroupEventIgnored ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    atomically $ writeTQueue (groupEventQ env) (GEInGroup (gid + 1) (GACommand GROwner "/issue legend"))
    awaitLane cc env
    queryColumn cc "SELECT badge_type FROM sx_badge_service_badge_codes" `shouldReturn` ["supporter" :: Text]

testBulkIssue :: HasCallStack => TestParams -> IO ()
testBulkIssue ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    codes <- map extractCode . T.lines <$> replyTo cc alice "/bulk legend months 2 count 3"
    length codes `shouldBe` 3
    codeCount cc `shouldReturn` 3
    singleUseCodeCount cc `shouldReturn` 3
    freeCodeCount cc `shouldReturn` 3
    forM_ codes $ \code -> do
      r <- redeemViaService gsKey cc env code
      badgeTypeOf r `shouldBe` Just BTLegend
      map (\e -> let (c, m, _) = entryOf e in (c, m)) (fst $ statementOf r) `shouldBe` [(2, 2), (-1, 1)]
    forM_ codes $ \code -> redeemViaService gsKey cc env code >>= (`shouldAnswerError` BSECodeUsed)
    countRows cc "sx_badge_service_badge_codes WHERE group_item_id IS NOT NULL" `shouldReturn` 0

testIssueMonths :: HasCallStack => TestParams -> IO ()
testIssueMonths ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    sendGroupCmd alice "/issue legend months 12"
    waitCodeMonths cc "legend" `shouldReturn` 12
    freeCodeCount cc `shouldReturn` 1

testBulkPartialFailureListsIssued :: HasCallStack => TestParams -> IO ()
testBulkPartialFailureListsIssued ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    codeLines <- withCodeTableCapped cc 2 $ do
      (codeLines, summary) <- splitAt 2 . T.lines <$> replyTo cc alice "/bulk supporter count 3"
      summary `shouldBe` ["issued 2 of 3, the rest failed"]
      pure codeLines
    codeCount cc `shouldReturn` 2
    forM_ codeLines $ redeemOk gsKey cc env . extractCode

testIssueFailureIsOpaque :: HasCallStack => TestParams -> IO ()
testIssueFailureIsOpaque ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    withCodeTableHidden cc $ do
      replyTo cc alice "/issue supporter" `shouldReturn` "issuing the code failed"
      issueRaw cc "supporter" `shouldReturn` Left "issuing the code failed"
    void $ replyTo cc alice "/issue supporter"
    codeCount cc `shouldReturn` 1

testRevokeFailureIsOpaque :: HasCallStack => TestParams -> IO ()
testRevokeFailureIsOpaque ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    code <- extractCode <$> replyTo cc alice "/issue supporter"
    withCodeTableHidden cc $
      replyTo cc alice (revokeCmd code) `shouldReturn` revokeReply code "revoking the code failed"
    revokeInGroup cc alice code "revoked"

withCodeTableHidden :: ChatController -> IO a -> IO a
withCodeTableHidden cc action = rename codeTable hidden >> (action `finally` rename hidden codeTable)
  where
    codeTable = "sx_badge_service_badge_codes"
    hidden = codeTable <> "_hidden"
    rename from to = executeSql cc $ "ALTER TABLE " <> from <> " RENAME TO " <> to

-- A test-only trigger fails every insert once the code table holds cap rows.
withCodeTableCapped :: ChatController -> Int -> IO a -> IO a
withCodeTableCapped cc cap action = mapM_ (executeSql cc) createCap >> (action `finally` mapM_ (executeSql cc) dropCap)
  where
#if defined(dbPostgres)
    createCap =
      [ "CREATE FUNCTION sx_badge_service_test_cap() RETURNS trigger AS $$ BEGIN IF (SELECT COUNT(*) FROM sx_badge_service_badge_codes) >= "
          <> show cap
          <> " THEN RAISE EXCEPTION 'code table full'; END IF; RETURN NEW; END; $$ LANGUAGE plpgsql",
        "CREATE TRIGGER sx_badge_service_test_cap BEFORE INSERT ON sx_badge_service_badge_codes FOR EACH ROW EXECUTE FUNCTION sx_badge_service_test_cap()"
      ]
    dropCap = ["DROP FUNCTION sx_badge_service_test_cap() CASCADE"]
#else
    createCap =
      [ "CREATE TRIGGER sx_badge_service_test_cap BEFORE INSERT ON sx_badge_service_badge_codes WHEN (SELECT COUNT(*) FROM sx_badge_service_badge_codes) >= "
          <> show cap
          <> " BEGIN SELECT RAISE(ABORT, 'code table full'); END"
      ]
    dropCap = ["DROP TRIGGER sx_badge_service_test_cap"]
#endif

testMultiUseTracker :: HasCallStack => TestParams -> IO ()
testMultiUseTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    tracker0 `shouldSatisfy` T.isInfixOf "2/2 remaining"
    redeemOk gsKey cc env code
    t1 <- waitItemText cc trackerItemId "1/2 remaining"
    t1 `shouldSatisfy` T.isInfixOf "Last redeemed"
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "0/2 remaining"
    waitExhausted cc `shouldReturn` exhaustedText code 2
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` BSECodeUsed)

testStaleRefreshIgnored :: HasCallStack => TestParams -> IO ()
testStaleRefreshIgnored ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "0/2 remaining"
    notice <- waitExhausted cc
    replayRefresh cc env code 2 1
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` [notice]
    readItemText cc trackerItemId >>= (`shouldSatisfy` T.isInfixOf "0/2 remaining")
    redeemedTrackerCount cc `shouldReturn` 1

testSecondMultiUseCodeExhausted :: HasCallStack => TestParams -> IO ()
testSecondMultiUseCodeExhausted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    -- The first code gets 3 claims, the second code's limit, so reading the wrong count would post the notice early.
    (firstItemId, _, firstCode) <- issueTracked cc alice "supporter" 5
    replicateM_ 3 (redeemOk gsKey cc env firstCode)
    void $ waitItemText cc firstItemId "2/5 remaining"
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` []
    (secondItemId, _, secondCode) <- issueTracked cc alice "legend" 3
    -- Each claim is awaited because the lane coalesces refreshes queued together.
    redeemOk gsKey cc env secondCode
    void $ waitItemText cc secondItemId "2/3 remaining"
    redeemOk gsKey cc env secondCode
    void $ waitItemText cc secondItemId "1/3 remaining"
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` []
    redeemOk gsKey cc env secondCode
    void $ waitItemText cc secondItemId "0/3 remaining"
    waitExhausted cc `shouldReturn` exhaustedText secondCode 3
    readItemText cc firstItemId >>= (`shouldSatisfy` T.isInfixOf "2/5 remaining")

testLanelessRedeemRefreshesTracker :: HasCallStack => TestParams -> IO ()
testLanelessRedeemRefreshesTracker ps =
  withGroupOwner ps $ \gsKey cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOkWith gsKey cc Nothing code
    -- The refresh runs before the response, so the counter is already written here.
    t1 <- readItemText cc trackerItemId
    t1 `shouldSatisfy` T.isInfixOf "1/2 remaining"
    t1 `shouldSatisfy` T.isInfixOf "Last redeemed"
    exhaustedNotices cc `shouldReturn` []
    redeemOkWith gsKey cc Nothing code
    readItemText cc trackerItemId >>= (`shouldSatisfy` T.isInfixOf "0/2 remaining")
    exhaustedNotices cc `shouldReturn` [exhaustedText code 2]
    redeemedTrackerCount cc `shouldReturn` 1

testQueuedRequestRefreshesTracker :: HasCallStack => TestParams -> IO ()
testQueuedRequestRefreshesTracker ps =
  withGroupOwner ps $ \_ cc env alice -> do
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    queueRedemption cc env code
    void $ waitItemText cc trackerItemId "1/2 remaining"

testQueuedRequestWithoutGroupConfig :: HasCallStack => TestParams -> IO ()
testQueuedRequestWithoutGroupConfig ps = do
  svc <- prepareGroupService ps
  (trackerItemId, code) <- runWithOwner svc $ \cc _ _ alice ->
    (\(i, _, c) -> (i, c)) <$> issueTracked cc alice "supporter" 2
  runGroupServiceAs svc Nothing $ \cc env _ -> do
    queueRedemption cc env code
    void $ waitItemText cc trackerItemId "1/2 remaining"

testTrackerRepost :: HasCallStack => TestParams -> IO ()
testTrackerRepost ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    backdated <- backdateTracker cc 2
    redeemOk gsKey cc env code
    (_, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "1/2 remaining"
    readItemText cc itemId0 `shouldReturn` tracker0
    sentAt <- trackerSentAt cc 2
    sentAt `shouldSatisfy` (> backdated)

testRefusedEditReposted :: HasCallStack => TestParams -> IO ()
testRefusedEditReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    backdateTrackerItem cc itemId0
    redeemOk gsKey cc env code
    (itemId1, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "1/2 remaining"
    redeemOk gsKey cc env code
    void $ waitItemText cc itemId1 "0/2 remaining"
    waitExhausted cc `shouldReturn` exhaustedText code 2
    readItemText cc itemId0 `shouldReturn` tracker0

testFailedRepostDropped :: HasCallStack => TestParams -> IO ()
testFailedRepostDropped ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    void $ backdateTracker cc 2
    setBotRole cc "observer"
    redeemOk gsKey cc env code
    awaitLane cc env
    sentItemsWithCode cc code `shouldReturn` [tracker0]
    readItemText cc itemId0 `shouldReturn` tracker0
    trackerAnchor cc 2 `shouldReturn` Just itemId0
    setBotRole cc "owner"
    redeemOk gsKey cc env code
    (_, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "0/2 remaining"
    waitExhausted cc `shouldReturn` exhaustedText code 2

testDeletedTrackerNotReposted :: HasCallStack => TestParams -> IO ()
testDeletedTrackerNotReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    readItemText cc itemId0 `shouldReturn` ""
    trackerNeverReposted gsKey cc env code itemId0 []

testDeletedTrackerNotRepostedOnRevoke :: HasCallStack => TestParams -> IO ()
testDeletedTrackerNotRepostedOnRevoke ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    -- Past the edit window the revoke goes straight to the repost path and its guard.
    void $ backdateTracker cc 2
    revokeNeverReposts cc alice code []

-- Both claims fall inside the edit window, so each edit fails because the message is gone.
testDeletedTrackerNoNotice :: HasCallStack => TestParams -> IO ()
testDeletedTrackerNoNotice ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    replicateM_ 2 $ redeemOk gsKey cc env code
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` []
    sentItemsWithCode cc code `shouldReturn` []

testModeratedTrackerNotRepostedOnRevoke :: HasCallStack => TestParams -> IO ()
testModeratedTrackerNotRepostedOnRevoke ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    moderateBotItem alice tracker0
    void $ waitModeratedItem cc itemId0
    revokeNeverReposts cc alice code [tracker0]

testModeratedTrackerNotReposted :: HasCallStack => TestParams -> IO ()
testModeratedTrackerNotReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    moderateBotItem alice tracker0
    (deleted, byMember, content) <- waitModeratedItem cc itemId0
    deleted `shouldBe` moderatedMark
    byMember `shouldSatisfy` isJust
    content `shouldSatisfy` T.isInfixOf code
    readItemText cc itemId0 `shouldReturn` tracker0
    trackerNeverReposted gsKey cc env code itemId0 [tracker0]

-- This is the item_deleted value the core writes for a message a member deleted for everyone.
moderatedMark :: Int
moderatedMark = 1

-- The code must be the 2-use tracked one: two claims, the second past the edit window and using it up,
-- leave the code's bot items as they were.
trackerNeverReposted :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> Text -> Int64 -> [Text] -> IO ()
trackerNeverReposted key cc env code itemId0 codeItems = do
  redeemOk key cc env code
  awaitLane cc env
  sentItemsWithCode cc code `shouldReturn` codeItems
  trackerAnchor cc 2 `shouldReturn` Just itemId0
  void $ backdateTracker cc 2
  redeemOk key cc env code
  awaitLane cc env
  sentItemsWithCode cc code `shouldReturn` codeItems
  exhaustedNotices cc `shouldReturn` []
  trackerAnchor cc 2 `shouldReturn` Just itemId0

revokeNeverReposts :: HasCallStack => ChatController -> TestCC -> Text -> [Text] -> IO ()
revokeNeverReposts cc member code codeItems = do
  revokeInGroup cc member code "revoked"
  sentItemsWithCode cc code `shouldReturn` codeItems <> [revokeReply code "revoked"]
  revokedTrackerCount cc `shouldReturn` 0

testGroupRevoke :: HasCallStack => TestParams -> IO ()
testGroupRevoke ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    code <- extractCode <$> replyTo cc alice "/issue supporter"
    other <- extractCode <$> replyTo cc alice "/issue legend"
    revokeInGroup cc alice code "revoked"
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` BSECodeInvalid)
    redeemOk gsKey cc env other
    revokeInGroup cc alice code "already revoked"
    -- The reply follows the tracker step, so the revoke has already passed it here.
    revokedTrackerCount cc `shouldReturn` 0
    unissued <- formatBadgeCode <$> randomBadgeCode (random cc)
    revokeInGroup cc alice unissued "no such code"

testGroupRevokeRepairsTracker :: HasCallStack => TestParams -> IO ()
testGroupRevokeRepairsTracker ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    sendGroupCmd alice (revokeCmd code)
    retired <- waitItemText cc trackerItemId "revoked"
    retired `shouldBe` retiredText code
    waitStoredItem cc (revokeReply code "already revoked")
    revokedTrackerCount cc `shouldReturn` 1

testRevokeRepeatPastWindow :: HasCallStack => TestParams -> IO ()
testRevokeRepeatPastWindow ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    void $ backdateTracker cc 2
    revokeInGroup cc alice code "revoked"
    (itemId1, retired) <- waitTrackerRepost cc 2 itemId0
    retired `shouldBe` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    void $ backdateTracker cc 2
    revokeInGroup cc alice code "already revoked"
    revokedTrackerCount cc `shouldReturn` 1
    trackerAnchor cc 2 `shouldReturn` Just itemId1
    readItemText cc itemId1 `shouldReturn` retiredText code
    readItemText cc itemId0 `shouldReturn` tracker0

testExhaustedTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testExhaustedTrackerReconciledOnRestart ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (lostItemId, notice, lostCode) <- runWithOwner svc $ \cc env _ alice -> do
    (doneItemId, _, doneCode) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env doneCode
    redeemOk gsKey cc env doneCode
    notice <- waitExhausted cc
    (lostItemId, lost0, lostCode) <- issueTracked cc alice "legend" 3
    replicateM_ 3 (redeemLosingRefresh gsKey cc lostCode)
    readItemText cc lostItemId `shouldReturn` lost0
    trackerAnchor cc 2 `shouldReturn` Just doneItemId
    dateRedemption cc 3 lostRedeemedAt
    pure (lostItemId, notice, lostCode)
  runGroupService svc $ \cc env _ -> do
    corrected <- waitItemText cc lostItemId "0/3 remaining"
    corrected `shouldBe` ("!2 " <> lostCode <> "!\nLast redeemed: 2026-02-28 — 0/3 remaining")
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` [notice]
    redeemedTrackerCount cc `shouldReturn` 2

-- This date is far from any day the test runs on, so a body dated now cannot match it.
lostRedeemedAt :: UTCTime
lostRedeemedAt = UTCTime (fromGregorian 2026 2 28) (23 * 3600 + 50 * 60)

testStalledTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testStalledTrackerReconciledOnRestart ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  stalledItemId <- runWithOwner svc $ \cc env _ alice -> do
    (stalledItemId, _, code) <- issueTracked cc alice "supporter" 5
    redeemOk gsKey cc env code
    redeemOk gsKey cc env code
    awaitLane cc env
    stalled <- waitItemText cc stalledItemId "3/5 remaining"
    redeemLosingRefresh gsKey cc code
    readItemText cc stalledItemId `shouldReturn` stalled
    pure stalledItemId
  runGroupService svc $ \cc env _ -> do
    corrected <- waitItemText cc stalledItemId "2/5 remaining"
    corrected `shouldSatisfy` T.isInfixOf "Last redeemed: "
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` []
    redeemedTrackerCount cc `shouldReturn` 1

testRevokedTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testRevokedTrackerReconciledOnRestart ps = do
  svc <- prepareGroupService ps
  (trackerItemId, code) <- runWithOwner svc $ \cc _ _ alice -> do
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    pure (trackerItemId, code)
  runGroupService svc $ \cc env _ -> do
    retired <- waitItemText cc trackerItemId "revoked"
    retired `shouldBe` retiredText code
    awaitLane cc env
    revokedTrackerCount cc `shouldReturn` 1

testEveryStalledTrackerReconciled :: HasCallStack => TestParams -> IO ()
testEveryStalledTrackerReconciled ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (firstItemId, secondItemId) <- runWithOwner svc $ \cc _ _ alice -> do
    (firstItemId, first0, firstCode) <- issueTracked cc alice "supporter" 5
    replicateM_ 2 (redeemLosingRefresh gsKey cc firstCode)
    (secondItemId, second0, secondCode) <- issueTracked cc alice "legend" 3
    redeemLosingRefresh gsKey cc secondCode
    readItemText cc firstItemId `shouldReturn` first0
    readItemText cc secondItemId `shouldReturn` second0
    pure (firstItemId, secondItemId)
  runGroupService svc $ \cc env _ -> do
    first1 <- waitItemText cc firstItemId "3/5 remaining"
    first1 `shouldSatisfy` T.isInfixOf "Last redeemed: "
    second1 <- waitItemText cc secondItemId "2/3 remaining"
    second1 `shouldSatisfy` T.isInfixOf "Last redeemed: "
    awaitLane cc env
    exhaustedNotices cc `shouldReturn` []
    redeemedTrackerCount cc `shouldReturn` 2

testUneditableTrackerLeftAlone :: HasCallStack => TestParams -> IO ()
testUneditableTrackerLeftAlone ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (staleItemId, stale0, staleCode, liveItemId, liveCode) <- runWithOwner svc $ \cc _ _ alice -> do
    (staleItemId, stale0, staleCode) <- issueTracked cc alice "supporter" 2
    redeemLosingRefresh gsKey cc staleCode
    backdateTrackerItem cc staleItemId
    readItemText cc staleItemId `shouldReturn` stale0
    -- The lane drains only after the pass returns, so this code's update marks the pass as done.
    (liveItemId, _, liveCode) <- issueTracked cc alice "legend" 3
    pure (staleItemId, stale0, staleCode, liveItemId, liveCode)
  runGroupService svc $ \cc env _ -> do
    redeemOk gsKey cc env liveCode
    void $ waitItemText cc liveItemId "2/3 remaining"
    readItemText cc staleItemId `shouldReturn` stale0
    sentItemsWithCode cc staleCode `shouldReturn` [stale0]
    trackerAnchor cc 2 `shouldReturn` Just staleItemId

testRevokeRetiresTracker :: HasCallStack => TestParams -> IO ()
testRevokeRetiresTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "1/2 remaining"
    revokeInGroup cc alice code "revoked"
    retired <- waitItemText cc trackerItemId "revoked"
    retired `shouldBe` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    replayRefresh cc env code 2 2
    awaitLane cc env
    readItemText cc trackerItemId `shouldReturn` retired
    exhaustedNotices cc `shouldReturn` []
    revokedTrackerCount cc `shouldReturn` 1
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` BSECodeInvalid)

testServiceRevokeRetiresTracker :: HasCallStack => TestParams -> IO ()
testServiceRevokeRetiresTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "1/2 remaining"
    revokeRaw cc code `shouldReturn` Right "revoked"
    readItemText cc trackerItemId `shouldReturn` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    revokeRaw cc code `shouldReturn` Right "already revoked"
    revokedTrackerCount cc `shouldReturn` 1
    single <- extractCode <$> replyTo cc alice "/issue legend"
    revokeRaw cc single `shouldReturn` Right "revoked"
    revokedTrackerCount cc `shouldReturn` 1

testServiceRevokeRepairsTracker :: HasCallStack => TestParams -> IO ()
testServiceRevokeRepairsTracker ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    revokeRaw cc code `shouldReturn` Right "already revoked"
    readItemText cc trackerItemId `shouldReturn` retiredText code
    revokedTrackerCount cc `shouldReturn` 1

-- A discarded code makes later code ids differ from the group id, so swapped arguments fail.
offsetCodeIds :: HasCallStack => ChatController -> IO ()
offsetCodeIds cc = void $ issueCode cc BTSupporter 1

redeemOk :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> Text -> IO ()
redeemOk key cc env = redeemOkWith key cc (Just (groupEventQ env))

redeemOkWith :: HasCallStack => BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> Text -> IO ()
redeemOkWith key cc trackerQ_ code = do
  r <- redeemWithQueue key cc trackerQ_ code
  credentialOf r `shouldSatisfy` isJust

redeemViaService :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> Text -> IO BadgeServiceResponse
redeemViaService key cc env = redeemWithQueue key cc (Just (groupEventQ env))

-- The reply to the made-up request id fails after the redemption, and the service only logs it.
queueRedemption :: HasCallStack => ChatController -> ServiceState -> Text -> IO ()
queueRedemption cc env codeText = do
  user <- readTVarIO (currentUser cc) >>= maybe (error "no current user") pure
  (purchaseKey, masterKey) <- newPurchaseKeys
  let request = requestObject purchaseKey BSCRedeemBadgeCode {masterKey, code = codeText}
  atomically $ writeTQueue (serviceRequestQ env) (user, AgentInvId "test-request", Just purchaseKey, request)

redeemLosingRefresh :: HasCallStack => BadgeIssuerKey -> ChatController -> Text -> IO ()
redeemLosingRefresh key cc codeText = newTQueueIO >>= \q -> redeemOkWith key cc (Just q) codeText

replayRefresh :: HasCallStack => ChatController -> ServiceState -> Text -> Int -> Int -> IO ()
replayRefresh cc env codeText uses claim = case parseBadgeCode codeText of
  Nothing -> error $ "not a badge code: " <> T.unpack codeText
  Just code -> do
    badgeCodeId <- trackedCodeId cc uses
    atomically $ writeTQueue (groupEventQ env) (GETracker badgeCodeId code claim)

getStoredGroup :: ChatController -> IO (Maybe ManagedGroup)
getStoredGroup cc = withTransaction (chatStore cc) getManagedGroup

shortLinkContactType :: Text -> Either String ContactConnType
shortLinkContactType linkText = case strDecode (encodeUtf8 linkText) of
  Right (ACSL _ (CSLContact _ ct _ _)) -> Right ct
  Right (ACSL _ CSLInvitation {}) -> Left "invitation short link"
  Left e -> Left e

groupLink :: HasCallStack => ChatController -> IO String
groupLink cc = T.unpack . mgGroupLink <$> storedGroup cc

storedGroup :: HasCallStack => ChatController -> IO ManagedGroup
storedGroup cc = getStoredGroup cc >>= maybe (error "no managed group recorded") pure

groupCount :: ChatController -> IO Int
groupCount cc = countRows cc "groups"

-- The lane runs events in order, so once this command's code exists every event queued before it is done.
awaitLane :: HasCallStack => ChatController -> ServiceState -> IO ()
awaitLane cc env = do
  ManagedGroup {mgGroupId} <- storedGroup cc
  issued <- codeCount cc
  atomically $ writeTQueue (groupEventQ env) (GEInGroup mgGroupId (GACommand GROwner "/issue supporter"))
  pollUntilTrue $ (> issued) <$> codeCount cc

-- Saving a tracker's item id is the lane's last write for its command, so the lane is idle after it.
settleLane :: HasCallStack => ChatController -> ServiceState -> Int64 -> IO ()
settleLane cc env gid = do
  anchoredBefore <- anchored
  atomically $ writeTQueue (groupEventQ env) (GEInGroup gid (GACommand GROwner ("/issue supporter uses " <> tshow settleUses)))
  pollUntilTrue $ (> anchoredBefore) <$> anchored
  where
    anchored = countRows cc ("sx_badge_service_badge_codes WHERE redeem_limit = " <> show settleUses <> " AND group_item_id IS NOT NULL")

-- No test tracks a code with this many uses, so the settling codes never match a test's lookup.
settleUses :: Int
settleUses = maxUses

codeCount :: ChatController -> IO Int
codeCount cc = countRows cc "sx_badge_service_badge_codes"

freeCodeCount :: ChatController -> IO Int
freeCodeCount cc = countRows cc "sx_badge_service_badge_codes WHERE code_payment_status = 'free'"

singleUseCodeCount :: ChatController -> IO Int
singleUseCodeCount cc = countRows cc "sx_badge_service_badge_codes WHERE redeem_limit = 1"

countRows :: ChatController -> String -> IO Int
countRows cc fromWhere = fromMaybe 0 <$> queryFirst cc ("SELECT COUNT(*) FROM " <> fromWhere)

-- A test finds each tracked code by its number of uses, so no two of its codes may share one.
trackedCodeColumn :: (HasCallStack, DB.FromField a) => ChatController -> Int -> String -> IO a
trackedCodeColumn cc uses column =
  queryOne cc ("no code issued with " <> show uses <> " uses") $
    "SELECT " <> column <> " FROM sx_badge_service_badge_codes WHERE redeem_limit = " <> show uses

trackedCodeId :: HasCallStack => ChatController -> Int -> IO Int64
trackedCodeId cc uses = trackedCodeColumn cc uses "badge_code_id"

queryRows :: FromRow r => ChatController -> String -> IO [r]
queryRows cc sql = withTransaction (chatStore cc) (\db -> DB.query_ db (fromString sql))

executeSql :: ChatController -> String -> IO ()
executeSql cc sql = withTransaction (chatStore cc) (\db -> DB.execute_ db (fromString sql))

queryColumn :: DB.FromField a => ChatController -> String -> IO [a]
queryColumn cc sql = map fromOnly <$> queryRows cc sql

queryFirst :: DB.FromField a => ChatController -> String -> IO (Maybe a)
queryFirst cc sql = listToMaybe <$> queryColumn cc sql

queryOne :: (HasCallStack, DB.FromField a) => ChatController -> String -> String -> IO a
queryOne cc err sql = queryColumn cc sql >>= maybe (error err) pure . singleRow sql

singleRow :: HasCallStack => String -> [a] -> Maybe a
singleRow sql = \case
  [] -> Nothing
  [r] -> Just r
  _ -> error $ "more than one row from: " <> sql

-- The list is spelled out rather than taken from the service, so a change there fails here.
expectedCommands :: [ChatBotCommand]
expectedCommands =
  [ CBCCommand "issue" "Generate a badge code" (Just "<type> [months <M>] [uses <N>]"),
    CBCCommand "bulk" "Generate many single-use codes" (Just "<type> [months <M>] count <B>"),
    CBCCommand "revoke" "Revoke a code" (Just "<code>")
  ]

advertisedCommands :: HasCallStack => ChatController -> Int64 -> IO (Maybe [ChatBotCommand])
advertisedCommands cc gid =
  (>>= commands_)
    <$> (queryOne cc "no group profile to read commands from" (groupProfileSql "p.preferences" gid) :: IO (Maybe GroupPreferences))

profileDisplayName :: HasCallStack => ChatController -> Int64 -> IO Text
profileDisplayName cc gid =
  queryOne cc "no group profile to read a display name from" (groupProfileSql "p.display_name" gid)

profileFullNameShortDescr :: HasCallStack => ChatController -> Int64 -> IO (Text, Maybe Text)
profileFullNameShortDescr cc gid =
  queryRows cc sql >>= maybe (error "no group profile to read a full name from") pure . singleRow sql
  where
    sql = groupProfileSql "p.full_name, p.short_descr" gid

profileDescription :: HasCallStack => ChatController -> Int64 -> IO (Maybe Text)
profileDescription cc gid = queryOne cc "no group profile to read a description from" (groupProfileSql "p.description" gid)

profileUpdatedAt :: HasCallStack => ChatController -> Int64 -> IO UTCTime
profileUpdatedAt cc gid =
  queryOne cc "no group profile to read an update time from" (groupProfileSql "p.updated_at" gid)

groupFeaturePreference :: HasCallStack => ChatController -> Int64 -> SGroupFeature f -> IO (GroupFeaturePreference f)
groupFeaturePreference cc gid feature =
  getGroupPreference feature
    <$> (queryOne cc "no group profile to read preferences from" (groupProfileSql "p.preferences" gid) :: IO (Maybe GroupPreferences))

groupProfileSql :: String -> Int64 -> String
groupProfileSql columns gid =
  "SELECT "
    <> columns
    <> " FROM group_profiles p JOIN groups g ON g.group_profile_id = p.group_profile_id WHERE g.group_id = "
    <> show gid

clearCommandsSettingFullDelete :: ChatController -> Int64 -> IO ()
clearCommandsSettingFullDelete cc gid =
  withTransaction (chatStore cc) $ \db ->
    DB.execute
      db
      "UPDATE group_profiles SET preferences = ? WHERE group_profile_id IN (SELECT group_profile_id FROM groups WHERE group_id = ?)"
      (fullDeleteOnly, gid)
  where
    fullDeleteOnly =
      emptyGroupPrefs {fullDelete = Just FullDeleteGroupPreference {enable = FEOn, role = Nothing}} :: GroupPreferences

revokeInStore :: HasCallStack => ChatController -> Int -> IO ()
revokeInStore cc uses = getCurrentTime >>= setTrackedCodeTime cc uses "revoked_at"

backdateTracker :: HasCallStack => ChatController -> Int -> IO UTCTime
backdateTracker cc uses = do
  past <- addUTCTime pastEditWindow <$> getCurrentTime
  setTrackedCodeTime cc uses "group_item_sent_at" past
  pure past

-- This is an hour past the core's 24-hour edit window.
pastEditWindow :: NominalDiffTime
pastEditWindow = -25 * 3600

-- The code row keeps its sent time, so the service still tries an edit the core refuses.
backdateTrackerItem :: ChatController -> Int64 -> IO ()
backdateTrackerItem cc citemId = do
  past <- addUTCTime pastEditWindow <$> getCurrentTime
  withTransaction (chatStore cc) $ \db ->
    DB.execute db "UPDATE chat_items SET item_ts = ? WHERE chat_item_id = ?" (past, citemId)

-- Below author the core refuses every send from the bot.
setBotRole :: ChatController -> Text -> IO ()
setBotRole cc role =
  withTransaction (chatStore cc) $ \db ->
    DB.execute db "UPDATE group_members SET member_role = ? WHERE member_category = ?" (role, "user" :: Text)

dateRedemption :: HasCallStack => ChatController -> Int -> UTCTime -> IO ()
dateRedemption cc uses = setTrackedCodeTime cc uses "redeemed_at"

setTrackedCodeTime :: HasCallStack => ChatController -> Int -> String -> UTCTime -> IO ()
setTrackedCodeTime cc uses column ts = do
  badgeCodeId <- trackedCodeId cc uses
  withTransaction (chatStore cc) $ \db ->
    DB.execute db (fromString $ "UPDATE sx_badge_service_badge_codes SET " <> column <> " = ? WHERE badge_code_id = ?") (ts, badgeCodeId)

-- Member messages are excluded because a /revoke names the code too.
sentItemsWithCode :: ChatController -> Text -> IO [Text]
sentItemsWithCode cc code =
  queryColumn cc $
    "SELECT item_text FROM chat_items WHERE item_sent = 1 AND item_text LIKE '%"
      <> T.unpack code
      <> "%' ORDER BY chat_item_id"

-- It reads the code row, not a join, so it still answers after the message is deleted.
trackerAnchor :: HasCallStack => ChatController -> Int -> IO (Maybe Int64)
trackerAnchor cc uses = trackedCodeColumn cc uses "group_item_id"

trackerSentAt :: HasCallStack => ChatController -> Int -> IO UTCTime
trackerSentAt cc uses = trackedCodeColumn cc uses "group_item_sent_at"

deleteItem :: HasCallStack => ChatController -> Int64 -> IO ()
deleteItem cc citemId = do
  ManagedGroup {mgGroupId} <- storedGroup cc
  sendChatCmd cc (APIDeleteChatItem (ChatRef CTGroup mgGroupId Nothing) (citemId :| []) CIDMInternal) >>= \case
    Right CRChatItemsDeleted {} -> pure ()
    r -> error $ "deleting chat item " <> show citemId <> " failed: " <> show r

-- The member must have received the message before the command can find it.
moderateBotItem :: HasCallStack => TestCC -> Text -> IO ()
moderateBotItem member body = do
  void (pollUntil (queryFirst (chatController member) received) :: IO Int64)
  send member ("\\\\ #" <> groupName <> " @" <> botName <> " " <> T.unpack header)
  where
    header = T.takeWhile (/= '\n') body
    received = "SELECT chat_item_id FROM chat_items WHERE item_sent = 0 AND item_text LIKE '" <> T.unpack header <> "%'"

waitModeratedItem :: HasCallStack => ChatController -> Int64 -> IO (Int, Maybe Int64, Text)
waitModeratedItem cc citemId =
  pollUntil $
    mfilter (\(deleted, _, _) -> deleted /= 0) . listToMaybe
      <$> queryRows cc ("SELECT item_deleted, item_deleted_by_group_member_id, item_content FROM chat_items WHERE chat_item_id = " <> show citemId)

memberRoles :: ChatController -> Int64 -> IO [(Text, Text)]
memberRoles cc gid =
  filter ((/= badgeBotName) . fst)
    <$> queryRows cc ("SELECT local_display_name, member_role FROM group_members WHERE group_id = " <> show gid <> " ORDER BY group_member_id")

readItemText :: ChatController -> Int64 -> IO Text
readItemText cc citemId = fromMaybe "" <$> firstText cc ("chat_item_id = " <> show citemId)

-- A line left unread when the test ends fails the per-core teardown check.
drainUntil :: HasCallStack => TestCC -> [String] -> IO ()
drainUntil cc markers =
  timeout waitLimit (go markers) >>= \case
    Just () -> pure ()
    Nothing -> error $ "drainUntil: console never showed " <> show markers
  where
    go [] = pure ()
    go unseen = do
      l <- atomically $ readTQueue (termQ cc)
      go $ filter (not . (`isInfixOf` l)) unseen

drainConsole :: TestCC -> IO ()
drainConsole cc =
  timeout quietPeriod (atomically (readTQueue (termQ cc))) >>= \case
    Just _ -> drainConsole cc
    Nothing -> pure ()

-- A console silent this long is taken as drained.
quietPeriod :: Int
quietPeriod = 500000

pollInterval :: Int
pollInterval = 200000

maxPolls :: Int
maxPolls = 150

waitLimit :: Int
waitLimit = maxPolls * pollInterval

pollUntil :: HasCallStack => IO (Maybe a) -> IO a
pollUntil act = go maxPolls
  where
    go n =
      act >>= \case
        Just a -> pure a
        Nothing
          | n <= (0 :: Int) -> error "pollUntil: timed out waiting for chat state"
          | otherwise -> threadDelay pollInterval >> go (n - 1)

pollUntilTrue :: HasCallStack => IO Bool -> IO ()
pollUntilTrue cond = pollUntil (guard <$> cond)

waitMemberRole :: HasCallStack => ChatController -> Int64 -> Text -> Text -> IO ()
waitMemberRole cc gid name role = pollUntilTrue $ elem (name, role) <$> memberRoles cc gid

setMemberRole :: HasCallStack => ChatController -> Int64 -> Text -> Text -> IO ()
setMemberRole cc gid name role = do
  sendChatCmdStr cc ("/mr #" <> groupName <> " " <> T.unpack name <> " " <> T.unpack role) >>= (`shouldSatisfy` isRight)
  waitMemberRole cc gid name role

waitMemberBlocked :: HasCallStack => ChatController -> Int64 -> Text -> IO ()
waitMemberBlocked cc gid name =
  pollUntilTrue $ (== Just (Just ("blocked" :: Text))) <$> queryFirst cc restrictionSql
  where
    restrictionSql =
      "SELECT member_restriction FROM group_members WHERE group_id = "
        <> show gid
        <> " AND local_display_name = '"
        <> T.unpack name
        <> "'"

waitBlockedItem :: HasCallStack => ChatController -> Text -> IO (Int, Text)
waitBlockedItem cc txt =
  pollUntil $
    mfilter ((/= 0) . fst) . listToMaybe
      <$> queryRows cc ("SELECT item_deleted, item_content FROM chat_items WHERE item_sent = 0 AND item_text = '" <> T.unpack txt <> "'")

waitAdvertisedCommands :: HasCallStack => ChatController -> Int64 -> IO ()
waitAdvertisedCommands cc gid = pollUntilTrue $ (== Just expectedCommands) <$> advertisedCommands cc gid

waitStoredItem :: HasCallStack => ChatController -> Text -> IO ()
waitStoredItem cc txt = void . pollUntil $ firstText cc ("item_text = '" <> T.unpack txt <> "'")

-- The reply is the first item the service sends after the command arrives.
replyTo :: HasCallStack => ChatController -> TestCC -> String -> IO Text
replyTo cc member cmd = do
  lastId <- maxChatItemId cc
  sendGroupCmd member cmd
  waitReceivedItemId cc lastId (T.pack cmd) >>= waitReplyAfter cc

maxChatItemId :: ChatController -> IO Int64
maxChatItemId cc = fromMaybe 0 <$> queryFirst cc "SELECT COALESCE(MAX(chat_item_id), 0) FROM chat_items"

waitReceivedItemId :: HasCallStack => ChatController -> Int64 -> Text -> IO Int64
waitReceivedItemId cc afterId txt =
  pollUntil . queryFirst cc $
    "SELECT chat_item_id FROM chat_items WHERE item_sent = 0 AND chat_item_id > " <> show afterId <> " AND item_text = '" <> T.unpack txt <> "'"

waitReplyAfter :: HasCallStack => ChatController -> Int64 -> IO Text
waitReplyAfter cc afterId =
  pollUntil . firstText cc $ "item_sent = 1 AND chat_item_id > " <> show afterId <> " ORDER BY chat_item_id LIMIT 1"

waitItemText :: HasCallStack => ChatController -> Int64 -> Text -> IO Text
waitItemText cc citemId marker =
  pollUntil $ mfilter (marker `T.isInfixOf`) . Just <$> readItemText cc citemId

waitCodeOfType :: HasCallStack => ChatController -> Text -> IO ()
waitCodeOfType cc badgeType =
  pollUntilTrue $ (> 0) <$> countRows cc ("sx_badge_service_badge_codes WHERE badge_type = '" <> T.unpack badgeType <> "'")

waitCodeMonths :: HasCallStack => ChatController -> Text -> IO Int
waitCodeMonths cc badgeType =
  pollUntil $ do
    months <- queryColumn cc $ "SELECT months FROM sx_badge_service_badge_codes WHERE badge_type = '" <> T.unpack badgeType <> "'"
    pure $ case months of
      [m] -> Just m
      _ -> Nothing

firstText :: ChatController -> String -> IO (Maybe Text)
firstText cc cond = queryFirst cc ("SELECT item_text FROM chat_items WHERE " <> cond)

trackerItem :: HasCallStack => ChatController -> Int -> IO (Maybe (Int64, Text))
trackerItem cc uses = singleRow sql <$> queryRows cc sql
  where
    sql =
      "SELECT c.group_item_id, ci.item_text "
        <> "FROM sx_badge_service_badge_codes c "
        <> "JOIN chat_items ci ON ci.chat_item_id = c.group_item_id "
        <> "WHERE c.redeem_limit = "
        <> show uses

waitTrackerItemOf :: HasCallStack => ChatController -> Int -> IO (Int64, Text)
waitTrackerItemOf cc = pollUntil . trackerItem cc

waitTrackerRepost :: HasCallStack => ChatController -> Int -> Int64 -> IO (Int64, Text)
waitTrackerRepost cc uses itemId0 = pollUntil $ mfilter ((/= itemId0) . fst) <$> trackerItem cc uses

redeemedTrackerCount :: ChatController -> IO Int
redeemedTrackerCount cc = countRows cc "chat_items WHERE item_text LIKE '%Last redeemed%'"

revokedTrackerCount :: ChatController -> IO Int
revokedTrackerCount cc = countRows cc ("chat_items WHERE item_text LIKE '" <> T.unpack (retiredText "SB-%") <> "'")

retiredText :: Text -> Text
retiredText code = code <> " revoked — no longer redeemable"

exhaustedText :: Text -> Int -> Text
exhaustedText code uses = code <> " fully redeemed — all " <> tshow uses <> " used"

-- Notices are not filtered by code or total, so a wrong one still shows up.
exhaustedNotices :: ChatController -> IO [Text]
exhaustedNotices cc = queryColumn cc "SELECT item_text FROM chat_items WHERE item_text LIKE '%fully redeemed%' ORDER BY chat_item_id"

waitExhausted :: HasCallStack => ChatController -> IO Text
waitExhausted cc =
  pollUntil (mfilter (not . null) . Just <$> exhaustedNotices cc) >>= \case
    [t] -> pure t
    ts -> error $ "expected one exhaustion notice, got " <> show (length ts)

extractCode :: HasCallStack => Text -> Text
extractCode t = maybe (error ("no badge code in: " <> T.unpack t)) formatBadgeCode (codeInTracker t)
