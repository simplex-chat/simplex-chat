{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module BadgeService.Group
  ( ensureManagedGroup,
    runGroupLane,
    inertGroupConfig,
    GroupEvent (..),
    GroupAction (..),
    groupEvent,
    hasTracker,
    refreshTracker,
    revokeWithTracker,
    coalesceTrackerRefreshes,
    TrackerAction (..),
    trackerDecision,
    codeInTracker,
    noOwnerHint,
    orphanHint,
  )
where

import BadgeService.Codes (issueFailedText, issueOneCode, revokeBadgeCode, singleUse)
import BadgeService.Config (GroupConfig (..))
import BadgeService.Group.Command (CmdAction (..), GroupCmd (..), groupCmdAction, groupCommands)
import BadgeService.Log (logError, logInfo, logWarn)
import BadgeService.Store (CodeTracker (..), ManagedGroup (..), RevokeResult (..), clearCodeGroupItems, getCodeTracker, getEditableTrackers, getManagedGroup, insertManagedGroup, markOwnerBootstrapped, setCodeGroupItem)
import BadgeService.Store.Invoices (truncateToSecond)
import Control.Concurrent.STM (TQueue, atomically, flushTQueue, readTQueue, readTVarIO, writeTQueue)
import Control.Monad (forM_, forever, mfilter, replicateM, unless, void, when)
import Control.Monad.Except (runExceptT)
import Data.Either (partitionEithers)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Simplex.Chat.Badges.Code (BadgeCode, formatBadgeCode, parseBadgeCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd)
import Simplex.Chat.Markdown (viewName)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent (CIContent (..), ciContentToText)
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store.Messages (getGroupChatItem)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences (GroupPreferences (..), commands_, emptyGroupPrefs)
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Chat.View (simplexChatContact)
import Simplex.Messaging.Agent.Protocol (CreatedConnLink (..), UserId)
import Simplex.Messaging.Encoding.String (StrEncoding, strEncode)
import Simplex.Messaging.Util (catchOwn', safeDecodeUtf8, tshow, ($>>=))
import System.Exit (exitFailure)

buildGroupProfile :: GroupConfig -> GroupProfile
buildGroupProfile GroupConfig {gDisplayName, gDescription} =
  GroupProfile
    { displayName = gDisplayName,
      fullName = "",
      shortDescr = Nothing,
      description = gDescription,
      image = Nothing,
      publicGroup = Nothing,
      groupPreferences = Just (emptyGroupPrefs {commands = Just groupCommands} :: GroupPreferences),
      memberAdmission = Nothing
    }

ensureManagedGroup :: ChatController -> GroupConfig -> IO (Maybe GroupId)
ensureManagedGroup cc gc =
  withDB' "getManagedGroup" cc getManagedGroup >>= \case
    -- A read error must not fall through to create, which would orphan a second group.
    -- The service stops instead, so a restart retries rather than leaving the group unserved.
    Left _ -> logError "badge group lookup failed, stopping" >> exitFailure
    -- The join link is a bearer secret, logged only where it is created.
    Right (Just ManagedGroup {mgGroupId}) ->
      sendChatCmd cc (APIGroupInfo mgGroupId) >>= \case
        Right CRGroupInfo {groupInfo = GroupInfo {membership, groupProfile = p}}
          | memberCurrent membership -> do
              forM_ (inertGroupConfig gc p) logWarn
              unless (commandsCurrent p) $ advertiseCommands cc mgGroupId p
              logInfo "badge group ready"
              pure (Just mgGroupId)
          | otherwise -> groupGone
        Left (ChatErrorStore SEGroupNotFound {}) -> groupGone
        r -> do
          logError ("badge group info failed: " <> tshow r)
          pure (Just mgGroupId)
    Right Nothing ->
      readTVarIO (currentUser cc) >>= \case
        Nothing -> logError "badge group not created: no current user" >> pure Nothing
        Just User {userId} -> createManagedGroup cc gc userId
  where
    groupGone = do
      logError "badge group was deleted or the service was removed from it; delete the sx_badge_service_group row and restart to create a new group"
      pure Nothing

createManagedGroup :: ChatController -> GroupConfig -> UserId -> IO (Maybe GroupId)
createManagedGroup cc gc userId =
  sendChatCmd cc (APINewGroup userId False (buildGroupProfile gc)) >>= \case
    Right CRGroupCreated {groupInfo = g@GroupInfo {groupId}} ->
      sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
        Right CRGroupLinkCreated {groupLink = GroupLink {connLinkContact}} -> do
          now <- truncateToSecond <$> getCurrentTime
          let linkText = groupLinkText connLinkContact
          withDB' "insertManagedGroup" cc (\db -> insertManagedGroup db groupId linkText now >> clearCodeGroupItems db) >>= \case
            Right () -> do
              logInfo $ "badge group created, join link: " <> linkText
              pure (Just groupId)
            Left _ -> logError ("badge group " <> tshow groupId <> " not recorded - " <> orphanHint (groupName' g)) >> pure Nothing
        r -> logError ("badge group " <> tshow groupId <> " link failed: " <> tshow r <> " - " <> orphanHint (groupName' g)) >> pure Nothing
    r -> logError ("badge group creation failed: " <> tshow r) >> pure Nothing

-- The next start creates another group, so a partly created one is left for the operator to delete.
orphanHint :: GroupName -> Text
orphanHint gName = "delete this orphan group with /d #" <> viewName gName <> " in --run-cli mode"

-- An existing group gets the current commands, so an upgrade that changes them reaches its members.
commandsCurrent :: GroupProfile -> Bool
commandsCurrent groupProfile = (groupPreferences groupProfile >>= commands_) == Just groupCommands

advertiseCommands :: ChatController -> GroupId -> GroupProfile -> IO ()
advertiseCommands cc groupId groupProfile =
  sendChatCmd cc (APIUpdateGroupProfile groupId p') >>= \case
    Right CRGroupUpdated {} -> logInfo "badge group commands advertised"
    r -> logError ("badge group profile update failed: " <> tshow r)
  where
    prefs = fromMaybe emptyGroupPrefs (groupPreferences groupProfile)
    p' = groupProfile {groupPreferences = Just (prefs {commands = Just groupCommands} :: GroupPreferences)}

-- Config is applied only at creation, since a group rename is broadcast to every member.
inertGroupConfig :: GroupConfig -> GroupProfile -> Maybe Text
inertGroupConfig GroupConfig {gDisplayName, gDescription} GroupProfile {displayName, description}
  | null diverged = Nothing
  | otherwise = Just $ "badge group config is not applied to an existing group: " <> T.intercalate "; " diverged
  where
    diverged =
      [ field <> " \"" <> configured <> "\", group has \"" <> live <> "\""
        | (field, configured, live) <-
            [ ("display_name", gDisplayName, displayName),
              ("description", fromMaybe "" gDescription, fromMaybe "" description)
            ],
          configured /= live
      ]

groupLinkText :: CreatedLinkContact -> Text
groupLinkText (CCLink cReq sLnk_) = maybe (strEncodeTxt (simplexChatContact cReq)) strEncodeTxt sLnk_

strEncodeTxt :: StrEncoding a => a -> Text
strEncodeTxt = safeDecodeUtf8 . strEncode

-- GETracker carries the redeem count its claim reached.
data GroupEvent
  = GEInGroup GroupId GroupAction
  | GETracker Int64 BadgeCode Int
  deriving (Eq, Show)

data GroupAction
  = GAJoined
  | GACommand GroupMemberRole Text
  deriving (Eq, Show)

-- Support-scope, moderated or blocked, and live items are ignored: the reply would go to the main group,
-- moderation or blocking without Full Delete keeps the content, and a live item holds only partial text.
groupEvent :: ChatEvent -> Maybe GroupEvent
groupEvent = \case
  CEvtJoinedGroupMember {groupInfo = GroupInfo {groupId}} -> Just $ GEInGroup groupId GAJoined
  CEvtNewChatItems {chatItems = AChatItem _ _ (GroupChat GroupInfo {groupId} scope) ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent (MCText t), meta = CIMeta {itemDeleted, itemLive}} : _}
    | isNothing scope && isNothing itemDeleted && itemLive /= Just True -> Just $ GEInGroup groupId (GACommand (memberRole' m) t)
  _ -> Nothing

coalesceTrackerRefreshes :: [GroupEvent] -> [GroupEvent]
coalesceTrackerRefreshes evs = snd $ foldr keepOne (highestClaims, []) evs
  where
    -- The highest claim decides the exhaustion notice, whatever order the claims were queued in.
    highestClaims = M.fromListWith max [(badgeCodeId, n) | GETracker badgeCodeId _ n <- evs]
    keepOne ev (todo, kept) = case ev of
      GETracker badgeCodeId code _ -> case M.lookup badgeCodeId todo of
        Just n -> (M.delete badgeCodeId todo, GETracker badgeCodeId code n : kept)
        Nothing -> (todo, kept)
      _ -> (todo, ev : kept)

logUncaught :: HasCallStack => IO () -> IO ()
logUncaught a = a `catchOwn'` withFrozenCallStack (logError . tshow)

handleGroupEvent :: ChatController -> GroupId -> GroupEvent -> IO ()
handleGroupEvent cc groupId ev = logUncaught (handle ev)
  where
    handle = \case
      GEInGroup gid action
        | gid /= groupId -> pure ()
        | otherwise -> case action of
            GAJoined -> promoteOwner cc groupId
            GACommand role t -> case groupCmdAction role t of
              RunCmd cmd -> runGroupCmd cc groupId cmd
              ReplyText txt -> void $ sendGroupText cc groupId "command reply" txt
              IgnoreMsg -> pure ()
      GETracker badgeCodeId code claimedCount -> updateTracker cc groupId badgeCodeId code claimedCount

promoteFirstOwner :: ChatController -> GroupInfo -> GroupMember -> IO ()
promoteFirstOwner cc g@GroupInfo {groupId} member =
  -- The flag is set before promoting, so a retry cannot promote a second owner.
  withDB' "markOwnerBootstrapped" cc (`markOwnerBootstrapped` groupId) >>= \case
    Right True ->
      sendChatCmd cc (APIMembersRole groupId (groupMemberId' member :| []) GROwner) >>= \case
        -- The core returns no members when every store update failed.
        Right CRMembersRoleUser {members = _ : _} -> logInfo $ "badge group owner promoted: member " <> tshow (groupMemberId' member)
        r -> logError $ "badge group owner promotion failed: " <> tshow r <> " - " <> noOwnerHint (groupName' g)
    _ -> pure ()

-- Nothing is a group that is not served, so its events are drained.
runGroupLane :: ChatController -> TQueue GroupEvent -> Maybe GroupId -> IO ()
runGroupLane cc q groupId_ = case groupId_ of
  Just groupId -> do
    promoteOwner cc groupId
    -- Reconciling precedes the first batch, so queued events write after the correction.
    reconcileTrackers cc groupId
    forever $ do
      evs <- atomically $ (:) <$> readTQueue q <*> flushTQueue q
      mapM_ (handleGroupEvent cc groupId) (coalesceTrackerRefreshes evs)
  Nothing -> forever $ void $ atomically (readTQueue q)

-- The earliest joined member is promoted, not the one who just joined, so a join the lane never handled
-- (under --run-cli, or lost in a crash) or a failed attempt never hands the group to a later joiner.
promoteOwner :: ChatController -> GroupId -> IO ()
promoteOwner cc groupId =
  withDB' "getManagedGroup" cc getManagedGroup >>= \case
    Right (Just ManagedGroup {mgOwnerBootstrapped}) ->
      sendChatCmd cc (APIListMembers groupId) >>= \case
        Right CRGroupMembers {group = Group {groupInfo, members}}
          -- An owner made by hand only needs the flag set, or promoting would add a second owner.
          | any (\m -> memberRole' m == GROwner && memberCurrent m) members ->
              unless mgOwnerBootstrapped $ void $ withDB' "markOwnerBootstrapped" cc (`markOwnerBootstrapped` groupId)
          -- A crash between setting the flag and promoting leaves no owner, and only the operator may retry.
          | mgOwnerBootstrapped -> logError $ noOwnerHint (groupName' groupInfo)
          | otherwise -> mapM_ (promoteFirstOwner cc groupInfo) $ listToMaybe $ sortOn groupMemberId' $ filter joined members
        r -> logError $ "badge group members not listed: " <> tshow r
    _ -> pure ()
  where
    -- A member still joining cannot act yet, so it is not a candidate.
    joined m = memberStatus m `elem` [GSMemConnected, GSMemComplete]

-- The live local name, quoted as --run-cli parses it, can differ from the configured one after a name clash or a rename.
noOwnerHint :: GroupName -> Text
noOwnerHint gName = "the badge group has no member owner, make one with /mr #" <> viewName gName <> " <member> owner in --run-cli mode"

runGroupCmd :: ChatController -> GroupId -> GroupCmd -> IO ()
runGroupCmd cc groupId = \case
  GCIssue bt months uses ->
    issueOneCode cc bt months CPSFree uses >>= \case
      Left _ -> reply issueFailedText
      Right (code, badgeCodeId)
        | not (hasTracker uses) -> reply ("code " <> formatBadgeCode code)
        | otherwise -> do
            now <- truncateToSecond <$> getCurrentTime
            sendGroupText cc groupId ("tracker, code " <> tshow badgeCodeId <> " is lost") (initialTrackerBody code uses)
              >>= mapM_ (\iid -> withDB' "setCodeGroupItem" cc $ \db -> setCodeGroupItem db badgeCodeId iid now)
  GCBulk bt months count -> do
    codes <- replicateM count (issueOneCode cc bt months CPSFree singleUse)
    let (errs, ok) = partitionEithers codes
        issued = map (formatBadgeCode . fst) ok
    reply . T.intercalate "\n" $ case errs of
      [] -> issued
      _ : _ -> issued <> ["issued " <> tshow (length issued) <> " of " <> tshow count <> ", the rest failed"]
  -- Naming the code tells concurrent revokes apart; the command already made it public.
  GCRevoke code -> do
    outcome <- either id id <$> revokeWithTracker cc code
    reply (formatBadgeCode code <> ": " <> outcome)
  where
    reply = void . sendGroupText cc groupId "reply, any codes in it are lost"

-- The label says what is lost when the send fails, since the log line is its only trace.
sendGroupText :: HasCallStack => ChatController -> GroupId -> Text -> Text -> IO (Maybe ChatItemId)
sendGroupText cc groupId label txt =
  sendChatCmd cc (APISendMessages (SRGroup groupId Nothing False) False Nothing False (ComposedMessage Nothing Nothing (MCText txt) M.empty :| [])) >>= \case
    Right CRNewChatItems {chatItems = ci : _} -> pure (Just (aChatItemId ci))
    r -> withFrozenCallStack logError ("badge group message not sent (" <> label <> "): " <> tshow r) $> Nothing

initialTrackerBody :: BadgeCode -> Int -> Text
initialTrackerBody code total = trackerBody code total total Nothing

-- The body is dated by the last redemption, not now, because reconcile rewrites it after a restart.
trackerBody :: BadgeCode -> Int -> Int -> Maybe UTCTime -> Text
trackerBody code remaining total redeemedAt =
  "!2 " <> formatBadgeCode code <> "!\n" <> maybe "" lastRedeemed redeemedAt <> tshow remaining <> "/" <> tshow total <> " remaining"
  where
    lastRedeemed ts = "Last redeemed: " <> fmtDay ts <> " — "

exhaustedBody :: BadgeCode -> Int -> Text
exhaustedBody code total = formatBadgeCode code <> " fully redeemed — all " <> tshow total <> " used"

revokedBody :: BadgeCode -> Text
revokedBody code = formatBadgeCode code <> " revoked — no longer redeemable"

fmtDay :: UTCTime -> Text
fmtDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

data TrackerAction = Edit | Repost
  deriving (Eq, Show)

-- The core refuses to edit a sent message older than this.
editWindow :: NominalDiffTime
editWindow = nominalDay

trackerDecision :: UTCTime -> UTCTime -> TrackerAction
trackerDecision now sentAt
  | diffUTCTime now sentAt < editWindow = Edit
  | otherwise = Repost

-- A repost publishes the code a second time, so the reconcile pass may only edit.
data RepostPolicy = MayRepost | EditOnly

-- A deleted or moderated tracker is not reposted, because deleting it does not revoke the code.
-- With MayRepost the result is Nothing when there is nothing to write or the message is gone, so no notice follows.
setTrackerBody :: ChatController -> GroupId -> Int64 -> RepostPolicy -> (CodeTracker -> Maybe Text) -> IO (Maybe CodeTracker)
setTrackerBody cc groupId badgeCodeId policy mkBody =
  withDB' "getCodeTracker" cc (`getCodeTracker` badgeCodeId) >>= \case
    Right (Just tracker@CodeTracker {trackerItemId, trackerSentAt}) ->
      pure (mkBody tracker) $>>= \body -> do
        now <- truncateToSecond <$> getCurrentTime
        let handled = pure (Just tracker)
            repost =
              trackerItemText cc groupId trackerItemId >>= \case
                Nothing -> do
                  logWarn $ "badge group tracker not reposted, code " <> tshow badgeCodeId <> " is no longer published"
                  pure Nothing
                -- Past the window every write reposts, so an unchanged body is not posted again.
                Just current
                  | current == body -> handled
                  | otherwise -> do
                      sendGroupText cc groupId ("tracker repost, code " <> tshow badgeCodeId <> " keeps its old message") body
                        >>= mapM_ (\i -> withDB' "setCodeGroupItem" cc (\db -> setCodeGroupItem db badgeCodeId i now))
                      handled
            -- The core can also refuse an edit inside the window, because it uses the message's own timestamp.
            uneditable = case policy of
              MayRepost -> repost
              EditOnly -> do
                logWarn $ "badge group tracker left uncorrected, code " <> tshow badgeCodeId <> " can no longer be edited"
                handled
        case trackerDecision now trackerSentAt of
          Edit ->
            sendChatCmd cc (APIUpdateChatItem (ChatRef CTGroup groupId Nothing) trackerItemId False (UpdatedMessage (MCText body) M.empty)) >>= \case
              Right CRChatItemUpdated {} -> handled
              Right CRChatItemNotChanged {} -> handled
              Left (ChatError CEInvalidChatItemUpdate) -> uneditable
              -- Any other failure may still have applied the edit, so a repost could publish the code twice.
              -- The notice still follows the claim while the tracker is published.
              r -> do
                logError $ "badge group tracker not updated, code " <> tshow badgeCodeId <> ": " <> tshow r
                ($> tracker) <$> trackerItemText cc groupId trackerItemId
          Repost -> uneditable
    _ -> pure Nothing

-- A revoke and a redemption can arrive in either order, so a revoked tracker is left alone.
counterBody :: BadgeCode -> CodeTracker -> Maybe Text
counterBody code CodeTracker {redeemCount, redeemLimit, revokedAt, redeemedAt}
  | isJust revokedAt = Nothing
  | otherwise = Just $ trackerBody code (redeemLimit - redeemCount) redeemLimit redeemedAt

hasTracker :: Int -> Bool
hasTracker redeemLimit = redeemLimit > singleUse

-- The tracker edit reaches every member, so the group lane runs it off the request path.
-- Without a lane (--run-cli, or no [group]) it runs here, before the response.
refreshTracker :: ChatController -> Maybe (TQueue GroupEvent) -> Int64 -> BadgeCode -> Int -> IO ()
refreshTracker cc trackerQ_ badgeCodeId code claimedCount = case trackerQ_ of
  Just q -> atomically $ writeTQueue q (GETracker badgeCodeId code claimedCount)
  Nothing -> logUncaught $ withManagedGroup cc $ \groupId -> updateTracker cc groupId badgeCodeId code claimedCount

-- The notice follows the claim, not the count read now, so queued refreshes post it once.
updateTracker :: ChatController -> GroupId -> Int64 -> BadgeCode -> Int -> IO ()
updateTracker cc groupId badgeCodeId code claimedCount =
  setTrackerBody cc groupId badgeCodeId MayRepost (counterBody code)
    >>= mapM_ (\CodeTracker {redeemLimit} -> when (claimedCount == redeemLimit) $ postNotice redeemLimit)
  where
    postNotice redeemLimit = void $ sendGroupText cc groupId ("notice, code " <> tshow badgeCodeId) (exhaustedBody code redeemLimit)

-- | Left is a refusal or a failure; both sides are the text to show whoever sent the revoke.
revokeWithTracker :: ChatController -> BadgeCode -> IO (Either Text Text)
revokeWithTracker cc code =
  revokeBadgeCode cc code >>= \case
    -- The message is retired before the answer, so "revoked" never sits beside a live counter.
    Right (Revoked badgeCodeId) -> Right "revoked" <$ retire badgeCodeId
    -- A repeated revoke repairs a message that an earlier revoke failed to update.
    Right (AlreadyRevoked badgeCodeId) -> Right "already revoked" <$ retire badgeCodeId
    Right AlreadyRedeemed -> pure $ Left "code was redeemed already, so it cannot be revoked"
    Right NoSuchCode -> pure $ Left "no such code"
    Left _ -> pure $ Left "revoking the code failed"
  where
    retire badgeCodeId = logUncaught $ withManagedGroup cc $ \groupId ->
      void $ setTrackerBody cc groupId badgeCodeId MayRepost (const $ Just $ revokedBody code)

withManagedGroup :: ChatController -> (GroupId -> IO ()) -> IO ()
withManagedGroup cc action =
  withDB' "getManagedGroup" cc getManagedGroup >>= \case
    Right (Just ManagedGroup {mgGroupId}) -> action mgGroupId
    _ -> pure ()

-- A claim's refresh is queued after the claim commits, so a crash can drop it.
reconcileTrackers :: ChatController -> GroupId -> IO ()
reconcileTrackers cc groupId = do
  editableAfter <- addUTCTime (-editWindow) <$> getCurrentTime
  withDB' "getEditableTrackers" cc (`getEditableTrackers` editableAfter) >>= \case
    Left _ -> logError "badge group trackers not reconciled: tracked code lookup failed"
    Right codes -> forM_ codes $ \(badgeCodeId, itemId) -> logUncaught $ reconcileTracker cc groupId badgeCodeId itemId

-- An unchanged edit still walks every member, so a tracker already showing the right text is skipped.
reconcileTracker :: ChatController -> GroupId -> Int64 -> ChatItemId -> IO ()
reconcileTracker cc groupId badgeCodeId itemId =
  readTrackerCode cc groupId badgeCodeId itemId >>= mapM_ reconcile
  where
    reconcile (code, current) = void $ setTrackerBody cc groupId badgeCodeId EditOnly (correctedBody code current)
    -- A crash after a revoke committed can leave its tracker still counting, so a revoked code is retired here.
    correctedBody code current tracker@CodeTracker {revokedAt} =
      mfilter (/= current) $ if isJust revokedAt then Just (revokedBody code) else counterBody code tracker

readTrackerCode :: ChatController -> GroupId -> Int64 -> ChatItemId -> IO (Maybe (BadgeCode, Text))
readTrackerCode cc groupId badgeCodeId itemId =
  trackerItemText cc groupId itemId >>= \case
    Nothing -> do
      logWarn $ "badge group tracker not read, code " <> tshow badgeCodeId
      pure Nothing
    Just current -> case codeInTracker current of
      Nothing -> do
        logError $ "badge group tracker carries no readable code, code " <> tshow badgeCodeId
        pure Nothing
      Just code -> pure (Just (code, current))

codeInTracker :: Text -> Maybe BadgeCode
codeInTracker = listToMaybe . mapMaybe parseBadgeCode . T.words

-- The item is read from the store, because the core's item info also loads every edit and every member's delivery status.
-- Moderation can keep a deleted item's content, so itemDeleted is checked.
trackerItemText :: ChatController -> GroupId -> ChatItemId -> IO (Maybe Text)
trackerItemText cc groupId itemId =
  readTVarIO (currentUser cc) $>>= \user ->
    withDB' "getGroupChatItem" cc (\db -> runExceptT $ getGroupChatItem db user groupId itemId) <&> \case
      Right (Right (CChatItem _ ChatItem {content, meta = CIMeta {itemDeleted}})) | isNothing itemDeleted -> Just (ciContentToText content)
      _ -> Nothing
