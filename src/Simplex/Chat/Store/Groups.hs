{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Groups
  ( -- * Util methods
    GroupInfoRow,
    GroupMemberRow,
    MaybeGroupMemberRow,
    toGroupInfo,
    toGroupMember,
    toMaybeGroupMember,

    -- * Group functions
    createGroupLink,
    getGroupLinkConnection,
    deleteGroupLink,
    getGroupLink,
    getGroupLinkId,
    setGroupLinkMemberRole,
    setGroupLinkShortLink,
    createNewGroup,
    createGroupInvitation,
    deleteContactCardKeepConn,
    createPreparedGroup,
    updatePreparedGroupUser,
    updatePreparedUserAndHostMembersInvited,
    updatePreparedUserAndHostMembersRejected,
    createGroupInvitedViaLink,
    createGroupRejectedViaLink,
    setGroupInvitationChatItemId,
    getGroup,
    getGroupInfo,
    getGroupInfoByUserContactLinkConnReq,
    getGroupInfoViaUserShortLink,
    getGroupViaShortLinkToConnect,
    getGroupInfoByGroupLinkHash,
    updateGroupProfile,
    updateGroupPreferences,
    updateGroupProfileFromMember,
    getGroupIdByName,
    getGroupMemberIdByName,
    getActiveMembersByName,
    getGroupInfoByName,
    getGroupMember,
    getHostMember,
    getMentionedGroupMember,
    getMentionedMemberByMemberId,
    getGroupMemberById,
    getGroupMemberByMemberId,
    getGroupMemberIdViaMemberId,
    getScopeMemberIdViaMemberId,
    getGroupMembers,
    getGroupModerators,
    getGroupRelayMembers,
    getGroupMembersForExpiration,
    getGroupCurrentMembersCount,
    deleteGroupChatItems,
    deleteGroupMembers,
    cleanupHostGroupLinkConn,
    deleteGroup,
    getBaseGroupDetails,
    getContactGroupPreferences,
    getGroupInvitation,
    createNewContactMember,
    createGroupRelayRecord,
    getGroupRelayById,
    getGroupRelays,
    createRelayForOwner,
    createRelayForMember,
    createRelayConnection,
    updateRelayStatus,
    updateRelayStatusFromTo,
    setRelayLinkAccepted,
    createGroupRelayInvitation,
    updateRelayOwnStatusFromTo,
    createNewContactMemberAsync,
    createJoiningMember,
    getMemberJoinRequest,
    createJoiningMemberConnection,
    createBusinessRequestGroup,
    getContactViaMember,
    setNewContactMemberConnRequest,
    getMemberInvitation,
    createMemberConnection,
    createMemberConnectionAsync,
    updateGroupMemberStatus,
    updateGroupMemberStatusById,
    updateGroupMemberAccepted,
    deleteGroupMemberSupportChat,
    updateGroupMembersRequireAttention,
    decreaseGroupMembersRequireAttention,
    increaseGroupMembersRequireAttention,
    createNewGroupMember,
    checkGroupMemberHasItems,
    deleteGroupMember,
    deleteGroupMemberConnection,
    updateGroupMemberRole,
    createIntroductions,
    updateIntroStatus,
    getIntroduction,
    getIntroducedGroupMemberIds,
    getForwardIntroducedMembers,
    getForwardIntroducedModerators,
    getForwardInvitedMembers,
    getForwardInvitedModerators,
    getForwardScopeMember,
    createIntroReMember,
    createIntroReMemberConn,
    createIntroToMemberContact,
    getMatchingContacts,
    getMatchingMembers,
    getMatchingMemberContacts,
    createSentProbe,
    createSentProbeHash,
    matchReceivedProbe,
    matchReceivedProbeHash,
    matchSentProbe,
    associateMemberWithContactRecord,
    associateContactWithMemberRecord,
    deleteOldProbes,
    updateGroupSettings,
    updateGroupMemberSettings,
    updateGroupMemberBlocked,
    getHostConnId,
    createMemberContact,
    getMemberContact,
    setContactGrpInvSent,
    createMemberContactInvited,
    updateMemberContactInvited,
    createMemberContactConn,
    getMemberContactInvited,
    setMemberContactStartedConnection,
    resetMemberContactFields,
    updateMemberProfile,
    updateContactMemberProfile,
    getXGrpLinkMemReceived,
    setXGrpLinkMemReceived,
    createNewUnknownGroupMember,
    updateUnknownMemberAnnounced,
    updateUserMemberProfileSentAt,
    setGroupCustomData,
    setGroupUIThemes,
    updateGroupChatTags,
    getGroupChatTags,
    setGroupChatTTL,
    getGroupChatTTL,
    getUserGroupsToExpire,
    updateGroupAlias,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import Data.Bifunctor (second)
import Data.Char (toLower)
import Data.Either (rights)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (partition, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Messages
import Simplex.Chat.Operators
import Simplex.Chat.Protocol hiding (Binary)
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Protocol (ConnId, CreatedConnLink (..), UserId)
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, fromOnlyBI, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet (pattern PQEncOff, pattern PQSupportOff)
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Util (eitherToMaybe, firstRow', safeDecodeUtf8, ($>>), ($>>=), (<$$>))
import Simplex.Messaging.Version
import UnliftIO.STM
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

type MaybeGroupMemberRow = (Maybe Int64, Maybe Int64, Maybe MemberId, Maybe VersionChat, Maybe VersionChat, Maybe GroupMemberRole, Maybe GroupMemberCategory, Maybe GroupMemberStatus, Maybe BoolInt, Maybe MemberRestrictionStatus) :. (Maybe Int64, Maybe GroupMemberId, Maybe ContactName, Maybe ContactId, Maybe ProfileId) :. (Maybe ProfileId, Maybe ContactName, Maybe Text, Maybe Text, Maybe ImageData, Maybe ConnLinkContact, Maybe ChatPeerType, Maybe LocalAlias, Maybe Preferences) :. (Maybe UTCTime, Maybe UTCTime) :. (Maybe UTCTime, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime) :. (Maybe BoolInt, Maybe Int64, Maybe Int64, Maybe RelayStatus, Maybe ShortLinkContact)

toMaybeGroupMember :: Int64 -> MaybeGroupMemberRow -> Maybe GroupMember
toMaybeGroupMember userContactId ((Just groupMemberId, Just groupId, Just memberId, Just minVer, Just maxVer, Just memberRole, Just memberCategory, Just memberStatus, Just showMessages, memberBlocked') :. (invitedById, invitedByGroupMemberId, Just localDisplayName, memberContactId, Just memberContactProfileId) :. (Just profileId, Just displayName, Just fullName, shortDescr, image, contactLink, peerType, Just localAlias, contactPreferences) :. (Just createdAt, Just updatedAt) :. (supportChatTs, Just supportChatUnread, Just supportChatUnanswered, Just supportChatMentions, supportChatLastMsgFromMemberTs) :. (Just isRelay, groupRelayId, chatRelayId, relayStatus, relayLink)) =
  Just $ toGroupMember userContactId ((groupMemberId, groupId, memberId, minVer, maxVer, memberRole, memberCategory, memberStatus, showMessages, memberBlocked') :. (invitedById, invitedByGroupMemberId, localDisplayName, memberContactId, memberContactProfileId) :. (profileId, displayName, fullName, shortDescr, image, contactLink, peerType, localAlias, contactPreferences) :. (createdAt, updatedAt) :. (supportChatTs, supportChatUnread, supportChatUnanswered, supportChatMentions, supportChatLastMsgFromMemberTs) :. (isRelay, groupRelayId, chatRelayId, relayStatus, relayLink))
toMaybeGroupMember _ _ = Nothing

createGroupLink :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> ConnId -> CreatedLinkContact -> GroupLinkId -> GroupMemberRole -> SubscriptionMode -> ExceptT StoreError IO GroupLink
createGroupLink db gVar user@User {userId} groupInfo@GroupInfo {groupId, localDisplayName} agentConnId (CCLink cReq shortLink) groupLinkId memberRole subMode = do
  checkConstraint (SEDuplicateGroupLink groupInfo) . liftIO $ do
    currentTs <- getCurrentTime
    randSuffix <- liftIO $ encodedRandomBytes gVar 12
    let groupLinkLDN = "group_link_" <> localDisplayName <> "_" <> safeDecodeUtf8 randSuffix
        slDataSet = BI (isJust shortLink)
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, group_id, group_link_id, local_display_name, conn_req_contact, short_link_contact, short_link_data_set, short_link_large_data_set, group_link_member_role, auto_accept, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
      ((userId, groupId, groupLinkId, groupLinkLDN, cReq, shortLink, slDataSet, slDataSet) :. (memberRole, BI True, currentTs, currentTs))
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId ConnNew initialChatVersion chatInitialVRange Nothing Nothing Nothing 0 currentTs subMode PQSupportOff
  getGroupLink db user groupInfo

getGroupLinkConnection :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> ExceptT StoreError IO Connection
getGroupLinkConnection db vr User {userId} groupInfo@GroupInfo {groupId} =
  ExceptT . firstRow (toConnection vr) (SEGroupLinkNotFound groupInfo) $
    DB.query
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.xcontact_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.user_contact_link_id,
          c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
          c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
        FROM connections c
        JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
        WHERE c.user_id = ? AND uc.user_id = ? AND uc.group_id = ?
      |]
      (userId, userId, groupId)

deleteGroupLink :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupLink db User {userId} GroupInfo {groupId} = do
  DB.execute
    db
    [sql|
      DELETE FROM connections WHERE connection_id IN (
        SELECT connection_id
        FROM connections c
        JOIN user_contact_links uc USING (user_contact_link_id)
        WHERE uc.user_id = ? AND uc.group_id = ?
      )
    |]
    (userId, groupId)
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ?
        AND local_display_name in (
          SELECT cr.local_display_name
          FROM contact_requests cr
          JOIN user_contact_links uc USING (user_contact_link_id)
          WHERE uc.user_id = ? AND uc.group_id = ?
        )
        AND local_display_name NOT IN (SELECT local_display_name FROM users WHERE user_id = ?)
    |]
    (userId, userId, groupId, userId)
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT cr.contact_profile_id
        FROM contact_requests cr
        JOIN user_contact_links uc USING (user_contact_link_id)
        WHERE uc.user_id = ? AND uc.group_id = ?
      )
    |]
    (userId, groupId)
  DB.execute db "DELETE FROM user_contact_links WHERE user_id = ? AND group_id = ?" (userId, groupId)

getGroupLink :: DB.Connection -> User -> GroupInfo -> ExceptT StoreError IO GroupLink
getGroupLink db User {userId} gInfo@GroupInfo {groupId} =
  ExceptT . firstRow toGroupLink (SEGroupLinkNotFound gInfo) $
    DB.query db "SELECT user_contact_link_id, conn_req_contact, short_link_contact, short_link_data_set, short_link_large_data_set, group_link_id, group_link_member_role FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)
  where
    toGroupLink (userContactLinkId, cReq, shortLink, BI shortLinkDataSet, BI slLargeDataSet, groupLinkId, mRole_) =
      GroupLink {
        userContactLinkId,
        connLinkContact = CCLink cReq shortLink,
        shortLinkDataSet,
        shortLinkLargeDataSet = BoolDef slLargeDataSet,
        groupLinkId,
        acceptMemberRole = fromMaybe GRMember mRole_
      }

getGroupLinkId :: DB.Connection -> User -> GroupInfo -> IO (Maybe GroupLinkId)
getGroupLinkId db User {userId} GroupInfo {groupId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT group_link_id FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)

setGroupLinkMemberRole :: DB.Connection -> User -> GroupLink -> GroupMemberRole -> IO GroupLink
setGroupLinkMemberRole db User {userId} gLnk@GroupLink{userContactLinkId} memberRole = do
  DB.execute db "UPDATE user_contact_links SET group_link_member_role = ? WHERE user_id = ? AND user_contact_link_id = ?" (memberRole, userId, userContactLinkId)
  pure gLnk {acceptMemberRole = memberRole}

setGroupLinkShortLink :: DB.Connection -> GroupLink -> ShortLinkContact -> IO GroupLink
setGroupLinkShortLink db gLnk@GroupLink {userContactLinkId, connLinkContact = CCLink connFullLink _sLnk_} shortLink = do
  DB.execute
    db
    [sql|
      UPDATE user_contact_links
      SET short_link_contact = ?,
          short_link_data_set = ?,
          short_link_large_data_set = ?
      WHERE user_contact_link_id = ?
    |]
    (shortLink, BI True, BI True, userContactLinkId)
  pure gLnk {connLinkContact = CCLink connFullLink (Just shortLink), shortLinkDataSet = True, shortLinkLargeDataSet = BoolDef True}

-- | creates completely new group with a single member - the current user
createNewGroup :: DB.Connection -> VersionRangeChat -> TVar ChaChaDRG -> User -> GroupProfile -> Maybe Profile -> Bool -> ExceptT StoreError IO GroupInfo
createNewGroup db vr gVar user@User {userId} groupProfile incognitoProfile useRelays = ExceptT $ do
  let GroupProfile {displayName, fullName, shortDescr, description, image, groupPreferences, memberAdmission} = groupProfile
      fullGroupPreferences = mergeGroupPreferences groupPreferences
  currentTs <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId currentTs) incognitoProfile
  withLocalDisplayName db userId displayName $ \ldn -> runExceptT $ do
    groupId <- liftIO $ do
      DB.execute
        db
        "INSERT INTO group_profiles (display_name, full_name, short_descr, description, image, user_id, preferences, member_admission, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
        (displayName, fullName, shortDescr, description, image, userId, groupPreferences, memberAdmission, currentTs, currentTs)
      profileId <- insertedRowId db
      DB.execute
        db
        [sql|
          INSERT INTO groups
            (use_relays, local_display_name, user_id, group_profile_id, enable_ntfs,
             created_at, updated_at, chat_ts, user_member_profile_sent_at)
          VALUES (?,?,?,?,?,?,?,?,?)
        |]
        (BI useRelays, ldn, userId, profileId, BI True, currentTs, currentTs, currentTs, currentTs)
      insertedRowId db
    memberId <- liftIO $ encodedRandomBytes gVar 12
    membership <- createContactMemberInv_ db user groupId Nothing user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser customUserProfileId False currentTs vr
    let chatSettings = ChatSettings {enableNtfs = MFAll, sendRcpts = Nothing, favorite = False}
    pure
      GroupInfo
        { groupId,
          useRelays = BoolDef useRelays,
          relayOwnStatus = Nothing,
          localDisplayName = ldn,
          groupProfile,
          localAlias = "",
          businessChat = Nothing,
          fullGroupPreferences,
          membership,
          chatSettings,
          createdAt = currentTs,
          updatedAt = currentTs,
          chatTs = Just currentTs,
          userMemberProfileSentAt = Just currentTs,
          preparedGroup = Nothing,
          chatTags = [],
          chatItemTTL = Nothing,
          uiThemes = Nothing,
          groupSummary = GroupSummary 1,
          customData = Nothing,
          membersRequireAttention = 0,
          viaGroupLinkUri = Nothing
        }

-- | creates a new group record for the group the current user was invited to, or returns an existing one
createGroupInvitation :: DB.Connection -> VersionRangeChat -> User -> Contact -> GroupInvitation -> Maybe ProfileId -> ExceptT StoreError IO (GroupInfo, GroupMemberId)
createGroupInvitation _ _ _ Contact {localDisplayName, activeConn = Nothing} _ _ = throwError $ SEContactNotReady localDisplayName
createGroupInvitation db vr user@User {userId} contact@Contact {contactId, activeConn = Just Connection {peerChatVRange}} GroupInvitation {fromMember, invitedMember, connRequest, groupProfile, business} incognitoProfileId = do
  liftIO getInvitationGroupId_ >>= \case
    Nothing -> createGroupInvitation_
    Just gId -> do
      gInfo@GroupInfo {membership, groupProfile = p'} <- getGroupInfo db vr user gId
      hostId <- getHostMemberId_ db user gId
      let GroupMember {groupMemberId, memberId, memberRole} = membership
          MemberIdRole {memberId = invMemberId, memberRole = invMemberRole} = invitedMember
      liftIO . when (memberId /= invMemberId || memberRole /= invMemberRole) $
        DB.execute db "UPDATE group_members SET member_id = ?, member_role = ? WHERE group_member_id = ?" (invMemberId, invMemberRole, groupMemberId)
      gInfo' <-
        if p' == groupProfile
          then pure gInfo
          else updateGroupProfile db user gInfo groupProfile
      pure (gInfo', hostId)
  where
    getInvitationGroupId_ :: IO (Maybe Int64)
    getInvitationGroupId_ =
      maybeFirstRow fromOnly $
        DB.query db "SELECT group_id FROM groups WHERE inv_queue_info = ? AND user_id = ? LIMIT 1" (connRequest, userId)
    createGroupInvitation_ :: ExceptT StoreError IO (GroupInfo, GroupMemberId)
    createGroupInvitation_ = do
      let GroupProfile {displayName, fullName, shortDescr, description, image, groupPreferences, memberAdmission} = groupProfile
          fullGroupPreferences = mergeGroupPreferences groupPreferences
      ExceptT $
        withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
          currentTs <- liftIO getCurrentTime
          groupId <- liftIO $ do
            DB.execute
              db
              "INSERT INTO group_profiles (display_name, full_name, short_descr, description, image, user_id, preferences, member_admission, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
              (displayName, fullName, shortDescr, description, image, userId, groupPreferences, memberAdmission, currentTs, currentTs)
            profileId <- insertedRowId db
            DB.execute
              db
              [sql|
                INSERT INTO groups
                  (group_profile_id, local_display_name, inv_queue_info, user_id, enable_ntfs,
                   created_at, updated_at, chat_ts, user_member_profile_sent_at, business_chat, business_member_id, customer_member_id)
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
              |]
              ((profileId, localDisplayName, connRequest, userId, BI True, currentTs, currentTs, currentTs, currentTs) :. businessChatInfoRow business)
            insertedRowId db
          let hostVRange = adjustedMemberVRange vr peerChatVRange
          GroupMember {groupMemberId} <- createContactMemberInv_ db user groupId Nothing contact fromMember GCHostMember GSMemInvited IBUnknown Nothing False currentTs hostVRange
          membership <- createContactMemberInv_ db user groupId (Just groupMemberId) user invitedMember GCUserMember GSMemInvited (IBContact contactId) incognitoProfileId False currentTs vr
          let chatSettings = ChatSettings {enableNtfs = MFAll, sendRcpts = Nothing, favorite = False}
          pure
            ( GroupInfo
                { groupId,
                  useRelays = BoolDef False,
                  relayOwnStatus = Nothing,
                  localDisplayName,
                  groupProfile,
                  localAlias = "",
                  businessChat = Nothing,
                  fullGroupPreferences,
                  membership,
                  chatSettings,
                  createdAt = currentTs,
                  updatedAt = currentTs,
                  chatTs = Just currentTs,
                  userMemberProfileSentAt = Just currentTs,
                  preparedGroup = Nothing,
                  chatTags = [],
                  chatItemTTL = Nothing,
                  uiThemes = Nothing,
                  groupSummary = GroupSummary 2,
                  customData = Nothing,
                  membersRequireAttention = 0,
                  viaGroupLinkUri = Nothing
                },
              groupMemberId
            )

businessChatInfoRow :: Maybe BusinessChatInfo -> BusinessChatInfoRow
businessChatInfoRow = \case
  Just BusinessChatInfo {chatType, businessId, customerId} -> (Just chatType, Just businessId, Just customerId)
  Nothing -> (Nothing, Nothing, Nothing)

adjustedMemberVRange :: VersionRangeChat -> VersionRangeChat -> VersionRangeChat
adjustedMemberVRange chatVR vr@(VersionRange minV maxV) =
  let maxV' = min maxV (maxVersion chatVR)
   in fromMaybe vr $ safeVersionRange minV (max minV maxV')

getHostMemberId_ :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO GroupMemberId
getHostMemberId_ db User {userId} groupId =
  ExceptT . firstRow fromOnly (SEHostMemberIdNotFound groupId) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND member_category = ?" (userId, groupId, GCHostMember)

createContactMemberInv_ :: IsContact a => DB.Connection -> User -> GroupId -> Maybe GroupMemberId -> a -> MemberIdRole -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> Maybe ProfileId -> Bool -> UTCTime -> VersionRangeChat -> ExceptT StoreError IO GroupMember
createContactMemberInv_ db User {userId, userContactId} groupId invitedByGroupMemberId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy incognitoProfileId isRelay createdAt vr = do
  incognitoProfile <- forM incognitoProfileId $ \profileId -> getProfileById db userId profileId
  (localDisplayName, memberProfile) <- case (incognitoProfile, incognitoProfileId) of
    (Just profile@LocalProfile {displayName}, Just profileId) ->
      (,profile) <$> insertMemberIncognitoProfile_ displayName profileId
    _ -> (,profile' userOrContact) <$> liftIO insertMember_
  groupMemberId <- liftIO $ insertedRowId db
  pure
    GroupMember
      { groupMemberId,
        groupId,
        memberId,
        memberRole,
        memberCategory,
        memberStatus,
        memberSettings = defaultMemberSettings,
        blockedByAdmin = False,
        invitedBy,
        invitedByGroupMemberId,
        localDisplayName,
        memberProfile,
        memberContactId = Just $ contactId' userOrContact,
        memberContactProfileId = localProfileId (profile' userOrContact),
        activeConn = Nothing,
        memberChatVRange,
        createdAt,
        updatedAt = createdAt,
        supportChat = Nothing,
        isRelay = BoolDef isRelay,
        relayData = Nothing
      }
  where
    memberChatVRange@(VersionRange minV maxV) = vr
    insertMember_ :: IO ContactName
    insertMember_ = do
      let localDisplayName = localDisplayName' userOrContact
      DB.execute
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
              user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
              peer_chat_min_version, peer_chat_max_version, is_relay)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ( (groupId, memberId, memberRole, memberCategory, memberStatus, fromInvitedBy userContactId invitedBy, invitedByGroupMemberId)
            :. (userId, localDisplayName' userOrContact, contactId' userOrContact, localProfileId $ profile' userOrContact, createdAt, createdAt)
            :. (minV, maxV, BI isRelay)
        )
      pure localDisplayName
    insertMemberIncognitoProfile_ :: ContactName -> ProfileId -> ExceptT StoreError IO ContactName
    insertMemberIncognitoProfile_ incognitoDisplayName customUserProfileId = ExceptT $
      withLocalDisplayName db userId incognitoDisplayName $ \incognitoLdn -> do
        DB.execute
          db
          [sql|
            INSERT INTO group_members
              ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
                user_id, local_display_name, contact_id, contact_profile_id, member_profile_id, created_at, updated_at,
                peer_chat_min_version, peer_chat_max_version)
            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (groupId, memberId, memberRole, memberCategory, memberStatus, fromInvitedBy userContactId invitedBy, invitedByGroupMemberId)
              :. (userId, incognitoLdn, contactId' userOrContact, localProfileId $ profile' userOrContact, customUserProfileId, createdAt, createdAt)
              :. (minV, maxV)
          )
        pure $ Right incognitoLdn

deleteContactCardKeepConn :: DB.Connection -> Int64 -> Contact -> IO ()
deleteContactCardKeepConn db connId Contact {contactId, profile = LocalProfile {profileId}} = do
  DB.execute db "UPDATE connections SET contact_id = NULL WHERE connection_id = ?" (Only connId)
  DB.execute db "DELETE FROM contacts WHERE contact_id = ?" (Only contactId)
  DB.execute db "DELETE FROM contact_profiles WHERE contact_profile_id = ?" (Only profileId)

createPreparedGroup :: DB.Connection -> VersionRangeChat -> User -> GroupProfile -> Bool -> CreatedLinkContact -> Maybe SharedMsgId -> Bool -> ExceptT StoreError IO (GroupInfo, GroupMember)
createPreparedGroup db vr user@User {userId, userContactId} groupProfile business connLinkToConnect welcomeSharedMsgId useRelays = do
  currentTs <- liftIO getCurrentTime
  let prepared = Just (connLinkToConnect, welcomeSharedMsgId)
  (groupId, groupLDN) <- createGroup_ db userId groupProfile prepared Nothing useRelays Nothing currentTs
  hostMemberId <- insertHost_ currentTs groupId groupLDN
  let userMember = MemberIdRole (MemberId $ encodeUtf8 groupLDN <> "_user_unknown_id") GRMember
  membership <- createContactMemberInv_ db user groupId (Just hostMemberId) user userMember GCUserMember GSMemUnknown IBUnknown Nothing False currentTs vr
  hostMember <- getGroupMember db vr user groupId hostMemberId
  when business $ liftIO $ setGroupBusinessChatInfo groupId membership hostMember
  g <- getGroupInfo db vr user groupId
  pure (g, hostMember)
  where
    insertHost_ currentTs groupId groupLDN = do
      let memberId = MemberId $ encodeUtf8 groupLDN <> "_host_unknown_id"
          hostProfile = profileFromName $ nameFromMemberId memberId
      (localDisplayName, profileId) <- createNewMemberProfile_ db user hostProfile currentTs
      liftIO $ do
        DB.execute
          db
          [sql|
            INSERT INTO group_members
              ( group_id, member_id, member_role, member_category, member_status, invited_by,
                user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
            VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (groupId, memberId, GRAdmin, GCHostMember, GSMemAccepted, fromInvitedBy userContactId IBUnknown)
              :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, currentTs, currentTs)
          )
        insertedRowId db
    setGroupBusinessChatInfo :: GroupId -> GroupMember -> GroupMember -> IO ()
    setGroupBusinessChatInfo groupId membership hostMember = do
      let businessChatInfo = Just BusinessChatInfo {chatType = BCBusiness, businessId = memberId' hostMember, customerId = memberId' membership}
      updateBusinessChatInfo db groupId businessChatInfo

updateBusinessChatInfo :: DB.Connection -> GroupId -> Maybe BusinessChatInfo -> IO ()
updateBusinessChatInfo db groupId businessChatInfo =
  DB.execute
    db
    [sql|
      UPDATE groups
      SET business_chat = ?,
          business_member_id = ?,
          customer_member_id = ?
      WHERE group_id = ?
    |]
    (businessChatInfoRow businessChatInfo :. (Only groupId))

updatePreparedGroupUser :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> GroupMember -> User -> ExceptT StoreError IO GroupInfo
updatePreparedGroupUser db vr user gInfo@GroupInfo {groupId, membership} hostMember newUser@User {userId = newUserId} = do
  currentTs <- liftIO getCurrentTime
  updateGroup gInfo currentTs
  liftIO $ updateMembership membership currentTs
  updateHostMember hostMember currentTs
  getGroupInfo db vr newUser groupId
  where
    updateGroup GroupInfo {localDisplayName = oldGroupLDN, groupProfile = GroupProfile {displayName = groupDisplayName}} currentTs =
      ExceptT . withLocalDisplayName db newUserId groupDisplayName $ \newGroupLDN -> runExceptT $ do
        liftIO $ do
          DB.execute
            db
            [sql|
              UPDATE groups
              SET user_id = ?, local_display_name = ?, updated_at = ?
              WHERE group_id = ?
            |]
            (newUserId, newGroupLDN, currentTs, groupId)
          DB.execute
            db
            [sql|
              UPDATE group_profiles
              SET user_id = ?, updated_at = ?
              WHERE group_profile_id IN (SELECT group_profile_id FROM groups WHERE group_id = ?)
            |]
            (newUserId, currentTs, groupId)
          safeDeleteLDN db user oldGroupLDN
    updateMembership GroupMember {groupMemberId = membershipId} currentTs =
      DB.execute
        db
        [sql|
          UPDATE group_members
          SET user_id = ?, local_display_name = ?, contact_id = ?, contact_profile_id = ?, updated_at = ?
          WHERE group_member_id = ?
        |]
        (newUserId, localDisplayName' newUser, contactId' newUser, localProfileId $ profile' newUser, currentTs, membershipId)
    updateHostMember
      GroupMember
        { groupMemberId = hostId,
          localDisplayName = oldHostLDN,
          memberProfile = LocalProfile {profileId = hostProfileId, displayName = hostDisplayName}
        }
      currentTs =
        ExceptT . withLocalDisplayName db newUserId hostDisplayName $ \newHostLDN -> runExceptT $ do
          liftIO $ do
            DB.execute
              db
              [sql|
                UPDATE group_members
                SET user_id = ?, local_display_name = ?, updated_at = ?
                WHERE group_member_id = ?
              |]
              (newUserId, newHostLDN, currentTs, hostId)
            DB.execute
              db
              [sql|
                UPDATE contact_profiles
                SET user_id = ?, updated_at = ?
                WHERE contact_profile_id = ?
              |]
              (newUserId, currentTs, hostProfileId)
            safeDeleteLDN db user oldHostLDN

updatePreparedUserAndHostMembersInvited :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> GroupMember -> GroupLinkInvitation -> ExceptT StoreError IO (GroupInfo, GroupMember)
updatePreparedUserAndHostMembersInvited db vr user gInfo hostMember GroupLinkInvitation {fromMember, fromMemberName, invitedMember, groupProfile, accepted, business} = do
  let fromMemberProfile = profileFromName fromMemberName
      initialStatus = maybe GSMemAccepted (acceptanceToStatus $ memberAdmission groupProfile) accepted
  updatePreparedUserAndHostMembers' db vr user gInfo hostMember fromMember fromMemberProfile invitedMember groupProfile business initialStatus

updatePreparedUserAndHostMembersRejected :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> GroupMember -> GroupLinkRejection -> ExceptT StoreError IO (GroupInfo, GroupMember)
updatePreparedUserAndHostMembersRejected db vr user gInfo hostMember GroupLinkRejection {fromMember = fromMember@MemberIdRole {memberId}, invitedMember, groupProfile} = do
  let fromMemberProfile = profileFromName $ nameFromMemberId memberId
  updatePreparedUserAndHostMembers' db vr user gInfo hostMember fromMember fromMemberProfile invitedMember groupProfile Nothing GSMemRejected

updatePreparedUserAndHostMembers' :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> GroupMember -> MemberIdRole -> Profile -> MemberIdRole -> GroupProfile -> Maybe BusinessChatInfo -> GroupMemberStatus -> ExceptT StoreError IO (GroupInfo, GroupMember)
updatePreparedUserAndHostMembers'
  db
  vr
  user
  gInfo@GroupInfo {groupId, groupProfile = gp, businessChat}
  hostMember
  fromMember
  fromMemberProfile
  invitedMember
  groupProfile
  business
  membershipStatus = do
    currentTs <- liftIO getCurrentTime
    liftIO $ updateUserMember currentTs
    hostMember' <- updateHostMember currentTs
    when (gp /= groupProfile) $
      void $ updateGroupProfile db user gInfo groupProfile
    when (isJust businessChat && isJust business) $
      liftIO $ updateBusinessChatInfo db groupId business
    gInfo' <- getGroupInfo db vr user groupId
    pure (gInfo', hostMember')
    where
      updateUserMember currentTs = do
        let GroupInfo {membership} = gInfo
            MemberIdRole memberId memberRole = invitedMember
        DB.execute
          db
          [sql|
            UPDATE group_members
            SET member_id = ?,
                member_role = ?,
                member_status = ?,
                updated_at = ?
            WHERE group_member_id = ?
          |]
          (memberId, memberRole, membershipStatus, currentTs, groupMemberId' membership)
      updateHostMember currentTs = do
        _ <- updateMemberProfile db user hostMember fromMemberProfile
        let MemberIdRole memberId memberRole = fromMember
            gmId = groupMemberId' hostMember
        liftIO $
          DB.execute
            db
            [sql|
              UPDATE group_members
              SET member_id = ?,
                  member_role = ?,
                  updated_at = ?
              WHERE group_member_id = ?
            |]
            (memberId, memberRole, currentTs, gmId)
        getGroupMemberById db vr user gmId

createGroupInvitedViaLink :: DB.Connection -> VersionRangeChat -> User -> Connection -> GroupLinkInvitation -> ExceptT StoreError IO (GroupInfo, GroupMember)
createGroupInvitedViaLink db vr user conn GroupLinkInvitation {fromMember, fromMemberName, invitedMember, groupProfile, accepted, business} = do
  let fromMemberProfile = profileFromName fromMemberName
      initialStatus = maybe GSMemAccepted (acceptanceToStatus $ memberAdmission groupProfile) accepted
  createGroupViaLink' db vr user conn fromMember fromMemberProfile invitedMember groupProfile business initialStatus

createGroupRejectedViaLink :: DB.Connection -> VersionRangeChat -> User -> Connection -> GroupLinkRejection -> ExceptT StoreError IO (GroupInfo, GroupMember)
createGroupRejectedViaLink db vr user conn GroupLinkRejection {fromMember = fromMember@MemberIdRole {memberId}, invitedMember, groupProfile} = do
  let fromMemberProfile = profileFromName $ nameFromMemberId memberId
  createGroupViaLink' db vr user conn fromMember fromMemberProfile invitedMember groupProfile Nothing GSMemRejected

createGroupViaLink' :: DB.Connection -> VersionRangeChat -> User -> Connection -> MemberIdRole -> Profile -> MemberIdRole -> GroupProfile -> Maybe BusinessChatInfo -> GroupMemberStatus -> ExceptT StoreError IO (GroupInfo, GroupMember)
createGroupViaLink'
  db
  vr
  user@User {userId, userContactId}
  Connection {connId, customUserProfileId}
  fromMember
  fromMemberProfile
  invitedMember
  groupProfile
  business
  membershipStatus = do
    currentTs <- liftIO getCurrentTime
    (groupId, _groupLDN) <- createGroup_ db userId groupProfile Nothing business False Nothing currentTs
    hostMemberId <- insertHost_ currentTs groupId
    liftIO $ DB.execute db "UPDATE connections SET conn_type = ?, group_member_id = ?, updated_at = ? WHERE connection_id = ?" (ConnMember, hostMemberId, currentTs, connId)
    -- using IBUnknown since host is created without contact
    void $ createContactMemberInv_ db user groupId (Just hostMemberId) user invitedMember GCUserMember membershipStatus IBUnknown customUserProfileId False currentTs vr
    liftIO $ setViaGroupLinkUri db groupId connId
    (,) <$> getGroupInfo db vr user groupId <*> getGroupMemberById db vr user hostMemberId
    where
      insertHost_ currentTs groupId = do
        (localDisplayName, profileId) <- createNewMemberProfile_ db user fromMemberProfile currentTs
        let MemberIdRole {memberId, memberRole} = fromMember
        liftIO $ do
          DB.execute
            db
            [sql|
              INSERT INTO group_members
                ( group_id, member_id, member_role, member_category, member_status, invited_by,
                  user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
              VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
            |]
            ( (groupId, memberId, memberRole, GCHostMember, GSMemAccepted, fromInvitedBy userContactId IBUnknown)
                :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, currentTs, currentTs)
            )
          insertedRowId db

createGroup_ :: DB.Connection -> UserId -> GroupProfile -> Maybe (CreatedLinkContact, Maybe SharedMsgId) -> Maybe BusinessChatInfo -> Bool -> Maybe RelayStatus -> UTCTime -> ExceptT StoreError IO (GroupId, Text)
createGroup_ db userId groupProfile prepared business useRelays relayOwnStatus currentTs = ExceptT $ do
  let GroupProfile {displayName, fullName, shortDescr, description, image, groupPreferences, memberAdmission} = groupProfile
  withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
    liftIO $ do
      DB.execute
        db
        "INSERT INTO group_profiles (display_name, full_name, short_descr, description, image, user_id, preferences, member_admission, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
        (displayName, fullName, shortDescr, description, image, userId, groupPreferences, memberAdmission, currentTs, currentTs)
      profileId <- insertedRowId db
      DB.execute
        db
        [sql|
          INSERT INTO groups
            (group_profile_id, local_display_name, user_id, enable_ntfs,
              created_at, updated_at, chat_ts, user_member_profile_sent_at, conn_full_link_to_connect, conn_short_link_to_connect, welcome_shared_msg_id,
              business_chat, business_member_id, customer_member_id, use_relays, relay_own_status)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((profileId, localDisplayName, userId, BI True, currentTs, currentTs, currentTs, currentTs) :. toPreparedGroupRow prepared :. businessChatInfoRow business :. (BI useRelays, relayOwnStatus))
      groupId <- insertedRowId db
      pure (groupId, localDisplayName)

setGroupInvitationChatItemId :: DB.Connection -> User -> GroupId -> ChatItemId -> IO ()
setGroupInvitationChatItemId db User {userId} groupId chatItemId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE groups SET chat_item_id = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (chatItemId, currentTs, userId, groupId)

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroup :: DB.Connection -> VersionRangeChat -> User -> GroupId -> ExceptT StoreError IO Group
getGroup db vr user groupId = do
  gInfo <- getGroupInfo db vr user groupId
  members <- liftIO $ getGroupMembers db vr user gInfo
  pure $ Group gInfo members

deleteGroupChatItems :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupChatItems db User {userId} GroupInfo {groupId} =
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

deleteGroupMembers :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupMembers db User {userId} GroupInfo {groupId} = do
  DB.execute_ db "DROP TABLE IF EXISTS temp_delete_members"
#if defined(dbPostgres)
  DB.execute_ db "CREATE TABLE temp_delete_members (contact_profile_id BIGINT, member_profile_id BIGINT, local_display_name TEXT)"
#else
  DB.execute_ db "CREATE TABLE temp_delete_members (contact_profile_id INTEGER, member_profile_id INTEGER, local_display_name TEXT)"
#endif
  DB.execute
    db
    [sql|
      INSERT INTO temp_delete_members (contact_profile_id, member_profile_id, local_display_name)
      SELECT contact_profile_id, member_profile_id, local_display_name FROM group_members WHERE group_id = ?
    |]
    (Only groupId)
  DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_id = ?" (userId, groupId)
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE
        user_id = ?
        AND (contact_profile_id IN (SELECT contact_profile_id FROM temp_delete_members)
          OR contact_profile_id IN (SELECT member_profile_id FROM temp_delete_members WHERE member_profile_id IS NOT NULL))
        AND contact_profile_id NOT IN (SELECT contact_profile_id FROM group_members)
        AND contact_profile_id NOT IN (SELECT member_profile_id FROM group_members)
        AND contact_profile_id NOT IN (SELECT contact_profile_id FROM contacts)
        AND contact_profile_id NOT IN (SELECT contact_profile_id FROM contact_requests)
        AND contact_profile_id NOT IN (SELECT custom_user_profile_id FROM connections)
    |]
    (Only userId)
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE
        user_id = ?
        AND local_display_name IN (SELECT local_display_name FROM temp_delete_members)
        AND local_display_name NOT IN (SELECT local_display_name FROM group_members)
        AND local_display_name NOT IN (SELECT local_display_name FROM contacts)
        AND local_display_name NOT IN (SELECT local_display_name FROM users)
        AND local_display_name NOT IN (SELECT local_display_name FROM groups)
        AND local_display_name NOT IN (SELECT local_display_name FROM user_contact_links)
        AND local_display_name NOT IN (SELECT local_display_name FROM contact_requests)
    |]
    (Only userId)
  DB.execute_ db "DROP TABLE temp_delete_members"

-- to allow repeat connection via the same group link if one was used
cleanupHostGroupLinkConn :: DB.Connection -> User -> GroupInfo -> IO ()
cleanupHostGroupLinkConn db user@User {userId} GroupInfo {groupId} = do
  runExceptT (getHostMemberId_ db user groupId) >>= \case
    Left _ -> pure ()
    Right hostId ->
      DB.execute
        db
        [sql|
          UPDATE connections SET via_contact_uri = NULL, via_contact_uri_hash = NULL, xcontact_id = NULL
          WHERE user_id = ? AND via_group_link = 1 AND contact_id IN (
            SELECT contact_id
            FROM group_members
            WHERE user_id = ? AND group_member_id = ?
          )
        |]
        (userId, userId, hostId)

deleteGroup :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroup db user@User {userId} g@GroupInfo {groupId, localDisplayName} = do
  deleteGroupProfile_ db userId groupId
  DB.execute db "DELETE FROM groups WHERE user_id = ? AND group_id = ?" (userId, groupId)
  safeDeleteLDN db user localDisplayName
  forM_ (incognitoMembershipProfile g) $ deleteUnusedIncognitoProfileById_ db user . localProfileId

deleteGroupProfile_ :: DB.Connection -> UserId -> GroupId -> IO ()
deleteGroupProfile_ db userId groupId =
  DB.execute
    db
    [sql|
      DELETE FROM group_profiles
      WHERE group_profile_id in (
        SELECT group_profile_id
        FROM groups
        WHERE user_id = ? AND group_id = ?
      )
    |]
    (userId, groupId)

getBaseGroupDetails :: DB.Connection -> VersionRangeChat -> User -> Maybe ContactId -> Maybe Text -> IO [GroupInfo]
getBaseGroupDetails db vr User {userId, userContactId} _contactId_ search_ = do
  map (toGroupInfo vr userContactId [])
    <$> DB.query db (groupInfoQuery <> " " <> condition) (userId, userContactId, search, search, search, search)
  where
    condition =
      [sql|
        WHERE g.user_id = ? AND mu.contact_id = ?
          AND (LOWER(gp.display_name) LIKE '%' || ? || '%'
            OR LOWER(gp.full_name) LIKE '%' || ? || '%'
            OR LOWER(gp.short_descr) LIKE '%' || ? || '%'
            OR LOWER(gp.description) LIKE '%' || ? || '%'
          )
      |]
    search = maybe "" (T.map toLower) search_

getContactGroupPreferences :: DB.Connection -> User -> Contact -> IO [(GroupMemberRole, FullGroupPreferences)]
getContactGroupPreferences db User {userId} Contact {contactId} = do
  map (second mergeGroupPreferences)
    <$> DB.query
      db
      [sql|
        SELECT m.member_role, gp.preferences
        FROM groups g
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members m USING (group_id)
        WHERE g.user_id = ? AND m.contact_id = ?
      |]
      (userId, contactId)

getGroupInfoByName :: DB.Connection -> VersionRangeChat -> User -> GroupName -> ExceptT StoreError IO GroupInfo
getGroupInfoByName db vr user gName = do
  gId <- getGroupIdByName db user gName
  getGroupInfo db vr user gId

getGroupMember :: DB.Connection -> VersionRangeChat -> User -> GroupId -> GroupMemberId -> ExceptT StoreError IO GroupMember
getGroupMember db vr user@User {userId} groupId groupMemberId =
  ExceptT . firstRow (toContactMember vr user) (SEGroupMemberNotFound groupMemberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.group_member_id = ? AND m.user_id = ?")
      (groupId, groupMemberId, userId)

getHostMember :: DB.Connection -> VersionRangeChat -> User -> GroupId -> ExceptT StoreError IO GroupMember
getHostMember db vr user groupId =
  ExceptT . firstRow (toContactMember vr user) (SEGroupHostMemberNotFound groupId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.member_category = ?")
      (groupId, GCHostMember)

getMentionedGroupMember :: DB.Connection -> User -> GroupId -> GroupMemberId -> ExceptT StoreError IO CIMention
getMentionedGroupMember db User {userId} groupId gmId =
  ExceptT $
    firstRow toMentionedMember (SEGroupMemberNotFound gmId) $
      DB.query
        db
        (mentionedMemberQuery <> " WHERE m.group_id = ? AND m.group_member_id = ? AND m.user_id = ?")
        (groupId, gmId, userId)

getMentionedMemberByMemberId :: DB.Connection -> User -> GroupId -> MsgMention -> IO CIMention
getMentionedMemberByMemberId db User {userId} groupId MsgMention {memberId} =
  fmap (fromMaybe mentionedMember) $
    maybeFirstRow toMentionedMember $
      DB.query
        db
        (mentionedMemberQuery <> " WHERE m.group_id = ? AND m.member_id = ? AND m.user_id = ?")
        (groupId, memberId, userId)
  where
    mentionedMember = CIMention {memberId, memberRef = Nothing}

mentionedMemberQuery :: Query
mentionedMemberQuery =
  [sql|
    SELECT m.group_member_id, m.member_id, m.member_role, p.display_name, p.local_alias
    FROM group_members m
    JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
  |]

toMentionedMember :: (GroupMemberId, MemberId, GroupMemberRole, Text, Maybe Text) -> CIMention
toMentionedMember (groupMemberId, memberId, memberRole, displayName, localAlias) =
  let memberRef = Just CIMentionMember {groupMemberId, displayName, localAlias, memberRole}
   in CIMention {memberId, memberRef}

getGroupMemberById :: DB.Connection -> VersionRangeChat -> User -> GroupMemberId -> ExceptT StoreError IO GroupMember
getGroupMemberById db vr user@User {userId} groupMemberId =
  ExceptT . firstRow (toContactMember vr user) (SEGroupMemberNotFound groupMemberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_member_id = ? AND m.user_id = ?")
      (groupMemberId, userId)

getGroupMemberByMemberId :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> MemberId -> ExceptT StoreError IO GroupMember
getGroupMemberByMemberId db vr user GroupInfo {groupId} memberId =
  ExceptT . firstRow (toContactMember vr user) (SEGroupMemberNotFoundByMemberId memberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.member_id = ?")
      (groupId, memberId)

getScopeMemberIdViaMemberId :: DB.Connection -> User -> GroupInfo -> GroupMember -> MemberId -> ExceptT StoreError IO GroupMemberId
getScopeMemberIdViaMemberId db user g@GroupInfo {membership} sender scopeMemberId
  | scopeMemberId == memberId' membership = pure $ groupMemberId' membership
  | scopeMemberId == memberId' sender = pure $ groupMemberId' sender
  | otherwise = getGroupMemberIdViaMemberId db user g scopeMemberId

getGroupMemberIdViaMemberId :: DB.Connection -> User -> GroupInfo -> MemberId -> ExceptT StoreError IO GroupMemberId
getGroupMemberIdViaMemberId db User {userId} GroupInfo {groupId} memberId =
  ExceptT . firstRow fromOnly (SEGroupMemberNotFoundByMemberId memberId) $
    DB.query
      db
      "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND member_id = ?"
      (userId, groupId, memberId)

getGroupMembers :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> IO [GroupMember]
getGroupMembers db vr user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember vr user)
    <$> DB.query
      db
      (groupMemberQuery <> " WHERE m.user_id = ? AND m.group_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)")
      (userId, groupId, userContactId)

getGroupModerators :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> IO [GroupMember]
getGroupModerators db vr user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember vr user)
    <$> DB.query
      db
      (groupMemberQuery <> " WHERE m.user_id = ? AND m.group_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?) AND m.member_role IN (?,?,?)")
      (userId, groupId, userContactId, GRModerator, GRAdmin, GROwner)

getGroupRelayMembers :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> IO [GroupMember]
getGroupRelayMembers db vr user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember vr user)
    <$> DB.query
      db
      (groupMemberQuery <> " WHERE m.user_id = ? AND m.group_id = ? AND m.contact_id IS DISTINCT FROM ? AND m.is_relay = 1")
      (userId, groupId, userContactId)

getGroupMembersForExpiration :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> IO [GroupMember]
getGroupMembersForExpiration db vr user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember vr user)
    <$> DB.query
      db
      ( groupMemberQuery
          <> " "
          <> [sql|
                WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)
                  AND m.member_status IN (?, ?, ?, ?)
                  AND m.group_member_id NOT IN (
                    SELECT DISTINCT group_member_id FROM chat_items
                  )
              |]
      )
      (groupId, userId, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted, GSMemUnknown)

getGroupCurrentMembersCount :: DB.Connection -> User -> GroupInfo -> IO Int
getGroupCurrentMembersCount db User {userId} GroupInfo {groupId} = do
  statuses :: [GroupMemberStatus] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT member_status
          FROM group_members
          WHERE group_id = ? AND user_id = ?
        |]
        (groupId, userId)
  pure $ length $ filter memberCurrent' statuses

getGroupInvitation :: DB.Connection -> VersionRangeChat -> User -> GroupId -> ExceptT StoreError IO ReceivedGroupInvitation
getGroupInvitation db vr user groupId =
  getConnRec_ user >>= \case
    Just connRequest -> do
      groupInfo@GroupInfo {membership} <- getGroupInfo db vr user groupId
      when (memberStatus membership /= GSMemInvited) $ throwError SEGroupAlreadyJoined
      hostId <- getHostMemberId_ db user groupId
      fromMember <- getGroupMember db vr user groupId hostId
      pure ReceivedGroupInvitation {fromMember, connRequest, groupInfo}
    _ -> throwError SEGroupInvitationNotFound
  where
    getConnRec_ :: User -> ExceptT StoreError IO (Maybe ConnReqInvitation)
    getConnRec_ User {userId} = ExceptT $ do
      firstRow fromOnly (SEGroupNotFound groupId) $
        DB.query db "SELECT g.inv_queue_info FROM groups g WHERE g.group_id = ? AND g.user_id = ?" (groupId, userId)

createNewContactMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> Contact -> GroupMemberRole -> ConnId -> ConnReqInvitation -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createNewContactMember _ _ _ _ Contact {localDisplayName, activeConn = Nothing} _ _ _ _ = throwError $ SEContactNotReady localDisplayName
createNewContactMember db gVar User {userId, userContactId} GroupInfo {groupId, membership} Contact {contactId, localDisplayName, profile, activeConn = Just Connection {connChatVersion, peerChatVRange}} memberRole agentConnId connRequest subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    member@GroupMember {groupMemberId} <- createMember_ (MemberId memId) createdAt
    void $ createMemberConnection_ db userId groupMemberId agentConnId connChatVersion peerChatVRange Nothing 0 createdAt subMode
    pure member
  where
    VersionRange minV maxV = peerChatVRange
    invitedByGroupMemberId = groupMemberId' membership
    createMember_ memberId createdAt = do
      insertMember_
      groupMemberId <- liftIO $ insertedRowId db
      pure
        GroupMember
          { groupMemberId,
            groupId,
            memberId,
            memberRole,
            memberCategory = GCInviteeMember,
            memberStatus = GSMemInvited,
            memberSettings = defaultMemberSettings,
            blockedByAdmin = False,
            invitedBy = IBUser,
            invitedByGroupMemberId = Just invitedByGroupMemberId,
            localDisplayName,
            memberProfile = profile,
            memberContactId = Just contactId,
            memberContactProfileId = localProfileId profile,
            activeConn = Nothing,
            memberChatVRange = peerChatVRange,
            createdAt,
            updatedAt = createdAt,
            supportChat = Nothing,
            isRelay = BoolDef False,
            relayData = Nothing
          }
      where
        insertMember_ =
          DB.execute
            db
            [sql|
              INSERT INTO group_members
                ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
                  user_id, local_display_name, contact_id, contact_profile_id, sent_inv_queue_info, created_at, updated_at,
                  peer_chat_min_version, peer_chat_max_version)
              VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
            |]
            ( (groupId, memberId, memberRole, GCInviteeMember, GSMemInvited, fromInvitedBy userContactId IBUser, invitedByGroupMemberId)
                :. (userId, localDisplayName, contactId, localProfileId profile, connRequest, createdAt, createdAt)
                :. (minV, maxV)
            )

createGroupRelayRecord :: DB.Connection -> GroupInfo -> UserChatRelay -> ExceptT StoreError IO GroupRelay
createGroupRelayRecord db GroupInfo {groupId} UserChatRelay {chatRelayId} = do
  currentTs <- liftIO getCurrentTime
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO group_relays
          (group_id, chat_relay_id, relay_status, created_at, updated_at)
        VALUES (?,?,?,?,?)
      |]
      (groupId, chatRelayId, RSNew, currentTs, currentTs)
  relayId <- liftIO $ insertedRowId db
  getGroupRelayById db relayId

getGroupRelayById :: DB.Connection -> Int64 -> ExceptT StoreError IO GroupRelay
getGroupRelayById db relayId =
  ExceptT . firstRow toGroupRelay (SEGroupRelayNotFound relayId) $
    DB.query
      db
      (groupRelayQuery <> " WHERE group_relay_id = ?")
      (Only relayId)

getGroupRelays :: DB.Connection -> GroupInfo -> IO [GroupRelay]
getGroupRelays db GroupInfo {groupId} =
  map toGroupRelay
    <$> DB.query
      db
      (groupRelayQuery <> " WHERE group_id = ?")
      (Only groupId)

groupRelayQuery :: Query
groupRelayQuery =
  [sql|
    SELECT group_relay_id, chat_relay_id, relay_status, relay_link
    FROM group_relays
  |]

toGroupRelay :: (Int64, Int64, RelayStatus, Maybe ShortLinkContact) -> GroupRelay
toGroupRelay (groupRelayId, userChatRelayId, relayStatus, relayLink) =
  GroupRelay {groupRelayId, userChatRelayId, relayStatus, relayLink}

-- TODO [relays] TBC role, category, relay profile
-- TODO   - GCInviteeMember -> GCRelayMember?
-- TODO   - GRMember -> GRRelay?
-- TODO   - create 1 profile per relay, link to chat_relays?
-- TODO   - retrieve profile from relay address
createRelayForOwner :: DB.Connection -> VersionRangeChat -> TVar ChaChaDRG -> User -> GroupInfo -> UserChatRelay -> GroupRelay -> ExceptT StoreError IO GroupMember
createRelayForOwner db vr gVar user@User {userId, userContactId} GroupInfo {groupId, membership} UserChatRelay {name} GroupRelay {groupRelayId} = do
  currentTs <- liftIO getCurrentTime
  let relayProfile = profileFromName name
  (localDisplayName, memProfileId) <- createNewMemberProfile_ db user relayProfile currentTs
  groupMemberId <- createWithRandomId gVar $ \memId -> do
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
            user_id, local_display_name, contact_profile_id, created_at, updated_at,
            is_relay, group_relay_id
            )
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, MemberId memId, GRMember, GCInviteeMember, GSMemInvited, fromInvitedBy userContactId IBUser, groupMemberId' membership)
          :. (userId, localDisplayName, memProfileId, currentTs, currentTs)
          :. (BI True, groupRelayId)
      )
    insertedRowId db
  getGroupMemberById db vr user groupMemberId

-- TODO [relays] TBC role, category, relay profile
-- TODO   - TBC retrieve profile from somewhere before connection
-- TODO   - or update upon connection, as in regular prepared groups
-- TODO     (current logic mimics insertHost_ from createPreparedGroup)
createRelayForMember :: DB.Connection -> VersionRangeChat -> TVar ChaChaDRG -> User -> GroupInfo -> ExceptT StoreError IO GroupMember
createRelayForMember db vr gVar user@User {userId, userContactId} GroupInfo {groupId, localDisplayName = groupLDN} = do
  currentTs <- liftIO getCurrentTime
  randRelayId <- liftIO $ encodedRandomBytes gVar 12
  let memberId = MemberId $ encodeUtf8 groupLDN <> "_relay_" <> randRelayId <> "_unknown_id"
      relayProfile = profileFromName $ nameFromMemberId memberId
  (localDisplayName, profileId) <- createNewMemberProfile_ db user relayProfile currentTs
  groupMemberId <- liftIO $ do
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          ( group_id, member_id, member_role, member_category, member_status, invited_by,
            user_id, local_display_name, contact_profile_id, created_at, updated_at,
            is_relay
            )
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, memberId, GRMember, GCHostMember, GSMemAccepted, fromInvitedBy userContactId IBUnknown)
          :. (userId, localDisplayName, profileId, currentTs, currentTs, BI True)
      )
    insertedRowId db
  getGroupMember db vr user groupId groupMemberId

createRelayConnection :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ConnId -> ConnStatus -> VersionChat -> SubscriptionMode -> ExceptT StoreError IO Connection
createRelayConnection db vr user@User {userId} groupMemberId agentConnId connStatus chatV subMode = do
  currentTs <- liftIO getCurrentTime
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO connections (
          user_id, agent_conn_id, conn_level, conn_status, conn_type,
          group_member_id, conn_chat_version, to_subscribe, pq_support, pq_encryption,
          created_at, updated_at
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, agentConnId, 0 :: Int, connStatus, ConnMember)
          :. (groupMemberId, chatV, BI (subMode == SMOnlyCreate), PQSupportOff, PQSupportOff)
          :. (currentTs, currentTs)
      )
  connId <- liftIO $ insertedRowId db
  getConnectionById db vr user connId

updateRelayStatus :: DB.Connection -> GroupRelay -> RelayStatus -> IO GroupRelay
updateRelayStatus db relay@GroupRelay {groupRelayId} relayStatus =
  updateRelayStatus_ db groupRelayId relayStatus $> relay {relayStatus}

updateRelayStatusFromTo :: DB.Connection -> GroupRelay -> RelayStatus -> RelayStatus -> IO GroupRelay
updateRelayStatusFromTo db relay@GroupRelay {groupRelayId} fromStatus toStatus = do
  maybeFirstRow fromOnly (DB.query db "SELECT relay_status FROM group_relays WHERE group_relay_id = ?" (Only groupRelayId)) >>= \case
    Just status | status == fromStatus -> updateRelayStatus_ db groupRelayId toStatus $> relay {relayStatus = toStatus}
    _ -> pure relay

updateRelayStatus_ :: DB.Connection -> Int64 -> RelayStatus -> IO ()
updateRelayStatus_ db relayId relayStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE group_relays SET relay_status = ?, updated_at = ? WHERE group_relay_id = ?" (relayStatus, currentTs, relayId)

setRelayLinkAccepted :: DB.Connection -> GroupRelay -> ShortLinkContact -> IO GroupRelay
setRelayLinkAccepted db relay@GroupRelay {groupRelayId} relayLink = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_relays
      SET relay_link = ?, relay_status = ?, updated_at = ?
      WHERE group_relay_id = ?
    |]
    (relayLink, RSAccepted, currentTs, groupRelayId)
  pure relay {relayStatus = RSAccepted, relayLink = Just relayLink}

createGroupRelayInvitation :: DB.Connection -> VersionRangeChat -> User -> GroupProfile -> GroupRelayInvitation -> ExceptT StoreError IO (GroupInfo, GroupMember)
createGroupRelayInvitation db vr user@User {userId} groupProfile GroupRelayInvitation {fromMember, fromMemberProfile, invitedMember} = do
  currentTs <- liftIO getCurrentTime
  (groupId, _groupLDN) <- createGroup_ db userId groupProfile Nothing Nothing True (Just RSInvited) currentTs
  ownerMemberId <- insertOwner_ currentTs groupId
  _membership <- createContactMemberInv_ db user groupId (Just ownerMemberId) user invitedMember GCUserMember GSMemAccepted IBUnknown Nothing True currentTs vr
  ownerMember <- getGroupMember db vr user groupId ownerMemberId
  g <- getGroupInfo db vr user groupId
  pure (g, ownerMember)
  where
    insertOwner_ currentTs groupId = do
      let MemberIdRole {memberId, memberRole} = fromMember
      (localDisplayName, profileId) <- createNewMemberProfile_ db user fromMemberProfile currentTs
      liftIO $ do
        DB.execute
          db
          [sql|
            INSERT INTO group_members
              ( group_id, member_id, member_role, member_category, member_status,
                user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
            VALUES (?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (groupId, memberId, memberRole, GCHostMember, GSMemAccepted)
              :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, currentTs, currentTs)
          )
        insertedRowId db

updateRelayOwnStatusFromTo :: DB.Connection -> GroupInfo -> RelayStatus -> RelayStatus -> IO GroupInfo
updateRelayOwnStatusFromTo db gInfo@GroupInfo {groupId} fromStatus toStatus = do
  maybeFirstRow fromOnly (DB.query db "SELECT relay_own_status FROM groups WHERE group_id = ?" (Only groupId)) >>= \case
    Just status | status == fromStatus -> updateRelayOwnStatus_ db gInfo toStatus $> gInfo {relayOwnStatus = Just toStatus}
    _ -> pure gInfo

updateRelayOwnStatus_ :: DB.Connection -> GroupInfo -> RelayStatus -> IO ()
updateRelayOwnStatus_ db GroupInfo {groupId} relayStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE groups SET relay_own_status = ?, updated_at = ? WHERE group_id = ?" (relayStatus, currentTs, groupId)

createNewContactMemberAsync :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> Contact -> GroupMemberRole -> (CommandId, ConnId) -> VersionChat -> VersionRangeChat -> SubscriptionMode -> ExceptT StoreError IO ()
createNewContactMemberAsync db gVar user@User {userId, userContactId} GroupInfo {groupId, membership} Contact {contactId, localDisplayName, profile} memberRole (cmdId, agentConnId) chatV peerChatVRange subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    insertMember_ (MemberId memId) createdAt
    groupMemberId <- liftIO $ insertedRowId db
    Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId chatV peerChatVRange Nothing 0 createdAt subMode
    setCommandConnId db user cmdId connId
  where
    VersionRange minV maxV = peerChatVRange
    insertMember_ memberId createdAt =
      DB.execute
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
              user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
              peer_chat_min_version, peer_chat_max_version)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ( (groupId, memberId, memberRole, GCInviteeMember, GSMemInvited, fromInvitedBy userContactId IBUser, groupMemberId' membership)
            :. (userId, localDisplayName, contactId, localProfileId profile, createdAt, createdAt)
            :. (minV, maxV)
        )

createJoiningMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> VersionRangeChat -> Profile -> Maybe XContactId -> Maybe SharedMsgId -> GroupMemberRole -> GroupMemberStatus -> ExceptT StoreError IO (GroupMemberId, MemberId)
createJoiningMember
  db
  gVar
  User {userId, userContactId}
  GroupInfo {groupId, membership}
  cReqChatVRange
  Profile {displayName, fullName, shortDescr, image, contactLink, preferences}
  cReqXContactId_
  welcomeMsgId_
  memberRole
  memberStatus = do
    currentTs <- liftIO getCurrentTime
    ExceptT . withLocalDisplayName db userId displayName $ \ldn -> runExceptT $ do
      liftIO $
        DB.execute
          db
          "INSERT INTO contact_profiles (display_name, full_name, short_descr, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
          (displayName, fullName, shortDescr, image, contactLink, userId, preferences, currentTs, currentTs)
      profileId <- liftIO $ insertedRowId db
      createWithRandomId gVar $ \memId -> do
        insertMember_ ldn profileId (MemberId memId) currentTs
        groupMemberId <- liftIO $ insertedRowId db
        pure (groupMemberId, MemberId memId)
    where
      VersionRange minV maxV = cReqChatVRange
      insertMember_ ldn profileId memberId currentTs =
        DB.execute
          db
          [sql|
            INSERT INTO group_members
              ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
                user_id, local_display_name, contact_id, contact_profile_id, member_xcontact_id, member_welcome_shared_msg_id, created_at, updated_at,
                peer_chat_min_version, peer_chat_max_version)
            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (groupId, memberId, memberRole, GCInviteeMember, memberStatus, fromInvitedBy userContactId IBUser, groupMemberId' membership)
              :. (userId, ldn, Nothing :: (Maybe Int64), profileId, cReqXContactId_, welcomeMsgId_, currentTs, currentTs)
              :. (minV, maxV)
          )

getMemberJoinRequest :: DB.Connection -> User -> GroupInfo -> GroupMember -> IO (Maybe (Maybe XContactId, Maybe SharedMsgId))
getMemberJoinRequest db User {userId} GroupInfo {groupId} GroupMember {groupMemberId = mId} =
  maybeFirstRow id $
    DB.query db "SELECT member_xcontact_id, member_welcome_shared_msg_id FROM group_members WHERE user_id = ? AND group_id = ? AND group_member_id = ?" (userId, groupId, mId)

createJoiningMemberConnection :: DB.Connection -> User -> Int64 -> (CommandId, ConnId) -> VersionChat -> VersionRangeChat -> GroupMemberId -> SubscriptionMode -> IO ()
createJoiningMemberConnection
  db
  user@User {userId}
  uclId
  (cmdId, agentConnId)
  chatV
  cReqChatVRange
  groupMemberId
  subMode = do
    createdAt <- liftIO getCurrentTime
    Connection {connId} <- createConnection_ db userId ConnMember (Just groupMemberId) agentConnId ConnNew chatV cReqChatVRange Nothing (Just uclId) Nothing 0 createdAt subMode PQSupportOff
    setCommandConnId db user cmdId connId

createBusinessRequestGroup :: DB.Connection -> VersionRangeChat -> TVar ChaChaDRG -> User -> VersionRangeChat -> Profile -> Int64 -> Text -> GroupPreferences -> ExceptT StoreError IO (GroupInfo, GroupMember)
createBusinessRequestGroup
  db
  vr
  gVar
  user@User {userId, userContactId}
  cReqChatVRange
  Profile {displayName, fullName, shortDescr, image}
  profileId -- contact request profile id, to be used for member profile
  ldn -- contact request local display name, to be used for group local display name
  groupPreferences = do
    currentTs <- liftIO getCurrentTime
    (groupId, membership@GroupMember {memberId = userMemberId}) <- insertGroup_ currentTs
    (groupMemberId, memberId) <- insertClientMember_ currentTs groupId membership
    liftIO $ DB.execute db "UPDATE groups SET business_member_id = ?, customer_member_id = ? WHERE group_id = ?" (userMemberId, memberId, groupId)
    groupInfo <- getGroupInfo db vr user groupId
    clientMember <- getGroupMemberById db vr user groupMemberId
    pure (groupInfo, clientMember)
    where
      insertGroup_ currentTs = do
        liftIO $
          DB.execute
            db
            "INSERT INTO group_profiles (display_name, full_name, short_descr, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
            (displayName, fullName, shortDescr, image, userId, groupPreferences, currentTs, currentTs)
        groupProfileId <- liftIO $ insertedRowId db
        liftIO $
          DB.execute
            db
            [sql|
              INSERT INTO groups
                (group_profile_id, local_display_name, user_id, enable_ntfs,
                  created_at, updated_at, chat_ts, user_member_profile_sent_at, business_chat)
              VALUES (?,?,?,?,?,?,?,?,?)
            |]
            (groupProfileId, ldn, userId, BI True, currentTs, currentTs, currentTs, currentTs, BCCustomer)
        groupId <- liftIO $ insertedRowId db
        memberId <- liftIO $ encodedRandomBytes gVar 12
        membership <- createContactMemberInv_ db user groupId Nothing user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser Nothing False currentTs vr
        pure (groupId, membership)
      VersionRange minV maxV = cReqChatVRange
      insertClientMember_ currentTs groupId membership = ExceptT $ do
        withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
          createWithRandomId gVar $ \memId -> do
            DB.execute
              db
              [sql|
                INSERT INTO group_members
                  ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
                    user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
                    peer_chat_min_version, peer_chat_max_version)
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
              |]
              ( (groupId, MemberId memId, GRMember, GCInviteeMember, GSMemAccepted, fromInvitedBy userContactId IBUser, groupMemberId' membership)
                  :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, currentTs, currentTs)
                  :. (minV, maxV)
              )
            groupMemberId <- liftIO $ insertedRowId db
            pure (groupMemberId, MemberId memId)

getContactViaMember :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> ExceptT StoreError IO Contact
getContactViaMember db vr user@User {userId} GroupMember {groupMemberId} = do
  contactId <-
    ExceptT $
      firstRow fromOnly (SEContactNotFoundByMemberId groupMemberId) $
        DB.query
          db
          [sql|
            SELECT ct.contact_id
            FROM group_members m
            JOIN contacts ct ON ct.contact_id = m.contact_id
            WHERE m.user_id = ? AND m.group_member_id = ? AND ct.deleted = 0
            LIMIT 1
          |]
          (userId, groupMemberId)
  getContact db vr user contactId

setNewContactMemberConnRequest :: DB.Connection -> User -> GroupMember -> ConnReqInvitation -> IO ()
setNewContactMemberConnRequest db User {userId} GroupMember {groupMemberId} connRequest = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE group_members SET sent_inv_queue_info = ?, updated_at = ? WHERE user_id = ? AND group_member_id = ?" (connRequest, currentTs, userId, groupMemberId)

getMemberInvitation :: DB.Connection -> User -> Int64 -> IO (Maybe ConnReqInvitation)
getMemberInvitation db User {userId} groupMemberId =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT sent_inv_queue_info FROM group_members WHERE group_member_id = ? AND user_id = ?" (groupMemberId, userId)

createMemberConnection :: DB.Connection -> UserId -> GroupMember -> ConnId -> VersionChat -> VersionRangeChat -> SubscriptionMode -> IO Connection
createMemberConnection db userId GroupMember {groupMemberId} agentConnId chatV peerChatVRange subMode = do
  currentTs <- getCurrentTime
  createMemberConnection_ db userId groupMemberId agentConnId chatV peerChatVRange Nothing 0 currentTs subMode

createMemberConnectionAsync :: DB.Connection -> User -> GroupMemberId -> (CommandId, ConnId) -> VersionChat -> VersionRangeChat -> SubscriptionMode -> IO ()
createMemberConnectionAsync db user@User {userId} groupMemberId (cmdId, agentConnId) chatV peerChatVRange subMode = do
  currentTs <- getCurrentTime
  Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId chatV peerChatVRange Nothing 0 currentTs subMode
  setCommandConnId db user cmdId connId

updateGroupMemberStatus :: DB.Connection -> UserId -> GroupMember -> GroupMemberStatus -> IO ()
updateGroupMemberStatus db userId GroupMember {groupMemberId} = updateGroupMemberStatusById db userId groupMemberId

updateGroupMemberStatusById :: DB.Connection -> UserId -> GroupMemberId -> GroupMemberStatus -> IO ()
updateGroupMemberStatusById db userId groupMemberId memStatus = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET member_status = ?, updated_at = ?
      WHERE user_id = ? AND group_member_id = ?
    |]
    (memStatus, currentTs, userId, groupMemberId)

updateGroupMemberAccepted :: DB.Connection -> User -> GroupMember -> GroupMemberStatus -> GroupMemberRole -> IO GroupMember
updateGroupMemberAccepted db User {userId} m@GroupMember {groupMemberId} status role = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET member_status = ?, member_role = ?, updated_at = ?
      WHERE user_id = ? AND group_member_id = ?
    |]
    (status, role, currentTs, userId, groupMemberId)
  pure m {memberStatus = status, memberRole = role, updatedAt = currentTs}

deleteGroupMemberSupportChat :: DB.Connection -> User -> GroupInfo -> GroupMember -> IO (GroupInfo, GroupMember)
deleteGroupMemberSupportChat db user g m@GroupMember {groupMemberId} = do
  let requiredAttention = gmRequiresAttention m
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE group_scope_group_member_id = ?
    |]
    (Only groupMemberId)
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET support_chat_ts = NULL,
          support_chat_items_unread = 0,
          support_chat_items_member_attention = 0,
          support_chat_items_mentions = 0,
          support_chat_last_msg_from_member_ts = NULL,
          updated_at = ?
      WHERE group_member_id = ?
    |]
    (currentTs, groupMemberId)
  let m' = m {supportChat = Nothing, updatedAt = currentTs}
  g' <- if requiredAttention
    then decreaseGroupMembersRequireAttention db user g
    else pure g
  pure (g', m')

updateGroupMembersRequireAttention :: DB.Connection -> User -> GroupInfo -> GroupMember -> GroupMember -> IO GroupInfo
updateGroupMembersRequireAttention db user g member member'
  | nowRequires && not didRequire =
      increaseGroupMembersRequireAttention db user g
  | not nowRequires && didRequire =
      decreaseGroupMembersRequireAttention db user g
  | otherwise = pure g
  where
    didRequire = gmRequiresAttention member
    nowRequires = gmRequiresAttention member'

decreaseGroupMembersRequireAttention :: DB.Connection -> User -> GroupInfo -> IO GroupInfo
decreaseGroupMembersRequireAttention db User {userId} g@GroupInfo {groupId, membersRequireAttention} = do
  DB.execute
    db
#if defined(dbPostgres)
    [sql|
      UPDATE groups
      SET members_require_attention = GREATEST(0, members_require_attention - 1)
      WHERE user_id = ? AND group_id = ?
    |]
#else
    [sql|
      UPDATE groups
      SET members_require_attention = MAX(0, members_require_attention - 1)
      WHERE user_id = ? AND group_id = ?
    |]
#endif
    (userId, groupId)
  pure g {membersRequireAttention = max 0 (membersRequireAttention - 1)}

increaseGroupMembersRequireAttention :: DB.Connection -> User -> GroupInfo -> IO GroupInfo
increaseGroupMembersRequireAttention db User {userId} g@GroupInfo {groupId, membersRequireAttention} = do
  DB.execute
    db
    [sql|
      UPDATE groups
      SET members_require_attention = members_require_attention + 1
      WHERE user_id = ? AND group_id = ?
    |]
    (userId, groupId)
  pure g {membersRequireAttention = membersRequireAttention + 1}

-- | add new member with profile
createNewGroupMember :: DB.Connection -> User -> GroupInfo -> GroupMember -> MemberInfo -> GroupMemberCategory -> GroupMemberStatus -> ExceptT StoreError IO GroupMember
createNewGroupMember db user gInfo invitingMember memInfo@MemberInfo {profile} memCategory memStatus = do
  currentTs <- liftIO getCurrentTime
  (localDisplayName, memProfileId) <- createNewMemberProfile_ db user profile currentTs
  let newMember =
        NewGroupMember
          { memInfo,
            memCategory,
            memStatus,
            memRestriction = Nothing,
            memInvitedBy = IBUnknown,
            memInvitedByGroupMemberId = Just $ groupMemberId' invitingMember,
            localDisplayName,
            memContactId = Nothing,
            memProfileId,
            isRelay = False
          }
  liftIO $ createNewMember_ db user gInfo newMember currentTs

createNewMemberProfile_ :: DB.Connection -> User -> Profile -> UTCTime -> ExceptT StoreError IO (Text, ProfileId)
createNewMemberProfile_ db User {userId} Profile {displayName, fullName, shortDescr, image, contactLink, preferences} createdAt =
  ExceptT . withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, short_descr, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
      (displayName, fullName, shortDescr, image, contactLink, userId, preferences, createdAt, createdAt)
    profileId <- insertedRowId db
    pure $ Right (ldn, profileId)

createNewMember_ :: DB.Connection -> User -> GroupInfo -> NewGroupMember -> UTCTime -> IO GroupMember
createNewMember_
  db
  User {userId, userContactId}
  GroupInfo {groupId}
  NewGroupMember
    { memInfo = MemberInfo memberId memberRole memChatVRange memberProfile,
      memCategory = memberCategory,
      memStatus = memberStatus,
      memRestriction,
      memInvitedBy = invitedBy,
      memInvitedByGroupMemberId,
      localDisplayName,
      memContactId = memberContactId,
      memProfileId = memberContactProfileId,
      isRelay
    }
  createdAt = do
    let invitedById = fromInvitedBy userContactId invitedBy
        activeConn = Nothing
        memberChatVRange@(VersionRange minV maxV) = maybe chatInitialVRange fromChatVRange memChatVRange
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          (group_id, member_id, member_role, member_category, member_status, member_restriction, invited_by, invited_by_group_member_id,
           user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
           peer_chat_min_version, peer_chat_max_version, is_relay)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, memberId, memberRole, memberCategory, memberStatus, memRestriction, invitedById, memInvitedByGroupMemberId)
          :. (userId, localDisplayName, memberContactId, memberContactProfileId, createdAt, createdAt)
          :. (minV, maxV, BI isRelay)
      )
    groupMemberId <- insertedRowId db
    pure
      GroupMember
        { groupMemberId,
          groupId,
          memberId,
          memberRole,
          memberCategory,
          memberStatus,
          memberSettings = defaultMemberSettings,
          blockedByAdmin = maybe False mrsBlocked memRestriction,
          invitedBy,
          invitedByGroupMemberId = memInvitedByGroupMemberId,
          localDisplayName,
          memberProfile = toLocalProfile memberContactProfileId memberProfile "",
          memberContactId,
          memberContactProfileId,
          activeConn,
          memberChatVRange,
          createdAt,
          updatedAt = createdAt,
          supportChat = Nothing,
          isRelay = BoolDef isRelay,
          relayData = Nothing
        }

checkGroupMemberHasItems :: DB.Connection -> User -> GroupMember -> IO (Maybe ChatItemId)
checkGroupMemberHasItems db User {userId} GroupMember {groupMemberId, groupId} =
  maybeFirstRow fromOnly $ DB.query db "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND group_member_id = ? LIMIT 1" (userId, groupId, groupMemberId)

deleteGroupMember :: DB.Connection -> User -> GroupMember -> IO ()
deleteGroupMember db user@User {userId} m@GroupMember {groupMemberId, groupId, memberProfile} = do
  deleteGroupMemberConnection db user m
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ? AND group_member_id = ?" (userId, groupId, groupMemberId)
  DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId)
  cleanupMemberProfileAndName_ db user m
  when (memberIncognito m) $ deleteUnusedIncognitoProfileById_ db user $ localProfileId memberProfile

cleanupMemberProfileAndName_ :: DB.Connection -> User -> GroupMember -> IO ()
cleanupMemberProfileAndName_ db user@User {userId} GroupMember {groupMemberId, memberContactId, memberContactProfileId, localDisplayName} =
  -- check record has no memberContactId (contact_id) - it means contact has been deleted and doesn't use profile & ldn
  when (isNothing memberContactId) $ do
    -- check other group member records don't use profile & ldn
    sameProfileMember :: (Maybe GroupMemberId) <- maybeFirstRow fromOnly $ DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND contact_profile_id = ? AND group_member_id != ? LIMIT 1" (userId, memberContactProfileId, groupMemberId)
    when (isNothing sameProfileMember) $ do
      DB.execute db "DELETE FROM contact_profiles WHERE user_id = ? AND contact_profile_id = ?" (userId, memberContactProfileId)
      safeDeleteLDN db user localDisplayName

deleteGroupMemberConnection :: DB.Connection -> User -> GroupMember -> IO ()
deleteGroupMemberConnection db User {userId} GroupMember {groupMemberId} =
  DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId)

updateGroupMemberRole :: DB.Connection -> User -> GroupMember -> GroupMemberRole -> IO ()
updateGroupMemberRole db User {userId} GroupMember {groupMemberId} memRole =
  DB.execute db "UPDATE group_members SET member_role = ? WHERE user_id = ? AND group_member_id = ?" (memRole, userId, groupMemberId)

createIntroductions :: DB.Connection -> VersionChat -> [GroupMember] -> GroupMember -> IO [GroupMemberIntro]
createIntroductions db chatV members toMember = do
  let reMembers = filter (\m -> memberCurrent m && groupMemberId' m /= groupMemberId' toMember) members
  if null reMembers
    then pure []
    else do
      currentTs <- getCurrentTime
      catMaybes <$> mapM (createIntro_ currentTs) reMembers
  where
    createIntro_ :: UTCTime -> GroupMember -> IO (Maybe GroupMemberIntro)
    createIntro_ ts reMember =
      -- when members connect concurrently, host would try to create introductions between them in both directions;
      -- this check avoids creating second (redundant) introduction
      checkInverseIntro >>= \case
        Just _ -> pure Nothing
        Nothing -> do
          DB.execute
            db
            [sql|
              INSERT INTO group_member_intros
                (re_group_member_id, to_group_member_id, intro_status, intro_chat_protocol_version, created_at, updated_at)
              VALUES (?,?,?,?,?,?)
            |]
            (groupMemberId' reMember, groupMemberId' toMember, GMIntroPending, chatV, ts, ts)
          introId <- insertedRowId db
          pure $ Just GroupMemberIntro {introId, reMember, toMember, introStatus = GMIntroPending}
      where
        checkInverseIntro :: IO (Maybe Int64)
        checkInverseIntro =
          maybeFirstRow fromOnly $
            DB.query
              db
              "SELECT 1 FROM group_member_intros WHERE re_group_member_id = ? AND to_group_member_id = ? LIMIT 1"
              (groupMemberId' toMember, groupMemberId' reMember)

updateIntroStatus :: DB.Connection -> Int64 -> GroupMemberIntroStatus -> IO ()
updateIntroStatus db introId introStatus = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_member_intros
      SET intro_status = ?, updated_at = ?
      WHERE group_member_intro_id = ?
    |]
    (introStatus, currentTs, introId)

getIntroduction :: DB.Connection -> GroupMember -> GroupMember -> ExceptT StoreError IO GroupMemberIntro
getIntroduction db reMember toMember = ExceptT $
  firstRow toIntro SEIntroNotFound $
    DB.query
      db
      [sql|
        SELECT group_member_intro_id, intro_status
        FROM group_member_intros
        WHERE re_group_member_id = ? AND to_group_member_id = ?
      |]
      (groupMemberId' reMember, groupMemberId' toMember)
  where
    toIntro :: (Int64, GroupMemberIntroStatus) -> GroupMemberIntro
    toIntro (introId, introStatus) =
      GroupMemberIntro {introId, reMember, toMember, introStatus}

getIntroducedGroupMemberIds :: DB.Connection -> GroupMember -> IO [GroupMemberId]
getIntroducedGroupMemberIds db invitee =
  map fromOnly <$>
    DB.query
      db
      "SELECT re_group_member_id FROM group_member_intros WHERE to_group_member_id = ?"
      (Only $ groupMemberId' invitee)

getForwardIntroducedMembers :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> Bool -> IO [GroupMember]
getForwardIntroducedMembers db vr user invitee highlyAvailable = do
  memberIds <- map fromOnly <$> query
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    mId = groupMemberId' invitee
    query
      | highlyAvailable = DB.query db q (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected)
      | otherwise =
          DB.query
            db
            (q <> " AND intro_chat_protocol_version >= ?")
            (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, groupForwardVersion)
    q =
      [sql|
        SELECT re_group_member_id
        FROM group_member_intros
        WHERE to_group_member_id = ? AND intro_status NOT IN (?,?,?)
      |]

-- for support scope we don't need to filter by intro_chat_protocol_version for non highly available client,
-- as we will filter moderators supporting this feature by a higher version (as opposed to getForwardIntroducedMembers)
getForwardIntroducedModerators :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> IO [GroupMember]
getForwardIntroducedModerators db vr user@User {userContactId} invitee = do
  memberIds <- map fromOnly <$> query
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    mId = groupMemberId' invitee
    query =
      DB.query
        db
        [sql|
          SELECT i.re_group_member_id
          FROM group_member_intros i
          JOIN group_members m ON m.group_member_id = i.re_group_member_id
          WHERE i.to_group_member_id = ? AND i.intro_status NOT IN (?,?,?)
            AND (m.contact_id IS NULL OR m.contact_id != ?) AND m.member_role IN (?,?,?)
        |]
        (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, userContactId, GRModerator, GRAdmin, GROwner)

getForwardInvitedMembers :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> Bool -> IO [GroupMember]
getForwardInvitedMembers db vr user forwardMember highlyAvailable = do
  memberIds <- map fromOnly <$> query
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    mId = groupMemberId' forwardMember
    query
      | highlyAvailable = DB.query db q (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected)
      | otherwise =
          DB.query
            db
            (q <> " AND intro_chat_protocol_version >= ?")
            (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, groupForwardVersion)
    q =
      [sql|
        SELECT to_group_member_id
        FROM group_member_intros
        WHERE re_group_member_id = ? AND intro_status NOT IN (?,?,?)
      |]

-- for support scope we don't need to filter by intro_chat_protocol_version for non highly available client,
-- as we will filter moderators supporting this feature by a higher version (as opposed to getForwardInvitedMembers)
getForwardInvitedModerators :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> IO [GroupMember]
getForwardInvitedModerators db vr user@User {userContactId} forwardMember = do
  memberIds <- map fromOnly <$> query
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    mId = groupMemberId' forwardMember
    query =
      DB.query
        db
        [sql|
          SELECT i.to_group_member_id
          FROM group_member_intros i
          JOIN group_members m ON m.group_member_id = i.to_group_member_id
          WHERE i.re_group_member_id = ? AND i.intro_status NOT IN (?,?,?)
            AND (m.contact_id IS NULL OR m.contact_id != ?) AND m.member_role IN (?,?,?)
        |]
        (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, userContactId, GRModerator, GRAdmin, GROwner)

getForwardScopeMember :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> GroupMemberId -> IO (Maybe GroupMember)
getForwardScopeMember db vr user GroupMember {groupMemberId = sendingGMId} scopeGMId = do
  (introExists_ :: Maybe Int64) <-
    liftIO $ maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT 1
          FROM group_member_intros
          WHERE
            (
              (re_group_member_id = ? AND to_group_member_id = ?) OR
              (re_group_member_id = ? AND to_group_member_id = ?)
            )
            AND intro_status NOT IN (?,?,?)
          LIMIT 1
        |]
        (sendingGMId, scopeGMId, scopeGMId, sendingGMId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected)
  pure introExists_ $>> (eitherToMaybe <$> runExceptT (getGroupMemberById db vr user scopeGMId))

createIntroReMember :: DB.Connection -> User -> GroupInfo -> MemberInfo -> Maybe MemberRestrictions -> ExceptT StoreError IO GroupMember
createIntroReMember
  db
  user
  gInfo
  memInfo@(MemberInfo _ _ _ memberProfile)
  memRestrictions_ = do
    currentTs <- liftIO getCurrentTime
    (localDisplayName, memProfileId) <- createNewMemberProfile_ db user memberProfile currentTs
    let memRestriction = restriction <$> memRestrictions_
        newMember = NewGroupMember {memInfo, memCategory = GCPreMember, memStatus = GSMemIntroduced, memRestriction, memInvitedBy = IBUnknown, memInvitedByGroupMemberId = Nothing, localDisplayName, memContactId = Nothing, memProfileId, isRelay = False}
    liftIO $ createNewMember_ db user gInfo newMember currentTs

createIntroReMemberConn :: DB.Connection -> User -> GroupMember -> GroupMember -> VersionChat -> MemberInfo -> (CommandId, ConnId) -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createIntroReMemberConn
  db
  user@User {userId}
  _host@GroupMember {memberContactId, activeConn}
  reMember@GroupMember {groupMemberId}
  chatV
  (MemberInfo _ _ memChatVRange _)
  (groupCmdId, groupAgentConnId)
  subMode = do
    let mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
        cLevel = 1 + maybe 0 (\Connection {connLevel} -> connLevel) activeConn
    currentTs <- liftIO getCurrentTime
    conn@Connection {connId = groupConnId} <- liftIO $ createMemberConnection_ db userId groupMemberId groupAgentConnId chatV mcvr memberContactId cLevel currentTs subMode
    liftIO $ setCommandConnId db user groupCmdId groupConnId
    pure (reMember :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: DB.Connection -> User -> GroupMember -> GroupMember -> VersionChat -> VersionRangeChat -> (CommandId, ConnId) -> Maybe (CommandId, ConnId) -> Maybe ProfileId -> SubscriptionMode -> IO ()
createIntroToMemberContact db user@User {userId} GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} chatV mcvr (groupCmdId, groupAgentConnId) directConnIds customUserProfileId subMode = do
  let cLevel = 1 + maybe 0 (\Connection {connLevel} -> connLevel) activeConn
  currentTs <- getCurrentTime
  Connection {connId = groupConnId} <- createMemberConnection_ db userId groupMemberId groupAgentConnId chatV mcvr viaContactId cLevel currentTs subMode
  setCommandConnId db user groupCmdId groupConnId
  forM_ directConnIds $ \(directCmdId, directAgentConnId) -> do
    Connection {connId = directConnId} <- createConnection_ db userId ConnContact Nothing directAgentConnId ConnNew chatV mcvr viaContactId Nothing customUserProfileId cLevel currentTs subMode PQSupportOff
    setCommandConnId db user directCmdId directConnId
    contactId <- createMemberContact_ directConnId currentTs
    updateMember_ contactId currentTs
  where
    createMemberContact_ :: Int64 -> UTCTime -> IO Int64
    createMemberContact_ connId ts = do
      DB.execute
        db
        [sql|
          INSERT INTO contacts (contact_profile_id local_display_name, user_id, created_at, updated_at, chat_ts)
          SELECT contact_profile_id, ?, ?, ?, ?, ?
          FROM group_members
          WHERE group_member_id = ?
        |]
        (localDisplayName, userId, ts, ts, ts, groupMemberId)
      contactId <- insertedRowId db
      DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, ts, connId)
      pure contactId
    updateMember_ :: Int64 -> UTCTime -> IO ()
    updateMember_ contactId ts =
      DB.execute
        db
        [sql|
          UPDATE group_members
          SET contact_id = ?, updated_at = ?
          WHERE group_member_id = ?
        |]
        (contactId, ts, groupMemberId)

createMemberConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> VersionChat -> VersionRangeChat -> Maybe Int64 -> Int -> UTCTime -> SubscriptionMode -> IO Connection
createMemberConnection_ db userId groupMemberId agentConnId chatV peerChatVRange viaContact connLevel currentTs subMode =
  createConnection_ db userId ConnMember (Just groupMemberId) agentConnId ConnNew chatV peerChatVRange viaContact Nothing Nothing connLevel currentTs subMode PQSupportOff

updateGroupProfile :: DB.Connection -> User -> GroupInfo -> GroupProfile -> ExceptT StoreError IO GroupInfo
updateGroupProfile db user@User {userId} g@GroupInfo {groupId, localDisplayName, groupProfile = GroupProfile {displayName}} p'@GroupProfile {displayName = newName, fullName, shortDescr, description, image, groupLink, groupPreferences, memberAdmission}
  | displayName == newName = liftIO $ do
      currentTs <- getCurrentTime
      updateGroupProfile_ currentTs
      pure (g :: GroupInfo) {groupProfile = p', fullGroupPreferences}
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateGroupProfile_ currentTs
        updateGroup_ ldn currentTs
        pure $ Right (g :: GroupInfo) {localDisplayName = ldn, groupProfile = p', fullGroupPreferences}
  where
    fullGroupPreferences = mergeGroupPreferences groupPreferences
    updateGroupProfile_ currentTs =
      DB.execute
        db
        [sql|
          UPDATE group_profiles
          SET display_name = ?, full_name = ?, short_descr = ?, description = ?, image = ?, group_link = ?, preferences = ?, member_admission = ?, updated_at = ?
          WHERE group_profile_id IN (
            SELECT group_profile_id
            FROM groups
            WHERE user_id = ? AND group_id = ?
          )
        |]
        ( (newName, fullName, shortDescr, description, image, groupLink)
          :. (groupPreferences, memberAdmission, currentTs, userId, groupId)
        )
    updateGroup_ ldn currentTs = do
      DB.execute
        db
        "UPDATE groups SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND group_id = ?"
        (ldn, currentTs, userId, groupId)
      safeDeleteLDN db user localDisplayName

updateGroupPreferences :: DB.Connection -> User -> GroupInfo -> GroupPreferences -> IO GroupInfo
updateGroupPreferences db User {userId} g@GroupInfo {groupId, groupProfile = p} ps = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_profiles
      SET preferences = ?, updated_at = ?
      WHERE group_profile_id IN (
        SELECT group_profile_id
        FROM groups
        WHERE user_id = ? AND group_id = ?
      )
    |]
    (ps, currentTs, userId, groupId)
  pure (g :: GroupInfo) {groupProfile = p {groupPreferences = Just ps}, fullGroupPreferences = mergeGroupPreferences $ Just ps}

updateGroupProfileFromMember :: DB.Connection -> User -> GroupInfo -> Profile -> ExceptT StoreError IO GroupInfo
updateGroupProfileFromMember db user g@GroupInfo {groupId} Profile {displayName = n, fullName = fn, shortDescr = sd, image = img} = do
  p <- getGroupProfile -- to avoid any race conditions with UI
  let g' = g {groupProfile = p} :: GroupInfo
      p' = p {displayName = n, fullName = fn, shortDescr = sd, image = img} :: GroupProfile
  updateGroupProfile db user g' p'
  where
    getGroupProfile =
      ExceptT $
        firstRow toGroupProfile (SEGroupNotFound groupId) $
          DB.query
            db
            [sql|
            SELECT gp.display_name, gp.full_name, gp.short_descr, gp.description, gp.image, gp.group_link, gp.preferences, gp.member_admission
            FROM group_profiles gp
            JOIN groups g ON gp.group_profile_id = g.group_profile_id
            WHERE g.group_id = ?
          |]
            (Only groupId)
    toGroupProfile (displayName, fullName, shortDescr, description, image, groupLink, groupPreferences, memberAdmission) =
      GroupProfile {displayName, fullName, shortDescr, description, image, groupLink, groupPreferences, memberAdmission}

getGroupInfo :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO GroupInfo
getGroupInfo db vr User {userId, userContactId} groupId = ExceptT $ do
  chatTags <- getGroupChatTags db groupId
  firstRow (toGroupInfo vr userContactId chatTags) (SEGroupNotFound groupId) $
    DB.query
      db
      (groupInfoQuery <> " WHERE g.group_id = ? AND g.user_id = ? AND mu.contact_id = ?")
      (groupId, userId, userContactId)

getGroupInfoByUserContactLinkConnReq :: DB.Connection -> VersionRangeChat -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe GroupInfo)
getGroupInfoByUserContactLinkConnReq db vr user@User {userId} (cReqSchema1, cReqSchema2) = do
  -- fmap join is to support group_id = NULL if non-group contact request is sent to this function (e.g., if client data is appended).
  groupId_ <-
    fmap join . maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT group_id
          FROM user_contact_links
          WHERE user_id = ? AND conn_req_contact IN (?,?)
        |]
        (userId, cReqSchema1, cReqSchema2)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getGroupInfo db vr user) groupId_

getGroupInfoViaUserShortLink :: DB.Connection -> VersionRangeChat -> User -> ShortLinkContact -> IO (Maybe (ConnReqContact, GroupInfo))
getGroupInfoViaUserShortLink db vr user@User {userId} shortLink = fmap eitherToMaybe $ runExceptT $ do
  (cReq, groupId) <- ExceptT getConnReqGroup
  (cReq,) <$> getGroupInfo db vr user groupId
  where
    getConnReqGroup =
      firstRow' toConnReqGroupId (SEInternalError "group link not found") $
        DB.query
          db
          [sql|
            SELECT conn_req_contact, group_id
            FROM user_contact_links
            WHERE user_id = ? AND short_link_contact = ?
          |]
          (userId, shortLink)
    toConnReqGroupId = \case
      -- cReq is "not null", group_id is nullable
      (cReq, Just groupId) -> Right (cReq, groupId)
      _ -> Left $ SEInternalError "no conn req or group ID"

getGroupViaShortLinkToConnect :: DB.Connection -> VersionRangeChat -> User -> ShortLinkContact -> ExceptT StoreError IO (Maybe (ConnReqContact, GroupInfo))
getGroupViaShortLinkToConnect db vr user@User {userId} shortLink =
  liftIO (maybeFirstRow id $ DB.query db "SELECT group_id, conn_full_link_to_connect FROM groups WHERE user_id = ? AND conn_short_link_to_connect = ?" (userId, shortLink)) >>= \case
    Just (gId :: Int64, Just cReq) -> Just . (cReq,) <$> getGroupInfo db vr user gId
    _ -> pure Nothing

getGroupInfoByGroupLinkHash :: DB.Connection -> VersionRangeChat -> User -> (ConnReqUriHash, ConnReqUriHash) -> IO (Maybe GroupInfo)
getGroupInfoByGroupLinkHash db vr user@User {userId, userContactId} (groupLinkHash1, groupLinkHash2) = do
  groupId_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT g.group_id
          FROM groups g
          JOIN group_members mu ON mu.group_id = g.group_id
          WHERE g.user_id = ? AND g.via_group_link_uri_hash IN (?,?)
            AND mu.contact_id = ? AND mu.member_status NOT IN (?,?,?,?)
          LIMIT 1
        |]
        (userId, groupLinkHash1, groupLinkHash2, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted, GSMemUnknown)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getGroupInfo db vr user) groupId_

getGroupIdByName :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO GroupId
getGroupIdByName db User {userId} gName =
  ExceptT . firstRow fromOnly (SEGroupNotFoundByName gName) $
    DB.query db "SELECT group_id FROM groups WHERE user_id = ? AND local_display_name = ?" (userId, gName)

getGroupMemberIdByName :: DB.Connection -> User -> GroupId -> ContactName -> ExceptT StoreError IO GroupMemberId
getGroupMemberIdByName db User {userId} groupId groupMemberName =
  ExceptT . firstRow fromOnly (SEGroupMemberNameNotFound groupId groupMemberName) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND local_display_name = ?" (userId, groupId, groupMemberName)

getActiveMembersByName :: DB.Connection -> VersionRangeChat -> User -> ContactName -> ExceptT StoreError IO [(GroupInfo, GroupMember)]
getActiveMembersByName db vr user@User {userId} groupMemberName = do
  groupMemberIds :: [(GroupId, GroupMemberId)] <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT group_id, group_member_id
          FROM group_members
          WHERE user_id = ? AND local_display_name = ?
            AND member_status IN (?,?) AND member_category != ?
        |]
        (userId, groupMemberName, GSMemConnected, GSMemComplete, GCUserMember)
  possibleMembers <- forM groupMemberIds $ \(groupId, groupMemberId) -> do
    groupInfo <- getGroupInfo db vr user groupId
    groupMember <- getGroupMember db vr user groupId groupMemberId
    pure (groupInfo, groupMember)
  pure $ sortOn (Down . ts . fst) possibleMembers
  where
    ts GroupInfo {chatTs, updatedAt} = fromMaybe updatedAt chatTs

getMatchingContacts :: DB.Connection -> VersionRangeChat -> User -> Contact -> IO [Contact]
getMatchingContacts db vr user@User {userId} Contact {contactId, profile = LocalProfile {displayName, fullName, shortDescr, image}} = do
  contactIds <- map fromOnly <$> DB.query db q (userId, contactId, CSActive, displayName, fullName, shortDescr, image)
  rights <$> mapM (runExceptT . getContact db vr user) contactIds
  where
    -- this query is different from one in getMatchingMemberContacts
    -- it checks that it's not the same contact
    q =
      [sql|
        SELECT ct.contact_id
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        WHERE ct.user_id = ? AND ct.contact_id != ?
          AND ct.contact_status = ? AND ct.deleted = 0 AND ct.is_user = 0
          AND p.display_name = ? AND p.full_name = ?
          AND p.short_descr IS NOT DISTINCT FROM ? AND p.image IS NOT DISTINCT FROM ?
      |]

getMatchingMembers :: DB.Connection -> VersionRangeChat -> User -> Contact -> IO [GroupMember]
getMatchingMembers db vr user@User {userId} Contact {profile = LocalProfile {displayName, fullName, shortDescr, image}} = do
  memberIds <- map fromOnly <$> DB.query db q (userId, GCUserMember, displayName, fullName, shortDescr, image)
  filter memberCurrent . rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    -- only match with members without associated contact
    q =
      [sql|
        SELECT m.group_member_id
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        WHERE m.user_id = ? AND m.contact_id IS NULL
          AND m.member_category != ?
          AND p.display_name = ? AND p.full_name = ?
          AND p.short_descr IS NOT DISTINCT FROM ? AND p.image IS NOT DISTINCT FROM ?
      |]

getMatchingMemberContacts :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> IO [Contact]
getMatchingMemberContacts _ _ _ GroupMember {memberContactId = Just _} = pure []
getMatchingMemberContacts db vr user@User {userId} GroupMember {memberProfile = LocalProfile {displayName, fullName, shortDescr, image}} = do
  contactIds <- map fromOnly <$> DB.query db q (userId, CSActive, displayName, fullName, shortDescr, image)
  rights <$> mapM (runExceptT . getContact db vr user) contactIds
  where
    q =
      [sql|
        SELECT ct.contact_id
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        WHERE ct.user_id = ?
          AND ct.contact_status = ? AND ct.deleted = 0 AND ct.is_user = 0
          AND p.display_name = ? AND p.full_name = ?
          AND p.short_descr IS NOT DISTINCT FROM ? AND p.image IS NOT DISTINCT FROM ?
      |]

createSentProbe :: DB.Connection -> TVar ChaChaDRG -> UserId -> ContactOrMember -> ExceptT StoreError IO (Probe, Int64)
createSentProbe db gVar userId to =
  createWithRandomBytes 32 gVar $ \probe -> do
    currentTs <- getCurrentTime
    let (ctId, gmId) = contactOrMemberIds to
    DB.execute
      db
      "INSERT INTO sent_probes (contact_id, group_member_id, probe, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (ctId, gmId, Binary probe, userId, currentTs, currentTs)
    (Probe probe,) <$> insertedRowId db

createSentProbeHash :: DB.Connection -> UserId -> Int64 -> ContactOrMember -> IO ()
createSentProbeHash db userId probeId to = do
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds to
  DB.execute
    db
    "INSERT INTO sent_probe_hashes (sent_probe_id, contact_id, group_member_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (probeId, ctId, gmId, userId, currentTs, currentTs)

matchReceivedProbe :: DB.Connection -> VersionRangeChat -> User -> ContactOrMember -> Probe -> IO [ContactOrMember]
matchReceivedProbe db vr user@User {userId} from (Probe probe) = do
  let probeHash = C.sha256Hash probe
  cgmIds <-
    DB.query
      db
      [sql|
        SELECT r.contact_id, g.group_id, r.group_member_id
        FROM received_probes r
        LEFT JOIN contacts c ON r.contact_id = c.contact_id AND c.deleted = 0
        LEFT JOIN group_members m ON r.group_member_id = m.group_member_id
        LEFT JOIN groups g ON g.group_id = m.group_id
        WHERE r.user_id = ? AND r.probe_hash = ? AND r.probe IS NULL
      |]
      (userId, Binary probeHash)
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds from
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, group_member_id, probe, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (ctId, gmId, Binary probe, Binary probeHash, userId, currentTs, currentTs)
  let cgmIds' = filterFirstContactId cgmIds
  catMaybes <$> mapM (getContactOrMember_ db vr user) cgmIds'
  where
    filterFirstContactId :: [(Maybe ContactId, Maybe GroupId, Maybe GroupMemberId)] -> [(Maybe ContactId, Maybe GroupId, Maybe GroupMemberId)]
    filterFirstContactId cgmIds = do
      let (ctIds, memIds) = partition (\(ctId, _, _) -> isJust ctId) cgmIds
          ctIds' = case ctIds of
            [] -> []
            (x : _) -> [x]
      ctIds' <> memIds

matchReceivedProbeHash :: DB.Connection -> VersionRangeChat -> User -> ContactOrMember -> ProbeHash -> IO (Maybe (ContactOrMember, Probe))
matchReceivedProbeHash db vr user@User {userId} from (ProbeHash probeHash) = do
  probeIds <-
    maybeFirstRow id $
      DB.query
        db
        [sql|
          SELECT r.probe, r.contact_id, g.group_id, r.group_member_id
          FROM received_probes r
          LEFT JOIN contacts c ON r.contact_id = c.contact_id AND c.deleted = 0
          LEFT JOIN group_members m ON r.group_member_id = m.group_member_id
          LEFT JOIN groups g ON g.group_id = m.group_id
          WHERE r.user_id = ? AND r.probe_hash = ? AND r.probe IS NOT NULL
        |]
        (userId, Binary probeHash)
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds from
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, group_member_id, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (ctId, gmId, Binary probeHash, userId, currentTs, currentTs)
  pure probeIds $>>= \(Only probe :. cgmIds) -> (,Probe probe) <$$> getContactOrMember_ db vr user cgmIds

matchSentProbe :: DB.Connection -> VersionRangeChat -> User -> ContactOrMember -> Probe -> IO (Maybe ContactOrMember)
matchSentProbe db vr user@User {userId} _from (Probe probe) = do
  cgmIds $>>= getContactOrMember_ db vr user
  where
    (ctId, gmId) = contactOrMemberIds _from
    cgmIds =
      maybeFirstRow id $
        DB.query
          db
          [sql|
            SELECT s.contact_id, g.group_id, s.group_member_id
            FROM sent_probes s
            LEFT JOIN contacts c ON s.contact_id = c.contact_id AND c.deleted = 0
            LEFT JOIN group_members m ON s.group_member_id = m.group_member_id
            LEFT JOIN groups g ON g.group_id = m.group_id
            JOIN sent_probe_hashes h ON h.sent_probe_id = s.sent_probe_id
            WHERE s.user_id = ? AND s.probe = ?
              AND (h.contact_id = ? OR h.group_member_id = ?)
          |]
          (userId, Binary probe, ctId, gmId)

getContactOrMember_ :: DB.Connection -> VersionRangeChat -> User -> (Maybe ContactId, Maybe GroupId, Maybe GroupMemberId) -> IO (Maybe ContactOrMember)
getContactOrMember_ db vr user ids =
  fmap eitherToMaybe . runExceptT $ case ids of
    (Just ctId, _, _) -> COMContact <$> getContact db vr user ctId
    (_, Just gId, Just gmId) -> COMGroupMember <$> getGroupMember db vr user gId gmId
    _ -> throwError $ SEInternalError ""

associateMemberWithContactRecord :: DB.Connection -> User -> Contact -> GroupMember -> IO ()
associateMemberWithContactRecord
  db
  User {userId}
  Contact {contactId, localDisplayName, profile = LocalProfile {profileId}}
  GroupMember {groupId, groupMemberId, localDisplayName = memLDN, memberProfile = LocalProfile {profileId = memProfileId}} = do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        UPDATE group_members
        SET contact_id = ?, local_display_name = ?, contact_profile_id = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND group_member_id = ?
      |]
      (contactId, localDisplayName, profileId, currentTs, userId, groupId, groupMemberId)
    when (memProfileId /= profileId) $ deleteUnusedProfile_ db userId memProfileId
    when (memLDN /= localDisplayName) $ deleteUnusedDisplayName_ db userId memLDN

associateContactWithMemberRecord :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> Contact -> ExceptT StoreError IO Contact
associateContactWithMemberRecord
  db
  vr
  user@User {userId}
  GroupMember {groupId, groupMemberId, localDisplayName = memLDN, memberProfile = LocalProfile {profileId = memProfileId}}
  Contact {contactId, localDisplayName, profile = LocalProfile {profileId}} = do
    liftIO $ do
      currentTs <- getCurrentTime
      DB.execute
        db
        [sql|
          UPDATE group_members
          SET contact_id = ?, updated_at = ?
          WHERE user_id = ? AND group_id = ? AND group_member_id = ?
        |]
        (contactId, currentTs, userId, groupId, groupMemberId)
      DB.execute
        db
        [sql|
          UPDATE contacts
          SET local_display_name = ?, contact_profile_id = ?, updated_at = ?
          WHERE user_id = ? AND contact_id = ?
        |]
        (memLDN, memProfileId, currentTs, userId, contactId)
      when (profileId /= memProfileId) $ deleteUnusedProfile_ db userId profileId
      when (localDisplayName /= memLDN) $ deleteUnusedDisplayName_ db userId localDisplayName
    getContact db vr user contactId

deleteUnusedDisplayName_ :: DB.Connection -> UserId -> ContactName -> IO ()
deleteUnusedDisplayName_ db userId localDisplayName =
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ? AND local_display_name = ?
        AND 1 NOT IN (
          SELECT 1 FROM users
          WHERE local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contacts
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM groups
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM user_contact_links
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = ? AND local_display_name = ? LIMIT 1
        )
    |]
    ( (userId, localDisplayName, localDisplayName, userId, localDisplayName, userId, localDisplayName)
        :. (userId, localDisplayName, userId, localDisplayName, userId, localDisplayName)
        :. (userId, localDisplayName)
    )

deleteOldProbes :: DB.Connection -> UTCTime -> IO ()
deleteOldProbes db createdAtCutoff = do
  DB.execute db "DELETE FROM sent_probes WHERE created_at <= ?" (Only createdAtCutoff)
  DB.execute db "DELETE FROM sent_probe_hashes WHERE created_at <= ?" (Only createdAtCutoff)
  DB.execute db "DELETE FROM received_probes WHERE created_at <= ?" (Only createdAtCutoff)

updateGroupSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateGroupSettings db User {userId} groupId ChatSettings {enableNtfs, sendRcpts, favorite} =
  DB.execute db "UPDATE groups SET enable_ntfs = ?, send_rcpts = ?, favorite = ? WHERE user_id = ? AND group_id = ?" (enableNtfs, BI <$> sendRcpts, BI favorite, userId, groupId)

updateGroupMemberSettings :: DB.Connection -> User -> GroupId -> GroupMemberId -> GroupMemberSettings -> IO ()
updateGroupMemberSettings db User {userId} gId gMemberId GroupMemberSettings {showMessages} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET show_messages = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND group_member_id = ?
    |]
    (BI showMessages, currentTs, userId, gId, gMemberId)

updateGroupMemberBlocked :: DB.Connection -> User -> GroupInfo -> MemberRestrictionStatus -> GroupMember -> IO GroupMember
updateGroupMemberBlocked db User {userId} GroupInfo {groupId} mrs m@GroupMember {groupMemberId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET member_restriction = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND group_member_id = ?
    |]
    (mrs, currentTs, userId, groupId, groupMemberId)
  pure m {blockedByAdmin = mrsBlocked mrs}

getHostConnId :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO GroupMemberId
getHostConnId db user@User {userId} groupId = do
  hostMemberId <- getHostMemberId_ db user groupId
  ExceptT . firstRow fromOnly (SEConnectionNotFoundByMemberId hostMemberId) $
    DB.query db "SELECT connection_id FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, hostMemberId)

createMemberContact :: DB.Connection -> User -> ConnId -> ConnReqInvitation -> GroupInfo -> GroupMember -> Connection -> SubscriptionMode -> IO Contact
createMemberContact
  db
  user@User {userId, profile = LocalProfile {preferences}}
  acId
  cReq
  gInfo
  GroupMember {groupMemberId, localDisplayName, memberProfile, memberContactProfileId}
  Connection {connLevel, connChatVersion, peerChatVRange = peerChatVRange@(VersionRange minV maxV)}
  subMode = do
    currentTs <- getCurrentTime
    let incognitoProfile = incognitoMembershipProfile gInfo
        customUserProfileId = localProfileId <$> incognitoProfile
        userPreferences = fromMaybe emptyChatPrefs $ incognitoProfile >> preferences
    DB.execute
      db
      [sql|
        INSERT INTO contacts (
          user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, contact_used,
          contact_group_member_id, contact_grp_inv_sent, created_at, updated_at, chat_ts
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, localDisplayName, memberContactProfileId, BI True, userPreferences, BI True)
          :. (groupMemberId, BI False, currentTs, currentTs, currentTs)
      )
    contactId <- insertedRowId db
    DB.execute
      db
      "UPDATE group_members SET contact_id = ?, updated_at = ? WHERE contact_profile_id = ?"
      (contactId, currentTs, memberContactProfileId)
    DB.execute -- why do we insert conn_req_inv here? how is it used?
      db
      [sql|
        INSERT INTO connections (
          user_id, agent_conn_id, conn_req_inv, conn_level, conn_status, conn_type, contact_conn_initiated, contact_id, custom_user_profile_id,
          conn_chat_version, peer_chat_min_version, peer_chat_max_version, created_at, updated_at, to_subscribe
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, acId, cReq, connLevel, ConnNew, ConnContact, BI True, contactId, customUserProfileId)
          :. (connChatVersion, minV, maxV, currentTs, currentTs, BI (subMode == SMOnlyCreate))
      )
    connId <- insertedRowId db
    let ctConn =
          Connection
            { connId,
              agentConnId = AgentConnId acId,
              peerChatVRange,
              connChatVersion,
              connType = ConnContact,
              contactConnInitiated = True,
              entityId = Just contactId,
              viaContact = Nothing,
              viaUserContactLink = Nothing,
              viaGroupLink = False,
              groupLinkId = Nothing,
              xContactId = Nothing,
              customUserProfileId,
              connLevel,
              connStatus = ConnNew,
              localAlias = "",
              createdAt = currentTs,
              connectionCode = Nothing,
              pqSupport = PQSupportOff,
              pqEncryption = PQEncOff,
              pqSndEnabled = Nothing,
              pqRcvEnabled = Nothing,
              authErrCounter = 0,
              quotaErrCounter = 0
            }
        mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito ctConn
    pure Contact {contactId, localDisplayName, profile = memberProfile, activeConn = Just ctConn, contactUsed = True, contactStatus = CSActive, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs, preparedContact = Nothing, contactRequestId = Nothing, contactGroupMemberId = Just groupMemberId, contactGrpInvSent = False, groupDirectInv = Nothing, chatTags = [], chatItemTTL = Nothing, uiThemes = Nothing, chatDeleted = False, customData = Nothing}

getMemberContact :: DB.Connection -> VersionRangeChat -> User -> ContactId -> ExceptT StoreError IO (GroupInfo, GroupMember, Contact, ConnReqInvitation)
getMemberContact db vr user contactId = do
  ct <- getContact db vr user contactId
  let Contact {contactGroupMemberId, activeConn} = ct
  case (activeConn, contactGroupMemberId) of
    (Just Connection {connId}, Just groupMemberId) -> do
      cReq <- getConnReqInv db connId
      m@GroupMember {groupId} <- getGroupMemberById db vr user groupMemberId
      g <- getGroupInfo db vr user groupId
      pure (g, m, ct, cReq)
    _ ->
      throwError $ SEMemberContactGroupMemberNotFound contactId

setContactGrpInvSent :: DB.Connection -> Contact -> Bool -> IO ()
setContactGrpInvSent db Contact {contactId} xGrpDirectInvSent = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE contacts SET contact_grp_inv_sent = ?, updated_at = ? WHERE contact_id = ?"
    (BI xGrpDirectInvSent, currentTs, contactId)

createMemberContactInvited :: DB.Connection -> User -> GroupInfo -> GroupMember -> GroupDirectInvitation -> IO (ContactId, GroupMember)
createMemberContactInvited
  db
  User {userId, profile = LocalProfile {preferences}}
  gInfo
  m@GroupMember {localDisplayName = memberLDN, memberContactProfileId}
  GroupDirectInvitation {groupDirectInvLink, fromGroupId_, fromGroupMemberId_, fromGroupMemberConnId_, groupDirectInvStartedConnection} = do
    currentTs <- liftIO getCurrentTime
    let userPreferences = fromMaybe emptyChatPrefs $ incognitoMembershipProfile gInfo >> preferences
    contactId <- createContactUpdateMember currentTs userPreferences
    pure (contactId, m {memberContactId = Just contactId})
    where
      createContactUpdateMember :: UTCTime -> Preferences -> IO ContactId
      createContactUpdateMember currentTs userPreferences = do
        DB.execute
          db
          [sql|
            INSERT INTO contacts (
              user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, contact_used,
              grp_direct_inv_link, grp_direct_inv_from_group_id, grp_direct_inv_from_group_member_id, grp_direct_inv_from_member_conn_id, grp_direct_inv_started_connection,
              created_at, updated_at, chat_ts
            ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (userId, memberLDN, memberContactProfileId, BI True, userPreferences, BI True)
              :. (groupDirectInvLink, fromGroupId_, fromGroupMemberId_, fromGroupMemberConnId_, BI groupDirectInvStartedConnection)
              :. (currentTs, currentTs, currentTs)
          )
        contactId <- insertedRowId db
        DB.execute
          db
          "UPDATE group_members SET contact_id = ?, updated_at = ? WHERE contact_profile_id = ?"
          (contactId, currentTs, memberContactProfileId)
        pure contactId

updateMemberContactInvited :: DB.Connection -> User -> Contact -> GroupDirectInvitation -> ExceptT StoreError IO ()
updateMemberContactInvited _ _ Contact {localDisplayName, activeConn = Nothing} _ = throwError $ SEContactNotReady localDisplayName
updateMemberContactInvited db user Contact {contactId, activeConn = Just oldContactConn} groupDirectInv = liftIO $ do
  deleteConnectionRecord db user (dbConnId oldContactConn)
  updateMemberContactFields groupDirectInv
  where
    -- - reset status to active (in case contact was deleted)
    -- - reset fields used for sending invitation
    -- - set fields used for accepting invitation
    updateMemberContactFields GroupDirectInvitation {groupDirectInvLink, fromGroupId_, fromGroupMemberId_, fromGroupMemberConnId_, groupDirectInvStartedConnection} =
      DB.execute
        db
        [sql|
          UPDATE contacts
          SET contact_status = ?,
              contact_group_member_id = NULL, contact_grp_inv_sent = 0,
              grp_direct_inv_link = ?, grp_direct_inv_from_group_id = ?, grp_direct_inv_from_group_member_id = ?, grp_direct_inv_from_member_conn_id = ?, grp_direct_inv_started_connection = ?
          WHERE contact_id = ?
        |]
        (CSActive, groupDirectInvLink, fromGroupId_, fromGroupMemberId_, fromGroupMemberConnId_, BI groupDirectInvStartedConnection, contactId)

resetMemberContactFields :: DB.Connection -> Contact -> IO Contact
resetMemberContactFields db ct@Contact {contactId} = do
  currentTs <- liftIO getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contacts
      SET contact_group_member_id = NULL, contact_grp_inv_sent = 0, updated_at = ?
      WHERE contact_id = ?
    |]
    (currentTs, contactId)
  pure ct {contactGroupMemberId = Nothing, contactGrpInvSent = False, updatedAt = currentTs}

createMemberContactConn :: DB.Connection -> User -> ConnId -> Maybe CommandId -> GroupInfo -> Connection -> ConnStatus -> ContactId -> SubscriptionMode -> IO Int64
createMemberContactConn
  db
  user@User {userId}
  acId
  cmdId_
  gInfo
  _memberConn@Connection {connLevel, connChatVersion, peerChatVRange = VersionRange minV maxV}
  connStatus
  contactId
  subMode = do
    currentTs <- liftIO getCurrentTime
    let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
    DB.execute
      db
      [sql|
        INSERT INTO connections (
          user_id, agent_conn_id, conn_level, conn_status, conn_type, contact_id, custom_user_profile_id,
          conn_chat_version, peer_chat_min_version, peer_chat_max_version, created_at, updated_at, to_subscribe
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, acId, connLevel, connStatus, ConnContact, contactId, customUserProfileId)
          :. (connChatVersion, minV, maxV, currentTs, currentTs, BI (subMode == SMOnlyCreate))
      )
    connId <- insertedRowId db
    forM_ cmdId_ $ \cmdId -> setCommandConnId db user cmdId connId
    pure connId

getMemberContactInvited :: DB.Connection -> VersionRangeChat -> User -> ContactId -> ExceptT StoreError IO (GroupInfo, Connection, Contact, GroupDirectInvitation)
getMemberContactInvited db vr user contactId = do
  ct@Contact {groupDirectInv = groupDirectInv_} <- getContact db vr user contactId
  case groupDirectInv_ of
    Just groupDirectInv@GroupDirectInvitation {fromGroupId_ = Just groupId, fromGroupMemberId_ = Just _gmId, fromGroupMemberConnId_ = Just mConnId} -> do
      g <- getGroupInfo db vr user groupId
      mConn <- getConnectionById db vr user mConnId
      pure (g, mConn, ct, groupDirectInv)
    _ ->
      throwError $ SEMemberContactGroupMemberNotFound contactId

setMemberContactStartedConnection :: DB.Connection -> Contact -> IO ()
setMemberContactStartedConnection db Contact {contactId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE contacts SET grp_direct_inv_started_connection = ?, updated_at = ? WHERE contact_id = ?"
    (BI True, currentTs, contactId)

updateMemberProfile :: DB.Connection -> User -> GroupMember -> Profile -> ExceptT StoreError IO GroupMember
updateMemberProfile db user@User {userId} m p'
  | displayName == newName = do
      liftIO $ updateMemberContactProfileReset_ db userId profileId p'
      pure m {memberProfile = profile}
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateMemberContactProfileReset_' db userId profileId p' currentTs
        DB.execute
          db
          "UPDATE group_members SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND group_member_id = ?"
          (ldn, currentTs, userId, groupMemberId)
        safeDeleteLDN db user localDisplayName
        pure $ Right m {localDisplayName = ldn, memberProfile = profile}
  where
    GroupMember {groupMemberId, localDisplayName, memberProfile = LocalProfile {profileId, displayName, localAlias}} = m
    Profile {displayName = newName} = p'
    profile = toLocalProfile profileId p' localAlias

updateContactMemberProfile :: DB.Connection -> User -> GroupMember -> Contact -> Profile -> ExceptT StoreError IO (GroupMember, Contact)
updateContactMemberProfile db user@User {userId} m ct@Contact {contactId} p'
  | displayName == newName = do
      liftIO $ updateMemberContactProfile_ db userId profileId p'
      pure (m {memberProfile = profile}, ct {profile} :: Contact)
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateMemberContactProfile_' db userId profileId p' currentTs
        updateContactLDN_ db user contactId localDisplayName ldn currentTs
        pure $ Right (m {localDisplayName = ldn, memberProfile = profile}, ct {localDisplayName = ldn, profile} :: Contact)
  where
    GroupMember {localDisplayName, memberProfile = LocalProfile {profileId, displayName, localAlias}} = m
    Profile {displayName = newName} = p'
    profile = toLocalProfile profileId p' localAlias

getXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> ExceptT StoreError IO Bool
getXGrpLinkMemReceived db mId =
  ExceptT . firstRow fromOnlyBI (SEGroupMemberNotFound mId) $
    DB.query db "SELECT xgrplinkmem_received FROM group_members WHERE group_member_id = ?" (Only mId)

setXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> Bool -> IO ()
setXGrpLinkMemReceived db mId xGrpLinkMemReceived = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE group_members SET xgrplinkmem_received = ?, updated_at = ? WHERE group_member_id = ?"
    (BI xGrpLinkMemReceived, currentTs, mId)

createNewUnknownGroupMember :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> MemberId -> Text -> ExceptT StoreError IO GroupMember
createNewUnknownGroupMember db vr user@User {userId, userContactId} GroupInfo {groupId} memberId memberName = do
  currentTs <- liftIO getCurrentTime
  let memberProfile = profileFromName memberName
  (localDisplayName, profileId) <- createNewMemberProfile_ db user memberProfile currentTs
  groupMemberId <- liftIO $ do
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          ( group_id, member_id, member_role, member_category, member_status, invited_by,
            user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
            peer_chat_min_version, peer_chat_max_version)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, memberId, GRAuthor, GCPreMember, GSMemUnknown, fromInvitedBy userContactId IBUnknown)
          :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, currentTs, currentTs)
          :. (minV, maxV)
      )
    insertedRowId db
  getGroupMemberById db vr user groupMemberId
  where
    VersionRange minV maxV = vr

updateUnknownMemberAnnounced :: DB.Connection -> VersionRangeChat -> User -> GroupMember -> GroupMember -> MemberInfo -> GroupMemberStatus -> ExceptT StoreError IO GroupMember
updateUnknownMemberAnnounced db vr user@User {userId} invitingMember unknownMember@GroupMember {groupMemberId, memberChatVRange} MemberInfo {memberRole, v, profile} status = do
  _ <- updateMemberProfile db user unknownMember profile
  currentTs <- liftIO getCurrentTime
  liftIO $
    DB.execute
      db
      [sql|
        UPDATE group_members
        SET member_role = ?,
            member_category = ?,
            member_status = ?,
            invited_by_group_member_id = ?,
            peer_chat_min_version = ?,
            peer_chat_max_version = ?,
            updated_at = ?
        WHERE user_id = ? AND group_member_id = ?
      |]
      ( (memberRole, GCPostMember, status, groupMemberId' invitingMember)
          :. (minV, maxV, currentTs, userId, groupMemberId)
      )
  getGroupMemberById db vr user groupMemberId
  where
    VersionRange minV maxV = maybe memberChatVRange fromChatVRange v

updateUserMemberProfileSentAt :: DB.Connection -> User -> GroupInfo -> UTCTime -> IO ()
updateUserMemberProfileSentAt db User {userId} GroupInfo {groupId} sentTs =
  DB.execute
    db
    "UPDATE groups SET user_member_profile_sent_at = ? WHERE user_id = ? AND group_id = ?"
    (sentTs, userId, groupId)

setGroupCustomData :: DB.Connection -> User -> GroupInfo -> Maybe CustomData -> IO ()
setGroupCustomData db User {userId} GroupInfo {groupId} customData = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE groups SET custom_data = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (customData, updatedAt, userId, groupId)

setGroupUIThemes :: DB.Connection -> User -> GroupInfo -> Maybe UIThemeEntityOverrides -> IO ()
setGroupUIThemes db User {userId} GroupInfo {groupId} uiThemes = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE groups SET ui_themes = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (uiThemes, updatedAt, userId, groupId)

updateGroupChatTags :: DB.Connection -> GroupId -> [ChatTagId] -> IO ()
updateGroupChatTags db gId tIds = do
  currentTags <- getGroupChatTags db gId
  let tagsToAdd = filter (`notElem` currentTags) tIds
      tagsToDelete = filter (`notElem` tIds) currentTags
  forM_ tagsToDelete $ untagGroupChat db gId
  forM_ tagsToAdd $ tagGroupChat db gId

tagGroupChat :: DB.Connection -> GroupId -> ChatTagId -> IO ()
tagGroupChat db groupId tId =
  DB.execute
    db
    [sql|
      INSERT INTO chat_tags_chats (group_id, chat_tag_id)
      VALUES (?,?)
    |]
    (groupId, tId)

untagGroupChat :: DB.Connection -> GroupId -> ChatTagId -> IO ()
untagGroupChat db groupId tId =
  DB.execute
    db
    [sql|
      DELETE FROM chat_tags_chats
      WHERE group_id = ? AND chat_tag_id = ?
    |]
    (groupId, tId)

setGroupChatTTL :: DB.Connection -> GroupId -> Maybe Int64 -> IO ()
setGroupChatTTL db gId ttl = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    "UPDATE groups SET chat_item_ttl = ?, updated_at = ? WHERE group_id = ?"
    (ttl, updatedAt, gId)

getGroupChatTTL :: DB.Connection -> GroupId -> IO (Maybe Int64)
getGroupChatTTL db gId =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT chat_item_ttl FROM groups WHERE group_id = ? LIMIT 1" (Only gId)

getUserGroupsToExpire :: DB.Connection -> User -> Int64 -> IO [GroupId]
getUserGroupsToExpire db User {userId} globalTTL =
  map fromOnly <$> DB.query db ("SELECT group_id FROM groups WHERE user_id = ? AND chat_item_ttl > 0" <> cond) (Only userId)
  where
    cond = if globalTTL == 0 then "" else " OR chat_item_ttl IS NULL"

updateGroupAlias :: DB.Connection -> UserId -> GroupInfo -> LocalAlias -> IO GroupInfo
updateGroupAlias db userId g@GroupInfo {groupId} localAlias = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE groups SET local_alias = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (localAlias, updatedAt, userId, groupId)
  pure (g :: GroupInfo) {localAlias = localAlias}
