{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    getGroupAndMember,
    createNewGroup,
    createGroupInvitation,
    createGroupInvitedViaLink,
    setViaGroupLinkHash,
    setGroupInvitationChatItemId,
    getGroup,
    getGroupInfo,
    getGroupInfoByUserContactLinkConnReq,
    getGroupInfoByGroupLinkHash,
    updateGroupProfile,
    getGroupIdByName,
    getGroupMemberIdByName,
    getActiveMembersByName,
    getGroupInfoByName,
    getGroupMember,
    getGroupMemberById,
    getGroupMemberByMemberId,
    getGroupMembers,
    getGroupMembersForExpiration,
    getGroupCurrentMembersCount,
    deleteGroupConnectionsAndFiles,
    deleteGroupItemsAndMembers,
    deleteGroup,
    getUserGroups,
    getUserGroupDetails,
    getUserGroupsWithSummary,
    getGroupSummary,
    getContactGroupPreferences,
    checkContactHasGroups,
    getGroupInvitation,
    createNewContactMember,
    createNewContactMemberAsync,
    createAcceptedMember,
    createAcceptedMemberConnection,
    getContactViaMember,
    setNewContactMemberConnRequest,
    getMemberInvitation,
    createMemberConnection,
    createMemberConnectionAsync,
    updateGroupMemberStatus,
    updateGroupMemberStatusById,
    createNewGroupMember,
    checkGroupMemberHasItems,
    deleteGroupMember,
    deleteGroupMemberConnection,
    updateGroupMemberRole,
    createIntroductions,
    updateIntroStatus,
    saveIntroInvitation,
    getIntroduction,
    getForwardIntroducedMembers,
    getForwardInvitedMembers,
    createIntroReMember,
    createIntroToMemberContact,
    saveMemberInvitation,
    getViaGroupMember,
    getViaGroupContact,
    getMatchingContacts,
    getMatchingMembers,
    getMatchingMemberContacts,
    createSentProbe,
    createSentProbeHash,
    matchReceivedProbe,
    matchReceivedProbeHash,
    matchSentProbe,
    mergeContactRecords,
    associateMemberWithContactRecord,
    associateContactWithMemberRecord,
    deleteOldProbes,
    updateGroupSettings,
    updateGroupMemberSettings,
    getXGrpMemIntroContDirect,
    getXGrpMemIntroContGroup,
    getHostConnId,
    createMemberContact,
    getMemberContact,
    setContactGrpInvSent,
    createMemberContactInvited,
    updateMemberContactInvited,
    resetMemberContactFields,
    updateMemberProfile,
    getXGrpLinkMemReceived,
    setXGrpLinkMemReceived,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import Data.Either (rights)
import Data.Int (Int64)
import Data.List (partition, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), Query (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Messages
import Simplex.Chat.Protocol (groupForwardVRange)
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ConnId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Util (eitherToMaybe, ($>>=), (<$$>))
import Simplex.Messaging.Version
import UnliftIO.STM

type GroupInfoRow = (Int64, GroupName, GroupName, Text, Maybe Text, Maybe ImageData, Maybe ProfileId, Maybe MsgFilter, Maybe Bool, Bool, Maybe GroupPreferences) :. (UTCTime, UTCTime, Maybe UTCTime) :. GroupMemberRow

type GroupMemberRow = ((Int64, Int64, MemberId, Version, Version, GroupMemberRole, GroupMemberCategory, GroupMemberStatus, Bool) :. (Maybe Int64, Maybe GroupMemberId, ContactName, Maybe ContactId, ProfileId, ProfileId, ContactName, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Maybe Preferences))

type MaybeGroupMemberRow = ((Maybe Int64, Maybe Int64, Maybe MemberId, Maybe Version, Maybe Version, Maybe GroupMemberRole, Maybe GroupMemberCategory, Maybe GroupMemberStatus, Maybe Bool) :. (Maybe Int64, Maybe GroupMemberId, Maybe ContactName, Maybe ContactId, Maybe ProfileId, Maybe ProfileId, Maybe ContactName, Maybe Text, Maybe ImageData, Maybe ConnReqContact, Maybe LocalAlias, Maybe Preferences))

toGroupInfo :: VersionRange -> Int64 -> GroupInfoRow -> GroupInfo
toGroupInfo vr userContactId ((groupId, localDisplayName, displayName, fullName, description, image, hostConnCustomUserProfileId, enableNtfs_, sendRcpts, favorite, groupPreferences) :. (createdAt, updatedAt, chatTs) :. userMemberRow) =
  let membership = (toGroupMember userContactId userMemberRow) {memberChatVRange = JVersionRange vr}
      chatSettings = ChatSettings {enableNtfs = fromMaybe MFAll enableNtfs_, sendRcpts, favorite}
      fullGroupPreferences = mergeGroupPreferences groupPreferences
      groupProfile = GroupProfile {displayName, fullName, description, image, groupPreferences}
   in GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId, chatSettings, createdAt, updatedAt, chatTs}

toGroupMember :: Int64 -> GroupMemberRow -> GroupMember
toGroupMember userContactId ((groupMemberId, groupId, memberId, minVer, maxVer, memberRole, memberCategory, memberStatus, showMessages) :. (invitedById, invitedByGroupMemberId, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, contactLink, localAlias, preferences)) =
  let memberProfile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}
      memberSettings = GroupMemberSettings {showMessages}
      invitedBy = toInvitedBy userContactId invitedById
      activeConn = Nothing
      memberChatVRange = JVersionRange $ fromMaybe (versionToRange maxVer) $ safeVersionRange minVer maxVer
   in GroupMember {..}

toMaybeGroupMember :: Int64 -> MaybeGroupMemberRow -> Maybe GroupMember
toMaybeGroupMember userContactId ((Just groupMemberId, Just groupId, Just memberId, Just minVer, Just maxVer, Just memberRole, Just memberCategory, Just memberStatus, Just showMessages) :. (invitedById, invitedByGroupMemberId, Just localDisplayName, memberContactId, Just memberContactProfileId, Just profileId, Just displayName, Just fullName, image, contactLink, Just localAlias, contactPreferences)) =
  Just $ toGroupMember userContactId ((groupMemberId, groupId, memberId, minVer, maxVer, memberRole, memberCategory, memberStatus, showMessages) :. (invitedById, invitedByGroupMemberId, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, contactLink, localAlias, contactPreferences))
toMaybeGroupMember _ _ = Nothing

createGroupLink :: DB.Connection -> User -> GroupInfo -> ConnId -> ConnReqContact -> GroupLinkId -> GroupMemberRole -> SubscriptionMode -> ExceptT StoreError IO ()
createGroupLink db User {userId} groupInfo@GroupInfo {groupId, localDisplayName} agentConnId cReq groupLinkId memberRole subMode =
  checkConstraint (SEDuplicateGroupLink groupInfo) . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, group_id, group_link_id, local_display_name, conn_req_contact, group_link_member_role, auto_accept, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
      (userId, groupId, groupLinkId, "group_link_" <> localDisplayName, cReq, memberRole, True, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId chatInitialVRange Nothing Nothing Nothing 0 currentTs subMode

getGroupLinkConnection :: DB.Connection -> User -> GroupInfo -> ExceptT StoreError IO Connection
getGroupLinkConnection db User {userId} groupInfo@GroupInfo {groupId} =
  ExceptT . firstRow toConnection (SEGroupLinkNotFound groupInfo) $
    DB.query
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
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
    |]
    (userId, userId, groupId)
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

getGroupLink :: DB.Connection -> User -> GroupInfo -> ExceptT StoreError IO (Int64, ConnReqContact, GroupMemberRole)
getGroupLink db User {userId} gInfo@GroupInfo {groupId} =
  ExceptT . firstRow groupLink (SEGroupLinkNotFound gInfo) $
    DB.query db "SELECT user_contact_link_id, conn_req_contact, group_link_member_role FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)
  where
    groupLink (linkId, cReq, mRole_) = (linkId, cReq, fromMaybe GRMember mRole_)

getGroupLinkId :: DB.Connection -> User -> GroupInfo -> IO (Maybe GroupLinkId)
getGroupLinkId db User {userId} GroupInfo {groupId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT group_link_id FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)

setGroupLinkMemberRole :: DB.Connection -> User -> Int64 -> GroupMemberRole -> IO ()
setGroupLinkMemberRole db User {userId} userContactLinkId memberRole =
  DB.execute db "UPDATE user_contact_links SET group_link_member_role = ? WHERE user_id = ? AND user_contact_link_id = ?" (memberRole, userId, userContactLinkId)

getGroupAndMember :: DB.Connection -> User -> Int64 -> VersionRange -> ExceptT StoreError IO (GroupInfo, GroupMember)
getGroupAndMember db User {userId, userContactId} groupMemberId vr =
  ExceptT . firstRow toGroupAndMember (SEInternalError "referenced group member not found") $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category,
          mu.member_status, mu.show_messages, mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
          -- from GroupMember
          m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category, m.member_status, m.show_messages,
          m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        JOIN groups g ON g.group_id = m.group_id
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members mu ON g.group_id = mu.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.user_id = ? AND cc.group_member_id = m.group_member_id
        )
        WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (userId, groupMemberId, userId, userContactId)
  where
    toGroupAndMember :: (GroupInfoRow :. GroupMemberRow :. MaybeConnectionRow) -> (GroupInfo, GroupMember)
    toGroupAndMember (groupInfoRow :. memberRow :. connRow) =
      let groupInfo = toGroupInfo vr userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})

-- | creates completely new group with a single member - the current user
createNewGroup :: DB.Connection -> VersionRange -> TVar ChaChaDRG -> User -> GroupProfile -> Maybe Profile -> ExceptT StoreError IO GroupInfo
createNewGroup db vr gVar user@User {userId} groupProfile incognitoProfile = ExceptT $ do
  let GroupProfile {displayName, fullName, description, image, groupPreferences} = groupProfile
      fullGroupPreferences = mergeGroupPreferences groupPreferences
  currentTs <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId currentTs) incognitoProfile
  withLocalDisplayName db userId displayName $ \ldn -> runExceptT $ do
    groupId <- liftIO $ do
      DB.execute
        db
        "INSERT INTO group_profiles (display_name, full_name, description, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
        (displayName, fullName, description, image, userId, groupPreferences, currentTs, currentTs)
      profileId <- insertedRowId db
      DB.execute
        db
        "INSERT INTO groups (local_display_name, user_id, group_profile_id, enable_ntfs, created_at, updated_at, chat_ts) VALUES (?,?,?,?,?,?,?)"
        (ldn, userId, profileId, True, currentTs, currentTs, currentTs)
      insertedRowId db
    memberId <- liftIO $ encodedRandomBytes gVar 12
    membership <- createContactMemberInv_ db user groupId Nothing user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser customUserProfileId currentTs vr
    let chatSettings = ChatSettings {enableNtfs = MFAll, sendRcpts = Nothing, favorite = False}
    pure GroupInfo {groupId, localDisplayName = ldn, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = Nothing, chatSettings, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs}

-- | creates a new group record for the group the current user was invited to, or returns an existing one
createGroupInvitation :: DB.Connection -> VersionRange -> User -> Contact -> GroupInvitation -> Maybe ProfileId -> ExceptT StoreError IO (GroupInfo, GroupMemberId)
createGroupInvitation _ _ _ Contact {localDisplayName, activeConn = Nothing} _ _ = throwError $ SEContactNotReady localDisplayName
createGroupInvitation db vr user@User {userId} contact@Contact {contactId, activeConn = Just hostConn@Connection {customUserProfileId}} GroupInvitation {fromMember, invitedMember, connRequest, groupProfile} incognitoProfileId = do
  liftIO getInvitationGroupId_ >>= \case
    Nothing -> createGroupInvitation_
    Just gId -> do
      gInfo@GroupInfo {membership, groupProfile = p'} <- getGroupInfo db vr user gId
      hostId <- getHostMemberId_ db user gId
      let GroupMember {groupMemberId, memberId, memberRole} = membership
          MemberIdRole {memberId = memberId', memberRole = memberRole'} = invitedMember
      liftIO . when (memberId /= memberId' || memberRole /= memberRole') $
        DB.execute db "UPDATE group_members SET member_id = ?, member_role = ? WHERE group_member_id = ?" (memberId', memberRole', groupMemberId)
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
      let GroupProfile {displayName, fullName, description, image, groupPreferences} = groupProfile
          fullGroupPreferences = mergeGroupPreferences groupPreferences
      ExceptT $
        withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
          currentTs <- liftIO getCurrentTime
          groupId <- liftIO $ do
            DB.execute
              db
              "INSERT INTO group_profiles (display_name, full_name, description, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
              (displayName, fullName, description, image, userId, groupPreferences, currentTs, currentTs)
            profileId <- insertedRowId db
            DB.execute
              db
              "INSERT INTO groups (group_profile_id, local_display_name, inv_queue_info, host_conn_custom_user_profile_id, user_id, enable_ntfs, created_at, updated_at, chat_ts) VALUES (?,?,?,?,?,?,?,?,?)"
              (profileId, localDisplayName, connRequest, customUserProfileId, userId, True, currentTs, currentTs, currentTs)
            insertedRowId db
          let JVersionRange hostVRange = hostConn.peerChatVRange
          GroupMember {groupMemberId} <- createContactMemberInv_ db user groupId Nothing contact fromMember GCHostMember GSMemInvited IBUnknown Nothing currentTs hostVRange
          membership <- createContactMemberInv_ db user groupId (Just groupMemberId) user invitedMember GCUserMember GSMemInvited (IBContact contactId) incognitoProfileId currentTs vr
          let chatSettings = ChatSettings {enableNtfs = MFAll, sendRcpts = Nothing, favorite = False}
          pure (GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = customUserProfileId, chatSettings, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs}, groupMemberId)

getHostMemberId_ :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO GroupMemberId
getHostMemberId_ db User {userId} groupId =
  ExceptT . firstRow fromOnly (SEHostMemberIdNotFound groupId) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND member_category = ?" (userId, groupId, GCHostMember)

createContactMemberInv_ :: IsContact a => DB.Connection -> User -> GroupId -> Maybe GroupMemberId -> a -> MemberIdRole -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> Maybe ProfileId -> UTCTime -> VersionRange -> ExceptT StoreError IO GroupMember
createContactMemberInv_ db User {userId, userContactId} groupId invitedByGroupMemberId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy incognitoProfileId createdAt memberChatVRange@(VersionRange minV maxV) = do
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
        invitedBy,
        invitedByGroupMemberId,
        localDisplayName,
        memberProfile,
        memberContactId = Just $ contactId' userOrContact,
        memberContactProfileId = localProfileId (profile' userOrContact),
        activeConn = Nothing,
        memberChatVRange = JVersionRange memberChatVRange
      }
  where
    insertMember_ :: IO ContactName
    insertMember_ = do
      let localDisplayName = localDisplayName' userOrContact
      DB.execute
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
              user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
              peer_chat_min_version, peer_chat_max_version)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ( (groupId, memberId, memberRole, memberCategory, memberStatus, fromInvitedBy userContactId invitedBy, invitedByGroupMemberId)
            :. (userId, localDisplayName' userOrContact, contactId' userOrContact, localProfileId $ profile' userOrContact, createdAt, createdAt)
            :. (minV, maxV)
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

createGroupInvitedViaLink :: DB.Connection -> VersionRange -> User -> Connection -> GroupLinkInvitation -> ExceptT StoreError IO (GroupInfo, GroupMember)
createGroupInvitedViaLink
  db
  vr
  user@User {userId, userContactId}
  Connection {connId, customUserProfileId}
  GroupLinkInvitation {fromMember, fromMemberName, invitedMember, groupProfile} = do
    currentTs <- liftIO getCurrentTime
    groupId <- insertGroup_ currentTs
    hostMemberId <- insertHost_ currentTs groupId
    liftIO $ DB.execute db "UPDATE connections SET conn_type = ?, group_member_id = ?, updated_at = ? WHERE connection_id = ?" (ConnMember, hostMemberId, currentTs, connId)
    -- using IBUnknown since host is created without contact
    void $ createContactMemberInv_ db user groupId (Just hostMemberId) user invitedMember GCUserMember GSMemAccepted IBUnknown customUserProfileId currentTs vr
    liftIO $ setViaGroupLinkHash db groupId connId
    (,) <$> getGroupInfo db vr user groupId <*> getGroupMemberById db user hostMemberId
    where
      insertGroup_ currentTs = ExceptT $ do
        let GroupProfile {displayName, fullName, description, image, groupPreferences} = groupProfile
        withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
          liftIO $ do
            DB.execute
              db
              "INSERT INTO group_profiles (display_name, full_name, description, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
              (displayName, fullName, description, image, userId, groupPreferences, currentTs, currentTs)
            profileId <- insertedRowId db
            DB.execute
              db
              "INSERT INTO groups (group_profile_id, local_display_name, host_conn_custom_user_profile_id, user_id, enable_ntfs, created_at, updated_at, chat_ts) VALUES (?,?,?,?,?,?,?,?)"
              (profileId, localDisplayName, customUserProfileId, userId, True, currentTs, currentTs, currentTs)
            insertedRowId db
      insertHost_ currentTs groupId = ExceptT $ do
        let fromMemberProfile = profileFromName fromMemberName
        withLocalDisplayName db userId fromMemberName $ \localDisplayName -> runExceptT $ do
          (_, profileId) <- createNewMemberProfile_ db user fromMemberProfile currentTs
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

setViaGroupLinkHash :: DB.Connection -> GroupId -> Int64 -> IO ()
setViaGroupLinkHash db groupId connId =
  DB.execute
    db
    [sql|
      UPDATE groups
      SET via_group_link_uri_hash = (SELECT via_contact_uri_hash FROM connections WHERE connection_id = ?)
      WHERE group_id = ?
    |]
    (connId, groupId)

setGroupInvitationChatItemId :: DB.Connection -> User -> GroupId -> ChatItemId -> IO ()
setGroupInvitationChatItemId db User {userId} groupId chatItemId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE groups SET chat_item_id = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (chatItemId, currentTs, userId, groupId)

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroup :: DB.Connection -> VersionRange -> User -> GroupId -> ExceptT StoreError IO Group
getGroup db vr user groupId = do
  gInfo <- getGroupInfo db vr user groupId
  members <- liftIO $ getGroupMembers db user gInfo
  pure $ Group gInfo members

deleteGroupConnectionsAndFiles :: DB.Connection -> User -> GroupInfo -> [GroupMember] -> IO ()
deleteGroupConnectionsAndFiles db User {userId} GroupInfo {groupId} members = do
  forM_ members $ \m -> DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId' m)
  DB.execute db "DELETE FROM files WHERE user_id = ? AND group_id = ?" (userId, groupId)

deleteGroupItemsAndMembers :: DB.Connection -> User -> GroupInfo -> [GroupMember] -> IO ()
deleteGroupItemsAndMembers db user@User {userId} g@GroupInfo {groupId} members = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)
  void $ runExceptT cleanupHostGroupLinkConn_ -- to allow repeat connection via the same group link if one was used
  DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_id = ?" (userId, groupId)
  forM_ members $ cleanupMemberProfileAndName_ db user
  forM_ (incognitoMembershipProfile g) $ deleteUnusedIncognitoProfileById_ db user . localProfileId
  where
    cleanupHostGroupLinkConn_ = do
      hostId <- getHostMemberId_ db user groupId
      liftIO $
        DB.execute
          db
          [sql|
            UPDATE connections SET via_contact_uri_hash = NULL, xcontact_id = NULL
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
  DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
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

getUserGroups :: DB.Connection -> VersionRange -> User -> IO [Group]
getUserGroups db vr user@User {userId} = do
  groupIds <- map fromOnly <$> DB.query db "SELECT group_id FROM groups WHERE user_id = ?" (Only userId)
  rights <$> mapM (runExceptT . getGroup db vr user) groupIds

getUserGroupDetails :: DB.Connection -> VersionRange -> User -> Maybe ContactId -> Maybe String -> IO [GroupInfo]
getUserGroupDetails db vr User {userId, userContactId} _contactId_ search_ =
  map (toGroupInfo vr userContactId)
    <$> DB.query
      db
      [sql|
        SELECT g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          mu.group_member_id, g.group_id, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category, mu.member_status, mu.show_messages,
          mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id, pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences
        FROM groups g
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members mu USING (group_id)
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        WHERE g.user_id = ? AND mu.contact_id = ?
          AND (gp.display_name LIKE '%' || ? || '%' OR gp.full_name LIKE '%' || ? || '%' OR gp.description LIKE '%' || ? || '%')
      |]
      (userId, userContactId, search, search, search)
  where
    search = fromMaybe "" search_

getUserGroupsWithSummary :: DB.Connection -> VersionRange -> User -> Maybe ContactId -> Maybe String -> IO [(GroupInfo, GroupSummary)]
getUserGroupsWithSummary db vr user _contactId_ search_ =
  getUserGroupDetails db vr user _contactId_ search_
    >>= mapM (\g@GroupInfo {groupId} -> (g,) <$> getGroupSummary db user groupId)

-- the statuses on non-current members should match memberCurrent' function
getGroupSummary :: DB.Connection -> User -> GroupId -> IO GroupSummary
getGroupSummary db User {userId} groupId = do
  currentMembers_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT count (m.group_member_id)
          FROM groups g
          JOIN group_members m USING (group_id)
          WHERE g.user_id = ?
            AND g.group_id = ?
            AND m.member_status != ?
            AND m.member_status != ?
            AND m.member_status != ?
        |]
        (userId, groupId, GSMemRemoved, GSMemLeft, GSMemInvited)
  pure GroupSummary {currentMembers = fromMaybe 0 currentMembers_}

getContactGroupPreferences :: DB.Connection -> User -> Contact -> IO [FullGroupPreferences]
getContactGroupPreferences db User {userId} Contact {contactId} = do
  map (mergeGroupPreferences . fromOnly)
    <$> DB.query
      db
      [sql|
        SELECT gp.preferences
        FROM groups g
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members m USING (group_id)
        WHERE g.user_id = ? AND m.contact_id = ?
      |]
      (userId, contactId)

checkContactHasGroups :: DB.Connection -> User -> Contact -> IO (Maybe GroupId)
checkContactHasGroups db User {userId} Contact {contactId} =
  maybeFirstRow fromOnly $ DB.query db "SELECT group_id FROM group_members WHERE user_id = ? AND contact_id = ? LIMIT 1" (userId, contactId)

getGroupInfoByName :: DB.Connection -> VersionRange -> User -> GroupName -> ExceptT StoreError IO GroupInfo
getGroupInfoByName db vr user gName = do
  gId <- getGroupIdByName db user gName
  getGroupInfo db vr user gId

groupMemberQuery :: Query
groupMemberQuery =
  [sql|
    SELECT
      m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category, m.member_status, m.show_messages,
      m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
      c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
      c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
      c.peer_chat_min_version, c.peer_chat_max_version
    FROM group_members m
    JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
    LEFT JOIN connections c ON c.connection_id = (
      SELECT max(cc.connection_id)
      FROM connections cc
      WHERE cc.user_id = ? AND cc.group_member_id = m.group_member_id
    )
  |]

getGroupMember :: DB.Connection -> User -> GroupId -> GroupMemberId -> ExceptT StoreError IO GroupMember
getGroupMember db user@User {userId} groupId groupMemberId =
  ExceptT . firstRow (toContactMember user) (SEGroupMemberNotFound groupMemberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.group_member_id = ? AND m.user_id = ?")
      (userId, groupId, groupMemberId, userId)

getGroupMemberById :: DB.Connection -> User -> GroupMemberId -> ExceptT StoreError IO GroupMember
getGroupMemberById db user@User {userId} groupMemberId =
  ExceptT . firstRow (toContactMember user) (SEGroupMemberNotFound groupMemberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_member_id = ? AND m.user_id = ?")
      (userId, groupMemberId, userId)

getGroupMemberByMemberId :: DB.Connection -> User -> GroupInfo -> MemberId -> ExceptT StoreError IO GroupMember
getGroupMemberByMemberId db user@User {userId} GroupInfo {groupId} memberId =
  ExceptT . firstRow (toContactMember user) (SEGroupMemberNotFoundByMemberId memberId) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.member_id = ?")
      (userId, groupId, memberId)

getGroupMembers :: DB.Connection -> User -> GroupInfo -> IO [GroupMember]
getGroupMembers db user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember user)
    <$> DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)")
      (userId, groupId, userId, userContactId)

getGroupMembersForExpiration :: DB.Connection -> User -> GroupInfo -> IO [GroupMember]
getGroupMembersForExpiration db user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember user)
    <$> DB.query
      db
      ( groupMemberQuery
          <> [sql|
                WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)
                  AND m.member_status IN (?, ?, ?)
                  AND m.group_member_id NOT IN (
                    SELECT DISTINCT group_member_id FROM chat_items
                  )
              |]
      )
      (userId, groupId, userId, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted)

toContactMember :: User -> (GroupMemberRow :. MaybeConnectionRow) -> GroupMember
toContactMember User {userContactId} (memberRow :. connRow) =
  (toGroupMember userContactId memberRow) {activeConn = toMaybeConnection connRow}

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

getGroupInvitation :: DB.Connection -> VersionRange -> User -> GroupId -> ExceptT StoreError IO ReceivedGroupInvitation
getGroupInvitation db vr user groupId =
  getConnRec_ user >>= \case
    Just connRequest -> do
      groupInfo@GroupInfo {membership} <- getGroupInfo db vr user groupId
      when (memberStatus membership /= GSMemInvited) $ throwError SEGroupAlreadyJoined
      hostId <- getHostMemberId_ db user groupId
      fromMember <- getGroupMember db user groupId hostId
      pure ReceivedGroupInvitation {fromMember, connRequest, groupInfo}
    _ -> throwError SEGroupInvitationNotFound
  where
    getConnRec_ :: User -> ExceptT StoreError IO (Maybe ConnReqInvitation)
    getConnRec_ User {userId} = ExceptT $ do
      firstRow fromOnly (SEGroupNotFound groupId) $
        DB.query db "SELECT g.inv_queue_info FROM groups g WHERE g.group_id = ? AND g.user_id = ?" (groupId, userId)

createNewContactMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> Contact -> GroupMemberRole -> ConnId -> ConnReqInvitation -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createNewContactMember _ _ _ _ Contact {localDisplayName, activeConn = Nothing} _ _ _ _ = throwError $ SEContactNotReady localDisplayName
createNewContactMember db gVar User {userId, userContactId} GroupInfo {groupId, membership} Contact {contactId, localDisplayName, profile, activeConn = Just Connection {peerChatVRange}} memberRole agentConnId connRequest subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    member@GroupMember {groupMemberId} <- createMember_ (MemberId memId) createdAt
    void $ createMemberConnection_ db userId groupMemberId agentConnId (fromJVersionRange peerChatVRange) Nothing 0 createdAt subMode
    pure member
  where
    JVersionRange (VersionRange minV maxV) = peerChatVRange
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
            invitedBy = IBUser,
            invitedByGroupMemberId = Just invitedByGroupMemberId,
            localDisplayName,
            memberProfile = profile,
            memberContactId = Just contactId,
            memberContactProfileId = localProfileId profile,
            activeConn = Nothing,
            memberChatVRange = peerChatVRange
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

createNewContactMemberAsync :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> Contact -> GroupMemberRole -> (CommandId, ConnId) -> VersionRange -> SubscriptionMode -> ExceptT StoreError IO ()
createNewContactMemberAsync db gVar user@User {userId, userContactId} GroupInfo {groupId, membership} Contact {contactId, localDisplayName, profile} memberRole (cmdId, agentConnId) peerChatVRange subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    insertMember_ (MemberId memId) createdAt
    groupMemberId <- liftIO $ insertedRowId db
    Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId peerChatVRange Nothing 0 createdAt subMode
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

createAcceptedMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupInfo -> UserContactRequest -> GroupMemberRole -> ExceptT StoreError IO (GroupMemberId, MemberId)
createAcceptedMember
  db
  gVar
  User {userId, userContactId}
  GroupInfo {groupId, membership}
  UserContactRequest {cReqChatVRange, localDisplayName, profileId}
  memberRole = do
    liftIO $
      DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
    createWithRandomId gVar $ \memId -> do
      createdAt <- liftIO getCurrentTime
      insertMember_ (MemberId memId) createdAt
      groupMemberId <- liftIO $ insertedRowId db
      pure (groupMemberId, MemberId memId)
    where
      JVersionRange (VersionRange minV maxV) = cReqChatVRange
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
          ( (groupId, memberId, memberRole, GCInviteeMember, GSMemAccepted, fromInvitedBy userContactId IBUser, groupMemberId' membership)
              :. (userId, localDisplayName, Nothing :: (Maybe Int64), profileId, createdAt, createdAt)
              :. (minV, maxV)
          )

createAcceptedMemberConnection :: DB.Connection -> User -> (CommandId, ConnId) -> UserContactRequest -> GroupMemberId -> SubscriptionMode -> IO ()
createAcceptedMemberConnection
  db
  user@User {userId}
  (cmdId, agentConnId)
  UserContactRequest {cReqChatVRange, userContactLinkId}
  groupMemberId
  subMode = do
    createdAt <- liftIO getCurrentTime
    Connection {connId} <- createConnection_ db userId ConnMember (Just groupMemberId) agentConnId (fromJVersionRange cReqChatVRange) Nothing (Just userContactLinkId) Nothing 0 createdAt subMode
    setCommandConnId db user cmdId connId

getContactViaMember :: DB.Connection -> User -> GroupMember -> ExceptT StoreError IO Contact
getContactViaMember db user@User {userId} GroupMember {groupMemberId} = do
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
  getContact db user contactId

setNewContactMemberConnRequest :: DB.Connection -> User -> GroupMember -> ConnReqInvitation -> IO ()
setNewContactMemberConnRequest db User {userId} GroupMember {groupMemberId} connRequest = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE group_members SET sent_inv_queue_info = ?, updated_at = ? WHERE user_id = ? AND group_member_id = ?" (connRequest, currentTs, userId, groupMemberId)

getMemberInvitation :: DB.Connection -> User -> Int64 -> IO (Maybe ConnReqInvitation)
getMemberInvitation db User {userId} groupMemberId =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT sent_inv_queue_info FROM group_members WHERE group_member_id = ? AND user_id = ?" (groupMemberId, userId)

createMemberConnection :: DB.Connection -> UserId -> GroupMember -> ConnId -> VersionRange -> SubscriptionMode -> IO ()
createMemberConnection db userId GroupMember {groupMemberId} agentConnId peerChatVRange subMode = do
  currentTs <- getCurrentTime
  void $ createMemberConnection_ db userId groupMemberId agentConnId peerChatVRange Nothing 0 currentTs subMode

createMemberConnectionAsync :: DB.Connection -> User -> GroupMemberId -> (CommandId, ConnId) -> VersionRange -> SubscriptionMode -> IO ()
createMemberConnectionAsync db user@User {userId} groupMemberId (cmdId, agentConnId) peerChatVRange subMode = do
  currentTs <- getCurrentTime
  Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId peerChatVRange Nothing 0 currentTs subMode
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
            memInvitedBy = IBUnknown,
            memInvitedByGroupMemberId = Just $ groupMemberId' invitingMember,
            localDisplayName,
            memContactId = Nothing,
            memProfileId
          }
  liftIO $ createNewMember_ db user gInfo newMember currentTs

createNewMemberProfile_ :: DB.Connection -> User -> Profile -> UTCTime -> ExceptT StoreError IO (Text, ProfileId)
createNewMemberProfile_ db User {userId} Profile {displayName, fullName, image, contactLink, preferences} createdAt =
  ExceptT . withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (displayName, fullName, image, contactLink, userId, preferences, createdAt, createdAt)
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
      memInvitedBy = invitedBy,
      memInvitedByGroupMemberId,
      localDisplayName,
      memContactId = memberContactId,
      memProfileId = memberContactProfileId
    }
  createdAt = do
    let invitedById = fromInvitedBy userContactId invitedBy
        activeConn = Nothing
        mcvr@(VersionRange minV maxV) = maybe chatInitialVRange fromChatVRange memChatVRange
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          (group_id, member_id, member_role, member_category, member_status, invited_by, invited_by_group_member_id,
           user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at,
           peer_chat_min_version, peer_chat_max_version)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, memInvitedByGroupMemberId)
          :. (userId, localDisplayName, memberContactId, memberContactProfileId, createdAt, createdAt)
          :. (minV, maxV)
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
          invitedBy,
          invitedByGroupMemberId = memInvitedByGroupMemberId,
          localDisplayName,
          memberProfile = toLocalProfile memberContactProfileId memberProfile "",
          memberContactId,
          memberContactProfileId,
          activeConn,
          memberChatVRange = JVersionRange mcvr
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
cleanupMemberProfileAndName_ db User {userId} GroupMember {groupMemberId, memberContactId, memberContactProfileId, localDisplayName} =
  -- check record has no memberContactId (contact_id) - it means contact has been deleted and doesn't use profile & ldn
  when (isNothing memberContactId) $ do
    -- check other group member records don't use profile & ldn
    sameProfileMember :: (Maybe GroupMemberId) <- maybeFirstRow fromOnly $ DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND contact_profile_id = ? AND group_member_id != ? LIMIT 1" (userId, memberContactProfileId, groupMemberId)
    when (isNothing sameProfileMember) $ do
      DB.execute db "DELETE FROM contact_profiles WHERE user_id = ? AND contact_profile_id = ?" (userId, memberContactProfileId)
      DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)

deleteGroupMemberConnection :: DB.Connection -> User -> GroupMember -> IO ()
deleteGroupMemberConnection db User {userId} GroupMember {groupMemberId} =
  DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId)

updateGroupMemberRole :: DB.Connection -> User -> GroupMember -> GroupMemberRole -> IO ()
updateGroupMemberRole db User {userId} GroupMember {groupMemberId} memRole =
  DB.execute db "UPDATE group_members SET member_role = ? WHERE user_id = ? AND group_member_id = ?" (memRole, userId, groupMemberId)

createIntroductions :: DB.Connection -> Version -> [GroupMember] -> GroupMember -> IO [GroupMemberIntro]
createIntroductions db chatV members toMember = do
  let reMembers = filter (\m -> memberCurrent m && groupMemberId' m /= groupMemberId' toMember) members
  if null reMembers
    then pure []
    else do
      currentTs <- getCurrentTime
      mapM (insertIntro_ currentTs) reMembers
  where
    insertIntro_ :: UTCTime -> GroupMember -> IO GroupMemberIntro
    insertIntro_ ts reMember = do
      DB.execute
        db
        [sql|
          INSERT INTO group_member_intros
            (re_group_member_id, to_group_member_id, intro_status, intro_chat_protocol_version, created_at, updated_at)
          VALUES (?,?,?,?,?,?)
        |]
        (groupMemberId' reMember, groupMemberId' toMember, GMIntroPending, chatV, ts, ts)
      introId <- insertedRowId db
      pure GroupMemberIntro {introId, reMember, toMember, introStatus = GMIntroPending, introInvitation = Nothing}

updateIntroStatus :: DB.Connection -> Int64 -> GroupMemberIntroStatus -> IO ()
updateIntroStatus db introId introStatus = do
  currentTs <- getCurrentTime
  DB.executeNamed
    db
    [sql|
      UPDATE group_member_intros
      SET intro_status = :intro_status, updated_at = :updated_at
      WHERE group_member_intro_id = :intro_id
    |]
    [":intro_status" := introStatus, ":updated_at" := currentTs, ":intro_id" := introId]

saveIntroInvitation :: DB.Connection -> GroupMember -> GroupMember -> IntroInvitation -> ExceptT StoreError IO GroupMemberIntro
saveIntroInvitation db reMember toMember introInv = do
  intro <- getIntroduction db reMember toMember
  liftIO $ do
    currentTs <- getCurrentTime
    DB.executeNamed
      db
      [sql|
        UPDATE group_member_intros
        SET intro_status = :intro_status,
            group_queue_info = :group_queue_info,
            direct_queue_info = :direct_queue_info,
            updated_at = :updated_at
        WHERE group_member_intro_id = :intro_id
      |]
      [ ":intro_status" := GMIntroInvReceived,
        ":group_queue_info" := introInv.groupConnReq,
        ":direct_queue_info" := directConnReq introInv,
        ":updated_at" := currentTs,
        ":intro_id" := introId intro
      ]
  pure intro {introInvitation = Just introInv, introStatus = GMIntroInvReceived}

saveMemberInvitation :: DB.Connection -> GroupMember -> IntroInvitation -> IO ()
saveMemberInvitation db GroupMember {groupMemberId} IntroInvitation {groupConnReq, directConnReq} = do
  currentTs <- getCurrentTime
  DB.executeNamed
    db
    [sql|
      UPDATE group_members
      SET member_status = :member_status,
          group_queue_info = :group_queue_info,
          direct_queue_info = :direct_queue_info,
          updated_at = :updated_at
      WHERE group_member_id = :group_member_id
    |]
    [ ":member_status" := GSMemIntroInvited,
      ":group_queue_info" := groupConnReq,
      ":direct_queue_info" := directConnReq,
      ":updated_at" := currentTs,
      ":group_member_id" := groupMemberId
    ]

getIntroduction :: DB.Connection -> GroupMember -> GroupMember -> ExceptT StoreError IO GroupMemberIntro
getIntroduction db reMember toMember = ExceptT $ do
  toIntro
    <$> DB.query
      db
      [sql|
        SELECT group_member_intro_id, group_queue_info, direct_queue_info, intro_status
        FROM group_member_intros
        WHERE re_group_member_id = ? AND to_group_member_id = ?
      |]
      (groupMemberId' reMember, groupMemberId' toMember)
  where
    toIntro :: [(Int64, Maybe ConnReqInvitation, Maybe ConnReqInvitation, GroupMemberIntroStatus)] -> Either StoreError GroupMemberIntro
    toIntro [(introId, groupConnReq, directConnReq, introStatus)] =
      let introInvitation = IntroInvitation <$> groupConnReq <*> pure directConnReq
       in Right GroupMemberIntro {introId, reMember, toMember, introStatus, introInvitation}
    toIntro _ = Left SEIntroNotFound

getForwardIntroducedMembers :: DB.Connection -> User -> GroupMember -> Bool -> IO [GroupMember]
getForwardIntroducedMembers db user invitee highlyAvailable = do
  memberIds <- map fromOnly <$> query
  filter memberCurrent . rights <$> mapM (runExceptT . getGroupMemberById db user) memberIds
  where
    mId = groupMemberId' invitee
    query
      | highlyAvailable = DB.query db q (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected)
      | otherwise =
          DB.query
            db
            (q <> " AND intro_chat_protocol_version >= ?")
            (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, minVersion groupForwardVRange)
    q =
      [sql|
        SELECT re_group_member_id
        FROM group_member_intros
        WHERE to_group_member_id = ? AND intro_status NOT IN (?,?,?)
      |]

getForwardInvitedMembers :: DB.Connection -> User -> GroupMember -> Bool -> IO [GroupMember]
getForwardInvitedMembers db user forwardMember highlyAvailable = do
  memberIds <- map fromOnly <$> query
  filter memberCurrent . rights <$> mapM (runExceptT . getGroupMemberById db user) memberIds
  where
    mId = groupMemberId' forwardMember
    query
      | highlyAvailable = DB.query db q (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected)
      | otherwise =
          DB.query
            db
            (q <> " AND intro_chat_protocol_version >= ?")
            (mId, GMIntroReConnected, GMIntroToConnected, GMIntroConnected, minVersion groupForwardVRange)
    q =
      [sql|
        SELECT to_group_member_id
        FROM group_member_intros
        WHERE re_group_member_id = ? AND intro_status NOT IN (?,?,?)
      |]

createIntroReMember :: DB.Connection -> User -> GroupInfo -> GroupMember -> MemberInfo -> (CommandId, ConnId) -> Maybe (CommandId, ConnId) -> Maybe ProfileId -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createIntroReMember db user@User {userId} gInfo@GroupInfo {groupId} _host@GroupMember {memberContactId, activeConn} memInfo@(MemberInfo _ _ memChatVRange memberProfile) (groupCmdId, groupAgentConnId) directConnIds customUserProfileId subMode = do
  let mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
      cLevel = 1 + maybe 0 (\Connection {connLevel} -> connLevel) activeConn
  currentTs <- liftIO getCurrentTime
  newMember <- case directConnIds of
    Just (directCmdId, directAgentConnId) -> do
      Connection {connId = directConnId} <- liftIO $ createConnection_ db userId ConnContact Nothing directAgentConnId mcvr memberContactId Nothing customUserProfileId cLevel currentTs subMode
      liftIO $ setCommandConnId db user directCmdId directConnId
      (localDisplayName, contactId, memProfileId) <- createContact_ db userId memberProfile "" (Just groupId) currentTs False
      liftIO $ DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, currentTs, directConnId)
      pure $ NewGroupMember {memInfo, memCategory = GCPreMember, memStatus = GSMemIntroduced, memInvitedBy = IBUnknown, memInvitedByGroupMemberId = Nothing, localDisplayName, memContactId = Just contactId, memProfileId}
    Nothing -> do
      (localDisplayName, memProfileId) <- createNewMemberProfile_ db user memberProfile currentTs
      pure $ NewGroupMember {memInfo, memCategory = GCPreMember, memStatus = GSMemIntroduced, memInvitedBy = IBUnknown, memInvitedByGroupMemberId = Nothing, localDisplayName, memContactId = Nothing, memProfileId}
  liftIO $ do
    member <- createNewMember_ db user gInfo newMember currentTs
    conn@Connection {connId = groupConnId} <- createMemberConnection_ db userId (groupMemberId' member) groupAgentConnId mcvr memberContactId cLevel currentTs subMode
    liftIO $ setCommandConnId db user groupCmdId groupConnId
    pure (member :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: DB.Connection -> User -> GroupMember -> GroupMember -> VersionRange -> (CommandId, ConnId) -> Maybe (CommandId, ConnId) -> Maybe ProfileId -> SubscriptionMode -> IO ()
createIntroToMemberContact db user@User {userId} GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} mcvr (groupCmdId, groupAgentConnId) directConnIds customUserProfileId subMode = do
  let cLevel = 1 + maybe 0 (\Connection {connLevel} -> connLevel) activeConn
  currentTs <- getCurrentTime
  Connection {connId = groupConnId} <- createMemberConnection_ db userId groupMemberId groupAgentConnId mcvr viaContactId cLevel currentTs subMode
  setCommandConnId db user groupCmdId groupConnId
  forM_ directConnIds $ \(directCmdId, directAgentConnId) -> do
    Connection {connId = directConnId} <- createConnection_ db userId ConnContact Nothing directAgentConnId mcvr viaContactId Nothing customUserProfileId cLevel currentTs subMode
    setCommandConnId db user directCmdId directConnId
    contactId <- createMemberContact_ directConnId currentTs
    updateMember_ contactId currentTs
  where
    createMemberContact_ :: Int64 -> UTCTime -> IO Int64
    createMemberContact_ connId ts = do
      DB.execute
        db
        [sql|
          INSERT INTO contacts (contact_profile_id, via_group, local_display_name, user_id, created_at, updated_at, chat_ts)
          SELECT contact_profile_id, group_id, ?, ?, ?, ?, ?
          FROM group_members
          WHERE group_member_id = ?
        |]
        (localDisplayName, userId, ts, ts, ts, groupMemberId)
      contactId <- insertedRowId db
      DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, ts, connId)
      pure contactId
    updateMember_ :: Int64 -> UTCTime -> IO ()
    updateMember_ contactId ts =
      DB.executeNamed
        db
        [sql|
          UPDATE group_members
          SET contact_id = :contact_id, updated_at = :updated_at
          WHERE group_member_id = :group_member_id
        |]
        [":contact_id" := contactId, ":updated_at" := ts, ":group_member_id" := groupMemberId]

createMemberConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> VersionRange -> Maybe Int64 -> Int -> UTCTime -> SubscriptionMode -> IO Connection
createMemberConnection_ db userId groupMemberId agentConnId peerChatVRange viaContact = createConnection_ db userId ConnMember (Just groupMemberId) agentConnId peerChatVRange viaContact Nothing Nothing

getViaGroupMember :: DB.Connection -> VersionRange -> User -> Contact -> IO (Maybe (GroupInfo, GroupMember))
getViaGroupMember db vr User {userId, userContactId} Contact {contactId} =
  maybeFirstRow toGroupAndMember $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category,
          mu.member_status, mu.show_messages, mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
          -- via GroupMember
          m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category, m.member_status, m.show_messages,
          m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
        FROM group_members m
        JOIN contacts ct ON ct.contact_id = m.contact_id
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        JOIN groups g ON g.group_id = m.group_id AND g.group_id = ct.via_group
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members mu ON g.group_id = mu.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.user_id = ? AND cc.group_member_id = m.group_member_id
        )
        WHERE ct.user_id = ? AND ct.contact_id = ? AND mu.contact_id = ? AND ct.deleted = 0
      |]
      (userId, userId, contactId, userContactId)
  where
    toGroupAndMember :: (GroupInfoRow :. GroupMemberRow :. MaybeConnectionRow) -> (GroupInfo, GroupMember)
    toGroupAndMember (groupInfoRow :. memberRow :. connRow) =
      let groupInfo = toGroupInfo vr userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})

getViaGroupContact :: DB.Connection -> User -> GroupMember -> IO (Maybe Contact)
getViaGroupContact db user@User {userId} GroupMember {groupMemberId} = do
  contactId_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT ct.contact_id
          FROM group_members m
          JOIN groups g ON g.group_id = m.group_id
          JOIN contacts ct ON ct.contact_id = m.contact_id AND ct.via_group = g.group_id
          WHERE m.user_id = ? AND m.group_member_id = ? AND ct.deleted = 0
          LIMIT 1
        |]
        (userId, groupMemberId)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getContact db user) contactId_

updateGroupProfile :: DB.Connection -> User -> GroupInfo -> GroupProfile -> ExceptT StoreError IO GroupInfo
updateGroupProfile db User {userId} g@GroupInfo {groupId, localDisplayName, groupProfile = GroupProfile {displayName}} p'@GroupProfile {displayName = newName, fullName, description, image, groupPreferences}
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
          SET display_name = ?, full_name = ?, description = ?, image = ?, preferences = ?, updated_at = ?
          WHERE group_profile_id IN (
            SELECT group_profile_id
            FROM groups
            WHERE user_id = ? AND group_id = ?
          )
        |]
        (newName, fullName, description, image, groupPreferences, currentTs, userId, groupId)
    updateGroup_ ldn currentTs = do
      DB.execute
        db
        "UPDATE groups SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND group_id = ?"
        (ldn, currentTs, userId, groupId)
      DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (localDisplayName, userId)

getGroupInfo :: DB.Connection -> VersionRange -> User -> Int64 -> ExceptT StoreError IO GroupInfo
getGroupInfo db vr User {userId, userContactId} groupId =
  ExceptT . firstRow (toGroupInfo vr userContactId) (SEGroupNotFound groupId) $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category,
          mu.member_status, mu.show_messages, mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        WHERE g.group_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (groupId, userId, userContactId)

getGroupInfoByUserContactLinkConnReq :: DB.Connection -> VersionRange -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe GroupInfo)
getGroupInfoByUserContactLinkConnReq db vr user@User {userId} (cReqSchema1, cReqSchema2) = do
  groupId_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT group_id
          FROM user_contact_links
          WHERE user_id = ? AND conn_req_contact IN (?,?)
        |]
        (userId, cReqSchema1, cReqSchema2)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getGroupInfo db vr user) groupId_

getGroupInfoByGroupLinkHash :: DB.Connection -> VersionRange -> User -> (ConnReqUriHash, ConnReqUriHash) -> IO (Maybe GroupInfo)
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
            AND mu.contact_id = ? AND mu.member_status NOT IN (?,?,?)
          LIMIT 1
        |]
        (userId, groupLinkHash1, groupLinkHash2, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getGroupInfo db vr user) groupId_

getGroupIdByName :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO GroupId
getGroupIdByName db User {userId} gName =
  ExceptT . firstRow fromOnly (SEGroupNotFoundByName gName) $
    DB.query db "SELECT group_id FROM groups WHERE user_id = ? AND local_display_name = ?" (userId, gName)

getGroupMemberIdByName :: DB.Connection -> User -> GroupId -> ContactName -> ExceptT StoreError IO GroupMemberId
getGroupMemberIdByName db User {userId} groupId groupMemberName =
  ExceptT . firstRow fromOnly (SEGroupMemberNameNotFound groupId groupMemberName) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND local_display_name = ?" (userId, groupId, groupMemberName)

getActiveMembersByName :: DB.Connection -> VersionRange -> User -> ContactName -> ExceptT StoreError IO [(GroupInfo, GroupMember)]
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
    groupMember <- getGroupMember db user groupId groupMemberId
    pure (groupInfo, groupMember)
  pure $ sortOn (Down . ts . fst) possibleMembers
  where
    ts GroupInfo {chatTs, updatedAt} = fromMaybe updatedAt chatTs

getMatchingContacts :: DB.Connection -> User -> Contact -> IO [Contact]
getMatchingContacts db user@User {userId} Contact {contactId, profile = LocalProfile {displayName, fullName, image}} = do
  contactIds <-
    map fromOnly <$> case image of
      Just img -> DB.query db (q <> " AND p.image = ?") (userId, contactId, CSActive, displayName, fullName, img)
      Nothing -> DB.query db (q <> " AND p.image is NULL") (userId, contactId, CSActive, displayName, fullName)
  rights <$> mapM (runExceptT . getContact db user) contactIds
  where
    -- this query is different from one in getMatchingMemberContacts
    -- it checks that it's not the same contact
    q =
      [sql|
        SELECT ct.contact_id
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        WHERE ct.user_id = ? AND ct.contact_id != ?
          AND ct.contact_status = ? AND ct.deleted = 0
          AND p.display_name = ? AND p.full_name = ?
      |]

getMatchingMembers :: DB.Connection -> User -> Contact -> IO [GroupMember]
getMatchingMembers db user@User {userId} Contact {profile = LocalProfile {displayName, fullName, image}} = do
  memberIds <-
    map fromOnly <$> case image of
      Just img -> DB.query db (q <> " AND p.image = ?") (userId, GCUserMember, displayName, fullName, img)
      Nothing -> DB.query db (q <> " AND p.image is NULL") (userId, GCUserMember, displayName, fullName)
  filter memberCurrent . rights <$> mapM (runExceptT . getGroupMemberById db user) memberIds
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
      |]

getMatchingMemberContacts :: DB.Connection -> User -> GroupMember -> IO [Contact]
getMatchingMemberContacts _ _ GroupMember {memberContactId = Just _} = pure []
getMatchingMemberContacts db user@User {userId} GroupMember {memberProfile = LocalProfile {displayName, fullName, image}} = do
  contactIds <-
    map fromOnly <$> case image of
      Just img -> DB.query db (q <> " AND p.image = ?") (userId, CSActive, displayName, fullName, img)
      Nothing -> DB.query db (q <> " AND p.image is NULL") (userId, CSActive, displayName, fullName)
  rights <$> mapM (runExceptT . getContact db user) contactIds
  where
    q =
      [sql|
        SELECT ct.contact_id
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        WHERE ct.user_id = ?
          AND ct.contact_status = ? AND ct.deleted = 0
          AND p.display_name = ? AND p.full_name = ?
      |]

createSentProbe :: DB.Connection -> TVar ChaChaDRG -> UserId -> ContactOrMember -> ExceptT StoreError IO (Probe, Int64)
createSentProbe db gVar userId to =
  createWithRandomBytes 32 gVar $ \probe -> do
    currentTs <- getCurrentTime
    let (ctId, gmId) = contactOrMemberIds to
    DB.execute
      db
      "INSERT INTO sent_probes (contact_id, group_member_id, probe, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (ctId, gmId, probe, userId, currentTs, currentTs)
    (Probe probe,) <$> insertedRowId db

createSentProbeHash :: DB.Connection -> UserId -> Int64 -> ContactOrMember -> IO ()
createSentProbeHash db userId probeId to = do
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds to
  DB.execute
    db
    "INSERT INTO sent_probe_hashes (sent_probe_id, contact_id, group_member_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (probeId, ctId, gmId, userId, currentTs, currentTs)

matchReceivedProbe :: DB.Connection -> User -> ContactOrMember -> Probe -> IO [ContactOrMember]
matchReceivedProbe db user@User {userId} from (Probe probe) = do
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
      (userId, probeHash)
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds from
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, group_member_id, probe, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (ctId, gmId, probe, probeHash, userId, currentTs, currentTs)
  let cgmIds' = filterFirstContactId cgmIds
  catMaybes <$> mapM (getContactOrMember_ db user) cgmIds'
  where
    filterFirstContactId :: [(Maybe ContactId, Maybe GroupId, Maybe GroupMemberId)] -> [(Maybe ContactId, Maybe GroupId, Maybe GroupMemberId)]
    filterFirstContactId cgmIds = do
      let (ctIds, memIds) = partition (\(ctId, _, _) -> isJust ctId) cgmIds
          ctIds' = case ctIds of
            [] -> []
            (x : _) -> [x]
      ctIds' <> memIds

matchReceivedProbeHash :: DB.Connection -> User -> ContactOrMember -> ProbeHash -> IO (Maybe (ContactOrMember, Probe))
matchReceivedProbeHash db user@User {userId} from (ProbeHash probeHash) = do
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
        (userId, probeHash)
  currentTs <- getCurrentTime
  let (ctId, gmId) = contactOrMemberIds from
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, group_member_id, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (ctId, gmId, probeHash, userId, currentTs, currentTs)
  pure probeIds $>>= \(Only probe :. cgmIds) -> (,Probe probe) <$$> getContactOrMember_ db user cgmIds

matchSentProbe :: DB.Connection -> User -> ContactOrMember -> Probe -> IO (Maybe ContactOrMember)
matchSentProbe db user@User {userId} _from (Probe probe) = do
  cgmIds $>>= getContactOrMember_ db user
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
          (userId, probe, ctId, gmId)

getContactOrMember_ :: DB.Connection -> User -> (Maybe ContactId, Maybe GroupId, Maybe GroupMemberId) -> IO (Maybe ContactOrMember)
getContactOrMember_ db user ids =
  fmap eitherToMaybe . runExceptT $ case ids of
    (Just ctId, _, _) -> COMContact <$> getContact db user ctId
    (_, Just gId, Just gmId) -> COMGroupMember <$> getGroupMember db user gId gmId
    _ -> throwError $ SEInternalError ""

-- if requested merge direction is overruled (toFromContacts), keepLDN is kept
mergeContactRecords :: DB.Connection -> User -> Contact -> Contact -> ExceptT StoreError IO Contact
mergeContactRecords db user@User {userId} to@Contact {localDisplayName = keepLDN} from = do
  let (toCt, fromCt) = toFromContacts to from
      Contact {contactId = toContactId, localDisplayName = toLDN} = toCt
      Contact {contactId = fromContactId, localDisplayName = fromLDN} = fromCt
  liftIO $ do
    currentTs <- getCurrentTime
    -- next query fixes incorrect unused contacts deletion
    when (contactDirect toCt && not (contactUsed toCt)) $
      DB.execute
        db
        "UPDATE contacts SET contact_used = 1, updated_at = ? WHERE user_id = ? AND contact_id = ?"
        (currentTs, userId, toContactId)
    DB.execute
      db
      "UPDATE connections SET contact_id = ?, updated_at = ? WHERE contact_id = ? AND user_id = ?"
      (toContactId, currentTs, fromContactId, userId)
    DB.execute
      db
      "UPDATE connections SET via_contact = ?, updated_at = ? WHERE via_contact = ? AND user_id = ?"
      (toContactId, currentTs, fromContactId, userId)
    DB.execute
      db
      "UPDATE group_members SET invited_by = ?, updated_at = ? WHERE invited_by = ? AND user_id = ?"
      (toContactId, currentTs, fromContactId, userId)
    DB.execute
      db
      "UPDATE chat_items SET contact_id = ?, updated_at = ? WHERE contact_id = ? AND user_id = ?"
      (toContactId, currentTs, fromContactId, userId)
    DB.executeNamed
      db
      [sql|
        UPDATE group_members
        SET contact_id = :to_contact_id,
            local_display_name = (SELECT local_display_name FROM contacts WHERE contact_id = :to_contact_id),
            contact_profile_id = (SELECT contact_profile_id FROM contacts WHERE contact_id = :to_contact_id),
            updated_at = :updated_at
        WHERE contact_id = :from_contact_id
          AND user_id = :user_id
      |]
      [ ":to_contact_id" := toContactId,
        ":from_contact_id" := fromContactId,
        ":user_id" := userId,
        ":updated_at" := currentTs
      ]
    deleteContactProfile_ db userId fromContactId
    DB.execute db "DELETE FROM contacts WHERE contact_id = ? AND user_id = ?" (fromContactId, userId)
    deleteUnusedDisplayName_ db userId fromLDN
    when (keepLDN /= toLDN && keepLDN == fromLDN) $
      DB.execute
        db
        [sql|
          UPDATE display_names
          SET local_display_name = ?, updated_at = ?
          WHERE user_id = ? AND local_display_name = ?
        |]
        (keepLDN, currentTs, userId, toLDN)
  getContact db user toContactId
  where
    toFromContacts :: Contact -> Contact -> (Contact, Contact)
    toFromContacts c1 c2
      | d1 && not d2 = (c1, c2)
      | d2 && not d1 = (c2, c1)
      | ctCreatedAt c1 <= ctCreatedAt c2 = (c1, c2)
      | otherwise = (c2, c1)
      where
        d1 = directOrUsed c1
        d2 = directOrUsed c2
        ctCreatedAt Contact {createdAt} = createdAt

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

associateContactWithMemberRecord :: DB.Connection -> User -> GroupMember -> Contact -> ExceptT StoreError IO Contact
associateContactWithMemberRecord
  db
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
    getContact db user contactId

deleteUnusedDisplayName_ :: DB.Connection -> UserId -> ContactName -> IO ()
deleteUnusedDisplayName_ db userId localDisplayName =
  DB.executeNamed
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = :user_id AND local_display_name = :local_display_name
        AND 1 NOT IN (
          SELECT 1 FROM users
          WHERE local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contacts
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM groups
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM user_contact_links
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = :user_id AND local_display_name = :local_display_name LIMIT 1
        )
    |]
    [":user_id" := userId, ":local_display_name" := localDisplayName]

deleteOldProbes :: DB.Connection -> UTCTime -> IO ()
deleteOldProbes db createdAtCutoff = do
  DB.execute db "DELETE FROM sent_probes WHERE created_at <= ?" (Only createdAtCutoff)
  DB.execute db "DELETE FROM sent_probe_hashes WHERE created_at <= ?" (Only createdAtCutoff)
  DB.execute db "DELETE FROM received_probes WHERE created_at <= ?" (Only createdAtCutoff)

updateGroupSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateGroupSettings db User {userId} groupId ChatSettings {enableNtfs, sendRcpts, favorite} =
  DB.execute db "UPDATE groups SET enable_ntfs = ?, send_rcpts = ?, favorite = ? WHERE user_id = ? AND group_id = ?" (enableNtfs, sendRcpts, favorite, userId, groupId)

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
    (showMessages, currentTs, userId, gId, gMemberId)

getXGrpMemIntroContDirect :: DB.Connection -> User -> Contact -> IO (Maybe (Int64, XGrpMemIntroCont))
getXGrpMemIntroContDirect db User {userId} Contact {contactId} = do
  fmap join . maybeFirstRow toCont $
    DB.query
      db
      [sql|
        SELECT ch.connection_id, g.group_id, m.group_member_id, m.member_id, c.conn_req_inv
        FROM contacts ct
        JOIN group_members m ON m.contact_id = ct.contact_id
        LEFT JOIN connections c ON c.connection_id = (
          SELECT MAX(cc.connection_id)
          FROM connections cc
          WHERE cc.group_member_id = m.group_member_id
        )
        JOIN groups g ON g.group_id = m.group_id AND g.group_id = ct.via_group
        JOIN group_members mh ON mh.group_id = g.group_id
        LEFT JOIN connections ch ON ch.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.user_id = ? AND cc.group_member_id = mh.group_member_id
        )
        WHERE ct.user_id = ? AND ct.contact_id = ? AND ct.deleted = 0 AND mh.member_category = ?
      |]
      (userId, userId, contactId, GCHostMember)
  where
    toCont :: (Int64, GroupId, GroupMemberId, MemberId, Maybe ConnReqInvitation) -> Maybe (Int64, XGrpMemIntroCont)
    toCont (hostConnId, groupId, groupMemberId, memberId, connReq_) = case connReq_ of
      Just groupConnReq -> Just (hostConnId, XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq})
      _ -> Nothing

getXGrpMemIntroContGroup :: DB.Connection -> User -> GroupMember -> IO (Maybe (Int64, ConnReqInvitation))
getXGrpMemIntroContGroup db User {userId} GroupMember {groupMemberId} = do
  fmap join . maybeFirstRow toCont $
    DB.query
      db
      [sql|
        SELECT ch.connection_id, c.conn_req_inv
        FROM group_members m
        JOIN contacts ct ON ct.contact_id = m.contact_id
        LEFT JOIN connections c ON c.connection_id = (
          SELECT MAX(cc.connection_id)
          FROM connections cc
          WHERE cc.contact_id = ct.contact_id
        )
        JOIN groups g ON g.group_id = m.group_id AND g.group_id = ct.via_group
        JOIN group_members mh ON mh.group_id = g.group_id
        LEFT JOIN connections ch ON ch.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.user_id = ? AND cc.group_member_id = mh.group_member_id
        )
        WHERE m.user_id = ? AND m.group_member_id = ? AND mh.member_category = ? AND ct.deleted = 0
      |]
      (userId, userId, groupMemberId, GCHostMember)
  where
    toCont :: (Int64, Maybe ConnReqInvitation) -> Maybe (Int64, ConnReqInvitation)
    toCont (hostConnId, connReq_) = case connReq_ of
      Just connReq -> Just (hostConnId, connReq)
      _ -> Nothing

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
  Connection {connLevel, peerChatVRange = peerChatVRange@(JVersionRange (VersionRange minV maxV))}
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
      ( (userId, localDisplayName, memberContactProfileId, True, userPreferences, True)
          :. (groupMemberId, False, currentTs, currentTs, currentTs)
      )
    contactId <- insertedRowId db
    DB.execute
      db
      "UPDATE group_members SET contact_id = ?, updated_at = ? WHERE group_member_id = ?"
      (contactId, currentTs, groupMemberId)
    DB.execute
      db
      [sql|
        INSERT INTO connections (
          user_id, agent_conn_id, conn_req_inv, conn_level, conn_status, conn_type, contact_conn_initiated, contact_id, custom_user_profile_id,
          peer_chat_min_version, peer_chat_max_version, created_at, updated_at, to_subscribe
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, acId, cReq, connLevel, ConnNew, ConnContact, True, contactId, customUserProfileId)
          :. (minV, maxV, currentTs, currentTs, subMode == SMOnlyCreate)
      )
    connId <- insertedRowId db
    let ctConn = Connection {connId, agentConnId = AgentConnId acId, peerChatVRange, connType = ConnContact, contactConnInitiated = True, entityId = Just contactId, viaContact = Nothing, viaUserContactLink = Nothing, viaGroupLink = False, groupLinkId = Nothing, customUserProfileId, connLevel, connStatus = ConnNew, localAlias = "", createdAt = currentTs, connectionCode = Nothing, authErrCounter = 0}
        mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito ctConn
    pure Contact {contactId, localDisplayName, profile = memberProfile, activeConn = Just ctConn, viaGroup = Nothing, contactUsed = True, contactStatus = CSActive, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs, contactGroupMemberId = Just groupMemberId, contactGrpInvSent = False}

getMemberContact :: DB.Connection -> VersionRange -> User -> ContactId -> ExceptT StoreError IO (GroupInfo, GroupMember, Contact, ConnReqInvitation)
getMemberContact db vr user contactId = do
  ct <- getContact db user contactId
  let Contact {contactGroupMemberId, activeConn} = ct
  case (activeConn, contactGroupMemberId) of
    (Just Connection {connId}, Just groupMemberId) -> do
      cReq <- getConnReqInv db connId
      m@GroupMember {groupId} <- getGroupMemberById db user groupMemberId
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
    (xGrpDirectInvSent, currentTs, contactId)

createMemberContactInvited :: DB.Connection -> User -> (CommandId, ConnId) -> GroupInfo -> GroupMember -> Connection -> SubscriptionMode -> IO (Contact, GroupMember)
createMemberContactInvited
  db
  user@User {userId, profile = LocalProfile {preferences}}
  connIds
  gInfo
  m@GroupMember {groupMemberId, localDisplayName = memberLDN, memberProfile, memberContactProfileId}
  mConn
  subMode = do
    currentTs <- liftIO getCurrentTime
    let userPreferences = fromMaybe emptyChatPrefs $ incognitoMembershipProfile gInfo >> preferences
    contactId <- createContactUpdateMember currentTs userPreferences
    ctConn <- createMemberContactConn_ db user connIds gInfo mConn contactId subMode
    let mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito ctConn
        mCt' = Contact {contactId, localDisplayName = memberLDN, profile = memberProfile, activeConn = Just ctConn, viaGroup = Nothing, contactUsed = True, contactStatus = CSActive, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs, contactGroupMemberId = Nothing, contactGrpInvSent = False}
        m' = m {memberContactId = Just contactId}
    pure (mCt', m')
    where
      createContactUpdateMember :: UTCTime -> Preferences -> IO ContactId
      createContactUpdateMember currentTs userPreferences = do
        DB.execute
          db
          [sql|
            INSERT INTO contacts (
              user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, contact_used,
              created_at, updated_at, chat_ts
            ) VALUES (?,?,?,?,?,?,?,?,?)
          |]
          ( (userId, memberLDN, memberContactProfileId, True, userPreferences, True)
              :. (currentTs, currentTs, currentTs)
          )
        contactId <- insertedRowId db
        DB.execute
          db
          "UPDATE group_members SET contact_id = ?, updated_at = ? WHERE group_member_id = ?"
          (contactId, currentTs, groupMemberId)
        pure contactId

updateMemberContactInvited :: DB.Connection -> User -> (CommandId, ConnId) -> GroupInfo -> Connection -> Contact -> SubscriptionMode -> ExceptT StoreError IO Contact
updateMemberContactInvited _ _ _ _ _ Contact {localDisplayName, activeConn = Nothing} _ = throwError $ SEContactNotReady localDisplayName
updateMemberContactInvited db user connIds gInfo mConn ct@Contact {contactId, activeConn = Just oldContactConn} subMode = liftIO $ do
  updateConnectionStatus db oldContactConn ConnDeleted
  activeConn' <- createMemberContactConn_ db user connIds gInfo mConn contactId subMode
  ct' <- updateContactStatus db user ct CSActive
  ct'' <- resetMemberContactFields db ct'
  pure (ct'' :: Contact) {activeConn = Just activeConn'}

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

createMemberContactConn_ :: DB.Connection -> User -> (CommandId, ConnId) -> GroupInfo -> Connection -> ContactId -> SubscriptionMode -> IO Connection
createMemberContactConn_
  db
  user@User {userId}
  (cmdId, acId)
  gInfo
  _memberConn@Connection {connLevel, peerChatVRange = peerChatVRange@(JVersionRange (VersionRange minV maxV))}
  contactId
  subMode = do
    currentTs <- liftIO getCurrentTime
    let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
    DB.execute
      db
      [sql|
        INSERT INTO connections (
          user_id, agent_conn_id, conn_level, conn_status, conn_type, contact_id, custom_user_profile_id,
          peer_chat_min_version, peer_chat_max_version, created_at, updated_at, to_subscribe
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, acId, connLevel, ConnJoined, ConnContact, contactId, customUserProfileId)
          :. (minV, maxV, currentTs, currentTs, subMode == SMOnlyCreate)
      )
    connId <- insertedRowId db
    setCommandConnId db user cmdId connId
    pure Connection {connId, agentConnId = AgentConnId acId, peerChatVRange, connType = ConnContact, contactConnInitiated = False, entityId = Just contactId, viaContact = Nothing, viaUserContactLink = Nothing, viaGroupLink = False, groupLinkId = Nothing, customUserProfileId, connLevel, connStatus = ConnJoined, localAlias = "", createdAt = currentTs, connectionCode = Nothing, authErrCounter = 0}

updateMemberProfile :: DB.Connection -> User -> GroupMember -> Profile -> ExceptT StoreError IO GroupMember
updateMemberProfile db User {userId} m p'
  | displayName == newName = do
      liftIO $ updateContactProfile_ db userId profileId p'
      pure m {memberProfile = profile}
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateContactProfile_' db userId profileId p' currentTs
        DB.execute
          db
          "UPDATE group_members SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND group_member_id = ?"
          (ldn, currentTs, userId, groupMemberId)
        DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (localDisplayName, userId)
        pure $ Right m {localDisplayName = ldn, memberProfile = profile}
  where
    GroupMember {groupMemberId, localDisplayName, memberProfile = LocalProfile {profileId, displayName, localAlias}} = m
    Profile {displayName = newName} = p'
    profile = toLocalProfile profileId p' localAlias

getXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> ExceptT StoreError IO Bool
getXGrpLinkMemReceived db mId =
  ExceptT . firstRow fromOnly (SEGroupMemberNotFound mId) $
    DB.query db "SELECT xgrplinkmem_received FROM group_members WHERE group_member_id = ?" (Only mId)

setXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> Bool -> IO ()
setXGrpLinkMemReceived db mId xGrpLinkMemReceived = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE group_members SET xgrplinkmem_received = ?, updated_at = ? WHERE group_member_id = ?"
    (xGrpLinkMemReceived, currentTs, mId)
