{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

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
    setGroupInvitationChatItemId,
    getGroup,
    getGroupInfo,
    updateGroupProfile,
    getGroupIdByName,
    getGroupMemberIdByName,
    getGroupInfoByName,
    getGroupMember,
    getGroupMemberById,
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
    createIntroReMember,
    createIntroToMemberContact,
    saveMemberInvitation,
    getViaGroupMember,
    getViaGroupContact,
    getMatchingContacts,
    createSentProbe,
    createSentProbeHash,
    deleteSentProbe,
    matchReceivedProbe,
    matchReceivedProbeHash,
    matchSentProbe,
    mergeContactRecords,
    updateGroupSettings,
    getXGrpMemIntroContDirect,
    getXGrpMemIntroContGroup,
    getHostConnId,
  )
where

import Control.Monad.Except
import Crypto.Random (ChaChaDRG)
import Data.Either (rights)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), Query (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Messages
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ConnId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Protocol (SubscriptionMode)
import Simplex.Messaging.Util (eitherToMaybe)
import Simplex.Messaging.Version
import UnliftIO.STM

type GroupInfoRow = (Int64, GroupName, GroupName, Text, Maybe Text, Maybe ImageData, Maybe ProfileId, Maybe Bool, Maybe Bool, Bool, Maybe GroupPreferences) :. (UTCTime, UTCTime, Maybe UTCTime) :. GroupMemberRow

type GroupMemberRow = ((Int64, Int64, MemberId, GroupMemberRole, GroupMemberCategory, GroupMemberStatus) :. (Maybe Int64, ContactName, Maybe ContactId, ProfileId, ProfileId, ContactName, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Maybe Preferences))

type MaybeGroupMemberRow = ((Maybe Int64, Maybe Int64, Maybe MemberId, Maybe GroupMemberRole, Maybe GroupMemberCategory, Maybe GroupMemberStatus) :. (Maybe Int64, Maybe ContactName, Maybe ContactId, Maybe ProfileId, Maybe ProfileId, Maybe ContactName, Maybe Text, Maybe ImageData, Maybe ConnReqContact, Maybe LocalAlias, Maybe Preferences))

toGroupInfo :: Int64 -> GroupInfoRow -> GroupInfo
toGroupInfo userContactId ((groupId, localDisplayName, displayName, fullName, description, image, hostConnCustomUserProfileId, enableNtfs_, sendRcpts, favorite, groupPreferences) :. (createdAt, updatedAt, chatTs) :. userMemberRow) =
  let membership = toGroupMember userContactId userMemberRow
      chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_, sendRcpts, favorite}
      fullGroupPreferences = mergeGroupPreferences groupPreferences
      groupProfile = GroupProfile {displayName, fullName, description, image, groupPreferences}
   in GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId, chatSettings, createdAt, updatedAt, chatTs}

toGroupMember :: Int64 -> GroupMemberRow -> GroupMember
toGroupMember userContactId ((groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus) :. (invitedById, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, contactLink, localAlias, preferences)) =
  let memberProfile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}
      invitedBy = toInvitedBy userContactId invitedById
      activeConn = Nothing
   in GroupMember {..}

toMaybeGroupMember :: Int64 -> MaybeGroupMemberRow -> Maybe GroupMember
toMaybeGroupMember userContactId ((Just groupMemberId, Just groupId, Just memberId, Just memberRole, Just memberCategory, Just memberStatus) :. (invitedById, Just localDisplayName, memberContactId, Just memberContactProfileId, Just profileId, Just displayName, Just fullName, image, contactLink, Just localAlias, contactPreferences)) =
  Just $ toGroupMember userContactId ((groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus) :. (invitedById, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, contactLink, localAlias, contactPreferences))
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
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
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

getGroupAndMember :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (GroupInfo, GroupMember)
getGroupAndMember db User {userId, userContactId} groupMemberId =
  ExceptT . firstRow toGroupAndMember (SEInternalError "referenced group member not found") $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
          -- from GroupMember
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
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
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})

-- | creates completely new group with a single member - the current user
createNewGroup :: DB.Connection -> TVar ChaChaDRG -> User -> GroupProfile -> ExceptT StoreError IO GroupInfo
createNewGroup db gVar user@User {userId} groupProfile = ExceptT $ do
  let GroupProfile {displayName, fullName, description, image, groupPreferences} = groupProfile
      fullGroupPreferences = mergeGroupPreferences groupPreferences
  currentTs <- getCurrentTime
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
    membership <- createContactMemberInv_ db user groupId user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser Nothing currentTs
    let chatSettings = ChatSettings {enableNtfs = True, sendRcpts = Nothing, favorite = False}
    pure GroupInfo {groupId, localDisplayName = ldn, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = Nothing, chatSettings, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs}

-- | creates a new group record for the group the current user was invited to, or returns an existing one
createGroupInvitation :: DB.Connection -> User -> Contact -> GroupInvitation -> Maybe ProfileId -> ExceptT StoreError IO (GroupInfo, GroupMemberId)
createGroupInvitation db user@User {userId} contact@Contact {contactId, activeConn = Connection {customUserProfileId}} GroupInvitation {fromMember, invitedMember, connRequest, groupProfile} incognitoProfileId = do
  liftIO getInvitationGroupId_ >>= \case
    Nothing -> createGroupInvitation_
    Just gId -> do
      gInfo@GroupInfo {membership, groupProfile = p'} <- getGroupInfo db user gId
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
          GroupMember {groupMemberId} <- createContactMemberInv_ db user groupId contact fromMember GCHostMember GSMemInvited IBUnknown Nothing currentTs
          membership <- createContactMemberInv_ db user groupId user invitedMember GCUserMember GSMemInvited (IBContact contactId) incognitoProfileId currentTs
          let chatSettings = ChatSettings {enableNtfs = True, sendRcpts = Nothing, favorite = False}
          pure (GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = customUserProfileId, chatSettings, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs}, groupMemberId)

getHostMemberId_ :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO GroupMemberId
getHostMemberId_ db User {userId} groupId =
  ExceptT . firstRow fromOnly (SEHostMemberIdNotFound groupId) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND member_category = ?" (userId, groupId, GCHostMember)

createContactMemberInv_ :: IsContact a => DB.Connection -> User -> GroupId -> a -> MemberIdRole -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> Maybe ProfileId -> UTCTime -> ExceptT StoreError IO GroupMember
createContactMemberInv_ db User {userId, userContactId} groupId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy incognitoProfileId createdAt = do
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
        invitedBy,
        localDisplayName,
        memberProfile,
        memberContactId = Just $ contactId' userOrContact,
        memberContactProfileId = localProfileId (profile' userOrContact),
        activeConn = Nothing
      }
  where
    insertMember_ :: IO ContactName
    insertMember_ = do
      let localDisplayName = localDisplayName' userOrContact
      DB.execute
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by,
              user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ( (groupId, memberId, memberRole, memberCategory, memberStatus, fromInvitedBy userContactId invitedBy)
            :. (userId, localDisplayName' userOrContact, contactId' userOrContact, localProfileId $ profile' userOrContact, createdAt, createdAt)
        )
      pure localDisplayName
    insertMemberIncognitoProfile_ :: ContactName -> ProfileId -> ExceptT StoreError IO ContactName
    insertMemberIncognitoProfile_ incognitoDisplayName customUserProfileId = ExceptT $
      withLocalDisplayName db userId incognitoDisplayName $ \incognitoLdn -> do
        DB.execute
          db
          [sql|
            INSERT INTO group_members
              ( group_id, member_id, member_role, member_category, member_status, invited_by,
                user_id, local_display_name, contact_id, contact_profile_id, member_profile_id, created_at, updated_at)
            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
          |]
          ( (groupId, memberId, memberRole, memberCategory, memberStatus, fromInvitedBy userContactId invitedBy)
              :. (userId, incognitoLdn, contactId' userOrContact, localProfileId $ profile' userOrContact, customUserProfileId, createdAt, createdAt)
          )
        pure $ Right incognitoLdn

setGroupInvitationChatItemId :: DB.Connection -> User -> GroupId -> ChatItemId -> IO ()
setGroupInvitationChatItemId db User {userId} groupId chatItemId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE groups SET chat_item_id = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (chatItemId, currentTs, userId, groupId)

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroup :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO Group
getGroup db user groupId = do
  gInfo <- getGroupInfo db user groupId
  members <- liftIO $ getGroupMembers db user gInfo
  pure $ Group gInfo members

deleteGroupConnectionsAndFiles :: DB.Connection -> User -> GroupInfo -> [GroupMember] -> IO ()
deleteGroupConnectionsAndFiles db User {userId} GroupInfo {groupId} members = do
  forM_ members $ \m -> DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId' m)
  DB.execute db "DELETE FROM files WHERE user_id = ? AND group_id = ?" (userId, groupId)

deleteGroupItemsAndMembers :: DB.Connection -> User -> GroupInfo -> [GroupMember] -> IO ()
deleteGroupItemsAndMembers db user@User {userId} GroupInfo {groupId} members = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)
  void $ runExceptT cleanupHostGroupLinkConn_ -- to allow repeat connection via the same group link if one was used
  DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_id = ?" (userId, groupId)
  forM_ members $ \m@GroupMember {memberProfile = LocalProfile {profileId}} -> do
    cleanupMemberProfileAndName_ db user m
    when (memberIncognito m) $ deleteUnusedIncognitoProfileById_ db user profileId
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
deleteGroup db user@User {userId} GroupInfo {groupId, localDisplayName, membership = membership@GroupMember {memberProfile = LocalProfile {profileId}}} = do
  deleteGroupProfile_ db userId groupId
  DB.execute db "DELETE FROM groups WHERE user_id = ? AND group_id = ?" (userId, groupId)
  DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
  when (memberIncognito membership) $ deleteUnusedIncognitoProfileById_ db user profileId

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

getUserGroups :: DB.Connection -> User -> IO [Group]
getUserGroups db user@User {userId} = do
  groupIds <- map fromOnly <$> DB.query db "SELECT group_id FROM groups WHERE user_id = ?" (Only userId)
  rights <$> mapM (runExceptT . getGroup db user) groupIds

getUserGroupDetails :: DB.Connection -> User -> Maybe ContactId -> Maybe String -> IO [GroupInfo]
getUserGroupDetails db User {userId, userContactId} _contactId_ search_ =
  map (toGroupInfo userContactId)
    <$> DB.query
      db
      [sql|
        SELECT g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          mu.group_member_id, g.group_id, mu.member_id, mu.member_role, mu.member_category, mu.member_status,
          mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id, pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences
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

getUserGroupsWithSummary :: DB.Connection -> User -> Maybe ContactId -> Maybe String -> IO [(GroupInfo, GroupSummary)]
getUserGroupsWithSummary db user _contactId_ search_ =
  getUserGroupDetails db user _contactId_ search_
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

getGroupInfoByName :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO GroupInfo
getGroupInfoByName db user gName = do
  gId <- getGroupIdByName db user gName
  getGroupInfo db user gId

groupMemberQuery :: Query
groupMemberQuery =
  [sql|
    SELECT
      m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
      m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
      c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
      c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
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

getGroupInvitation :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO ReceivedGroupInvitation
getGroupInvitation db user groupId =
  getConnRec_ user >>= \case
    Just connRequest -> do
      groupInfo@GroupInfo {membership} <- getGroupInfo db user groupId
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

createNewContactMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupId -> Contact -> GroupMemberRole -> ConnId -> ConnReqInvitation -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createNewContactMember db gVar User {userId, userContactId} groupId Contact {contactId, localDisplayName, profile, activeConn = Connection {peerChatVRange}} memberRole agentConnId connRequest subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    member@GroupMember {groupMemberId} <- createMember_ (MemberId memId) createdAt
    void $ createMemberConnection_ db userId groupMemberId agentConnId (fromJVersionRange peerChatVRange) Nothing 0 createdAt subMode
    pure member
  where
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
            invitedBy = IBUser,
            localDisplayName,
            memberProfile = profile,
            memberContactId = Just contactId,
            memberContactProfileId = localProfileId profile,
            activeConn = Nothing
          }
      where
        insertMember_ =
          DB.execute
            db
            [sql|
              INSERT INTO group_members
                ( group_id, member_id, member_role, member_category, member_status, invited_by,
                  user_id, local_display_name, contact_id, contact_profile_id, sent_inv_queue_info, created_at, updated_at)
              VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
            |]
            ( (groupId, memberId, memberRole, GCInviteeMember, GSMemInvited, fromInvitedBy userContactId IBUser)
                :. (userId, localDisplayName, contactId, localProfileId profile, connRequest, createdAt, createdAt)
            )

createNewContactMemberAsync :: DB.Connection -> TVar ChaChaDRG -> User -> GroupId -> Contact -> GroupMemberRole -> (CommandId, ConnId) -> VersionRange -> SubscriptionMode -> ExceptT StoreError IO ()
createNewContactMemberAsync db gVar user@User {userId, userContactId} groupId Contact {contactId, localDisplayName, profile} memberRole (cmdId, agentConnId) peerChatVRange subMode =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    insertMember_ (MemberId memId) createdAt
    groupMemberId <- liftIO $ insertedRowId db
    Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId peerChatVRange Nothing 0 createdAt subMode
    setCommandConnId db user cmdId connId
  where
    insertMember_ memberId createdAt =
      DB.execute
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by,
              user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ( (groupId, memberId, memberRole, GCInviteeMember, GSMemInvited, fromInvitedBy userContactId IBUser)
            :. (userId, localDisplayName, contactId, localProfileId profile, createdAt, createdAt)
        )

getContactViaMember :: DB.Connection -> User -> GroupMember -> ExceptT StoreError IO Contact
getContactViaMember db user@User {userId} GroupMember {groupMemberId} =
  ExceptT $
    firstRow (toContact user) (SEContactNotFoundByMemberId groupMemberId) $
      DB.query
        db
        [sql|
          SELECT
            -- Contact
            ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
            cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts,
            -- Connection
            c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
            c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
            c.peer_chat_min_version, c.peer_chat_max_version
          FROM contacts ct
          JOIN contact_profiles cp ON cp.contact_profile_id = ct.contact_profile_id
          JOIN connections c ON c.connection_id = (
            SELECT max(cc.connection_id)
            FROM connections cc
            where cc.contact_id = ct.contact_id
          )
          JOIN group_members m ON m.contact_id = ct.contact_id
          WHERE ct.user_id = ? AND m.group_member_id = ? AND ct.deleted = 0
        |]
        (userId, groupMemberId)

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
createNewGroupMember :: DB.Connection -> User -> GroupInfo -> MemberInfo -> GroupMemberCategory -> GroupMemberStatus -> ExceptT StoreError IO GroupMember
createNewGroupMember db user gInfo memInfo memCategory memStatus = do
  currentTs <- liftIO getCurrentTime
  (localDisplayName, memProfileId) <- createNewMemberProfile_ db user memInfo currentTs
  let newMember =
        NewGroupMember
          { memInfo,
            memCategory,
            memStatus,
            memInvitedBy = IBUnknown,
            localDisplayName,
            memContactId = Nothing,
            memProfileId
          }
  liftIO $ createNewMember_ db user gInfo newMember currentTs

createNewMemberProfile_ :: DB.Connection -> User -> MemberInfo -> UTCTime -> ExceptT StoreError IO (Text, ProfileId)
createNewMemberProfile_ db User {userId} (MemberInfo _ _ _ Profile {displayName, fullName, image, contactLink, preferences}) createdAt =
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
    { memInfo = MemberInfo memberId memberRole _ memberProfile,
      memCategory = memberCategory,
      memStatus = memberStatus,
      memInvitedBy = invitedBy,
      localDisplayName,
      memContactId = memberContactId,
      memProfileId = memberContactProfileId
    }
  createdAt = do
    let invitedById = fromInvitedBy userContactId invitedBy
        activeConn = Nothing
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          (group_id, member_id, member_role, member_category, member_status,
           invited_by, user_id, local_display_name, contact_id, contact_profile_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      (groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, userId, localDisplayName, memberContactId, memberContactProfileId, createdAt, createdAt)
    groupMemberId <- insertedRowId db
    pure GroupMember {groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus, invitedBy, localDisplayName, memberProfile = toLocalProfile memberContactProfileId memberProfile "", memberContactId, memberContactProfileId, activeConn}

checkGroupMemberHasItems :: DB.Connection -> User -> GroupMember -> IO (Maybe ChatItemId)
checkGroupMemberHasItems db User {userId} GroupMember {groupMemberId, groupId} =
  maybeFirstRow fromOnly $ DB.query db "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND group_member_id = ? LIMIT 1" (userId, groupId, groupMemberId)

deleteGroupMember :: DB.Connection -> User -> GroupMember -> IO ()
deleteGroupMember db user@User {userId} m@GroupMember {groupMemberId, groupId, memberProfile = LocalProfile {profileId}} = do
  deleteGroupMemberConnection db user m
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ? AND group_member_id = ?" (userId, groupId, groupMemberId)
  DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId)
  cleanupMemberProfileAndName_ db user m
  when (memberIncognito m) $ deleteUnusedIncognitoProfileById_ db user profileId

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

createIntroductions :: DB.Connection -> [GroupMember] -> GroupMember -> IO [GroupMemberIntro]
createIntroductions db members toMember = do
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
            (re_group_member_id, to_group_member_id, intro_status, created_at, updated_at)
          VALUES (?,?,?,?,?)
        |]
        (groupMemberId' reMember, groupMemberId' toMember, GMIntroPending, ts, ts)
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
  intro <- getIntroduction_ db reMember toMember
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
        ":group_queue_info" := groupConnReq (introInv :: IntroInvitation),
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

getIntroduction_ :: DB.Connection -> GroupMember -> GroupMember -> ExceptT StoreError IO GroupMemberIntro
getIntroduction_ db reMember toMember = ExceptT $ do
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

createIntroReMember :: DB.Connection -> User -> GroupInfo -> GroupMember -> MemberInfo -> (CommandId, ConnId) -> Maybe (CommandId, ConnId) -> Maybe ProfileId -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createIntroReMember db user@User {userId} gInfo@GroupInfo {groupId} _host@GroupMember {memberContactId, activeConn} memInfo@(MemberInfo _ _ memberChatVRange memberProfile) (groupCmdId, groupAgentConnId) directConnIds customUserProfileId subMode = do
  let mcvr = maybe chatInitialVRange fromChatVRange memberChatVRange
      cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
  currentTs <- liftIO getCurrentTime
  newMember <- case directConnIds of
    Just (directCmdId, directAgentConnId) -> do
      Connection {connId = directConnId} <- liftIO $ createConnection_ db userId ConnContact Nothing directAgentConnId mcvr memberContactId Nothing customUserProfileId cLevel currentTs subMode
      liftIO $ setCommandConnId db user directCmdId directConnId
      (localDisplayName, contactId, memProfileId) <- createContact_ db userId directConnId memberProfile "" (Just groupId) currentTs Nothing
      pure $ NewGroupMember {memInfo, memCategory = GCPreMember, memStatus = GSMemIntroduced, memInvitedBy = IBUnknown, localDisplayName, memContactId = Just contactId, memProfileId}
    Nothing -> do
      (localDisplayName, memProfileId) <- createNewMemberProfile_ db user memInfo currentTs
      pure $ NewGroupMember {memInfo, memCategory = GCPreMember, memStatus = GSMemIntroduced, memInvitedBy = IBUnknown, localDisplayName, memContactId = Nothing, memProfileId}
  liftIO $ do
    member <- createNewMember_ db user gInfo newMember currentTs
    conn@Connection {connId = groupConnId} <- createMemberConnection_ db userId (groupMemberId' member) groupAgentConnId mcvr memberContactId cLevel currentTs subMode
    liftIO $ setCommandConnId db user groupCmdId groupConnId
    pure (member :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: DB.Connection -> User -> GroupMember -> GroupMember -> VersionRange -> (CommandId, ConnId) -> Maybe (CommandId, ConnId) -> Maybe ProfileId -> SubscriptionMode -> IO ()
createIntroToMemberContact db user@User {userId} GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} mcvr (groupCmdId, groupAgentConnId) directConnIds customUserProfileId subMode = do
  let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
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
          INSERT INTO contacts (contact_profile_id, via_group, local_display_name, user_id, created_at, updated_at)
          SELECT contact_profile_id, group_id, ?, ?, ?, ?
          FROM group_members
          WHERE group_member_id = ?
        |]
        (localDisplayName, userId, ts, ts, groupMemberId)
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

getViaGroupMember :: DB.Connection -> User -> Contact -> IO (Maybe (GroupInfo, GroupMember))
getViaGroupMember db User {userId, userContactId} Contact {contactId} =
  maybeFirstRow toGroupAndMember $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
          -- via GroupMember
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
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
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})

getViaGroupContact :: DB.Connection -> User -> GroupMember -> IO (Maybe Contact)
getViaGroupContact db user@User {userId} GroupMember {groupMemberId} =
  maybeFirstRow toContact' $
    DB.query
      db
      [sql|
        SELECT
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, ct.via_group, ct.contact_used, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
          p.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.contact_id = ct.contact_id
        )
        JOIN groups g ON g.group_id = ct.via_group
        JOIN group_members m ON m.group_id = g.group_id AND m.contact_id = ct.contact_id
        WHERE ct.user_id = ? AND m.group_member_id = ? AND ct.deleted = 0
      |]
      (userId, groupMemberId)
  where
    toContact' :: ((ContactId, ProfileId, ContactName, Text, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Maybe Int64, Bool) :. (Maybe Bool, Maybe Bool, Bool, Maybe Preferences, Preferences, UTCTime, UTCTime, Maybe UTCTime)) :. ConnectionRow -> Contact
    toContact' (((contactId, profileId, localDisplayName, displayName, fullName, image, contactLink, localAlias, viaGroup, contactUsed) :. (enableNtfs_, sendRcpts, favorite, preferences, userPreferences, createdAt, updatedAt, chatTs)) :. connRow) =
      let profile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}
          chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_, sendRcpts, favorite}
          activeConn = toConnection connRow
          mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
       in Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt, chatTs}

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

getGroupInfo :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO GroupInfo
getGroupInfo db User {userId, userContactId} groupId =
  ExceptT . firstRow (toGroupInfo userContactId) (SEGroupNotFound groupId) $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        WHERE g.group_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (groupId, userId, userContactId)

getGroupIdByName :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO GroupId
getGroupIdByName db User {userId} gName =
  ExceptT . firstRow fromOnly (SEGroupNotFoundByName gName) $
    DB.query db "SELECT group_id FROM groups WHERE user_id = ? AND local_display_name = ?" (userId, gName)

getGroupMemberIdByName :: DB.Connection -> User -> GroupId -> ContactName -> ExceptT StoreError IO GroupMemberId
getGroupMemberIdByName db User {userId} groupId groupMemberName =
  ExceptT . firstRow fromOnly (SEGroupMemberNameNotFound groupId groupMemberName) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND local_display_name = ?" (userId, groupId, groupMemberName)

getMatchingContacts :: DB.Connection -> User -> Contact -> IO [Contact]
getMatchingContacts db user@User {userId} Contact {contactId, profile = LocalProfile {displayName, fullName, image}} = do
  contactIds <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT ct.contact_id
          FROM contacts ct
          JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
          WHERE ct.user_id = ? AND ct.contact_id != ?
            AND ct.deleted = 0
            AND p.display_name = ? AND p.full_name = ?
            AND ((p.image IS NULL AND ? IS NULL) OR p.image = ?)
        |]
        (userId, contactId, displayName, fullName, image, image)
  rights <$> mapM (runExceptT . getContact db user) contactIds

createSentProbe :: DB.Connection -> TVar ChaChaDRG -> UserId -> Contact -> ExceptT StoreError IO (Probe, Int64)
createSentProbe db gVar userId _to@Contact {contactId} =
  createWithRandomBytes 32 gVar $ \probe -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO sent_probes (contact_id, probe, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (contactId, probe, userId, currentTs, currentTs)
    (Probe probe,) <$> insertedRowId db

createSentProbeHash :: DB.Connection -> UserId -> Int64 -> Contact -> IO ()
createSentProbeHash db userId probeId _to@Contact {contactId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO sent_probe_hashes (sent_probe_id, contact_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
    (probeId, contactId, userId, currentTs, currentTs)

deleteSentProbe :: DB.Connection -> UserId -> Int64 -> IO ()
deleteSentProbe db userId probeId =
  DB.execute
    db
    "DELETE FROM sent_probes WHERE user_id = ? AND sent_probe_id = ?"
    (userId, probeId)

matchReceivedProbe :: DB.Connection -> User -> Contact -> Probe -> IO (Maybe Contact)
matchReceivedProbe db user@User {userId} _from@Contact {contactId} (Probe probe) = do
  let probeHash = C.sha256Hash probe
  contactIds <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT c.contact_id
          FROM contacts c
          JOIN received_probes r ON r.contact_id = c.contact_id
          WHERE c.user_id = ? AND c.deleted = 0 AND r.probe_hash = ? AND r.probe IS NULL
        |]
        (userId, probeHash)
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, probe, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (contactId, probe, probeHash, userId, currentTs, currentTs)
  case contactIds of
    [] -> pure Nothing
    cId : _ -> eitherToMaybe <$> runExceptT (getContact db user cId)

matchReceivedProbeHash :: DB.Connection -> User -> Contact -> ProbeHash -> IO (Maybe (Contact, Probe))
matchReceivedProbeHash db user@User {userId} _from@Contact {contactId} (ProbeHash probeHash) = do
  namesAndProbes <-
    DB.query
      db
      [sql|
        SELECT c.contact_id, r.probe
        FROM contacts c
        JOIN received_probes r ON r.contact_id = c.contact_id
        WHERE c.user_id = ? AND c.deleted = 0 AND r.probe_hash = ? AND r.probe IS NOT NULL
      |]
      (userId, probeHash)
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO received_probes (contact_id, probe_hash, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
    (contactId, probeHash, userId, currentTs, currentTs)
  case namesAndProbes of
    [] -> pure Nothing
    (cId, probe) : _ ->
      either (const Nothing) (Just . (,Probe probe))
        <$> runExceptT (getContact db user cId)

matchSentProbe :: DB.Connection -> User -> Contact -> Probe -> IO (Maybe Contact)
matchSentProbe db user@User {userId} _from@Contact {contactId} (Probe probe) = do
  contactIds <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT c.contact_id
          FROM contacts c
          JOIN sent_probes s ON s.contact_id = c.contact_id
          JOIN sent_probe_hashes h ON h.sent_probe_id = s.sent_probe_id
          WHERE c.user_id = ? AND c.deleted = 0 AND s.probe = ? AND h.contact_id = ?
        |]
        (userId, probe, contactId)
  case contactIds of
    [] -> pure Nothing
    cId : _ -> eitherToMaybe <$> runExceptT (getContact db user cId)

mergeContactRecords :: DB.Connection -> UserId -> Contact -> Contact -> IO ()
mergeContactRecords db userId ct1 ct2 = do
  let (toCt, fromCt) = toFromContacts ct1 ct2
      Contact {contactId = toContactId} = toCt
      Contact {contactId = fromContactId, localDisplayName} = fromCt
  currentTs <- getCurrentTime
  -- TODO next query fixes incorrect unused contacts deletion; consider more thorough fix
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
  DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (localDisplayName, userId)
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

updateGroupSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateGroupSettings db User {userId} groupId ChatSettings {enableNtfs, sendRcpts, favorite} =
  DB.execute db "UPDATE groups SET enable_ntfs = ?, send_rcpts = ?, favorite = ? WHERE user_id = ? AND group_id = ?" (enableNtfs, sendRcpts, favorite, userId, groupId)

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
