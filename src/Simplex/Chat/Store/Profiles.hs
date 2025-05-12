{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Profiles
  ( AutoAccept (..),
    UserMsgReceiptSettings (..),
    UserContactLink (..),
    GroupLinkInfo (..),
    createUserRecord,
    createUserRecordAt,
    getUsersInfo,
    getUsers,
    setActiveUser,
    getUser,
    getUserIdByName,
    getUserByAConnId,
    getUserByASndFileId,
    getUserByARcvFileId,
    getUserByContactId,
    getUserByGroupId,
    getUserByNoteFolderId,
    getUserByFileId,
    getUserFileInfo,
    deleteUserRecord,
    updateUserPrivacy,
    updateAllContactReceipts,
    updateUserContactReceipts,
    updateUserGroupReceipts,
    updateUserProfile,
    setUserProfileContactLink,
    getUserContactProfiles,
    createUserContactLink,
    getUserAddressConnections,
    getUserContactLinks,
    deleteUserAddress,
    getUserAddress,
    getUserContactLinkById,
    getGroupLinkInfo,
    getUserContactLinkByConnReq,
    getUserContactLinkViaShortLink,
    getContactWithoutConnViaAddress,
    updateUserAddressAutoAccept,
    getProtocolServers,
    insertProtocolServer,
    getUpdateServerOperators,
    getServerOperators,
    getUserServers,
    setServerOperators,
    getCurrentUsageConditions,
    getLatestAcceptedConditions,
    setConditionsNotified,
    acceptConditions,
    setUserServers,
    setUserServers',
    createCall,
    deleteCalls,
    getCalls,
    createCommand,
    setCommandConnId,
    deleteCommand,
    updateCommandStatus,
    getCommandDataByCorrId,
    setUserUIThemes,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Aeson.TH as J
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Simplex.Chat.Call
import Simplex.Chat.Messages
import Simplex.Chat.Operators
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Env.SQLite (ServerRoles (..))
import Simplex.Messaging.Agent.Protocol (ACorrId, ConnId, ConnectionLink (..), CreatedConnLink (..), UserId)
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Protocol (BasicAuth (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), ProtocolTypeI (..), SProtocolType (..), SubscriptionMode)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

createUserRecord :: DB.Connection -> AgentUserId -> Profile -> Bool -> ExceptT StoreError IO User
createUserRecord db auId p activeUser = createUserRecordAt db auId p activeUser =<< liftIO getCurrentTime

createUserRecordAt :: DB.Connection -> AgentUserId -> Profile -> Bool -> UTCTime -> ExceptT StoreError IO User
createUserRecordAt db (AgentUserId auId) Profile {displayName, fullName, image, preferences = userPreferences} activeUser currentTs =
  checkConstraint SEDuplicateName . liftIO $ do
    when activeUser $ DB.execute_ db "UPDATE users SET active_user = 0"
    let showNtfs = True
        sendRcptsContacts = True
        sendRcptsSmallGroups = True
    order <- getNextActiveOrder db
    DB.execute
      db
      "INSERT INTO users (agent_user_id, local_display_name, active_user, active_order, contact_id, show_ntfs, send_rcpts_contacts, send_rcpts_small_groups, created_at, updated_at) VALUES (?,?,?,?,0,?,?,?,?,?)"
      (auId, displayName, BI activeUser, order, BI showNtfs, BI sendRcptsContacts, BI sendRcptsSmallGroups, currentTs, currentTs)
    userId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO display_names (local_display_name, ldn_base, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (displayName, displayName, userId, currentTs, currentTs)
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
      (displayName, fullName, image, userId, userPreferences, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, is_user, created_at, updated_at, chat_ts) VALUES (?,?,?,?,?,?,?)"
      (profileId, displayName, userId, BI True, currentTs, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure $ toUser $ (userId, auId, contactId, profileId, BI activeUser, order, displayName, fullName, image, Nothing, userPreferences) :. (BI showNtfs, BI sendRcptsContacts, BI sendRcptsSmallGroups, Nothing, Nothing, Nothing, Nothing)

-- TODO [mentions]
getUsersInfo :: DB.Connection -> IO [UserInfo]
getUsersInfo db = getUsers db >>= mapM getUserInfo
  where
    getUserInfo :: User -> IO UserInfo
    getUserInfo user@User {userId} = do
      ctCount <-
        maybeFirstRow fromOnly $
          DB.query
            db
            [sql|
              SELECT COUNT(1)
              FROM chat_items i
              JOIN contacts ct USING (contact_id)
              WHERE i.user_id = ? AND i.item_status = ? AND (ct.enable_ntfs = 1 OR ct.enable_ntfs IS NULL) AND ct.deleted = 0
            |]
            (userId, CISRcvNew)
      gCount <-
        maybeFirstRow fromOnly $
          DB.query
            db
            [sql|
              SELECT COUNT(1)
              FROM chat_items i
              JOIN groups g USING (group_id)
              WHERE i.user_id = ? AND i.item_status = ?
                AND (g.enable_ntfs = 1 OR g.enable_ntfs IS NULL OR (g.enable_ntfs = 2 AND i.user_mention = 1))
            |]
            (userId, CISRcvNew)
      pure UserInfo {user, unreadCount = fromMaybe 0 ctCount + fromMaybe 0 gCount}

getUsers :: DB.Connection -> IO [User]
getUsers db =
  map toUser <$> DB.query_ db userQuery

setActiveUser :: DB.Connection -> User -> IO User
setActiveUser db user@User {userId} = do
  DB.execute_ db "UPDATE users SET active_user = 0"
  activeOrder <- getNextActiveOrder db
  DB.execute db "UPDATE users SET active_user = 1, active_order = ? WHERE user_id = ?" (activeOrder, userId)
  pure user {activeUser = True, activeOrder}

getNextActiveOrder :: DB.Connection -> IO Int64
getNextActiveOrder db = do
  order <- fromMaybe 0 . join <$> maybeFirstRow fromOnly (DB.query_ db "SELECT max(active_order) FROM users")
  if order == maxBound
    then 0 <$ DB.execute db "UPDATE users SET active_order = active_order - ?" (Only (maxBound :: Int64))
    else pure $ order + 1

getUser :: DB.Connection -> UserId -> ExceptT StoreError IO User
getUser db userId =
  ExceptT . firstRow toUser (SEUserNotFound userId) $
    DB.query db (userQuery <> " WHERE u.user_id = ?") (Only userId)

getUserIdByName :: DB.Connection -> UserName -> ExceptT StoreError IO Int64
getUserIdByName db uName =
  ExceptT . firstRow fromOnly (SEUserNotFoundByName uName) $
    DB.query db "SELECT user_id FROM users WHERE local_display_name = ?" (Only uName)

getUserByAConnId :: DB.Connection -> AgentConnId -> IO (Maybe User)
getUserByAConnId db agentConnId =
  maybeFirstRow toUser $
    DB.query db (userQuery <> " JOIN connections c ON c.user_id = u.user_id WHERE c.agent_conn_id = ?") (Only agentConnId)

getUserByASndFileId :: DB.Connection -> AgentSndFileId -> IO (Maybe User)
getUserByASndFileId db aSndFileId =
  maybeFirstRow toUser $
    DB.query db (userQuery <> " JOIN files f ON f.user_id = u.user_id WHERE f.agent_snd_file_id = ?") (Only aSndFileId)

getUserByARcvFileId :: DB.Connection -> AgentRcvFileId -> IO (Maybe User)
getUserByARcvFileId db aRcvFileId =
  maybeFirstRow toUser $
    DB.query db (userQuery <> " JOIN files f ON f.user_id = u.user_id JOIN rcv_files r ON r.file_id = f.file_id WHERE r.agent_rcv_file_id = ?") (Only aRcvFileId)

getUserByContactId :: DB.Connection -> ContactId -> ExceptT StoreError IO User
getUserByContactId db contactId =
  ExceptT . firstRow toUser (SEUserNotFoundByContactId contactId) $
    DB.query db (userQuery <> " JOIN contacts ct ON ct.user_id = u.user_id WHERE ct.contact_id = ? AND ct.deleted = 0") (Only contactId)

getUserByGroupId :: DB.Connection -> GroupId -> ExceptT StoreError IO User
getUserByGroupId db groupId =
  ExceptT . firstRow toUser (SEUserNotFoundByGroupId groupId) $
    DB.query db (userQuery <> " JOIN groups g ON g.user_id = u.user_id WHERE g.group_id = ?") (Only groupId)

getUserByNoteFolderId :: DB.Connection -> NoteFolderId -> ExceptT StoreError IO User
getUserByNoteFolderId db contactId =
  ExceptT . firstRow toUser (SEUserNotFoundByContactId contactId) $
    DB.query db (userQuery <> " JOIN note_folders nf ON nf.user_id = u.user_id WHERE nf.note_folder_id = ?") (Only contactId)

getUserByFileId :: DB.Connection -> FileTransferId -> ExceptT StoreError IO User
getUserByFileId db fileId =
  ExceptT . firstRow toUser (SEUserNotFoundByFileId fileId) $
    DB.query db (userQuery <> " JOIN files f ON f.user_id = u.user_id WHERE f.file_id = ?") (Only fileId)

getUserFileInfo :: DB.Connection -> User -> IO [CIFileInfo]
getUserFileInfo db User {userId} =
  map toFileInfo
    <$> DB.query db (fileInfoQuery <> " WHERE i.user_id = ?") (Only userId)

deleteUserRecord :: DB.Connection -> User -> IO ()
deleteUserRecord db User {userId} =
  DB.execute db "DELETE FROM users WHERE user_id = ?" (Only userId)

updateUserPrivacy :: DB.Connection -> User -> IO ()
updateUserPrivacy db User {userId, showNtfs, viewPwdHash} =
  DB.execute
    db
    [sql|
      UPDATE users
      SET view_pwd_hash = ?, view_pwd_salt = ?, show_ntfs = ?
      WHERE user_id = ?
    |]
    (hashSalt viewPwdHash :. (BI showNtfs, userId))
  where
    hashSalt = L.unzip . fmap (\UserPwdHash {hash, salt} -> (hash, salt))

updateAllContactReceipts :: DB.Connection -> Bool -> IO ()
updateAllContactReceipts db onOff =
  DB.execute
    db
    "UPDATE users SET send_rcpts_contacts = ?, send_rcpts_small_groups = ? WHERE view_pwd_hash IS NULL"
    (BI onOff, BI onOff)

updateUserContactReceipts :: DB.Connection -> User -> UserMsgReceiptSettings -> IO ()
updateUserContactReceipts db User {userId} UserMsgReceiptSettings {enable, clearOverrides} = do
  DB.execute db "UPDATE users SET send_rcpts_contacts = ? WHERE user_id = ?" (BI enable, userId)
  when clearOverrides $ DB.execute_ db "UPDATE contacts SET send_rcpts = NULL"

updateUserGroupReceipts :: DB.Connection -> User -> UserMsgReceiptSettings -> IO ()
updateUserGroupReceipts db User {userId} UserMsgReceiptSettings {enable, clearOverrides} = do
  DB.execute db "UPDATE users SET send_rcpts_small_groups = ? WHERE user_id = ?" (BI enable, userId)
  when clearOverrides $ DB.execute_ db "UPDATE groups SET send_rcpts = NULL"

updateUserProfile :: DB.Connection -> User -> Profile -> ExceptT StoreError IO User
updateUserProfile db user p'
  | displayName == newName = liftIO $ do
      updateContactProfile_ db userId profileId p'
      currentTs <- getCurrentTime
      userMemberProfileUpdatedAt' <- updateUserMemberProfileUpdatedAt_ currentTs
      pure user {profile, fullPreferences, userMemberProfileUpdatedAt = userMemberProfileUpdatedAt'}
  | otherwise =
      checkConstraint SEDuplicateName . liftIO $ do
        currentTs <- getCurrentTime
        DB.execute db "UPDATE users SET local_display_name = ?, updated_at = ? WHERE user_id = ?" (newName, currentTs, userId)
        userMemberProfileUpdatedAt' <- updateUserMemberProfileUpdatedAt_ currentTs
        DB.execute
          db
          "INSERT INTO display_names (local_display_name, ldn_base, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
          (newName, newName, userId, currentTs, currentTs)
        updateContactProfile_' db userId profileId p' currentTs
        updateContactLDN_ db user userContactId localDisplayName newName currentTs
        pure user {localDisplayName = newName, profile, fullPreferences, userMemberProfileUpdatedAt = userMemberProfileUpdatedAt'}
  where
    updateUserMemberProfileUpdatedAt_ currentTs
      | userMemberProfileChanged = do
          DB.execute db "UPDATE users SET user_member_profile_updated_at = ? WHERE user_id = ?" (currentTs, userId)
          pure $ Just currentTs
      | otherwise = pure userMemberProfileUpdatedAt
    userMemberProfileChanged = newName /= displayName || newFullName /= fullName || newImage /= image
    User {userId, userContactId, localDisplayName, profile = LocalProfile {profileId, displayName, fullName, image, localAlias}, userMemberProfileUpdatedAt} = user
    Profile {displayName = newName, fullName = newFullName, image = newImage, preferences} = p'
    profile = toLocalProfile profileId p' localAlias
    fullPreferences = mergePreferences Nothing preferences

setUserProfileContactLink :: DB.Connection -> User -> Maybe UserContactLink -> IO User
setUserProfileContactLink db user@User {userId, profile = p@LocalProfile {profileId}} ucl_ = do
  ts <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET contact_link = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (contactLink, ts, userId, profileId)
  pure (user :: User) {profile = p {contactLink}}
  where
    -- TODO [short links] this should be replaced with short links once they are supported by all clients.
    -- Or, maybe, we want to allow both, when both are optional.
    contactLink = case ucl_ of
      Just UserContactLink {connLinkContact = CCLink cReq _} -> Just $ CLFull cReq
      _ -> Nothing

-- only used in tests
getUserContactProfiles :: DB.Connection -> User -> IO [Profile]
getUserContactProfiles db User {userId} =
  map toContactProfile
    <$> DB.query
      db
      [sql|
        SELECT display_name, full_name, image, contact_link, preferences
        FROM contact_profiles
        WHERE user_id = ?
      |]
      (Only userId)
  where
    toContactProfile :: (ContactName, Text, Maybe ImageData, Maybe ConnLinkContact, Maybe Preferences) -> Profile
    toContactProfile (displayName, fullName, image, contactLink, preferences) = Profile {displayName, fullName, image, contactLink, preferences}

createUserContactLink :: DB.Connection -> User -> ConnId -> CreatedLinkContact -> SubscriptionMode -> ExceptT StoreError IO ()
createUserContactLink db User {userId} agentConnId (CCLink cReq shortLink) subMode =
  checkConstraint SEDuplicateContactLink . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, conn_req_contact, short_link_contact, created_at, updated_at) VALUES (?,?,?,?,?)"
      (userId, cReq, shortLink, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId ConnNew initialChatVersion chatInitialVRange Nothing Nothing Nothing 0 currentTs subMode CR.PQSupportOff

getUserAddressConnections :: DB.Connection -> VersionRangeChat -> User -> ExceptT StoreError IO [Connection]
getUserAddressConnections db vr User {userId} = do
  cs <- liftIO getUserAddressConnections_
  if null cs then throwError SEUserContactLinkNotFound else pure cs
  where
    getUserAddressConnections_ :: IO [Connection]
    getUserAddressConnections_ =
      map (toConnection vr)
        <$> DB.query
          db
          [sql|
            SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
              c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id,
              c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
              c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
            FROM connections c
            JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
            WHERE c.user_id = ? AND uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
          |]
          (userId, userId)

getUserContactLinks :: DB.Connection -> VersionRangeChat -> User -> IO [(Connection, UserContact)]
getUserContactLinks db vr User {userId} =
  map toUserContactConnection
    <$> DB.query
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id,
          c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
          c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version,
          uc.user_contact_link_id, uc.conn_req_contact, uc.group_id
        FROM connections c
        JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
        WHERE c.user_id = ? AND uc.user_id = ?
      |]
      (userId, userId)
  where
    toUserContactConnection :: (ConnectionRow :. (Int64, ConnReqContact, Maybe GroupId)) -> (Connection, UserContact)
    toUserContactConnection (connRow :. (userContactLinkId, connReqContact, groupId)) = (toConnection vr connRow, UserContact {userContactLinkId, connReqContact, groupId})

deleteUserAddress :: DB.Connection -> User -> IO ()
deleteUserAddress db user@User {userId} = do
  DB.execute
    db
    [sql|
      DELETE FROM connections WHERE connection_id IN (
        SELECT connection_id
        FROM connections c
        JOIN user_contact_links uc USING (user_contact_link_id)
        WHERE uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
      )
    |]
    (Only userId)
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ?
        AND local_display_name in (
          SELECT cr.local_display_name
          FROM contact_requests cr
          JOIN user_contact_links uc USING (user_contact_link_id)
          WHERE uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
        )
        AND local_display_name NOT IN (SELECT local_display_name FROM users WHERE user_id = ?)
    |]
    (userId, userId, userId)
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT cr.contact_profile_id
        FROM contact_requests cr
        JOIN user_contact_links uc USING (user_contact_link_id)
        WHERE uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
      )
    |]
    (Only userId)
  void $ setUserProfileContactLink db user Nothing
  DB.execute db "DELETE FROM user_contact_links WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL" (Only userId)

data UserMsgReceiptSettings = UserMsgReceiptSettings
  { enable :: Bool,
    clearOverrides :: Bool
  }
  deriving (Show)

data UserContactLink = UserContactLink
  { connLinkContact :: CreatedLinkContact,
    autoAccept :: Maybe AutoAccept
  }
  deriving (Show)

data GroupLinkInfo = GroupLinkInfo
  { groupId :: GroupId,
    memberRole :: GroupMemberRole
  }
  deriving (Show)

data AutoAccept = AutoAccept
  { businessAddress :: Bool, -- possibly, it can be wrapped together with acceptIncognito, or AutoAccept made sum type
    acceptIncognito :: IncognitoEnabled,
    autoReply :: Maybe MsgContent
  }
  deriving (Show)

$(J.deriveJSON defaultJSON ''AutoAccept)

$(J.deriveJSON defaultJSON ''UserContactLink)

toUserContactLink :: (ConnReqContact, Maybe ShortLinkContact, BoolInt, BoolInt, BoolInt, Maybe MsgContent) -> UserContactLink
toUserContactLink (connReq, shortLink, BI autoAccept, BI businessAddress, BI acceptIncognito, autoReply) =
  UserContactLink (CCLink connReq shortLink) $
    if autoAccept then Just AutoAccept {businessAddress, acceptIncognito, autoReply} else Nothing

getUserAddress :: DB.Connection -> User -> ExceptT StoreError IO UserContactLink
getUserAddress db User {userId} =
  ExceptT . firstRow toUserContactLink SEUserContactLinkNotFound $
    DB.query db (userContactLinkQuery <> " WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL") (Only userId)

getUserContactLinkById :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO (UserContactLink, Maybe GroupLinkInfo)
getUserContactLinkById db userId userContactLinkId =
  ExceptT . firstRow (\(ucl :. gli) -> (toUserContactLink ucl, toGroupLinkInfo gli)) SEUserContactLinkNotFound $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, short_link_contact, auto_accept, business_address, auto_accept_incognito, auto_reply_msg_content, group_id, group_link_member_role
        FROM user_contact_links
        WHERE user_id = ? AND user_contact_link_id = ?
      |]
      (userId, userContactLinkId)

toGroupLinkInfo :: (Maybe GroupId, Maybe GroupMemberRole) -> Maybe GroupLinkInfo
toGroupLinkInfo (groupId_, mRole_) =
  (\groupId -> GroupLinkInfo {groupId, memberRole = fromMaybe GRMember mRole_})
    <$> groupId_

getGroupLinkInfo :: DB.Connection -> UserId -> GroupId -> IO (Maybe GroupLinkInfo)
getGroupLinkInfo db userId groupId =
  fmap join $ maybeFirstRow toGroupLinkInfo $ 
    DB.query
      db
      [sql|
        SELECT group_id, group_link_member_role
        FROM user_contact_links
        WHERE user_id = ? AND group_id = ?
      |]      
      (userId, groupId)

getUserContactLinkByConnReq :: DB.Connection -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe UserContactLink)
getUserContactLinkByConnReq db User {userId} (cReqSchema1, cReqSchema2) =
  maybeFirstRow toUserContactLink $
    DB.query db (userContactLinkQuery <> " WHERE user_id = ? AND conn_req_contact IN (?,?)") (userId, cReqSchema1, cReqSchema2)

getUserContactLinkViaShortLink :: DB.Connection -> User -> ShortLinkContact -> IO (Maybe UserContactLink)
getUserContactLinkViaShortLink db User {userId} shortLink =
  maybeFirstRow toUserContactLink $
    DB.query db (userContactLinkQuery <> " WHERE user_id = ? AND short_link_contact = ?") (userId, shortLink)

userContactLinkQuery :: Query
userContactLinkQuery =
  [sql|
    SELECT conn_req_contact, short_link_contact, auto_accept, business_address, auto_accept_incognito, auto_reply_msg_content
    FROM user_contact_links
  |]

getContactWithoutConnViaAddress :: DB.Connection -> VersionRangeChat -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe Contact)
getContactWithoutConnViaAddress db vr user@User {userId} (cReqSchema1, cReqSchema2) = do
  ctId_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT ct.contact_id
          FROM contacts ct
          JOIN contact_profiles cp ON cp.contact_profile_id = ct.contact_profile_id
          LEFT JOIN connections c ON c.contact_id = ct.contact_id
          WHERE cp.user_id = ? AND cp.contact_link IN (?,?) AND c.connection_id IS NULL
        |]
        (userId, cReqSchema1, cReqSchema2)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getContact db vr user) ctId_

updateUserAddressAutoAccept :: DB.Connection -> User -> Maybe AutoAccept -> ExceptT StoreError IO UserContactLink
updateUserAddressAutoAccept db user@User {userId} autoAccept = do
  link <- getUserAddress db user
  liftIO updateUserAddressAutoAccept_ $> link {autoAccept}
  where
    updateUserAddressAutoAccept_ =
      DB.execute
        db
        [sql|
          UPDATE user_contact_links
          SET auto_accept = ?, business_address = ?, auto_accept_incognito = ?, auto_reply_msg_content = ?
          WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL
        |]
        (ucl :. Only userId)
    ucl = case autoAccept of
      Just AutoAccept {businessAddress, acceptIncognito, autoReply} -> (BI True, BI businessAddress, BI acceptIncognito, autoReply)
      _ -> (BI False, BI False, BI False, Nothing)

getProtocolServers :: forall p. ProtocolTypeI p => DB.Connection -> SProtocolType p -> User -> IO [UserServer p]
getProtocolServers db p User {userId} =
  map toUserServer
    <$> DB.query
      db
      [sql|
        SELECT smp_server_id, host, port, key_hash, basic_auth, preset, tested, enabled
        FROM protocol_servers
        WHERE user_id = ? AND protocol = ?
      |]
      (userId, decodeLatin1 $ strEncode p)
  where
    toUserServer :: (DBEntityId, NonEmpty TransportHost, String, C.KeyHash, Maybe Text, BoolInt, Maybe BoolInt, BoolInt) -> UserServer p
    toUserServer (serverId, host, port, keyHash, auth_, BI preset, tested, BI enabled) =
      let server = ProtoServerWithAuth (ProtocolServer p host port keyHash) (BasicAuth . encodeUtf8 <$> auth_)
       in UserServer {serverId, server, preset, tested = unBI <$> tested, enabled, deleted = False}

insertProtocolServer :: forall p. ProtocolTypeI p => DB.Connection -> SProtocolType p -> User -> UTCTime -> NewUserServer p -> IO (UserServer p)
insertProtocolServer db p User {userId} ts srv@UserServer {server, preset, tested, enabled} = do
  DB.execute
    db
    [sql|
      INSERT INTO protocol_servers
        (protocol, host, port, key_hash, basic_auth, preset, tested, enabled, user_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?,?,?)
    |]
    (serverColumns p server :. (BI preset, BI <$> tested, BI enabled, userId, ts, ts))
  sId <- insertedRowId db
  pure (srv :: NewUserServer p) {serverId = DBEntityId sId}

updateProtocolServer :: ProtocolTypeI p => DB.Connection -> SProtocolType p -> UTCTime -> UserServer p -> IO ()
updateProtocolServer db p ts UserServer {serverId, server, preset, tested, enabled} =
  DB.execute
    db
    [sql|
      UPDATE protocol_servers
      SET protocol = ?, host = ?, port = ?, key_hash = ?, basic_auth = ?,
          preset = ?, tested = ?, enabled = ?, updated_at = ?
      WHERE smp_server_id = ?
    |]
    (serverColumns p server :. (BI preset, BI <$> tested, BI enabled, ts, serverId))

serverColumns :: ProtocolTypeI p => SProtocolType p -> ProtoServerWithAuth p -> (Text, NonEmpty TransportHost, String, C.KeyHash, Maybe Text)
serverColumns p (ProtoServerWithAuth ProtocolServer {host, port, keyHash} auth_) =
  let protocol = decodeLatin1 $ strEncode p
      auth = safeDecodeUtf8 . unBasicAuth <$> auth_
   in (protocol, host, port, keyHash, auth)

getServerOperators :: DB.Connection -> ExceptT StoreError IO ServerOperatorConditions
getServerOperators db = do
  currentConditions <- getCurrentUsageConditions db
  liftIO $ do
    now <- getCurrentTime
    latestAcceptedConds_ <- getLatestAcceptedConditions db
    let getConds op = (\ca -> op {conditionsAcceptance = ca}) <$> getOperatorConditions_ db op currentConditions latestAcceptedConds_ now
    ops <- mapM getConds =<< getServerOperators_ db
    let conditionsAction = usageConditionsAction ops currentConditions now
    pure ServerOperatorConditions {serverOperators = ops, currentConditions, conditionsAction}

getUserServers :: DB.Connection -> User -> ExceptT StoreError IO ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP])
getUserServers db user =
  (,,)
    <$> (map Just . serverOperators <$> getServerOperators db)
    <*> liftIO (getProtocolServers db SPSMP user)
    <*> liftIO (getProtocolServers db SPXFTP user)

setServerOperators :: DB.Connection -> NonEmpty ServerOperator -> IO ()
setServerOperators db ops = do
  currentTs <- getCurrentTime
  mapM_ (updateServerOperator db currentTs) ops

updateServerOperator :: DB.Connection -> UTCTime -> ServerOperator -> IO ()
updateServerOperator db currentTs ServerOperator {operatorId, enabled, smpRoles, xftpRoles} =
  DB.execute
    db
    [sql|
      UPDATE server_operators
      SET enabled = ?, smp_role_storage = ?, smp_role_proxy = ?, xftp_role_storage = ?, xftp_role_proxy = ?, updated_at = ?
      WHERE server_operator_id = ?
    |]
    (BI enabled, BI (storage smpRoles), BI (proxy smpRoles), BI (storage xftpRoles), BI (proxy xftpRoles), currentTs, operatorId)

getUpdateServerOperators :: DB.Connection -> NonEmpty PresetOperator -> Bool -> IO [(Maybe PresetOperator, Maybe ServerOperator)]
getUpdateServerOperators db presetOps newUser = do
  conds <- map toUsageConditions <$> DB.query_ db usageCondsQuery
  now <- getCurrentTime
  let (currentConds, condsToAdd) = usageConditionsToAdd newUser now conds
  mapM_ insertConditions condsToAdd
  latestAcceptedConds_ <- getLatestAcceptedConditions db
  ops <- updatedServerOperators presetOps <$> getServerOperators_ db
  forM ops $ traverse $ mapM $ \(ASO _ op) ->
    -- traverse for tuple, mapM for Maybe
    case operatorId op of
      DBNewEntity -> insertOperator op
      DBEntityId _ -> do
        updateOperator op
        getOperatorConditions_ db op currentConds latestAcceptedConds_ now >>= \case
          CARequired (Just ts) | ts < now -> autoAcceptConditions op currentConds now
          ca -> pure op {conditionsAcceptance = ca}
  where
    insertConditions UsageConditions {conditionsId, conditionsCommit, notifiedAt, createdAt} =
      DB.execute
        db
        [sql|
          INSERT INTO usage_conditions
            (usage_conditions_id, conditions_commit, notified_at, created_at)
          VALUES (?,?,?,?)
        |]
        (conditionsId, conditionsCommit, notifiedAt, createdAt)
    updateOperator :: ServerOperator -> IO ()
    updateOperator ServerOperator {operatorId, tradeName, legalName, serverDomains, enabled, smpRoles, xftpRoles} =
      DB.execute
        db
        [sql|
          UPDATE server_operators
          SET trade_name = ?, legal_name = ?, server_domains = ?, enabled = ?, smp_role_storage = ?, smp_role_proxy = ?, xftp_role_storage = ?, xftp_role_proxy = ?
          WHERE server_operator_id = ?
        |]
        (tradeName, legalName, T.intercalate "," serverDomains, BI enabled, BI (storage smpRoles), BI (proxy smpRoles), BI (storage xftpRoles), BI (proxy xftpRoles), operatorId)
    insertOperator :: NewServerOperator -> IO ServerOperator
    insertOperator op@ServerOperator {operatorTag, tradeName, legalName, serverDomains, enabled, smpRoles, xftpRoles} = do
      DB.execute
        db
        [sql|
          INSERT INTO server_operators
            (server_operator_tag, trade_name, legal_name, server_domains, enabled, smp_role_storage, smp_role_proxy, xftp_role_storage, xftp_role_proxy)
          VALUES (?,?,?,?,?,?,?,?,?)
        |]
        (operatorTag, tradeName, legalName, T.intercalate "," serverDomains, BI enabled, BI (storage smpRoles), BI (proxy smpRoles), BI (storage xftpRoles), BI (proxy xftpRoles))
      opId <- insertedRowId db
      pure op {operatorId = DBEntityId opId}
    autoAcceptConditions op UsageConditions {conditionsCommit} now =
      acceptConditions_ db op conditionsCommit now True
        $> op {conditionsAcceptance = CAAccepted (Just now) True}

serverOperatorQuery :: Query
serverOperatorQuery =
  [sql|
    SELECT server_operator_id, server_operator_tag, trade_name, legal_name,
      server_domains, enabled, smp_role_storage, smp_role_proxy, xftp_role_storage, xftp_role_proxy
    FROM server_operators
  |]

getServerOperators_ :: DB.Connection -> IO [ServerOperator]
getServerOperators_ db = map toServerOperator <$> DB.query_ db serverOperatorQuery

toServerOperator :: (DBEntityId, Maybe OperatorTag, Text, Maybe Text, Text, BoolInt) :. (BoolInt, BoolInt) :. (BoolInt, BoolInt) -> ServerOperator
toServerOperator ((operatorId, operatorTag, tradeName, legalName, domains, BI enabled) :. smpRoles' :. xftpRoles') =
  ServerOperator
    { operatorId,
      operatorTag,
      tradeName,
      legalName,
      serverDomains = T.splitOn "," domains,
      conditionsAcceptance = CARequired Nothing,
      enabled,
      smpRoles = serverRoles smpRoles',
      xftpRoles = serverRoles xftpRoles'
    }
  where
    serverRoles (BI storage, BI proxy) = ServerRoles {storage, proxy}

getOperatorConditions_ :: DB.Connection -> ServerOperator -> UsageConditions -> Maybe UsageConditions -> UTCTime -> IO ConditionsAcceptance
getOperatorConditions_ db ServerOperator {operatorId} UsageConditions {conditionsCommit = currentCommit, createdAt, notifiedAt} latestAcceptedConds_ now = do
  case latestAcceptedConds_ of
    Nothing -> pure $ CARequired Nothing -- no conditions accepted by any operator
    Just UsageConditions {conditionsCommit = latestAcceptedCommit} -> do
      operatorAcceptedConds_ <-
        maybeFirstRow id $
          DB.query
            db
            [sql|
              SELECT conditions_commit, accepted_at, auto_accepted
              FROM operator_usage_conditions
              WHERE server_operator_id = ?
              ORDER BY operator_usage_conditions_id DESC
              LIMIT 1
            |]
            (Only operatorId)
      pure $ case operatorAcceptedConds_ of
        Just (operatorCommit, acceptedAt_, BI autoAccept)
          | operatorCommit /= latestAcceptedCommit -> CARequired Nothing -- TODO should we consider this operator disabled?
          | currentCommit /= latestAcceptedCommit -> CARequired $ conditionsRequiredOrDeadline createdAt (fromMaybe now notifiedAt)
          | otherwise -> CAAccepted acceptedAt_ autoAccept
        _ -> CARequired Nothing -- no conditions were accepted for this operator

getCurrentUsageConditions :: DB.Connection -> ExceptT StoreError IO UsageConditions
getCurrentUsageConditions db =
  ExceptT . firstRow toUsageConditions SEUsageConditionsNotFound $
    DB.query_ db (usageCondsQuery <> " DESC LIMIT 1")

usageCondsQuery :: Query
usageCondsQuery =
  [sql|
    SELECT usage_conditions_id, conditions_commit, notified_at, created_at
    FROM usage_conditions
    ORDER BY usage_conditions_id
  |]

toUsageConditions :: (Int64, Text, Maybe UTCTime, UTCTime) -> UsageConditions
toUsageConditions (conditionsId, conditionsCommit, notifiedAt, createdAt) =
  UsageConditions {conditionsId, conditionsCommit, notifiedAt, createdAt}

getLatestAcceptedConditions :: DB.Connection -> IO (Maybe UsageConditions)
getLatestAcceptedConditions db =
  maybeFirstRow toUsageConditions $
    DB.query_
      db
      [sql|
        SELECT usage_conditions_id, conditions_commit, notified_at, created_at
        FROM usage_conditions
        WHERE conditions_commit = (
          SELECT conditions_commit
          FROM operator_usage_conditions
          ORDER BY accepted_at DESC
          LIMIT 1
        )
      |]

setConditionsNotified :: DB.Connection -> Int64 -> UTCTime -> IO ()
setConditionsNotified db condId notifiedAt =
  DB.execute db "UPDATE usage_conditions SET notified_at = ? WHERE usage_conditions_id = ?" (notifiedAt, condId)

acceptConditions :: DB.Connection -> Int64 -> NonEmpty Int64 -> UTCTime -> ExceptT StoreError IO ()
acceptConditions db condId opIds acceptedAt = do
  UsageConditions {conditionsCommit} <- getUsageConditionsById_ db condId
  operators <- mapM getServerOperator_ opIds
  liftIO $ forM_ operators $ \op -> acceptConditions_ db op conditionsCommit acceptedAt False
  where
    getServerOperator_ opId =
      ExceptT $
        firstRow toServerOperator (SEOperatorNotFound opId) $
          DB.query db (serverOperatorQuery <> " WHERE server_operator_id = ?") (Only opId)

acceptConditions_ :: DB.Connection -> ServerOperator -> Text -> UTCTime -> Bool -> IO ()
acceptConditions_ db ServerOperator {operatorId, operatorTag} conditionsCommit acceptedAt autoAccepted = do
  acceptedAt_ :: Maybe (Maybe UTCTime) <- maybeFirstRow fromOnly $ DB.query db "SELECT accepted_at FROM operator_usage_conditions WHERE server_operator_id = ? AND conditions_commit = ?" (operatorId, conditionsCommit)
  case acceptedAt_ of
    Just Nothing ->
      DB.execute
        db
        (q <> "ON CONFLICT (server_operator_id, conditions_commit) DO UPDATE SET accepted_at = ?, auto_accepted = ?")
        (operatorId, operatorTag, conditionsCommit, acceptedAt, BI autoAccepted, acceptedAt, BI autoAccepted)
    Just (Just _) ->
      DB.execute
        db
        (q <> "ON CONFLICT (server_operator_id, conditions_commit) DO NOTHING")
        (operatorId, operatorTag, conditionsCommit, acceptedAt, BI autoAccepted)
    Nothing ->
      DB.execute
        db
        q
        (operatorId, operatorTag, conditionsCommit, acceptedAt, BI autoAccepted)
  where
    q =
      [sql|
        INSERT INTO operator_usage_conditions
          (server_operator_id, server_operator_tag, conditions_commit, accepted_at, auto_accepted)
        VALUES (?,?,?,?,?)
      |]

getUsageConditionsById_ :: DB.Connection -> Int64 -> ExceptT StoreError IO UsageConditions
getUsageConditionsById_ db conditionsId =
  ExceptT . firstRow toUsageConditions SEUsageConditionsNotFound $
    DB.query
      db
      [sql|
        SELECT usage_conditions_id, conditions_commit, notified_at, created_at
        FROM usage_conditions
        WHERE usage_conditions_id = ?
      |]
      (Only conditionsId)

setUserServers :: DB.Connection -> User -> UTCTime -> UpdatedUserOperatorServers -> ExceptT StoreError IO UserOperatorServers
setUserServers db user ts = checkConstraint SEUniqueID . liftIO . setUserServers' db user ts

setUserServers' :: DB.Connection -> User -> UTCTime -> UpdatedUserOperatorServers -> IO UserOperatorServers
setUserServers' db user@User {userId} ts UpdatedUserOperatorServers {operator, smpServers, xftpServers} = do
  mapM_ (updateServerOperator db ts) operator
  smpSrvs' <- catMaybes <$> mapM (upsertOrDelete SPSMP) smpServers
  xftpSrvs' <- catMaybes <$> mapM (upsertOrDelete SPXFTP) xftpServers
  pure UserOperatorServers {operator, smpServers = smpSrvs', xftpServers = xftpSrvs'}
  where
    upsertOrDelete :: ProtocolTypeI p => SProtocolType p -> AUserServer p -> IO (Maybe (UserServer p))
    upsertOrDelete p (AUS _ s@UserServer {serverId, deleted}) = case serverId of
      DBNewEntity
        | deleted -> pure Nothing
        | otherwise -> Just <$> insertProtocolServer db p user ts s
      DBEntityId srvId
        | deleted -> Nothing <$ DB.execute db "DELETE FROM protocol_servers WHERE user_id = ? AND smp_server_id = ? AND preset = ?" (userId, srvId, BI False)
        | otherwise -> Just s <$ updateProtocolServer db p ts s

createCall :: DB.Connection -> User -> Call -> UTCTime -> IO ()
createCall db user@User {userId} Call {contactId, callId, callUUID, chatItemId, callState} callTs = do
  currentTs <- getCurrentTime
  deleteCalls db user contactId
  DB.execute
    db
    [sql|
      INSERT INTO calls
        (contact_id, shared_call_id, call_uuid, chat_item_id, call_state, call_ts, user_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?)
    |]
    (contactId, callId, callUUID, chatItemId, callState, callTs, userId, currentTs, currentTs)

deleteCalls :: DB.Connection -> User -> ContactId -> IO ()
deleteCalls db User {userId} contactId = do
  DB.execute db "DELETE FROM calls WHERE user_id = ? AND contact_id = ?" (userId, contactId)

getCalls :: DB.Connection -> IO [Call]
getCalls db =
  map toCall
    <$> DB.query_
      db
      [sql|
        SELECT
          contact_id, shared_call_id, call_uuid, chat_item_id, call_state, call_ts
        FROM calls
        ORDER BY call_ts ASC
      |]
  where
    toCall :: (ContactId, CallId, Text, ChatItemId, CallState, UTCTime) -> Call
    toCall (contactId, callId, callUUID, chatItemId, callState, callTs) = Call {contactId, callId, callUUID, chatItemId, callState, callTs}

createCommand :: DB.Connection -> User -> Maybe Int64 -> CommandFunction -> IO CommandId
createCommand db User {userId} connId commandFunction = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO commands (connection_id, command_function, command_status, user_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?)
    |]
    (connId, commandFunction, CSCreated, userId, currentTs, currentTs)
  insertedRowId db

deleteCommand :: DB.Connection -> User -> CommandId -> IO ()
deleteCommand db User {userId} cmdId =
  DB.execute db "DELETE FROM commands WHERE user_id = ? AND command_id = ?" (userId, cmdId)

updateCommandStatus :: DB.Connection -> User -> CommandId -> CommandStatus -> IO ()
updateCommandStatus db User {userId} cmdId status = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE commands
      SET command_status = ?, updated_at = ?
      WHERE user_id = ? AND command_id = ?
    |]
    (status, updatedAt, userId, cmdId)

getCommandDataByCorrId :: DB.Connection -> User -> ACorrId -> IO (Maybe CommandData)
getCommandDataByCorrId db User {userId} corrId =
  maybeFirstRow toCommandData $
    DB.query
      db
      [sql|
        SELECT command_id, connection_id, command_function, command_status
        FROM commands
        WHERE user_id = ? AND command_id = ?
      |]
      (userId, commandId corrId)
  where
    toCommandData :: (CommandId, Maybe Int64, CommandFunction, CommandStatus) -> CommandData
    toCommandData (cmdId, cmdConnId, cmdFunction, cmdStatus) = CommandData {cmdId, cmdConnId, cmdFunction, cmdStatus}

setUserUIThemes :: DB.Connection -> User -> Maybe UIThemeEntityOverrides -> IO ()
setUserUIThemes db User {userId} uiThemes = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE users SET ui_themes = ?, updated_at = ? WHERE user_id = ?" (uiThemes, updatedAt, userId)
