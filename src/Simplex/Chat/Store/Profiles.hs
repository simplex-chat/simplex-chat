{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    createUserRecord,
    createUserRecordAt,
    getUsersInfo,
    getUsers,
    setActiveUser,
    getSetActiveUser,
    getUser,
    getUserIdByName,
    getUserByAConnId,
    getUserByASndFileId,
    getUserByARcvFileId,
    getUserByContactId,
    getUserByGroupId,
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
    getUserContactLinkByConnReq,
    getContactWithoutConnViaAddress,
    updateUserAddressAutoAccept,
    getProtocolServers,
    overwriteProtocolServers,
    createCall,
    deleteCalls,
    getCalls,
    createCommand,
    setCommandConnId,
    deleteCommand,
    updateCommandStatus,
    getCommandDataByCorrId,
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Call
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ACorrId, ConnId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Protocol (BasicAuth (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolTypeI (..), SubscriptionMode)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)

createUserRecord :: DB.Connection -> AgentUserId -> Profile -> Bool -> ExceptT StoreError IO User
createUserRecord db auId p activeUser = createUserRecordAt db auId p activeUser =<< liftIO getCurrentTime

createUserRecordAt :: DB.Connection -> AgentUserId -> Profile -> Bool -> UTCTime -> ExceptT StoreError IO User
createUserRecordAt db (AgentUserId auId) Profile {displayName, fullName, image, preferences = userPreferences} activeUser currentTs =
  checkConstraint SEDuplicateName . liftIO $ do
    when activeUser $ DB.execute_ db "UPDATE users SET active_user = 0"
    let showNtfs = True
        sendRcptsContacts = True
        sendRcptsSmallGroups = True
    DB.execute
      db
      "INSERT INTO users (agent_user_id, local_display_name, active_user, contact_id, show_ntfs, send_rcpts_contacts, send_rcpts_small_groups, created_at, updated_at) VALUES (?,?,?,0,?,?,?,?,?)"
      (auId, displayName, activeUser, showNtfs, sendRcptsContacts, sendRcptsSmallGroups, currentTs, currentTs)
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
      (profileId, displayName, userId, True, currentTs, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure $ toUser $ (userId, auId, contactId, profileId, activeUser, displayName, fullName, image, Nothing, userPreferences) :. (showNtfs, sendRcptsContacts, sendRcptsSmallGroups, Nothing, Nothing)

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
              WHERE i.user_id = ? AND i.item_status = ? AND (g.enable_ntfs = 1 OR g.enable_ntfs IS NULL)
            |]
            (userId, CISRcvNew)
      pure UserInfo {user, unreadCount = fromMaybe 0 ctCount + fromMaybe 0 gCount}

getUsers :: DB.Connection -> IO [User]
getUsers db =
  map toUser <$> DB.query_ db userQuery

setActiveUser :: DB.Connection -> UserId -> IO ()
setActiveUser db userId = do
  DB.execute_ db "UPDATE users SET active_user = 0"
  DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?" (Only userId)

getSetActiveUser :: DB.Connection -> UserId -> ExceptT StoreError IO User
getSetActiveUser db userId = do
  liftIO $ setActiveUser db userId
  getUser db userId

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
    (hashSalt viewPwdHash :. (showNtfs, userId))
  where
    hashSalt = L.unzip . fmap (\UserPwdHash {hash, salt} -> (hash, salt))

updateAllContactReceipts :: DB.Connection -> Bool -> IO ()
updateAllContactReceipts db onOff =
  DB.execute
    db
    "UPDATE users SET send_rcpts_contacts = ?, send_rcpts_small_groups = ? WHERE view_pwd_hash IS NULL"
    (onOff, onOff)

updateUserContactReceipts :: DB.Connection -> User -> UserMsgReceiptSettings -> IO ()
updateUserContactReceipts db User {userId} UserMsgReceiptSettings {enable, clearOverrides} = do
  DB.execute db "UPDATE users SET send_rcpts_contacts = ? WHERE user_id = ?" (enable, userId)
  when clearOverrides $ DB.execute_ db "UPDATE contacts SET send_rcpts = NULL"

updateUserGroupReceipts :: DB.Connection -> User -> UserMsgReceiptSettings -> IO ()
updateUserGroupReceipts db User {userId} UserMsgReceiptSettings {enable, clearOverrides} = do
  DB.execute db "UPDATE users SET send_rcpts_small_groups = ? WHERE user_id = ?" (enable, userId)
  when clearOverrides $ DB.execute_ db "UPDATE groups SET send_rcpts = NULL"

updateUserProfile :: DB.Connection -> User -> Profile -> ExceptT StoreError IO User
updateUserProfile db user p'
  | displayName == newName = do
      liftIO $ updateContactProfile_ db userId profileId p'
      pure user {profile, fullPreferences}
  | otherwise =
      checkConstraint SEDuplicateName . liftIO $ do
        currentTs <- getCurrentTime
        DB.execute db "UPDATE users SET local_display_name = ?, updated_at = ? WHERE user_id = ?" (newName, currentTs, userId)
        DB.execute
          db
          "INSERT INTO display_names (local_display_name, ldn_base, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
          (newName, newName, userId, currentTs, currentTs)
        updateContactProfile_' db userId profileId p' currentTs
        updateContact_ db userId userContactId localDisplayName newName currentTs
        pure user {localDisplayName = newName, profile, fullPreferences}
  where
    User {userId, userContactId, localDisplayName, profile = LocalProfile {profileId, displayName, localAlias}} = user
    Profile {displayName = newName, preferences} = p'
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
    (connReqContact_, ts, userId, profileId)
  pure (user :: User) {profile = p {contactLink = connReqContact_}}
  where
    connReqContact_ = case ucl_ of
      Just UserContactLink {connReqContact} -> Just connReqContact
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
    toContactProfile :: (ContactName, Text, Maybe ImageData, Maybe ConnReqContact, Maybe Preferences) -> Profile
    toContactProfile (displayName, fullName, image, contactLink, preferences) = Profile {displayName, fullName, image, contactLink, preferences}

createUserContactLink :: DB.Connection -> User -> ConnId -> ConnReqContact -> SubscriptionMode -> ExceptT StoreError IO ()
createUserContactLink db User {userId} agentConnId cReq subMode =
  checkConstraint SEDuplicateContactLink . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, conn_req_contact, created_at, updated_at) VALUES (?,?,?,?)"
      (userId, cReq, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId chatInitialVRange Nothing Nothing Nothing 0 currentTs subMode

getUserAddressConnections :: DB.Connection -> User -> ExceptT StoreError IO [Connection]
getUserAddressConnections db User {userId} = do
  cs <- liftIO getUserAddressConnections_
  if null cs then throwError SEUserContactLinkNotFound else pure cs
  where
    getUserAddressConnections_ :: IO [Connection]
    getUserAddressConnections_ =
      map toConnection
        <$> DB.query
          db
          [sql|
            SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
              c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
              c.peer_chat_min_version, c.peer_chat_max_version
            FROM connections c
            JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
            WHERE c.user_id = ? AND uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
          |]
          (userId, userId)

getUserContactLinks :: DB.Connection -> User -> IO [(Connection, UserContact)]
getUserContactLinks db User {userId} =
  map toUserContactConnection
    <$> DB.query
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version,
          uc.user_contact_link_id, uc.conn_req_contact, uc.group_id
        FROM connections c
        JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
        WHERE c.user_id = ? AND uc.user_id = ?
      |]
      (userId, userId)
  where
    toUserContactConnection :: (ConnectionRow :. (Int64, ConnReqContact, Maybe GroupId)) -> (Connection, UserContact)
    toUserContactConnection (connRow :. (userContactLinkId, connReqContact, groupId)) = (toConnection connRow, UserContact {userContactLinkId, connReqContact, groupId})

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
  DB.executeNamed
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = :user_id
        AND local_display_name in (
          SELECT cr.local_display_name
          FROM contact_requests cr
          JOIN user_contact_links uc USING (user_contact_link_id)
          WHERE uc.user_id = :user_id AND uc.local_display_name = '' AND uc.group_id IS NULL
        )
    |]
    [":user_id" := userId]
  DB.executeNamed
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT cr.contact_profile_id
        FROM contact_requests cr
        JOIN user_contact_links uc USING (user_contact_link_id)
        WHERE uc.user_id = :user_id AND uc.local_display_name = '' AND uc.group_id IS NULL
      )
    |]
    [":user_id" := userId]
  void $ setUserProfileContactLink db user Nothing
  DB.execute db "DELETE FROM user_contact_links WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL" (Only userId)

data UserMsgReceiptSettings = UserMsgReceiptSettings
  { enable :: Bool,
    clearOverrides :: Bool
  }
  deriving (Show)

data UserContactLink = UserContactLink
  { connReqContact :: ConnReqContact,
    autoAccept :: Maybe AutoAccept
  }
  deriving (Show)

data AutoAccept = AutoAccept
  { acceptIncognito :: IncognitoEnabled,
    autoReply :: Maybe MsgContent
  }
  deriving (Show)

$(J.deriveJSON defaultJSON ''AutoAccept)

$(J.deriveJSON defaultJSON ''UserContactLink)

toUserContactLink :: (ConnReqContact, Bool, IncognitoEnabled, Maybe MsgContent) -> UserContactLink
toUserContactLink (connReq, autoAccept, acceptIncognito, autoReply) =
  UserContactLink connReq $
    if autoAccept then Just AutoAccept {acceptIncognito, autoReply} else Nothing

getUserAddress :: DB.Connection -> User -> ExceptT StoreError IO UserContactLink
getUserAddress db User {userId} =
  ExceptT . firstRow toUserContactLink SEUserContactLinkNotFound $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, auto_accept, auto_accept_incognito, auto_reply_msg_content
        FROM user_contact_links
        WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL
      |]
      (Only userId)

getUserContactLinkById :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO (UserContactLink, Maybe GroupId, GroupMemberRole)
getUserContactLinkById db userId userContactLinkId =
  ExceptT . firstRow (\(ucl :. (groupId_, mRole_)) -> (toUserContactLink ucl, groupId_, fromMaybe GRMember mRole_)) SEUserContactLinkNotFound $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, auto_accept, auto_accept_incognito, auto_reply_msg_content, group_id, group_link_member_role
        FROM user_contact_links
        WHERE user_id = ?
          AND user_contact_link_id = ?
      |]
      (userId, userContactLinkId)

getUserContactLinkByConnReq :: DB.Connection -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe UserContactLink)
getUserContactLinkByConnReq db User {userId} (cReqSchema1, cReqSchema2) =
  maybeFirstRow toUserContactLink $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, auto_accept, auto_accept_incognito, auto_reply_msg_content
        FROM user_contact_links
        WHERE user_id = ? AND conn_req_contact IN (?,?)
      |]
      (userId, cReqSchema1, cReqSchema2)

getContactWithoutConnViaAddress :: DB.Connection -> User -> (ConnReqContact, ConnReqContact) -> IO (Maybe Contact)
getContactWithoutConnViaAddress db user@User {userId} (cReqSchema1, cReqSchema2) = do
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
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getContact db user) ctId_

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
          SET auto_accept = ?, auto_accept_incognito = ?, auto_reply_msg_content = ?
          WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL
        |]
        (ucl :. Only userId)
    ucl = case autoAccept of
      Just AutoAccept {acceptIncognito, autoReply} -> (True, acceptIncognito, autoReply)
      _ -> (False, False, Nothing)

getProtocolServers :: forall p. ProtocolTypeI p => DB.Connection -> User -> IO [ServerCfg p]
getProtocolServers db User {userId} =
  map toServerCfg
    <$> DB.query
      db
      [sql|
        SELECT host, port, key_hash, basic_auth, preset, tested, enabled
        FROM protocol_servers
        WHERE user_id = ? AND protocol = ?;
      |]
      (userId, decodeLatin1 $ strEncode protocol)
  where
    protocol = protocolTypeI @p
    toServerCfg :: (NonEmpty TransportHost, String, C.KeyHash, Maybe Text, Bool, Maybe Bool, Bool) -> ServerCfg p
    toServerCfg (host, port, keyHash, auth_, preset, tested, enabled) =
      let server = ProtoServerWithAuth (ProtocolServer protocol host port keyHash) (BasicAuth . encodeUtf8 <$> auth_)
       in ServerCfg {server, preset, tested, enabled}

overwriteProtocolServers :: forall p. ProtocolTypeI p => DB.Connection -> User -> [ServerCfg p] -> ExceptT StoreError IO ()
overwriteProtocolServers db User {userId} servers =
  checkConstraint SEUniqueID . ExceptT $ do
    currentTs <- getCurrentTime
    DB.execute db "DELETE FROM protocol_servers WHERE user_id = ? AND protocol = ? " (userId, protocol)
    forM_ servers $ \ServerCfg {server, preset, tested, enabled} -> do
      let ProtoServerWithAuth ProtocolServer {host, port, keyHash} auth_ = server
      DB.execute
        db
        [sql|
          INSERT INTO protocol_servers
            (protocol, host, port, key_hash, basic_auth, preset, tested, enabled, user_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((protocol, host, port, keyHash, safeDecodeUtf8 . unBasicAuth <$> auth_) :. (preset, tested, enabled, userId, currentTs, currentTs))
    pure $ Right ()
  where
    protocol = decodeLatin1 $ strEncode $ protocolTypeI @p

createCall :: DB.Connection -> User -> Call -> UTCTime -> IO ()
createCall db user@User {userId} Call {contactId, callId, chatItemId, callState} callTs = do
  currentTs <- getCurrentTime
  deleteCalls db user contactId
  DB.execute
    db
    [sql|
      INSERT INTO calls
        (contact_id, shared_call_id, chat_item_id, call_state, call_ts, user_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (contactId, callId, chatItemId, callState, callTs, userId, currentTs, currentTs)

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
          contact_id, shared_call_id, chat_item_id, call_state, call_ts
        FROM calls
        ORDER BY call_ts ASC
      |]
  where
    toCall :: (ContactId, CallId, ChatItemId, CallState, UTCTime) -> Call
    toCall (contactId, callId, chatItemId, callState, callTs) = Call {contactId, callId, chatItemId, callState, callTs}

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
