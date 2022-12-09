{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store
  ( SQLiteStore,
    StoreError (..),
    UserContactLink (..),
    AutoAccept (..),
    createChatStore,
    chatStoreFile,
    agentStoreFile,
    createUser,
    getUsers,
    setActiveUser,
    createDirectConnection,
    createConnReqConnection,
    getProfileById,
    getConnReqContactXContactId,
    createDirectContact,
    deleteContactConnectionsAndFiles,
    deleteContact,
    deleteContactWithoutGroups,
    getContactByName,
    getContact,
    getContactIdByName,
    updateUserProfile,
    updateContactProfile,
    updateContactUserPreferences,
    updateContactAlias,
    updateContactConnectionAlias,
    updateContactUsed,
    updateContactUnreadChat,
    updateGroupUnreadChat,
    setConnectionVerified,
    getUserContacts,
    getUserContactProfiles,
    createUserContactLink,
    getUserAddressConnections,
    getUserContactLinks,
    deleteUserAddress,
    getUserAddress,
    getUserContactLinkById,
    updateUserAddressAutoAccept,
    createGroupLink,
    getGroupLinkConnection,
    deleteGroupLink,
    getGroupLink,
    getGroupLinkId,
    createOrUpdateContactRequest,
    getContactRequest,
    getContactRequestIdByName,
    deleteContactRequest,
    createAcceptedContact,
    getLiveSndFileTransfers,
    getLiveRcvFileTransfers,
    getPendingSndChunks,
    getPendingContactConnections,
    getContactConnections,
    getConnectionEntity,
    getConnectionById,
    getConnectionsContacts,
    getGroupAndMember,
    updateConnectionStatus,
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
    getGroupMembers,
    getGroupMembersForExpiration,
    deleteGroupConnectionsAndFiles,
    deleteGroupItemsAndMembers,
    deleteGroup,
    getUserGroups,
    getUserGroupDetails,
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
    randomBytes,
    createSentProbe,
    createSentProbeHash,
    deleteSentProbe,
    matchReceivedProbe,
    matchReceivedProbeHash,
    matchSentProbe,
    mergeContactRecords,
    createSndDirectFileTransfer,
    createSndDirectFTConnection,
    createSndGroupFileTransfer,
    createSndGroupFileTransferConnection,
    createSndDirectInlineFT,
    createSndGroupInlineFT,
    updateSndDirectFTDelivery,
    updateSndGroupFTDelivery,
    getSndInlineFTViaMsgDelivery,
    updateFileCancelled,
    updateCIFileStatus,
    getSharedMsgIdByFileId,
    getFileIdBySharedMsgId,
    getGroupFileIdBySharedMsgId,
    getDirectFileIdBySharedMsgId,
    getChatRefByFileId,
    updateSndFileStatus,
    createSndFileChunk,
    updateSndFileChunkMsg,
    updateSndFileChunkSent,
    deleteSndFileChunks,
    createRcvFileTransfer,
    createRcvGroupFileTransfer,
    getRcvFileTransfer,
    acceptRcvFileTransfer,
    acceptRcvInlineFT,
    startRcvInlineFT,
    updateRcvFileStatus,
    createRcvFileChunk,
    updatedRcvFileChunkStored,
    deleteRcvFileChunks,
    updateFileTransferChatItemId,
    getFileTransfer,
    getFileTransferProgress,
    getFileTransferMeta,
    getSndFileTransfer,
    getContactFileInfo,
    getContactMaxItemTs,
    deleteContactCIs,
    updateContactTs,
    getGroupFileInfo,
    getGroupMaxItemTs,
    deleteGroupCIs,
    updateGroupTs,
    createNewSndMessage,
    createSndMsgDelivery,
    createNewMessageAndRcvMsgDelivery,
    createSndMsgDeliveryEvent,
    createRcvMsgDeliveryEvent,
    createPendingGroupMessage,
    getPendingGroupMessages,
    deletePendingGroupMessage,
    createNewSndChatItem,
    createNewRcvChatItem,
    createNewChatItemNoMsg,
    getChatPreviews,
    getDirectChat,
    getGroupChat,
    getAllChatItems,
    getChatItemIdByAgentMsgId,
    getDirectChatItem,
    getDirectChatItemBySharedMsgId,
    getDirectChatItemByAgentMsgId,
    getGroupChatItem,
    getGroupChatItemBySharedMsgId,
    getDirectChatItemIdByText,
    getGroupChatItemIdByText,
    getChatItemByFileId,
    getChatItemByGroupId,
    updateDirectChatItemStatus,
    updateDirectCIFileStatus,
    updateDirectChatItem,
    deleteDirectChatItem,
    markDirectChatItemDeleted,
    updateGroupChatItem,
    deleteGroupChatItem,
    markGroupChatItemDeleted,
    updateDirectChatItemsRead,
    updateGroupChatItemsRead,
    getSMPServers,
    overwriteSMPServers,
    createCall,
    deleteCalls,
    getCalls,
    createCommand,
    setCommandConnId,
    deleteCommand,
    updateCommandStatus,
    getCommandDataByCorrId,
    setConnConnReqInv,
    getXGrpMemIntroContDirect,
    getXGrpMemIntroContGroup,
    getChatItemTTL,
    setChatItemTTL,
    getContactExpiredFileInfo,
    deleteContactExpiredCIs,
    getContactCICount,
    getGroupExpiredFileInfo,
    deleteGroupExpiredCIs,
    getGroupCICount,
    getPendingContactConnection,
    deletePendingContactConnection,
    updateContactSettings,
    updateGroupSettings,
    withTransaction,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (stateTVar)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad.Except
import Crypto.Random (ChaChaDRG, randomBytesGenerate)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import Data.Either (rights)
import Data.Function (on)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import Data.Type.Equality
import Database.SQLite.Simple (NamedParam (..), Only (..), Query (..), SQLError, (:.) (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Simplex.Chat.Call
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Migrations.M20220101_initial
import Simplex.Chat.Migrations.M20220122_v1_1
import Simplex.Chat.Migrations.M20220205_chat_item_status
import Simplex.Chat.Migrations.M20220210_deduplicate_contact_requests
import Simplex.Chat.Migrations.M20220224_messages_fks
import Simplex.Chat.Migrations.M20220301_smp_servers
import Simplex.Chat.Migrations.M20220302_profile_images
import Simplex.Chat.Migrations.M20220304_msg_quotes
import Simplex.Chat.Migrations.M20220321_chat_item_edited
import Simplex.Chat.Migrations.M20220404_files_status_fields
import Simplex.Chat.Migrations.M20220514_profiles_user_id
import Simplex.Chat.Migrations.M20220626_auto_reply
import Simplex.Chat.Migrations.M20220702_calls
import Simplex.Chat.Migrations.M20220715_groups_chat_item_id
import Simplex.Chat.Migrations.M20220811_chat_items_indices
import Simplex.Chat.Migrations.M20220812_incognito_profiles
import Simplex.Chat.Migrations.M20220818_chat_notifications
import Simplex.Chat.Migrations.M20220822_groups_host_conn_custom_user_profile_id
import Simplex.Chat.Migrations.M20220823_delete_broken_group_event_chat_items
import Simplex.Chat.Migrations.M20220824_profiles_local_alias
import Simplex.Chat.Migrations.M20220909_commands
import Simplex.Chat.Migrations.M20220926_connection_alias
import Simplex.Chat.Migrations.M20220928_settings
import Simplex.Chat.Migrations.M20221001_shared_msg_id_indices
import Simplex.Chat.Migrations.M20221003_delete_broken_integrity_error_chat_items
import Simplex.Chat.Migrations.M20221004_idx_msg_deliveries_message_id
import Simplex.Chat.Migrations.M20221011_user_contact_links_group_id
import Simplex.Chat.Migrations.M20221012_inline_files
import Simplex.Chat.Migrations.M20221019_unread_chat
import Simplex.Chat.Migrations.M20221021_auto_accept__group_links
import Simplex.Chat.Migrations.M20221024_contact_used
import Simplex.Chat.Migrations.M20221025_chat_settings
import Simplex.Chat.Migrations.M20221029_group_link_id
import Simplex.Chat.Migrations.M20221112_server_password
import Simplex.Chat.Migrations.M20221115_server_cfg
import Simplex.Chat.Migrations.M20221129_delete_group_feature_items
import Simplex.Chat.Migrations.M20221130_delete_item_deleted
import Simplex.Chat.Migrations.M20221209_verified_connection
import Simplex.Chat.Migrations.M20221210_idxs
import Simplex.Chat.Migrations.M20221211_group_description
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (ACorrId, AgentMsgId, ConnId, InvitationId, MsgMeta (..))
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, firstRow, firstRow', maybeFirstRow, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Protocol (BasicAuth (..), ProtoServerWithAuth (..), ProtocolServer (..), pattern SMPServer)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)
import UnliftIO.STM

schemaMigrations :: [(String, Query)]
schemaMigrations =
  [ ("20220101_initial", m20220101_initial),
    ("20220122_v1_1", m20220122_v1_1),
    ("20220205_chat_item_status", m20220205_chat_item_status),
    ("20220210_deduplicate_contact_requests", m20220210_deduplicate_contact_requests),
    ("20220224_messages_fks", m20220224_messages_fks),
    ("20220301_smp_servers", m20220301_smp_servers),
    ("20220302_profile_images", m20220302_profile_images),
    ("20220304_msg_quotes", m20220304_msg_quotes),
    ("20220321_chat_item_edited", m20220321_chat_item_edited),
    ("20220404_files_status_fields", m20220404_files_status_fields),
    ("20220514_profiles_user_id", m20220514_profiles_user_id),
    ("20220626_auto_reply", m20220626_auto_reply),
    ("20220702_calls", m20220702_calls),
    ("20220715_groups_chat_item_id", m20220715_groups_chat_item_id),
    ("20220811_chat_items_indices", m20220811_chat_items_indices),
    ("20220812_incognito_profiles", m20220812_incognito_profiles),
    ("20220818_chat_notifications", m20220818_chat_notifications),
    ("20220822_groups_host_conn_custom_user_profile_id", m20220822_groups_host_conn_custom_user_profile_id),
    ("20220823_delete_broken_group_event_chat_items", m20220823_delete_broken_group_event_chat_items),
    ("20220824_profiles_local_alias", m20220824_profiles_local_alias),
    ("20220909_commands", m20220909_commands),
    ("20220926_connection_alias", m20220926_connection_alias),
    ("20220928_settings", m20220928_settings),
    ("20221001_shared_msg_id_indices", m20221001_shared_msg_id_indices),
    ("20221003_delete_broken_integrity_error_chat_items", m20221003_delete_broken_integrity_error_chat_items),
    ("20221004_idx_msg_deliveries_message_id", m20221004_idx_msg_deliveries_message_id),
    ("20221011_user_contact_links_group_id", m20221011_user_contact_links_group_id),
    ("20221012_inline_files", m20221012_inline_files),
    ("20221019_unread_chat", m20221019_unread_chat),
    ("20221021_auto_accept__group_links", m20221021_auto_accept__group_links),
    ("20221024_contact_used", m20221024_contact_used),
    ("20221025_chat_settings", m20221025_chat_settings),
    ("20221029_group_link_id", m20221029_group_link_id),
    ("20221112_server_password", m20221112_server_password),
    ("20221115_server_cfg", m20221115_server_cfg),
    ("20221129_delete_group_feature_items", m20221129_delete_group_feature_items),
    ("20221130_delete_item_deleted", m20221130_delete_item_deleted),
    ("20221209_verified_connection", m20221209_verified_connection),
    ("20221210_idxs", m20221210_idxs),
    ("20221211_group_description", m20221211_group_description)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortBy (compare `on` name) $ map migration schemaMigrations
  where
    migration (name, query) = Migration {name = name, up = fromQuery query}

createChatStore :: FilePath -> String -> Bool -> IO SQLiteStore
createChatStore dbFilePath dbKey = createSQLiteStore dbFilePath dbKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")

checkConstraint :: StoreError -> ExceptT StoreError IO a -> ExceptT StoreError IO a
checkConstraint err action = ExceptT $ runExceptT action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | DB.sqlError e == DB.ErrorConstraint = err
  | otherwise = SEInternalError $ show e

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid()"

createUser :: DB.Connection -> Profile -> Bool -> ExceptT StoreError IO User
createUser db Profile {displayName, fullName, image, preferences = userPreferences} activeUser =
  checkConstraint SEDuplicateName . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO users (local_display_name, active_user, contact_id, created_at, updated_at) VALUES (?,?,0,?,?)"
      (displayName, activeUser, currentTs, currentTs)
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
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, is_user, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (profileId, displayName, userId, True, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure $ toUser (userId, contactId, profileId, activeUser, displayName, fullName, image, userPreferences)

getUsers :: DB.Connection -> IO [User]
getUsers db =
  map toUser
    <$> DB.query_
      db
      [sql|
        SELECT u.user_id, u.contact_id, p.contact_profile_id, u.active_user, u.local_display_name, p.full_name, p.image, p.preferences
        FROM users u
        JOIN contacts c ON u.contact_id = c.contact_id
        JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
      |]

toUser :: (UserId, ContactId, ProfileId, Bool, ContactName, Text, Maybe ImageData, Maybe Preferences) -> User
toUser (userId, userContactId, profileId, activeUser, displayName, fullName, image, userPreferences) =
  let profile = LocalProfile {profileId, displayName, fullName, image, preferences = userPreferences, localAlias = ""}
   in User {userId, userContactId, localDisplayName = displayName, profile, activeUser, fullPreferences = mergePreferences Nothing userPreferences}

setActiveUser :: DB.Connection -> UserId -> IO ()
setActiveUser db userId = do
  DB.execute_ db "UPDATE users SET active_user = 0"
  DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?" (Only userId)

createConnReqConnection :: DB.Connection -> UserId -> ConnId -> ConnReqUriHash -> XContactId -> Maybe Profile -> Maybe GroupLinkId -> IO PendingContactConnection
createConnReqConnection db userId acId cReqHash xContactId incognitoProfile groupLinkId = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  let pccConnStatus = ConnJoined
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_status, conn_type,
        via_contact_uri_hash, xcontact_id, custom_user_profile_id, via_group_link, group_link_id, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, acId, pccConnStatus, ConnContact, cReqHash, xContactId) :. (customUserProfileId, isJust groupLinkId, groupLinkId, createdAt, createdAt))
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = True, viaUserContactLink = Nothing, groupLinkId, customUserProfileId, connReqInv = Nothing, localAlias = "", createdAt, updatedAt = createdAt}

getConnReqContactXContactId :: DB.Connection -> User -> ConnReqUriHash -> IO (Maybe Contact, Maybe XContactId)
getConnReqContactXContactId db user@User {userId} cReqHash = do
  getContact' >>= \case
    c@(Just _) -> pure (c, Nothing)
    Nothing -> (Nothing,) <$> getXContactId
  where
    getContact' :: IO (Maybe Contact)
    getContact' =
      maybeFirstRow (toContact user) $
        DB.query
          db
          [sql|
            SELECT
              -- Contact
              ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.local_alias, ct.contact_used, ct.enable_ntfs, cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
              -- Connection
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
              c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
            FROM contacts ct
            JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
            JOIN connections c ON c.contact_id = ct.contact_id
            WHERE ct.user_id = ? AND c.via_contact_uri_hash = ?
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          (userId, cReqHash)
    getXContactId :: IO (Maybe XContactId)
    getXContactId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT xcontact_id FROM connections WHERE user_id = ? AND via_contact_uri_hash = ? LIMIT 1"
          (userId, cReqHash)

createDirectConnection :: DB.Connection -> UserId -> ConnId -> ConnReqInvitation -> ConnStatus -> Maybe Profile -> IO PendingContactConnection
createDirectConnection db userId acId cReq pccConnStatus incognitoProfile = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  DB.execute
    db
    [sql|
      INSERT INTO connections
        (user_id, agent_conn_id, conn_req_inv, conn_status, conn_type, custom_user_profile_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)
    |]
    (userId, acId, cReq, pccConnStatus, ConnContact, customUserProfileId, createdAt, createdAt)
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = False, viaUserContactLink = Nothing, groupLinkId = Nothing, customUserProfileId, connReqInv = Just cReq, localAlias = "", createdAt, updatedAt = createdAt}

createIncognitoProfile_ :: DB.Connection -> UserId -> UTCTime -> Profile -> IO Int64
createIncognitoProfile_ db userId createdAt Profile {displayName, fullName, image} = do
  DB.execute
    db
    [sql|
      INSERT INTO contact_profiles (display_name, full_name, image, user_id, incognito, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?)
    |]
    (displayName, fullName, image, userId, Just True, createdAt, createdAt)
  insertedRowId db

getProfileById :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO LocalProfile
getProfileById db userId profileId =
  ExceptT . firstRow toProfile (SEProfileNotFound profileId) $
    DB.query
      db
      [sql|
        SELECT cp.display_name, cp.full_name, cp.image, cp.local_alias, cp.preferences -- , ct.user_preferences
        FROM contact_profiles cp
          -- JOIN contacts ct ON cp.contact_profile_id = ct.contact_profile_id
        WHERE cp.user_id = ? AND cp.contact_profile_id = ?
      |]
      (userId, profileId)
  where
    toProfile :: (ContactName, Text, Maybe ImageData, LocalAlias, Maybe Preferences) -> LocalProfile
    toProfile (displayName, fullName, image, localAlias, preferences) = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}

createConnection_ :: DB.Connection -> UserId -> ConnType -> Maybe Int64 -> ConnId -> Maybe ContactId -> Maybe Int64 -> Maybe ProfileId -> Int -> UTCTime -> IO Connection
createConnection_ db userId connType entityId acId viaContact viaUserContactLink customUserProfileId connLevel currentTs = do
  viaLinkGroupId :: Maybe Int64 <- fmap join . forM viaUserContactLink $ \ucLinkId ->
    maybeFirstRow fromOnly $ DB.query db "SELECT group_id FROM user_contact_links WHERE user_id = ? AND user_contact_link_id = ? AND group_id IS NOT NULL" (userId, ucLinkId)
  let viaGroupLink = isJust viaLinkGroupId
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, custom_user_profile_id, conn_status, conn_type,
        contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, acId, connLevel, viaContact, viaUserContactLink, viaGroupLink, customUserProfileId, ConnNew, connType)
        :. (ent ConnContact, ent ConnMember, ent ConnSndFile, ent ConnRcvFile, ent ConnUserContact, currentTs, currentTs)
    )
  connId <- insertedRowId db
  pure Connection {connId, agentConnId = AgentConnId acId, connType, entityId, viaContact, viaUserContactLink, viaGroupLink, groupLinkId = Nothing, customUserProfileId, connLevel, connStatus = ConnNew, localAlias = "", createdAt = currentTs, connectionCode = Nothing}
  where
    ent ct = if connType == ct then entityId else Nothing

createDirectContact :: DB.Connection -> User -> Connection -> Profile -> ExceptT StoreError IO Contact
createDirectContact db user@User {userId} activeConn@Connection {connId, localAlias} p@Profile {preferences} = do
  createdAt <- liftIO getCurrentTime
  (localDisplayName, contactId, profileId) <- createContact_ db userId connId p localAlias Nothing createdAt
  let profile = toLocalProfile profileId p localAlias
      userPreferences = emptyChatPrefs
      mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
  pure $ Contact {contactId, localDisplayName, profile, activeConn, viaGroup = Nothing, contactUsed = False, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt, updatedAt = createdAt}

createContact_ :: DB.Connection -> UserId -> Int64 -> Profile -> LocalAlias -> Maybe Int64 -> UTCTime -> ExceptT StoreError IO (Text, ContactId, ProfileId)
createContact_ db userId connId Profile {displayName, fullName, image, preferences} localAlias viaGroup currentTs =
  ExceptT . withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, image, user_id, local_alias, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (displayName, fullName, image, userId, localAlias, preferences, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, via_group, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (profileId, ldn, userId, viaGroup, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, currentTs, connId)
    pure . Right $ (ldn, contactId, profileId)

deleteContactConnectionsAndFiles :: DB.Connection -> UserId -> Contact -> IO ()
deleteContactConnectionsAndFiles db userId Contact {contactId} = do
  DB.execute
    db
    [sql|
      DELETE FROM connections WHERE connection_id IN (
        SELECT connection_id
        FROM connections c
        JOIN contacts ct ON ct.contact_id = c.contact_id
        WHERE ct.user_id = ? AND ct.contact_id = ?
      )
    |]
    (userId, contactId)
  DB.execute db "DELETE FROM files WHERE user_id = ? AND contact_id = ?" (userId, contactId)

deleteContact :: DB.Connection -> User -> Contact -> IO ()
deleteContact db user@User {userId} Contact {contactId, localDisplayName, activeConn = Connection {customUserProfileId}} = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  ctMember :: (Maybe ContactId) <- maybeFirstRow fromOnly $ DB.query db "SELECT contact_id FROM group_members WHERE user_id = ? AND contact_id = ? LIMIT 1" (userId, contactId)
  if isNothing ctMember
    then do
      deleteContactProfile_ db userId contactId
      DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
    else do
      currentTs <- getCurrentTime
      DB.execute db "UPDATE group_members SET contact_id = NULL, updated_at = ? WHERE user_id = ? AND contact_id = ?" (currentTs, userId, contactId)
  DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  forM_ customUserProfileId $ \profileId -> deleteUnusedIncognitoProfileById_ db user profileId

-- should only be used if contact is not member of any groups
deleteContactWithoutGroups :: DB.Connection -> User -> Contact -> IO ()
deleteContactWithoutGroups db user@User {userId} Contact {contactId, localDisplayName, activeConn = Connection {customUserProfileId}} = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  deleteContactProfile_ db userId contactId
  DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
  DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  forM_ customUserProfileId $ \profileId -> deleteUnusedIncognitoProfileById_ db user profileId

deleteUnusedIncognitoProfileById_ :: DB.Connection -> User -> ProfileId -> IO ()
deleteUnusedIncognitoProfileById_ db User {userId} profile_id =
  DB.executeNamed
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = :user_id AND contact_profile_id = :profile_id AND incognito = 1
        AND 1 NOT IN (
          SELECT 1 FROM connections
          WHERE user_id = :user_id AND custom_user_profile_id = :profile_id LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = :user_id AND member_profile_id = :profile_id LIMIT 1
        )
    |]
    [":user_id" := userId, ":profile_id" := profile_id]

deleteContactProfile_ :: DB.Connection -> UserId -> ContactId -> IO ()
deleteContactProfile_ db userId contactId =
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT contact_profile_id
        FROM contacts
        WHERE user_id = ? AND contact_id = ?
      )
    |]
    (userId, contactId)

updateUserProfile :: DB.Connection -> User -> Profile -> ExceptT StoreError IO ()
updateUserProfile db User {userId, userContactId, localDisplayName, profile = LocalProfile {profileId, displayName}} p'@Profile {displayName = newName}
  | displayName == newName =
    liftIO $ updateContactProfile_ db userId profileId p'
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

updateContactProfile :: DB.Connection -> User -> Contact -> Profile -> ExceptT StoreError IO Contact
updateContactProfile db user@User {userId} c p'
  | displayName == newName = do
    liftIO $ updateContactProfile_ db userId profileId p'
    pure $ c {profile, mergedPreferences}
  | otherwise =
    ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
      currentTs <- getCurrentTime
      updateContactProfile_' db userId profileId p' currentTs
      updateContact_ db userId contactId localDisplayName ldn currentTs
      pure . Right $ c {localDisplayName = ldn, profile, mergedPreferences}
  where
    Contact {contactId, localDisplayName, profile = LocalProfile {profileId, displayName, localAlias}, activeConn, userPreferences} = c
    Profile {displayName = newName, preferences} = p'
    profile = toLocalProfile profileId p' localAlias
    mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn

updateContactUserPreferences :: DB.Connection -> User -> Contact -> Preferences -> IO Contact
updateContactUserPreferences db user@User {userId} c@Contact {contactId, activeConn} userPreferences = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    "UPDATE contacts SET user_preferences = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (userPreferences, updatedAt, userId, contactId)
  let mergedPreferences = contactUserPreferences user userPreferences (preferences' c) $ connIncognito activeConn
  pure $ c {mergedPreferences, userPreferences}

updateContactAlias :: DB.Connection -> UserId -> Contact -> LocalAlias -> IO Contact
updateContactAlias db userId c@Contact {profile = lp@LocalProfile {profileId}} localAlias = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET local_alias = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (localAlias, updatedAt, userId, profileId)
  pure $ (c :: Contact) {profile = lp {localAlias}}

updateContactConnectionAlias :: DB.Connection -> UserId -> PendingContactConnection -> LocalAlias -> IO PendingContactConnection
updateContactConnectionAlias db userId conn localAlias = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET local_alias = ?, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (localAlias, updatedAt, userId, pccConnId conn)
  pure (conn :: PendingContactConnection) {localAlias}

updateContactUsed :: DB.Connection -> User -> Contact -> IO ()
updateContactUsed db User {userId} Contact {contactId} = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET contact_used = 1, updated_at = ? WHERE user_id = ? AND contact_id = ?" (updatedAt, userId, contactId)

updateContactUnreadChat :: DB.Connection -> User -> Contact -> Bool -> IO ()
updateContactUnreadChat db User {userId} Contact {contactId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (unreadChat, updatedAt, userId, contactId)

updateGroupUnreadChat :: DB.Connection -> User -> GroupInfo -> Bool -> IO ()
updateGroupUnreadChat db User {userId} GroupInfo {groupId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE groups SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (unreadChat, updatedAt, userId, groupId)

setConnectionVerified :: DB.Connection -> User -> Int64 -> Maybe Text -> IO ()
setConnectionVerified db User {userId} connId code = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET security_code = ?, security_code_verified_at = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (code, code $> updatedAt, updatedAt, userId, connId)

updateContactProfile_ :: DB.Connection -> UserId -> ProfileId -> Profile -> IO ()
updateContactProfile_ db userId profileId profile = do
  currentTs <- getCurrentTime
  updateContactProfile_' db userId profileId profile currentTs

updateContactProfile_' :: DB.Connection -> UserId -> ProfileId -> Profile -> UTCTime -> IO ()
updateContactProfile_' db userId profileId Profile {displayName, fullName, image, preferences} updatedAt = do
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET display_name = ?, full_name = ?, image = ?, preferences = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (displayName, fullName, image, preferences, updatedAt, userId, profileId)

updateContact_ :: DB.Connection -> UserId -> Int64 -> ContactName -> ContactName -> UTCTime -> IO ()
updateContact_ db userId contactId displayName newName updatedAt = do
  DB.execute
    db
    "UPDATE contacts SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  DB.execute
    db
    "UPDATE group_members SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (displayName, userId)

type ContactRow = (ContactId, ProfileId, ContactName, Maybe Int64, ContactName, Text, Maybe ImageData, LocalAlias, Bool, Maybe Bool) :. (Maybe Preferences, Preferences, UTCTime, UTCTime)

toContact :: User -> ContactRow :. ConnectionRow -> Contact
toContact user (((contactId, profileId, localDisplayName, viaGroup, displayName, fullName, image, localAlias, contactUsed, enableNtfs_) :. (preferences, userPreferences, createdAt, updatedAt)) :. connRow) =
  let profile = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}
      activeConn = toConnection connRow
      chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_}
      mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
   in Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt}

toContactOrError :: User -> ContactRow :. MaybeConnectionRow -> Either StoreError Contact
toContactOrError user (((contactId, profileId, localDisplayName, viaGroup, displayName, fullName, image, localAlias, contactUsed, enableNtfs_) :. (preferences, userPreferences, createdAt, updatedAt)) :. connRow) =
  let profile = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}
      chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_}
   in case toMaybeConnection connRow of
        Just activeConn ->
          let mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
           in Right Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt}
        _ -> Left $ SEContactNotReady localDisplayName

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContactByName :: DB.Connection -> User -> ContactName -> ExceptT StoreError IO Contact
getContactByName db user localDisplayName = do
  cId <- getContactIdByName db user localDisplayName
  getContact db user cId

getUserContacts :: DB.Connection -> User -> IO [Contact]
getUserContacts db user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ?" (Only userId)
  rights <$> mapM (runExceptT . getContact db user) contactIds

-- only used in tests
getUserContactProfiles :: DB.Connection -> User -> IO [Profile]
getUserContactProfiles db User {userId} =
  map toContactProfile
    <$> DB.query
      db
      [sql|
        SELECT display_name, full_name, image, preferences
        FROM contact_profiles
        WHERE user_id = ?
      |]
      (Only userId)
  where
    toContactProfile :: (ContactName, Text, Maybe ImageData, Maybe Preferences) -> (Profile)
    toContactProfile (displayName, fullName, image, preferences) = Profile {displayName, fullName, image, preferences}

createUserContactLink :: DB.Connection -> UserId -> ConnId -> ConnReqContact -> ExceptT StoreError IO ()
createUserContactLink db userId agentConnId cReq =
  checkConstraint SEDuplicateContactLink . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, conn_req_contact, created_at, updated_at) VALUES (?,?,?,?)"
      (userId, cReq, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId Nothing Nothing Nothing 0 currentTs

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
              c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
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
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at,
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
deleteUserAddress db User {userId} = do
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
  DB.execute db "DELETE FROM user_contact_links WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL" (Only userId)

data UserContactLink = UserContactLink
  { connReqContact :: ConnReqContact,
    autoAccept :: Maybe AutoAccept
  }
  deriving (Show, Generic)

instance ToJSON UserContactLink where toEncoding = J.genericToEncoding J.defaultOptions

data AutoAccept = AutoAccept
  { acceptIncognito :: Bool,
    autoReply :: Maybe MsgContent
  }
  deriving (Show, Generic)

instance ToJSON AutoAccept where toEncoding = J.genericToEncoding J.defaultOptions

toUserContactLink :: (ConnReqContact, Bool, Bool, Maybe MsgContent) -> UserContactLink
toUserContactLink (connReq, autoAccept, acceptIncognito, autoReply) =
  UserContactLink connReq $
    if autoAccept then Just AutoAccept {acceptIncognito, autoReply} else Nothing

getUserAddress :: DB.Connection -> UserId -> ExceptT StoreError IO UserContactLink
getUserAddress db userId =
  ExceptT . firstRow toUserContactLink SEUserContactLinkNotFound $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, auto_accept, auto_accept_incognito, auto_reply_msg_content
        FROM user_contact_links
        WHERE user_id = ? AND local_display_name = '' AND group_id IS NULL
      |]
      (Only userId)

getUserContactLinkById :: DB.Connection -> UserId -> Int64 -> IO (Maybe (UserContactLink, Maybe GroupId))
getUserContactLinkById db userId userContactLinkId =
  maybeFirstRow (\(ucl :. Only groupId_) -> (toUserContactLink ucl, groupId_)) $
    DB.query
      db
      [sql|
        SELECT conn_req_contact, auto_accept, auto_accept_incognito, auto_reply_msg_content, group_id
        FROM user_contact_links
        WHERE user_id = ?
          AND user_contact_link_id = ?
      |]
      (userId, userContactLinkId)

updateUserAddressAutoAccept :: DB.Connection -> UserId -> Maybe AutoAccept -> ExceptT StoreError IO UserContactLink
updateUserAddressAutoAccept db userId autoAccept = do
  link <- getUserAddress db userId
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

createGroupLink :: DB.Connection -> User -> GroupInfo -> ConnId -> ConnReqContact -> GroupLinkId -> ExceptT StoreError IO ()
createGroupLink db User {userId} groupInfo@GroupInfo {groupId, localDisplayName} agentConnId cReq groupLinkId =
  checkConstraint (SEDuplicateGroupLink groupInfo) . liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, group_id, group_link_id, local_display_name, conn_req_contact, auto_accept, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (userId, groupId, groupLinkId, "group_link_" <> localDisplayName, cReq, True, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    void $ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId Nothing Nothing Nothing 0 currentTs

getGroupLinkConnection :: DB.Connection -> User -> GroupInfo -> ExceptT StoreError IO Connection
getGroupLinkConnection db User {userId} groupInfo@GroupInfo {groupId} =
  ExceptT . firstRow toConnection (SEGroupLinkNotFound groupInfo) $
    DB.query
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
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

getGroupLink :: DB.Connection -> User -> GroupInfo -> ExceptT StoreError IO ConnReqContact
getGroupLink db User {userId} gInfo@GroupInfo {groupId} =
  ExceptT . firstRow fromOnly (SEGroupLinkNotFound gInfo) $
    DB.query db "SELECT conn_req_contact FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)

getGroupLinkId :: DB.Connection -> User -> GroupInfo -> IO (Maybe GroupLinkId)
getGroupLinkId db User {userId} GroupInfo {groupId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT group_link_id FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1" (userId, groupId)

createOrUpdateContactRequest :: DB.Connection -> User -> Int64 -> InvitationId -> Profile -> Maybe XContactId -> ExceptT StoreError IO ContactOrRequest
createOrUpdateContactRequest db user@User {userId} userContactLinkId invId Profile {displayName, fullName, image, preferences} xContactId_ =
  liftIO (maybeM getContact' xContactId_) >>= \case
    Just contact -> pure $ CORContact contact
    Nothing -> CORRequest <$> createOrUpdate_
  where
    maybeM = maybe (pure Nothing)
    createOrUpdate_ :: ExceptT StoreError IO UserContactRequest
    createOrUpdate_ = do
      cReqId <-
        ExceptT $
          maybeM getContactRequest' xContactId_ >>= \case
            Nothing -> createContactRequest
            Just cr -> updateContactRequest cr $> Right (contactRequestId (cr :: UserContactRequest))
      getContactRequest db userId cReqId
    createContactRequest :: IO (Either StoreError Int64)
    createContactRequest = do
      currentTs <- getCurrentTime
      withLocalDisplayName db userId displayName (fmap Right . createContactRequest_ currentTs)
      where
        createContactRequest_ currentTs ldn = do
          DB.execute
            db
            "INSERT INTO contact_profiles (display_name, full_name, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
            (displayName, fullName, image, userId, preferences, currentTs, currentTs)
          profileId <- insertedRowId db
          DB.execute
            db
            [sql|
              INSERT INTO contact_requests
                (user_contact_link_id, agent_invitation_id, contact_profile_id, local_display_name, user_id, created_at, updated_at, xcontact_id)
              VALUES (?,?,?,?,?,?,?,?)
            |]
            (userContactLinkId, invId, profileId, ldn, userId, currentTs, currentTs, xContactId_)
          insertedRowId db
    getContact' :: XContactId -> IO (Maybe Contact)
    getContact' xContactId =
      maybeFirstRow (toContact user) $
        DB.query
          db
          [sql|
            SELECT
              -- Contact
              ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.local_alias, ct.contact_used, ct.enable_ntfs, cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
              -- Connection
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
              c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
            FROM contacts ct
            JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
            LEFT JOIN connections c ON c.contact_id = ct.contact_id
            WHERE ct.user_id = ? AND ct.xcontact_id = ?
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          (userId, xContactId)
    getContactRequest' :: XContactId -> IO (Maybe UserContactRequest)
    getContactRequest' xContactId =
      maybeFirstRow toContactRequest $
        DB.query
          db
          [sql|
            SELECT
              cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
              c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at
            FROM contact_requests cr
            JOIN connections c USING (user_contact_link_id)
            JOIN contact_profiles p USING (contact_profile_id)
            WHERE cr.user_id = ?
              AND cr.xcontact_id = ?
            LIMIT 1
          |]
          (userId, xContactId)
    updateContactRequest :: UserContactRequest -> IO (Either StoreError ())
    updateContactRequest UserContactRequest {contactRequestId = cReqId, localDisplayName = oldLdn, profile = Profile {displayName = oldDisplayName}} = do
      currentTs <- liftIO getCurrentTime
      updateProfile currentTs
      if displayName == oldDisplayName
        then Right <$> DB.execute db "UPDATE contact_requests SET agent_invitation_id = ?, updated_at = ? WHERE user_id = ? AND contact_request_id = ?" (invId, currentTs, userId, cReqId)
        else withLocalDisplayName db userId displayName $ \ldn ->
          Right <$> do
            DB.execute db "UPDATE contact_requests SET agent_invitation_id = ?, local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_request_id = ?" (invId, ldn, currentTs, userId, cReqId)
            DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (oldLdn, userId)
      where
        updateProfile currentTs =
          DB.execute
            db
            [sql|
              UPDATE contact_profiles
              SET display_name = ?,
                  full_name = ?,
                  image = ?,
                  updated_at = ?
              WHERE contact_profile_id IN (
                SELECT contact_profile_id
                FROM contact_requests
                WHERE user_id = ?
                  AND contact_request_id = ?
              )
            |]
            (displayName, fullName, image, currentTs, userId, cReqId)

getContactRequest :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO UserContactRequest
getContactRequest db userId contactRequestId =
  ExceptT . firstRow toContactRequest (SEContactRequestNotFound contactRequestId) $
    DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at
        FROM contact_requests cr
        JOIN connections c USING (user_contact_link_id)
        JOIN contact_profiles p USING (contact_profile_id)
        WHERE cr.user_id = ?
          AND cr.contact_request_id = ?
      |]
      (userId, contactRequestId)

type ContactRequestRow = (Int64, ContactName, AgentInvId, Int64, AgentConnId, Int64, ContactName, Text, Maybe ImageData) :. (Maybe XContactId, Maybe Preferences, UTCTime, UTCTime)

toContactRequest :: ContactRequestRow -> UserContactRequest
toContactRequest ((contactRequestId, localDisplayName, agentInvitationId, userContactLinkId, agentContactConnId, profileId, displayName, fullName, image) :. (xContactId, preferences, createdAt, updatedAt)) = do
  let profile = Profile {displayName, fullName, image, preferences}
   in UserContactRequest {contactRequestId, agentInvitationId, userContactLinkId, agentContactConnId, localDisplayName, profileId, profile, xContactId, createdAt, updatedAt}

getContactRequestIdByName :: DB.Connection -> UserId -> ContactName -> ExceptT StoreError IO Int64
getContactRequestIdByName db userId cName =
  ExceptT . firstRow fromOnly (SEContactRequestNotFoundByName cName) $
    DB.query db "SELECT contact_request_id FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, cName)

deleteContactRequest :: DB.Connection -> UserId -> Int64 -> IO ()
deleteContactRequest db userId contactRequestId = do
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT contact_profile_id
        FROM contact_requests
        WHERE user_id = ? AND contact_request_id = ?
      )
    |]
    (userId, contactRequestId)
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ? AND local_display_name = (
        SELECT local_display_name FROM contact_requests
        WHERE user_id = ? AND contact_request_id = ?
      )
    |]
    (userId, userId, contactRequestId)
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND contact_request_id = ?" (userId, contactRequestId)

createAcceptedContact :: DB.Connection -> User -> ConnId -> ContactName -> ProfileId -> Profile -> Int64 -> Maybe XContactId -> Maybe IncognitoProfile -> IO Contact
createAcceptedContact db user@User {userId, profile = LocalProfile {preferences}} agentConnId localDisplayName profileId profile userContactLinkId xContactId incognitoProfile = do
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
  createdAt <- getCurrentTime
  customUserProfileId <- forM incognitoProfile $ \case
    NewIncognito p -> createIncognitoProfile_ db userId createdAt p
    ExistingIncognito LocalProfile {profileId = pId} -> pure pId
  let userPreferences = fromMaybe emptyChatPrefs $ incognitoProfile >> preferences
  DB.execute
    db
    "INSERT INTO contacts (user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, created_at, updated_at, xcontact_id) VALUES (?,?,?,?,?,?,?,?)"
    (userId, localDisplayName, profileId, True, userPreferences, createdAt, createdAt, xContactId)
  contactId <- insertedRowId db
  activeConn <- createConnection_ db userId ConnContact (Just contactId) agentConnId Nothing (Just userContactLinkId) customUserProfileId 0 createdAt
  let mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
  pure $ Contact {contactId, localDisplayName, profile = toLocalProfile profileId profile "", activeConn, viaGroup = Nothing, contactUsed = False, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = createdAt, updatedAt = createdAt}

getLiveSndFileTransfers :: DB.Connection -> User -> IO [SndFileTransfer]
getLiveSndFileTransfers db User {userId} = do
  fileIds :: [Int64] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT DISTINCT f.file_id
          FROM files f
          JOIN snd_files s
          WHERE f.user_id = ? AND s.file_status IN (?, ?, ?) AND s.file_inline IS NULL
        |]
        (userId, FSNew, FSAccepted, FSConnected)
  concatMap (filter liveTransfer) . rights <$> mapM (getSndFileTransfers_ db userId) fileIds
  where
    liveTransfer :: SndFileTransfer -> Bool
    liveTransfer SndFileTransfer {fileStatus} = fileStatus `elem` [FSNew, FSAccepted, FSConnected]

getLiveRcvFileTransfers :: DB.Connection -> User -> IO [RcvFileTransfer]
getLiveRcvFileTransfers db user@User {userId} = do
  fileIds :: [Int64] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT f.file_id
          FROM files f
          JOIN rcv_files r
          WHERE f.user_id = ? AND r.file_status IN (?, ?) AND r.rcv_file_inline IS NULL
        |]
        (userId, FSAccepted, FSConnected)
  rights <$> mapM (runExceptT . getRcvFileTransfer db user) fileIds

getPendingSndChunks :: DB.Connection -> Int64 -> Int64 -> IO [Integer]
getPendingSndChunks db fileId connId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chunk_number
        FROM snd_file_chunks
        WHERE file_id = ? AND connection_id = ? AND chunk_agent_msg_id IS NULL
        ORDER BY chunk_number
      |]
      (fileId, connId)

getPendingContactConnections :: DB.Connection -> User -> IO [PendingContactConnection]
getPendingContactConnections db User {userId} = do
  map toPendingContactConnection
    <$> DB.queryNamed
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = :user_id
          AND conn_type = :conn_type
          AND contact_id IS NULL
      |]
      [":user_id" := userId, ":conn_type" := ConnContact]

getContactConnections :: DB.Connection -> UserId -> Contact -> ExceptT StoreError IO [Connection]
getContactConnections db userId Contact {contactId} =
  connections =<< liftIO getConnections_
  where
    getConnections_ =
      DB.query
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
            c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
          FROM connections c
          JOIN contacts ct ON ct.contact_id = c.contact_id
          WHERE c.user_id = ? AND ct.user_id = ? AND ct.contact_id = ?
        |]
        (userId, userId, contactId)
    connections [] = throwError $ SEContactNotFound contactId
    connections rows = pure $ map toConnection rows

type EntityIdsRow = (Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64)

type ConnectionRow = (Int64, ConnId, Int, Maybe Int64, Maybe Int64, Bool, Maybe GroupLinkId, Maybe Int64, ConnStatus, ConnType, LocalAlias) :. EntityIdsRow :. (UTCTime, Maybe Text, Maybe UTCTime)

type MaybeConnectionRow = (Maybe Int64, Maybe ConnId, Maybe Int, Maybe Int64, Maybe Int64, Maybe Bool, Maybe GroupLinkId, Maybe Int64, Maybe ConnStatus, Maybe ConnType, Maybe LocalAlias) :. EntityIdsRow :. (Maybe UTCTime, Maybe Text, Maybe UTCTime)

toConnection :: ConnectionRow -> Connection
toConnection ((connId, acId, connLevel, viaContact, viaUserContactLink, viaGroupLink, groupLinkId, customUserProfileId, connStatus, connType, localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (createdAt, code_, verifiedAt_)) =
  let entityId = entityId_ connType
      connectionCode = SecurityCode <$> code_ <*> verifiedAt_
   in Connection {connId, agentConnId = AgentConnId acId, connLevel, viaContact, viaUserContactLink, viaGroupLink, groupLinkId, customUserProfileId, connStatus, connType, localAlias, entityId, connectionCode, createdAt}
  where
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId
    entityId_ ConnRcvFile = rcvFileId
    entityId_ ConnSndFile = sndFileId
    entityId_ ConnUserContact = userContactLinkId

toMaybeConnection :: MaybeConnectionRow -> Maybe Connection
toMaybeConnection ((Just connId, Just agentConnId, Just connLevel, viaContact, viaUserContactLink, Just viaGroupLink, groupLinkId, customUserProfileId, Just connStatus, Just connType, Just localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (Just createdAt, code_, verifiedAt_)) =
  Just $ toConnection ((connId, agentConnId, connLevel, viaContact, viaUserContactLink, viaGroupLink, groupLinkId, customUserProfileId, connStatus, connType, localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (createdAt, code_, verifiedAt_))
toMaybeConnection _ = Nothing

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
          WHERE c.user_id = ? AND r.probe_hash = ? AND r.probe IS NULL
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
        WHERE c.user_id = ? AND r.probe_hash = ? AND r.probe IS NOT NULL
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
          WHERE c.user_id = ? AND s.probe = ? AND h.contact_id = ?
        |]
        (userId, probe, contactId)
  case contactIds of
    [] -> pure Nothing
    cId : _ -> eitherToMaybe <$> runExceptT (getContact db user cId)

mergeContactRecords :: DB.Connection -> UserId -> Contact -> Contact -> IO ()
mergeContactRecords db userId Contact {contactId = toContactId} Contact {contactId = fromContactId, localDisplayName} = do
  currentTs <- getCurrentTime
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

getConnectionEntity :: DB.Connection -> User -> AgentConnId -> ExceptT StoreError IO ConnectionEntity
getConnectionEntity db user@User {userId, userContactId} agentConnId = do
  c@Connection {connType, entityId} <- getConnection_
  case entityId of
    Nothing ->
      if connType == ConnContact
        then pure $ RcvDirectMsgConnection c Nothing
        else throwError $ SEInternalError $ "connection " <> show connType <> " without entity"
    Just entId ->
      case connType of
        ConnMember -> uncurry (RcvGroupMsgConnection c) <$> getGroupAndMember_ entId c
        ConnContact -> RcvDirectMsgConnection c . Just <$> getContactRec_ entId c
        ConnSndFile -> SndFileConnection c <$> getConnSndFileTransfer_ entId c
        ConnRcvFile -> RcvFileConnection c <$> getRcvFileTransfer db user entId
        ConnUserContact -> UserContactConnection c <$> getUserContact_ entId
  where
    getConnection_ :: ExceptT StoreError IO Connection
    getConnection_ = ExceptT $ do
      firstRow toConnection (SEConnectionNotFound agentConnId) $
        DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, custom_user_profile_id,
              conn_status, conn_type, local_alias, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, security_code, security_code_verified_at
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?
          |]
          (userId, agentConnId)
    getContactRec_ :: Int64 -> Connection -> ExceptT StoreError IO Contact
    getContactRec_ contactId c = ExceptT $ do
      toContact' contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.contact_profile_id, c.local_display_name, p.display_name, p.full_name, p.image, p.local_alias, c.via_group, c.contact_used, c.enable_ntfs, p.preferences, c.user_preferences, c.created_at, c.updated_at
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact' :: Int64 -> Connection -> [(ProfileId, ContactName, Text, Text, Maybe ImageData, LocalAlias, Maybe Int64, Bool, Maybe Bool) :. (Maybe Preferences, Preferences, UTCTime, UTCTime)] -> Either StoreError Contact
    toContact' contactId activeConn [(profileId, localDisplayName, displayName, fullName, image, localAlias, viaGroup, contactUsed, enableNtfs_) :. (preferences, userPreferences, createdAt, updatedAt)] =
      let profile = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}
          chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_}
          mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
       in Right Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt}
    toContact' _ _ _ = Left $ SEInternalError "referenced contact not found"
    getGroupAndMember_ :: Int64 -> Connection -> ExceptT StoreError IO (GroupInfo, GroupMember)
    getGroupAndMember_ groupMemberId c = ExceptT $ do
      firstRow (toGroupAndMember c) (SEInternalError "referenced group member not found") $
        DB.query
          db
          [sql|
            SELECT
              -- GroupInfo
              g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
              -- GroupInfo {membership}
              mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
              mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
              -- GroupInfo {membership = GroupMember {memberProfile}}
              pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences,
              -- from GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
              m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias, p.preferences
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
            JOIN groups g ON g.group_id = m.group_id
            JOIN group_profiles gp USING (group_profile_id)
            JOIN group_members mu ON g.group_id = mu.group_id
            JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
            WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
          |]
          (groupMemberId, userId, userContactId)
    toGroupAndMember :: Connection -> GroupInfoRow :. GroupMemberRow -> (GroupInfo, GroupMember)
    toGroupAndMember c (groupInfoRow :. memberRow) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = Just c})
    getConnSndFileTransfer_ :: Int64 -> Connection -> ExceptT StoreError IO SndFileTransfer
    getConnSndFileTransfer_ fileId Connection {connId} =
      ExceptT $
        firstRow' (sndFileTransfer_ fileId connId) (SESndFileNotFound fileId) $
          DB.query
            db
            [sql|
              SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_inline, cs.local_display_name, m.local_display_name
              FROM snd_files s
              JOIN files f USING (file_id)
              LEFT JOIN contacts cs USING (contact_id)
              LEFT JOIN group_members m USING (group_member_id)
              WHERE f.user_id = ? AND f.file_id = ? AND s.connection_id = ?
            |]
            (userId, fileId, connId)
    sndFileTransfer_ :: Int64 -> Int64 -> (FileStatus, String, Integer, Integer, FilePath, Maybe InlineFileMode, Maybe ContactName, Maybe ContactName) -> Either StoreError SndFileTransfer
    sndFileTransfer_ fileId connId (fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, contactName_, memberName_) =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, recipientDisplayName, connId, agentConnId}
        Nothing -> Left $ SESndFileInvalid fileId
    getUserContact_ :: Int64 -> ExceptT StoreError IO UserContact
    getUserContact_ userContactLinkId = ExceptT $ do
      userContact_
        <$> DB.query
          db
          [sql|
            SELECT conn_req_contact, group_id
            FROM user_contact_links
            WHERE user_id = ? AND user_contact_link_id = ?
          |]
          (userId, userContactLinkId)
      where
        userContact_ :: [(ConnReqContact, Maybe GroupId)] -> Either StoreError UserContact
        userContact_ [(cReq, groupId)] = Right UserContact {userContactLinkId, connReqContact = cReq, groupId}
        userContact_ _ = Left SEUserContactLinkNotFound

getConnectionById :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO Connection
getConnectionById db User {userId} connId = ExceptT $ do
  firstRow toConnection (SEConnectionNotFoundById connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, custom_user_profile_id,
          conn_status, conn_type, local_alias, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, security_code, security_code_verified_at
        FROM connections
        WHERE user_id = ? AND connection_id = ?
      |]
      (userId, connId)

getConnectionsContacts :: DB.Connection -> UserId -> [ConnId] -> IO [ContactRef]
getConnectionsContacts db userId agentConnIds = do
  DB.execute_ db "DROP TABLE IF EXISTS temp.conn_ids"
  DB.execute_ db "CREATE TABLE temp.conn_ids (conn_id BLOB)"
  DB.executeMany db "INSERT INTO temp.conn_ids (conn_id) VALUES (?)" $ map Only agentConnIds
  conns <-
    map (uncurry ContactRef)
      <$> DB.query
        db
        [sql|
          SELECT ct.contact_id, ct.local_display_name
          FROM contacts ct
          JOIN connections c ON c.contact_id = ct.contact_id
          WHERE ct.user_id = ?
            AND c.agent_conn_id IN (SELECT conn_id FROM temp.conn_ids)
            AND c.conn_type = ?
        |]
        (userId, ConnContact)
  DB.execute_ db "DROP TABLE temp.conn_ids"
  pure conns

getGroupAndMember :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (GroupInfo, GroupMember)
getGroupAndMember db User {userId, userContactId} groupMemberId =
  ExceptT . firstRow toGroupAndMember (SEInternalError "referenced group member not found") $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences,
          -- from GroupMember
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        JOIN groups g ON g.group_id = m.group_id
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members mu ON g.group_id = mu.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.group_member_id = m.group_member_id
        )
        WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (groupMemberId, userId, userContactId)
  where
    toGroupAndMember :: (GroupInfoRow :. GroupMemberRow :. MaybeConnectionRow) -> (GroupInfo, GroupMember)
    toGroupAndMember (groupInfoRow :. memberRow :. connRow) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})

updateConnectionStatus :: DB.Connection -> Connection -> ConnStatus -> IO ()
updateConnectionStatus db Connection {connId} connStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ? WHERE connection_id = ?" (connStatus, currentTs, connId)

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
        "INSERT INTO groups (local_display_name, user_id, group_profile_id, enable_ntfs, created_at, updated_at) VALUES (?,?,?,?,?,?)"
        (ldn, userId, profileId, True, currentTs, currentTs)
      insertedRowId db
    memberId <- liftIO $ encodedRandomBytes gVar 12
    membership <- createContactMemberInv_ db user groupId user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser Nothing currentTs
    let chatSettings = ChatSettings {enableNtfs = True}
    pure GroupInfo {groupId, localDisplayName = ldn, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = Nothing, chatSettings, createdAt = currentTs, updatedAt = currentTs}

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
              "INSERT INTO groups (group_profile_id, local_display_name, inv_queue_info, host_conn_custom_user_profile_id, user_id, enable_ntfs, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
              (profileId, localDisplayName, connRequest, customUserProfileId, userId, True, currentTs, currentTs)
            insertedRowId db
          GroupMember {groupMemberId} <- createContactMemberInv_ db user groupId contact fromMember GCHostMember GSMemInvited IBUnknown Nothing currentTs
          membership <- createContactMemberInv_ db user groupId user invitedMember GCUserMember GSMemInvited (IBContact contactId) incognitoProfileId currentTs
          let chatSettings = ChatSettings {enableNtfs = True}
          pure (GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId = customUserProfileId, chatSettings, createdAt = currentTs, updatedAt = currentTs}, groupMemberId)

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

getUserGroupDetails :: DB.Connection -> User -> IO [GroupInfo]
getUserGroupDetails db User {userId, userContactId} =
  map (toGroupInfo userContactId)
    <$> DB.query
      db
      [sql|
        SELECT g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
          mu.group_member_id, g.group_id, mu.member_id, mu.member_role, mu.member_category, mu.member_status,
          mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id, pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences
        FROM groups g
        JOIN group_profiles gp USING (group_profile_id)
        JOIN group_members mu USING (group_id)
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        WHERE g.user_id = ? AND mu.contact_id = ?
      |]
      (userId, userContactId)

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

type GroupInfoRow = (Int64, GroupName, GroupName, Text, Maybe Text, Maybe ImageData, Maybe ProfileId, Maybe Bool, Maybe GroupPreferences, UTCTime, UTCTime) :. GroupMemberRow

toGroupInfo :: Int64 -> GroupInfoRow -> GroupInfo
toGroupInfo userContactId ((groupId, localDisplayName, displayName, fullName, description, image, hostConnCustomUserProfileId, enableNtfs_, groupPreferences, createdAt, updatedAt) :. userMemberRow) =
  let membership = toGroupMember userContactId userMemberRow
      chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_}
      fullGroupPreferences = mergeGroupPreferences groupPreferences
      groupProfile = GroupProfile {displayName, fullName, description, image, groupPreferences}
   in GroupInfo {groupId, localDisplayName, groupProfile, fullGroupPreferences, membership, hostConnCustomUserProfileId, chatSettings, createdAt, updatedAt}

getGroupMember :: DB.Connection -> User -> GroupId -> GroupMemberId -> ExceptT StoreError IO GroupMember
getGroupMember db user@User {userId} groupId groupMemberId =
  ExceptT . firstRow (toContactMember user) (SEGroupMemberNotFound {groupId, groupMemberId}) $
    DB.query
      db
      [sql|
        SELECT
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.group_member_id = m.group_member_id
        )
        WHERE m.group_id = ? AND m.group_member_id = ? AND m.user_id = ?
      |]
      (groupId, groupMemberId, userId)

getGroupMembers :: DB.Connection -> User -> GroupInfo -> IO [GroupMember]
getGroupMembers db user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember user)
    <$> DB.query
      db
      [sql|
        SELECT
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.group_member_id = m.group_member_id
        )
        WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)
      |]
      (groupId, userId, userContactId)

getGroupMembersForExpiration :: DB.Connection -> User -> GroupInfo -> IO [GroupMember]
getGroupMembersForExpiration db user@User {userId, userContactId} GroupInfo {groupId} = do
  map (toContactMember user)
    <$> DB.query
      db
      [sql|
        SELECT
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.group_member_id = m.group_member_id
        )
        WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)
          AND m.member_status IN (?, ?, ?)
          AND m.group_member_id NOT IN (
            SELECT DISTINCT group_member_id FROM chat_items
          )
      |]
      (groupId, userId, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted)

toContactMember :: User -> (GroupMemberRow :. MaybeConnectionRow) -> GroupMember
toContactMember User {userContactId} (memberRow :. connRow) =
  (toGroupMember userContactId memberRow) {activeConn = toMaybeConnection connRow}

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

type GroupMemberRow = ((Int64, Int64, MemberId, GroupMemberRole, GroupMemberCategory, GroupMemberStatus) :. (Maybe Int64, ContactName, Maybe ContactId, ProfileId, ProfileId, ContactName, Text, Maybe ImageData, LocalAlias, Maybe Preferences))

type MaybeGroupMemberRow = ((Maybe Int64, Maybe Int64, Maybe MemberId, Maybe GroupMemberRole, Maybe GroupMemberCategory, Maybe GroupMemberStatus) :. (Maybe Int64, Maybe ContactName, Maybe ContactId, Maybe ProfileId, Maybe ProfileId, Maybe ContactName, Maybe Text, Maybe ImageData, Maybe LocalAlias, Maybe Preferences))

toGroupMember :: Int64 -> GroupMemberRow -> GroupMember
toGroupMember userContactId ((groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus) :. (invitedById, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, localAlias, preferences)) =
  let memberProfile = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}
      invitedBy = toInvitedBy userContactId invitedById
      activeConn = Nothing
   in GroupMember {..}

toMaybeGroupMember :: Int64 -> MaybeGroupMemberRow -> Maybe GroupMember
toMaybeGroupMember userContactId ((Just groupMemberId, Just groupId, Just memberId, Just memberRole, Just memberCategory, Just memberStatus) :. (invitedById, Just localDisplayName, memberContactId, Just memberContactProfileId, Just profileId, Just displayName, Just fullName, image, Just localAlias, contactPreferences)) =
  Just $ toGroupMember userContactId ((groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus) :. (invitedById, localDisplayName, memberContactId, memberContactProfileId, profileId, displayName, fullName, image, localAlias, contactPreferences))
toMaybeGroupMember _ _ = Nothing

createNewContactMember :: DB.Connection -> TVar ChaChaDRG -> User -> GroupId -> Contact -> GroupMemberRole -> ConnId -> ConnReqInvitation -> ExceptT StoreError IO GroupMember
createNewContactMember db gVar User {userId, userContactId} groupId Contact {contactId, localDisplayName, profile} memberRole agentConnId connRequest =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    member@GroupMember {groupMemberId} <- createMember_ (MemberId memId) createdAt
    void $ createMemberConnection_ db userId groupMemberId agentConnId Nothing 0 createdAt
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

createNewContactMemberAsync :: DB.Connection -> TVar ChaChaDRG -> User -> GroupId -> Contact -> GroupMemberRole -> (CommandId, ConnId) -> ExceptT StoreError IO ()
createNewContactMemberAsync db gVar user@User {userId, userContactId} groupId Contact {contactId, localDisplayName, profile} memberRole (cmdId, agentConnId) =
  createWithRandomId gVar $ \memId -> do
    createdAt <- liftIO getCurrentTime
    insertMember_ (MemberId memId) createdAt
    groupMemberId <- liftIO $ insertedRowId db
    Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId Nothing 0 createdAt
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

getContactViaMember :: DB.Connection -> User -> GroupMember -> IO (Maybe Contact)
getContactViaMember db user@User {userId} GroupMember {groupMemberId} =
  maybeFirstRow (toContact user) $
    DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.local_alias, ct.contact_used, ct.enable_ntfs, cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM contacts ct
        JOIN contact_profiles cp ON cp.contact_profile_id = ct.contact_profile_id
        JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.contact_id = ct.contact_id
        )
        JOIN group_members m ON m.contact_id = ct.contact_id
        WHERE ct.user_id = ? AND m.group_member_id = ?
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

createMemberConnection :: DB.Connection -> UserId -> GroupMember -> ConnId -> IO ()
createMemberConnection db userId GroupMember {groupMemberId} agentConnId = do
  currentTs <- getCurrentTime
  void $ createMemberConnection_ db userId groupMemberId agentConnId Nothing 0 currentTs

createMemberConnectionAsync :: DB.Connection -> User -> GroupMemberId -> (CommandId, ConnId) -> IO ()
createMemberConnectionAsync db user@User {userId} groupMemberId (cmdId, agentConnId) = do
  currentTs <- getCurrentTime
  Connection {connId} <- createMemberConnection_ db userId groupMemberId agentConnId Nothing 0 currentTs
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
createNewGroupMember db user@User {userId} gInfo memInfo@(MemberInfo _ _ Profile {displayName, fullName, image, preferences}) memCategory memStatus =
  ExceptT . withLocalDisplayName db userId displayName $ \localDisplayName -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, image, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
      (displayName, fullName, image, userId, preferences, currentTs, currentTs)
    memProfileId <- insertedRowId db
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
    Right <$> createNewMember_ db user gInfo newMember currentTs

createNewMember_ :: DB.Connection -> User -> GroupInfo -> NewGroupMember -> UTCTime -> IO GroupMember
createNewMember_
  db
  User {userId, userContactId}
  GroupInfo {groupId}
  NewGroupMember
    { memInfo = MemberInfo memberId memberRole memberProfile,
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
      let introInvitation = IntroInvitation <$> groupConnReq <*> directConnReq
       in Right GroupMemberIntro {introId, reMember, toMember, introStatus, introInvitation}
    toIntro _ = Left SEIntroNotFound

createIntroReMember :: DB.Connection -> User -> GroupInfo -> GroupMember -> MemberInfo -> (CommandId, ConnId) -> (CommandId, ConnId) -> Maybe ProfileId -> ExceptT StoreError IO GroupMember
createIntroReMember db user@User {userId} gInfo@GroupInfo {groupId} _host@GroupMember {memberContactId, activeConn} memInfo@(MemberInfo _ _ memberProfile) (groupCmdId, groupAgentConnId) (directCmdId, directAgentConnId) customUserProfileId = do
  let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
  currentTs <- liftIO getCurrentTime
  Connection {connId = directConnId} <- liftIO $ createConnection_ db userId ConnContact Nothing directAgentConnId memberContactId Nothing customUserProfileId cLevel currentTs
  liftIO $ setCommandConnId db user directCmdId directConnId
  (localDisplayName, contactId, memProfileId) <- createContact_ db userId directConnId memberProfile "" (Just groupId) currentTs
  liftIO $ do
    let newMember =
          NewGroupMember
            { memInfo,
              memCategory = GCPreMember,
              memStatus = GSMemIntroduced,
              memInvitedBy = IBUnknown,
              localDisplayName,
              memContactId = Just contactId,
              memProfileId
            }
    member <- createNewMember_ db user gInfo newMember currentTs
    conn@Connection {connId = groupConnId} <- createMemberConnection_ db userId (groupMemberId' member) groupAgentConnId memberContactId cLevel currentTs
    liftIO $ setCommandConnId db user groupCmdId groupConnId
    pure (member :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: DB.Connection -> User -> GroupMember -> GroupMember -> (CommandId, ConnId) -> (CommandId, ConnId) -> Maybe ProfileId -> IO ()
createIntroToMemberContact db user@User {userId} GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} (groupCmdId, groupAgentConnId) (directCmdId, directAgentConnId) customUserProfileId = do
  let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
  currentTs <- getCurrentTime
  Connection {connId = groupConnId} <- createMemberConnection_ db userId groupMemberId groupAgentConnId viaContactId cLevel currentTs
  setCommandConnId db user groupCmdId groupConnId
  Connection {connId = directConnId} <- createConnection_ db userId ConnContact Nothing directAgentConnId viaContactId Nothing customUserProfileId cLevel currentTs
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

createMemberConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> Maybe Int64 -> Int -> UTCTime -> IO Connection
createMemberConnection_ db userId groupMemberId agentConnId viaContact = createConnection_ db userId ConnMember (Just groupMemberId) agentConnId viaContact Nothing Nothing

getViaGroupMember :: DB.Connection -> User -> Contact -> IO (Maybe (GroupInfo, GroupMember))
getViaGroupMember db User {userId, userContactId} Contact {contactId} =
  maybeFirstRow toGroupAndMember $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
          -- GroupInfo {membership}
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          -- GroupInfo {membership = GroupMember {memberProfile}}
          pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences,
          -- via GroupMember
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
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
          where cc.group_member_id = m.group_member_id
        )
        WHERE ct.user_id = ? AND ct.contact_id = ? AND mu.contact_id = ?
      |]
      (userId, contactId, userContactId)
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
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, p.display_name, p.full_name, p.image, p.local_alias, ct.via_group, ct.contact_used, ct.enable_ntfs, p.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
          c.conn_status, c.conn_type, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM contacts ct
        JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
        JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.contact_id = ct.contact_id
        )
        JOIN groups g ON g.group_id = ct.via_group
        JOIN group_members m ON m.group_id = g.group_id AND m.contact_id = ct.contact_id
        WHERE ct.user_id = ? AND m.group_member_id = ?
      |]
      (userId, groupMemberId)
  where
    toContact' :: ((ContactId, ProfileId, ContactName, Text, Text, Maybe ImageData, LocalAlias, Maybe Int64, Bool, Maybe Bool) :. (Maybe Preferences, Preferences, UTCTime, UTCTime)) :. ConnectionRow -> Contact
    toContact' (((contactId, profileId, localDisplayName, displayName, fullName, image, localAlias, viaGroup, contactUsed, enableNtfs_) :. (preferences, userPreferences, createdAt, updatedAt)) :. connRow) =
      let profile = LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}
          chatSettings = ChatSettings {enableNtfs = fromMaybe True enableNtfs_}
          activeConn = toConnection connRow
          mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito activeConn
       in Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt}

createSndDirectFileTransfer :: DB.Connection -> UserId -> Contact -> FilePath -> FileInvitation -> Maybe ConnId -> Integer -> IO FileTransferMeta
createSndDirectFileTransfer db userId Contact {contactId} filePath FileInvitation {fileName, fileSize, fileInline} acId_ chunkSize = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, contact_id, file_name, file_path, file_size, chunk_size, file_inline, ci_file_status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
    (userId, contactId, fileName, filePath, fileSize, chunkSize, fileInline, CIFSSndStored, currentTs, currentTs)
  fileId <- insertedRowId db
  forM_ acId_ $ \acId -> do
    Connection {connId} <- createSndFileConnection_ db userId fileId acId
    let fileStatus = FSNew
    DB.execute
      db
      "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (fileId, fileStatus, fileInline, connId, currentTs, currentTs)
  pure FileTransferMeta {fileId, fileName, filePath, fileSize, fileInline, chunkSize, cancelled = False}

createSndDirectFTConnection :: DB.Connection -> User -> Int64 -> (CommandId, ConnId) -> IO ()
createSndDirectFTConnection db user@User {userId} fileId (cmdId, acId) = do
  currentTs <- getCurrentTime
  Connection {connId} <- createSndFileConnection_ db userId fileId acId
  setCommandConnId db user cmdId connId
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, connection_id, created_at, updated_at) VALUES (?,?,?,?,?)"
    (fileId, FSAccepted, connId, currentTs, currentTs)

createSndGroupFileTransfer :: DB.Connection -> UserId -> GroupInfo -> FilePath -> FileInvitation -> Integer -> IO FileTransferMeta
createSndGroupFileTransfer db userId GroupInfo {groupId} filePath FileInvitation {fileName, fileSize, fileInline} chunkSize = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, group_id, file_name, file_path, file_size, chunk_size, file_inline, ci_file_status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
    (userId, groupId, fileName, filePath, fileSize, chunkSize, fileInline, CIFSSndStored, currentTs, currentTs)
  fileId <- insertedRowId db
  pure FileTransferMeta {fileId, fileName, filePath, fileSize, fileInline, chunkSize, cancelled = False}

createSndGroupFileTransferConnection :: DB.Connection -> User -> Int64 -> (CommandId, ConnId) -> GroupMember -> IO ()
createSndGroupFileTransferConnection db user@User {userId} fileId (cmdId, acId) GroupMember {groupMemberId} = do
  currentTs <- getCurrentTime
  Connection {connId} <- createSndFileConnection_ db userId fileId acId
  setCommandConnId db user cmdId connId
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, connection_id, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (fileId, FSAccepted, connId, groupMemberId, currentTs, currentTs)

createSndDirectInlineFT :: DB.Connection -> Contact -> FileTransferMeta -> IO SndFileTransfer
createSndDirectInlineFT db Contact {localDisplayName = n, activeConn = Connection {connId, agentConnId}} FileTransferMeta {fileId, fileName, filePath, fileSize, chunkSize, fileInline} = do
  currentTs <- getCurrentTime
  let fileStatus = FSConnected
      fileInline' = Just $ fromMaybe IFMOffer fileInline
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (fileId, fileStatus, fileInline', connId, currentTs, currentTs)
  pure SndFileTransfer {fileId, fileName, filePath, fileSize, chunkSize, recipientDisplayName = n, connId, agentConnId, fileStatus, fileInline = fileInline'}

createSndGroupInlineFT :: DB.Connection -> GroupMember -> Connection -> FileTransferMeta -> IO SndFileTransfer
createSndGroupInlineFT db GroupMember {groupMemberId, localDisplayName = n} Connection {connId, agentConnId} FileTransferMeta {fileId, fileName, filePath, fileSize, chunkSize, fileInline} = do
  currentTs <- getCurrentTime
  let fileStatus = FSConnected
      fileInline' = Just $ fromMaybe IFMOffer fileInline
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (fileId, fileStatus, fileInline', connId, groupMemberId, currentTs, currentTs)
  pure SndFileTransfer {fileId, fileName, filePath, fileSize, chunkSize, recipientDisplayName = n, connId, agentConnId, fileStatus, fileInline = fileInline'}

updateSndDirectFTDelivery :: DB.Connection -> Contact -> FileTransferMeta -> Int64 -> IO ()
updateSndDirectFTDelivery db Contact {activeConn = Connection {connId}} FileTransferMeta {fileId} msgDeliveryId =
  DB.execute
    db
    "UPDATE snd_files SET last_inline_msg_delivery_id = ? WHERE connection_id = ? AND file_id = ? AND file_inline IS NOT NULL"
    (msgDeliveryId, connId, fileId)

updateSndGroupFTDelivery :: DB.Connection -> GroupMember -> Connection -> FileTransferMeta -> Int64 -> IO ()
updateSndGroupFTDelivery db GroupMember {groupMemberId} Connection {connId} FileTransferMeta {fileId} msgDeliveryId =
  DB.execute
    db
    "UPDATE snd_files SET last_inline_msg_delivery_id = ? WHERE group_member_id = ? AND connection_id = ? AND file_id = ? AND file_inline IS NOT NULL"
    (msgDeliveryId, groupMemberId, connId, fileId)

getSndInlineFTViaMsgDelivery :: DB.Connection -> User -> Connection -> AgentMsgId -> IO (Maybe SndFileTransfer)
getSndInlineFTViaMsgDelivery db User {userId} Connection {connId, agentConnId} agentMsgId = do
  (sndFileTransfer_ <=< listToMaybe)
    <$> DB.query
      db
      [sql|
        SELECT s.file_id, s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_inline, c.local_display_name, m.local_display_name
        FROM msg_deliveries d
        JOIN snd_files s ON s.connection_id = d.connection_id AND s.last_inline_msg_delivery_id = d.msg_delivery_id
        JOIN files f ON f.file_id = s.file_id
        LEFT JOIN contacts c USING (contact_id)
        LEFT JOIN group_members m USING (group_member_id)
        WHERE d.connection_id = ? AND d.agent_msg_id = ? AND f.user_id = ? AND s.file_inline IS NOT NULL
      |]
      (connId, agentMsgId, userId)
  where
    sndFileTransfer_ :: (Int64, FileStatus, String, Integer, Integer, FilePath, Maybe InlineFileMode, Maybe ContactName, Maybe ContactName) -> Maybe SndFileTransfer
    sndFileTransfer_ (fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, contactName_, memberName_) =
      (\n -> SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, recipientDisplayName = n, connId, agentConnId})
        <$> (contactName_ <|> memberName_)

updateFileCancelled :: MsgDirectionI d => DB.Connection -> User -> Int64 -> CIFileStatus d -> IO ()
updateFileCancelled db User {userId} fileId ciFileStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET cancelled = 1, ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?" (ciFileStatus, currentTs, userId, fileId)

updateCIFileStatus :: MsgDirectionI d => DB.Connection -> User -> Int64 -> CIFileStatus d -> IO ()
updateCIFileStatus db User {userId} fileId ciFileStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?" (ciFileStatus, currentTs, userId, fileId)

getSharedMsgIdByFileId :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO SharedMsgId
getSharedMsgIdByFileId db userId fileId =
  ExceptT . firstRow fromOnly (SESharedMsgIdNotFoundByFileId fileId) $
    DB.query
      db
      [sql|
        SELECT i.shared_msg_id
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE f.user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)

getFileIdBySharedMsgId :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getFileIdBySharedMsgId db userId contactId sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.shared_msg_id = ?
      |]
      (userId, contactId, sharedMsgId)

getGroupFileIdBySharedMsgId :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getGroupFileIdBySharedMsgId db userId groupId sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.group_id = ? AND i.shared_msg_id = ?
      |]
      (userId, groupId, sharedMsgId)

getDirectFileIdBySharedMsgId :: DB.Connection -> User -> Contact -> SharedMsgId -> ExceptT StoreError IO Int64
getDirectFileIdBySharedMsgId db User {userId} Contact {contactId} sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.shared_msg_id = ?
      |]
      (userId, contactId, sharedMsgId)

getChatRefByFileId :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO ChatRef
getChatRefByFileId db User {userId} fileId =
  liftIO getChatRef >>= \case
    [(Just contactId, Nothing)] -> pure $ ChatRef CTDirect contactId
    [(Nothing, Just groupId)] -> pure $ ChatRef CTGroup groupId
    _ -> throwError $ SEInternalError "could not retrieve chat ref by file id"
  where
    getChatRef =
      DB.query
        db
        [sql|
          SELECT contact_id, group_id
          FROM files
          WHERE user_id = ? AND file_id = ?
          LIMIT 1
        |]
        (userId, fileId)

createSndFileConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> IO Connection
createSndFileConnection_ db userId fileId agentConnId = do
  currentTs <- getCurrentTime
  createConnection_ db userId ConnSndFile (Just fileId) agentConnId Nothing Nothing Nothing 0 currentTs

updateSndFileStatus :: DB.Connection -> SndFileTransfer -> FileStatus -> IO ()
updateSndFileStatus db SndFileTransfer {fileId, connId} status = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE snd_files SET file_status = ?, updated_at = ? WHERE file_id = ? AND connection_id = ?" (status, currentTs, fileId, connId)

createSndFileChunk :: DB.Connection -> SndFileTransfer -> IO (Maybe Integer)
createSndFileChunk db SndFileTransfer {fileId, connId, fileSize, chunkSize} = do
  chunkNo <- getLastChunkNo
  insertChunk chunkNo
  pure chunkNo
  where
    getLastChunkNo = do
      ns <- DB.query db "SELECT chunk_number FROM snd_file_chunks WHERE file_id = ? AND connection_id = ? AND chunk_sent = 1 ORDER BY chunk_number DESC LIMIT 1" (fileId, connId)
      pure $ case map fromOnly ns of
        [] -> Just 1
        n : _ -> if n * chunkSize >= fileSize then Nothing else Just (n + 1)
    insertChunk chunkNo_ = forM_ chunkNo_ $ \chunkNo -> do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT OR REPLACE INTO snd_file_chunks (file_id, connection_id, chunk_number, created_at, updated_at) VALUES (?,?,?,?,?)"
        (fileId, connId, chunkNo, currentTs, currentTs)

updateSndFileChunkMsg :: DB.Connection -> SndFileTransfer -> Integer -> AgentMsgId -> IO ()
updateSndFileChunkMsg db SndFileTransfer {fileId, connId} chunkNo msgId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE snd_file_chunks
      SET chunk_agent_msg_id = ?, updated_at = ?
      WHERE file_id = ? AND connection_id = ? AND chunk_number = ?
    |]
    (msgId, currentTs, fileId, connId, chunkNo)

updateSndFileChunkSent :: DB.Connection -> SndFileTransfer -> AgentMsgId -> IO ()
updateSndFileChunkSent db SndFileTransfer {fileId, connId} msgId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE snd_file_chunks
      SET chunk_sent = 1, updated_at = ?
      WHERE file_id = ? AND connection_id = ? AND chunk_agent_msg_id = ?
    |]
    (currentTs, fileId, connId, msgId)

deleteSndFileChunks :: DB.Connection -> SndFileTransfer -> IO ()
deleteSndFileChunks db SndFileTransfer {fileId, connId} =
  DB.execute db "DELETE FROM snd_file_chunks WHERE file_id = ? AND connection_id = ?" (fileId, connId)

createRcvFileTransfer :: DB.Connection -> UserId -> Contact -> FileInvitation -> Maybe InlineFileMode -> Integer -> IO RcvFileTransfer
createRcvFileTransfer db userId Contact {contactId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq, fileInline} rcvFileInline chunkSize = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, contact_id, file_name, file_size, chunk_size, file_inline, ci_file_status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
    (userId, contactId, fileName, fileSize, chunkSize, fileInline, CIFSRcvInvitation, currentTs, currentTs)
  fileId <- insertedRowId db
  DB.execute
    db
    "INSERT INTO rcv_files (file_id, file_status, file_queue_info, file_inline, rcv_file_inline, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (fileId, FSNew, fileConnReq, fileInline, rcvFileInline, currentTs, currentTs)
  pure RcvFileTransfer {fileId, fileInvitation = f, fileStatus = RFSNew, rcvFileInline, senderDisplayName = c, chunkSize, cancelled = False, grpMemberId = Nothing}

createRcvGroupFileTransfer :: DB.Connection -> UserId -> GroupMember -> FileInvitation -> Maybe InlineFileMode -> Integer -> IO RcvFileTransfer
createRcvGroupFileTransfer db userId GroupMember {groupId, groupMemberId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq, fileInline} rcvFileInline chunkSize = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, group_id, file_name, file_size, chunk_size, file_inline, ci_file_status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
    (userId, groupId, fileName, fileSize, chunkSize, fileInline, CIFSRcvInvitation, currentTs, currentTs)
  fileId <- insertedRowId db
  DB.execute
    db
    "INSERT INTO rcv_files (file_id, file_status, file_queue_info, file_inline, rcv_file_inline, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
    (fileId, FSNew, fileConnReq, fileInline, rcvFileInline, groupMemberId, currentTs, currentTs)
  pure RcvFileTransfer {fileId, fileInvitation = f, fileStatus = RFSNew, rcvFileInline, senderDisplayName = c, chunkSize, cancelled = False, grpMemberId = Just groupMemberId}

getRcvFileTransfer :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO RcvFileTransfer
getRcvFileTransfer db user@User {userId} fileId = do
  rftRow <-
    ExceptT . firstRow id (SERcvFileNotFound fileId) $
      DB.query
        db
        [sql|
          SELECT r.file_status, r.file_queue_info, r.group_member_id, f.file_name,
            f.file_size, f.chunk_size, f.cancelled, cs.contact_id, cs.local_display_name, m.group_id, m.group_member_id, m.local_display_name,
            f.file_path, r.file_inline, r.rcv_file_inline, c.connection_id, c.agent_conn_id
          FROM rcv_files r
          JOIN files f USING (file_id)
          LEFT JOIN connections c ON r.file_id = c.rcv_file_id
          LEFT JOIN contacts cs USING (contact_id)
          LEFT JOIN group_members m USING (group_member_id)
          WHERE f.user_id = ? AND f.file_id = ?
        |]
        (userId, fileId)
  rcvFileTransfer rftRow
  where
    rcvFileTransfer ::
      (FileStatus, Maybe ConnReqInvitation, Maybe Int64, String, Integer, Integer, Maybe Bool) :. (Maybe Int64, Maybe ContactName, Maybe Int64, Maybe Int64, Maybe ContactName, Maybe FilePath, Maybe InlineFileMode, Maybe InlineFileMode) :. (Maybe Int64, Maybe AgentConnId) ->
      ExceptT StoreError IO RcvFileTransfer
    rcvFileTransfer ((fileStatus', fileConnReq, grpMemberId, fileName, fileSize, chunkSize, cancelled_) :. (contactId_, contactName_, groupId_, groupMemberId_, memberName_, filePath_, fileInline, rcvFileInline) :. (connId_, agentConnId_)) = do
      let fileInv = FileInvitation {fileName, fileSize, fileConnReq, fileInline}
          fileInfo = (filePath_, connId_, agentConnId_, contactId_, groupId_, groupMemberId_, isJust fileInline)
      case contactName_ <|> memberName_ of
        Nothing -> throwError $ SERcvFileInvalid fileId
        Just name -> do
          case fileStatus' of
            FSNew -> pure $ ft name fileInv RFSNew
            FSAccepted -> ft name fileInv . RFSAccepted <$> rfi fileInfo
            FSConnected -> ft name fileInv . RFSConnected <$> rfi fileInfo
            FSComplete -> ft name fileInv . RFSComplete <$> rfi fileInfo
            FSCancelled -> ft name fileInv . RFSCancelled <$> rfi_ fileInfo
      where
        ft senderDisplayName fileInvitation fileStatus =
          RcvFileTransfer {fileId, fileInvitation, fileStatus, rcvFileInline, senderDisplayName, chunkSize, cancelled, grpMemberId}
        rfi fileInfo = maybe (throwError $ SERcvFileInvalid fileId) pure =<< rfi_ fileInfo
        rfi_ = \case
          (Just filePath, Just connId, Just agentConnId, _, _, _, _) -> pure $ Just RcvFileInfo {filePath, connId, agentConnId}
          (Just filePath, Nothing, Nothing, Just contactId, _, _, True) -> do
            Contact {activeConn = Connection {connId, agentConnId}} <- getContact db user contactId
            pure $ Just RcvFileInfo {filePath, connId, agentConnId}
          (Just filePath, Nothing, Nothing, _, Just groupId, Just groupMemberId, True) -> do
            getGroupMember db user groupId groupMemberId >>= \case
              GroupMember {activeConn = Just Connection {connId, agentConnId}} ->
                pure $ Just RcvFileInfo {filePath, connId, agentConnId}
              _ -> pure Nothing
          _ -> pure Nothing
        cancelled = fromMaybe False cancelled_

acceptRcvFileTransfer :: DB.Connection -> User -> Int64 -> ConnId -> ConnStatus -> FilePath -> ExceptT StoreError IO AChatItem
acceptRcvFileTransfer db user@User {userId} fileId agentConnId connStatus filePath = ExceptT $ do
  currentTs <- getCurrentTime
  acceptRcvFT_ db user fileId filePath currentTs
  DB.execute
    db
    "INSERT INTO connections (agent_conn_id, conn_status, conn_type, rcv_file_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (agentConnId, connStatus, ConnRcvFile, fileId, userId, currentTs, currentTs)
  runExceptT $ getChatItemByFileId db user fileId

acceptRcvInlineFT :: DB.Connection -> User -> Int64 -> FilePath -> ExceptT StoreError IO AChatItem
acceptRcvInlineFT db user fileId filePath = do
  liftIO $ acceptRcvFT_ db user fileId filePath =<< getCurrentTime
  getChatItemByFileId db user fileId

startRcvInlineFT :: DB.Connection -> User -> RcvFileTransfer -> FilePath -> IO ()
startRcvInlineFT db user RcvFileTransfer {fileId} filePath =
  acceptRcvFT_ db user fileId filePath =<< getCurrentTime

acceptRcvFT_ :: DB.Connection -> User -> Int64 -> FilePath -> UTCTime -> IO ()
acceptRcvFT_ db User {userId} fileId filePath currentTs = do
  DB.execute
    db
    "UPDATE files SET file_path = ?, ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?"
    (filePath, CIFSRcvAccepted, currentTs, userId, fileId)
  DB.execute
    db
    "UPDATE rcv_files SET file_status = ?, updated_at = ? WHERE file_id = ?"
    (FSAccepted, currentTs, fileId)

updateRcvFileStatus :: DB.Connection -> RcvFileTransfer -> FileStatus -> IO ()
updateRcvFileStatus db RcvFileTransfer {fileId} status = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE rcv_files SET file_status = ?, updated_at = ? WHERE file_id = ?" (status, currentTs, fileId)

createRcvFileChunk :: DB.Connection -> RcvFileTransfer -> Integer -> AgentMsgId -> IO RcvChunkStatus
createRcvFileChunk db RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileSize}, chunkSize} chunkNo msgId = do
  status <- getLastChunkNo
  unless (status == RcvChunkError) $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT OR REPLACE INTO rcv_file_chunks (file_id, chunk_number, chunk_agent_msg_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (fileId, chunkNo, msgId, currentTs, currentTs)
  pure status
  where
    getLastChunkNo = do
      ns <- DB.query db "SELECT chunk_number FROM rcv_file_chunks WHERE file_id = ? ORDER BY chunk_number DESC LIMIT 1" (Only fileId)
      pure $ case map fromOnly ns of
        []
          | chunkNo == 1 ->
            if chunkSize >= fileSize
              then RcvChunkFinal
              else RcvChunkOk
          | otherwise -> RcvChunkError
        n : _
          | chunkNo == n -> RcvChunkDuplicate
          | chunkNo == n + 1 ->
            let prevSize = n * chunkSize
             in if prevSize >= fileSize
                  then RcvChunkError
                  else
                    if prevSize + chunkSize >= fileSize
                      then RcvChunkFinal
                      else RcvChunkOk
          | otherwise -> RcvChunkError

updatedRcvFileChunkStored :: DB.Connection -> RcvFileTransfer -> Integer -> IO ()
updatedRcvFileChunkStored db RcvFileTransfer {fileId} chunkNo = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE rcv_file_chunks
      SET chunk_stored = 1, updated_at = ?
      WHERE file_id = ? AND chunk_number = ?
    |]
    (currentTs, fileId, chunkNo)

deleteRcvFileChunks :: DB.Connection -> RcvFileTransfer -> IO ()
deleteRcvFileChunks db RcvFileTransfer {fileId} =
  DB.execute db "DELETE FROM rcv_file_chunks WHERE file_id = ?" (Only fileId)

updateFileTransferChatItemId :: DB.Connection -> FileTransferId -> ChatItemId -> IO ()
updateFileTransferChatItemId db fileId ciId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET chat_item_id = ?, updated_at = ? WHERE file_id = ?" (ciId, currentTs, fileId)

getFileTransferProgress :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (FileTransfer, [Integer])
getFileTransferProgress db user fileId = do
  ft <- getFileTransfer db user fileId
  liftIO $
    (ft,) . map fromOnly <$> case ft of
      FTSnd _ [] -> pure [Only 0]
      FTSnd _ _ -> DB.query db "SELECT COUNT(*) FROM snd_file_chunks WHERE file_id = ? and chunk_sent = 1 GROUP BY connection_id" (Only fileId)
      FTRcv _ -> DB.query db "SELECT COUNT(*) FROM rcv_file_chunks WHERE file_id = ? AND chunk_stored = 1" (Only fileId)

getFileTransfer :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO FileTransfer
getFileTransfer db user@User {userId} fileId =
  fileTransfer =<< liftIO getFileTransferRow
  where
    fileTransfer :: [(Maybe Int64, Maybe Int64)] -> ExceptT StoreError IO FileTransfer
    fileTransfer [(Nothing, Just _)] = FTRcv <$> getRcvFileTransfer db user fileId
    fileTransfer _ = do
      (ftm, fts) <- getSndFileTransfer db user fileId
      pure $ FTSnd {fileTransferMeta = ftm, sndFileTransfers = fts}
    getFileTransferRow :: IO [(Maybe Int64, Maybe Int64)]
    getFileTransferRow =
      DB.query
        db
        [sql|
          SELECT s.file_id, r.file_id
          FROM files f
          LEFT JOIN snd_files s ON s.file_id = f.file_id
          LEFT JOIN rcv_files r ON r.file_id = f.file_id
          WHERE user_id = ? AND f.file_id = ?
        |]
        (userId, fileId)

getSndFileTransfer :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (FileTransferMeta, [SndFileTransfer])
getSndFileTransfer db user@User {userId} fileId = do
  fileTransferMeta <- getFileTransferMeta db user fileId
  sndFileTransfers <- ExceptT $ getSndFileTransfers_ db userId fileId
  pure (fileTransferMeta, sndFileTransfers)

getSndFileTransfers_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError [SndFileTransfer])
getSndFileTransfers_ db userId fileId =
  mapM sndFileTransfer
    <$> DB.query
      db
      [sql|
        SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_inline, s.connection_id, c.agent_conn_id,
          cs.local_display_name, m.local_display_name
        FROM snd_files s
        JOIN files f USING (file_id)
        JOIN connections c USING (connection_id)
        LEFT JOIN contacts cs USING (contact_id)
        LEFT JOIN group_members m USING (group_member_id)
        WHERE f.user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)
  where
    sndFileTransfer :: (FileStatus, String, Integer, Integer, FilePath, Maybe InlineFileMode, Int64, AgentConnId, Maybe ContactName, Maybe ContactName) -> Either StoreError SndFileTransfer
    sndFileTransfer (fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, connId, agentConnId, contactName_, memberName_) =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileInline, recipientDisplayName, connId, agentConnId}
        Nothing -> Left $ SESndFileInvalid fileId

getFileTransferMeta :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO FileTransferMeta
getFileTransferMeta db User {userId} fileId =
  ExceptT . firstRow fileTransferMeta (SEFileNotFound fileId) $
    DB.query
      db
      [sql|
        SELECT f.file_name, f.file_size, f.chunk_size, f.file_path, f.file_inline, f.cancelled
        FROM files f
        WHERE f.user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)
  where
    fileTransferMeta :: (String, Integer, Integer, FilePath, Maybe InlineFileMode, Maybe Bool) -> FileTransferMeta
    fileTransferMeta (fileName, fileSize, chunkSize, filePath, fileInline, cancelled_) =
      FileTransferMeta {fileId, fileName, fileSize, chunkSize, filePath, fileInline, cancelled = fromMaybe False cancelled_}

getContactFileInfo :: DB.Connection -> User -> Contact -> IO [CIFileInfo]
getContactFileInfo db User {userId} Contact {contactId} =
  map toFileInfo
    <$> DB.query
      db
      [sql|
        SELECT f.file_id, f.ci_file_status, f.file_path
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ?
      |]
      (userId, contactId)

toFileInfo :: (Int64, Maybe ACIFileStatus, Maybe FilePath) -> CIFileInfo
toFileInfo (fileId, fileStatus, filePath) = CIFileInfo {fileId, fileStatus, filePath}

getContactMaxItemTs :: DB.Connection -> User -> Contact -> IO (Maybe UTCTime)
getContactMaxItemTs db User {userId} Contact {contactId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT MAX(item_ts) FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)

deleteContactCIs :: DB.Connection -> User -> Contact -> IO ()
deleteContactCIs db user@User {userId} ct@Contact {contactId} = do
  connIds <- getContactConnIds_ db user ct
  forM_ connIds $ \connId ->
    DB.execute db "DELETE FROM messages WHERE connection_id = ?" (Only connId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)

getContactConnIds_ :: DB.Connection -> User -> Contact -> IO [Int64]
getContactConnIds_ db User {userId} Contact {contactId} =
  map fromOnly
    <$> DB.query db "SELECT connection_id FROM connections WHERE user_id = ? AND contact_id = ?" (userId, contactId)

updateContactTs :: DB.Connection -> User -> Contact -> UTCTime -> IO ()
updateContactTs db User {userId} Contact {contactId} updatedAt =
  DB.execute
    db
    "UPDATE contacts SET updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (updatedAt, userId, contactId)

getGroupFileInfo :: DB.Connection -> User -> GroupInfo -> IO [CIFileInfo]
getGroupFileInfo db User {userId} GroupInfo {groupId} =
  map toFileInfo
    <$> DB.query
      db
      [sql|
        SELECT f.file_id, f.ci_file_status, f.file_path
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE i.user_id = ? AND i.group_id = ?
      |]
      (userId, groupId)

getGroupMaxItemTs :: DB.Connection -> User -> GroupInfo -> IO (Maybe UTCTime)
getGroupMaxItemTs db User {userId} GroupInfo {groupId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT MAX(item_ts) FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

deleteGroupCIs :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupCIs db User {userId} GroupInfo {groupId} = do
  DB.execute db "DELETE FROM messages WHERE group_id = ?" (Only groupId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

updateGroupTs :: DB.Connection -> User -> GroupInfo -> UTCTime -> IO ()
updateGroupTs db User {userId} GroupInfo {groupId} updatedAt =
  DB.execute
    db
    "UPDATE groups SET updated_at = ? WHERE user_id = ? AND group_id = ?"
    (updatedAt, userId, groupId)

createNewSndMessage :: MsgEncodingI e => DB.Connection -> TVar ChaChaDRG -> ConnOrGroupId -> (SharedMsgId -> NewMessage e) -> ExceptT StoreError IO SndMessage
createNewSndMessage db gVar connOrGroupId mkMessage =
  createWithRandomId gVar $ \sharedMsgId -> do
    let NewMessage {chatMsgEvent, msgBody} = mkMessage $ SharedMsgId sharedMsgId
    createdAt <- getCurrentTime
    DB.execute
      db
      [sql|
        INSERT INTO messages (
          msg_sent, chat_msg_event, msg_body, connection_id, group_id,
          shared_msg_id, shared_msg_id_user, created_at, updated_at
        ) VALUES (?,?,?,?,?,?,?,?,?)
      |]
      (MDSnd, toCMEventTag chatMsgEvent, msgBody, connId_, groupId_, sharedMsgId, Just True, createdAt, createdAt)
    msgId <- insertedRowId db
    pure SndMessage {msgId, sharedMsgId = SharedMsgId sharedMsgId, msgBody}
  where
    (connId_, groupId_) = case connOrGroupId of
      ConnectionId connId -> (Just connId, Nothing)
      GroupId groupId -> (Nothing, Just groupId)

createSndMsgDelivery :: DB.Connection -> SndMsgDelivery -> MessageId -> IO Int64
createSndMsgDelivery db sndMsgDelivery messageId = do
  currentTs <- getCurrentTime
  msgDeliveryId <- createSndMsgDelivery_ db sndMsgDelivery messageId currentTs
  createMsgDeliveryEvent_ db msgDeliveryId MDSSndAgent currentTs
  pure msgDeliveryId

createNewMessageAndRcvMsgDelivery :: forall e. MsgEncodingI e => DB.Connection -> ConnOrGroupId -> NewMessage e -> Maybe SharedMsgId -> RcvMsgDelivery -> IO RcvMessage
createNewMessageAndRcvMsgDelivery db connOrGroupId NewMessage {chatMsgEvent, msgBody} sharedMsgId_ RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO messages (msg_sent, chat_msg_event, msg_body, created_at, updated_at, connection_id, group_id, shared_msg_id) VALUES (?,?,?,?,?,?,?,?)"
    (MDRcv, toCMEventTag chatMsgEvent, msgBody, currentTs, currentTs, connId_, groupId_, sharedMsgId_)
  msgId <- insertedRowId db
  DB.execute
    db
    "INSERT INTO msg_deliveries (message_id, connection_id, agent_msg_id, agent_msg_meta, agent_ack_cmd_id, chat_ts, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
    (msgId, connId, agentMsgId, msgMetaJson agentMsgMeta, agentAckCmdId, snd $ broker agentMsgMeta, currentTs, currentTs)
  msgDeliveryId <- insertedRowId db
  createMsgDeliveryEvent_ db msgDeliveryId MDSRcvAgent currentTs
  pure RcvMessage {msgId, chatMsgEvent = ACME (encoding @e) chatMsgEvent, sharedMsgId_, msgBody}
  where
    (connId_, groupId_) = case connOrGroupId of
      ConnectionId connId' -> (Just connId', Nothing)
      GroupId groupId -> (Nothing, Just groupId)

createSndMsgDeliveryEvent :: DB.Connection -> Int64 -> AgentMsgId -> MsgDeliveryStatus 'MDSnd -> ExceptT StoreError IO ()
createSndMsgDeliveryEvent db connId agentMsgId sndMsgDeliveryStatus = do
  msgDeliveryId <- getMsgDeliveryId_ db connId agentMsgId
  liftIO $ do
    currentTs <- getCurrentTime
    createMsgDeliveryEvent_ db msgDeliveryId sndMsgDeliveryStatus currentTs

createRcvMsgDeliveryEvent :: DB.Connection -> Int64 -> CommandId -> MsgDeliveryStatus 'MDRcv -> IO ()
createRcvMsgDeliveryEvent db connId cmdId rcvMsgDeliveryStatus = do
  msgDeliveryId <- getMsgDeliveryIdByCmdId_ db connId cmdId
  forM_ msgDeliveryId $ \mdId -> do
    currentTs <- getCurrentTime
    createMsgDeliveryEvent_ db mdId rcvMsgDeliveryStatus currentTs

createSndMsgDelivery_ :: DB.Connection -> SndMsgDelivery -> MessageId -> UTCTime -> IO Int64
createSndMsgDelivery_ db SndMsgDelivery {connId, agentMsgId} messageId createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO msg_deliveries
        (message_id, connection_id, agent_msg_id, agent_msg_meta, chat_ts, created_at, updated_at)
      VALUES (?,?,?,NULL,?,?,?)
    |]
    (messageId, connId, agentMsgId, createdAt, createdAt, createdAt)
  insertedRowId db

createMsgDeliveryEvent_ :: DB.Connection -> Int64 -> MsgDeliveryStatus d -> UTCTime -> IO ()
createMsgDeliveryEvent_ db msgDeliveryId msgDeliveryStatus createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO msg_delivery_events
        (msg_delivery_id, delivery_status, created_at, updated_at)
      VALUES (?,?,?,?)
    |]
    (msgDeliveryId, msgDeliveryStatus, createdAt, createdAt)

getMsgDeliveryId_ :: DB.Connection -> Int64 -> AgentMsgId -> ExceptT StoreError IO Int64
getMsgDeliveryId_ db connId agentMsgId =
  ExceptT . firstRow fromOnly (SENoMsgDelivery connId agentMsgId) $
    DB.query
      db
      [sql|
        SELECT msg_delivery_id
        FROM msg_deliveries m
        WHERE m.connection_id = ? AND m.agent_msg_id = ?
        LIMIT 1
      |]
      (connId, agentMsgId)

getMsgDeliveryIdByCmdId_ :: DB.Connection -> Int64 -> CommandId -> IO (Maybe AgentMsgId)
getMsgDeliveryIdByCmdId_ db connId cmdId =
  maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT msg_delivery_id
        FROM msg_deliveries
        WHERE connection_id = ? AND agent_ack_cmd_id = ?
        LIMIT 1
      |]
      (connId, cmdId)

createPendingGroupMessage :: DB.Connection -> Int64 -> MessageId -> Maybe Int64 -> IO ()
createPendingGroupMessage db groupMemberId messageId introId_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO pending_group_messages
        (group_member_id, message_id, group_member_intro_id, created_at, updated_at) VALUES (?,?,?,?,?)
    |]
    (groupMemberId, messageId, introId_, currentTs, currentTs)

getPendingGroupMessages :: DB.Connection -> Int64 -> IO [PendingGroupMessage]
getPendingGroupMessages db groupMemberId =
  map pendingGroupMessage
    <$> DB.query
      db
      [sql|
        SELECT pgm.message_id, m.chat_msg_event, m.msg_body, pgm.group_member_intro_id
        FROM pending_group_messages pgm
        JOIN messages m USING (message_id)
        WHERE pgm.group_member_id = ?
        ORDER BY pgm.message_id ASC
      |]
      (Only groupMemberId)
  where
    pendingGroupMessage (msgId, cmEventTag, msgBody, introId_) =
      PendingGroupMessage {msgId, cmEventTag, msgBody, introId_}

deletePendingGroupMessage :: DB.Connection -> Int64 -> MessageId -> IO ()
deletePendingGroupMessage db groupMemberId messageId =
  DB.execute db "DELETE FROM pending_group_messages WHERE group_member_id = ? AND message_id = ?" (groupMemberId, messageId)

type NewQuoteRow = (Maybe SharedMsgId, Maybe UTCTime, Maybe MsgContent, Maybe Bool, Maybe MemberId)

createNewSndChatItem :: DB.Connection -> User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIQuote c) -> UTCTime -> IO ChatItemId
createNewSndChatItem db user chatDirection SndMessage {msgId, sharedMsgId} ciContent quotedItem createdAt =
  createNewChatItem_ db user chatDirection createdByMsgId (Just sharedMsgId) ciContent quoteRow createdAt createdAt
  where
    createdByMsgId = if msgId == 0 then Nothing else Just msgId
    quoteRow :: NewQuoteRow
    quoteRow = case quotedItem of
      Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
      Just CIQuote {chatDir, sharedMsgId = quotedSharedMsgId, sentAt, content} ->
        uncurry (quotedSharedMsgId,Just sentAt,Just content,,) $ case chatDir of
          CIQDirectSnd -> (Just True, Nothing)
          CIQDirectRcv -> (Just False, Nothing)
          CIQGroupSnd -> (Just True, Nothing)
          CIQGroupRcv (Just GroupMember {memberId}) -> (Just False, Just memberId)
          CIQGroupRcv Nothing -> (Just False, Nothing)

createNewRcvChatItem :: DB.Connection -> User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> CIContent 'MDRcv -> UTCTime -> UTCTime -> IO (ChatItemId, Maybe (CIQuote c))
createNewRcvChatItem db user chatDirection RcvMessage {msgId, chatMsgEvent} sharedMsgId_ ciContent itemTs createdAt = do
  ciId <- createNewChatItem_ db user chatDirection (Just msgId) sharedMsgId_ ciContent quoteRow itemTs createdAt
  quotedItem <- mapM (getChatItemQuote_ db user chatDirection) quotedMsg
  pure (ciId, quotedItem)
  where
    quotedMsg = cmToQuotedMsg chatMsgEvent
    quoteRow :: NewQuoteRow
    quoteRow = case quotedMsg of
      Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
      Just QuotedMsg {msgRef = MsgRef {msgId = sharedMsgId, sentAt, sent, memberId}, content} ->
        uncurry (sharedMsgId,Just sentAt,Just content,,) $ case chatDirection of
          CDDirectRcv _ -> (Just $ not sent, Nothing)
          CDGroupRcv GroupInfo {membership = GroupMember {memberId = userMemberId}} _ ->
            (Just $ Just userMemberId == memberId, memberId)

createNewChatItemNoMsg :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> CIContent d -> UTCTime -> UTCTime -> IO ChatItemId
createNewChatItemNoMsg db user chatDirection ciContent =
  createNewChatItem_ db user chatDirection Nothing Nothing ciContent quoteRow
  where
    quoteRow :: NewQuoteRow
    quoteRow = (Nothing, Nothing, Nothing, Nothing, Nothing)

createNewChatItem_ :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> Maybe MessageId -> Maybe SharedMsgId -> CIContent d -> NewQuoteRow -> UTCTime -> UTCTime -> IO ChatItemId
createNewChatItem_ db User {userId} chatDirection msgId_ sharedMsgId ciContent quoteRow itemTs createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO chat_items (
        -- user and IDs
        user_id, created_by_msg_id, contact_id, group_id, group_member_id,
        -- meta
        item_sent, item_ts, item_content, item_text, item_status, shared_msg_id, created_at, updated_at,
        -- quote
        quoted_shared_msg_id, quoted_sent_at, quoted_content, quoted_sent, quoted_member_id
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, msgId_) :. idsRow :. itemRow :. quoteRow)
  ciId <- insertedRowId db
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db ciId msgId createdAt
  pure ciId
  where
    itemRow :: (SMsgDirection d, UTCTime, CIContent d, Text, CIStatus d, Maybe SharedMsgId, UTCTime, UTCTime)
    itemRow = (msgDirection @d, itemTs, ciContent, ciContentToText ciContent, ciCreateStatus ciContent, sharedMsgId, createdAt, createdAt)
    idsRow :: (Maybe Int64, Maybe Int64, Maybe Int64)
    idsRow = case chatDirection of
      CDDirectRcv Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDDirectSnd Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDGroupRcv GroupInfo {groupId} GroupMember {groupMemberId} -> (Nothing, Just groupId, Just groupMemberId)
      CDGroupSnd GroupInfo {groupId} -> (Nothing, Just groupId, Nothing)

insertChatItemMessage_ :: DB.Connection -> ChatItemId -> MessageId -> UTCTime -> IO ()
insertChatItemMessage_ db ciId msgId ts = DB.execute db "INSERT INTO chat_item_messages (chat_item_id, message_id, created_at, updated_at) VALUES (?,?,?,?)" (ciId, msgId, ts, ts)

getChatItemQuote_ :: DB.Connection -> User -> ChatDirection c 'MDRcv -> QuotedMsg -> IO (CIQuote c)
getChatItemQuote_ db User {userId, userContactId} chatDirection QuotedMsg {msgRef = MsgRef {msgId, sentAt, sent, memberId}, content} =
  case chatDirection of
    CDDirectRcv Contact {contactId} -> getDirectChatItemQuote_ contactId (not sent)
    CDGroupRcv GroupInfo {groupId, membership = GroupMember {memberId = userMemberId}} sender@GroupMember {memberId = senderMemberId} ->
      case memberId of
        Just mId
          | mId == userMemberId -> (`ciQuote` CIQGroupSnd) <$> getUserGroupChatItemId_ groupId
          | mId == senderMemberId -> (`ciQuote` CIQGroupRcv (Just sender)) <$> getGroupChatItemId_ groupId mId
          | otherwise -> getGroupChatItemQuote_ groupId mId
        _ -> pure . ciQuote Nothing $ CIQGroupRcv Nothing
  where
    ciQuote :: Maybe ChatItemId -> CIQDirection c -> CIQuote c
    ciQuote itemId dir = CIQuote dir itemId msgId sentAt content . parseMaybeMarkdownList $ msgContentText content
    getDirectChatItemQuote_ :: Int64 -> Bool -> IO (CIQuote 'CTDirect)
    getDirectChatItemQuote_ contactId userSent = do
      fmap ciQuoteDirect . maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND contact_id = ? AND shared_msg_id = ? AND item_sent = ?"
          (userId, contactId, msgId, userSent)
      where
        ciQuoteDirect :: Maybe ChatItemId -> CIQuote 'CTDirect
        ciQuoteDirect = (`ciQuote` if userSent then CIQDirectSnd else CIQDirectRcv)
    getUserGroupChatItemId_ :: Int64 -> IO (Maybe ChatItemId)
    getUserGroupChatItemId_ groupId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND shared_msg_id = ? AND item_sent = ? AND group_member_id IS NULL"
          (userId, groupId, msgId, MDSnd)
    getGroupChatItemId_ :: Int64 -> MemberId -> IO (Maybe ChatItemId)
    getGroupChatItemId_ groupId mId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND shared_msg_id = ? AND item_sent = ? AND group_member_id = ?"
          (userId, groupId, msgId, MDRcv, mId)
    getGroupChatItemQuote_ :: Int64 -> MemberId -> IO (CIQuote 'CTGroup)
    getGroupChatItemQuote_ groupId mId = do
      ciQuoteGroup
        <$> DB.queryNamed
          db
          [sql|
            SELECT i.chat_item_id, 
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
              m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
              p.display_name, p.full_name, p.image, p.local_alias, p.preferences
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
            LEFT JOIN contacts c ON m.contact_id = c.contact_id
            LEFT JOIN chat_items i ON i.group_id = m.group_id
                                      AND m.group_member_id = i.group_member_id
                                      AND i.shared_msg_id = :msg_id
            WHERE m.user_id = :user_id AND m.group_id = :group_id AND m.member_id = :member_id
          |]
          [":user_id" := userId, ":group_id" := groupId, ":member_id" := mId, ":msg_id" := msgId]
      where
        ciQuoteGroup :: [Only (Maybe ChatItemId) :. GroupMemberRow] -> CIQuote 'CTGroup
        ciQuoteGroup [] = ciQuote Nothing $ CIQGroupRcv Nothing
        ciQuoteGroup ((Only itemId :. memberRow) : _) = ciQuote itemId . CIQGroupRcv . Just $ toGroupMember userContactId memberRow

getChatPreviews :: DB.Connection -> User -> Bool -> IO [AChat]
getChatPreviews db user withPCC = do
  directChats <- getDirectChatPreviews_ db user
  groupChats <- getGroupChatPreviews_ db user
  cReqChats <- getContactRequestChatPreviews_ db user
  connChats <- getContactConnectionChatPreviews_ db user withPCC
  pure $ sortOn (Down . ts) (directChats <> groupChats <> cReqChats <> connChats)
  where
    ts :: AChat -> UTCTime
    ts (AChat _ Chat {chatInfo, chatItems = ci : _}) = max (chatItemTs ci) (chatInfoUpdatedAt chatInfo)
    ts (AChat _ Chat {chatInfo}) = chatInfoUpdatedAt chatInfo

getDirectChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getDirectChatPreviews_ db user@User {userId} = do
  tz <- getCurrentTimeZone
  currentTs <- getCurrentTime
  map (toDirectChatPreview tz currentTs)
    <$> DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.local_alias, ct.contact_used, ct.enable_ntfs, cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at,
          -- ChatStats
          COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat,
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
          -- DirectQuote
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        JOIN connections c ON c.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, MAX(chat_item_id) AS MaxId
          FROM chat_items
          GROUP BY contact_id
        ) MaxIds ON MaxIds.contact_id = ct.contact_id
        LEFT JOIN chat_items i ON i.contact_id = MaxIds.contact_id
                               AND i.chat_item_id = MaxIds.MaxId
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = ?
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
        LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.contact_id = i.contact_id
        WHERE ct.user_id = ?
          AND ((c.conn_level = 0 AND c.via_group_link = 0) OR ct.contact_used = 1)
          AND c.connection_id = (
            SELECT cc_connection_id FROM (
              SELECT
                cc.connection_id AS cc_connection_id,
                (CASE WHEN cc.conn_status = ? OR cc.conn_status = ? THEN 1 ELSE 0 END) AS cc_conn_status_ord
              FROM connections cc
              WHERE cc.user_id = ct.user_id AND cc.contact_id = ct.contact_id
              ORDER BY cc_conn_status_ord DESC, cc_connection_id DESC
              LIMIT 1
            )
          )
        ORDER BY i.item_ts DESC
      |]
      (CISRcvNew, userId, ConnReady, ConnSndReady)
  where
    toDirectChatPreview :: TimeZone -> UTCTime -> ContactRow :. ConnectionRow :. ChatStatsRow :. MaybeChatItemRow :. QuoteRow -> AChat
    toDirectChatPreview tz currentTs (contactRow :. connRow :. statsRow :. ciRow_) =
      let contact = toContact user $ contactRow :. connRow
          ci_ = toDirectChatItemList tz currentTs ciRow_
          stats = toChatStats statsRow
       in AChat SCTDirect $ Chat (DirectChat contact) ci_ stats

getGroupChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getGroupChatPreviews_ db User {userId, userContactId} = do
  tz <- getCurrentTimeZone
  currentTs <- getCurrentTime
  map (toGroupChatPreview tz currentTs)
    <$> DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences,
          -- ChatStats
          COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat,
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
          -- Maybe GroupMember - sender
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
          m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
          p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
          -- quoted ChatItem
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
          -- quoted GroupMember
          rm.group_member_id, rm.group_id, rm.member_id, rm.member_role, rm.member_category,
          rm.member_status, rm.invited_by, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
          rp.display_name, rp.full_name, rp.image, rp.local_alias, rp.preferences
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        LEFT JOIN (
          SELECT group_id, MAX(chat_item_id) AS MaxId
          FROM chat_items
          GROUP BY group_id
        ) MaxIds ON MaxIds.group_id = g.group_id
        LEFT JOIN chat_items i ON i.group_id = MaxIds.group_id
                               AND i.chat_item_id = MaxIds.MaxId
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = ?
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
        LEFT JOIN group_members m ON m.group_member_id = i.group_member_id
        LEFT JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.group_id = i.group_id
        LEFT JOIN group_members rm ON rm.group_member_id = ri.group_member_id
        LEFT JOIN contact_profiles rp ON rp.contact_profile_id = COALESCE(rm.member_profile_id, rm.contact_profile_id)
        WHERE g.user_id = ? AND mu.contact_id = ?
        ORDER BY i.item_ts DESC
      |]
      (CISRcvNew, userId, userContactId)
  where
    toGroupChatPreview :: TimeZone -> UTCTime -> GroupInfoRow :. ChatStatsRow :. MaybeGroupChatItemRow -> AChat
    toGroupChatPreview tz currentTs (groupInfoRow :. statsRow :. ciRow_) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          ci_ = toGroupChatItemList tz currentTs userContactId ciRow_
          stats = toChatStats statsRow
       in AChat SCTGroup $ Chat (GroupChat groupInfo) ci_ stats

getContactRequestChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getContactRequestChatPreviews_ db User {userId} =
  map toContactRequestChatPreview
    <$> DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at
        FROM contact_requests cr
        JOIN connections c ON c.user_contact_link_id = cr.user_contact_link_id
        JOIN contact_profiles p ON p.contact_profile_id = cr.contact_profile_id
        JOIN user_contact_links uc ON uc.user_contact_link_id = cr.user_contact_link_id
        WHERE cr.user_id = ? AND uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
      |]
      (userId, userId)
  where
    toContactRequestChatPreview :: ContactRequestRow -> AChat
    toContactRequestChatPreview cReqRow =
      let cReq = toContactRequest cReqRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
       in AChat SCTContactRequest $ Chat (ContactRequest cReq) [] stats

getContactConnectionChatPreviews_ :: DB.Connection -> User -> Bool -> IO [AChat]
getContactConnectionChatPreviews_ _ _ False = pure []
getContactConnectionChatPreviews_ db User {userId} _ =
  map toContactConnectionChatPreview
    <$> DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ? AND conn_type = ? AND contact_id IS NULL AND conn_level = 0 AND via_contact IS NULL AND (via_group_link = 0 || (via_group_link = 1 AND group_link_id IS NOT NULL))
      |]
      (userId, ConnContact)
  where
    toContactConnectionChatPreview :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, LocalAlias, UTCTime, UTCTime) -> AChat
    toContactConnectionChatPreview connRow =
      let conn = toPendingContactConnection connRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
       in AChat SCTContactConnection $ Chat (ContactConnection conn) [] stats

getPendingContactConnection :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO PendingContactConnection
getPendingContactConnection db userId connId = do
  ExceptT . firstRow toPendingContactConnection (SEPendingConnectionNotFound connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ?
          AND connection_id = ?
          AND conn_type = ?
          AND contact_id IS NULL
          AND conn_level = 0
          AND via_contact IS NULL
      |]
      (userId, connId, ConnContact)

deletePendingContactConnection :: DB.Connection -> UserId -> Int64 -> IO ()
deletePendingContactConnection db userId connId =
  DB.execute
    db
    [sql|
      DELETE FROM connections
        WHERE user_id = ?
          AND connection_id = ?
          AND conn_type = ?
          AND contact_id IS NULL
          AND conn_level = 0
          AND via_contact IS NULL
    |]
    (userId, connId, ConnContact)

updateContactSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateContactSettings db User {userId} contactId ChatSettings {enableNtfs} =
  DB.execute db "UPDATE contacts SET enable_ntfs = ? WHERE user_id = ? AND contact_id = ?" (enableNtfs, userId, contactId)

updateGroupSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateGroupSettings db User {userId} groupId ChatSettings {enableNtfs} =
  DB.execute db "UPDATE groups SET enable_ntfs = ? WHERE user_id = ? AND group_id = ?" (enableNtfs, userId, groupId)

toPendingContactConnection :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, LocalAlias, UTCTime, UTCTime) -> PendingContactConnection
toPendingContactConnection (pccConnId, acId, pccConnStatus, connReqHash, viaUserContactLink, groupLinkId, customUserProfileId, connReqInv, localAlias, createdAt, updatedAt) =
  PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = isJust connReqHash, viaUserContactLink, groupLinkId, customUserProfileId, connReqInv, localAlias, createdAt, updatedAt}

getDirectChat :: DB.Connection -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChat db user contactId pagination search_ = do
  let search = fromMaybe "" search_
  case pagination of
    CPLast count -> getDirectChatLast_ db user contactId count search
    CPAfter afterId count -> getDirectChatAfter_ db user contactId afterId count search
    CPBefore beforeId count -> getDirectChatBefore_ db user contactId beforeId count search

getDirectChatLast_ :: DB.Connection -> User -> Int64 -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatLast_ db user@User {userId} contactId count search = do
  contact <- getContact db user contactId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- ExceptT getDirectChatItemsLast_
  pure $ Chat (DirectChat contact) (reverse chatItems) stats
  where
    getDirectChatItemsLast_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsLast_ = do
      tz <- getCurrentTimeZone
      currentTs <- getCurrentTime
      mapM (toDirectChatItem tz currentTs)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.contact_id = i.contact_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
            ORDER BY i.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, search, count)

getDirectChatAfter_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatAfter_ db user@User {userId} contactId afterChatItemId count search = do
  contact <- getContact db user contactId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- ExceptT getDirectChatItemsAfter_
  pure $ Chat (DirectChat contact) chatItems stats
  where
    getDirectChatItemsAfter_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsAfter_ = do
      tz <- getCurrentTimeZone
      currentTs <- getCurrentTime
      mapM (toDirectChatItem tz currentTs)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.contact_id = i.contact_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id > ?
            ORDER BY i.chat_item_id ASC
            LIMIT ?
          |]
          (userId, contactId, search, afterChatItemId, count)

getDirectChatBefore_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatBefore_ db user@User {userId} contactId beforeChatItemId count search = do
  contact <- getContact db user contactId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- ExceptT getDirectChatItemsBefore_
  pure $ Chat (DirectChat contact) (reverse chatItems) stats
  where
    getDirectChatItemsBefore_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsBefore_ = do
      tz <- getCurrentTimeZone
      currentTs <- getCurrentTime
      mapM (toDirectChatItem tz currentTs)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.contact_id = i.contact_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id < ?
            ORDER BY i.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, search, beforeChatItemId, count)

getContactIdByName :: DB.Connection -> User -> ContactName -> ExceptT StoreError IO Int64
getContactIdByName db User {userId} cName =
  ExceptT . firstRow fromOnly (SEContactNotFoundByName cName) $
    DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND local_display_name = ?" (userId, cName)

getContact :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO Contact
getContact db user@User {userId} contactId =
  ExceptT . fmap join . firstRow (toContactOrError user) (SEContactNotFound contactId) $
    DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.local_alias, ct.contact_used, ct.enable_ntfs, cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        LEFT JOIN connections c ON c.contact_id = ct.contact_id
        WHERE ct.user_id = ? AND ct.contact_id = ?
          AND c.connection_id = (
            SELECT cc_connection_id FROM (
              SELECT
                cc.connection_id AS cc_connection_id,
                (CASE WHEN cc.conn_status = ? OR cc.conn_status = ? THEN 1 ELSE 0 END) AS cc_conn_status_ord
              FROM connections cc
              WHERE cc.user_id = ct.user_id AND cc.contact_id = ct.contact_id
              ORDER BY cc_conn_status_ord DESC, cc_connection_id DESC
              LIMIT 1
            )
          )
      |]
      (userId, contactId, ConnReady, ConnSndReady)

getGroupChat :: DB.Connection -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChat db user groupId pagination search_ = do
  let search = fromMaybe "" search_
  case pagination of
    CPLast count -> getGroupChatLast_ db user groupId count search
    CPAfter afterId count -> getGroupChatAfter_ db user groupId afterId count search
    CPBefore beforeId count -> getGroupChatBefore_ db user groupId beforeId count search

getGroupChatLast_ :: DB.Connection -> User -> Int64 -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatLast_ db user@User {userId} groupId count search = do
  groupInfo <- getGroupInfo db user groupId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItemIds <- liftIO getGroupChatItemIdsLast_
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat groupInfo) (reverse chatItems) stats
  where
    getGroupChatItemIdsLast_ :: IO [ChatItemId]
    getGroupChatItemIdsLast_ =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, search, count)

getGroupChatAfter_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatAfter_ db user@User {userId} groupId afterChatItemId count search = do
  groupInfo <- getGroupInfo db user groupId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  afterChatItem <- getGroupChatItem db user groupId afterChatItemId
  chatItemIds <- liftIO $ getGroupChatItemIdsAfter_ (chatItemTs afterChatItem)
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat groupInfo) chatItems stats
  where
    getGroupChatItemIdsAfter_ :: UTCTime -> IO [ChatItemId]
    getGroupChatItemIdsAfter_ afterChatItemTs =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts > ? OR (item_ts = ? AND chat_item_id > ?))
            ORDER BY item_ts ASC, chat_item_id ASC
            LIMIT ?
          |]
          (userId, groupId, search, afterChatItemTs, afterChatItemTs, afterChatItemId, count)

getGroupChatBefore_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatBefore_ db user@User {userId} groupId beforeChatItemId count search = do
  groupInfo <- getGroupInfo db user groupId
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  beforeChatItem <- getGroupChatItem db user groupId beforeChatItemId
  chatItemIds <- liftIO $ getGroupChatItemIdsBefore_ (chatItemTs beforeChatItem)
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat groupInfo) (reverse chatItems) stats
  where
    getGroupChatItemIdsBefore_ :: UTCTime -> IO [ChatItemId]
    getGroupChatItemIdsBefore_ beforeChatItemTs =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, search, beforeChatItemTs, beforeChatItemTs, beforeChatItemId, count)

getGroupInfo :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO GroupInfo
getGroupInfo db User {userId, userContactId} groupId =
  ExceptT . firstRow (toGroupInfo userContactId) (SEGroupNotFound groupId) $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, gp.preferences, g.created_at, g.updated_at,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          pu.display_name, pu.full_name, pu.image, pu.local_alias, pu.preferences
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        WHERE g.group_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (groupId, userId, userContactId)

updateGroupProfile :: DB.Connection -> User -> GroupInfo -> GroupProfile -> ExceptT StoreError IO GroupInfo
updateGroupProfile db User {userId} g@GroupInfo {groupId, localDisplayName, groupProfile = GroupProfile {displayName}} p'@GroupProfile {displayName = newName, fullName, description, image, groupPreferences}
  | displayName == newName = liftIO $ do
    currentTs <- getCurrentTime
    updateGroupProfile_ currentTs $> (g :: GroupInfo) {groupProfile = p', fullGroupPreferences}
  | otherwise =
    ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
      currentTs <- getCurrentTime
      updateGroupProfile_ currentTs
      updateGroup_ ldn currentTs
      pure . Right $ (g :: GroupInfo) {localDisplayName = ldn, groupProfile = p', fullGroupPreferences}
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

getAllChatItems :: DB.Connection -> User -> ChatPagination -> Maybe String -> ExceptT StoreError IO [AChatItem]
getAllChatItems db user pagination search_ = do
  let search = fromMaybe "" search_
  case pagination of
    CPLast count -> getAllChatItemsLast_ db user count search
    CPAfter _afterId _count -> throwError $ SEInternalError "not implemented"
    CPBefore _beforeId _count -> throwError $ SEInternalError "not implemented"

getAllChatItemsLast_ :: DB.Connection -> User -> Int -> String -> ExceptT StoreError IO [AChatItem]
getAllChatItemsLast_ db user@User {userId} count search = do
  itemRefs <-
    liftIO $
      reverse . rights . map toChatItemRef
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id, contact_id, group_id
            FROM chat_items
            WHERE user_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, search, count)
  mapM (uncurry $ getAChatItem_ db user) itemRefs

getGroupIdByName :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO GroupId
getGroupIdByName db User {userId} gName =
  ExceptT . firstRow fromOnly (SEGroupNotFoundByName gName) $
    DB.query db "SELECT group_id FROM groups WHERE user_id = ? AND local_display_name = ?" (userId, gName)

getGroupMemberIdByName :: DB.Connection -> User -> GroupId -> ContactName -> ExceptT StoreError IO GroupMemberId
getGroupMemberIdByName db User {userId} groupId groupMemberName =
  ExceptT . firstRow fromOnly (SEGroupMemberNameNotFound groupId groupMemberName) $
    DB.query db "SELECT group_member_id FROM group_members WHERE user_id = ? AND group_id = ? AND local_display_name = ?" (userId, groupId, groupMemberName)

getChatItemIdByAgentMsgId :: DB.Connection -> Int64 -> AgentMsgId -> IO (Maybe ChatItemId)
getChatItemIdByAgentMsgId db connId msgId =
  fmap join . maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_item_messages
        WHERE message_id = (
          SELECT message_id
          FROM msg_deliveries
          WHERE connection_id = ? AND agent_msg_id = ?
          LIMIT 1
        )
      |]
      (connId, msgId)

updateDirectChatItemStatus :: forall d. MsgDirectionI d => DB.Connection -> UserId -> Int64 -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItemStatus db userId contactId itemId itemStatus = do
  ci <- liftEither . correctDir =<< getDirectChatItem db userId contactId itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, contactId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

updateDirectChatItem :: forall d. MsgDirectionI d => DB.Connection -> UserId -> Int64 -> ChatItemId -> CIContent d -> Maybe MessageId -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItem db userId contactId itemId newContent msgId_ = do
  currentTs <- liftIO getCurrentTime
  ci <- updateDirectChatItem_ db userId contactId itemId newContent currentTs
  forM_ msgId_ $ \msgId -> liftIO $ insertChatItemMessage_ db itemId msgId currentTs
  pure ci

updateDirectChatItem_ :: forall d. (MsgDirectionI d) => DB.Connection -> UserId -> Int64 -> ChatItemId -> CIContent d -> UTCTime -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItem_ db userId contactId itemId newContent currentTs = do
  ci <- liftEither . correctDir =<< getDirectChatItem db userId contactId itemId
  let newText = ciContentToText newContent
  liftIO $ do
    DB.execute
      db
      [sql|
        UPDATE chat_items
        SET item_content = ?, item_text = ?, item_deleted = 0, item_edited = 1, updated_at = ?
        WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
      |]
      (newContent, newText, currentTs, userId, contactId, itemId)
  pure ci {content = newContent, meta = (meta ci) {itemText = newText, itemEdited = True}, formattedText = parseMaybeMarkdownList newText}
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

deleteDirectChatItem :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> IO ()
deleteDirectChatItem db User {userId} Contact {contactId} (CChatItem _ ci) = do
  let itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (userId, contactId, itemId)

deleteChatItemMessages_ :: DB.Connection -> ChatItemId -> IO ()
deleteChatItemMessages_ db itemId =
  DB.execute
    db
    [sql|
      DELETE FROM messages
      WHERE message_id IN (
        SELECT message_id
        FROM chat_item_messages
        WHERE chat_item_id = ?
      )
    |]
    (Only itemId)

markDirectChatItemDeleted :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> MessageId -> IO AChatItem
markDirectChatItemDeleted db User {userId} ct@Contact {contactId} (CChatItem msgDir ci) msgId = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = 1, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (currentTs, userId, contactId, itemId)
  pure $ AChatItem SCTDirect msgDir (DirectChat ct) (ci {meta = (meta ci) {itemDeleted = True, editable = False}})

getDirectChatItemBySharedMsgId :: DB.Connection -> UserId -> ContactId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItemBySharedMsgId db userId contactId sharedMsgId = do
  itemId <- getDirectChatItemIdBySharedMsgId_ db userId contactId sharedMsgId
  getDirectChatItem db userId contactId itemId

getDirectChatItemByAgentMsgId :: DB.Connection -> UserId -> ContactId -> Int64 -> AgentMsgId -> IO (Maybe (CChatItem 'CTDirect))
getDirectChatItemByAgentMsgId db userId contactId connId msgId = do
  itemId_ <- getChatItemIdByAgentMsgId db connId msgId
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getDirectChatItem db userId contactId) itemId_

getDirectChatItemIdBySharedMsgId_ :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getDirectChatItemIdBySharedMsgId_ db userId contactId sharedMsgId =
  ExceptT . firstRow fromOnly (SEChatItemSharedMsgIdNotFound sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND shared_msg_id = ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, contactId, sharedMsgId)

getDirectChatItem :: DB.Connection -> UserId -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItem db userId contactId itemId = ExceptT $ do
  tz <- getCurrentTimeZone
  currentTs <- getCurrentTime
  join <$> firstRow (toDirectChatItem tz currentTs) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
            -- DirectQuote
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
          FROM chat_items i
          LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
          LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.contact_id = i.contact_id
          WHERE i.user_id = ? AND i.contact_id = ? AND i.chat_item_id = ?
        |]
        (userId, contactId, itemId)

getDirectChatItemIdByText :: DB.Connection -> UserId -> Int64 -> SMsgDirection d -> Text -> ExceptT StoreError IO ChatItemId
getDirectChatItemIdByText db userId contactId msgDir quotedMsg =
  ExceptT . firstRow fromOnly SEQuotedChatItemNotFound $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_sent = ? AND item_text like ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, contactId, msgDir, quotedMsg <> "%")

updateGroupChatItem :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItemId -> CIContent d -> MessageId -> ExceptT StoreError IO (ChatItem 'CTGroup d)
updateGroupChatItem db user@User {userId} groupId itemId newContent msgId = do
  ci <- liftEither . correctDir =<< getGroupChatItem db user groupId itemId
  currentTs <- liftIO getCurrentTime
  let newText = ciContentToText newContent
  liftIO $ do
    DB.execute
      db
      [sql|
        UPDATE chat_items
        SET item_content = ?, item_text = ?, item_deleted = 0, item_edited = 1, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
      |]
      (newContent, newText, currentTs, userId, groupId, itemId)
    insertChatItemMessage_ db itemId msgId currentTs
  pure ci {content = newContent, meta = (meta ci) {itemText = newText, itemEdited = True}, formattedText = parseMaybeMarkdownList newText}
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

deleteGroupChatItem :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> IO ()
deleteGroupChatItem db User {userId} GroupInfo {groupId} (CChatItem _ ci) = do
  let itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (userId, groupId, itemId)

markGroupChatItemDeleted :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> MessageId -> IO AChatItem
markGroupChatItemDeleted db User {userId} gInfo@GroupInfo {groupId} (CChatItem msgDir ci) msgId = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = 1, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (currentTs, userId, groupId, itemId)
  pure $ AChatItem SCTGroup msgDir (GroupChat gInfo) (ci {meta = (meta ci) {itemDeleted = True, editable = False}})

getGroupChatItemBySharedMsgId :: DB.Connection -> User -> GroupId -> GroupMemberId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItemBySharedMsgId db user@User {userId} groupId groupMemberId sharedMsgId = do
  itemId <-
    ExceptT . firstRow fromOnly (SEChatItemSharedMsgIdNotFound sharedMsgId) $
      DB.query
        db
        [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND group_member_id = ? AND shared_msg_id = ?
            ORDER BY chat_item_id DESC
            LIMIT 1
          |]
        (userId, groupId, groupMemberId, sharedMsgId)
  getGroupChatItem db user groupId itemId

getGroupChatItem :: DB.Connection -> User -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItem db User {userId, userContactId} groupId itemId = ExceptT $ do
  tz <- getCurrentTimeZone
  currentTs <- getCurrentTime
  join <$> firstRow (toGroupChatItem tz currentTs userContactId) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_edited, i.created_at, i.updated_at,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.ci_file_status,
            -- GroupMember
            m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
            m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
            p.display_name, p.full_name, p.image, p.local_alias, p.preferences,
            -- quoted ChatItem
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
            -- quoted GroupMember
            rm.group_member_id, rm.group_id, rm.member_id, rm.member_role, rm.member_category,
            rm.member_status, rm.invited_by, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
            rp.display_name, rp.full_name, rp.image, rp.local_alias, rp.preferences
          FROM chat_items i
          LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
          LEFT JOIN group_members m ON m.group_member_id = i.group_member_id
          LEFT JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
          LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.group_id = i.group_id
          LEFT JOIN group_members rm ON rm.group_member_id = ri.group_member_id
          LEFT JOIN contact_profiles rp ON rp.contact_profile_id = COALESCE(rm.member_profile_id, rm.contact_profile_id)
          WHERE i.user_id = ? AND i.group_id = ? AND i.chat_item_id = ?
        |]
        (userId, groupId, itemId)

getGroupChatItemIdByText :: DB.Connection -> User -> Int64 -> Maybe ContactName -> Text -> ExceptT StoreError IO ChatItemId
getGroupChatItemIdByText db User {userId, localDisplayName = userName} groupId contactName_ quotedMsg =
  ExceptT . firstRow fromOnly SEQuotedChatItemNotFound $ case contactName_ of
    Nothing -> anyMemberChatItem_
    Just cName
      | userName == cName -> userChatItem_
      | otherwise -> memberChatItem_ cName
  where
    anyMemberChatItem_ =
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND item_text like ?
          ORDER BY chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, quotedMsg <> "%")
    userChatItem_ =
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND group_member_id IS NULL AND item_text like ?
          ORDER BY chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, quotedMsg <> "%")
    memberChatItem_ cName =
      DB.query
        db
        [sql|
          SELECT i.chat_item_id
          FROM chat_items i
          JOIN group_members m ON m.group_member_id = i.group_member_id
          JOIN contacts c ON c.contact_id = m.contact_id
          WHERE i.user_id = ? AND i.group_id = ? AND c.local_display_name = ? AND i.item_text like ?
          ORDER BY i.chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, cName, quotedMsg <> "%")

getChatItemByFileId :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO AChatItem
getChatItemByFileId db user@User {userId} fileId = do
  (itemId, chatRef) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByFileId fileId) $
      DB.query
        db
        [sql|
            SELECT i.chat_item_id, i.contact_id, i.group_id
            FROM chat_items i
            JOIN files f ON f.chat_item_id = i.chat_item_id
            WHERE f.user_id = ? AND f.file_id = ?
            LIMIT 1
          |]
        (userId, fileId)
  getAChatItem_ db user itemId chatRef

getChatItemByGroupId :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO AChatItem
getChatItemByGroupId db user@User {userId} groupId = do
  (itemId, chatRef) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByGroupId groupId) $
      DB.query
        db
        [sql|
          SELECT i.chat_item_id, i.contact_id, i.group_id
          FROM chat_items i
          JOIN groups g ON g.chat_item_id = i.chat_item_id
          WHERE g.user_id = ? AND g.group_id = ?
          LIMIT 1
        |]
        (userId, groupId)
  getAChatItem_ db user itemId chatRef

getAChatItem_ :: DB.Connection -> User -> ChatItemId -> ChatRef -> ExceptT StoreError IO AChatItem
getAChatItem_ db user@User {userId} itemId = \case
  ChatRef CTDirect contactId -> do
    ct <- getContact db user contactId
    (CChatItem msgDir ci) <- getDirectChatItem db userId contactId itemId
    pure $ AChatItem SCTDirect msgDir (DirectChat ct) ci
  ChatRef CTGroup groupId -> do
    gInfo <- getGroupInfo db user groupId
    (CChatItem msgDir ci) <- getGroupChatItem db user groupId itemId
    pure $ AChatItem SCTGroup msgDir (GroupChat gInfo) ci
  _ -> throwError $ SEChatItemNotFound itemId

updateDirectCIFileStatus :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> CIFileStatus d -> ExceptT StoreError IO AChatItem
updateDirectCIFileStatus db user fileId fileStatus = do
  aci@(AChatItem cType d cInfo ci) <- getChatItemByFileId db user fileId
  case (cType, testEquality d $ msgDirection @d) of
    (SCTDirect, Just Refl) -> do
      liftIO $ updateCIFileStatus db user fileId fileStatus
      pure $ AChatItem SCTDirect d cInfo $ updateFileStatus ci fileStatus
    _ -> pure aci

toChatItemRef :: (ChatItemId, Maybe Int64, Maybe Int64) -> Either StoreError (ChatItemId, ChatRef)
toChatItemRef = \case
  (itemId, Just contactId, Nothing) -> Right (itemId, ChatRef CTDirect contactId)
  (itemId, Nothing, Just groupId) -> Right (itemId, ChatRef CTGroup groupId)
  (itemId, _, _) -> Left $ SEBadChatItem itemId

updateDirectChatItemsRead :: DB.Connection -> Int64 -> Maybe (ChatItemId, ChatItemId) -> IO ()
updateDirectChatItemsRead db contactId itemsRange_ = do
  currentTs <- getCurrentTime
  case itemsRange_ of
    Just (fromItemId, toItemId) ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE contact_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, contactId, fromItemId, toItemId, CISRcvNew)
    _ ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE contact_id = ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, contactId, CISRcvNew)

updateGroupChatItemsRead :: DB.Connection -> Int64 -> Maybe (ChatItemId, ChatItemId) -> IO ()
updateGroupChatItemsRead db groupId itemsRange_ = do
  currentTs <- getCurrentTime
  case itemsRange_ of
    Just (fromItemId, toItemId) ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE group_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, groupId, fromItemId, toItemId, CISRcvNew)
    _ ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE group_id = ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, groupId, CISRcvNew)

type ChatStatsRow = (Int, ChatItemId, Bool)

toChatStats :: ChatStatsRow -> ChatStats
toChatStats (unreadCount, minUnreadItemId, unreadChat) = ChatStats {unreadCount, minUnreadItemId, unreadChat}

type MaybeCIFIleRow = (Maybe Int64, Maybe String, Maybe Integer, Maybe FilePath, Maybe ACIFileStatus)

type ChatItemRow = (Int64, ChatItemTs, ACIContent, Text, ACIStatus, Maybe SharedMsgId, Bool, Maybe Bool, UTCTime, UTCTime) :. MaybeCIFIleRow

type MaybeChatItemRow = (Maybe Int64, Maybe ChatItemTs, Maybe ACIContent, Maybe Text, Maybe ACIStatus, Maybe SharedMsgId, Maybe Bool, Maybe Bool, Maybe UTCTime, Maybe UTCTime) :. MaybeCIFIleRow

type QuoteRow = (Maybe ChatItemId, Maybe SharedMsgId, Maybe UTCTime, Maybe MsgContent, Maybe Bool)

toDirectQuote :: QuoteRow -> Maybe (CIQuote 'CTDirect)
toDirectQuote qr@(_, _, _, _, quotedSent) = toQuote qr $ direction <$> quotedSent
  where
    direction sent = if sent then CIQDirectSnd else CIQDirectRcv

toQuote :: QuoteRow -> Maybe (CIQDirection c) -> Maybe (CIQuote c)
toQuote (quotedItemId, quotedSharedMsgId, quotedSentAt, quotedMsgContent, _) dir =
  CIQuote <$> dir <*> pure quotedItemId <*> pure quotedSharedMsgId <*> quotedSentAt <*> quotedMsgContent <*> (parseMaybeMarkdownList . msgContentText <$> quotedMsgContent)

toDirectChatItem :: TimeZone -> UTCTime -> ChatItemRow :. QuoteRow -> Either StoreError (CChatItem 'CTDirect)
toDirectChatItem tz currentTs (((itemId, itemTs, itemContent, itemText, itemStatus, sharedMsgId, itemDeleted, itemEdited, createdAt, updatedAt) :. (fileId_, fileName_, fileSize_, filePath, fileStatus_)) :. quoteRow) =
  case (itemContent, itemStatus, fileStatus_) of
    (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Just (AFS SMDSnd fileStatus)) ->
      Right $ cItem SMDSnd CIDirectSnd ciStatus ciContent (maybeCIFile fileStatus)
    (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Nothing) ->
      Right $ cItem SMDSnd CIDirectSnd ciStatus ciContent Nothing
    (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just (AFS SMDRcv fileStatus)) ->
      Right $ cItem SMDRcv CIDirectRcv ciStatus ciContent (maybeCIFile fileStatus)
    (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Nothing) ->
      Right $ cItem SMDRcv CIDirectRcv ciStatus ciContent Nothing
    _ -> badItem
  where
    maybeCIFile :: CIFileStatus d -> Maybe (CIFile d)
    maybeCIFile fileStatus =
      case (fileId_, fileName_, fileSize_) of
        (Just fileId, Just fileName, Just fileSize) -> Just CIFile {fileId, fileName, fileSize, filePath, fileStatus}
        _ -> Nothing
    cItem :: MsgDirectionI d => SMsgDirection d -> CIDirection 'CTDirect d -> CIStatus d -> CIContent d -> Maybe (CIFile d) -> CChatItem 'CTDirect
    cItem d chatDir ciStatus content file =
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, formattedText = parseMaybeMarkdownList itemText, quotedItem = toDirectQuote quoteRow, file}
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIContent d -> CIStatus d -> CIMeta d
    ciMeta content status = mkCIMeta itemId content itemText status sharedMsgId itemDeleted (fromMaybe False itemEdited) tz currentTs itemTs createdAt updatedAt

toDirectChatItemList :: TimeZone -> UTCTime -> MaybeChatItemRow :. QuoteRow -> [CChatItem 'CTDirect]
toDirectChatItemList tz currentTs (((Just itemId, Just itemTs, Just itemContent, Just itemText, Just itemStatus, sharedMsgId, Just itemDeleted, itemEdited, Just createdAt, Just updatedAt) :. fileRow) :. quoteRow) =
  either (const []) (: []) $ toDirectChatItem tz currentTs (((itemId, itemTs, itemContent, itemText, itemStatus, sharedMsgId, itemDeleted, itemEdited, createdAt, updatedAt) :. fileRow) :. quoteRow)
toDirectChatItemList _ _ _ = []

type GroupQuoteRow = QuoteRow :. MaybeGroupMemberRow

type MaybeGroupChatItemRow = MaybeChatItemRow :. MaybeGroupMemberRow :. GroupQuoteRow

toGroupQuote :: QuoteRow -> Maybe GroupMember -> Maybe (CIQuote 'CTGroup)
toGroupQuote qr@(_, _, _, _, quotedSent) quotedMember_ = toQuote qr $ direction quotedSent quotedMember_
  where
    direction (Just True) _ = Just CIQGroupSnd
    direction (Just False) (Just member) = Just . CIQGroupRcv $ Just member
    direction (Just False) Nothing = Just $ CIQGroupRcv Nothing
    direction _ _ = Nothing

toGroupChatItem :: TimeZone -> UTCTime -> Int64 -> ChatItemRow :. MaybeGroupMemberRow :. GroupQuoteRow -> Either StoreError (CChatItem 'CTGroup)
toGroupChatItem tz currentTs userContactId (((itemId, itemTs, itemContent, itemText, itemStatus, sharedMsgId, itemDeleted, itemEdited, createdAt, updatedAt) :. (fileId_, fileName_, fileSize_, filePath, fileStatus_)) :. memberRow_ :. quoteRow :. quotedMemberRow_) = do
  let member_ = toMaybeGroupMember userContactId memberRow_
  let quotedMember_ = toMaybeGroupMember userContactId quotedMemberRow_
  case (itemContent, itemStatus, member_, fileStatus_) of
    (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, _, Just (AFS SMDSnd fileStatus)) ->
      Right $ cItem SMDSnd CIGroupSnd ciStatus ciContent quotedMember_ (maybeCIFile fileStatus)
    (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, _, Nothing) ->
      Right $ cItem SMDSnd CIGroupSnd ciStatus ciContent quotedMember_ Nothing
    (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just member, Just (AFS SMDRcv fileStatus)) ->
      Right $ cItem SMDRcv (CIGroupRcv member) ciStatus ciContent quotedMember_ (maybeCIFile fileStatus)
    (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just member, Nothing) ->
      Right $ cItem SMDRcv (CIGroupRcv member) ciStatus ciContent quotedMember_ Nothing
    _ -> badItem
  where
    maybeCIFile :: CIFileStatus d -> Maybe (CIFile d)
    maybeCIFile fileStatus =
      case (fileId_, fileName_, fileSize_) of
        (Just fileId, Just fileName, Just fileSize) -> Just CIFile {fileId, fileName, fileSize, filePath, fileStatus}
        _ -> Nothing
    cItem :: MsgDirectionI d => SMsgDirection d -> CIDirection 'CTGroup d -> CIStatus d -> CIContent d -> Maybe GroupMember -> Maybe (CIFile d) -> CChatItem 'CTGroup
    cItem d chatDir ciStatus content quotedMember_ file =
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, formattedText = parseMaybeMarkdownList itemText, quotedItem = toGroupQuote quoteRow quotedMember_, file}
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIContent d -> CIStatus d -> CIMeta d
    ciMeta content status = mkCIMeta itemId content itemText status sharedMsgId itemDeleted (fromMaybe False itemEdited) tz currentTs itemTs createdAt updatedAt

toGroupChatItemList :: TimeZone -> UTCTime -> Int64 -> MaybeGroupChatItemRow -> [CChatItem 'CTGroup]
toGroupChatItemList tz currentTs userContactId (((Just itemId, Just itemTs, Just itemContent, Just itemText, Just itemStatus, sharedMsgId, Just itemDeleted, itemEdited, Just createdAt, Just updatedAt) :. fileRow) :. memberRow_ :. quoteRow :. quotedMemberRow_) =
  either (const []) (: []) $ toGroupChatItem tz currentTs userContactId (((itemId, itemTs, itemContent, itemText, itemStatus, sharedMsgId, itemDeleted, itemEdited, createdAt, updatedAt) :. fileRow) :. memberRow_ :. quoteRow :. quotedMemberRow_)
toGroupChatItemList _ _ _ _ = []

getSMPServers :: DB.Connection -> User -> IO [ServerCfg]
getSMPServers db User {userId} =
  map toServerCfg
    <$> DB.query
      db
      [sql|
        SELECT host, port, key_hash, basic_auth, preset, tested, enabled
        FROM smp_servers
        WHERE user_id = ?;
      |]
      (Only userId)
  where
    toServerCfg :: (NonEmpty TransportHost, String, C.KeyHash, Maybe Text, Bool, Maybe Bool, Bool) -> ServerCfg
    toServerCfg (host, port, keyHash, auth_, preset, tested, enabled) =
      let server = ProtoServerWithAuth (SMPServer host port keyHash) (BasicAuth . encodeUtf8 <$> auth_)
       in ServerCfg {server, preset, tested, enabled}

overwriteSMPServers :: DB.Connection -> User -> [ServerCfg] -> ExceptT StoreError IO ()
overwriteSMPServers db User {userId} servers =
  checkConstraint SEUniqueID . ExceptT $ do
    currentTs <- getCurrentTime
    DB.execute db "DELETE FROM smp_servers WHERE user_id = ?" (Only userId)
    forM_ servers $ \ServerCfg {server, preset, tested, enabled} -> do
      let ProtoServerWithAuth ProtocolServer {host, port, keyHash} auth_ = server
      DB.execute
        db
        [sql|
          INSERT INTO smp_servers
            (host, port, key_hash, basic_auth, preset, tested, enabled, user_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?)
        |]
        (host, port, keyHash, safeDecodeUtf8 . unBasicAuth <$> auth_, preset, tested, enabled, userId, currentTs, currentTs)
    pure $ Right ()

createCall :: DB.Connection -> User -> Call -> UTCTime -> IO ()
createCall db User {userId} Call {contactId, callId, chatItemId, callState} callTs = do
  currentTs <- getCurrentTime
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

getCalls :: DB.Connection -> User -> IO [Call]
getCalls db User {userId} = do
  map toCall
    <$> DB.query
      db
      [sql|
        SELECT
          contact_id, shared_call_id, chat_item_id, call_state, call_ts
        FROM calls
        WHERE user_id = ?
        ORDER BY call_ts ASC
      |]
      (Only userId)
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

setCommandConnId :: DB.Connection -> User -> CommandId -> Int64 -> IO ()
setCommandConnId db User {userId} cmdId connId = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE commands
      SET connection_id = ?, updated_at = ?
      WHERE user_id = ? AND command_id = ?
    |]
    (connId, updatedAt, userId, cmdId)

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

setConnConnReqInv :: DB.Connection -> User -> Int64 -> ConnReqInvitation -> IO ()
setConnConnReqInv db User {userId} connId connReq = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET conn_req_inv = ?, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (connReq, updatedAt, userId, connId)

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
          where cc.group_member_id = mh.group_member_id
        )
        WHERE ct.user_id = ? AND ct.contact_id = ? AND mh.member_category = ?
      |]
      (userId, contactId, GCHostMember)
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
          where cc.group_member_id = mh.group_member_id
        )
        WHERE m.user_id = ? AND m.group_member_id = ? AND mh.member_category = ?
      |]
      (userId, groupMemberId, GCHostMember)
  where
    toCont :: (Int64, Maybe ConnReqInvitation) -> Maybe (Int64, ConnReqInvitation)
    toCont (hostConnId, connReq_) = case connReq_ of
      Just connReq -> Just (hostConnId, connReq)
      _ -> Nothing

getChatItemTTL :: DB.Connection -> User -> IO (Maybe Int64)
getChatItemTTL db User {userId} =
  fmap join . maybeFirstRow fromOnly $ DB.query db "SELECT chat_item_ttl FROM settings WHERE user_id = ? LIMIT 1" (Only userId)

setChatItemTTL :: DB.Connection -> User -> Maybe Int64 -> IO ()
setChatItemTTL db User {userId} chatItemTTL = do
  currentTs <- getCurrentTime
  r :: (Maybe Int64) <- maybeFirstRow fromOnly $ DB.query db "SELECT 1 FROM settings WHERE user_id = ? LIMIT 1" (Only userId)
  case r of
    Just _ -> do
      DB.execute
        db
        "UPDATE settings SET chat_item_ttl = ?, updated_at = ? WHERE user_id = ?"
        (chatItemTTL, currentTs, userId)
    Nothing -> do
      DB.execute
        db
        "INSERT INTO settings (user_id, chat_item_ttl, created_at, updated_at) VALUES (?,?,?,?)"
        (userId, chatItemTTL, currentTs, currentTs)

getContactExpiredFileInfo :: DB.Connection -> User -> Contact -> UTCTime -> IO [CIFileInfo]
getContactExpiredFileInfo db User {userId} Contact {contactId} expirationDate =
  map toFileInfo
    <$> DB.query
      db
      [sql|
        SELECT f.file_id, f.ci_file_status, f.file_path
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.created_at <= ?
      |]
      (userId, contactId, expirationDate)

deleteContactExpiredCIs :: DB.Connection -> User -> Contact -> UTCTime -> IO ()
deleteContactExpiredCIs db user@User {userId} ct@Contact {contactId} expirationDate = do
  connIds <- getContactConnIds_ db user ct
  forM_ connIds $ \connId ->
    DB.execute db "DELETE FROM messages WHERE connection_id = ? AND created_at <= ?" (connId, expirationDate)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ? AND created_at <= ?" (userId, contactId, expirationDate)

getContactCICount :: DB.Connection -> User -> Contact -> IO (Maybe Int64)
getContactCICount db User {userId} Contact {contactId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT COUNT(1) FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)

getGroupExpiredFileInfo :: DB.Connection -> User -> GroupInfo -> UTCTime -> UTCTime -> IO [CIFileInfo]
getGroupExpiredFileInfo db User {userId} GroupInfo {groupId} expirationDate createdAtCutoff =
  map toFileInfo
    <$> DB.query
      db
      [sql|
        SELECT f.file_id, f.ci_file_status, f.file_path
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE i.user_id = ? AND i.group_id = ? AND i.item_ts <= ? AND i.created_at <= ?
      |]
      (userId, groupId, expirationDate, createdAtCutoff)

deleteGroupExpiredCIs :: DB.Connection -> User -> GroupInfo -> UTCTime -> UTCTime -> IO ()
deleteGroupExpiredCIs db User {userId} GroupInfo {groupId} expirationDate createdAtCutoff = do
  DB.execute db "DELETE FROM messages WHERE group_id = ? AND created_at <= ?" (groupId, min expirationDate createdAtCutoff)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ? AND item_ts <= ? AND created_at <= ?" (userId, groupId, expirationDate, createdAtCutoff)

getGroupCICount :: DB.Connection -> User -> GroupInfo -> IO (Maybe Int64)
getGroupCICount db User {userId} GroupInfo {groupId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT COUNT(1) FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

-- | Saves unique local display name based on passed displayName, suffixed with _N if required.
-- This function should be called inside transaction.
withLocalDisplayName :: forall a. DB.Connection -> UserId -> Text -> (Text -> IO (Either StoreError a)) -> IO (Either StoreError a)
withLocalDisplayName db userId displayName action = getLdnSuffix >>= (`tryCreateName` 20)
  where
    getLdnSuffix :: IO Int
    getLdnSuffix =
      maybe 0 ((+ 1) . fromOnly) . listToMaybe
        <$> DB.queryNamed
          db
          [sql|
            SELECT ldn_suffix FROM display_names
            WHERE user_id = :user_id AND ldn_base = :display_name
            ORDER BY ldn_suffix DESC
            LIMIT 1
          |]
          [":user_id" := userId, ":display_name" := displayName]
    tryCreateName :: Int -> Int -> IO (Either StoreError a)
    tryCreateName _ 0 = pure $ Left SEDuplicateName
    tryCreateName ldnSuffix attempts = do
      currentTs <- getCurrentTime
      let ldn = displayName <> (if ldnSuffix == 0 then "" else T.pack $ '_' : show ldnSuffix)
      E.try (insertName ldn currentTs) >>= \case
        Right () -> action ldn
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreateName (ldnSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        insertName ldn ts =
          DB.execute
            db
            [sql|
              INSERT INTO display_names
                (local_display_name, ldn_base, ldn_suffix, user_id, created_at, updated_at)
              VALUES (?,?,?,?,?,?)
            |]
            (ldn, displayName, ldnSuffix, userId, ts, ts)

createWithRandomId :: forall a. TVar ChaChaDRG -> (ByteString -> IO a) -> ExceptT StoreError IO a
createWithRandomId = createWithRandomBytes 12

createWithRandomBytes :: forall a. Int -> TVar ChaChaDRG -> (ByteString -> IO a) -> ExceptT StoreError IO a
createWithRandomBytes size gVar create = tryCreate 3
  where
    tryCreate :: Int -> ExceptT StoreError IO a
    tryCreate 0 = throwError SEUniqueID
    tryCreate n = do
      id' <- liftIO $ encodedRandomBytes gVar size
      liftIO (E.try $ create id') >>= \case
        Right x -> pure x
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> throwError . SEInternalError $ show e

encodedRandomBytes :: TVar ChaChaDRG -> Int -> IO ByteString
encodedRandomBytes gVar = fmap B64.encode . randomBytes gVar

randomBytes :: TVar ChaChaDRG -> Int -> IO ByteString
randomBytes gVar = atomically . stateTVar gVar . randomBytesGenerate

-- These error type constructors must be added to mobile apps
data StoreError
  = SEDuplicateName
  | SEContactNotFound {contactId :: Int64}
  | SEContactNotFoundByName {contactName :: ContactName}
  | SEContactNotReady {contactName :: ContactName}
  | SEDuplicateContactLink
  | SEUserContactLinkNotFound
  | SEContactRequestNotFound {contactRequestId :: Int64}
  | SEContactRequestNotFoundByName {contactName :: ContactName}
  | SEGroupNotFound {groupId :: GroupId}
  | SEGroupNotFoundByName {groupName :: GroupName}
  | SEGroupMemberNameNotFound {groupId :: GroupId, groupMemberName :: ContactName}
  | SEGroupMemberNotFound {groupId :: GroupId, groupMemberId :: GroupMemberId}
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEGroupAlreadyJoined
  | SEGroupInvitationNotFound
  | SESndFileNotFound {fileId :: FileTransferId}
  | SESndFileInvalid {fileId :: FileTransferId}
  | SERcvFileNotFound {fileId :: FileTransferId}
  | SEFileNotFound {fileId :: FileTransferId}
  | SERcvFileInvalid {fileId :: FileTransferId}
  | SESharedMsgIdNotFoundByFileId {fileId :: FileTransferId}
  | SEFileIdNotFoundBySharedMsgId {sharedMsgId :: SharedMsgId}
  | SEConnectionNotFound {agentConnId :: AgentConnId}
  | SEConnectionNotFoundById {connId :: Int64}
  | SEPendingConnectionNotFound {connId :: Int64}
  | SEIntroNotFound
  | SEUniqueID
  | SEInternalError {message :: String}
  | SENoMsgDelivery {connId :: Int64, agentMsgId :: AgentMsgId}
  | SEBadChatItem {itemId :: ChatItemId}
  | SEChatItemNotFound {itemId :: ChatItemId}
  | SEQuotedChatItemNotFound
  | SEChatItemSharedMsgIdNotFound {sharedMsgId :: SharedMsgId}
  | SEChatItemNotFoundByFileId {fileId :: FileTransferId}
  | SEChatItemNotFoundByGroupId {groupId :: GroupId}
  | SEProfileNotFound {profileId :: Int64}
  | SEDuplicateGroupLink {groupInfo :: GroupInfo}
  | SEGroupLinkNotFound {groupInfo :: GroupInfo}
  | SEHostMemberIdNotFound {groupId :: Int64}
  deriving (Show, Exception, Generic)

instance ToJSON StoreError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SE"
