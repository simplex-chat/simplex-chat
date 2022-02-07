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
    createStore,
    chatStoreFile,
    createUser,
    getUsers,
    setActiveUser,
    createDirectConnection,
    createDirectContact,
    getContactGroupNames,
    deleteContact,
    getContactByName,
    getContact,
    getContactIdByName,
    updateUserProfile,
    updateContactProfile,
    getUserContacts,
    createUserContactLink,
    getUserContactLinkConnections,
    deleteUserContactLink,
    getUserContactLink,
    createContactRequest,
    getContactRequest,
    getContactRequestIdByName,
    deleteContactRequest,
    createAcceptedContact,
    getLiveSndFileTransfers,
    getLiveRcvFileTransfers,
    getPendingSndChunks,
    getPendingConnections,
    getContactConnections,
    getConnectionEntity,
    updateConnectionStatus,
    createNewGroup,
    createGroupInvitation,
    getGroup,
    getGroupInfo,
    getGroupIdByName,
    getGroupByName,
    getGroupInfoByName,
    getGroupMembers,
    deleteGroup,
    getUserGroups,
    getUserGroupDetails,
    getGroupInvitation,
    createContactMember,
    getMemberInvitation,
    createMemberConnection,
    updateGroupMemberStatus,
    createNewGroupMember,
    deleteGroupMemberConnection,
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
    matchReceivedProbe,
    matchReceivedProbeHash,
    matchSentProbe,
    mergeContactRecords,
    createSndFileTransfer,
    createSndGroupFileTransfer,
    updateSndFileStatus,
    createSndFileChunk,
    updateSndFileChunkMsg,
    updateSndFileChunkSent,
    deleteSndFileChunks,
    createRcvFileTransfer,
    createRcvGroupFileTransfer,
    getRcvFileTransfer,
    acceptRcvFileTransfer,
    updateRcvFileStatus,
    createRcvFileChunk,
    updatedRcvFileChunkStored,
    deleteRcvFileChunks,
    updateFileTransferChatItemId,
    getFileTransfer,
    getFileTransferProgress,
    createNewMessage,
    createSndMsgDelivery,
    createNewMessageAndRcvMsgDelivery,
    createSndMsgDeliveryEvent,
    createRcvMsgDeliveryEvent,
    createPendingGroupMessage,
    getPendingGroupMessages,
    deletePendingGroupMessage,
    createNewChatItem,
    getChatPreviews,
    getDirectChat,
    getGroupChat,
    getChatItemIdByAgentMsgId,
    updateDirectChatItem,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (stateTVar)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.IO.Unlift
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
import Data.List (find, sortBy, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import Data.Type.Equality
import Database.SQLite.Simple (NamedParam (..), Only (..), Query (..), SQLError, (:.) (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Simplex.Chat.Messages
import Simplex.Chat.Migrations.M20220101_initial
import Simplex.Chat.Migrations.M20220122_v1_1
import Simplex.Chat.Migrations.M20220205_chat_item_status
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Util (eitherToMaybe)
import Simplex.Messaging.Agent.Protocol (AgentMsgId, ConnId, InvitationId, MsgMeta (..))
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, firstRow, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Util (liftIOEither, (<$$>))
import System.FilePath (takeFileName)
import UnliftIO.STM

schemaMigrations :: [(String, Query)]
schemaMigrations =
  [ ("20220101_initial", m20220101_initial),
    ("20220122_v1_1", m20220122_v1_1),
    ("20220205_chat_item_status", m20220205_chat_item_status)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortBy (compare `on` name) $ map migration schemaMigrations
  where
    migration (name, query) = Migration {name = name, up = fromQuery query}

createStore :: FilePath -> Int -> Bool -> IO SQLiteStore
createStore dbFilePath poolSize = createSQLiteStore dbFilePath poolSize migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

checkConstraint :: StoreError -> IO (Either StoreError a) -> IO (Either StoreError a)
checkConstraint err action = action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | DB.sqlError e == DB.ErrorConstraint = err
  | otherwise = SEInternalError $ show e

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid()"

type StoreMonad m = (MonadUnliftIO m, MonadError StoreError m)

createUser :: StoreMonad m => SQLiteStore -> Profile -> Bool -> m User
createUser st Profile {displayName, fullName} activeUser =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
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
      "INSERT INTO contact_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
      (displayName, fullName, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, is_user, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (profileId, displayName, userId, True, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure . Right $ toUser (userId, contactId, activeUser, displayName, fullName)

getUsers :: SQLiteStore -> IO [User]
getUsers st =
  withTransaction st $ \db ->
    map toUser
      <$> DB.query_
        db
        [sql|
          SELECT u.user_id, u.contact_id, u.active_user, u.local_display_name, p.full_name
          FROM users u
          JOIN contacts c ON u.contact_id = c.contact_id
          JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
        |]

toUser :: (UserId, Int64, Bool, ContactName, Text) -> User
toUser (userId, userContactId, activeUser, displayName, fullName) =
  let profile = Profile {displayName, fullName}
   in User {userId, userContactId, localDisplayName = displayName, profile, activeUser}

setActiveUser :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
setActiveUser st userId = do
  liftIO . withTransaction st $ \db -> do
    DB.execute_ db "UPDATE users SET active_user = 0"
    DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?" (Only userId)

createDirectConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> m ()
createDirectConnection st userId agentConnId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    void $ createContactConnection_ db userId agentConnId Nothing 0 currentTs

createContactConnection_ :: DB.Connection -> UserId -> ConnId -> Maybe Int64 -> Int -> UTCTime -> IO Connection
createContactConnection_ db userId = do createConnection_ db userId ConnContact Nothing

createConnection_ :: DB.Connection -> UserId -> ConnType -> Maybe Int64 -> ConnId -> Maybe Int64 -> Int -> UTCTime -> IO Connection
createConnection_ db userId connType entityId acId viaContact connLevel currentTs = do
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_level, via_contact, conn_status, conn_type,
        contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, acId, connLevel, viaContact, ConnNew, connType)
        :. (ent ConnContact, ent ConnMember, ent ConnSndFile, ent ConnRcvFile, ent ConnUserContact, currentTs, currentTs)
    )
  connId <- insertedRowId db
  pure Connection {connId, agentConnId = AgentConnId acId, connType, entityId, viaContact, connLevel, connStatus = ConnNew, createdAt = currentTs}
  where
    ent ct = if connType == ct then entityId else Nothing

createDirectContact :: StoreMonad m => SQLiteStore -> UserId -> Connection -> Profile -> m ()
createDirectContact st userId Connection {connId} profile =
  void $
    liftIOEither . withTransaction st $ \db -> do
      currentTs <- getCurrentTime
      createContact_ db userId connId profile Nothing currentTs

createContact_ :: DB.Connection -> UserId -> Int64 -> Profile -> Maybe Int64 -> UTCTime -> IO (Either StoreError (Text, Int64, Int64))
createContact_ db userId connId Profile {displayName, fullName} viaGroup currentTs =
  withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
      (displayName, fullName, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, via_group, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (profileId, ldn, userId, viaGroup, currentTs, currentTs)
    contactId <- insertedRowId db
    DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, currentTs, connId)
    pure (ldn, contactId, profileId)

getContactGroupNames :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> m [GroupName]
getContactGroupNames st userId Contact {contactId} =
  liftIO . withTransaction st $ \db -> do
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT DISTINCT g.local_display_name
          FROM groups g
          JOIN group_members m ON m.group_id = g.group_id
          WHERE g.user_id = ? AND m.contact_id = ?
        |]
        (userId, contactId)

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> m ()
deleteContact st userId Contact {contactId, localDisplayName} =
  liftIO . withTransaction st $ \db -> do
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
    DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
    DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)

updateUserProfile :: StoreMonad m => SQLiteStore -> User -> Profile -> m ()
updateUserProfile st User {userId, userContactId, localDisplayName, profile = Profile {displayName}} p'@Profile {displayName = newName}
  | displayName == newName =
    liftIO . withTransaction st $ \db ->
      updateContactProfile_ db userId userContactId p'
  | otherwise =
    liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
      currentTs <- getCurrentTime
      DB.execute db "UPDATE users SET local_display_name = ?, updated_at = ? WHERE user_id = ?" (newName, currentTs, userId)
      DB.execute
        db
        "INSERT INTO display_names (local_display_name, ldn_base, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
        (newName, newName, userId, currentTs, currentTs)
      updateContactProfile_' db userId userContactId p' currentTs
      updateContact_ db userId userContactId localDisplayName newName currentTs
      pure $ Right ()

updateContactProfile :: StoreMonad m => SQLiteStore -> UserId -> Contact -> Profile -> m Contact
updateContactProfile st userId c@Contact {contactId, localDisplayName, profile = Profile {displayName}} p'@Profile {displayName = newName}
  | displayName == newName =
    liftIO . withTransaction st $ \db ->
      updateContactProfile_ db userId contactId p' $> (c :: Contact) {profile = p'}
  | otherwise =
    liftIOEither . withTransaction st $ \db ->
      withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateContactProfile_' db userId contactId p' currentTs
        updateContact_ db userId contactId localDisplayName ldn currentTs
        pure $ (c :: Contact) {localDisplayName = ldn, profile = p'}

updateContactProfile_ :: DB.Connection -> UserId -> Int64 -> Profile -> IO ()
updateContactProfile_ db userId contactId profile = do
  currentTs <- getCurrentTime
  updateContactProfile_' db userId contactId profile currentTs

updateContactProfile_' :: DB.Connection -> UserId -> Int64 -> Profile -> UTCTime -> IO ()
updateContactProfile_' db userId contactId Profile {displayName, fullName} updatedAt = do
  DB.executeNamed
    db
    [sql|
      UPDATE contact_profiles
      SET display_name = :display_name,
          full_name = :full_name,
          updated_at = :updated_at
      WHERE contact_profile_id IN (
        SELECT contact_profile_id
        FROM contacts
        WHERE user_id = :user_id
          AND contact_id = :contact_id
      )
    |]
    [ ":display_name" := displayName,
      ":full_name" := fullName,
      ":updated_at" := updatedAt,
      ":user_id" := userId,
      ":contact_id" := contactId
    ]

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

type ContactRow = (Int64, ContactName, Maybe Int64, ContactName, Text, UTCTime)

toContact :: ContactRow :. ConnectionRow -> Contact
toContact ((contactId, localDisplayName, viaGroup, displayName, fullName, createdAt) :. connRow) =
  let profile = Profile {displayName, fullName}
      activeConn = toConnection connRow
   in Contact {contactId, localDisplayName, profile, activeConn, viaGroup, createdAt}

toContactOrError :: ContactRow :. MaybeConnectionRow -> Either StoreError Contact
toContactOrError ((contactId, localDisplayName, viaGroup, displayName, fullName, createdAt) :. connRow) =
  let profile = Profile {displayName, fullName}
   in case toMaybeConnection connRow of
        Just activeConn ->
          Right Contact {contactId, localDisplayName, profile, activeConn, viaGroup, createdAt}
        _ -> Left $ SEContactNotReady localDisplayName

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContactByName :: StoreMonad m => SQLiteStore -> UserId -> ContactName -> m Contact
getContactByName st userId localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    cId <- ExceptT $ getContactIdByName_ db userId localDisplayName
    ExceptT $ getContact_ db userId cId

getUserContacts :: MonadUnliftIO m => SQLiteStore -> User -> m [Contact]
getUserContacts st User {userId} =
  liftIO . withTransaction st $ \db -> do
    contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ?" (Only userId)
    rights <$> mapM (getContact_ db userId) contactIds

createUserContactLink :: StoreMonad m => SQLiteStore -> UserId -> ConnId -> ConnReqContact -> m ()
createUserContactLink st userId agentConnId cReq =
  liftIOEither . checkConstraint SEDuplicateContactLink . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO user_contact_links (user_id, conn_req_contact, created_at, updated_at) VALUES (?,?,?,?)"
      (userId, cReq, currentTs, currentTs)
    userContactLinkId <- insertedRowId db
    Right () <$ createConnection_ db userId ConnUserContact (Just userContactLinkId) agentConnId Nothing 0 currentTs

getUserContactLinkConnections :: StoreMonad m => SQLiteStore -> UserId -> m [Connection]
getUserContactLinkConnections st userId =
  liftIOEither . withTransaction st $ \db ->
    connections
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
          FROM connections c
          JOIN user_contact_links uc ON c.user_contact_link_id = uc.user_contact_link_id
          WHERE c.user_id = :user_id
            AND uc.user_id = :user_id
            AND uc.local_display_name = ''
        |]
        [":user_id" := userId]
  where
    connections [] = Left SEUserContactLinkNotFound
    connections rows = Right $ map toConnection rows

deleteUserContactLink :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
deleteUserContactLink st userId =
  liftIO . withTransaction st $ \db -> do
    DB.execute
      db
      [sql|
        DELETE FROM connections WHERE connection_id IN (
          SELECT connection_id
          FROM connections c
          JOIN user_contact_links uc USING (user_contact_link_id)
          WHERE uc.user_id = ? AND uc.local_display_name = ''
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
            WHERE uc.user_id = :user_id
              AND uc.local_display_name = ''
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
          WHERE uc.user_id = :user_id
            AND uc.local_display_name = ''
        )
      |]
      [":user_id" := userId]
    DB.execute db "DELETE FROM user_contact_links WHERE user_id = ? AND local_display_name = ''" (Only userId)

getUserContactLink :: StoreMonad m => SQLiteStore -> UserId -> m ConnReqContact
getUserContactLink st userId =
  liftIOEither . withTransaction st $ \db ->
    connReq
      <$> DB.query
        db
        [sql|
          SELECT conn_req_contact
          FROM user_contact_links
          WHERE user_id = ?
            AND local_display_name = ''
        |]
        (Only userId)
  where
    connReq [Only cReq] = Right cReq
    connReq _ = Left SEUserContactLinkNotFound

createContactRequest :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> InvitationId -> Profile -> m UserContactRequest
createContactRequest st userId userContactId invId Profile {displayName, fullName} =
  liftIOEither . withTransaction st $ \db ->
    join <$> withLocalDisplayName db userId displayName (createContactRequest' db)
  where
    createContactRequest' db ldn = do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT INTO contact_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
        (displayName, fullName, currentTs, currentTs)
      profileId <- insertedRowId db
      DB.execute
        db
        [sql|
          INSERT INTO contact_requests
            (user_contact_link_id, agent_invitation_id, contact_profile_id, local_display_name, user_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?)
        |]
        (userContactId, invId, profileId, ldn, userId, currentTs, currentTs)
      contactRequestId <- insertedRowId db
      getContactRequest_ db userId contactRequestId

getContactRequest :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> m UserContactRequest
getContactRequest st userId contactRequestId =
  liftIOEither . withTransaction st $ \db ->
    getContactRequest_ db userId contactRequestId

getContactRequest_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError UserContactRequest)
getContactRequest_ db userId contactRequestId =
  firstRow toContactRequest (SEContactRequestNotFound contactRequestId) $
    DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, cr.created_at
        FROM contact_requests cr
        JOIN connections c USING (user_contact_link_id)
        JOIN contact_profiles p USING (contact_profile_id)
        WHERE cr.user_id = ?
          AND cr.contact_request_id = ?
      |]
      (userId, contactRequestId)

type ContactRequestRow = (Int64, ContactName, AgentInvId, Int64, AgentConnId, Int64, ContactName, Text, UTCTime)

toContactRequest :: ContactRequestRow -> UserContactRequest
toContactRequest (contactRequestId, localDisplayName, agentInvitationId, userContactLinkId, agentContactConnId, profileId, displayName, fullName, createdAt) = do
  let profile = Profile {displayName, fullName}
   in UserContactRequest {contactRequestId, agentInvitationId, userContactLinkId, agentContactConnId, localDisplayName, profileId, profile, createdAt}

getContactRequestIdByName :: StoreMonad m => SQLiteStore -> UserId -> ContactName -> m Int64
getContactRequestIdByName st userId cName =
  liftIOEither . withTransaction st $ \db ->
    firstRow fromOnly (SEContactRequestNotFoundByName cName) $
      DB.query db "SELECT contact_request_id FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, cName)

deleteContactRequest :: MonadUnliftIO m => SQLiteStore -> UserId -> Int64 -> m ()
deleteContactRequest st userId contactRequestId =
  liftIO . withTransaction st $ \db -> do
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

createAcceptedContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> ContactName -> Int64 -> Profile -> m Contact
createAcceptedContact st userId agentConnId localDisplayName profileId profile =
  liftIO . withTransaction st $ \db -> do
    DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO contacts (user_id, local_display_name, contact_profile_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (userId, localDisplayName, profileId, currentTs, currentTs)
    contactId <- insertedRowId db
    activeConn <- createConnection_ db userId ConnContact (Just contactId) agentConnId Nothing 0 currentTs
    pure $ Contact {contactId, localDisplayName, profile, activeConn, viaGroup = Nothing, createdAt = currentTs}

getLiveSndFileTransfers :: MonadUnliftIO m => SQLiteStore -> User -> m [SndFileTransfer]
getLiveSndFileTransfers st User {userId} =
  liftIO . withTransaction st $ \db -> do
    fileIds :: [Int64] <-
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT DISTINCT f.file_id
            FROM files f
            JOIN snd_files s
            WHERE f.user_id = ? AND s.file_status IN (?, ?, ?)
          |]
          (userId, FSNew, FSAccepted, FSConnected)
    concatMap (filter liveTransfer) . rights <$> mapM (getSndFileTransfers_ db userId) fileIds
  where
    liveTransfer :: SndFileTransfer -> Bool
    liveTransfer SndFileTransfer {fileStatus} = fileStatus `elem` [FSNew, FSAccepted, FSConnected]

getLiveRcvFileTransfers :: MonadUnliftIO m => SQLiteStore -> User -> m [RcvFileTransfer]
getLiveRcvFileTransfers st User {userId} =
  liftIO . withTransaction st $ \db -> do
    fileIds :: [Int64] <-
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT f.file_id
            FROM files f
            JOIN rcv_files r
            WHERE f.user_id = ? AND r.file_status IN (?, ?)
          |]
          (userId, FSAccepted, FSConnected)
    rights <$> mapM (getRcvFileTransfer_ db userId) fileIds

getPendingSndChunks :: MonadUnliftIO m => SQLiteStore -> Int64 -> Int64 -> m [Integer]
getPendingSndChunks st fileId connId =
  liftIO . withTransaction st $ \db ->
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

getPendingConnections :: MonadUnliftIO m => SQLiteStore -> User -> m [Connection]
getPendingConnections st User {userId} =
  liftIO . withTransaction st $ \db ->
    map toConnection
      <$> DB.queryNamed
        db
        [sql|
          SELECT connection_id, agent_conn_id, conn_level, via_contact,
            conn_status, conn_type, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at
          FROM connections
          WHERE user_id = :user_id
            AND conn_type = :conn_type
            AND contact_id IS NULL
        |]
        [":user_id" := userId, ":conn_type" := ConnContact]

getContactConnections :: StoreMonad m => SQLiteStore -> UserId -> Contact -> m [Connection]
getContactConnections st userId Contact {contactId} =
  liftIOEither . withTransaction st $ \db ->
    connections
      <$> DB.query
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
          FROM connections c
          JOIN contacts ct ON ct.contact_id = c.contact_id
          WHERE c.user_id = ? AND ct.user_id = ? AND ct.contact_id = ?
        |]
        (userId, userId, contactId)
  where
    connections [] = Left $ SEContactNotFound contactId
    connections rows = Right $ map toConnection rows

type ConnectionRow = (Int64, ConnId, Int, Maybe Int64, ConnStatus, ConnType, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, UTCTime)

type MaybeConnectionRow = (Maybe Int64, Maybe ConnId, Maybe Int, Maybe Int64, Maybe ConnStatus, Maybe ConnType, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime)

toConnection :: ConnectionRow -> Connection
toConnection (connId, acId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId, createdAt) =
  let entityId = entityId_ connType
   in Connection {connId, agentConnId = AgentConnId acId, connLevel, viaContact, connStatus, connType, entityId, createdAt}
  where
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId
    entityId_ ConnRcvFile = rcvFileId
    entityId_ ConnSndFile = sndFileId
    entityId_ ConnUserContact = userContactLinkId

toMaybeConnection :: MaybeConnectionRow -> Maybe Connection
toMaybeConnection (Just connId, Just agentConnId, Just connLevel, viaContact, Just connStatus, Just connType, contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId, Just createdAt) =
  Just $ toConnection (connId, agentConnId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId, createdAt)
toMaybeConnection _ = Nothing

getMatchingContacts :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> m [Contact]
getMatchingContacts st userId Contact {contactId, profile = Profile {displayName, fullName}} =
  liftIO . withTransaction st $ \db -> do
    contactIds <-
      map fromOnly
        <$> DB.queryNamed
          db
          [sql|
            SELECT ct.contact_id
            FROM contacts ct
            JOIN contact_profiles p ON ct.contact_profile_id = p.contact_profile_id
            WHERE ct.user_id = :user_id AND ct.contact_id != :contact_id
              AND p.display_name = :display_name AND p.full_name = :full_name
          |]
          [ ":user_id" := userId,
            ":contact_id" := contactId,
            ":display_name" := displayName,
            ":full_name" := fullName
          ]
    rights <$> mapM (getContact_ db userId) contactIds

createSentProbe :: StoreMonad m => SQLiteStore -> TVar ChaChaDRG -> UserId -> Contact -> m (Probe, Int64)
createSentProbe st gVar userId _to@Contact {contactId} =
  liftIOEither . withTransaction st $ \db ->
    createWithRandomBytes 32 gVar $ \probe -> do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT INTO sent_probes (contact_id, probe, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
        (contactId, probe, userId, currentTs, currentTs)
      (Probe probe,) <$> insertedRowId db

createSentProbeHash :: MonadUnliftIO m => SQLiteStore -> UserId -> Int64 -> Contact -> m ()
createSentProbeHash st userId probeId _to@Contact {contactId} =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO sent_probe_hashes (sent_probe_id, contact_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (probeId, contactId, userId, currentTs, currentTs)

matchReceivedProbe :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> Probe -> m (Maybe Contact)
matchReceivedProbe st userId _from@Contact {contactId} (Probe probe) =
  liftIO . withTransaction st $ \db -> do
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
      cId : _ -> eitherToMaybe <$> getContact_ db userId cId

matchReceivedProbeHash :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> ProbeHash -> m (Maybe (Contact, Probe))
matchReceivedProbeHash st userId _from@Contact {contactId} (ProbeHash probeHash) =
  liftIO . withTransaction st $ \db -> do
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
          <$> getContact_ db userId cId

matchSentProbe :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> Probe -> m (Maybe Contact)
matchSentProbe st userId _from@Contact {contactId} (Probe probe) =
  liftIO . withTransaction st $ \db -> do
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
      cId : _ -> eitherToMaybe <$> getContact_ db userId cId

mergeContactRecords :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> Contact -> m ()
mergeContactRecords st userId Contact {contactId = toContactId} Contact {contactId = fromContactId, localDisplayName} =
  liftIO . withTransaction st $ \db -> do
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
    DB.execute db "DELETE FROM contacts WHERE contact_id = ? AND user_id = ?" (fromContactId, userId)
    DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (localDisplayName, userId)

getConnectionEntity :: StoreMonad m => SQLiteStore -> User -> ConnId -> m ConnectionEntity
getConnectionEntity st User {userId, userContactId} agentConnId =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    c@Connection {connType, entityId} <- getConnection_ db
    case entityId of
      Nothing ->
        if connType == ConnContact
          then pure $ RcvDirectMsgConnection c Nothing
          else throwError $ SEInternalError $ "connection " <> show connType <> " without entity"
      Just entId ->
        case connType of
          ConnMember -> uncurry (RcvGroupMsgConnection c) <$> getGroupAndMember_ db entId c
          ConnContact -> RcvDirectMsgConnection c . Just <$> getContactRec_ db entId c
          ConnSndFile -> SndFileConnection c <$> getConnSndFileTransfer_ db entId c
          ConnRcvFile -> RcvFileConnection c <$> ExceptT (getRcvFileTransfer_ db userId entId)
          ConnUserContact -> UserContactConnection c <$> getUserContact_ db entId
  where
    getConnection_ :: DB.Connection -> ExceptT StoreError IO Connection
    getConnection_ db = ExceptT $ do
      connection
        <$> DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact,
              conn_status, conn_type, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?
          |]
          (userId, agentConnId)
    connection :: [ConnectionRow] -> Either StoreError Connection
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left . SEConnectionNotFound $ AgentConnId agentConnId
    getContactRec_ :: DB.Connection -> Int64 -> Connection -> ExceptT StoreError IO Contact
    getContactRec_ db contactId c = ExceptT $ do
      toContact' contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.local_display_name, p.display_name, p.full_name, c.via_group, c.created_at
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact' :: Int64 -> Connection -> [(ContactName, Text, Text, Maybe Int64, UTCTime)] -> Either StoreError Contact
    toContact' contactId activeConn [(localDisplayName, displayName, fullName, viaGroup, createdAt)] =
      let profile = Profile {displayName, fullName}
       in Right $ Contact {contactId, localDisplayName, profile, activeConn, viaGroup, createdAt}
    toContact' _ _ _ = Left $ SEInternalError "referenced contact not found"
    getGroupAndMember_ :: DB.Connection -> Int64 -> Connection -> ExceptT StoreError IO (GroupInfo, GroupMember)
    getGroupAndMember_ db groupMemberId c = ExceptT $ do
      firstRow (toGroupAndMember c) (SEInternalError "referenced group member not found") $
        DB.query
          db
          [sql|
            SELECT
              -- GroupInfo
              g.group_id, g.local_display_name, gp.display_name, gp.full_name, g.created_at,
              -- GroupInfo {membership}
              mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
              mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id,
              -- GroupInfo {membership = GroupMember {memberProfile}}
              pu.display_name, pu.full_name,
              -- from GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
              m.invited_by, m.local_display_name, m.contact_id, p.display_name, p.full_name
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            JOIN groups g ON g.group_id = m.group_id
            JOIN group_profiles gp USING (group_profile_id)
            JOIN group_members mu ON g.group_id = mu.group_id
            JOIN contact_profiles pu ON pu.contact_profile_id = mu.contact_profile_id
            WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
          |]
          (groupMemberId, userId, userContactId)
    toGroupAndMember :: Connection -> GroupInfoRow :. GroupMemberRow -> (GroupInfo, GroupMember)
    toGroupAndMember c (groupInfoRow :. memberRow) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = Just c})
    getConnSndFileTransfer_ :: DB.Connection -> Int64 -> Connection -> ExceptT StoreError IO SndFileTransfer
    getConnSndFileTransfer_ db fileId Connection {connId} =
      ExceptT $
        sndFileTransfer_ fileId connId
          <$> DB.query
            db
            [sql|
              SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, cs.local_display_name, m.local_display_name
              FROM snd_files s
              JOIN files f USING (file_id)
              LEFT JOIN contacts cs USING (contact_id)
              LEFT JOIN group_members m USING (group_member_id)    
              WHERE f.user_id = ? AND f.file_id = ? AND s.connection_id = ?
            |]
            (userId, fileId, connId)
    sndFileTransfer_ :: Int64 -> Int64 -> [(FileStatus, String, Integer, Integer, FilePath, Maybe ContactName, Maybe ContactName)] -> Either StoreError SndFileTransfer
    sndFileTransfer_ fileId connId [(fileStatus, fileName, fileSize, chunkSize, filePath, contactName_, memberName_)] =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, recipientDisplayName, connId, agentConnId = AgentConnId agentConnId}
        Nothing -> Left $ SESndFileInvalid fileId
    sndFileTransfer_ fileId _ _ = Left $ SESndFileNotFound fileId
    getUserContact_ :: DB.Connection -> Int64 -> ExceptT StoreError IO UserContact
    getUserContact_ db userContactLinkId = ExceptT $ do
      userContact_
        <$> DB.query
          db
          [sql|
            SELECT conn_req_contact
            FROM user_contact_links
            WHERE user_id = ? AND user_contact_link_id = ?
          |]
          (userId, userContactLinkId)
      where
        userContact_ :: [Only ConnReqContact] -> Either StoreError UserContact
        userContact_ [Only cReq] = Right UserContact {userContactLinkId, connReqContact = cReq}
        userContact_ _ = Left SEUserContactLinkNotFound

updateConnectionStatus :: MonadUnliftIO m => SQLiteStore -> Connection -> ConnStatus -> m ()
updateConnectionStatus st Connection {connId} connStatus =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ? WHERE connection_id = ?" (connStatus, currentTs, connId)

-- | creates completely new group with a single member - the current user
createNewGroup :: StoreMonad m => SQLiteStore -> TVar ChaChaDRG -> User -> GroupProfile -> m GroupInfo
createNewGroup st gVar user groupProfile =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
    let GroupProfile {displayName, fullName} = groupProfile
        uId = userId user
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO display_names (local_display_name, ldn_base, user_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (displayName, displayName, uId, currentTs, currentTs)
    DB.execute
      db
      "INSERT INTO group_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
      (displayName, fullName, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO groups (local_display_name, user_id, group_profile_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (displayName, uId, profileId, currentTs, currentTs)
    groupId <- insertedRowId db
    memberId <- randomBytes gVar 12
    membership <- createContactMember_ db user groupId user (MemberIdRole (MemberId memberId) GROwner) GCUserMember GSMemCreator IBUser currentTs
    pure $ Right GroupInfo {groupId, localDisplayName = displayName, groupProfile, membership, createdAt = currentTs}

-- | creates a new group record for the group the current user was invited to, or returns an existing one
createGroupInvitation ::
  StoreMonad m => SQLiteStore -> User -> Contact -> GroupInvitation -> m GroupInfo
createGroupInvitation st user@User {userId} contact@Contact {contactId} GroupInvitation {fromMember, invitedMember, connRequest, groupProfile} =
  liftIOEither . withTransaction st $ \db -> do
    getInvitationGroupId_ db >>= \case
      Nothing -> createGroupInvitation_ db
      -- TODO treat the case that the invitation details could've changed
      Just gId -> getGroupInfo_ db user gId
  where
    getInvitationGroupId_ :: DB.Connection -> IO (Maybe Int64)
    getInvitationGroupId_ db =
      listToMaybe . map fromOnly
        <$> DB.query db "SELECT group_id FROM groups WHERE inv_queue_info = ? AND user_id = ? LIMIT 1" (connRequest, userId)
    createGroupInvitation_ :: DB.Connection -> IO (Either StoreError GroupInfo)
    createGroupInvitation_ db = do
      let GroupProfile {displayName, fullName} = groupProfile
      withLocalDisplayName db userId displayName $ \localDisplayName -> do
        currentTs <- getCurrentTime
        DB.execute
          db
          "INSERT INTO group_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
          (displayName, fullName, currentTs, currentTs)
        profileId <- insertedRowId db
        DB.execute
          db
          "INSERT INTO groups (group_profile_id, local_display_name, inv_queue_info, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
          (profileId, localDisplayName, connRequest, userId, currentTs, currentTs)
        groupId <- insertedRowId db
        _ <- createContactMember_ db user groupId contact fromMember GCHostMember GSMemInvited IBUnknown currentTs
        membership <- createContactMember_ db user groupId user invitedMember GCUserMember GSMemInvited (IBContact contactId) currentTs
        pure $ GroupInfo {groupId, localDisplayName, groupProfile, membership, createdAt = currentTs}

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroupByName :: StoreMonad m => SQLiteStore -> User -> GroupName -> m Group
getGroupByName st user gName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    groupId <- ExceptT $ getGroupIdByName_ db user gName
    ExceptT $ getGroup_ db user groupId

getGroup :: StoreMonad m => SQLiteStore -> User -> Int64 -> m Group
getGroup st user groupId =
  liftIOEither . withTransaction st $ \db -> getGroup_ db user groupId

getGroup_ :: DB.Connection -> User -> Int64 -> IO (Either StoreError Group)
getGroup_ db user groupId = runExceptT $ do
  gInfo <- ExceptT $ getGroupInfo_ db user groupId
  members <- liftIO $ getGroupMembers_ db user gInfo
  pure $ Group gInfo members

deleteGroup :: MonadUnliftIO m => SQLiteStore -> User -> Group -> m ()
deleteGroup st User {userId} (Group GroupInfo {groupId, localDisplayName} members) =
  liftIO . withTransaction st $ \db -> do
    forM_ members $ \m -> DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId m)
    DB.execute db "DELETE FROM group_members WHERE user_id = ? AND group_id = ?" (userId, groupId)
    DB.execute db "DELETE FROM groups WHERE user_id = ? AND group_id = ?" (userId, groupId)
    -- TODO ? delete group profile
    DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)

getUserGroups :: MonadUnliftIO m => SQLiteStore -> User -> m [Group]
getUserGroups st user@User {userId} =
  liftIO . withTransaction st $ \db -> do
    groupIds <- map fromOnly <$> DB.query db "SELECT group_id FROM groups WHERE user_id = ?" (Only userId)
    rights <$> mapM (getGroup_ db user) groupIds

getUserGroupDetails :: MonadUnliftIO m => SQLiteStore -> User -> m [GroupInfo]
getUserGroupDetails st User {userId, userContactId} =
  liftIO . withTransaction st $ \db ->
    map (toGroupInfo userContactId)
      <$> DB.query
        db
        [sql|
          SELECT g.group_id, g.local_display_name, gp.display_name, gp.full_name, g.created_at,
            m.group_member_id, g.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
            m.invited_by, m.local_display_name, m.contact_id, mp.display_name, mp.full_name
          FROM groups g
          JOIN group_profiles gp USING (group_profile_id)
          JOIN group_members m USING (group_id)
          JOIN contact_profiles mp USING (contact_profile_id)
          WHERE g.user_id = ? AND m.contact_id = ?
        |]
        (userId, userContactId)

getGroupInfoByName :: StoreMonad m => SQLiteStore -> User -> GroupName -> m GroupInfo
getGroupInfoByName st user gName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    gId <- ExceptT $ getGroupIdByName_ db user gName
    ExceptT $ getGroupInfo_ db user gId

type GroupInfoRow = (Int64, GroupName, GroupName, Text, UTCTime) :. GroupMemberRow

toGroupInfo :: Int64 -> GroupInfoRow -> GroupInfo
toGroupInfo userContactId ((groupId, localDisplayName, displayName, fullName, createdAt) :. userMemberRow) =
  let membership = toGroupMember userContactId userMemberRow
   in GroupInfo {groupId, localDisplayName, groupProfile = GroupProfile {displayName, fullName}, membership, createdAt}

getGroupMembers :: MonadUnliftIO m => SQLiteStore -> User -> GroupInfo -> m [GroupMember]
getGroupMembers st user gInfo = liftIO . withTransaction st $ \db -> getGroupMembers_ db user gInfo

getGroupMembers_ :: DB.Connection -> User -> GroupInfo -> IO [GroupMember]
getGroupMembers_ db User {userId, userContactId} GroupInfo {groupId} = do
  map toContactMember
    <$> DB.query
      db
      [sql|
        SELECT
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
          m.invited_by, m.local_display_name, m.contact_id, p.display_name, p.full_name,
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
          c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
        FROM group_members m
        JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
        LEFT JOIN connections c ON c.connection_id = (
          SELECT max(cc.connection_id)
          FROM connections cc
          where cc.group_member_id = m.group_member_id
        )
        WHERE m.group_id = ? AND m.user_id = ? AND (m.contact_id IS NULL OR m.contact_id != ?)
      |]
      (groupId, userId, userContactId)
  where
    toContactMember :: (GroupMemberRow :. MaybeConnectionRow) -> GroupMember
    toContactMember (memberRow :. connRow) =
      (toGroupMember userContactId memberRow) {activeConn = toMaybeConnection connRow}

-- TODO no need to load all members to find the member who invited the used,
-- instead of findFromContact there could be a query
getGroupInvitation :: StoreMonad m => SQLiteStore -> User -> GroupName -> m ReceivedGroupInvitation
getGroupInvitation st user localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    cReq <- getConnRec_ db user
    groupId <- ExceptT $ getGroupIdByName_ db user localDisplayName
    Group groupInfo@GroupInfo {membership} members <- ExceptT $ getGroup_ db user groupId
    when (memberStatus membership /= GSMemInvited) $ throwError SEGroupAlreadyJoined
    case (cReq, findFromContact (invitedBy membership) members) of
      (Just connRequest, Just fromMember) ->
        pure ReceivedGroupInvitation {fromMember, connRequest, groupInfo}
      _ -> throwError SEGroupInvitationNotFound
  where
    getConnRec_ :: DB.Connection -> User -> ExceptT StoreError IO (Maybe ConnReqInvitation)
    getConnRec_ db User {userId} = ExceptT $ do
      firstRow fromOnly (SEGroupNotFoundByName localDisplayName) $
        DB.query db "SELECT g.inv_queue_info FROM groups g WHERE g.local_display_name = ? AND g.user_id = ?" (localDisplayName, userId)
    findFromContact :: InvitedBy -> [GroupMember] -> Maybe GroupMember
    findFromContact (IBContact contactId) = find ((== Just contactId) . memberContactId)
    findFromContact _ = const Nothing

type GroupMemberRow = (Int64, Int64, MemberId, GroupMemberRole, GroupMemberCategory, GroupMemberStatus, Maybe Int64, ContactName, Maybe Int64, ContactName, Text)

type MaybeGroupMemberRow = (Maybe Int64, Maybe Int64, Maybe MemberId, Maybe GroupMemberRole, Maybe GroupMemberCategory, Maybe GroupMemberStatus, Maybe Int64, Maybe ContactName, Maybe Int64, Maybe ContactName, Maybe Text)

toGroupMember :: Int64 -> GroupMemberRow -> GroupMember
toGroupMember userContactId (groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, localDisplayName, memberContactId, displayName, fullName) =
  let memberProfile = Profile {displayName, fullName}
      invitedBy = toInvitedBy userContactId invitedById
      activeConn = Nothing
   in GroupMember {..}

toMaybeGroupMember :: Int64 -> MaybeGroupMemberRow -> Maybe GroupMember
toMaybeGroupMember userContactId (Just groupMemberId, Just groupId, Just memberId, Just memberRole, Just memberCategory, Just memberStatus, invitedById, Just localDisplayName, memberContactId, Just displayName, Just fullName) =
  Just $ toGroupMember userContactId (groupMemberId, groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, localDisplayName, memberContactId, displayName, fullName)
toMaybeGroupMember _ _ = Nothing

createContactMember :: StoreMonad m => SQLiteStore -> TVar ChaChaDRG -> User -> Int64 -> Contact -> GroupMemberRole -> ConnId -> ConnReqInvitation -> m GroupMember
createContactMember st gVar user groupId contact memberRole agentConnId connRequest =
  liftIOEither . withTransaction st $ \db ->
    createWithRandomId gVar $ \memId -> do
      currentTs <- getCurrentTime
      member@GroupMember {groupMemberId} <- createContactMemberInv_ db user groupId contact (MemberIdRole (MemberId memId) memberRole) GCInviteeMember GSMemInvited IBUser (Just connRequest) currentTs
      void $ createMemberConnection_ db (userId user) groupMemberId agentConnId Nothing 0 currentTs
      pure member

getMemberInvitation :: StoreMonad m => SQLiteStore -> User -> Int64 -> m (Maybe ConnReqInvitation)
getMemberInvitation st User {userId} groupMemberId =
  liftIO . withTransaction st $ \db ->
    join . listToMaybe . map fromOnly
      <$> DB.query db "SELECT sent_inv_queue_info FROM group_members WHERE group_member_id = ? AND user_id = ?" (groupMemberId, userId)

createMemberConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> GroupMember -> ConnId -> m ()
createMemberConnection st userId GroupMember {groupMemberId} agentConnId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    void $ createMemberConnection_ db userId groupMemberId agentConnId Nothing 0 currentTs

updateGroupMemberStatus :: MonadUnliftIO m => SQLiteStore -> UserId -> GroupMember -> GroupMemberStatus -> m ()
updateGroupMemberStatus st userId GroupMember {groupMemberId} memStatus =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.executeNamed
      db
      [sql|
        UPDATE group_members
        SET member_status = :member_status, updated_at = :updated_at
        WHERE user_id = :user_id AND group_member_id = :group_member_id
      |]
      [ ":user_id" := userId,
        ":group_member_id" := groupMemberId,
        ":member_status" := memStatus,
        ":updated_at" := currentTs
      ]

-- | add new member with profile
createNewGroupMember :: StoreMonad m => SQLiteStore -> User -> GroupInfo -> MemberInfo -> GroupMemberCategory -> GroupMemberStatus -> m GroupMember
createNewGroupMember st user@User {userId} gInfo memInfo@(MemberInfo _ _ Profile {displayName, fullName}) memCategory memStatus =
  liftIOEither . withTransaction st $ \db ->
    withLocalDisplayName db userId displayName $ \localDisplayName -> do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT INTO contact_profiles (display_name, full_name, created_at, updated_at) VALUES (?,?,?,?)"
        (displayName, fullName, currentTs, currentTs)
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
      createNewMember_ db user gInfo newMember currentTs

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
      memProfileId
    }
  createdAt = do
    let invitedById = fromInvitedBy userContactId invitedBy
        activeConn = Nothing
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          (group_id, member_id, member_role, member_category, member_status,
           invited_by, user_id, local_display_name, contact_profile_id, contact_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      (groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, userId, localDisplayName, memProfileId, memberContactId, createdAt, createdAt)
    groupMemberId <- insertedRowId db
    pure GroupMember {..}

deleteGroupMemberConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> GroupMember -> m ()
deleteGroupMemberConnection st userId m =
  liftIO . withTransaction st $ \db -> deleteGroupMemberConnection_ db userId m

deleteGroupMemberConnection_ :: DB.Connection -> UserId -> GroupMember -> IO ()
deleteGroupMemberConnection_ db userId GroupMember {groupMemberId} =
  DB.execute db "DELETE FROM connections WHERE user_id = ? AND group_member_id = ?" (userId, groupMemberId)

createIntroductions :: MonadUnliftIO m => SQLiteStore -> [GroupMember] -> GroupMember -> m [GroupMemberIntro]
createIntroductions st members toMember = do
  let reMembers = filter (\m -> memberCurrent m && groupMemberId m /= groupMemberId toMember) members
  if null reMembers
    then pure []
    else liftIO . withTransaction st $ \db -> do
      currentTs <- getCurrentTime
      mapM (insertIntro_ db currentTs) reMembers
  where
    insertIntro_ :: DB.Connection -> UTCTime -> GroupMember -> IO GroupMemberIntro
    insertIntro_ db ts reMember = do
      DB.execute
        db
        [sql|
          INSERT INTO group_member_intros
            (re_group_member_id, to_group_member_id, intro_status, created_at, updated_at)
          VALUES (?,?,?,?,?)
        |]
        (groupMemberId reMember, groupMemberId toMember, GMIntroPending, ts, ts)
      introId <- insertedRowId db
      pure GroupMemberIntro {introId, reMember, toMember, introStatus = GMIntroPending, introInvitation = Nothing}

updateIntroStatus :: MonadUnliftIO m => SQLiteStore -> Int64 -> GroupMemberIntroStatus -> m ()
updateIntroStatus st introId introStatus =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.executeNamed
      db
      [sql|
        UPDATE group_member_intros
        SET intro_status = :intro_status, updated_at = :updated_at
        WHERE group_member_intro_id = :intro_id
      |]
      [":intro_status" := introStatus, ":updated_at" := currentTs, ":intro_id" := introId]

saveIntroInvitation :: StoreMonad m => SQLiteStore -> GroupMember -> GroupMember -> IntroInvitation -> m GroupMemberIntro
saveIntroInvitation st reMember toMember introInv = do
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
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
          ":group_queue_info" := groupConnReq introInv,
          ":direct_queue_info" := directConnReq introInv,
          ":updated_at" := currentTs,
          ":intro_id" := introId intro
        ]
    pure intro {introInvitation = Just introInv, introStatus = GMIntroInvReceived}

saveMemberInvitation :: StoreMonad m => SQLiteStore -> GroupMember -> IntroInvitation -> m ()
saveMemberInvitation st GroupMember {groupMemberId} IntroInvitation {groupConnReq, directConnReq} =
  liftIO . withTransaction st $ \db -> do
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
      (groupMemberId reMember, groupMemberId toMember)
  where
    toIntro :: [(Int64, Maybe ConnReqInvitation, Maybe ConnReqInvitation, GroupMemberIntroStatus)] -> Either StoreError GroupMemberIntro
    toIntro [(introId, groupConnReq, directConnReq, introStatus)] =
      let introInvitation = IntroInvitation <$> groupConnReq <*> directConnReq
       in Right GroupMemberIntro {introId, reMember, toMember, introStatus, introInvitation}
    toIntro _ = Left SEIntroNotFound

createIntroReMember :: StoreMonad m => SQLiteStore -> User -> GroupInfo -> GroupMember -> MemberInfo -> ConnId -> ConnId -> m GroupMember
createIntroReMember st user@User {userId} gInfo@GroupInfo {groupId} _host@GroupMember {memberContactId, activeConn} memInfo@(MemberInfo _ _ memberProfile) groupAgentConnId directAgentConnId =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
    currentTs <- liftIO getCurrentTime
    Connection {connId = directConnId} <- liftIO $ createContactConnection_ db userId directAgentConnId memberContactId cLevel currentTs
    (localDisplayName, contactId, memProfileId) <- ExceptT $ createContact_ db userId directConnId memberProfile (Just groupId) currentTs
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
      conn <- createMemberConnection_ db userId (groupMemberId member) groupAgentConnId memberContactId cLevel currentTs
      pure (member :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: StoreMonad m => SQLiteStore -> UserId -> GroupMember -> GroupMember -> ConnId -> ConnId -> m ()
createIntroToMemberContact st userId GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} groupAgentConnId directAgentConnId =
  liftIO . withTransaction st $ \db -> do
    let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
    currentTs <- getCurrentTime
    void $ createMemberConnection_ db userId groupMemberId groupAgentConnId viaContactId cLevel currentTs
    Connection {connId = directConnId} <- createContactConnection_ db userId directAgentConnId viaContactId cLevel currentTs
    contactId <- createMemberContact_ db directConnId currentTs
    updateMember_ db contactId currentTs
  where
    createMemberContact_ :: DB.Connection -> Int64 -> UTCTime -> IO Int64
    createMemberContact_ db connId ts = do
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
    updateMember_ :: DB.Connection -> Int64 -> UTCTime -> IO ()
    updateMember_ db contactId ts =
      DB.executeNamed
        db
        [sql|
          UPDATE group_members
          SET contact_id = :contact_id, updated_at = :updated_at
          WHERE group_member_id = :group_member_id
        |]
        [":contact_id" := contactId, ":updated_at" := ts, ":group_member_id" := groupMemberId]

createMemberConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> Maybe Int64 -> Int -> UTCTime -> IO Connection
createMemberConnection_ db userId groupMemberId = createConnection_ db userId ConnMember (Just groupMemberId)

createContactMember_ :: IsContact a => DB.Connection -> User -> Int64 -> a -> MemberIdRole -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> UTCTime -> IO GroupMember
createContactMember_ db user groupId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy =
  createContactMemberInv_ db user groupId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy Nothing

createContactMemberInv_ :: IsContact a => DB.Connection -> User -> Int64 -> a -> MemberIdRole -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> Maybe ConnReqInvitation -> UTCTime -> IO GroupMember
createContactMemberInv_ db User {userId, userContactId} groupId userOrContact MemberIdRole {memberId, memberRole} memberCategory memberStatus invitedBy connRequest createdAt = do
  insertMember_
  groupMemberId <- insertedRowId db
  let memberProfile = profile' userOrContact
      memberContactId = Just $ contactId' userOrContact
      localDisplayName = localDisplayName' userOrContact
      activeConn = Nothing
  pure GroupMember {..}
  where
    insertMember_ =
      DB.executeNamed
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by,
              user_id, local_display_name, contact_profile_id, contact_id, sent_inv_queue_info, created_at, updated_at)
          VALUES
            (:group_id,:member_id,:member_role,:member_category,:member_status,:invited_by,
             :user_id,:local_display_name,
              (SELECT contact_profile_id FROM contacts WHERE contact_id = :contact_id),
              :contact_id, :sent_inv_queue_info, :created_at, :updated_at)
        |]
        [ ":group_id" := groupId,
          ":member_id" := memberId,
          ":member_role" := memberRole,
          ":member_category" := memberCategory,
          ":member_status" := memberStatus,
          ":invited_by" := fromInvitedBy userContactId invitedBy,
          ":user_id" := userId,
          ":local_display_name" := localDisplayName' userOrContact,
          ":contact_id" := contactId' userOrContact,
          ":sent_inv_queue_info" := connRequest,
          ":created_at" := createdAt,
          ":updated_at" := createdAt
        ]

getViaGroupMember :: MonadUnliftIO m => SQLiteStore -> User -> Contact -> m (Maybe (GroupInfo, GroupMember))
getViaGroupMember st User {userId, userContactId} Contact {contactId} =
  liftIO . withTransaction st $ \db ->
    toGroupAndMember
      <$> DB.query
        db
        [sql|
          SELECT
            -- GroupInfo
            g.group_id, g.local_display_name, gp.display_name, gp.full_name, g.created_at,
            -- GroupInfo {membership}
            mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
            mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id,
            -- GroupInfo {membership = GroupMember {memberProfile}}
            pu.display_name, pu.full_name,
            -- via GroupMember
            m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category, m.member_status,
            m.invited_by, m.local_display_name, m.contact_id, p.display_name, p.full_name,
            c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
          FROM group_members m
          JOIN contacts ct ON ct.contact_id = m.contact_id
          JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
          JOIN groups g ON g.group_id = m.group_id AND g.group_id = ct.via_group
          JOIN group_profiles gp USING (group_profile_id)
          JOIN group_members mu ON g.group_id = mu.group_id
          JOIN contact_profiles pu ON pu.contact_profile_id = mu.contact_profile_id
          LEFT JOIN connections c ON c.connection_id = (
            SELECT max(cc.connection_id)
            FROM connections cc
            where cc.group_member_id = m.group_member_id
          )
          WHERE ct.user_id = ? AND ct.contact_id = ? AND mu.contact_id = ?
        |]
        (userId, contactId, userContactId)
  where
    toGroupAndMember :: [GroupInfoRow :. GroupMemberRow :. MaybeConnectionRow] -> Maybe (GroupInfo, GroupMember)
    toGroupAndMember [groupInfoRow :. memberRow :. connRow] =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in Just (groupInfo, (member :: GroupMember) {activeConn = toMaybeConnection connRow})
    toGroupAndMember _ = Nothing

getViaGroupContact :: MonadUnliftIO m => SQLiteStore -> User -> GroupMember -> m (Maybe Contact)
getViaGroupContact st User {userId} GroupMember {groupMemberId} =
  liftIO . withTransaction st $ \db ->
    toContact'
      <$> DB.query
        db
        [sql|
          SELECT
            ct.contact_id, ct.local_display_name, p.display_name, p.full_name, ct.via_group, ct.created_at,
            c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
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
    toContact' :: [(Int64, ContactName, Text, Text, Maybe Int64, UTCTime) :. ConnectionRow] -> Maybe Contact
    toContact' [(contactId, localDisplayName, displayName, fullName, viaGroup, createdAt) :. connRow] =
      let profile = Profile {displayName, fullName}
          activeConn = toConnection connRow
       in Just Contact {contactId, localDisplayName, profile, activeConn, viaGroup, createdAt}
    toContact' _ = Nothing

createSndFileTransfer :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> FilePath -> FileInvitation -> ConnId -> Integer -> m SndFileTransfer
createSndFileTransfer st userId Contact {contactId, localDisplayName = recipientDisplayName} filePath FileInvitation {fileName, fileSize} acId chunkSize =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO files (user_id, contact_id, file_name, file_path, file_size, chunk_size, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (userId, contactId, fileName, filePath, fileSize, chunkSize, currentTs, currentTs)
    fileId <- insertedRowId db
    Connection {connId} <- createSndFileConnection_ db userId fileId acId
    let fileStatus = FSNew
    DB.execute
      db
      "INSERT INTO snd_files (file_id, file_status, connection_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (fileId, fileStatus, connId, currentTs, currentTs)
    pure SndFileTransfer {fileId, fileName, filePath, fileSize, chunkSize, recipientDisplayName, connId, fileStatus, agentConnId = AgentConnId acId}

createSndGroupFileTransfer :: MonadUnliftIO m => SQLiteStore -> UserId -> GroupInfo -> [(GroupMember, ConnId, FileInvitation)] -> FilePath -> Integer -> Integer -> m Int64
createSndGroupFileTransfer st userId GroupInfo {groupId} ms filePath fileSize chunkSize =
  liftIO . withTransaction st $ \db -> do
    let fileName = takeFileName filePath
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO files (user_id, group_id, file_name, file_path, file_size, chunk_size, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (userId, groupId, fileName, filePath, fileSize, chunkSize, currentTs, currentTs)
    fileId <- insertedRowId db
    forM_ ms $ \(GroupMember {groupMemberId}, agentConnId, _) -> do
      Connection {connId} <- createSndFileConnection_ db userId fileId agentConnId
      DB.execute
        db
        "INSERT INTO snd_files (file_id, file_status, connection_id, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
        (fileId, FSNew, connId, groupMemberId, currentTs, currentTs)
    pure fileId

createSndFileConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> IO Connection
createSndFileConnection_ db userId fileId agentConnId = do
  currentTs <- getCurrentTime
  createConnection_ db userId ConnSndFile (Just fileId) agentConnId Nothing 0 currentTs

updateSndFileStatus :: MonadUnliftIO m => SQLiteStore -> SndFileTransfer -> FileStatus -> m ()
updateSndFileStatus st SndFileTransfer {fileId, connId} status =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute db "UPDATE snd_files SET file_status = ?, updated_at = ? WHERE file_id = ? AND connection_id = ?" (status, currentTs, fileId, connId)

createSndFileChunk :: MonadUnliftIO m => SQLiteStore -> SndFileTransfer -> m (Maybe Integer)
createSndFileChunk st SndFileTransfer {fileId, connId, fileSize, chunkSize} =
  liftIO . withTransaction st $ \db -> do
    chunkNo <- getLastChunkNo db
    insertChunk db chunkNo
    pure chunkNo
  where
    getLastChunkNo db = do
      ns <- DB.query db "SELECT chunk_number FROM snd_file_chunks WHERE file_id = ? AND connection_id = ? AND chunk_sent = 1 ORDER BY chunk_number DESC LIMIT 1" (fileId, connId)
      pure $ case map fromOnly ns of
        [] -> Just 1
        n : _ -> if n * chunkSize >= fileSize then Nothing else Just (n + 1)
    insertChunk db = \case
      Just chunkNo -> do
        currentTs <- getCurrentTime
        DB.execute
          db
          "INSERT OR REPLACE INTO snd_file_chunks (file_id, connection_id, chunk_number, created_at, updated_at) VALUES (?,?,?,?,?)"
          (fileId, connId, chunkNo, currentTs, currentTs)
      Nothing -> pure ()

updateSndFileChunkMsg :: MonadUnliftIO m => SQLiteStore -> SndFileTransfer -> Integer -> AgentMsgId -> m ()
updateSndFileChunkMsg st SndFileTransfer {fileId, connId} chunkNo msgId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        UPDATE snd_file_chunks
        SET chunk_agent_msg_id = ?, updated_at = ?
        WHERE file_id = ? AND connection_id = ? AND chunk_number = ?
      |]
      (msgId, currentTs, fileId, connId, chunkNo)

updateSndFileChunkSent :: MonadUnliftIO m => SQLiteStore -> SndFileTransfer -> AgentMsgId -> m ()
updateSndFileChunkSent st SndFileTransfer {fileId, connId} msgId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        UPDATE snd_file_chunks
        SET chunk_sent = 1, updated_at = ?
        WHERE file_id = ? AND connection_id = ? AND chunk_agent_msg_id = ?
      |]
      (currentTs, fileId, connId, msgId)

deleteSndFileChunks :: MonadUnliftIO m => SQLiteStore -> SndFileTransfer -> m ()
deleteSndFileChunks st SndFileTransfer {fileId, connId} =
  liftIO . withTransaction st $ \db ->
    DB.execute db "DELETE FROM snd_file_chunks WHERE file_id = ? AND connection_id = ?" (fileId, connId)

createRcvFileTransfer :: MonadUnliftIO m => SQLiteStore -> UserId -> Contact -> FileInvitation -> Integer -> m RcvFileTransfer
createRcvFileTransfer st userId Contact {contactId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq} chunkSize =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO files (user_id, contact_id, file_name, file_size, chunk_size, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
      (userId, contactId, fileName, fileSize, chunkSize, currentTs, currentTs)
    fileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO rcv_files (file_id, file_status, file_queue_info, created_at, updated_at) VALUES (?,?,?,?,?)"
      (fileId, FSNew, fileConnReq, currentTs, currentTs)
    pure RcvFileTransfer {fileId, fileInvitation = f, fileStatus = RFSNew, senderDisplayName = c, chunkSize}

createRcvGroupFileTransfer :: MonadUnliftIO m => SQLiteStore -> UserId -> GroupMember -> FileInvitation -> Integer -> m RcvFileTransfer
createRcvGroupFileTransfer st userId GroupMember {groupId, groupMemberId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq} chunkSize =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT INTO files (user_id, group_id, file_name, file_size, chunk_size, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
      (userId, groupId, fileName, fileSize, chunkSize, currentTs, currentTs)
    fileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO rcv_files (file_id, file_status, file_queue_info, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (fileId, FSNew, fileConnReq, groupMemberId, currentTs, currentTs)
    pure RcvFileTransfer {fileId, fileInvitation = f, fileStatus = RFSNew, senderDisplayName = c, chunkSize}

getRcvFileTransfer :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> m RcvFileTransfer
getRcvFileTransfer st userId fileId =
  liftIOEither . withTransaction st $ \db ->
    getRcvFileTransfer_ db userId fileId

getRcvFileTransfer_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError RcvFileTransfer)
getRcvFileTransfer_ db userId fileId =
  rcvFileTransfer
    <$> DB.query
      db
      [sql|
          SELECT r.file_status, r.file_queue_info, f.file_name,
            f.file_size, f.chunk_size, cs.local_display_name, m.local_display_name,
            f.file_path, c.connection_id, c.agent_conn_id
          FROM rcv_files r
          JOIN files f USING (file_id)
          LEFT JOIN connections c ON r.file_id = c.rcv_file_id
          LEFT JOIN contacts cs USING (contact_id)
          LEFT JOIN group_members m USING (group_member_id)
          WHERE f.user_id = ? AND f.file_id = ?
        |]
      (userId, fileId)
  where
    rcvFileTransfer ::
      [(FileStatus, ConnReqInvitation, String, Integer, Integer, Maybe ContactName, Maybe ContactName, Maybe FilePath, Maybe Int64, Maybe AgentConnId)] ->
      Either StoreError RcvFileTransfer
    rcvFileTransfer [(fileStatus', fileConnReq, fileName, fileSize, chunkSize, contactName_, memberName_, filePath_, connId_, agentConnId_)] =
      let fileInv = FileInvitation {fileName, fileSize, fileConnReq}
          fileInfo = (filePath_, connId_, agentConnId_)
       in case contactName_ <|> memberName_ of
            Nothing -> Left $ SERcvFileInvalid fileId
            Just name ->
              case fileStatus' of
                FSNew -> Right RcvFileTransfer {fileId, fileInvitation = fileInv, fileStatus = RFSNew, senderDisplayName = name, chunkSize}
                FSAccepted -> ft name fileInv RFSAccepted fileInfo
                FSConnected -> ft name fileInv RFSConnected fileInfo
                FSComplete -> ft name fileInv RFSComplete fileInfo
                FSCancelled -> ft name fileInv RFSCancelled fileInfo
      where
        ft senderDisplayName fileInvitation rfs = \case
          (Just filePath, Just connId, Just agentConnId) ->
            let fileStatus = rfs RcvFileInfo {filePath, connId, agentConnId}
             in Right RcvFileTransfer {..}
          _ -> Left $ SERcvFileInvalid fileId
    rcvFileTransfer _ = Left $ SERcvFileNotFound fileId

acceptRcvFileTransfer :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> ConnId -> FilePath -> m ()
acceptRcvFileTransfer st userId fileId agentConnId filePath =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      "UPDATE files SET file_path = ?, updated_at = ? WHERE user_id = ? AND file_id = ?"
      (filePath, currentTs, userId, fileId)
    DB.execute
      db
      "UPDATE rcv_files SET file_status = ?, updated_at = ? WHERE file_id = ?"
      (FSAccepted, currentTs, fileId)
    DB.execute
      db
      "INSERT INTO connections (agent_conn_id, conn_status, conn_type, rcv_file_id, user_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
      (agentConnId, ConnJoined, ConnRcvFile, fileId, userId, currentTs, currentTs)

updateRcvFileStatus :: MonadUnliftIO m => SQLiteStore -> RcvFileTransfer -> FileStatus -> m ()
updateRcvFileStatus st RcvFileTransfer {fileId} status =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute db "UPDATE rcv_files SET file_status = ?, updated_at = ? WHERE file_id = ?" (status, currentTs, fileId)

createRcvFileChunk :: MonadUnliftIO m => SQLiteStore -> RcvFileTransfer -> Integer -> AgentMsgId -> m RcvChunkStatus
createRcvFileChunk st RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileSize}, chunkSize} chunkNo msgId =
  liftIO . withTransaction st $ \db -> do
    status <- getLastChunkNo db
    unless (status == RcvChunkError) $ do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT OR REPLACE INTO rcv_file_chunks (file_id, chunk_number, chunk_agent_msg_id, created_at, updated_at) VALUES (?,?,?,?,?)"
        (fileId, chunkNo, msgId, currentTs, currentTs)
    pure status
  where
    getLastChunkNo db = do
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

updatedRcvFileChunkStored :: MonadUnliftIO m => SQLiteStore -> RcvFileTransfer -> Integer -> m ()
updatedRcvFileChunkStored st RcvFileTransfer {fileId} chunkNo =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        UPDATE rcv_file_chunks
        SET chunk_stored = 1, updated_at = ?
        WHERE file_id = ? AND chunk_number = ?
      |]
      (currentTs, fileId, chunkNo)

deleteRcvFileChunks :: MonadUnliftIO m => SQLiteStore -> RcvFileTransfer -> m ()
deleteRcvFileChunks st RcvFileTransfer {fileId} =
  liftIO . withTransaction st $ \db ->
    DB.execute db "DELETE FROM rcv_file_chunks WHERE file_id = ?" (Only fileId)

updateFileTransferChatItemId :: MonadUnliftIO m => SQLiteStore -> FileTransferId -> ChatItemId -> m ()
updateFileTransferChatItemId st fileId ciId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute db "UPDATE files SET chat_item_id = ?, updated_at = ? WHERE file_id = ?" (ciId, currentTs, fileId)

getFileTransfer :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> m FileTransfer
getFileTransfer st userId fileId =
  liftIOEither . withTransaction st $ \db ->
    getFileTransfer_ db userId fileId

getFileTransferProgress :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> m (FileTransfer, [Integer])
getFileTransferProgress st userId fileId =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    ft <- ExceptT $ getFileTransfer_ db userId fileId
    liftIO $
      (ft,) . map fromOnly <$> case ft of
        FTSnd _ -> DB.query db "SELECT COUNT(*) FROM snd_file_chunks WHERE file_id = ? and chunk_sent = 1 GROUP BY connection_id" (Only fileId)
        FTRcv _ -> DB.query db "SELECT COUNT(*) FROM rcv_file_chunks WHERE file_id = ? AND chunk_stored = 1" (Only fileId)

getFileTransfer_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError FileTransfer)
getFileTransfer_ db userId fileId =
  fileTransfer
    =<< DB.query
      db
      [sql|
        SELECT s.file_id, r.file_id
        FROM files f
        LEFT JOIN snd_files s ON s.file_id = f.file_id
        LEFT JOIN rcv_files r ON r.file_id = f.file_id
        WHERE user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)
  where
    fileTransfer :: [(Maybe Int64, Maybe Int64)] -> IO (Either StoreError FileTransfer)
    fileTransfer ((Just _, Nothing) : _) = FTSnd <$$> getSndFileTransfers_ db userId fileId
    fileTransfer [(Nothing, Just _)] = FTRcv <$$> getRcvFileTransfer_ db userId fileId
    fileTransfer _ = pure . Left $ SEFileNotFound fileId

getSndFileTransfers_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError [SndFileTransfer])
getSndFileTransfers_ db userId fileId =
  sndFileTransfers
    <$> DB.query
      db
      [sql|
        SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.connection_id, c.agent_conn_id,
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
    sndFileTransfers :: [(FileStatus, String, Integer, Integer, FilePath, Int64, AgentConnId, Maybe ContactName, Maybe ContactName)] -> Either StoreError [SndFileTransfer]
    sndFileTransfers [] = Left $ SESndFileNotFound fileId
    sndFileTransfers fts = mapM sndFileTransfer fts
    sndFileTransfer (fileStatus, fileName, fileSize, chunkSize, filePath, connId, agentConnId, contactName_, memberName_) =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, recipientDisplayName, connId, agentConnId}
        Nothing -> Left $ SESndFileInvalid fileId

createNewMessage :: MonadUnliftIO m => SQLiteStore -> NewMessage -> m MessageId
createNewMessage st newMsg =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    createNewMessage_ db newMsg currentTs

createSndMsgDelivery :: MonadUnliftIO m => SQLiteStore -> SndMsgDelivery -> MessageId -> m ()
createSndMsgDelivery st sndMsgDelivery messageId =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    msgDeliveryId <- createSndMsgDelivery_ db sndMsgDelivery messageId currentTs
    createMsgDeliveryEvent_ db msgDeliveryId MDSSndAgent currentTs

createNewMessageAndRcvMsgDelivery :: MonadUnliftIO m => SQLiteStore -> NewMessage -> RcvMsgDelivery -> m MessageId
createNewMessageAndRcvMsgDelivery st newMsg rcvMsgDelivery =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    messageId <- createNewMessage_ db newMsg currentTs
    msgDeliveryId <- createRcvMsgDelivery_ db rcvMsgDelivery messageId currentTs
    createMsgDeliveryEvent_ db msgDeliveryId MDSRcvAgent currentTs
    pure messageId

createSndMsgDeliveryEvent :: StoreMonad m => SQLiteStore -> Int64 -> AgentMsgId -> MsgDeliveryStatus 'MDSnd -> m ()
createSndMsgDeliveryEvent st connId agentMsgId sndMsgDeliveryStatus =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    msgDeliveryId <- ExceptT $ getMsgDeliveryId_ db connId agentMsgId
    liftIO $ do
      currentTs <- getCurrentTime
      createMsgDeliveryEvent_ db msgDeliveryId sndMsgDeliveryStatus currentTs

createRcvMsgDeliveryEvent :: StoreMonad m => SQLiteStore -> Int64 -> AgentMsgId -> MsgDeliveryStatus 'MDRcv -> m ()
createRcvMsgDeliveryEvent st connId agentMsgId rcvMsgDeliveryStatus =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    msgDeliveryId <- ExceptT $ getMsgDeliveryId_ db connId agentMsgId
    liftIO $ do
      currentTs <- getCurrentTime
      createMsgDeliveryEvent_ db msgDeliveryId rcvMsgDeliveryStatus currentTs

createNewMessage_ :: DB.Connection -> NewMessage -> UTCTime -> IO MessageId
createNewMessage_ db NewMessage {direction, cmEventTag, msgBody} createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO messages
        (msg_sent, chat_msg_event, msg_body, created_at, updated_at)
      VALUES (?,?,?,?,?)
    |]
    (direction, cmEventTag, msgBody, createdAt, createdAt)
  insertedRowId db

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

createRcvMsgDelivery_ :: DB.Connection -> RcvMsgDelivery -> MessageId -> UTCTime -> IO Int64
createRcvMsgDelivery_ db RcvMsgDelivery {connId, agentMsgId, agentMsgMeta} messageId createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO msg_deliveries
        (message_id, connection_id, agent_msg_id, agent_msg_meta, chat_ts, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?)
    |]
    (messageId, connId, agentMsgId, msgMetaJson agentMsgMeta, snd $ broker agentMsgMeta, createdAt, createdAt)
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

getMsgDeliveryId_ :: DB.Connection -> Int64 -> AgentMsgId -> IO (Either StoreError Int64)
getMsgDeliveryId_ db connId agentMsgId =
  firstRow fromOnly (SENoMsgDelivery connId agentMsgId) $
    DB.query
      db
      [sql|
        SELECT msg_delivery_id
        FROM msg_deliveries m
        WHERE m.connection_id = ? AND m.agent_msg_id = ?
        LIMIT 1
      |]
      (connId, agentMsgId)

createPendingGroupMessage :: MonadUnliftIO m => SQLiteStore -> Int64 -> MessageId -> Maybe Int64 -> m ()
createPendingGroupMessage st groupMemberId messageId introId_ =
  liftIO . withTransaction st $ \db -> do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        INSERT INTO pending_group_messages
          (group_member_id, message_id, group_member_intro_id, created_at, updated_at) VALUES (?,?,?,?,?)
      |]
      (groupMemberId, messageId, introId_, currentTs, currentTs)

getPendingGroupMessages :: MonadUnliftIO m => SQLiteStore -> Int64 -> m [PendingGroupMessage]
getPendingGroupMessages st groupMemberId =
  liftIO . withTransaction st $ \db ->
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

deletePendingGroupMessage :: MonadUnliftIO m => SQLiteStore -> Int64 -> MessageId -> m ()
deletePendingGroupMessage st groupMemberId messageId =
  liftIO . withTransaction st $ \db ->
    DB.execute db "DELETE FROM pending_group_messages WHERE group_member_id = ? AND message_id = ?" (groupMemberId, messageId)

createNewChatItem :: (MonadUnliftIO m, MsgDirectionI d) => SQLiteStore -> UserId -> ChatDirection c d -> NewChatItem d -> m ChatItemId
createNewChatItem st userId chatDirection NewChatItem {createdByMsgId, itemSent, itemTs, itemContent, itemText, itemStatus, createdAt} =
  liftIO . withTransaction st $ \db -> do
    let (contactId_, groupId_, groupMemberId_) = ids
    DB.execute
      db
      [sql|
        INSERT INTO chat_items (
          user_id, contact_id, group_id, group_member_id, created_by_msg_id,
          item_sent, item_ts, item_content, item_text, item_status, created_at, updated_at
        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (userId, contactId_, groupId_, groupMemberId_, createdByMsgId)
          :. (itemSent, itemTs, itemContent, itemText, itemStatus, createdAt, createdAt)
      )
    ciId <- insertedRowId db
    case createdByMsgId of
      Nothing -> pure ()
      Just msgId ->
        DB.execute
          db
          "INSERT INTO chat_item_messages (chat_item_id, message_id, created_at, updated_at) VALUES (?,?,?,?)"
          (ciId, msgId, createdAt, createdAt)
    pure ciId
  where
    ids :: (Maybe Int64, Maybe Int64, Maybe Int64)
    ids = case chatDirection of
      CDDirectSnd Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDDirectRcv Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDGroupSnd GroupInfo {groupId} -> (Nothing, Just groupId, Nothing)
      CDGroupRcv GroupInfo {groupId} GroupMember {groupMemberId} -> (Nothing, Just groupId, Just groupMemberId)

getChatPreviews :: MonadUnliftIO m => SQLiteStore -> User -> m [AChat]
getChatPreviews st user =
  liftIO . withTransaction st $ \db -> do
    directChats <- getDirectChatPreviews_ db user
    groupChats <- getGroupChatPreviews_ db user
    cReqChats <- getContactRequestChatPreviews_ db user
    pure $ sortOn (Down . ts) (directChats <> groupChats <> cReqChats)
  where
    ts :: AChat -> UTCTime
    ts (AChat _ (Chat _ (ci : _))) = chatItemTs ci
    ts (AChat _ (Chat (DirectChat Contact {createdAt}) [])) = createdAt
    ts (AChat _ (Chat (GroupChat GroupInfo {createdAt}) [])) = createdAt
    ts (AChat _ (Chat (ContactRequest UserContactRequest {createdAt}) [])) = createdAt

chatItemTs :: CChatItem d -> UTCTime
chatItemTs (CChatItem _ (ChatItem _ CIMeta {itemTs} _)) = itemTs

getDirectChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getDirectChatPreviews_ db User {userId} = do
  tz <- getCurrentTimeZone
  map (toDirectChatPreview tz)
    <$> DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, ct.created_at,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.conn_status, c.conn_type,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at,
          -- ChatItem
          ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        JOIN connections c ON c.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, MAX(item_ts) MaxDate
          FROM chat_items
          WHERE item_deleted != 1
          GROUP BY contact_id
        ) CIMaxDates ON CIMaxDates.contact_id = c.contact_id
        LEFT JOIN chat_items ci ON ci.contact_id = CIMaxDates.contact_id
                               AND ci.item_ts = CIMaxDates.MaxDate
        WHERE ct.user_id = ?
          AND c.connection_id IN (
            SELECT cc_connection_id FROM (
              SELECT
                cc.connection_id AS cc_connection_id,
                (CASE WHEN cc.conn_status = ? OR cc.conn_status = ? THEN 1 ELSE 0 END) AS cc_conn_ready
              FROM connections cc
              WHERE cc.user_id = ct.user_id AND cc.contact_id = ct.contact_id
              ORDER BY cc_conn_ready DESC, cc_connection_id DESC
              LIMIT 1
            )
          )
        ORDER BY ci.item_ts DESC
      |]
      (userId, ConnReady, ConnSndReady)
  where
    toDirectChatPreview :: TimeZone -> ContactRow :. ConnectionRow :. MaybeChatItemRow -> AChat
    toDirectChatPreview tz (contactRow :. connRow :. ciRow_) =
      let contact = toContact $ contactRow :. connRow
          ci_ = toDirectChatItemList tz ciRow_
       in AChat SCTDirect $ Chat (DirectChat contact) ci_

getGroupChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getGroupChatPreviews_ db User {userId, userContactId} = do
  tz <- getCurrentTimeZone
  map (toGroupChatPreview tz)
    <$> DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, g.created_at,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id,
          pu.display_name, pu.full_name,
          -- ChatItem
          ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at,
          -- Maybe GroupMember - sender
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
          m.member_status, m.invited_by, m.local_display_name, m.contact_id,
          p.display_name, p.full_name
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = mu.contact_profile_id
        LEFT JOIN (
          SELECT group_id, MAX(item_ts) MaxDate
          FROM chat_items
          WHERE item_deleted != 1
          GROUP BY group_id
        ) GIMaxDates ON GIMaxDates.group_id = g.group_id
        LEFT JOIN chat_items ci ON ci.group_id = GIMaxDates.group_id
                               AND ci.item_ts = GIMaxDates.MaxDate
        LEFT JOIN group_members m ON m.group_member_id = ci.group_member_id
        LEFT JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
        WHERE g.user_id = ? AND mu.contact_id = ?
        ORDER BY ci.item_ts DESC
      |]
      (userId, userContactId)
  where
    toGroupChatPreview :: TimeZone -> GroupInfoRow :. MaybeGroupChatItemRow -> AChat
    toGroupChatPreview tz (groupInfoRow :. ciRow_) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          ci_ = toGroupChatItemList tz userContactId ciRow_
       in AChat SCTGroup $ Chat (GroupChat groupInfo) ci_

getContactRequestChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getContactRequestChatPreviews_ db User {userId} =
  map toContactRequestChatPreview
    <$> DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, cr.created_at
        FROM contact_requests cr
        JOIN connections c USING (user_contact_link_id)
        JOIN contact_profiles p USING (contact_profile_id)
        WHERE cr.user_id = ?
      |]
      (Only userId)
  where
    toContactRequestChatPreview :: ContactRequestRow -> AChat
    toContactRequestChatPreview cReqRow =
      let cReq = toContactRequest cReqRow
       in AChat SCTContactRequest $ Chat (ContactRequest cReq) []

getDirectChat :: StoreMonad m => SQLiteStore -> User -> Int64 -> ChatPagination -> m (Chat 'CTDirect)
getDirectChat st user contactId pagination =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    case pagination of
      CPLast count -> getDirectChatLast_ db user contactId count
      CPAfter afterId count -> getDirectChatAfter_ db user contactId afterId count
      CPBefore beforeId count -> getDirectChatBefore_ db user contactId beforeId count

getDirectChatLast_ :: DB.Connection -> User -> Int64 -> Int -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatLast_ db User {userId} contactId count = do
  contact <- ExceptT $ getContact_ db userId contactId
  chatItems <- ExceptT getDirectChatItemsLast_
  pure $ Chat (DirectChat contact) (reverse chatItems)
  where
    getDirectChatItemsLast_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsLast_ = do
      tz <- getCurrentTimeZone
      mapM (toDirectChatItem tz)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.contact_id = ?
            ORDER BY ci.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, count)

getDirectChatAfter_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatAfter_ db User {userId} contactId afterChatItemId count = do
  contact <- ExceptT $ getContact_ db userId contactId
  chatItems <- ExceptT getDirectChatItemsAfter_
  pure $ Chat (DirectChat contact) chatItems
  where
    getDirectChatItemsAfter_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsAfter_ = do
      tz <- getCurrentTimeZone
      mapM (toDirectChatItem tz)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.contact_id = ? AND ci.chat_item_id > ?
            ORDER BY ci.chat_item_id ASC
            LIMIT ?
          |]
          (userId, contactId, afterChatItemId, count)

getDirectChatBefore_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatBefore_ db User {userId} contactId beforeChatItemId count = do
  contact <- ExceptT $ getContact_ db userId contactId
  chatItems <- ExceptT getDirectChatItemsBefore_
  pure $ Chat (DirectChat contact) (reverse chatItems)
  where
    getDirectChatItemsBefore_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsBefore_ = do
      tz <- getCurrentTimeZone
      mapM (toDirectChatItem tz)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.contact_id = ? AND ci.chat_item_id < ?
            ORDER BY ci.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, beforeChatItemId, count)

getContactIdByName :: StoreMonad m => SQLiteStore -> UserId -> ContactName -> m Int64
getContactIdByName st userId cName =
  liftIOEither . withTransaction st $ \db -> getContactIdByName_ db userId cName

getContactIdByName_ :: DB.Connection -> UserId -> ContactName -> IO (Either StoreError Int64)
getContactIdByName_ db userId cName =
  firstRow fromOnly (SEContactNotFoundByName cName) $
    DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND local_display_name = ?" (userId, cName)

getContact :: StoreMonad m => SQLiteStore -> UserId -> Int64 -> m Contact
getContact st userId contactId =
  liftIOEither . withTransaction st $ \db -> getContact_ db userId contactId

getContact_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError Contact)
getContact_ db userId contactId =
  join
    <$> firstRow
      toContactOrError
      (SEContactNotFound contactId)
      ( DB.query
          db
          [sql|
            SELECT
              -- Contact
              ct.contact_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, ct.created_at,
              -- Connection
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.conn_status, c.conn_type,
              c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at
            FROM contacts ct
            JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
            LEFT JOIN connections c ON c.contact_id = ct.contact_id
            WHERE ct.user_id = ? AND ct.contact_id = ? AND (c.conn_status = ? OR c.conn_status = ?)
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          (userId, contactId, ConnReady, ConnSndReady)
      )

getGroupChat :: StoreMonad m => SQLiteStore -> User -> Int64 -> ChatPagination -> m (Chat 'CTGroup)
getGroupChat st user groupId pagination =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    case pagination of
      CPLast count -> getGroupChatLast_ db user groupId count
      CPAfter afterId count -> getGroupChatAfter_ db user groupId afterId count
      CPBefore beforeId count -> getGroupChatBefore_ db user groupId beforeId count

getGroupChatLast_ :: DB.Connection -> User -> Int64 -> Int -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatLast_ db user@User {userId, userContactId} groupId count = do
  groupInfo <- ExceptT $ getGroupInfo_ db user groupId
  chatItems <- ExceptT getGroupChatItemsLast_
  pure $ Chat (GroupChat groupInfo) (reverse chatItems)
  where
    getGroupChatItemsLast_ :: IO (Either StoreError [CChatItem 'CTGroup])
    getGroupChatItemsLast_ = do
      tz <- getCurrentTimeZone
      mapM (toGroupChatItem tz userContactId)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at,
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
              m.member_status, m.invited_by, m.local_display_name, m.contact_id,
              p.display_name, p.full_name
            FROM chat_items ci
            LEFT JOIN group_members m ON m.group_member_id = ci.group_member_id
            LEFT JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            WHERE ci.user_id = ? AND ci.group_id = ?
            ORDER BY ci.chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, count)

getGroupChatAfter_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatAfter_ db user@User {userId, userContactId} groupId afterChatItemId count = do
  groupInfo <- ExceptT $ getGroupInfo_ db user groupId
  chatItems <- ExceptT getGroupChatItemsAfter_
  pure $ Chat (GroupChat groupInfo) chatItems
  where
    getGroupChatItemsAfter_ :: IO (Either StoreError [CChatItem 'CTGroup])
    getGroupChatItemsAfter_ = do
      tz <- getCurrentTimeZone
      mapM (toGroupChatItem tz userContactId)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at,
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
              m.member_status, m.invited_by, m.local_display_name, m.contact_id,
              p.display_name, p.full_name
            FROM chat_items ci
            LEFT JOIN group_members m ON m.group_member_id = ci.group_member_id
            LEFT JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            WHERE ci.user_id = ? AND ci.group_id = ? AND ci.chat_item_id > ?
            ORDER BY ci.chat_item_id ASC
            LIMIT ?
          |]
          (userId, groupId, afterChatItemId, count)

getGroupChatBefore_ :: DB.Connection -> User -> Int64 -> ChatItemId -> Int -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatBefore_ db user@User {userId, userContactId} groupId beforeChatItemId count = do
  groupInfo <- ExceptT $ getGroupInfo_ db user groupId
  chatItems <- ExceptT getGroupChatItemsBefore_
  pure $ Chat (GroupChat groupInfo) (reverse chatItems)
  where
    getGroupChatItemsBefore_ :: IO (Either StoreError [CChatItem 'CTGroup])
    getGroupChatItemsBefore_ = do
      tz <- getCurrentTimeZone
      mapM (toGroupChatItem tz userContactId)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at,
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
              m.member_status, m.invited_by, m.local_display_name, m.contact_id,
              p.display_name, p.full_name
            FROM chat_items ci
            LEFT JOIN group_members m ON m.group_member_id = ci.group_member_id
            LEFT JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            WHERE ci.user_id = ? AND ci.group_id = ? AND ci.chat_item_id < ?
            ORDER BY ci.chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, beforeChatItemId, count)

getGroupInfo :: StoreMonad m => SQLiteStore -> User -> Int64 -> m GroupInfo
getGroupInfo st user groupId =
  liftIOEither . withTransaction st $ \db ->
    getGroupInfo_ db user groupId

getGroupInfo_ :: DB.Connection -> User -> Int64 -> IO (Either StoreError GroupInfo)
getGroupInfo_ db User {userId, userContactId} groupId =
  firstRow (toGroupInfo userContactId) (SEGroupNotFound groupId) $
    DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, g.created_at,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id,
          pu.display_name, pu.full_name
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = mu.contact_profile_id
        WHERE g.group_id = ? AND g.user_id = ? AND mu.contact_id = ?
      |]
      (groupId, userId, userContactId)

getGroupIdByName :: StoreMonad m => SQLiteStore -> User -> GroupName -> m Int64
getGroupIdByName st user gName =
  liftIOEither . withTransaction st $ \db -> getGroupIdByName_ db user gName

getGroupIdByName_ :: DB.Connection -> User -> GroupName -> IO (Either StoreError Int64)
getGroupIdByName_ db User {userId} gName =
  firstRow fromOnly (SEGroupNotFoundByName gName) $
    DB.query db "SELECT group_id FROM groups WHERE user_id = ? AND local_display_name = ?" (userId, gName)

getChatItemIdByAgentMsgId :: StoreMonad m => SQLiteStore -> Int64 -> AgentMsgId -> m (Maybe ChatItemId)
getChatItemIdByAgentMsgId st connId msgId =
  liftIO . withTransaction st $ \db ->
    join . listToMaybe . map fromOnly
      <$> DB.query
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

updateDirectChatItem :: (StoreMonad m, MsgDirectionI d) => SQLiteStore -> ChatItemId -> CIStatus d -> m (ChatItem 'CTDirect d)
updateDirectChatItem st itemId itemStatus =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    ci <- ExceptT $ getDirectChatItem_ db itemId
    liftIO $ DB.execute db "UPDATE chat_items SET item_status = ? WHERE chat_item_id = ?" (itemStatus, itemId)
    pure ci {meta = (meta ci) {itemStatus}}

getDirectChatItem_ :: forall d. MsgDirectionI d => DB.Connection -> ChatItemId -> IO (Either StoreError (ChatItem 'CTDirect d))
getDirectChatItem_ db itemId = do
  tz <- getCurrentTimeZone
  join
    <$> firstRow
      (correctDir <=< toDirectChatItem tz)
      (SEChatItemNotFound itemId)
      ( DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              ci.chat_item_id, ci.item_ts, ci.item_content, ci.item_text, ci.item_status, ci.created_at
            FROM chat_items ci
            WHERE ci.chat_item_id = ?
          |]
          (Only itemId)
      )
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

type ChatItemRow = (Int64, ChatItemTs, ACIContent, Text, ACIStatus, UTCTime)

type MaybeChatItemRow = (Maybe Int64, Maybe ChatItemTs, Maybe ACIContent, Maybe Text, Maybe ACIStatus, Maybe UTCTime)

toDirectChatItem :: TimeZone -> ChatItemRow -> Either StoreError (CChatItem 'CTDirect)
toDirectChatItem tz (itemId, itemTs, itemContent, itemText, itemStatus, createdAt) =
  case (itemContent, itemStatus) of
    (ACIContent d@SMDSnd ciContent, ACIStatus d'@SMDSnd ciStatus) -> case testEquality d d' of
      Just Refl -> Right $ CChatItem d (ChatItem CIDirectSnd (ciMeta ciStatus) ciContent)
      _ -> badItem
    (ACIContent d@SMDRcv ciContent, ACIStatus d'@SMDRcv ciStatus) -> case testEquality d d' of
      Just Refl -> Right $ CChatItem d (ChatItem CIDirectRcv (ciMeta ciStatus) ciContent)
      _ -> badItem
    _ -> badItem
  where
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIStatus d -> CIMeta d
    ciMeta status = mkCIMeta itemId itemText status tz itemTs createdAt

toDirectChatItemList :: TimeZone -> MaybeChatItemRow -> [CChatItem 'CTDirect]
toDirectChatItemList tz (Just itemId, Just itemTs, Just itemContent, Just itemText, Just itemStatus, Just createdAt) =
  either (const []) (: []) $ toDirectChatItem tz (itemId, itemTs, itemContent, itemText, itemStatus, createdAt)
toDirectChatItemList _ _ = []

type GroupChatItemRow = ChatItemRow :. MaybeGroupMemberRow

type MaybeGroupChatItemRow = MaybeChatItemRow :. MaybeGroupMemberRow

toGroupChatItem :: TimeZone -> Int64 -> GroupChatItemRow -> Either StoreError (CChatItem 'CTGroup)
toGroupChatItem tz userContactId ((itemId, itemTs, itemContent, itemText, itemStatus, createdAt) :. memberRow_) = do
  let member_ = toMaybeGroupMember userContactId memberRow_
  case (itemContent, itemStatus, member_) of
    (ACIContent d@SMDSnd ciContent, ACIStatus d'@SMDSnd ciStatus, Nothing) -> case testEquality d d' of
      Just Refl -> Right $ CChatItem d (ChatItem CIGroupSnd (ciMeta ciStatus) ciContent)
      _ -> badItem
    (ACIContent d@SMDRcv ciContent, ACIStatus d'@SMDRcv ciStatus, Just member) -> case testEquality d d' of
      Just Refl -> Right $ CChatItem d (ChatItem (CIGroupRcv member) (ciMeta ciStatus) ciContent)
      _ -> badItem
    _ -> badItem
  where
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIStatus d -> CIMeta d
    ciMeta status = mkCIMeta itemId itemText status tz itemTs createdAt

toGroupChatItemList :: TimeZone -> Int64 -> MaybeGroupChatItemRow -> [CChatItem 'CTGroup]
toGroupChatItemList tz userContactId ((Just itemId, Just itemTs, Just itemContent, Just itemText, Just itemStatus, Just createdAt) :. memberRow_) =
  either (const []) (: []) $ toGroupChatItem tz userContactId ((itemId, itemTs, itemContent, itemText, itemStatus, createdAt) :. memberRow_)
toGroupChatItemList _ _ _ = []

-- | Saves unique local display name based on passed displayName, suffixed with _N if required.
-- This function should be called inside transaction.
withLocalDisplayName :: forall a. DB.Connection -> UserId -> Text -> (Text -> IO a) -> IO (Either StoreError a)
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
        Right () -> Right <$> action ldn
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

createWithRandomId :: forall a. TVar ChaChaDRG -> (ByteString -> IO a) -> IO (Either StoreError a)
createWithRandomId = createWithRandomBytes 12

createWithRandomBytes :: forall a. Int -> TVar ChaChaDRG -> (ByteString -> IO a) -> IO (Either StoreError a)
createWithRandomBytes size gVar create = tryCreate 3
  where
    tryCreate :: Int -> IO (Either StoreError a)
    tryCreate 0 = pure $ Left SEUniqueID
    tryCreate n = do
      id' <- randomBytes gVar size
      E.try (create id') >>= \case
        Right x -> pure $ Right x
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> pure . Left . SEInternalError $ show e

randomBytes :: TVar ChaChaDRG -> Int -> IO ByteString
randomBytes gVar n = B64.encode <$> (atomically . stateTVar gVar $ randomBytesGenerate n)

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
  | SEGroupNotFound {groupId :: Int64}
  | SEGroupNotFoundByName {groupName :: GroupName}
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEGroupAlreadyJoined
  | SEGroupInvitationNotFound
  | SESndFileNotFound {fileId :: FileTransferId}
  | SESndFileInvalid {fileId :: FileTransferId}
  | SERcvFileNotFound {fileId :: FileTransferId}
  | SEFileNotFound {fileId :: FileTransferId}
  | SERcvFileInvalid {fileId :: FileTransferId}
  | SEConnectionNotFound {agentConnId :: AgentConnId}
  | SEIntroNotFound
  | SEUniqueID
  | SEInternalError {message :: String}
  | SENoMsgDelivery {connId :: Int64, agentMsgId :: AgentMsgId}
  | SEBadChatItem {itemId :: ChatItemId}
  | SEChatItemNotFound {itemId :: ChatItemId}
  deriving (Show, Exception, Generic)

instance ToJSON StoreError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SE"
