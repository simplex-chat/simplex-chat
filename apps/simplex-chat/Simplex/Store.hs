{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Store
  ( SQLiteStore,
    StoreError (..),
    createStore,
    createUser,
    getUsers,
    setActiveUser,
    createDirectConnection,
    createDirectContact,
    deleteContact,
    getContactConnection,
    getContactConnections,
    getConnectionChatDirection,
  )
where

import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), SQLError)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AParty (..), ConnId)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Util (bshow, liftIOEither, (<$$>))
import System.FilePath (takeBaseName, takeExtension)

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations =
  sortBy (compare `on` name) . map migration . filter sqlFile $
    $(makeRelativeToProject "migrations" >>= embedDir)
  where
    sqlFile (file, _) = takeExtension file == ".sql"
    migration (file, qStr) = Migration {name = takeBaseName file, up = decodeUtf8 qStr}

createStore :: FilePath -> Int -> IO SQLiteStore
createStore dbFilePath poolSize = createSQLiteStore dbFilePath poolSize migrations

checkConstraint :: StoreError -> IO (Either StoreError a) -> IO (Either StoreError a)
checkConstraint err action = action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | DB.sqlError e == DB.ErrorConstraint = err
  | otherwise = SEInternal $ bshow e

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid();"

createUser :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> Profile -> Bool -> m User
createUser st Profile {contactRef, displayName} activeUser =
  liftIOEither . checkConstraint SEDuplicateContactRef . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO contact_profiles (contact_ref, display_name) VALUES (?, ?);" (contactRef, displayName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO users (contact_id, active_user) VALUES (0, ?);" (Only activeUser)
    userId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_contact_ref, lcr_base, user_id, user) VALUES (?, ?, ?, ?, 1);"
      (profileId, contactRef, contactRef, userId)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?;" (contactId, userId)
    pure . Right $ toUser (userId, activeUser, contactRef, displayName)

getUsers :: SQLiteStore -> IO [User]
getUsers st =
  withTransaction st $ \db ->
    map toUser
      <$> DB.query_
        db
        [sql|
          SELECT u.user_id, u.active_user, c.local_contact_ref, p.display_name
          FROM users u
          JOIN contacts c ON u.contact_id = c.contact_id
          JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
        |]

toUser :: (UserId, Bool, ContactRef, Text) -> User
toUser (userId, activeUser, contactRef, displayName) =
  let profile = Profile {contactRef, displayName}
   in User {userId, localContactRef = contactRef, profile, activeUser}

setActiveUser :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
setActiveUser st userId = do
  liftIO . withTransaction st $ \db -> do
    DB.execute_ db "UPDATE users SET active_user = 0;"
    DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?;" (Only userId)

createDirectConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> m ()
createDirectConnection st userId agentConnId =
  liftIO . withTransaction st $ \db ->
    DB.execute
      db
      [sql|
        INSERT INTO connections
          (user_id, agent_conn_id, conn_status, conn_type) VALUES (?,?,?,?);
      |]
      (userId, agentConnId, ConnNew, ConnContact)

createDirectContact ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> Connection -> Profile -> m ()
createDirectContact st userId Connection {connId} Profile {contactRef, displayName} =
  liftIOEither . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO contact_profiles (contact_ref, display_name) VALUES (?, ?);" (contactRef, displayName)
    profileId <- insertedRowId db
    lcrSuffix :: Int <-
      maybe 0 ((+ 1) . fromOnly) . listToMaybe
        <$> DB.queryNamed
          db
          [sql|
            SELECT lcr_suffix FROM contacts
            WHERE user_id = :user_id AND lcr_base = :contact_ref
            ORDER BY lcr_suffix DESC
            LIMIT 1;
          |]
          [":user_id" := userId, ":contact_ref" := contactRef]
    create db profileId lcrSuffix 20
  where
    create :: DB.Connection -> Int64 -> Int -> Int -> IO (Either StoreError ())
    create _ _ _ 0 = pure $ Left SEDuplicateContactRef
    create db profileId lcrSuffix attempts = do
      let lcr = localContactRef' lcrSuffix
      E.try (insertUser lcr) >>= \case
        Right () -> do
          contactId <- insertedRowId db
          DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, connId)
          pure $ Right ()
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> create db profileId (lcrSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        localContactRef' 0 = contactRef
        localContactRef' n = contactRef <> T.pack ('_' : show n)
        insertUser lcr =
          DB.execute
            db
            [sql|
              INSERT INTO contacts
                (contact_profile_id, local_contact_ref, lcr_base, lcr_suffix, user_id) VALUES (?, ?, ?, ?, ?)
            |]
            (profileId, lcr, contactRef, lcrSuffix, userId)

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactRef -> m ()
deleteContact st userId contactRef =
  liftIO . withTransaction st $ \db ->
    forM_
      [ [sql|
          DELETE FROM connections WHERE connection_id IN (
            SELECT connection_id
            FROM connections c
            JOIN contacts cs ON c.contact_id = cs.contact_id
            WHERE cs.user_id = :user_id AND cs.local_contact_ref = :contact_ref
          );
        |],
        [sql|
          DELETE FROM contacts
          WHERE user_id = :user_id AND local_contact_ref = :contact_ref;
        |]
      ]
      $ \q -> DB.executeNamed db q [":user_id" := userId, ":contact_ref" := contactRef]

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContactConnection ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactRef -> m Connection
getContactConnection st userId contactRef =
  liftIOEither . withTransaction st $ \db ->
    connection
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
          FROM connections c
          JOIN contacts cs ON c.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_contact_ref == :contact_ref
          ORDER BY c.connection_id DESC
          LIMIT 1;
        |]
        [":user_id" := userId, ":contact_ref" := contactRef]
  where
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEContactNotFound contactRef

getContactConnections :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactRef -> m [Connection]
getContactConnections st userId contactRef =
  liftIO . withTransaction st $ \db ->
    map toConnection
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
          FROM connections c
          JOIN contacts cs ON c.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_contact_ref == :contact_ref;
        |]
        [":user_id" := userId, ":contact_ref" := contactRef]

toConnection ::
  (Int64, ConnId, Int, Maybe Int64, ConnStatus, ConnType, Maybe Int64, Maybe Int64, UTCTime) -> Connection
toConnection (connId, agentConnId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, createdAt) =
  let entityId = entityId_ connType
   in Connection {connId, agentConnId, connLevel, viaContact, connStatus, connType, entityId, createdAt}
  where
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId

getConnectionChatDirection ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ConnId -> m (ChatDirection 'Agent)
getConnectionChatDirection st userId agentConnId =
  liftIOEither . withTransaction st $ \db -> do
    getConnection db >>= \case
      Left e -> pure $ Left e
      Right c@Connection {connType, entityId} -> case connType of
        ConnMember -> pure . Left $ SEInternal "group members not supported yet"
        ConnContact ->
          ReceivedDirectMessage <$$> case entityId of
            Nothing -> pure $ Right NewContact {activeConn = c}
            Just cId -> getContact db cId c
  where
    getConnection db =
      connection
        <$> DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact,
              conn_status, conn_type, contact_id, group_member_id, created_at
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?;
          |]
          (userId, agentConnId)
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEConnectionNotFound agentConnId
    getContact db contactId c =
      toContact contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.local_contact_ref, p.contact_ref, p.display_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact contactId c [(localContactRef, contactRef, displayName)] =
      let profile = Profile {contactRef, displayName}
       in Right Contact {contactId, localContactRef, profile, activeConn = c}
    toContact _ _ _ = Left $ SEInternal "referenced contact not found"

-- chatDirection :: [ChatDirRow] -> Either StoreError (ChatDirection 'Agent)
-- chatDirection [d] = Right $ toChatDirection agentConnId d
-- chatDirection _ = Left SEConnectionNotFound agentConnId

-- type ChatDirRow = (Int64, Text, Int64, Int, Maybe Int64, ConnStatus, ContactRef, Text)

-- toChatDirection :: ConnId -> ChatDirRow -> ChatDirection 'Agent
-- toChatDirection
--   agentConnId
--   (contactId, localContactRef, connId, connLevel, viaContact, connStatus, contactRef, displayName) =
--     let profile = Profile {contactRef, displayName}
--         activeConn = Connection {connId, agentConnId, connLevel, viaContact, connStatus}
--      in ReceivedDirectMessage $ Contact {contactId, localContactRef, profile, activeConn}

data StoreError
  = SEDuplicateContactRef
  | SEContactNotFound ContactRef
  | SEConnectionNotFound ConnId
  | SEInternal ByteString
  deriving (Show, Exception)
