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
    createDirectContact,
    deleteContact,
    getContactConnection,
    getContactConnections,
    getConnectionChatDirection,
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (NamedParam (..), Only (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AParty (..), ConnId)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Util (liftIOEither)
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

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid();"

createDirectContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> Maybe Text -> m Contact'
createDirectContact st userId agentConnId contactRef =
  liftIO . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO connections (user_id, agent_conn_id, conn_status) VALUES (?,?,?);" (userId, agentConnId, ConnNew)
    connId <- insertedRowId db
    let activeConn = Connection {connId, agentConnId, connLevel = 0, viaContact = Nothing, connStatus = ConnNew}
    -- TODO support undefined localContactRef (Nothing) - currently it would fail
    let localContactRef = fromMaybe "" contactRef
    DB.execute db "INSERT INTO contacts (user_id, local_contact_ref) VALUES (?,?);" (userId, localContactRef)
    contactId <- insertedRowId db
    DB.execute db "INSERT INTO contact_connections (connection_id, contact_id, active) VALUES (?,?,1);" (connId, contactId)
    pure Contact' {contactId, localContactRef, profile = Nothing, activeConn}

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactRef -> m ()
deleteContact st userId contactRef =
  liftIO . withTransaction st $ \db ->
    forM_
      [ [sql|
          DELETE FROM connections
          WHERE user_id = :user_id AND connection_id IN (
            SELECT cc.connection_id
            FROM contact_connections AS cc
            JOIN contacts AS cs ON cs.contact_id = cc.contact_id
            WHERE local_contact_ref = :contact_ref
          );
        |],
        [sql|
          DELETE FROM contacts
          WHERE user_id = :user_id AND local_contact_ref = :contact_ref;
        |]
      ]
      $ \q -> DB.executeNamed db q [":user_id" := userId, ":contact_ref" := contactRef]

getContactConnection :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactRef -> m Connection
getContactConnection st userId contactRef =
  liftIOEither . withTransaction st $ \db ->
    connection
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.conn_status
          FROM connections AS c
          JOIN contact_connections AS cc ON cc.connection_id == c.connection_id
          JOIN contacts AS cs ON cc.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_contact_ref == :contact_ref
            AND cc.active == 1;
        |]
        [":user_id" := userId, ":contact_ref" := contactRef]
  where
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left SEContactNotFound

getContactConnections :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactRef -> m [Connection]
getContactConnections st userId contactRef =
  liftIO . withTransaction st $ \db ->
    map toConnection
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.conn_status
          FROM connections AS c
          JOIN contact_connections AS cc ON cc.connection_id == c.connection_id
          JOIN contacts AS cs ON cc.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_contact_ref == :contact_ref
            AND cc.active == 1;
        |]
        [":user_id" := userId, ":contact_ref" := contactRef]

toConnection :: (Int64, ConnId, Int, Maybe Int64, ConnStatus) -> Connection
toConnection (connId, agentConnId, connLevel, viaContact, connStatus) =
  Connection {connId, agentConnId, connLevel, viaContact, connStatus}

getConnectionChatDirection ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ConnId -> m (ChatDirection 'Agent)
getConnectionChatDirection st userId agentConnId =
  liftIOEither . withTransaction st $ \db ->
    chatDirection
      <$> DB.queryNamed
        db
        [sql|
          SELECT
            cs.contact_id, cs.local_contact_ref,
            a.connection_id, a.conn_level, a.via_contact, a.conn_status,
            p.contact_profile_id, p.contact_ref, p.display_name
          FROM contacts AS cs
          JOIN contact_connections AS cc ON cs.contact_id = cc.contact_id
          JOIN contact_connections AS ac ON cs.contact_id = ac.contact_id
          JOIN connections AS c ON c.connection_id = cc.connection_id
          JOIN connections AS a ON a.connection_id = ac.connection_id
          LEFT JOIN contact_profiles AS p ON p.contact_profile_id = cs.contact_profile_id
          WHERE cs.user_id = :user_id
            AND c.agent_conn_id = :agent_conn_id
            AND ac.active = 1
        |]
        [":user_id" := userId, ":agent_conn_id" := agentConnId]
  where
    chatDirection :: [ChatDirRow] -> Either StoreError (ChatDirection 'Agent)
    chatDirection [d] = Right $ toChatDirection agentConnId d
    chatDirection _ = Left SEContactNotFound

type ChatDirRow = (Int64, Text, Int64, Int, Maybe Int64, ConnStatus, Maybe Int64, Maybe ContactRef, Maybe Text)

toChatDirection :: ConnId -> ChatDirRow -> ChatDirection 'Agent
toChatDirection
  agentConnId
  (contactId, localContactRef, connId, connLevel, viaContact, connStatus, profileId, contactRef, displayName) =
    let profile = Profile <$> profileId <*> contactRef <*> displayName
        activeConn = Connection {connId, agentConnId, connLevel, viaContact, connStatus}
     in ReceivedDirectMessage $ Contact' {contactId, localContactRef, profile, activeConn}

data StoreError = SEContactNotFound | SEInternal ByteString
  deriving (Show, Exception)
