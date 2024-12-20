module Simplex.Chat.Store
  ( SQLiteStore,
    StoreError (..),
    ChatLockEntity (..),
    UserMsgReceiptSettings (..),
    UserContactLink (..),
    AutoAccept (..),
    createChatStore,
    migrations, -- used in tests
    chatStoreFile,
    agentStoreFile,
    withTransaction,
  )
where

import Data.ByteArray (ScrubbedBytes)
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.SQLite.Migrations
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation, MigrationError, SQLiteStore (..), createSQLiteStore, withTransaction)

createChatStore :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> IO (Either MigrationError SQLiteStore)
createChatStore dbPath key keepKey = createSQLiteStore dbPath key keepKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
