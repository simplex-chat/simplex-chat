module Simplex.Chat.Store
  ( SQLiteStore,
    StoreError (..),
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

import Simplex.Chat.Store.Migrations
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation, MigrationError, SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Crypto.Memory (LockedBytes)

createChatStore :: FilePath -> LockedBytes -> Bool -> MigrationConfirmation -> IO (Either MigrationError SQLiteStore)
createChatStore dbPath key keepKey = createSQLiteStore dbPath key keepKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
