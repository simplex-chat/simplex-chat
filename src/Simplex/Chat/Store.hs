module Simplex.Chat.Store
  ( DBStore,
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
import Simplex.Chat.Store.Migrations
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.SQLite (createDBStore)
import Simplex.Messaging.Agent.Store.SQLite.Common (DBStore (..), withTransaction)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation, MigrationError)

createChatStore :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> Bool -> IO (Either MigrationError DBStore)
createChatStore dbPath key keepKey = createDBStore dbPath key keepKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
