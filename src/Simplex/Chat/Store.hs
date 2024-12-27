{-# LANGUAGE CPP #-}

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
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.SQLite.Migrations
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.Common (DBStore (..), withTransaction)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation, MigrationError)
#if defined(dbPostgres)
import Simplex.Messaging.Agent.Store.Postgres (createDBStore)
#else
import Simplex.Messaging.Agent.Store.SQLite (createDBStore)
#endif

createChatStore :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> IO (Either MigrationError DBStore)
createChatStore dbPath key keepKey = createDBStore dbPath key keepKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
