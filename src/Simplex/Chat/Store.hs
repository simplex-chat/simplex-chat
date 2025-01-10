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
#if defined(dbPostgres)
    chatSchema,
    agentSchema,
#else
    chatStoreFile,
    agentStoreFile,
#endif
    withTransaction,
  )
where

import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.Common (DBStore (..), withTransaction)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation, MigrationError)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Simplex.Chat.Store.Postgres.Migrations
import Simplex.Messaging.Agent.Store.Postgres (createDBStore)
#else
import Data.ByteArray (ScrubbedBytes)
import Simplex.Chat.Store.SQLite.Migrations
import Simplex.Messaging.Agent.Store.SQLite (createDBStore)
#endif

#if defined(dbPostgres)
createChatStore :: ConnectInfo -> String -> MigrationConfirmation -> IO (Either MigrationError DBStore)
createChatStore connectInfo schema = createDBStore connectInfo schema migrations

chatSchema :: String -> String
chatSchema "" = "chat_schema"
chatSchema prefix = prefix <> "_chat_schema"

agentSchema :: String -> String
agentSchema "" = "agent_schema"
agentSchema prefix = prefix <> "_agent_schema"
#else
createChatStore :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> Bool -> IO (Either MigrationError DBStore)
createChatStore dbPath key keepKey = createDBStore dbPath key keepKey migrations

chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
#endif
