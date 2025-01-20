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
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation, MigrationError)
#if defined(dbPostgres)
import Simplex.Chat.Store.Postgres.Migrations
#else
import Simplex.Chat.Store.SQLite.Migrations
#endif

createChatStore :: DBCreateOpts -> MigrationConfirmation -> IO (Either MigrationError DBStore)
createChatStore dbCreateOpts = createDBStore dbCreateOpts migrations

#if defined(dbPostgres)
chatSchema :: String -> String
chatSchema "" = "simplex_v1_chat_schema"
chatSchema prefix = prefix <> "_chat_schema"

agentSchema :: String -> String
agentSchema "" = "simplex_v1_agent_schema"
agentSchema prefix = prefix <> "_agent_schema"
#else
chatStoreFile :: FilePath -> FilePath
chatStoreFile = (<> "_chat.db")

agentStoreFile :: FilePath -> FilePath
agentStoreFile = (<> "_agent.db")
#endif
