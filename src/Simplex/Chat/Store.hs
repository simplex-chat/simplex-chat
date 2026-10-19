{-# LANGUAGE CPP #-}

module Simplex.Chat.Store
  ( DBStore,
    StoreError (..),
    ChatLockEntity (..),
    UserMsgReceiptSettings (..),
    UserContactLink (..),
    GroupLinkInfo (..),
    AddressSettings (..),
    AutoAccept (..),
    createChatStore,
    migrations, -- used in tests
    withTransaction,
  )
where

import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.Common (DBStore (..), withTransaction)
import Simplex.Messaging.Agent.Store.Interface (DBOpts, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig, MigrationError)
#if defined(dbPostgres)
import Simplex.Chat.Store.Postgres.Migrations
#else
import Simplex.Chat.Store.SQLite.Migrations
#endif

createChatStore :: DBOpts -> MigrationConfig -> IO (Either MigrationError DBStore)
createChatStore dbCreateOpts = createDBStore dbCreateOpts migrations
