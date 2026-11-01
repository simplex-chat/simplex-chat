{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Store.Migrate
  ( runStoreServiceMigrations,
  )
where

import StoreService.Options
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Common
import Simplex.Messaging.Agent.Store.Interface (migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import System.Exit (exitFailure)

#if defined(dbPostgres)
import StoreService.Store.Postgres.Migrations
#else
import StoreService.Store.SQLite.Migrations
#endif

runStoreServiceMigrations :: StoreServiceOpts -> ChatConfig -> DBStore -> IO ()
runStoreServiceMigrations opts ChatConfig {confirmMigrations} chatStore =
  migrateDBSchema
    chatStore
    (toDBOpts dbOptions chatSuffix False [])
    (Just "sx_badge_service_migrations")
    storeServiceSchemaMigrations
    MigrationConfig {confirm, backupPath = Nothing}
    >>= either (exit . ("store service migrations " <>) . show) pure
  where
    StoreServiceOpts {coreOptions = CoreChatOpts {dbOptions, yesToUpMigrations}} = opts
    confirm = if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure
