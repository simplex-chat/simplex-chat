{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BadgeService.Store.Migrate
  ( runBadgeServiceMigrations,
  )
where

import BadgeService.Options
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Common
import Simplex.Messaging.Agent.Store.Interface (migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import System.Exit (exitFailure)

#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations
#else
import BadgeService.Store.SQLite.Migrations
#endif

runBadgeServiceMigrations :: BadgeServiceOpts -> ChatConfig -> DBStore -> IO ()
runBadgeServiceMigrations opts ChatConfig {confirmMigrations} chatStore =
  migrateDBSchema
    chatStore
    (toDBOpts dbOptions chatSuffix False [])
    (Just "sx_badge_service_migrations")
    badgeServiceSchemaMigrations
    MigrationConfig {confirm, backupPath = Nothing}
    >>= either (exit . ("badge service migrations " <>) . show) pure
  where
    BadgeServiceOpts {coreOptions = CoreChatOpts {dbOptions, yesToUpMigrations}} = opts
    confirm = if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure
