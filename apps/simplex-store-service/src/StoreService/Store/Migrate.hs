{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Store.Migrate
  ( runStoreServiceMigrations,
    withInstanceLock,
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
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Only (..))
import qualified Database.PostgreSQL.Simple as PSQL
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

-- | One process per database, as a second would send with the gas key's nonces and run the chat agent's queues too: the
-- lock is held on a connection of its own for the whole run. SQLite, used only in development, takes none.
withInstanceLock :: StoreServiceOpts -> IO a -> IO a
#if defined(dbPostgres)
withInstanceLock StoreServiceOpts {coreOptions = CoreChatOpts {dbOptions = ChatDbOpts {dbConnstr}}} action =
  bracket (PSQL.connectPostgreSQL $ B.pack dbConnstr) PSQL.close $ \db ->
    PSQL.query db "SELECT pg_try_advisory_lock(?)" (Only instanceLockKey) >>= \case
      [Only True] -> action
      _ -> exit "another store service holds this database; stop it before starting this one, --run-cli included"

-- | "SXSTORE" in ASCII, the lock's key in the database's advisory lock space.
instanceLockKey :: Int64
instanceLockKey = 0x535853544f5245
#else
withInstanceLock _ action = action
#endif

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure
