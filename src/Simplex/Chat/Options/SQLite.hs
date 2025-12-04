{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Options.SQLite where

import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import Options.Applicative
import Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2
import Simplex.Messaging.Agent.Store.Interface (DBOpts (..))
import Simplex.Messaging.Agent.Store.SQLite.Common (SQLiteFuncDef (..), SQLiteFuncPtrs (..))
import Simplex.Messaging.Agent.Store.SQLite.DB (TrackQueries (..))
import System.FilePath (combine)

data ChatDbOpts = ChatDbOpts
  { dbFilePrefix :: String,
    dbKey :: ScrubbedBytes,
    trackQueries :: TrackQueries,
    vacuumOnMigration :: Bool
  }

chatDbOptsP :: FilePath -> FilePath -> Parser ChatDbOpts
chatDbOptsP appDir defaultDbName = do
  dbFilePrefix <-
    strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help "Path prefix to chat and agent database files"
          <> value (combine appDir defaultDbName)
          <> showDefault
      )
  dbKey <-
    strOption
      ( long "key"
          <> short 'k'
          <> metavar "KEY"
          <> help "Database encryption key/pass-phrase"
          <> value ""
      )
  disableVacuum <-
    switch
      ( long "disable-vacuum"
          <> help "Do not vacuum database after migrations"
      )
  pure
    ChatDbOpts
      { dbFilePrefix,
        dbKey,
        trackQueries = TQSlow 5000, -- 5ms
        vacuumOnMigration = not disableVacuum
      }

migrationBackupPathP :: Parser (Maybe FilePath)
migrationBackupPathP =
  flag' Nothing
    ( long "disable-backup"
        <> help "Disable backup when migrating database"
    )
    <|>
      (fmap Just . strOption)
        ( long "backup-directory"
            <> help "Directory to backup database for migration"
            <> value ""
        )

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbFilePrefix} = dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"

toDBOpts :: ChatDbOpts -> String -> Bool -> [SQLiteFuncDef] -> DBOpts
toDBOpts ChatDbOpts {dbFilePrefix, dbKey, trackQueries, vacuumOnMigration} dbSuffix keepKey dbFunctions = do
  DBOpts
    { dbFilePath = dbFilePrefix <> dbSuffix,
      dbFunctions,
      dbKey,
      keepKey,
      vacuum = vacuumOnMigration,
      track = trackQueries
    }

chatSuffix :: String
chatSuffix = "_chat.db"

agentSuffix :: String
agentSuffix = "_agent.db"

chatDBFunctions :: [SQLiteFuncDef]
chatDBFunctions =
  [ SQLiteFuncDef "migrate_relations_vector" 3 (SQLiteAggrPtrs sqliteMemberRelationsStepPtr sqliteMemberRelationsFinalPtr)
  ]

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts fp key = do
  dbFilePrefix <- peekCString fp
  dbKey <- BA.convert <$> B.packCString key
  pure $
    ChatDbOpts
      { dbFilePrefix,
        dbKey,
        trackQueries = TQSlow 5000, -- 5ms
        vacuumOnMigration = True
      }

-- used to create new chat controller,
-- at that point database is already opened, and the key in options is not used
removeDbKey :: ChatDbOpts -> ChatDbOpts
removeDbKey opts = opts {dbKey = ""} :: ChatDbOpts

errorDbStr :: DBOpts -> String
errorDbStr DBOpts {dbFilePath} = dbFilePath
