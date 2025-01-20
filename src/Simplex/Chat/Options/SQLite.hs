{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options.SQLite where

import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import Options.Applicative
import Simplex.Messaging.Agent.Store.Interface (DBOpts (..))
import System.FilePath (combine)

data ChatDbOpts = ChatDbOpts
  { dbFilePrefix :: String,
    dbKey :: ScrubbedBytes,
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
  pure ChatDbOpts {dbFilePrefix, dbKey, vacuumOnMigration = not disableVacuum}

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbFilePrefix} = dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"

toDBOpts :: ChatDbOpts -> String -> Bool -> DBOpts
toDBOpts ChatDbOpts {dbFilePrefix, dbKey, vacuumOnMigration} dbSuffix keepKey = do
  DBOpts
    { dbFilePath = dbFilePrefix <> dbSuffix,
      dbKey,
      keepKey,
      vacuum = vacuumOnMigration
    }

chatSuffix :: String
chatSuffix = "_chat.db"

agentSuffix :: String
agentSuffix = "_agent.db"

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts fp key = do
  dbFilePrefix <- peekCString fp
  dbKey <- BA.convert <$> B.packCString key
  pure $
    ChatDbOpts
      { dbFilePrefix,
        dbKey,
        vacuumOnMigration = True
      }

-- used to create new chat controller,
-- at that point database is already opened, and the key in options is not used
removeDbKey :: ChatDbOpts -> ChatDbOpts
removeDbKey ChatDbOpts {dbFilePrefix, vacuumOnMigration} =
  ChatDbOpts
    { dbFilePrefix,
      dbKey = "",
      vacuumOnMigration
    }

errorDbStr :: DBOpts -> String
errorDbStr DBOpts {dbFilePath} = dbFilePath
