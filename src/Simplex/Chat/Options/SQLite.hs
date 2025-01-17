{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options.SQLite where

import Data.ByteArray (ScrubbedBytes)
import Options.Applicative
import Simplex.Chat.Store
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts (..))
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

toDBCreateOpts :: ChatDbOpts -> Bool -> (DBCreateOpts, DBCreateOpts)
toDBCreateOpts ChatDbOpts {dbFilePrefix, dbKey, vacuumOnMigration} keepKey = do
  let agentDbOpts =
        DBCreateOpts
          { dbFilePath = agentStoreFile dbFilePrefix,
            dbKey,
            keepKey,
            vacuum = vacuumOnMigration
          }
  let chatDbOpts =
        DBCreateOpts
          { dbFilePath = chatStoreFile dbFilePrefix,
            dbKey,
            keepKey,
            vacuum = vacuumOnMigration
          }
  (agentDbOpts, chatDbOpts)
