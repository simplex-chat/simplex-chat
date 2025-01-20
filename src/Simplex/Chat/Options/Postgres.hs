{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Options.Postgres where

import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Store.Interface (DBOpts (..))
import Simplex.Chat.Store

data ChatDbOpts = ChatDbOpts
  { dbConnstr :: String,
    dbSchemaPrefix :: String
  }

chatDbOptsP :: FilePath -> String -> Parser ChatDbOpts
chatDbOptsP _appDir defaultDbName = do
  dbConnstr <-
    strOption
      ( long "database-connstr"
          <> short 'd'
          <> metavar "DB_CONNSTR"
          <> help "Database connection string"
          <> value ("postgresql://simplex@/" <> defaultDbName)
          <> showDefault
      )
  dbSchemaPrefix <-
    strOption
      ( long "database-schema-prefix"
          <> metavar "DB_SCHEMA_PREFIX"
          <> help "Database schema prefix"
          <> value "simplex_v1"
          <> showDefault
      )
  pure ChatDbOpts {dbConnstr, dbSchemaPrefix}

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbConnstr} = dbConnstr

toDBOpts :: ChatDbOpts -> String -> Bool -> DBOpts
toDBOpts ChatDbOpts {dbConnstr, dbSchemaPrefix} dbSuffix _keepKey =
  DBOpts
    { connstr = B.pack dbConnstr,
      schema = if null dbSchemaPrefix then "simplex_v1" <> dbSuffix else dbSchemaPrefix <> dbSuffix
    }
