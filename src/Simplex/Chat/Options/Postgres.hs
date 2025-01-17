{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Options.Postgres where

import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts (..))
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

toDBCreateOpts :: ChatDbOpts -> Bool -> (DBCreateOpts, DBCreateOpts)
toDBCreateOpts ChatDbOpts {dbConnstr, dbSchemaPrefix} _keepKey = do
  let agentDbOpts =
        DBCreateOpts
          { connstr = B.pack dbConnstr,
            schema = agentSchema dbSchemaPrefix
          }
  let chatDbOpts =
        DBCreateOpts
          { connstr = B.pack dbConnstr,
            schema = chatSchema dbSchemaPrefix
          }
  (agentDbOpts, chatDbOpts)
