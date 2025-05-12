{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Options.Postgres where

import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import Options.Applicative
import Numeric.Natural (Natural)
import Simplex.Messaging.Agent.Store.Interface (DBOpts (..))

data ChatDbOpts = ChatDbOpts
  { dbConnstr :: String,
    dbSchemaPrefix :: String,
    dbPoolSize :: Natural,
    dbCreateSchema :: Bool
  }

chatDbOptsP :: FilePath -> String -> Parser ChatDbOpts
chatDbOptsP _appDir defaultDbName = do
  dbConnstr <-
    strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_CONN"
          <> help "Database connection string"
          <> value ("postgresql://simplex@/" <> defaultDbName)
          <> showDefault
      )
  dbSchemaPrefix <-
    strOption
      ( long "schema-prefix"
          <> metavar "DB_SCHEMA_PREFIX"
          <> help "Database schema prefix"
          <> value "simplex_v1"
          <> showDefault
      )
  dbPoolSize <-
    option
      auto
      ( long "pool-size"
          <> metavar "DB_POOL_SIZE"
          <> help "Database connection pool size"
          <> value 1
          <> showDefault
      )
  dbCreateSchema <-
    switch
      ( long "create-schema"
          <> help "Create database schema when it does not exist"
      )
  pure ChatDbOpts {dbConnstr, dbSchemaPrefix, dbPoolSize, dbCreateSchema}

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbConnstr} = dbConnstr

toDBOpts :: ChatDbOpts -> String -> Bool -> DBOpts
toDBOpts ChatDbOpts {dbConnstr, dbSchemaPrefix, dbPoolSize, dbCreateSchema} dbSuffix _keepKey =
  DBOpts
    { connstr = B.pack dbConnstr,
      schema = B.pack $ if null dbSchemaPrefix then "simplex_v1" <> dbSuffix else dbSchemaPrefix <> dbSuffix,
      poolSize = dbPoolSize,
      createSchema = dbCreateSchema
    }

chatSuffix :: String
chatSuffix = "_chat_schema"

agentSuffix :: String
agentSuffix = "_agent_schema"

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts schemaPrefix connstr = do
  dbSchemaPrefix <- peekCString schemaPrefix
  dbConnstr <- peekCString connstr
  pure $
    ChatDbOpts
      { dbConnstr,
        dbSchemaPrefix,
        dbPoolSize = 1,
        dbCreateSchema = True
      }

removeDbKey :: ChatDbOpts -> ChatDbOpts
removeDbKey = id

errorDbStr :: DBOpts -> String
errorDbStr DBOpts {schema} = B.unpack schema
