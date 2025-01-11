{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Options.Postgres where

import Options.Applicative

data ChatDbOpts = ChatDbOpts
  { dbName :: String,
    dbUser :: String,
    dbSchemaPrefix :: String
  }

chatDbOptsP :: FilePath -> String -> Parser ChatDbOpts
chatDbOptsP _appDir defaultDbName = do
  dbName <-
    strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_NAME"
          <> help "Database name"
          <> value defaultDbName
          <> showDefault
      )
  dbUser <-
    strOption
      ( long "database-user"
          <> short 'u'
          <> metavar "DB_USER"
          <> help "Database user"
          <> value "simplex"
          <> showDefault
      )
  pure ChatDbOpts {dbName, dbUser, dbSchemaPrefix = ""}

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbName} = dbName
