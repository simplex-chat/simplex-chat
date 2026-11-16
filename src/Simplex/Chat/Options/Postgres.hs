{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options.Postgres where

import Control.Monad (void)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlphaNum, isSpace)
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

migrationBackupPathP :: Parser (Maybe FilePath)
migrationBackupPathP = pure Nothing

dbString :: ChatDbOpts -> String
dbString ChatDbOpts {dbConnstr} = either (const "(unreadable connection string)") B.unpack $ A.parseOnly (redactedConnstrP <* A.endOfInput) (B.pack dbConnstr)

redactedConnstrP :: A.Parser ByteString
redactedConnstrP = uriP <|> keywordsP
  where
    uriP = do
      scheme <- "postgresql://" <|> "postgres://"
      authority <- A.takeWhile (\c -> c /= '/' && c /= '?')
      path <- A.takeWhile (/= '?')
      query_ <- optional (A.char '?' *> A.takeByteString)
      pure $ scheme <> maskedUserInfo authority <> path <> maybe "" (("?" <>) . B.intercalate "&" . map maskedParam . B.split '&') query_
    maskedUserInfo authority =
      let (userInfo, hosts) = B.breakEnd (== '@') authority
          (user, password) = B.break (== ':') userInfo
       in if B.null password then authority else user <> ":***@" <> hosts
    maskedParam param
      | B.takeWhile (/= '=') param == "password" = "password=***"
      | otherwise = param
    keywordsP = B.unwords <$> (A.skipSpace *> A.many' (keywordP <* A.skipSpace))
    keywordP = do
      (setting, keyword) <- A.match (A.takeWhile1 (\c -> isAlphaNum c || c == '_') <* A.skipSpace <* A.char '=' <* A.skipSpace <* valueP)
      pure $ if keyword == "password" then "password=***" else setting
    valueP = void (A.char '\'' *> A.skipMany (A.char '\\' *> A.anyChar <|> A.notChar '\'') *> A.char '\'') <|> void (A.takeWhile1 (not . isSpace))

toDBOpts :: ChatDbOpts -> String -> Bool -> [()] -> DBOpts
toDBOpts ChatDbOpts {dbConnstr, dbSchemaPrefix, dbPoolSize, dbCreateSchema} dbSuffix _keepKey _dbFunctions =
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

chatDBFunctions :: [()]
chatDBFunctions = []

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
