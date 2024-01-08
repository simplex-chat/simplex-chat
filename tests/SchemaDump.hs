{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaDump where

import ChatClient (withTmpFiles)
import Control.DeepSeq
import Control.Monad (unless, void)
import Data.List (dropWhileEnd)
import Data.Maybe (fromJust, isJust)
import Simplex.Chat.Store (createChatStore)
import qualified Simplex.Chat.Store as Store
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..), closeSQLiteStore, createSQLiteStore)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..), MigrationsToRun (..), toDownMigration)
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
import Simplex.Messaging.Util (ifM, whenM)
import System.Directory (doesFileExist, removeFile)
import System.Process (readCreateProcess, shell)
import Test.Hspec

testDB :: FilePath
testDB = "tests/tmp/test_chat.db"

appSchema :: FilePath
appSchema = "src/Simplex/Chat/Migrations/chat_schema.sql"

testSchema :: FilePath
testSchema = "tests/tmp/test_agent_schema.sql"

schemaDumpTest :: Spec
schemaDumpTest = do
  it "verify and overwrite schema dump" testVerifySchemaDump
  it "verify schema down migrations" testSchemaMigrations

testVerifySchemaDump :: IO ()
testVerifySchemaDump = withTmpFiles $ do
  savedSchema <- ifM (doesFileExist appSchema) (readFile appSchema) (pure "")
  savedSchema `deepseq` pure ()
  void $ createChatStore testDB "" False MCError
  getSchema testDB appSchema `shouldReturn` savedSchema
  removeFile testDB

testSchemaMigrations :: IO ()
testSchemaMigrations = withTmpFiles $ do
  let noDownMigrations = dropWhileEnd (\Migration {down} -> isJust down) Store.migrations
  Right st <- createSQLiteStore testDB "" False noDownMigrations MCError
  mapM_ (testDownMigration st) $ drop (length noDownMigrations) Store.migrations
  closeSQLiteStore st
  removeFile testDB
  whenM (doesFileExist testSchema) $ removeFile testSchema
  where
    testDownMigration st m = do
      putStrLn $ "down migration " <> name m
      let downMigr = fromJust $ toDownMigration m
      schema <- getSchema testDB testSchema
      Migrations.run st $ MTRUp [m]
      schema' <- getSchema testDB testSchema
      schema' `shouldNotBe` schema
      Migrations.run st $ MTRDown [downMigr]
      unless (name m `elem` skipComparisonForDownMigrations) $ do
        schema'' <- getSchema testDB testSchema
        schema'' `shouldBe` schema
      Migrations.run st $ MTRUp [m]
      schema''' <- getSchema testDB testSchema
      schema''' `shouldBe` schema'

skipComparisonForDownMigrations :: [String]
skipComparisonForDownMigrations =
  [ -- on down migration msg_delivery_events table moves down to the end of the file
    "20230504_recreate_msg_delivery_events_cleanup_messages",
    -- on down migration idx_chat_items_timed_delete_at index moves down to the end of the file
    "20230529_indexes",
    -- table and index definitions move down the file, so fields are re-created as not unique
    "20230914_member_probes",
    -- on down migration idx_connections_via_contact_uri_hash index moves down to the end of the file
    "20231019_indexes",
    -- table and indexes move down to the end of the file
    "20231215_recreate_msg_deliveries"
  ]

getSchema :: FilePath -> FilePath -> IO String
getSchema dpPath schemaPath = do
  void $ readCreateProcess (shell $ "sqlite3 " <> dpPath <> " '.schema --indent' > " <> schemaPath) ""
  sch <- readFile schemaPath
  sch `deepseq` pure sch
