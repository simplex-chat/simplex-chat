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
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import Simplex.Messaging.Agent.Store.SQLite (closeSQLiteStore, createSQLiteStore)
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
import Simplex.Messaging.Util (ifM, whenM)
import System.Directory (doesFileExist, removeFile)
import System.Process (readCreateProcess, shell)
import Test.Hspec

testDB :: FilePath
testDB = "tests/tmp/test_chat.db"

appSchema :: FilePath
appSchema = "src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql"

-- Some indexes found by `.lint fkey-indexes` are not added to schema, explanation:
--
-- - CREATE INDEX 'chat_items_group_id' ON 'chat_items'('group_id'); --> groups(group_id)
--
--   Covering index is used instead. See for example:
--   EXPLAIN QUERY PLAN DELETE FROM groups;
--   (uses idx_chat_items_groups_item_status)
--
-- - CREATE INDEX 'connections_group_member_id' ON 'connections'('group_member_id'); --> group_members(group_member_id)
--
--   Covering index is used instead. See for example:
--   EXPLAIN QUERY PLAN DELETE FROM group_members;
--   (uses idx_connections_group_member)
appLint :: FilePath
appLint = "src/Simplex/Chat/Store/SQLite/Migrations/chat_lint.sql"

testSchema :: FilePath
testSchema = "tests/tmp/test_agent_schema.sql"

schemaDumpTest :: Spec
schemaDumpTest = do
  it "verify and overwrite schema dump" testVerifySchemaDump
  it "verify .lint fkey-indexes" testVerifyLintFKeyIndexes
  it "verify schema down migrations" testSchemaMigrations

testVerifySchemaDump :: IO ()
testVerifySchemaDump = withTmpFiles $ do
  savedSchema <- ifM (doesFileExist appSchema) (readFile appSchema) (pure "")
  savedSchema `deepseq` pure ()
  void $ createChatStore testDB "" False MCError
  getSchema testDB appSchema `shouldReturn` savedSchema
  removeFile testDB

testVerifyLintFKeyIndexes :: IO ()
testVerifyLintFKeyIndexes = withTmpFiles $ do
  savedLint <- ifM (doesFileExist appLint) (readFile appLint) (pure "")
  savedLint `deepseq` pure ()
  void $ createChatStore testDB "" False MCError
  getLintFKeyIndexes testDB "tests/tmp/chat_lint.sql" `shouldReturn` savedLint
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
    "20231215_recreate_msg_deliveries",
    -- on down migration idx_msg_deliveries_agent_ack_cmd_id index moves down to the end of the file
    "20240313_drop_agent_ack_cmd_id",
    -- sequence table moves down to the end of the file
    "20241023_chat_item_autoincrement_id",
    -- indexes move down to the end of the file
    "20241125_indexes"
  ]

getSchema :: FilePath -> FilePath -> IO String
getSchema dbPath schemaPath = do
  void $ readCreateProcess (shell $ "sqlite3 " <> dbPath <> " '.schema --indent' > " <> schemaPath) ""
  sch <- readFile schemaPath
  sch `deepseq` pure sch

getLintFKeyIndexes :: FilePath -> FilePath -> IO String
getLintFKeyIndexes dbPath lintPath = do
  void $ readCreateProcess (shell $ "sqlite3 " <> dbPath <> " '.lint fkey-indexes' > " <> lintPath) ""
  lint <- readFile lintPath
  lint `deepseq` pure lint
