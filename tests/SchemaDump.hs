{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchemaDump where

import ChatClient (withTmpFiles)
import ChatTests.DBUtils
import Control.Concurrent.STM
import Control.DeepSeq
import qualified Control.Exception as E
import Control.Monad (unless, void)
import Data.List (dropWhileEnd, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple (Query (..))
import Simplex.Chat.Store (createChatStore)
import qualified Simplex.Chat.Store as Store
import Simplex.Messaging.Agent.Env.SQLite (createAgentStore)
import Simplex.Messaging.Agent.Store.Common (withConnection)
import Simplex.Messaging.Agent.Store.DB (TrackQueries (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import Simplex.Messaging.Util (ifM, tshow, whenM)
import System.Directory (doesFileExist, removeFile)
import System.Process (readCreateProcess, shell)
import Test.Hspec

testDB :: FilePath
testDB = "tests/tmp/test_chat.db"

testAgentDB :: FilePath
testAgentDB = "tests/tmp/test_agent.db"

appSchema :: FilePath
appSchema = "src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql"

appLint :: FilePath
appLint = "src/Simplex/Chat/Store/SQLite/Migrations/chat_lint.sql"

appChatQueryPlans :: FilePath
appChatQueryPlans = "src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt"

appAgentQueryPlans :: FilePath
appAgentQueryPlans = "src/Simplex/Chat/Store/SQLite/Migrations/agent_query_plans.txt"

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
  void $ createChatStore (DBOpts testDB "" False True TQOff) MCError
  getSchema testDB appSchema `shouldReturn` savedSchema
  removeFile testDB

testVerifyLintFKeyIndexes :: IO ()
testVerifyLintFKeyIndexes = withTmpFiles $ do
  savedLint <- ifM (doesFileExist appLint) (readFile appLint) (pure "")
  savedLint `deepseq` pure ()
  void $ createChatStore (DBOpts testDB "" False True TQOff) MCError
  getLintFKeyIndexes testDB "tests/tmp/chat_lint.sql" `shouldReturn` savedLint
  removeFile testDB

testSchemaMigrations :: IO ()
testSchemaMigrations = withTmpFiles $ do
  let noDownMigrations = dropWhileEnd (\Migration {down} -> isJust down) Store.migrations
  Right st <- createDBStore (DBOpts testDB "" False True TQOff) noDownMigrations MCError
  mapM_ (testDownMigration st) $ drop (length noDownMigrations) Store.migrations
  closeDBStore st
  removeFile testDB
  whenM (doesFileExist testSchema) $ removeFile testSchema
  where
    testDownMigration st m = do
      putStrLn $ "down migration " <> name m
      let downMigr = fromJust $ toDownMigration m
      schema <- getSchema testDB testSchema
      Migrations.run st True $ MTRUp [m]
      schema' <- getSchema testDB testSchema
      unless (name m `elem` skipComparisonForUpMigrations) $
        schema' `shouldNotBe` schema
      Migrations.run st True $ MTRDown [downMigr]
      unless (name m `elem` skipComparisonForDownMigrations) $ do
        schema'' <- getSchema testDB testSchema
        schema'' `shouldBe` schema
      Migrations.run st True $ MTRUp [m]
      schema''' <- getSchema testDB testSchema
      schema''' `shouldBe` schema'

skipComparisonForUpMigrations :: [String]
skipComparisonForUpMigrations =
  [ -- schema doesn't change
    "20250129_delete_unused_contacts"
  ]

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
    "20241125_indexes",
    -- indexes move down to the end of the file
    "20250130_indexes",
    -- index moves down to the end of the file
    "20250227_member_acceptance"
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

saveQueryPlans :: SpecWith TestParams
saveQueryPlans = it "verify and overwrite query plans" $ \TestParams {chatQueryStats, agentQueryStats} -> do
  (chatSavedPlans, chatSavedPlans') <-
    updatePlans
      appChatQueryPlans
      chatQueryStats
      (createChatStore (DBOpts testDB "" False True TQOff) MCError)
      (\db -> do
        DB.execute_ db "CREATE TABLE IF NOT EXISTS temp_conn_ids (conn_id BLOB)"
        DB.execute_ db "CREATE TABLE IF NOT EXISTS temp_delete_members (contact_profile_id INTEGER, member_profile_id INTEGER, local_display_name TEXT)"
      )
  (agentSavedPlans, agentSavedPlans') <-
    updatePlans
      appAgentQueryPlans
      agentQueryStats
      (createAgentStore (DBOpts testAgentDB "" False True TQOff) MCError)
      (const $ pure ())
  chatSavedPlans' == chatSavedPlans `shouldBe` True
  agentSavedPlans' == agentSavedPlans `shouldBe` True
  removeFile testDB
  removeFile testAgentDB
  where
    updatePlans plansFile statsSel createStore prepareStore = do
      savedPlans <- ifM (doesFileExist plansFile) (T.readFile plansFile) (pure "")
      savedPlans `deepseq` pure ()
      queries <- sort . M.keys <$> readTVarIO statsSel
      Right st <- createStore
      plans' <- withConnection st $ \db -> do
        void $ prepareStore db
        mapM (getQueryPlan db) queries
      let savedPlans' = T.unlines plans'
      T.writeFile plansFile savedPlans'
      pure (savedPlans, savedPlans')
    getQueryPlan :: DB.Connection -> Query -> IO Text
    getQueryPlan db q =
      (("Query: " <> fromQuery q) <>) . result <$> E.try (DB.query_ db $ "explain query plan " <> q)
    result = \case
      Right r -> "\nPlan:\n" <> T.unlines (map planDetail r)
      Left (e :: E.SomeException) -> "\nError: " <> tshow e <> "\n"
    planDetail :: (Int, Int, Int, Text) -> Text
    planDetail (_, _, _, detail) = detail
