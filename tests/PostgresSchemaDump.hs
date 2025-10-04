{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgresSchemaDump (postgresSchemaDumpTest) where

import ChatTests.Utils hiding (it)
import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Monad (unless, void)
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import Data.Maybe (fromJust, isJust)
import Simplex.Messaging.Agent.Store.Postgres (closeDBStore, createDBStore)
import Simplex.Messaging.Agent.Store.Postgres.Common (DBOpts (..))
import qualified Simplex.Messaging.Agent.Store.Postgres.Migrations as Migrations
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import Simplex.Messaging.Util (ifM, whenM)
import System.Directory (doesFileExist, removeFile)
import System.Process (readCreateProcess, shell)
import Test.Hspec

testSchemaPath :: FilePath
testSchemaPath = "tests/tmp/test_schema.sql"

-- copied from simplexmq
postgresSchemaDumpTest :: [Migration] -> [String] -> DBOpts -> FilePath -> Spec
postgresSchemaDumpTest migrations skipComparisonForDownMigrations testDBOpts@DBOpts {connstr, schema = testDBSchema} srcSchemaPath = do
  it "verify and overwrite schema dump" testVerifySchemaDump
  it "verify schema down migrations" testSchemaMigrations
  where
    testVerifySchemaDump = do
      savedSchema <- ifM (doesFileExist srcSchemaPath) (readFile srcSchemaPath) (pure "")
      savedSchema `deepseq` pure ()
      void $ createDBStore testDBOpts migrations MCConsole
      getSchema srcSchemaPath `shouldReturn` savedSchema

    testSchemaMigrations = do
      let noDownMigrations = dropWhileEnd (\Migration {down} -> isJust down) migrations
      st <- createDBStore testDBOpts noDownMigrations MCYesUpDown >>= \case
        Right st -> pure st
        Left e -> error $ show e
      mapM_ (testDownMigration st) $ drop (length noDownMigrations) migrations
      closeDBStore st
      whenM (doesFileExist testSchemaPath) $ removeFile testSchemaPath
      where
        testDownMigration st m = do
          putStrLn $ "down migration " <> name m
          let downMigr = fromJust $ toDownMigration m
          schema <- getSchema testSchemaPath
          Migrations.run st $ MTRUp [m]
          schema' <- getSchema testSchemaPath
          schema' `shouldNotBe` schema
          Migrations.run st $ MTRDown [downMigr]
          unless (name m `elem` skipComparisonForDownMigrations) $ do
            schema'' <- getSchema testSchemaPath
            schema'' `shouldBe` schema
          Migrations.run st $ MTRUp [m]
          schema''' <- getSchema testSchemaPath
          schema''' `shouldBe` schema'

    getSchema :: FilePath -> IO String
    getSchema schemaPath = do
      ci <- envCI
      let cmd =
            ("pg_dump " <> B.unpack connstr <> " --schema " <> B.unpack testDBSchema)
              <> " --schema-only --no-owner --no-privileges --no-acl --no-subscriptions --no-tablespaces > "
              <> schemaPath
      void $ readCreateProcess (shell cmd) ""
      threadDelay 20000
      let sed = (if ci then "sed -i" else "sed -i ''")
      void $ readCreateProcess (shell $ sed <> " '/^--/d' " <> schemaPath) ""
      sch <- readFile schemaPath
      sch `deepseq` pure sch
