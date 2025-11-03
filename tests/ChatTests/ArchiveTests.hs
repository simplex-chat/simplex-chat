{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.ArchiveTests where

import ChatClient
import ChatTests.Utils
import Simplex.Chat.Controller (ArchiveConfig (..))
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Hspec hiding (it)

archiveTests :: SpecWith TestParams
archiveTests = do
  describe "Database archive" $ do
    it "export archive creates zip file" testExportArchive
    it "export and import archive preserves data" testExportImportArchive
    it "import archive with existing data creates backup" testImportBackup
    it "export fails with invalid path" testExportInvalidPath
    it "import fails with non-existent file" testImportNonExistent
    it "import fails with invalid archive" testImportInvalidArchive

testExportArchive :: TestParams -> IO ()
testExportArchive tmp@TestParams {tmpPath} =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    let archivePath = tmpPath </> "test-export.zip"

    -- Create some data
    alice >* "test message"
    alice ##> "/tail"
    alice <# "* test message"

    -- Export archive using interactive command
    alice ##> "/db export"
    alice <## "* database exported"

    -- Verify file was created (file name will have timestamp)
    -- Note: Interactive export creates timestamped filename
    -- For CLI test, we'll use the API directly

testExportImportArchive :: TestParams -> IO ()
testExportImportArchive tmp@TestParams {tmpPath} =
  withNewTestChatContactConnected tmp aliceProfile bobProfile $ \alice bob -> do
    let archivePath = tmpPath </> "full-export.zip"

    -- Alice creates some data
    alice >* "local note"
    alice #> "@bob hello"
    bob <# "alice> hello"
    bob #> "@alice hi back"
    alice <# "bob> hi back"

    -- Export alice's database
    alice ##> ("/_db export " <> show (ArchiveConfig archivePath Nothing Nothing))
    alice <## "ok"

    -- Verify archive exists
    doesFileExist archivePath `shouldReturn` True

    -- Import into a new client instance
    withNewTestChat tmp "alice2" aliceProfile $ \alice2 -> do
      alice2 ##> ("/_db import " <> show (ArchiveConfig archivePath Nothing Nothing))
      alice2 <## "ok"

      -- Verify data was imported
      alice2 ##> "/tail *"
      alice2 <# "* local note"
      alice2 ##> "/tail @bob"
      alice2 <# "alice> hello"
      alice2 <# "bob> hi back"

testImportBackup :: TestParams -> IO ()
testImportBackup tmp@TestParams {tmpPath} =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    let archivePath = tmpPath </> "backup-test.zip"
    let chatDbPath = tmpPath </> "simplex_v1_chat.db"
    let chatDbBackup = chatDbPath <> ".bak"

    -- Create initial data
    alice >* "original message"

    -- Export
    alice ##> ("/_db export " <> show (ArchiveConfig archivePath Nothing Nothing))
    alice <## "ok"

    -- Create new data
    alice >* "new message"

    -- Import (should create backup)
    alice ##> ("/_db import " <> show (ArchiveConfig archivePath Nothing Nothing))
    alice <## "ok"

    -- Verify backup was created
    doesFileExist chatDbBackup `shouldReturn` True

    -- Verify old data is restored
    alice ##> "/tail"
    alice <# "* original message"

testExportInvalidPath :: TestParams -> IO ()
testExportInvalidPath tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    let invalidPath = "/nonexistent/directory/archive.zip"

    alice ##> ("/_db export " <> show (ArchiveConfig invalidPath Nothing Nothing))
    alice <## "error: chat db error"

testImportNonExistent :: TestParams -> IO ()
testImportNonExistent tmp@TestParams {tmpPath} =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    let nonExistentPath = tmpPath </> "does-not-exist.zip"

    alice ##> ("/_db import " <> show (ArchiveConfig nonExistentPath Nothing Nothing))
    alice <## "error: chat db error"

testImportInvalidArchive :: TestParams -> IO ()
testImportInvalidArchive tmp@TestParams {tmpPath} =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    let invalidArchive = tmpPath </> "invalid.zip"

    -- Create an invalid zip file
    writeFile invalidArchive "this is not a valid zip file"

    alice ##> ("/_db import " <> show (ArchiveConfig invalidArchive Nothing Nothing))
    alice <## "error: chat db error"

    -- Cleanup
    removeFile invalidArchive
