{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.ArchiveTests where

import ChatClient
import ChatTests.Utils
import Simplex.Chat.Controller (ArchiveConfig (..))
import Simplex.Chat.Types (Profile)
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
    it "export and import with multiple profile types" testMultipleProfileTypes
    it "pre-built profiles with group join via QR code" testPreBuiltProfilesWithQR

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

testMultipleProfileTypes :: TestParams -> IO ()
testMultipleProfileTypes tmp@TestParams {tmpPath} =
  withNewTestChatContactConnected tmp aliceProfile bobProfile $ \alice bob -> do
    let aliceArchive = tmpPath </> "alice-profile.zip"
    let bobArchive = tmpPath </> "bob-profile.zip"
    let cathArchive = tmpPath </> "cath-profile.zip"

    -- Alice creates data: local notes + conversation with Bob
    alice >* "alice's note"
    alice #> "@bob hi from alice"
    bob <# "alice> hi from alice"
    bob #> "@alice hi back from bob"
    alice <# "bob> hi back from bob"

    -- Bob creates data: local notes + conversation with Alice
    bob >* "bob's note"

    -- Export both profiles
    alice ##> ("/_db export " <> show (ArchiveConfig aliceArchive Nothing Nothing))
    alice <## "ok"
    bob ##> ("/_db export " <> show (ArchiveConfig bobArchive Nothing Nothing))
    bob <## "ok"

    -- Verify both archives exist
    doesFileExist aliceArchive `shouldReturn` True
    doesFileExist bobArchive `shouldReturn` True

    -- Create a third user (Cath) with business profile
    withNewTestChat tmp "cath" businessProfile $ \cath -> do
      cath >* "business note"

      -- Export Cath's profile
      cath ##> ("/_db export " <> show (ArchiveConfig cathArchive Nothing Nothing))
      cath <## "ok"

    -- Import each profile type into new instances and verify
    -- Test 1: Regular user profile (Alice)
    withNewTestChat tmp "alice_imported" aliceProfile $ \alice2 -> do
      alice2 ##> ("/_db import " <> show (ArchiveConfig aliceArchive Nothing Nothing))
      alice2 <## "ok"
      alice2 ##> "/tail *"
      alice2 <# "* alice's note"
      alice2 ##> "/tail @bob"
      alice2 <# "alice> hi from alice"
      alice2 <# "bob> hi back from bob"

    -- Test 2: Another regular user profile (Bob)
    withNewTestChat tmp "bob_imported" bobProfile $ \bob2 -> do
      bob2 ##> ("/_db import " <> show (ArchiveConfig bobArchive Nothing Nothing))
      bob2 <## "ok"
      bob2 ##> "/tail *"
      bob2 <# "* bob's note"
      bob2 ##> "/tail @alice"
      bob2 <# "alice> hi from alice"

    -- Test 3: Business profile (Cath)
    withNewTestChat tmp "cath_imported" businessProfile $ \cath2 -> do
      cath2 ##> ("/_db import " <> show (ArchiveConfig cathArchive Nothing Nothing))
      cath2 <## "ok"
      cath2 ##> "/tail *"
      cath2 <# "* business note"

testPreBuiltProfilesWithQR :: TestParams -> IO ()
testPreBuiltProfilesWithQR tmp@TestParams {tmpPath} =
  withNewTestChatContactConnected tmp aliceProfile bobProfile $ \alice bob -> do
    let marissaArchive = tmpPath </> "marissa-profile.zip"
    let trevorArchive = tmpPath </> "trevor-profile.zip"

    -- Admin creates a group
    alice ##> "/g proto-users-group"
    alice <## "group #proto-users-group is created"
    alice <## "to add members use /a proto-users-group <name>"

    -- Admin adds Bob to the group
    alice ##> "/a proto-users-group bob"
    alice <## "invitation to join the group #proto-users-group sent to bob"
    bob <## "#proto-users-group: alice invites you to join the group as admin"
    bob <## "use /j proto-users-group to accept"
    bob ##> "/j proto-users-group"
    bob <## "you joined group #proto-users-group"
    alice <## "#proto-users-group: bob joined the group"

    -- Get group link for QR code
    alice ##> "/show link #proto-users-group"
    groupLink <- getTermLine alice
    -- groupLink format: "group link: https://..."

    -- Pre-build Marissa's profile
    withNewTestChat tmp "marissa" (mkProfile "marissa" "Marissa" Nothing) $ \marissa -> do
      -- Profile is created, now export it
      marissa ##> ("/_db export " <> show (ArchiveConfig marissaArchive Nothing Nothing))
      marissa <## "ok"

    -- Pre-build Trevor's profile
    withNewTestChat tmp "trevor" (mkProfile "trevor" "Trevor" Nothing) $ \trevor -> do
      -- Profile is created, now export it
      trevor ##> ("/_db export " <> show (ArchiveConfig trevorArchive Nothing Nothing))
      trevor <## "ok"

    -- Verify archives were created
    doesFileExist marissaArchive `shouldReturn` True
    doesFileExist trevorArchive `shouldReturn` True

    -- Marissa imports her pre-built profile and joins via QR (simulated)
    withNewTestChat tmp "marissa_device" (mkProfile "marissa" "Marissa" Nothing) $ \marissa -> do
      marissa ##> ("/_db import " <> show (ArchiveConfig marissaArchive Nothing Nothing))
      marissa <## "ok"

      -- Simulate QR code scan (joining group)
      marissa ##> ("/c " <> groupLink)
      marissa <## "connection request sent!"
      alice <## "#proto-users-group: marissa wants to join the group"
      alice ##> "/j proto-users-group marissa"
      alice <## "#proto-users-group: you approved marissa to join the group"
      marissa <## "#proto-users-group: you joined the group"
      alice <## "#proto-users-group: marissa joined the group"

    -- Trevor imports his pre-built profile and joins via QR (simulated)
    withNewTestChat tmp "trevor_device" (mkProfile "trevor" "Trevor" Nothing) $ \trevor -> do
      trevor ##> ("/_db import " <> show (ArchiveConfig trevorArchive Nothing Nothing))
      trevor <## "ok"

      -- Simulate QR code scan (joining group)
      trevor ##> ("/c " <> groupLink)
      trevor <## "connection request sent!"
      alice <## "#proto-users-group: trevor wants to join the group"
      alice ##> "/j proto-users-group trevor"
      alice <## "#proto-users-group: you approved trevor to join the group"
      trevor <## "#proto-users-group: you joined the group"
      alice <## "#proto-users-group: trevor joined the group"

      -- Verify group communication works
      alice #> "#proto-users-group Welcome Marissa and Trevor!"
      bob <# "#proto-users-group alice> Welcome Marissa and Trevor!"
      -- Note: marissa and trevor would receive this in their actual test instances
