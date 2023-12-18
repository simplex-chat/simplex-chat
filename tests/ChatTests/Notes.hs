{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Notes where

import ChatClient
import ChatTests.Utils
import Test.Hspec

chatNotesTests :: SpecWith FilePath
chatNotesTests = do
  fdescribe "notes folders" $ do
    it "create folders, add notes, read, search" testNotes
    it "switch users" testUserNotes

testNotes :: FilePath -> IO ()
testNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  alice ##> "/create notes folder self"
  alice <## "notes folder created, use *self to add notes"

  alice ##> "*self keep in mind"
  alice <## "ok"

testUserNotes :: FilePath -> IO ()
testUserNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  alice ##> "/create notes folder self"
  alice <## "notes folder created, use *self to add notes"

  alice ##> "*self keep in mind"
  alice <## "ok"
  alice ##> "/contacts"
  alice <## "*self"

  alice ##> "/create user secret"
  alice <## "user profile: secret"
  alice <## "use /p <display name> to change it"
  alice <## "(the updated profile will be sent to all your contacts)"

  alice ##> "/contacts"
