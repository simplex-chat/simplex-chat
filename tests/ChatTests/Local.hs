{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Local where

import ChatClient
import ChatTests.Utils
import Test.Hspec

chatLocalTests :: SpecWith FilePath
chatLocalTests = do
  fdescribe "note folders" $ do
    it "create folders, add notes, read, search" testNotes
    it "switch users" testUserNotes

testNotes :: FilePath -> IO ()
testNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  alice ##> "/create note folder self"
  alice <## "note folder created, write to $self to add notes"

  alice ##> "$self keep in mind"
  alice <## "ok"

testUserNotes :: FilePath -> IO ()
testUserNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  alice ##> "/create note folder self"
  alice <## "note folder created, write to $self to add notes"

  alice ##> "$self keep in mind"
  alice <## "ok"
  alice ##> "/chats"
  alice <## "$self keep in mind"

  alice ##> "/create user secret"
  alice <## "user profile: secret"
  alice <## "use /p <display name> to change it"
  alice <## "(the updated profile will be sent to all your contacts)"

  alice ##> "/chats"
