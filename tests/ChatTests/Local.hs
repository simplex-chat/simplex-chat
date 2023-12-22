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
  alice ##> "/note folder self"
  alice <## "new note folder created, write to $self to add notes"

  alice #> "$self keep in mind"
  alice ##> "/tail"
  alice <# "$self keep in mind"

  alice ##> "/_delete item $1 1 internal"
  alice <## "message deleted"
  alice ##> "/tail"

testUserNotes :: FilePath -> IO ()
testUserNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  alice ##> "/note folder self"
  alice <## "new note folder created, write to $self to add notes"

  alice #> "$self keep in mind"
  alice ##> "/tail"
  alice <# "$self keep in mind"

  alice ##> "/create user secret"
  alice <## "user profile: secret"
  alice <## "use /p <display name> to change it"
  alice <## "(the updated profile will be sent to all your contacts)"

  alice ##> "/note folder gossip"
  alice <## "new note folder created, write to $gossip to add notes"

  alice ##> "/tail"
