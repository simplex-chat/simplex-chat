{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Local where

import ChatClient
import ChatTests.Utils
import Test.Hspec
import System.Directory (copyFile)

chatLocalTests :: SpecWith FilePath
chatLocalTests = do
  fdescribe "note folders" $ do
    it "create folders, add notes, read, search" testNotes
    it "switch users" testUserNotes
    it "stores files" testFiles

testNotes :: FilePath -> IO ()
testNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  -- createFolder alice "self"

  alice ##> "/contacts"
  -- not a contact

  alice #> "$notes keep in mind"
  alice ##> "/tail"
  alice <# "$notes keep in mind"
  alice ##> "/chats"
  alice <# "$notes keep in mind"
  alice ##> "/? keep"
  alice <# "$notes keep in mind"

  alice #$> ("/_read chat $1 from=1 to=100", id, "ok")
  alice ##> "/_unread chat $1 on"
  alice <## "ok"

  alice ##> "/_delete item $1 1 internal"
  alice <## "message deleted"
  alice ##> "/tail"
  alice ##> "/chats"

  alice #> "$notes ahoy!"
  alice ##> "/_update item $1 1 text Greetings."
  alice ##> "/tail $notes"
  alice <# "$notes Greetings."

testUserNotes :: FilePath -> IO ()
testUserNotes tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do

  alice #> "$notes keep in mind"
  alice ##> "/tail"
  alice <# "$notes keep in mind"

  alice ##> "/create user secret"
  alice <## "user profile: secret"
  alice <## "use /p <display name> to change it"
  alice <## "(the updated profile will be sent to all your contacts)"

  alice ##> "/tail"

  alice ##> "/_delete item $1 1 internal"
  alice <## "chat db error: SENoteFolderNotFound {noteFolderId = 1}"

testFiles :: FilePath -> IO ()
testFiles tmp = withNewTestChat tmp "alice" aliceProfile $ \alice -> do
  -- createFolder alice "self"

  alice #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
  copyFile "./tests/fixtures/test.jpg" "./tests/tmp/app_files/test.jpg"
  alice ##> "/_create $1 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
  alice <# "$notes file 1 (test.jpg)"
  alice ##> "/tail"
  alice <# "$notes file 1 (test.jpg)"
  alice ##> "/fs 1"
  alice <## "local file 1 (test.jpg)"

  alice ##> "/clear $notes"
  alice ##> "/fs 1"
  alice <## "chat db error: SEChatItemNotFoundByFileId {fileId = 1}"
