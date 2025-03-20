{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Local where

import ChatClient
import ChatTests.ChatList (getChats_)
import ChatTests.DBUtils
import ChatTests.Utils
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Simplex.Chat.Controller (ChatConfig (..), InlineFilesConfig (..), defaultInlineFilesConfig)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec hiding (it)

chatLocalChatsTests :: SpecWith TestParams
chatLocalChatsTests = do
  describe "note folders" $ do
    it "create folders, add notes, read, search" testNotes
    it "switch users" testUserNotes
    it "preview pagination for notes" testPreviewsPagination
    it "chat pagination" testChatPagination
    it "stores files" testFiles
    it "deleting files does not interfere with other chat types" testOtherFiles
  describe "batch create messages" $ do
    it "create multiple messages api" testCreateMulti
    it "create multiple messages with files" testCreateMultiFiles

testNotes :: TestParams -> IO ()
testNotes ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice

  alice ##> "/contacts"
  -- not a contact

  alice >* "keep in mind"
  alice ##> "/tail"
  alice <# "* keep in mind"
  alice ##> "/chats"
  alice <# "* keep in mind"
  alice ##> "/? keep"
  alice <# "* keep in mind"

  alice #$> ("/_read chat *1", id, "ok")
  alice ##> "/_unread chat *1 on"
  alice <## "ok"

  alice ##> "/_delete item *1 1 internal"
  alice <## "message deleted"
  alice ##> "/tail"
  alice ##> "/chats"

  alice >* "ahoy!"
  alice ##> "/_update item *1 2 text Greetings."
  alice ##> "/tail *"
  alice <# "* Greetings."

testUserNotes :: TestParams -> IO ()
testUserNotes ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice

  alice >* "keep in mind"
  alice ##> "/tail"
  alice <# "* keep in mind"

  alice ##> "/create user secret"
  alice <## "user profile: secret"
  alice <## "use /p <display name> to change it"
  alice <## "(the updated profile will be sent to all your contacts)"

  alice ##> "/tail"

  alice ##> "/_delete item *1 1 internal"
  alice <## "chat db error: SENoteFolderNotFound {noteFolderId = 1}"

testPreviewsPagination :: TestParams -> IO ()
testPreviewsPagination ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice

  tsS <- iso8601Show <$> getCurrentTime
  alice >* "first"
  tsM <- iso8601Show <$> getCurrentTime
  alice >* "last"
  tsE <- iso8601Show <$> getCurrentTime

  -- there's only one folder that got updated after tsM and before tsE
  getChats_ alice "count=3" [("*", "last")]
  getChats_ alice ("after=" <> tsE <> " count=10") []
  getChats_ alice ("after=" <> tsS <> " count=10") [("*", "last")]
  getChats_ alice ("before=" <> tsM <> " count=10") []
  getChats_ alice ("before=" <> tsE <> " count=10") [("*", "last")]
  getChats_ alice ("before=" <> tsS <> " count=10") []

testChatPagination :: TestParams -> IO ()
testChatPagination ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice

  alice >* "hello world"
  alice >* "memento mori"
  alice >* "knock-knock"
  alice >* "who's there?"

  alice #$> ("/_get chat *1 count=100", chat, [(1, "hello world"), (1, "memento mori"), (1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 count=1", chat, [(1, "who's there?")])
  alice #$> ("/_get chat *1 around=2 count=1", chat, [(1, "hello world"), (1, "memento mori"), (1, "knock-knock")])
  alice #$> ("/_get chat *1 around=2 count=3", chat, [(1, "hello world"), (1, "memento mori"), (1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 around=3 count=10", chat, [(1, "hello world"), (1, "memento mori"), (1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 around=4 count=1", chat, [(1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 after=2 count=10", chat, [(1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 after=2 count=2", chat, [(1, "knock-knock"), (1, "who's there?")])
  alice #$> ("/_get chat *1 after=1 count=2", chat, [(1, "memento mori"), (1, "knock-knock")])
  alice #$> ("/_get chat *1 before=3 count=10", chat, [(1, "hello world"), (1, "memento mori")])
  alice #$> ("/_get chat *1 before=3 count=2", chat, [(1, "hello world"), (1, "memento mori")])
  alice #$> ("/_get chat *1 before=4 count=2", chat, [(1, "memento mori"), (1, "knock-knock")])

  alice #$> ("/_get chat *1 count=10 search=k-k", chat, [(1, "knock-knock")])

testFiles :: TestParams -> IO ()
testFiles ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  -- setup
  createCCNoteFolder alice
  let files = "./tests/tmp/app_files"
  alice ##> ("/_files_folder " <> files)
  alice <## "ok"

  -- ui-like upload
  let source = "./tests/fixtures/test.jpg"
  let stored = files </> "test.jpg"
  copyFile source stored
  alice ##> "/_create *1 json [{\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"hi myself\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}]"
  alice <# "* hi myself"
  alice <# "* file 1 (test.jpg)"

  alice ##> "/tail"
  alice <# "* hi myself"
  alice <# "* file 1 (test.jpg)"

  alice ##> "/_get chat *1 count=100"
  r <- chatF <$> getTermLine alice
  r `shouldBe` [((1, "hi myself"), Just "test.jpg")]

  alice ##> "/fs 1"
  alice <## "bad chat command: not supported for local files"

  alice ##> "/fc 1"
  alice <## "chat db error: SELocalFileNoTransfer {fileId = 1}"

  -- one more file
  let stored2 = files </> "another_test.jpg"
  copyFile source stored2
  alice ##> "/_create *1 json [{\"filePath\": \"another_test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}]"
  alice <# "* file 2 (another_test.jpg)"

  alice ##> "/_delete item *1 2 internal"
  alice <## "message deleted"
  doesFileExist stored2 `shouldReturn` False
  doesFileExist stored `shouldReturn` True

  alice ##> "/clear *"
  alice <## "notes: all messages are removed"
  alice ##> "/fs 1"
  alice <## "file 1 not found"
  alice ##> "/tail"
  doesFileExist stored `shouldReturn` False

testOtherFiles :: TestParams -> IO ()
testOtherFiles =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> withXFTPServer $ do
    connectUsers alice bob
    createCCNoteFolder bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"

    alice #> "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    alice <## "completed uploading file 1 (test.jpg) for bob"

    bob ##> "/fr 1"
    bob
      <### [ "saving file 1 from alice to test.jpg",
             "started receiving file 1 (test.jpg) from alice"
           ]
    bob <## "completed receiving file 1 (test.jpg) from alice"

    bob >* "test"
    bob ##> "/tail *"
    bob <# "* test"
    bob ##> "/clear *"
    bob <## "notes: all messages are removed"
    bob ##> "/tail *"
    bob ##> "/fs 1"
    bob <## "receiving file 1 (test.jpg) complete, path: test.jpg"
    doesFileExist "./tests/tmp/test.jpg" `shouldReturn` True
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, sendChunks = 100, receiveChunks = 100}}

testCreateMulti :: TestParams -> IO ()
testCreateMulti ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice

  alice ##> "/_create *1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"test 1\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 2\"}}]"
  alice <# "* test 1"
  alice <# "* test 2"

testCreateMultiFiles :: TestParams -> IO ()
testCreateMultiFiles ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  createCCNoteFolder alice
  alice #$> ("/_files_folder ./tests/tmp/alice_app_files", id, "ok")
  copyFile "./tests/fixtures/test.jpg" "./tests/tmp/alice_app_files/test.jpg"
  copyFile "./tests/fixtures/test.pdf" "./tests/tmp/alice_app_files/test.pdf"

  let cm1 = "{\"msgContent\": {\"type\": \"text\", \"text\": \"message without file\"}}"
      cm2 = "{\"filePath\": \"test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"sending file 1\"}}"
      cm3 = "{\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"sending file 2\"}}"
  alice ##> ("/_create *1 json [" <> cm1 <> "," <> cm2 <> "," <> cm3 <> "]")

  alice <# "* message without file"
  alice <# "* sending file 1"
  alice <# "* file 1 (test.jpg)"
  alice <# "* sending file 2"
  alice <# "* file 2 (test.pdf)"

  doesFileExist "./tests/tmp/alice_app_files/test.jpg" `shouldReturn` True
  doesFileExist "./tests/tmp/alice_app_files/test.pdf" `shouldReturn` True

  alice ##> "/_get chat *1 count=3"
  r <- chatF <$> getTermLine alice
  r `shouldBe` [((1, "message without file"), Nothing), ((1, "sending file 1"), Just "test.jpg"), ((1, "sending file 2"), Just "test.pdf")]
