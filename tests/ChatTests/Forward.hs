{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Forward where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate)
import qualified Data.Text as T
import Simplex.Chat.Library.Commands (fixedImagePreview)
import Simplex.Chat.Types (ImageData (..))
import System.Directory (copyFile, doesFileExist, removeFile)
import Test.Hspec hiding (it)

chatForwardTests :: SpecWith TestParams
chatForwardTests = do
  describe "forward messages" $ do
    it "from contact to contact" testForwardContactToContact
    it "from contact to group" testForwardContactToGroup
    it "from contact to notes" testForwardContactToNotes
    it "from group to contact" testForwardGroupToContact
    it "from group to group" testForwardGroupToGroup
    it "from group to notes" testForwardGroupToNotes
    it "from notes to contact" testForwardNotesToContact
    it "from notes to group" testForwardNotesToGroup
    it "from notes to notes" testForwardNotesToNotes -- TODO forward between different folders when supported
  describe "interactions with forwarded messages" $ do
    it "preserve original forward info" testForwardPreserveInfo
    it "received forwarded message is saved with new forward info" testForwardRcvMsgNewInfo
    it "quoted message is not included" testForwardQuotedMsg
    it "editing is prohibited" testForwardEditProhibited
    it "delete for other" testForwardDeleteForOther
  describe "forward files" $ do
    it "from contact to contact" testForwardFileNoFilesFolder
    it "with relative paths: from contact to contact" testForwardFileContactToContact
    it "with relative paths: from group to notes" testForwardFileGroupToNotes
    it "with relative paths: from notes to group" testForwardFileNotesToGroup
  describe "multi forward api" $ do
    it "from contact to contact" testForwardContactToContactMulti
    it "from group to group" testForwardGroupToGroupMulti
    it "with relative paths: multiple files from contact to contact" testMultiForwardFiles

testForwardContactToContact :: HasCallStack => TestParams -> IO ()
testForwardContactToContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      msgId <- lastItemId alice
      bob #> "@alice hey"
      alice <# "bob> hey"

      alice ##> ("/_forward @3 @2 " <> msgId)
      alice <# "@cath <- you @bob"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "@cath <- @bob hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      -- read chat
      alice ##> "/tail @cath 2"
      alice <# "@cath <- you @bob"
      alice <## "      hi"
      alice <# "@cath <- @bob"
      alice <## "      hey"

      cath ##> "/tail @alice 2"
      cath <# "alice> -> forwarded"
      cath <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      -- item info
      alice ##> "/item info @cath hey"
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hey"
      alice <##. "forwarded from: @bob, chat item id:"

testForwardContactToGroup :: HasCallStack => TestParams -> IO ()
testForwardContactToGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      createGroup2 "team" alice cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "#team <- @bob hi"
      alice <# "#team <- you @bob"
      alice <## "      hi"
      cath <# "#team alice> -> forwarded"
      cath <## "      hi"

      alice `send` "#team <- @bob hey"
      alice <# "#team <- @bob"
      alice <## "      hey"
      cath <# "#team alice> -> forwarded"
      cath <## "      hey"

testForwardContactToNotes :: HasCallStack => TestParams -> IO ()
testForwardContactToNotes =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createCCNoteFolder alice
      connectUsers alice bob

      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "* <- @bob hi"
      alice <# "* <- you @bob"
      alice <## "      hi"

      alice `send` "* <- @bob hey"
      alice <# "* <- @bob"
      alice <## "      hey"

testForwardGroupToContact :: HasCallStack => TestParams -> IO ()
testForwardGroupToContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      connectUsers alice cath

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      alice `send` "@cath <- #team hi"
      alice <# "@cath <- you #team"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "@cath <- #team @bob hey"
      alice <# "@cath <- #team"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardGroupToGroup :: HasCallStack => TestParams -> IO ()
testForwardGroupToGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      createGroup2 "club" alice cath

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      threadDelay 1000000

      alice `send` "#club <- #team hi"
      alice <# "#club <- you #team"
      alice <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"

      threadDelay 1000000

      alice `send` "#club <- #team hey"
      alice <# "#club <- #team"
      alice <## "      hey"
      cath <# "#club alice> -> forwarded"
      cath <## "      hey"

      -- read chat
      alice ##> "/tail #club 2"
      alice <# "#club <- you #team"
      alice <## "      hi"
      alice <# "#club <- #team"
      alice <## "      hey"

      cath ##> "/tail #club 2"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hey"

testForwardGroupToNotes :: HasCallStack => TestParams -> IO ()
testForwardGroupToNotes =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createCCNoteFolder alice
      createGroup2 "team" alice bob

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      alice `send` "* <- #team hi"
      alice <# "* <- you #team"
      alice <## "      hi"

      alice `send` "* <- #team hey"
      alice <# "* <- #team"
      alice <## "      hey"

testForwardNotesToContact :: HasCallStack => TestParams -> IO ()
testForwardNotesToContact =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      connectUsers alice cath

      alice >* "hi"

      alice `send` "@cath <- * hi"
      alice <# "@cath hi"
      cath <# "alice> hi"

testForwardNotesToGroup :: HasCallStack => TestParams -> IO ()
testForwardNotesToGroup =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      createGroup2 "team" alice cath

      alice >* "hi"

      alice `send` "#team <- * hi"
      alice <# "#team hi"
      cath <# "#team alice> hi"

testForwardNotesToNotes :: HasCallStack => TestParams -> IO ()
testForwardNotesToNotes ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    createCCNoteFolder alice

    alice >* "hi"

    alice `send` "* <- * hi"
    alice <# "* hi"

    alice ##> "/tail * 2"
    alice <# "* hi"
    alice <# "* hi"

testForwardPreserveInfo :: HasCallStack => TestParams -> IO ()
testForwardPreserveInfo =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createCCNoteFolder alice
      connectUsers alice bob
      connectUsers alice cath
      createGroup2 "team" alice dan

      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "* <- @bob hey"
      alice <# "* <- @bob"
      alice <## "      hey"

      alice `send` "@cath <- * hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      alice `send` "#team <- @cath hey"
      alice <# "#team <- @bob"
      alice <## "      hey"
      dan <# "#team alice> -> forwarded"
      dan <## "      hey"

testForwardRcvMsgNewInfo :: HasCallStack => TestParams -> IO ()
testForwardRcvMsgNewInfo =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      connectUsers bob dan
      createCCNoteFolder alice
      connectUsers alice bob
      connectUsers alice cath

      dan #> "@bob hey"
      bob <# "dan> hey"

      bob `send` "@alice <- @dan hey"
      bob <# "@alice <- @dan"
      bob <## "      hey"
      alice <# "bob> -> forwarded"
      alice <## "      hey"

      alice `send` "* <- @bob hey"
      alice <# "* <- @bob"
      alice <## "      hey"

      alice `send` "@cath <- * hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardQuotedMsg :: HasCallStack => TestParams -> IO ()
testForwardQuotedMsg =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      bob `send` "> @alice (hi) hey"
      bob <# "@alice > hi"
      bob <## "      hey"
      alice <# "bob> > hi"
      alice <## "      hey"

      alice `send` "@cath <- @bob hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      -- read chat
      alice ##> "/tail @cath 1"
      alice <# "@cath <- @bob"
      alice <## "      hey"

      cath ##> "/tail @alice 1"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardEditProhibited :: HasCallStack => TestParams -> IO ()
testForwardEditProhibited =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath

      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "@cath <- @bob hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      msgId <- lastItemId alice
      alice ##> ("/_update item @3 " <> msgId <> " text hey edited")
      alice <## "cannot update this item"

testForwardDeleteForOther :: HasCallStack => TestParams -> IO ()
testForwardDeleteForOther =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath

      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "@cath <- @bob hey"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      msgId <- lastItemId alice
      alice ##> ("/_delete item @3 " <> msgId <> " broadcast")
      alice <## "message marked deleted"
      cath <# "alice> [marked deleted] hey"

testForwardFileNoFilesFolder :: HasCallStack => TestParams -> IO ()
testForwardFileNoFilesFolder =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      connectUsers alice bob
      connectUsers bob cath

      -- send original file
      alice ##> "/_send @2 json [{\"filePath\": \"./tests/fixtures/test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}]"
      alice <# "@bob hi"
      alice <# "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> hi"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob ##> "/fr 1 ./tests/tmp"
      concurrentlyN_
        [ alice <## "completed uploading file 1 (test.pdf) for bob",
          bob
            <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
                   "started receiving file 1 (test.pdf) from alice"
                 ]
        ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src

      -- forward file
      bob `send` "@cath <- @alice hi"
      bob <# "@cath <- @alice"
      bob <## "      hi"
      bob <# "/f @cath ./tests/tmp/test.pdf"
      bob <## "use /fc 2 to cancel sending"
      cath <# "bob> -> forwarded"
      cath <## "      hi"
      cath <# "bob> sends file test.pdf (266.0 KiB / 272376 bytes)"
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath ##> "/fr 1 ./tests/tmp"
      concurrentlyN_
        [ bob <## "completed uploading file 2 (test.pdf) for cath",
          cath
            <### [ "saving file 1 from bob to ./tests/tmp/test_1.pdf",
                   "started receiving file 1 (test.pdf) from bob"
                 ]
        ]
      cath <## "completed receiving file 1 (test.pdf) from bob"

      dest2 <- B.readFile "./tests/tmp/test_1.pdf"
      dest2 `shouldBe` src

testForwardFileContactToContact :: HasCallStack => TestParams -> IO ()
testForwardFileContactToContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/fixtures" "./tests/tmp/alice_xftp"
      setRelativePaths bob "./tests/tmp/bob_files" "./tests/tmp/bob_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      connectUsers alice bob
      connectUsers bob cath

      -- send original file
      alice ##> "/_send @2 json [{\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}]"
      alice <# "@bob hi"
      alice <# "/f @bob test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> hi"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob ##> "/fr 1"
      concurrentlyN_
        [ alice <## "completed uploading file 1 (test.pdf) for bob",
          bob
            <### [ "saving file 1 from alice to test.pdf",
                   "started receiving file 1 (test.pdf) from alice"
                 ]
        ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/bob_files/test.pdf"
      dest `shouldBe` src

      -- forward file
      bob `send` "@cath <- @alice hi"
      bob <# "@cath <- @alice"
      bob <## "      hi"
      bob <# "/f @cath test_1.pdf"
      bob <## "use /fc 2 to cancel sending"
      cath <# "bob> -> forwarded"
      cath <## "      hi"
      cath <# "bob> sends file test_1.pdf (266.0 KiB / 272376 bytes)"
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath ##> "/fr 1"
      concurrentlyN_
        [ bob <## "completed uploading file 2 (test_1.pdf) for cath",
          cath
            <### [ "saving file 1 from bob to test_1.pdf",
                   "started receiving file 1 (test_1.pdf) from bob"
                 ]
        ]
      cath <## "completed receiving file 1 (test_1.pdf) from bob"

      src2 <- B.readFile "./tests/tmp/bob_files/test_1.pdf"
      src2 `shouldBe` dest
      dest2 <- B.readFile "./tests/tmp/cath_files/test_1.pdf"
      dest2 `shouldBe` src2

      -- deleting original file doesn't delete forwarded file
      checkActionDeletesFile "./tests/tmp/bob_files/test.pdf" $ do
        bob ##> "/clear alice"
        bob <## "alice: all messages are removed locally ONLY"
      fwdFileExists <- doesFileExist "./tests/tmp/bob_files/test_1.pdf"
      fwdFileExists `shouldBe` True

testForwardFileGroupToNotes :: HasCallStack => TestParams -> IO ()
testForwardFileGroupToNotes =
  testChat2 aliceProfile cathProfile $
    \alice cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/fixtures" "./tests/tmp/alice_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      createGroup2 "team" alice cath
      createCCNoteFolder cath

      -- send original file
      alice ##> "/_send #1 json [{\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}]"
      alice <# "#team hi"
      alice <# "/f #team test.pdf"
      alice <## "use /fc 1 to cancel sending"
      cath <# "#team alice> hi"
      cath <# "#team alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath ##> "/fr 1"
      concurrentlyN_
        [ alice <## "completed uploading file 1 (test.pdf) for #team",
          cath
            <### [ "saving file 1 from alice to test.pdf",
                   "started receiving file 1 (test.pdf) from alice"
                 ]
        ]
      cath <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/cath_files/test.pdf"
      dest `shouldBe` src

      -- forward file
      cath `send` "* <- #team hi"
      cath <# "* <- #team"
      cath <## "      hi"
      cath <# "* file 2 (test_1.pdf)"

      dest2 <- B.readFile "./tests/tmp/cath_files/test_1.pdf"
      dest2 `shouldBe` dest

      -- deleting original file doesn't delete forwarded file
      checkActionDeletesFile "./tests/tmp/cath_files/test.pdf" $ do
        cath ##> "/clear #team"
        cath <## "#team: all messages are removed locally ONLY"
      fwdFileExists <- doesFileExist "./tests/tmp/cath_files/test_1.pdf"
      fwdFileExists `shouldBe` True

testForwardFileNotesToGroup :: HasCallStack => TestParams -> IO ()
testForwardFileNotesToGroup =
  testChat2 aliceProfile cathProfile $
    \alice cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/tmp/alice_files" "./tests/tmp/alice_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      copyFile "./tests/fixtures/test.pdf" "./tests/tmp/alice_files/test.pdf"
      createCCNoteFolder alice
      createGroup2 "team" alice cath

      -- create original file
      alice ##> "/_create *1 json [{\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}]"
      alice <# "* hi"
      alice <# "* file 1 (test.pdf)"

      -- forward file
      alice `send` "#team <- * hi"
      alice <# "#team hi"
      alice <# "/f #team test_1.pdf"
      alice <## "use /fc 2 to cancel sending"
      cath <# "#team alice> hi"
      cath <# "#team alice> sends file test_1.pdf (266.0 KiB / 272376 bytes)"
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath ##> "/fr 1"
      concurrentlyN_
        [ alice <## "completed uploading file 2 (test_1.pdf) for #team",
          cath
            <### [ "saving file 1 from alice to test_1.pdf",
                   "started receiving file 1 (test_1.pdf) from alice"
                 ]
        ]
      cath <## "completed receiving file 1 (test_1.pdf) from alice"

      src <- B.readFile "./tests/tmp/alice_files/test.pdf"
      src2 <- B.readFile "./tests/tmp/alice_files/test_1.pdf"
      src2 `shouldBe` src
      dest2 <- B.readFile "./tests/tmp/cath_files/test_1.pdf"
      dest2 `shouldBe` src2

      -- deleting original file doesn't delete forwarded file
      checkActionDeletesFile "./tests/tmp/alice_files/test.pdf" $ do
        alice ##> "/clear *"
        alice <## "notes: all messages are removed"
      fwdFileExists <- doesFileExist "./tests/tmp/alice_files/test_1.pdf"
      fwdFileExists `shouldBe` True

testForwardContactToContactMulti :: HasCallStack => TestParams -> IO ()
testForwardContactToContactMulti =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      msgId1 <- lastItemId alice

      threadDelay 1000000

      bob #> "@alice hey"
      alice <# "bob> hey"
      msgId2 <- lastItemId alice

      alice ##> ("/_forward plan @2 " <> msgId1 <> "," <> msgId2)
      alice <## "all messages can be forwarded"
      alice ##> ("/_forward @3 @2 " <> msgId1 <> "," <> msgId2)
      alice <# "@cath <- you @bob"
      alice <## "      hi"
      alice <# "@cath <- @bob"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardGroupToGroupMulti :: HasCallStack => TestParams -> IO ()
testForwardGroupToGroupMulti =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      createGroup2 "club" alice cath

      threadDelay 1000000

      alice #> "#team hi"
      bob <# "#team alice> hi"
      msgId1 <- lastItemId alice

      threadDelay 1000000

      bob #> "#team hey"
      alice <# "#team bob> hey"
      msgId2 <- lastItemId alice

      alice ##> ("/_forward plan #1 " <> msgId1 <> "," <> msgId2)
      alice <## "all messages can be forwarded"
      alice ##> ("/_forward #2 #1 " <> msgId1 <> "," <> msgId2)
      alice <# "#club <- you #team"
      alice <## "      hi"
      alice <# "#club <- #team"
      alice <## "      hey"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hey"

      -- read chat
      alice ##> "/tail #club 2"
      alice <# "#club <- you #team"
      alice <## "      hi"
      alice <# "#club <- #team"
      alice <## "      hey"

      cath ##> "/tail #club 2"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hey"

testMultiForwardFiles :: HasCallStack => TestParams -> IO ()
testMultiForwardFiles =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/tmp/alice_app_files" "./tests/tmp/alice_xftp"
      copyFile "./tests/fixtures/test.jpg" "./tests/tmp/alice_app_files/test.jpg"
      copyFile "./tests/fixtures/test.pdf" "./tests/tmp/alice_app_files/test.pdf"
      copyFile "./tests/fixtures/test_1MB.pdf" "./tests/tmp/alice_app_files/test_1MB.pdf"
      copyFile "./tests/fixtures/logo.jpg" "./tests/tmp/alice_app_files/logo.jpg"
      setRelativePaths bob "./tests/tmp/bob_app_files" "./tests/tmp/bob_xftp"
      setRelativePaths cath "./tests/tmp/cath_app_files" "./tests/tmp/cath_xftp"
      connectUsers alice bob
      connectUsers bob cath

      threadDelay 1000000

      msgIdZero <- lastItemId bob

      bob #> "@alice hi"
      alice <# "bob> hi"

      -- send original files
      let cm1 = "{\"msgContent\": {\"type\": \"text\", \"text\": \"message without file\"}}"
          ImageData img = fixedImagePreview
          cm2 = "{\"filePath\": \"test.jpg\", \"msgContent\": {\"type\": \"image\", \"image\":\"" <> T.unpack img <> "\", \"text\": \"\"}}"
          cm3 = "{\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"file\", \"text\": \"\"}}"
          cm4 = "{\"filePath\": \"test_1MB.pdf\", \"msgContent\": {\"type\": \"file\", \"text\": \"message with large file\"}}"
          cm5 = "{\"filePath\": \"logo.jpg\", \"msgContent\": {\"type\": \"image\", \"image\":\"" <> T.unpack img <> "\", \"text\": \"\"}}"
      alice ##> ("/_send @2 json [" <> intercalate "," [cm1, cm2, cm3, cm4, cm5] <> "]")

      alice <# "@bob message without file"

      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"

      alice <# "/f @bob test.pdf"
      alice <## "use /fc 2 to cancel sending"

      alice <# "@bob message with large file"
      alice <# "/f @bob test_1MB.pdf"
      alice <## "use /fc 3 to cancel sending"

      alice <# "/f @bob logo.jpg"
      alice <## "use /fc 4 to cancel sending"

      bob <# "alice> message without file"

      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 2 [<dir>/ | <path>] to receive it"

      bob <# "alice> message with large file"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 3 [<dir>/ | <path>] to receive it"

      bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
      bob <## "use /fr 4 [<dir>/ | <path>] to receive it"

      alice <## "completed uploading file 1 (test.jpg) for bob"
      alice <## "completed uploading file 2 (test.pdf) for bob"
      alice <## "completed uploading file 3 (test_1MB.pdf) for bob"
      alice <## "completed uploading file 4 (logo.jpg) for bob"

      -- IDs to forward
      let msgId1 = (read msgIdZero :: Int) + 1
          msgIds = intercalate "," $ map (show . (msgId1 +)) [0 .. 5]
      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "Files can be received: 1, 2, 3, 4"
      bob <## "5 message(s) out of 6 can be forwarded"

      bob ##> "/fr 1"
      bob
        <### [ "saving file 1 from alice to test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 1 (test.jpg) from alice"

      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "Files can be received: 2, 3, 4"
      bob <## "5 message(s) out of 6 can be forwarded"

      bob ##> "/fr 2"
      bob
        <### [ "saving file 2 from alice to test.pdf",
               "started receiving file 2 (test.pdf) from alice"
             ]
      bob <## "completed receiving file 2 (test.pdf) from alice"

      src1 <- B.readFile "./tests/tmp/alice_app_files/test.jpg"
      dest1 <- B.readFile "./tests/tmp/bob_app_files/test.jpg"
      dest1 `shouldBe` src1

      src2 <- B.readFile "./tests/tmp/alice_app_files/test.pdf"
      dest2 <- B.readFile "./tests/tmp/bob_app_files/test.pdf"
      dest2 `shouldBe` src2

      -- forward file
      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "Files can be received: 3, 4"
      bob <## "all messages can be forwarded"
      bob ##> ("/_forward @3 @2 " <> msgIds)

      -- messages printed for bob
      bob <# "@cath <- you @alice"
      bob <## "      hi"

      bob <# "@cath <- @alice"
      bob <## "      message without file"

      bob <# "@cath <- @alice"

      jpgFileName <- T.unpack . T.strip . T.pack <$> getTermLine bob
      bob <# ("/f @cath " <> jpgFileName)
      bob <## "use /fc 5 to cancel sending"

      bob <# "@cath <- @alice"
      bob <## "      test_1.pdf"
      bob <# "/f @cath test_1.pdf"
      bob <## "use /fc 6 to cancel sending"

      bob <# "@cath <- @alice"
      bob <## "      message with large file"

      bob <# "@cath <- @alice"
      bob <## ""

      -- messages printed for cath
      cath <# "bob> -> forwarded"
      cath <## "      hi"

      cath <# "bob> -> forwarded"
      cath <## "      message without file"

      cath <# "bob> -> forwarded"
      cath <## ("      " <> jpgFileName)
      cath <# ("bob> sends file " <> jpgFileName <> " (136.5 KiB / 139737 bytes)")
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath <# "bob> -> forwarded"
      cath <## "      test_1.pdf"
      cath <# "bob> sends file test_1.pdf (266.0 KiB / 272376 bytes)"
      cath <## "use /fr 2 [<dir>/ | <path>] to receive it"

      cath <# "bob> -> forwarded"
      cath <## "      message with large file"

      cath <# "bob> -> forwarded"
      cath <## ""

      -- file transfer
      bob <## ("completed uploading file 5 (" <> jpgFileName <> ") for cath")
      bob <## "completed uploading file 6 (test_1.pdf) for cath"

      cath ##> "/fr 1"
      cath
        <### [ ConsoleString $ "saving file 1 from bob to " <> jpgFileName,
               ConsoleString $ "started receiving file 1 (" <> jpgFileName <> ") from bob"
             ]
      cath <## ("completed receiving file 1 (" <> jpgFileName <> ") from bob")

      cath ##> "/fr 2"
      cath
        <### [ "saving file 2 from bob to test_1.pdf",
               "started receiving file 2 (test_1.pdf) from bob"
             ]
      cath <## "completed receiving file 2 (test_1.pdf) from bob"

      src1B <- B.readFile ("./tests/tmp/bob_app_files/" <> jpgFileName)
      src1B `shouldBe` dest1
      dest1C <- B.readFile ("./tests/tmp/cath_app_files/" <> jpgFileName)
      dest1C `shouldBe` src1B

      src2B <- B.readFile "./tests/tmp/bob_app_files/test_1.pdf"
      src2B `shouldBe` dest2
      dest2C <- B.readFile "./tests/tmp/cath_app_files/test_1.pdf"
      dest2C `shouldBe` src2B

      bob ##> "/fr 3"
      bob
        <### [ "saving file 3 from alice to test_1MB.pdf",
               "started receiving file 3 (test_1MB.pdf) from alice"
             ]
      bob <## "completed receiving file 3 (test_1MB.pdf) from alice"

      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "Files can be received: 4"
      bob <## "all messages can be forwarded"

      bob ##> "/fr 4"
      bob
        <### [ "saving file 4 from alice to logo.jpg",
               "started receiving file 4 (logo.jpg) from alice"
             ]
      bob <## "completed receiving file 4 (logo.jpg) from alice"

      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "all messages can be forwarded"

      removeFile "./tests/tmp/bob_app_files/test_1MB.pdf"
      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "1 file(s) are missing"
      bob <## "all messages can be forwarded"

      removeFile "./tests/tmp/bob_app_files/test.pdf"
      bob ##> ("/_forward plan @2 " <> msgIds)
      bob <## "2 file(s) are missing"
      bob <## "5 message(s) out of 6 can be forwarded"

      -- deleting original file doesn't delete forwarded file
      checkActionDeletesFile "./tests/tmp/bob_app_files/test.jpg" $ do
        bob ##> "/clear alice"
        bob <## "alice: all messages are removed locally ONLY"
      fwdFileExists <- doesFileExist ("./tests/tmp/bob_app_files/" <> jpgFileName)
      fwdFileExists `shouldBe` True
