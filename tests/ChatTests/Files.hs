{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatTests.Files where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Logger.Simple
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Simplex.Chat (roundedFDCount)
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Mobile.File
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.FileTransfer.Server.Env (XFTPServerConfig (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize)
import Test.Hspec hiding (it)

chatFileTests :: SpecWith FilePath
chatFileTests = do
  describe "messages with files" $ do
    it "send and receive message with file" runTestMessageWithFile
    it "send and receive image" testSendImage
    it "sender marking chat item deleted cancels file" testSenderMarkItemDeleted
    it "files folder: send and receive image" testFilesFoldersSendImage
    it "files folder: sender deleted file" testFilesFoldersImageSndDelete -- TODO add test deleting during upload
    it "files folder: recipient deleted file" testFilesFoldersImageRcvDelete -- TODO add test deleting during download
    it "send and receive image with text and quote" testSendImageWithTextAndQuote
    it "send and receive image to group" testGroupSendImage
    it "send and receive image with text and quote to group" testGroupSendImageWithTextAndQuote
  describe "file transfer over XFTP" $ do
    it "round file description count" $ const testXFTPRoundFDCount
    it "send and receive file" testXFTPFileTransfer
    it "send and receive locally encrypted files" testXFTPFileTransferEncrypted
    it "send and receive file, accepting after upload" testXFTPAcceptAfterUpload
    it "send and receive file in group" testXFTPGroupFileTransfer
    it "delete uploaded file" testXFTPDeleteUploadedFile
    it "delete uploaded file in group" testXFTPDeleteUploadedFileGroup
    it "with relative paths: send and receive file" testXFTPWithRelativePaths
    xit' "continue receiving file after restart" testXFTPContinueRcv
    xit' "receive file marked to receive on chat start" testXFTPMarkToReceive
    it "error receiving file" testXFTPRcvError
    it "cancel receiving file, repeat receive" testXFTPCancelRcvRepeat
    it "should accept file automatically with CLI option" testAutoAcceptFile
    it "should prohibit file transfers in groups based on preference" testProhibitFiles
  describe "file transfer over XFTP without chat items" $ do
    it "send and receive small standalone file" testXFTPStandaloneSmall
    it "send and receive large standalone file" testXFTPStandaloneLarge
    it "send and receive large standalone file using relative paths" testXFTPStandaloneRelativePaths
    xit "removes sent file from server" testXFTPStandaloneCancelSnd -- no error shown in tests
    it "removes received temporary files" testXFTPStandaloneCancelRcv

runTestMessageWithFile :: HasCallStack => FilePath -> IO ()
runTestMessageWithFile = testChat2 aliceProfile bobProfile $ \alice bob -> withXFTPServer $ do
  connectUsers alice bob

  alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
  alice <# "@bob hi, sending a file"
  alice <# "/f @bob ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> hi, sending a file"
  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  alice <## "completed uploading file 1 (test.jpg) for bob"

  bob ##> "/fr 1 ./tests/tmp"
  bob
    <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
           "started receiving file 1 (test.jpg) from alice"
         ]
  bob <## "completed receiving file 1 (test.jpg) from alice"

  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src
  alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, "hi, sending a file"), Just "./tests/fixtures/test.jpg")])
  bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, "hi, sending a file"), Just "./tests/tmp/test.jpg")])

testSendImage :: HasCallStack => FilePath -> IO ()
testSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test.jpg) for bob"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 1 (test.jpg) from alice"

      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "./tests/tmp/test.jpg")])
      -- deleting contact without files folder set should not remove file
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      alice <## "bob (Bob) deleted contact with you"
      fileExists <- doesFileExist "./tests/tmp/test.jpg"
      fileExists `shouldBe` True

testSenderMarkItemDeleted :: HasCallStack => FilePath -> IO ()
testSenderMarkItemDeleted =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test_1MB.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
      alice <# "@bob hi, sending a file"
      alice <# "/f @bob ./tests/fixtures/test_1MB.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> hi, sending a file"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test_1MB.pdf) for bob"

      alice #$> ("/_delete item @2 " <> itemId 1 <> " broadcast", id, "message marked deleted")
      bob <# "alice> [marked deleted] hi, sending a file"

      bob ##> "/fr 1 ./tests/tmp"
      bob <## "file cancelled: test_1MB.pdf"

      bob ##> "/fs 1"
      bob <## "receiving file 1 (test_1MB.pdf) cancelled"

testFilesFoldersSendImage :: HasCallStack => FilePath -> IO ()
testFilesFoldersSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test.jpg"
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

      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/app_files/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "test.jpg")])
      -- deleting contact with files folder set should remove file
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"
        alice <## "bob (Bob) deleted contact with you"

testFilesFoldersImageSndDelete :: HasCallStack => FilePath -> IO ()
testFilesFoldersImageSndDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/tmp/alice_app_files", id, "ok")
      copyFile "./tests/fixtures/test_1MB.pdf" "./tests/tmp/alice_app_files/test_1MB.pdf"
      bob #$> ("/_files_folder ./tests/tmp/bob_app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test_1MB.pdf\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test_1MB.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test_1MB.pdf) for bob"

      bob ##> "/fr 1"
      bob
        <### [ "saving file 1 from alice to test_1MB.pdf",
               "started receiving file 1 (test_1MB.pdf) from alice"
             ]
      bob <## "completed receiving file 1 (test_1MB.pdf) from alice"

      -- deleting contact should remove file
      checkActionDeletesFile "./tests/tmp/alice_app_files/test_1MB.pdf" $ do
        alice ##> "/d bob"
        alice <## "bob: contact is deleted"
        bob <## "alice (Alice) deleted contact with you"
        bob ##> "/fs 1"
        bob <##. "receiving file 1 (test_1MB.pdf) complete"
      checkActionDeletesFile "./tests/tmp/bob_app_files/test_1MB.pdf" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageRcvDelete :: HasCallStack => FilePath -> IO ()
testFilesFoldersImageRcvDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test.jpg"
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

      -- deleting contact should remove file
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"
        alice <## "bob (Bob) deleted contact with you"

testSendImageWithTextAndQuote :: HasCallStack => FilePath -> IO ()
testSendImageWithTextAndQuote =
  testChat2 aliceProfile bobProfile $
    \alice bob -> withXFTPServer $ do
      connectUsers alice bob
      bob #> "@alice hi alice"
      alice <# "bob> hi alice"
      alice ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> itemId 1 <> ", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
      alice <# "@bob > hi alice"
      alice <## "      hey bob"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> > hi alice"
      bob <## "      hey bob"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test.jpg) for bob"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 1 (test.jpg) from alice"

      src <- B.readFile "./tests/fixtures/test.jpg"
      B.readFile "./tests/tmp/test.jpg" `shouldReturn` src
      alice #$> ("/_get chat @2 count=100", chat'', chatFeatures'' <> [((0, "hi alice"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi alice"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("@bob", "hey bob")]
      bob #$> ("/_get chat @2 count=100", chat'', chatFeatures'' <> [((1, "hi alice"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi alice"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("@alice", "hey bob")]

      -- quoting (file + text) with file uses quoted text
      bob ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.pdf\", \"quotedItemId\": " <> itemId 2 <> ", \"msgContent\": {\"text\":\"\",\"type\":\"file\"}}")
      bob <# "@alice > hey bob"
      bob <## "      test.pdf"
      bob <# "/f @alice ./tests/fixtures/test.pdf"
      bob <## "use /fc 2 to cancel sending"
      alice <# "bob> > hey bob"
      alice <## "      test.pdf"
      alice <# "bob> sends file test.pdf (266.0 KiB / 272376 bytes)"
      alice <## "use /fr 2 [<dir>/ | <path>] to receive it"
      bob <## "completed uploading file 2 (test.pdf) for alice"

      alice ##> "/fr 2 ./tests/tmp"
      alice
        <### [ "saving file 2 from bob to ./tests/tmp/test.pdf",
               "started receiving file 2 (test.pdf) from bob"
             ]
      alice <## "completed receiving file 2 (test.pdf) from bob"

      txtSrc <- B.readFile "./tests/fixtures/test.pdf"
      B.readFile "./tests/tmp/test.pdf" `shouldReturn` txtSrc

      -- quoting (file without text) with file uses file name
      alice ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> itemId 3 <> ", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
      alice <# "@bob > test.pdf"
      alice <## "      test.jpg"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 3 to cancel sending"
      bob <# "alice> > test.pdf"
      bob <## "      test.jpg"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 3 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 3 (test.jpg) for bob"

      bob ##> "/fr 3 ./tests/tmp"
      bob
        <### [ "saving file 3 from alice to ./tests/tmp/test_1.jpg",
               "started receiving file 3 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 3 (test.jpg) from alice"

      B.readFile "./tests/tmp/test_1.jpg" `shouldReturn` src

testGroupSendImage :: HasCallStack => FilePath -> IO ()
testGroupSendImage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice ##> "/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice <## "completed uploading file 1 (test.jpg) for #team"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 1 (test.jpg) from alice"

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from alice to ./tests/tmp/test_1.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      cath <## "completed receiving file 1 (test.jpg) from alice"

      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      dest2 <- B.readFile "./tests/tmp/test_1.jpg"
      dest2 `shouldBe` src
      alice #$> ("/_get chat #1 count=1", chatF, [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat #1 count=1", chatF, [((0, ""), Just "./tests/tmp/test.jpg")])
      cath #$> ("/_get chat #1 count=1", chatF, [((0, ""), Just "./tests/tmp/test_1.jpg")])

testGroupSendImageWithTextAndQuote :: HasCallStack => FilePath -> IO ()
testGroupSendImageWithTextAndQuote =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      bob #> "#team hi team"
      concurrently_
        (alice <# "#team bob> hi team")
        (cath <# "#team bob> hi team")
      threadDelay 1000000
      msgItemId <- lastItemId alice
      alice ##> ("/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> msgItemId <> ", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
      alice <# "#team > bob hi team"
      alice <## "      hey bob"
      alice <# "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> > bob hi team"
            bob <## "      hey bob"
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> > bob hi team"
            cath <## "      hey bob"
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice <## "completed uploading file 1 (test.jpg) for #team"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      bob <## "completed receiving file 1 (test.jpg) from alice"

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from alice to ./tests/tmp/test_1.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      cath <## "completed receiving file 1 (test.jpg) from alice"

      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      dest2 <- B.readFile "./tests/tmp/test_1.jpg"
      dest2 `shouldBe` src
      alice #$> ("/_get chat #1 count=2", chat'', [((0, "hi team"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi team"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("#team", "hey bob"), ("@bob", "sent invitation to join group team as admin"), ("@cath", "sent invitation to join group team as admin")]
      bob #$> ("/_get chat #1 count=2", chat'', [((1, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi team"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("#team", "hey bob"), ("@alice", "received invitation to join group team as admin")]
      cath #$> ("/_get chat #1 count=2", chat'', [((0, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (0, "hi team"), Just "./tests/tmp/test_1.jpg")])
      cath @@@ [("#team", "hey bob"), ("@alice", "received invitation to join group team as admin")]

testXFTPRoundFDCount :: Expectation
testXFTPRoundFDCount = do
  roundedFDCount (-100) `shouldBe` 4
  roundedFDCount (-1) `shouldBe` 4
  roundedFDCount 0 `shouldBe` 4
  roundedFDCount 1 `shouldBe` 4
  roundedFDCount 2 `shouldBe` 4
  roundedFDCount 4 `shouldBe` 4
  roundedFDCount 5 `shouldBe` 8
  roundedFDCount 20 `shouldBe` 32
  roundedFDCount 128 `shouldBe` 128
  roundedFDCount 500 `shouldBe` 512

testXFTPFileTransfer :: HasCallStack => FilePath -> IO ()
testXFTPFileTransfer =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
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

      alice ##> "/fs 1"
      alice <## "sending file 1 (test.pdf) complete"
      bob ##> "/fs 1"
      bob <## "receiving file 1 (test.pdf) complete, path: ./tests/tmp/test.pdf"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src

testXFTPFileTransferEncrypted :: HasCallStack => FilePath -> IO ()
testXFTPFileTransferEncrypted =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    src <- B.readFile "./tests/fixtures/test.pdf"
    srcLen <- getFileSize "./tests/fixtures/test.pdf"
    let srcPath = "./tests/tmp/alice/test.pdf"
    createDirectoryIfMissing True "./tests/tmp/alice/"
    createDirectoryIfMissing True "./tests/tmp/bob/"
    WFResult cfArgs <- chatWriteFile (chatController alice) srcPath src
    let fileJSON = LB.unpack $ J.encode $ CryptoFile srcPath $ Just cfArgs
    withXFTPServer $ do
      connectUsers alice bob
      alice ##> ("/_send @2 json {\"msgContent\":{\"type\":\"file\", \"text\":\"\"}, \"fileSource\": " <> fileJSON <> "}")
      alice <# "/f @bob ./tests/tmp/alice/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 encrypt=on ./tests/tmp/bob/"
      bob <## "saving file 1 from alice to ./tests/tmp/bob/test.pdf"
      alice <## "completed uploading file 1 (test.pdf) for bob"
      bob <## "started receiving file 1 (test.pdf) from alice"
      bob <## "completed receiving file 1 (test.pdf) from alice"
      Just (CFArgs key nonce) <- J.decode . LB.pack <$> getTermLine bob
      Right dest <- chatReadFile "./tests/tmp/bob/test.pdf" (strEncode key) (strEncode nonce)
      LB.length dest `shouldBe` fromIntegral srcLen
      LB.toStrict dest `shouldBe` src

testXFTPAcceptAfterUpload :: HasCallStack => FilePath -> IO ()
testXFTPAcceptAfterUpload =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test.pdf) for bob"

      threadDelay 100000

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
               "started receiving file 1 (test.pdf) from alice"
             ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src

testXFTPGroupFileTransfer :: HasCallStack => FilePath -> IO ()
testXFTPGroupFileTransfer =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    withXFTPServer $ do
      createGroup3 "team" alice bob cath

      alice #> "/f #team ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice <## "completed uploading file 1 (test.pdf) for #team"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
               "started receiving file 1 (test.pdf) from alice"
             ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from alice to ./tests/tmp/test_1.pdf",
               "started receiving file 1 (test.pdf) from alice"
             ]
      cath <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest1 <- B.readFile "./tests/tmp/test.pdf"
      dest2 <- B.readFile "./tests/tmp/test_1.pdf"
      dest1 `shouldBe` src
      dest2 `shouldBe` src

testXFTPDeleteUploadedFile :: HasCallStack => FilePath -> IO ()
testXFTPDeleteUploadedFile =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test.pdf) for bob"

      alice ##> "/fc 1"
      concurrentlyN_
        [ alice <## "cancelled sending file 1 (test.pdf)",
          bob <## "alice cancelled sending file 1 (test.pdf)"
        ]

      bob ##> "/fr 1 ./tests/tmp"
      bob <## "file cancelled: test.pdf"

testXFTPDeleteUploadedFileGroup :: HasCallStack => FilePath -> IO ()
testXFTPDeleteUploadedFileGroup =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    withXFTPServer $ do
      createGroup3 "team" alice bob cath

      alice #> "/f #team ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice <## "completed uploading file 1 (test.pdf) for #team"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
               "started receiving file 1 (test.pdf) from alice"
             ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      alice ##> "/fs 1"
      alice <## "sending file 1 (test.pdf) complete"
      bob ##> "/fs 1"
      bob <## "receiving file 1 (test.pdf) complete, path: ./tests/tmp/test.pdf"
      cath ##> "/fs 1"
      cath <## "receiving file 1 (test.pdf) not accepted yet, use /fr 1 to receive file"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src

      alice ##> "/fc 1"
      concurrentlyN_
        [ alice <## "cancelled sending file 1 (test.pdf) to bob, cath",
          cath <## "alice cancelled sending file 1 (test.pdf)"
        ]

      alice ##> "/fs 1"
      alice <## "sending file 1 (test.pdf) cancelled"
      bob ##> "/fs 1"
      bob <## "receiving file 1 (test.pdf) complete, path: ./tests/tmp/test.pdf"
      cath ##> "/fs 1"
      cath <## "receiving file 1 (test.pdf) cancelled"

      cath ##> "/fr 1 ./tests/tmp"
      cath <## "file cancelled: test.pdf"

testXFTPWithRelativePaths :: HasCallStack => FilePath -> IO ()
testXFTPWithRelativePaths =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      -- agent is passed xftp work directory only on chat start,
      -- so for test we work around by stopping and starting chat
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      alice #$> ("/_temp_folder ./tests/tmp/alice_xftp", id, "ok")
      alice ##> "/_start"
      alice <## "chat started"

      bob ##> "/_stop"
      bob <## "chat stopped"
      bob #$> ("/_files_folder ./tests/tmp/bob_files", id, "ok")
      bob #$> ("/_temp_folder ./tests/tmp/bob_xftp", id, "ok")
      bob ##> "/_start"
      bob <## "chat started"

      connectUsers alice bob

      alice #> "/f @bob test.pdf"
      alice <## "use /fc 1 to cancel sending"
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

testXFTPContinueRcv :: HasCallStack => FilePath -> IO ()
testXFTPContinueRcv tmp = do
  withXFTPServer $ do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      withNewTestChat tmp "bob" bobProfile $ \bob -> do
        connectUsers alice bob

        alice #> "/f @bob ./tests/fixtures/test.pdf"
        alice <## "use /fc 1 to cancel sending"
        bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
        alice <## "completed uploading file 1 (test.pdf) for bob"

  -- server is down - file is not received
  withTestChat tmp "bob" $ \bob -> do
    bob <## "1 contacts connected (use /cs for the list)"
    bob ##> "/fr 1 ./tests/tmp"
    bob
      <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
             "started receiving file 1 (test.pdf) from alice"
           ]

    bob ##> "/fs 1"
    bob <## "receiving file 1 (test.pdf) progress 0% of 266.0 KiB"

    (bob </)

  withXFTPServer $ do
    -- server is up - file reception is continued
    withTestChat tmp "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      bob <## "completed receiving file 1 (test.pdf) from alice"
      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src

testXFTPMarkToReceive :: HasCallStack => FilePath -> IO ()
testXFTPMarkToReceive = do
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed uploading file 1 (test.pdf) for bob"
      bob #$> ("/_set_file_to_receive 1", id, "ok")

      threadDelay 100000

      bob ##> "/_stop"
      bob <## "chat stopped"

      bob #$> ("/_files_folder ./tests/tmp/bob_files", id, "ok")
      bob #$> ("/_temp_folder ./tests/tmp/bob_xftp", id, "ok")

      threadDelay 100000

      bob ##> "/_start"
      bob <## "chat started"

      bob
        <### [ "1 contacts connected (use /cs for the list)",
               "started receiving file 1 (test.pdf) from alice",
               "saving file 1 from alice to test.pdf"
             ]
      bob <## "completed receiving file 1 (test.pdf) from alice"

      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/bob_files/test.pdf"
      dest `shouldBe` src

testXFTPRcvError :: HasCallStack => FilePath -> IO ()
testXFTPRcvError tmp = do
  withXFTPServer $ do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      withNewTestChat tmp "bob" bobProfile $ \bob -> do
        connectUsers alice bob

        alice #> "/f @bob ./tests/fixtures/test.pdf"
        alice <## "use /fc 1 to cancel sending"
        bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
        alice <## "completed uploading file 1 (test.pdf) for bob"

  -- server is up w/t store log - file reception should fail
  withXFTPServer' xftpServerConfig {storeLogFile = Nothing} $ do
    withTestChat tmp "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/test.pdf",
               "started receiving file 1 (test.pdf) from alice"
             ]
      bob <## "error receiving file 1 (test.pdf) from alice"
      _ <- getTermLine bob

      bob ##> "/fs 1"
      bob <## "receiving file 1 (test.pdf) error"

testXFTPCancelRcvRepeat :: HasCallStack => FilePath -> IO ()
testXFTPCancelRcvRepeat =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile"]

      connectUsers alice bob

      alice #> "/f @bob ./tests/tmp/testfile"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file testfile (17.0 MiB / 17825792 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      concurrentlyN_
        [ alice <## "completed uploading file 1 (testfile) for bob",
          bob
            <### [ "saving file 1 from alice to ./tests/tmp/testfile_1",
                   "started receiving file 1 (testfile) from alice"
                 ]
        ]

      threadDelay 100000

      bob ##> "/fs 1"
      bob <##. "receiving file 1 (testfile) progress"

      bob ##> "/fc 1"
      bob <## "cancelled receiving file 1 (testfile) from alice"

      bob ##> "/fs 1"
      bob <## "receiving file 1 (testfile) not accepted yet, use /fr 1 to receive file"

      bob ##> "/fr 1 ./tests/tmp"
      bob
        <### [ "saving file 1 from alice to ./tests/tmp/testfile_1",
               "started receiving file 1 (testfile) from alice"
             ]
      bob <## "completed receiving file 1 (testfile) from alice"

      bob ##> "/fs 1"
      bob <## "receiving file 1 (testfile) complete, path: ./tests/tmp/testfile_1"

      src <- B.readFile "./tests/tmp/testfile"
      dest <- B.readFile "./tests/tmp/testfile_1"
      dest `shouldBe` src
  where
    cfg = testCfg {xftpDescrPartSize = 200}

testAutoAcceptFile :: HasCallStack => FilePath -> IO ()
testAutoAcceptFile =
  testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> withXFTPServer $ do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/bob_files"
    bob <## "ok"
    alice #> "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
    alice <## "completed uploading file 1 (test.jpg) for bob"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "saving file 1 from alice to test.jpg"
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
    (bob </)
    alice #> "/f @bob ./tests/fixtures/test_1MB.pdf"
    alice <## "use /fc 2 to cancel sending"
    alice <## "completed uploading file 2 (test_1MB.pdf) for bob"
    bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
    bob <## "use /fr 2 [<dir>/ | <path>] to receive it"
    -- no auto accept for large files
    (bob </)
  where
    opts = (testOpts :: ChatOpts) {autoAcceptFileSize = 200000}

testProhibitFiles :: HasCallStack => FilePath -> IO ()
testProhibitFiles =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> withXFTPServer $ do
    createGroup3 "team" alice bob cath
    alice ##> "/set files #team off"
    alice <## "updated group preferences:"
    alice <## "Files and media: off"
    concurrentlyN_
      [ do
          bob <## "alice updated group #team:"
          bob <## "updated group preferences:"
          bob <## "Files and media: off",
        do
          cath <## "alice updated group #team:"
          cath <## "updated group preferences:"
          cath <## "Files and media: off"
      ]
    alice ##> "/f #team ./tests/fixtures/test.jpg"
    alice <## "bad chat command: feature not allowed Files and media"
    (bob </)
    (cath </)

testXFTPStandaloneSmall :: HasCallStack => FilePath -> IO ()
testXFTPStandaloneSmall = testChat2 aliceProfile aliceDesktopProfile $ \src dst -> do
  withXFTPServer $ do
    logNote "sending"
    src ##> "/_upload 1 ./tests/fixtures/test.jpg"
    src <## "started standalone uploading file 1 (test.jpg)"
    -- silent progress events
    threadDelay 250000
    src <## "file 1 (test.jpg) upload complete. download with:"
    -- file description fits, enjoy the direct URIs
    _uri1 <- getTermLine src
    _uri2 <- getTermLine src
    uri3 <- getTermLine src
    _uri4 <- getTermLine src

    logNote "receiving"
    let dstFile = "./tests/tmp/test.jpg"
    dst ##> ("/_download 1 " <> uri3 <> " " <> dstFile)
    dst <## "started standalone receiving file 1 (test.jpg)"
    -- silent progress events
    threadDelay 250000
    dst <## "completed standalone receiving file 1 (test.jpg)"
    srcBody <- B.readFile "./tests/fixtures/test.jpg"
    B.readFile dstFile `shouldReturn` srcBody

testXFTPStandaloneLarge :: HasCallStack => FilePath -> IO ()
testXFTPStandaloneLarge = testChat2 aliceProfile aliceDesktopProfile $ \src dst -> do
  withXFTPServer $ do
    xftpCLI ["rand", "./tests/tmp/testfile.in", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile.in"]

    logNote "sending"
    src ##> "/_upload 1 ./tests/tmp/testfile.in"
    src <## "started standalone uploading file 1 (testfile.in)"
    -- silent progress events
    threadDelay 250000
    src <## "file 1 (testfile.in) uploaded, preparing redirect file 2"
    src <## "file 1 (testfile.in) upload complete. download with:"
    uri <- getTermLine src
    _uri2 <- getTermLine src
    _uri3 <- getTermLine src
    _uri4 <- getTermLine src

    logNote "receiving"
    let dstFile = "./tests/tmp/testfile.out"
    dst ##> ("/_download 1 " <> uri <> " " <> dstFile)
    dst <## "started standalone receiving file 1 (testfile.out)"
    -- silent progress events
    threadDelay 250000
    dst <## "completed standalone receiving file 1 (testfile.out)"
    srcBody <- B.readFile "./tests/tmp/testfile.in"
    B.readFile dstFile `shouldReturn` srcBody

testXFTPStandaloneCancelSnd :: HasCallStack => FilePath -> IO ()
testXFTPStandaloneCancelSnd = testChat2 aliceProfile aliceDesktopProfile $ \src dst -> do
  withXFTPServer $ do
    xftpCLI ["rand", "./tests/tmp/testfile.in", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile.in"]

    logNote "sending"
    src ##> "/_upload 1 ./tests/tmp/testfile.in"
    src <## "started standalone uploading file 1 (testfile.in)"
    -- silent progress events
    threadDelay 250000
    src <## "file 1 (testfile.in) uploaded, preparing redirect file 2"
    src <## "file 1 (testfile.in) upload complete. download with:"
    uri <- getTermLine src
    _uri2 <- getTermLine src
    _uri3 <- getTermLine src
    _uri4 <- getTermLine src

    logNote "cancelling"
    src ##> "/fc 1"
    src <## "cancelled sending file 1 (testfile.in)"
    threadDelay 1000000

    logNote "trying to receive cancelled"
    dst ##> ("/_download 1 " <> uri <> " " <> "./tests/tmp/should.not.extist")
    dst <## "started standalone receiving file 1 (should.not.extist)"
    threadDelay 100000
    logWarn "no error?"
    dst <## "error receiving file 1 (should.not.extist)"
    dst <## "INTERNAL {internalErr = \"XFTP {xftpErr = AUTH}\"}"

testXFTPStandaloneRelativePaths :: HasCallStack => FilePath -> IO ()
testXFTPStandaloneRelativePaths = testChat2 aliceProfile aliceDesktopProfile $ \src dst -> do
  withXFTPServer $ do
    logNote "sending"
    src #$> ("/_files_folder ./tests/tmp/src_files", id, "ok")
    src #$> ("/_temp_folder ./tests/tmp/src_xftp_temp", id, "ok")

    xftpCLI ["rand", "./tests/tmp/src_files/testfile.in", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/src_files/testfile.in"]

    src ##> "/_upload 1 testfile.in"
    src <## "started standalone uploading file 1 (testfile.in)"
    -- silent progress events
    threadDelay 250000
    src <## "file 1 (testfile.in) uploaded, preparing redirect file 2"
    src <## "file 1 (testfile.in) upload complete. download with:"
    uri <- getTermLine src
    _uri2 <- getTermLine src
    _uri3 <- getTermLine src
    _uri4 <- getTermLine src

    logNote "receiving"
    dst #$> ("/_files_folder ./tests/tmp/dst_files", id, "ok")
    dst #$> ("/_temp_folder ./tests/tmp/dst_xftp_temp", id, "ok")
    dst ##> ("/_download 1 " <> uri <> " testfile.out")
    dst <## "started standalone receiving file 1 (testfile.out)"
    -- silent progress events
    threadDelay 250000
    dst <## "completed standalone receiving file 1 (testfile.out)"
    srcBody <- B.readFile "./tests/tmp/src_files/testfile.in"
    B.readFile "./tests/tmp/dst_files/testfile.out" `shouldReturn` srcBody

testXFTPStandaloneCancelRcv :: HasCallStack => FilePath -> IO ()
testXFTPStandaloneCancelRcv = testChat2 aliceProfile aliceDesktopProfile $ \src dst -> do
  withXFTPServer $ do
    xftpCLI ["rand", "./tests/tmp/testfile.in", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile.in"]

    logNote "sending"
    src ##> "/_upload 1 ./tests/tmp/testfile.in"
    src <## "started standalone uploading file 1 (testfile.in)"
    -- silent progress events
    threadDelay 250000
    src <## "file 1 (testfile.in) uploaded, preparing redirect file 2"
    src <## "file 1 (testfile.in) upload complete. download with:"
    uri <- getTermLine src
    _uri2 <- getTermLine src
    _uri3 <- getTermLine src
    _uri4 <- getTermLine src

    logNote "receiving"
    let dstFile = "./tests/tmp/testfile.out"
    dst ##> ("/_download 1 " <> uri <> " " <> dstFile)
    dst <## "started standalone receiving file 1 (testfile.out)"
    threadDelay 25000 -- give workers some time to avoid internal errors from starting tasks
    logNote "cancelling"
    dst ##> "/fc 1"
    dst <## "cancelled receiving file 1 (testfile.out)"
    threadDelay 25000
    doesFileExist dstFile `shouldReturn` False
