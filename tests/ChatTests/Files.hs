{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Files where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Controller (ChatConfig (..), InlineFilesConfig (..), defaultInlineFilesConfig)
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Messaging.Util (unlessM)
import System.Directory (copyFile, doesFileExist)
import Test.Hspec

chatFileTests :: SpecWith FilePath
chatFileTests = do
  describe "sending and receiving files" $ do
    describe "send and receive file" $ fileTestMatrix2 runTestFileTransfer
    it "send and receive file inline (without accepting)" testInlineFileTransfer
    it "accept inline file transfer, sender cancels during transfer" testAcceptInlineFileSndCancelDuringTransfer
    it "send and receive small file inline (default config)" testSmallInlineFileTransfer
    it "small file sent without acceptance is ignored in terminal by default" testSmallInlineFileIgnored
    it "receive file inline with inline=on option" testReceiveInline
    describe "send and receive a small file" $ fileTestMatrix2 runTestSmallFileTransfer
    describe "sender cancelled file transfer before transfer" $ fileTestMatrix2 runTestFileSndCancelBeforeTransfer
    it "sender cancelled file transfer during transfer" testFileSndCancelDuringTransfer
    it "recipient cancelled file transfer" testFileRcvCancel
    describe "send and receive file to group" $ fileTestMatrix3 runTestGroupFileTransfer
    it "send and receive file inline to group (without accepting)" testInlineGroupFileTransfer
    it "send and receive small file inline to group (default config)" testSmallInlineGroupFileTransfer
    it "small file sent without acceptance is ignored in terminal by default" testSmallInlineGroupFileIgnored
    describe "sender cancelled group file transfer before transfer" $ fileTestMatrix3 runTestGroupFileSndCancelBeforeTransfer
  describe "messages with files" $ do
    describe "send and receive message with file" $ fileTestMatrix2 runTestMessageWithFile
    it "send and receive image" testSendImage
    it "files folder: send and receive image" testFilesFoldersSendImage
    it "files folder: sender deleted file during transfer" testFilesFoldersImageSndDelete
    it "files folder: recipient deleted file during transfer" testFilesFoldersImageRcvDelete
    it "send and receive image with text and quote" testSendImageWithTextAndQuote
    describe "send and receive image to group" testGroupSendImage
    it "send and receive image with text and quote to group" testGroupSendImageWithTextAndQuote
  describe "async sending and receiving files" $ do
    it "send and receive file, sender restarts" testAsyncFileTransferSenderRestarts
    it "send and receive file, receiver restarts" testAsyncFileTransferReceiverRestarts
    xdescribe "send and receive file, fully asynchronous" $ do
      it "v2" testAsyncFileTransfer
      it "v1" testAsyncFileTransferV1
    xit "send and receive file to group, fully asynchronous" testAsyncGroupFileTransfer

runTestFileTransfer :: HasCallStack => TestCC -> TestCC -> IO ()
runTestFileTransfer alice bob = do
  connectUsers alice bob
  startFileTransfer' alice bob "test.pdf" "266.0 KiB / 272376 bytes"
  concurrentlyN_
    [ do
        bob #> "@alice receiving here..."
        bob <## "completed receiving file 1 (test.pdf) from alice",
      alice
        <### [ WithTime "bob> receiving here...",
               "completed sending file 1 (test.pdf) to bob"
             ]
    ]
  src <- B.readFile "./tests/fixtures/test.pdf"
  dest <- B.readFile "./tests/tmp/test.pdf"
  dest `shouldBe` src

testInlineFileTransfer :: HasCallStack => FilePath -> IO ()
testInlineFileTransfer =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/test.jpg\"}"
    alice <# "@bob voice message (00:10)"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    -- below is not shown in "sent" mode
    -- alice <## "use /fc 1 to cancel sending"
    bob <# "alice> voice message (00:10)"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    -- below is not shown in "sent" mode
    -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "started receiving file 1 (test.jpg) from alice"
    concurrently_
      (alice <## "completed sending file 1 (test.jpg) to bob")
      (bob <## "completed receiving file 1 (test.jpg) from alice")
    src <- B.readFile "./tests/fixtures/test.jpg"
    dest <- B.readFile "./tests/tmp/test.jpg"
    dest `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, sendChunks = 100, receiveChunks = 100}}

testAcceptInlineFileSndCancelDuringTransfer :: HasCallStack => FilePath -> IO ()
testAcceptInlineFileSndCancelDuringTransfer =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    alice #> "/f @bob ./tests/fixtures/test_1MB.pdf"
    alice <## "use /fc 1 to cancel sending"
    bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 inline=on"
    bob <## "saving file 1 from alice to test_1MB.pdf"
    alice <## "started sending file 1 (test_1MB.pdf) to bob"
    bob <## "started receiving file 1 (test_1MB.pdf) from alice"
    alice ##> "/fc 1" -- test that inline file cancel doesn't delete contact connection
    concurrentlyN_
      [ do
          alice <##. "cancelled sending file 1 (test_1MB.pdf)"
          alice <## "completed sending file 1 (test_1MB.pdf) to bob",
        do
          bob <## "completed receiving file 1 (test_1MB.pdf) from alice"
          bob <## "alice cancelled sending file 1 (test_1MB.pdf)"
      ]
    alice #> "@bob hi"
    bob <# "alice> hi"
    bob #> "@alice hey"
    alice <# "bob> hey"
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, receiveChunks = 50}}

testSmallInlineFileTransfer :: HasCallStack => FilePath -> IO ()
testSmallInlineFileTransfer =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
    alice <# "@bob voice message (00:10)"
    alice <# "/f @bob ./tests/fixtures/logo.jpg"
    -- below is not shown in "sent" mode
    -- alice <## "use /fc 1 to cancel sending"
    bob <# "alice> voice message (00:10)"
    bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
    -- below is not shown in "sent" mode
    -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "started receiving file 1 (logo.jpg) from alice"
    concurrently_
      (alice <## "completed sending file 1 (logo.jpg) to bob")
      (bob <## "completed receiving file 1 (logo.jpg) from alice")
    src <- B.readFile "./tests/fixtures/logo.jpg"
    dest <- B.readFile "./tests/tmp/logo.jpg"
    dest `shouldBe` src

testSmallInlineFileIgnored :: HasCallStack => FilePath -> IO ()
testSmallInlineFileIgnored tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice ->
    withNewTestChatOpts tmp testOpts {allowInstantFiles = False} "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      bob ##> "/_files_folder ./tests/tmp/"
      bob <## "ok"
      alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "@bob voice message (00:10)"
      alice <# "/f @bob ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      bob <# "alice> voice message (00:10)"
      bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob <## "A small file sent without acceptance - you can enable receiving such files with -f option."
      -- below is not shown in "sent" mode
      -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed sending file 1 (logo.jpg) to bob"
      bob ##> "/fr 1"
      bob <## "file is already being received: logo.jpg"

testReceiveInline :: HasCallStack => FilePath -> IO ()
testReceiveInline =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice #> "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 inline=on ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
    src <- B.readFile "./tests/fixtures/test.jpg"
    dest <- B.readFile "./tests/tmp/test.jpg"
    dest `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 10, receiveChunks = 5}}

runTestSmallFileTransfer :: HasCallStack => TestCC -> TestCC -> IO ()
runTestSmallFileTransfer alice bob = do
  connectUsers alice bob
  alice #> "/f @bob ./tests/fixtures/test.txt"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.txt (11 bytes / 11 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.txt"
  concurrentlyN_
    [ do
        bob <## "started receiving file 1 (test.txt) from alice"
        bob <## "completed receiving file 1 (test.txt) from alice",
      do
        alice <## "started sending file 1 (test.txt) to bob"
        alice <## "completed sending file 1 (test.txt) to bob"
    ]
  src <- B.readFile "./tests/fixtures/test.txt"
  dest <- B.readFile "./tests/tmp/test.txt"
  dest `shouldBe` src

runTestFileSndCancelBeforeTransfer :: HasCallStack => TestCC -> TestCC -> IO ()
runTestFileSndCancelBeforeTransfer alice bob = do
  connectUsers alice bob
  alice #> "/f @bob ./tests/fixtures/test.txt"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.txt (11 bytes / 11 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  alice ##> "/fc 1"
  concurrentlyN_
    [ alice <##. "cancelled sending file 1 (test.txt)",
      bob <## "alice cancelled sending file 1 (test.txt)"
    ]
  alice ##> "/fs 1"
  alice
    <##.. [ "sending file 1 (test.txt): no file transfers",
            "sending file 1 (test.txt) cancelled: bob"
          ]
  alice <## "file transfer cancelled"
  bob ##> "/fs 1"
  bob <## "receiving file 1 (test.txt) cancelled"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "file cancelled: test.txt"

testFileSndCancelDuringTransfer :: HasCallStack => FilePath -> IO ()
testFileSndCancelDuringTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer' alice bob "test_1MB.pdf" "1017.7 KiB / 1042157 bytes"
      alice ##> "/fc 1"
      concurrentlyN_
        [ do
            alice <## "cancelled sending file 1 (test_1MB.pdf) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test_1MB.pdf) cancelled: bob"
            alice <## "file transfer cancelled",
          do
            bob <## "alice cancelled sending file 1 (test_1MB.pdf)"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test_1MB.pdf) cancelled, received part path: ./tests/tmp/test_1MB.pdf"
        ]
      checkPartialTransfer "test_1MB.pdf"

testFileRcvCancel :: HasCallStack => FilePath -> IO ()
testFileRcvCancel =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      bob ##> "/fs 1"
      getTermLine bob >>= (`shouldStartWith` "receiving file 1 (test.jpg) progress")
      waitFileExists "./tests/tmp/test.jpg"
      bob ##> "/fc 1"
      concurrentlyN_
        [ do
            bob <## "cancelled receiving file 1 (test.jpg) from alice"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg",
          do
            alice <## "bob cancelled receiving file 1 (test.jpg)"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled: bob"
        ]
      checkPartialTransfer "test.jpg"

runTestGroupFileTransfer :: HasCallStack => TestCC -> TestCC -> TestCC -> IO ()
runTestGroupFileTransfer alice bob cath = do
  createGroup3 "team" alice bob cath
  alice #> "/f #team ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  concurrentlyN_
    [ do
        bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
      do
        cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
        cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
    ]
  alice ##> "/fs 1"
  getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg): no file transfers")
  bob ##> "/fr 1 ./tests/tmp/"
  bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  concurrentlyN_
    [ do
        alice <## "started sending file 1 (test.jpg) to bob"
        alice <## "completed sending file 1 (test.jpg) to bob"
        alice ##> "/fs 1"
        alice <## "sending file 1 (test.jpg) complete: bob",
      do
        bob <## "started receiving file 1 (test.jpg) from alice"
        bob <## "completed receiving file 1 (test.jpg) from alice"
    ]
  cath ##> "/fr 1 ./tests/tmp/"
  cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
  concurrentlyN_
    [ do
        alice <## "started sending file 1 (test.jpg) to cath"
        alice <## "completed sending file 1 (test.jpg) to cath"
        alice ##> "/fs 1"
        getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) complete"),
      do
        cath <## "started receiving file 1 (test.jpg) from alice"
        cath <## "completed receiving file 1 (test.jpg) from alice"
    ]
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest1 <- B.readFile "./tests/tmp/test.jpg"
  dest2 <- B.readFile "./tests/tmp/test_1.jpg"
  dest1 `shouldBe` src
  dest2 `shouldBe` src

testInlineGroupFileTransfer :: HasCallStack => FilePath -> IO ()
testInlineGroupFileTransfer =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/_files_folder ./tests/tmp/bob/"
      bob <## "ok"
      cath ##> "/_files_folder ./tests/tmp/cath/"
      cath <## "ok"
      alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "#team voice message (00:10)"
      alice <# "/f #team ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            alice
              <### [ "completed sending file 1 (logo.jpg) to bob",
                     "completed sending file 1 (logo.jpg) to cath"
                   ]
            alice ##> "/fs 1"
            alice <##. "sending file 1 (logo.jpg) complete",
          do
            bob <# "#team alice> voice message (00:10)"
            bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            bob <## "started receiving file 1 (logo.jpg) from alice"
            bob <## "completed receiving file 1 (logo.jpg) from alice",
          do
            cath <# "#team alice> voice message (00:10)"
            cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            cath <## "started receiving file 1 (logo.jpg) from alice"
            cath <## "completed receiving file 1 (logo.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/logo.jpg"
      dest1 <- B.readFile "./tests/tmp/bob/logo.jpg"
      dest2 <- B.readFile "./tests/tmp/cath/logo.jpg"
      dest1 `shouldBe` src
      dest2 `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, sendChunks = 100, totalSendChunks = 100, receiveChunks = 100}}

testSmallInlineGroupFileTransfer :: HasCallStack => FilePath -> IO ()
testSmallInlineGroupFileTransfer =
  testChatCfg3 testCfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/_files_folder ./tests/tmp/bob/"
      bob <## "ok"
      cath ##> "/_files_folder ./tests/tmp/cath/"
      cath <## "ok"
      alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "#team voice message (00:10)"
      alice <# "/f #team ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            alice
              <### [ "completed sending file 1 (logo.jpg) to bob",
                     "completed sending file 1 (logo.jpg) to cath"
                   ]
            alice ##> "/fs 1"
            alice <##. "sending file 1 (logo.jpg) complete",
          do
            bob <# "#team alice> voice message (00:10)"
            bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            bob <## "started receiving file 1 (logo.jpg) from alice"
            bob <## "completed receiving file 1 (logo.jpg) from alice",
          do
            cath <# "#team alice> voice message (00:10)"
            cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            cath <## "started receiving file 1 (logo.jpg) from alice"
            cath <## "completed receiving file 1 (logo.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/logo.jpg"
      dest1 <- B.readFile "./tests/tmp/bob/logo.jpg"
      dest2 <- B.readFile "./tests/tmp/cath/logo.jpg"
      dest1 `shouldBe` src
      dest2 `shouldBe` src

testSmallInlineGroupFileIgnored :: HasCallStack => FilePath -> IO ()
testSmallInlineGroupFileIgnored tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice ->
    withNewTestChatOpts tmp testOpts {allowInstantFiles = False} "bob" bobProfile $ \bob -> do
      withNewTestChatOpts tmp testOpts {allowInstantFiles = False} "cath" cathProfile $ \cath -> do
        createGroup3 "team" alice bob cath
        bob ##> "/_files_folder ./tests/tmp/bob/"
        bob <## "ok"
        cath ##> "/_files_folder ./tests/tmp/cath/"
        cath <## "ok"
        alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
        alice <# "#team voice message (00:10)"
        alice <# "/f #team ./tests/fixtures/logo.jpg"
        -- below is not shown in "sent" mode
        -- alice <## "use /fc 1 to cancel sending"
        concurrentlyN_
          [ do
              alice
                <### [ "completed sending file 1 (logo.jpg) to bob",
                       "completed sending file 1 (logo.jpg) to cath"
                     ]
              alice ##> "/fs 1"
              alice <##. "sending file 1 (logo.jpg) complete",
            do
              bob <# "#team alice> voice message (00:10)"
              bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
              bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
              bob <## "A small file sent without acceptance - you can enable receiving such files with -f option."
              bob ##> "/fr 1"
              bob <## "file is already being received: logo.jpg",
            do
              cath <# "#team alice> voice message (00:10)"
              cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
              cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
              cath <## "A small file sent without acceptance - you can enable receiving such files with -f option."
              cath ##> "/fr 1"
              cath <## "file is already being received: logo.jpg"
          ]

runTestGroupFileSndCancelBeforeTransfer :: HasCallStack => TestCC -> TestCC -> TestCC -> IO ()
runTestGroupFileSndCancelBeforeTransfer alice bob cath = do
  createGroup3 "team" alice bob cath
  alice #> "/f #team ./tests/fixtures/test.txt"
  alice <## "use /fc 1 to cancel sending"
  concurrentlyN_
    [ do
        bob <# "#team alice> sends file test.txt (11 bytes / 11 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
      do
        cath <# "#team alice> sends file test.txt (11 bytes / 11 bytes)"
        cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
    ]
  alice ##> "/fc 1"
  concurrentlyN_
    [ alice <## "cancelled sending file 1 (test.txt)",
      bob <## "alice cancelled sending file 1 (test.txt)",
      cath <## "alice cancelled sending file 1 (test.txt)"
    ]
  alice ##> "/fs 1"
  alice <## "sending file 1 (test.txt): no file transfers"
  alice <## "file transfer cancelled"
  bob ##> "/fs 1"
  bob <## "receiving file 1 (test.txt) cancelled"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "file cancelled: test.txt"

runTestMessageWithFile :: HasCallStack => TestCC -> TestCC -> IO ()
runTestMessageWithFile alice bob = do
  connectUsers alice bob
  alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
  alice <# "@bob hi, sending a file"
  alice <# "/f @bob ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> hi, sending a file"
  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  concurrently_
    (bob <## "started receiving file 1 (test.jpg) from alice")
    (alice <## "started sending file 1 (test.jpg) to bob")
  concurrently_
    (bob <## "completed receiving file 1 (test.jpg) from alice")
    (alice <## "completed sending file 1 (test.jpg) to bob")
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src
  alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, "hi, sending a file"), Just "./tests/fixtures/test.jpg")])
  bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, "hi, sending a file"), Just "./tests/tmp/test.jpg")])

testSendImage :: HasCallStack => FilePath -> IO ()
testSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "./tests/tmp/test.jpg")])
      -- deleting contact without files folder set should not remove file
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      fileExists <- doesFileExist "./tests/tmp/test.jpg"
      fileExists `shouldBe` True

testFilesFoldersSendImage :: HasCallStack => FilePath -> IO ()
testFilesFoldersSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/app_files/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "test.jpg")])
      -- deleting contact with files folder set should remove file
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageSndDelete :: HasCallStack => FilePath -> IO ()
testFilesFoldersImageSndDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/tmp/alice_app_files", id, "ok")
      copyFile "./tests/fixtures/test_1MB.pdf" "./tests/tmp/alice_app_files/test_1MB.pdf"
      bob #$> ("/_files_folder ./tests/tmp/bob_app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test_1MB.pdf\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test_1MB.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test_1MB.pdf"
      concurrently_
        (bob <## "started receiving file 1 (test_1MB.pdf) from alice")
        (alice <## "started sending file 1 (test_1MB.pdf) to bob")
      -- deleting contact should cancel and remove file
      checkActionDeletesFile "./tests/tmp/alice_app_files/test_1MB.pdf" $ do
        alice ##> "/d bob"
        alice <## "bob: contact is deleted"
        bob ##> "/fs 1"
        bob <##. "receiving file 1 (test_1MB.pdf) progress"
      -- deleting contact should remove cancelled file
      checkActionDeletesFile "./tests/tmp/bob_app_files/test_1MB.pdf" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageRcvDelete :: HasCallStack => FilePath -> IO ()
testFilesFoldersImageRcvDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      -- deleting contact should cancel and remove file
      waitFileExists "./tests/tmp/app_files/test.jpg"
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"
        alice <## "bob cancelled receiving file 1 (test.jpg)"
        alice ##> "/fs 1"
        alice <## "sending file 1 (test.jpg) cancelled: bob"

testSendImageWithTextAndQuote :: HasCallStack => FilePath -> IO ()
testSendImageWithTextAndQuote =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
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
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
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
      alice ##> "/fr 2 ./tests/tmp"
      alice <## "saving file 2 from bob to ./tests/tmp/test.pdf"
      concurrently_
        (alice <## "started receiving file 2 (test.pdf) from bob")
        (bob <## "started sending file 2 (test.pdf) to alice")
      concurrently_
        (alice <## "completed receiving file 2 (test.pdf) from bob")
        (bob <## "completed sending file 2 (test.pdf) to alice")
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
      bob ##> "/fr 3 ./tests/tmp"
      bob <## "saving file 3 from alice to ./tests/tmp/test_1.jpg"
      concurrently_
        (bob <## "started receiving file 3 (test.jpg) from alice")
        (alice <## "started sending file 3 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 3 (test.jpg) from alice")
        (alice <## "completed sending file 3 (test.jpg) to bob")
      B.readFile "./tests/tmp/test_1.jpg" `shouldReturn` src

testGroupSendImage :: SpecWith FilePath
testGroupSendImage = versionTestMatrix3 runTestGroupSendImage
  where
    runTestGroupSendImage :: HasCallStack => TestCC -> TestCC -> TestCC -> IO ()
    runTestGroupSendImage alice bob cath = do
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
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob",
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath",
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]
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
    \alice bob cath -> do
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
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob",
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath",
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]
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

testAsyncFileTransferSenderRestarts :: HasCallStack => FilePath -> IO ()
testAsyncFileTransferSenderRestarts tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      connectUsers alice bob
      startFileTransfer' alice bob "test_1MB.pdf" "1017.7 KiB / 1042157 bytes"
    threadDelay 100000
    withTestChatContactConnected tmp "alice" $ \alice -> do
      alice <## "completed sending file 1 (test_1MB.pdf) to bob"
      bob <## "completed receiving file 1 (test_1MB.pdf) from alice"
      src <- B.readFile "./tests/fixtures/test_1MB.pdf"
      dest <- B.readFile "./tests/tmp/test_1MB.pdf"
      dest `shouldBe` src

testAsyncFileTransferReceiverRestarts :: HasCallStack => FilePath -> IO ()
testAsyncFileTransferReceiverRestarts tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      startFileTransfer' alice bob "test_1MB.pdf" "1017.7 KiB / 1042157 bytes"
    threadDelay 100000
    withTestChatContactConnected tmp "bob" $ \bob -> do
      alice <## "completed sending file 1 (test_1MB.pdf) to bob"
      bob <## "completed receiving file 1 (test_1MB.pdf) from alice"
      src <- B.readFile "./tests/fixtures/test_1MB.pdf"
      dest <- B.readFile "./tests/tmp/test_1MB.pdf"
      dest `shouldBe` src

testAsyncFileTransfer :: HasCallStack => FilePath -> IO ()
testAsyncFileTransfer tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      connectUsers alice bob
  withTestChatContactConnected tmp "alice" $ \alice -> do
    alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\":\"text\", \"text\": \"hi, sending a file\"}}"
    alice <# "@bob hi, sending a file"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatContactConnected tmp "bob" $ \bob -> do
    bob <# "alice> hi, sending a file"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  -- withTestChatContactConnected' tmp "alice" -- TODO not needed in v2
  -- withTestChatContactConnected' tmp "bob" -- TODO not needed in v2
  withTestChatContactConnected' tmp "alice"
  withTestChatContactConnected' tmp "bob"
  withTestChatContactConnected tmp "alice" $ \alice -> do
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
  withTestChatContactConnected tmp "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src

testAsyncFileTransferV1 :: HasCallStack => FilePath -> IO ()
testAsyncFileTransferV1 tmp = do
  withNewTestChatV1 tmp "alice" aliceProfile $ \alice ->
    withNewTestChatV1 tmp "bob" bobProfile $ \bob ->
      connectUsers alice bob
  withTestChatContactConnectedV1 tmp "alice" $ \alice -> do
    alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\":\"text\", \"text\": \"hi, sending a file\"}}"
    alice <# "@bob hi, sending a file"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatContactConnectedV1 tmp "bob" $ \bob -> do
    bob <# "alice> hi, sending a file"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  withTestChatContactConnectedV1' tmp "alice" -- TODO not needed in v2
  withTestChatContactConnectedV1' tmp "bob" -- TODO not needed in v2
  withTestChatContactConnectedV1' tmp "alice"
  withTestChatContactConnectedV1' tmp "bob"
  withTestChatContactConnectedV1 tmp "alice" $ \alice -> do
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
  withTestChatContactConnectedV1 tmp "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src

testAsyncGroupFileTransfer :: HasCallStack => FilePath -> IO ()
testAsyncGroupFileTransfer tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath ->
        createGroup3 "team" alice bob cath
  withTestChatGroup3Connected tmp "alice" $ \alice -> do
    alice ##> "/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"text\"}}"
    alice <# "/f #team ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatGroup3Connected tmp "bob" $ \bob -> do
    bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp/"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  withTestChatGroup3Connected tmp "cath" $ \cath -> do
    cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
    cath ##> "/fr 1 ./tests/tmp/"
    cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
  withTestChatGroup3Connected' tmp "alice"
  withTestChatGroup3Connected' tmp "bob"
  withTestChatGroup3Connected' tmp "cath"
  -- withTestChatGroup3Connected' tmp "alice" -- TODO not needed in v2
  -- withTestChatGroup3Connected' tmp "bob" -- TODO not needed in v2
  -- withTestChatGroup3Connected' tmp "cath" -- TODO not needed in v2
  withTestChatGroup3Connected' tmp "alice"
  withTestChatGroup3Connected tmp "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected tmp "cath" $ \cath -> do
    cath <## "started receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected tmp "alice" $ \alice -> do
    alice
      <### [ "started sending file 1 (test.jpg) to bob",
             "completed sending file 1 (test.jpg) to bob",
             "started sending file 1 (test.jpg) to cath",
             "completed sending file 1 (test.jpg) to cath"
           ]
  withTestChatGroup3Connected tmp "bob" $ \bob -> do
    bob <## "completed receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected tmp "cath" $ \cath -> do
    cath <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src
  dest2 <- B.readFile "./tests/tmp/test_1.jpg"
  dest2 `shouldBe` src

startFileTransfer :: HasCallStack => TestCC -> TestCC -> IO ()
startFileTransfer alice bob =
  startFileTransfer' alice bob "test.jpg" "136.5 KiB / 139737 bytes"

startFileTransfer' :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
startFileTransfer' cc1 cc2 fileName fileSize = startFileTransferWithDest' cc1 cc2 fileName fileSize $ Just "./tests/tmp"

checkPartialTransfer :: HasCallStack => String -> IO ()
checkPartialTransfer fileName = do
  src <- B.readFile $ "./tests/fixtures/" <> fileName
  dest <- B.readFile $ "./tests/tmp/" <> fileName
  B.unpack src `shouldStartWith` B.unpack dest
  B.length src > B.length dest `shouldBe` True

waitFileExists :: HasCallStack => FilePath -> IO ()
waitFileExists f = unlessM (doesFileExist f) $ waitFileExists f
