{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatTests.Files where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Simplex.Chat (roundedFDCount)
import Simplex.Chat.Controller (ChatConfig (..), InlineFilesConfig (..), XFTPFileConfig (..), defaultInlineFilesConfig)
import Simplex.Chat.Mobile.File
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.FileTransfer.Server.Env (XFTPServerConfig (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (unlessM)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize)
import Test.Hspec

chatFileTests :: SpecWith FilePath
chatFileTests = do
  describe "sending and receiving files" $ do
    describe "send and receive file" $ fileTestMatrix2 runTestFileTransfer
    describe "send file, receive and locally encrypt file" $ fileTestMatrix2 runTestFileTransferEncrypted
    it "send and receive file inline (without accepting)" testInlineFileTransfer
    it "send inline file, receive (without accepting) and locally encrypt" testInlineFileTransferEncrypted
    xit'' "accept inline file transfer, sender cancels during transfer" testAcceptInlineFileSndCancelDuringTransfer
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
    it "sender marking chat item deleted during file transfer cancels file" testSenderMarkItemDeletedTransfer
    it "files folder: send and receive image" testFilesFoldersSendImage
    it "files folder: sender deleted file during transfer" testFilesFoldersImageSndDelete
    it "files folder: recipient deleted file during transfer" testFilesFoldersImageRcvDelete
    it "send and receive image with text and quote" testSendImageWithTextAndQuote
    it "send and receive image to group" testGroupSendImage
    it "send and receive image with text and quote to group" testGroupSendImageWithTextAndQuote
  describe "async sending and receiving files" $ do
    -- fails on CI
    xit'' "send and receive file, sender restarts" testAsyncFileTransferSenderRestarts
    xit'' "send and receive file, receiver restarts" testAsyncFileTransferReceiverRestarts
    xdescribe "send and receive file, fully asynchronous" $ do
      it "v2" testAsyncFileTransfer
      it "v1" testAsyncFileTransferV1
    xit "send and receive file to group, fully asynchronous" testAsyncGroupFileTransfer
  describe "file transfer over XFTP" $ do
    it "round file description count" $ const testXFTPRoundFDCount
    it "send and receive file" testXFTPFileTransfer
    it "send and receive locally encrypted files" testXFTPFileTransferEncrypted
    it "send and receive file, accepting after upload" testXFTPAcceptAfterUpload
    it "send and receive file in group" testXFTPGroupFileTransfer
    it "delete uploaded file" testXFTPDeleteUploadedFile
    it "delete uploaded file in group" testXFTPDeleteUploadedFileGroup
    it "with changed XFTP config: send and receive file" testXFTPWithChangedConfig
    it "with relative paths: send and receive file" testXFTPWithRelativePaths
    xit' "continue receiving file after restart" testXFTPContinueRcv
    xit' "receive file marked to receive on chat start" testXFTPMarkToReceive
    it "error receiving file" testXFTPRcvError
    it "cancel receiving file, repeat receive" testXFTPCancelRcvRepeat
    it "should accept file automatically with CLI option" testAutoAcceptFile
    it "should prohibit file transfers in groups based on preference" testProhibitFiles

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

runTestFileTransferEncrypted :: HasCallStack => TestCC -> TestCC -> IO ()
runTestFileTransferEncrypted alice bob = do
  connectUsers alice bob
  alice #> "/f @bob ./tests/fixtures/test.pdf"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 encrypt=on ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.pdf"
  concurrently_
    (bob <## "started receiving file 1 (test.pdf) from alice")
    (alice <## "started sending file 1 (test.pdf) to bob")

  concurrentlyN_
    [ do
        bob #> "@alice receiving here..."
        -- uncomment this and below to test encryption error in encryptFile
        -- bob <## "cannot write file ./tests/tmp/test.pdf: test error, received file not encrypted"
        bob <## "completed receiving file 1 (test.pdf) from alice",
      alice
        <### [ WithTime "bob> receiving here...",
               "completed sending file 1 (test.pdf) to bob"
             ]
    ]
  Just (CFArgs key nonce) <- J.decode . LB.pack <$> getTermLine bob
  src <- B.readFile "./tests/fixtures/test.pdf"
  -- dest <- B.readFile "./tests/tmp/test.pdf"
  -- dest `shouldBe` src
  Right dest <- chatReadFile "./tests/tmp/test.pdf" (strEncode key) (strEncode nonce)
  LB.toStrict dest `shouldBe` src

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

testInlineFileTransferEncrypted :: HasCallStack => FilePath -> IO ()
testInlineFileTransferEncrypted =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    bob ##> "/_files_encrypt on"
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
    Just (CFArgs key nonce) <- J.decode . LB.pack <$> getTermLine bob
    src <- B.readFile "./tests/fixtures/test.jpg"
    Right dest <- chatReadFile "./tests/tmp/test.jpg" (strEncode key) (strEncode nonce)
    LB.toStrict dest `shouldBe` src
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
        bob <## "completed receiving file 1 (test_1MB.pdf) from alice"
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
            alice <## "file transfer cancelled"
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
      alice <## "bob (Bob) deleted contact with you"
      fileExists <- doesFileExist "./tests/tmp/test.jpg"
      fileExists `shouldBe` True

testSenderMarkItemDeletedTransfer :: HasCallStack => FilePath -> IO ()
testSenderMarkItemDeletedTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test_1MB.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
      alice <# "@bob hi, sending a file"
      alice <# "/f @bob ./tests/fixtures/test_1MB.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> hi, sending a file"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test_1MB.pdf"
      concurrently_
        (bob <## "started receiving file 1 (test_1MB.pdf) from alice")
        (alice <## "started sending file 1 (test_1MB.pdf) to bob")

      alice #$> ("/_delete item @2 " <> itemId 1 <> " broadcast", id, "message marked deleted")

      alice ##> "/fs 1"
      alice <## "sending file 1 (test_1MB.pdf) cancelled: bob"
      alice <## "file transfer cancelled"

      bob <# "alice> [marked deleted] hi, sending a file"
      bob ##> "/fs 1"
      bob <## "receiving file 1 (test_1MB.pdf) cancelled, received part path: ./tests/tmp/test_1MB.pdf"

      checkPartialTransfer "test_1MB.pdf"

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
        alice <## "bob (Bob) deleted contact with you"

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
        bob <## "alice (Alice) deleted contact with you"
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
        alice
          <### [ "bob (Bob) deleted contact with you",
                 "bob cancelled receiving file 1 (test.jpg)"
               ]
        alice ##> "/fs 1"
        alice <## "sending file 1 (test.jpg) cancelled: bob"
        alice <## "file transfer cancelled"

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

testGroupSendImage :: HasCallStack => FilePath -> IO ()
testGroupSendImage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
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
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPFileTransferEncrypted :: HasCallStack => FilePath -> IO ()
testXFTPFileTransferEncrypted =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPAcceptAfterUpload :: HasCallStack => FilePath -> IO ()
testXFTPAcceptAfterUpload =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPGroupFileTransfer :: HasCallStack => FilePath -> IO ()
testXFTPGroupFileTransfer =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $ \alice bob cath -> do
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
      -- alice <## "started sending file 1 (test.pdf) to #team" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPDeleteUploadedFile :: HasCallStack => FilePath -> IO ()
testXFTPDeleteUploadedFile =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
      alice <## "completed uploading file 1 (test.pdf) for bob"

      alice ##> "/fc 1"
      concurrentlyN_
        [ alice <## "cancelled sending file 1 (test.pdf)",
          bob <## "alice cancelled sending file 1 (test.pdf)"
        ]

      bob ##> "/fr 1 ./tests/tmp"
      bob <## "file cancelled: test.pdf"
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPDeleteUploadedFileGroup :: HasCallStack => FilePath -> IO ()
testXFTPDeleteUploadedFileGroup =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $ \alice bob cath -> do
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
      -- alice <## "started sending file 1 (test.pdf) to #team" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPWithChangedConfig :: HasCallStack => FilePath -> IO ()
testXFTPWithChangedConfig =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      alice #$> ("/_xftp off", id, "ok")
      alice #$> ("/_xftp on {\"minFileSize\":1024}", id, "ok")

      bob #$> ("/xftp off", id, "ok")
      bob #$> ("/xftp on size=1kb", id, "ok")

      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {tempDir = Just "./tests/tmp"}

testXFTPWithRelativePaths :: HasCallStack => FilePath -> IO ()
testXFTPWithRelativePaths =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
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
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}}

testXFTPContinueRcv :: HasCallStack => FilePath -> IO ()
testXFTPContinueRcv tmp = do
  withXFTPServer $ do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      withNewTestChatCfg tmp cfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob

        alice #> "/f @bob ./tests/fixtures/test.pdf"
        alice <## "use /fc 1 to cancel sending"
        bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
        -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
        alice <## "completed uploading file 1 (test.pdf) for bob"

  -- server is down - file is not received
  withTestChatCfg tmp cfg "bob" $ \bob -> do
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
    withTestChatCfg tmp cfg "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      bob <## "completed receiving file 1 (test.pdf) from alice"
      src <- B.readFile "./tests/fixtures/test.pdf"
      dest <- B.readFile "./tests/tmp/test.pdf"
      dest `shouldBe` src
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPMarkToReceive :: HasCallStack => FilePath -> IO ()
testXFTPMarkToReceive = do
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      connectUsers alice bob

      alice #> "/f @bob ./tests/fixtures/test.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}}

testXFTPRcvError :: HasCallStack => FilePath -> IO ()
testXFTPRcvError tmp = do
  withXFTPServer $ do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      withNewTestChatCfg tmp cfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob

        alice #> "/f @bob ./tests/fixtures/test.pdf"
        alice <## "use /fc 1 to cancel sending"
        bob <# "alice> sends file test.pdf (266.0 KiB / 272376 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
        -- alice <## "started sending file 1 (test.pdf) to bob" -- TODO "started uploading" ?
        alice <## "completed uploading file 1 (test.pdf) for bob"

  -- server is up w/t store log - file reception should fail
  withXFTPServer' xftpServerConfig {storeLogFile = Nothing} $ do
    withTestChatCfg tmp cfg "bob" $ \bob -> do
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testXFTPCancelRcvRepeat :: HasCallStack => FilePath -> IO ()
testXFTPCancelRcvRepeat =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile"]

      connectUsers alice bob

      alice #> "/f @bob ./tests/tmp/testfile"
      alice <## "use /fc 1 to cancel sending"
      -- alice <## "started sending file 1 (testfile) to bob" -- TODO "started uploading" ?
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
    cfg = testCfg {xftpDescrPartSize = 200, xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

testAutoAcceptFile :: HasCallStack => FilePath -> IO ()
testAutoAcceptFile =
  testChatCfgOpts2 cfg opts aliceProfile bobProfile $ \alice bob -> withXFTPServer $ do
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
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}
    opts = (testOpts :: ChatOpts) {autoAcceptFileSize = 200000}

testProhibitFiles :: HasCallStack => FilePath -> IO ()
testProhibitFiles =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $ \alice bob cath -> withXFTPServer $ do
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
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp"}

startFileTransfer :: HasCallStack => TestCC -> TestCC -> IO ()
startFileTransfer alice bob =
  startFileTransfer' alice bob "test.jpg" "136.5 KiB / 139737 bytes"

startFileTransfer' :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
startFileTransfer' cc1 cc2 fName fSize = startFileTransferWithDest' cc1 cc2 fName fSize $ Just "./tests/tmp"

checkPartialTransfer :: HasCallStack => String -> IO ()
checkPartialTransfer fileName = do
  src <- B.readFile $ "./tests/fixtures/" <> fileName
  dest <- B.readFile $ "./tests/tmp/" <> fileName
  B.unpack src `shouldStartWith` B.unpack dest
  B.length src > B.length dest `shouldBe` True

waitFileExists :: HasCallStack => FilePath -> IO ()
waitFileExists f = unlessM (doesFileExist f) $ waitFileExists f
