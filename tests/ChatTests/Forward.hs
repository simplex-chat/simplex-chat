{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Forward where

import ChatClient
import ChatTests.Utils
import qualified Data.ByteString.Char8 as B
import System.Directory (copyFile, doesFileExist)
import Test.Hspec hiding (it)

chatForwardTests :: SpecWith FilePath
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
    it "preserve original forward info" testForwardPreserveInfo
  describe "forward files" $ do
    it "from contact to contact" testForwardFileNoFilesFolder
    it "with relative paths: from contact to contact" testForwardFileContactToContact
    it "with relative paths: from group to notes" testForwardFileGroupToNotes
    it "with relative paths: from notes to group" testForwardFileNotesToGroup

testForwardContactToContact :: HasCallStack => FilePath -> IO ()
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

      alice ##> ("/_forward @2 @3 " <> msgId)
      alice <# "@cath -> from you"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> @bob -> @cath hey"
      alice <# "@cath -> from bob (contact id: 2)"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      alice ##> "/tail @cath 2"
      alice <# "@cath -> from you"
      alice <## "      hi"
      alice <# "@cath -> from bob (contact id: 2)"
      alice <## "      hey"

      cath ##> "/tail @alice 2"
      cath <# "alice> -> forwarded"
      cath <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardContactToGroup :: HasCallStack => FilePath -> IO ()
testForwardContactToGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      createGroup2 "team" alice cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` ">> @bob -> #team hi"
      alice <# "#team -> from you"
      alice <## "      hi"
      cath <# "#team alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> @bob -> #team hey"
      alice <# "#team -> from bob (contact id: 2)"
      alice <## "      hey"
      cath <# "#team alice> -> forwarded"
      cath <## "      hey"

testForwardContactToNotes :: HasCallStack => FilePath -> IO ()
testForwardContactToNotes =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createCCNoteFolder alice
      connectUsers alice bob

      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` ">> @bob -> * hi"
      alice <# "* -> from you"
      alice <## "      hi"

      alice `send` "> @bob -> * hey"
      alice <# "* -> from bob (contact id: 2)"
      alice <## "      hey"

testForwardGroupToContact :: HasCallStack => FilePath -> IO ()
testForwardGroupToContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      connectUsers alice cath

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      alice `send` "> #team -> @cath hi"
      alice <# "@cath -> from you"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> #team -> @cath hey"
      alice <# "@cath -> from #team (group id: 1)"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

testForwardGroupToGroup :: HasCallStack => FilePath -> IO ()
testForwardGroupToGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      createGroup2 "club" alice cath

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      alice `send` "> #team -> #club hi"
      alice <# "#club -> from you"
      alice <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> #team -> #club hey"
      alice <# "#club -> from #team (group id: 1)"
      alice <## "      hey"
      cath <# "#club alice> -> forwarded"
      cath <## "      hey"

testForwardGroupToNotes :: HasCallStack => FilePath -> IO ()
testForwardGroupToNotes =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createCCNoteFolder alice
      createGroup2 "team" alice bob

      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"

      alice `send` "> #team -> * hi"
      alice <# "* -> from you"
      alice <## "      hi"

      alice `send` "> #team -> * hey"
      alice <# "* -> from #team (group id: 1)"
      alice <## "      hey"

testForwardNotesToContact :: HasCallStack => FilePath -> IO ()
testForwardNotesToContact =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      connectUsers alice cath

      alice /* "hi"

      alice `send` "> * -> @cath hi"
      alice <# "@cath hi"
      cath <# "alice> hi"

testForwardNotesToGroup :: HasCallStack => FilePath -> IO ()
testForwardNotesToGroup =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      createGroup2 "team" alice cath

      alice /* "hi"

      alice `send` "> * -> #team hi"
      alice <# "#team hi"
      cath <# "#team alice> hi"

testForwardNotesToNotes :: HasCallStack => FilePath -> IO ()
testForwardNotesToNotes tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    createCCNoteFolder alice

    alice /* "hi"

    alice `send` "> * -> * hi"
    alice <# "* hi"

    alice ##> "/tail * 2"
    alice <# "* hi"
    alice <# "* hi"

testForwardPreserveInfo :: HasCallStack => FilePath -> IO ()
testForwardPreserveInfo =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createCCNoteFolder alice
      connectUsers alice bob
      connectUsers alice cath
      createGroup2 "team" alice dan

      bob #> "@alice hey"
      alice <# "bob> hey"

      alice `send` "> @bob -> * hey"
      alice <# "* -> from bob (contact id: 2)"
      alice <## "      hey"

      alice `send` "> * -> @cath hey"
      alice <# "@cath -> from bob (contact id: 2)"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      alice `send` ">> @cath -> #team hey"
      alice <# "#team -> from bob (contact id: 2)"
      alice <## "      hey"
      dan <# "#team alice> -> forwarded"
      dan <## "      hey"

testForwardFileNoFilesFolder :: HasCallStack => FilePath -> IO ()
testForwardFileNoFilesFolder =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      connectUsers alice bob
      connectUsers bob cath

      -- send original file
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}"
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
      bob `send` "> @alice -> @cath hi"
      bob <# "@cath -> from alice (contact id: 2)"
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

testForwardFileContactToContact :: HasCallStack => FilePath -> IO ()
testForwardFileContactToContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/fixtures" "./tests/tmp/alice_xftp"
      setRelativePaths bob "./tests/tmp/bob_files" "./tests/tmp/bob_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      connectUsers alice bob
      connectUsers bob cath

      -- send original file
      alice ##> "/_send @2 json {\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}"
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
      bob `send` "> @alice -> @cath hi"
      bob <# "@cath -> from alice (contact id: 2)"
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

testForwardFileGroupToNotes :: HasCallStack => FilePath -> IO ()
testForwardFileGroupToNotes =
  testChat2 aliceProfile cathProfile $
    \alice cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/fixtures" "./tests/tmp/alice_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      createGroup2 "team" alice cath
      createCCNoteFolder cath

      -- send original file
      alice ##> "/_send #1 json {\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}"
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
      cath `send` "> #team -> * hi"
      cath <# "* -> from #team (group id: 1)"
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

testForwardFileNotesToGroup :: HasCallStack => FilePath -> IO ()
testForwardFileNotesToGroup =
  testChat2 aliceProfile cathProfile $
    \alice cath -> withXFTPServer $ do
      setRelativePaths alice "./tests/tmp/alice_files" "./tests/tmp/alice_xftp"
      setRelativePaths cath "./tests/tmp/cath_files" "./tests/tmp/cath_xftp"
      copyFile "./tests/fixtures/test.pdf" "./tests/tmp/alice_files/test.pdf"
      createCCNoteFolder alice
      createGroup2 "team" alice cath

      -- create original file
      alice ##> "/_create *1 json {\"filePath\": \"test.pdf\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi\"}}"
      alice <# "* hi"
      alice <# "* file 1 (test.pdf)"

      -- forward file
      alice `send` "> * -> #team hi"
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
