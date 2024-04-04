{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Forward where

import ChatClient
import ChatTests.Utils
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
    it "from notes to notes" testForwardNotesToNotes

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
      alice <# "@cath -> forwarded"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> @bob -> @cath hey"
      alice <# "@cath -> forwarded"
      alice <## "      hey"
      cath <# "alice> -> forwarded"
      cath <## "      hey"

      alice ##> "/tail @cath 2"
      alice <# "@cath -> forwarded"
      alice <## "      hi"
      alice <# "@cath -> forwarded"
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
      alice <# "#team -> forwarded"
      alice <## "      hi"
      cath <# "#team alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> @bob -> #team hey"
      alice <# "#team -> forwarded"
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
      alice <# "* -> forwarded"
      alice <## "      hi"

      alice `send` "> @bob -> * hey"
      alice <# "* -> forwarded"
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
      alice <# "@cath -> forwarded"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> #team -> @cath hey"
      alice <# "@cath -> forwarded"
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
      alice <# "#club -> forwarded"
      alice <## "      hi"
      cath <# "#club alice> -> forwarded"
      cath <## "      hi"

      alice `send` "> #team -> #club hey"
      alice <# "#club -> forwarded"
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
      alice <# "* -> forwarded"
      alice <## "      hi"

      alice `send` "> #team -> * hey"
      alice <# "* -> forwarded"
      alice <## "      hey"

testForwardNotesToContact :: HasCallStack => FilePath -> IO ()
testForwardNotesToContact =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      connectUsers alice cath

      alice /* "hi"

      alice `send` "> * -> @cath hi"
      alice <# "@cath -> forwarded"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

testForwardNotesToGroup :: HasCallStack => FilePath -> IO ()
testForwardNotesToGroup =
  testChat2 aliceProfile cathProfile $
    \alice cath -> do
      createCCNoteFolder alice
      createGroup2 "team" alice cath

      alice /* "hi"

      alice `send` "> * -> #team hi"
      alice <# "#team -> forwarded"
      alice <## "      hi"
      cath <# "#team alice> -> forwarded"
      cath <## "      hi"

testForwardNotesToNotes :: HasCallStack => FilePath -> IO ()
testForwardNotesToNotes tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    createCCNoteFolder alice

    alice /* "hi"

    alice `send` "> * -> * hi"
    alice <# "* -> forwarded"
    alice <## "      hi"

    alice ##> "/tail * 2"
    alice <# "* hi"
    alice <# "* -> forwarded"
    alice <## "      hi"
