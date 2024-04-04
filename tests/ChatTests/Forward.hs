{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Forward where

import ChatClient
import ChatTests.Utils
import Test.Hspec hiding (it)

chatForwardTests :: SpecWith FilePath
chatForwardTests = do
  describe "forward messages" $ do
    it "from contact to contact" testForwardContactContact

testForwardContactContact :: HasCallStack => FilePath -> IO ()
testForwardContactContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob cath

      alice #> "@bob hi"
      bob <# "alice> hi"
      msgId <- lastItemId alice

      alice #> "@bob hey"
      bob <# "alice> hey"

      alice ##> ("/_forward @2 @3 " <> msgId)
      alice <# "@cath -> forwarded"
      alice <## "      hi"
      cath <# "alice> -> forwarded"
      cath <## "      hi"

      bob `send` "> @alice -> @cath hey"
      bob <# "@cath -> forwarded"
      bob <## "      hey"
      cath <# "bob> -> forwarded"
      cath <## "      hey"
