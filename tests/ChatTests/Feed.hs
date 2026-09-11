{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Feed where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Controller (ChatConfig (..))
import Test.Hspec hiding (it)

chatFeedTests :: SpecWith TestParams
chatFeedTests = do
  describe "feed" $ do
    it "broadcast to contacts and customer groups in several buckets" testFeedBuckets
    it "edit and delete the broadcast in every chat" testFeedEditDelete
    it "recipient with dropped feed receives nothing" testFeedDropped
    it "instance edited in its chat is detached from the feed" testFeedDetached
    it "broadcast a file to all contacts" testFeedFile
    it "incognito contact is excluded from the broadcast" testFeedSkipsIncognito

feedTestCfg :: ChatConfig
feedTestCfg = testCfg {feedBucketSize = 1}

testFeedBuckets :: HasCallStack => TestParams -> IO ()
testFeedBuckets =
  testChatCfg5 feedTestCfg businessProfile aliceProfile bobProfile cathProfile danProfile $
    \biz alice bob cath dan -> do
      createCCFeed biz
      connectUsers biz alice
      connectUsers biz bob
      biz ##> "/ad"
      cLink <- getContactLink biz True
      biz ##> "/auto_accept on business"
      biz <## "auto_accept on, business"
      connectToBusiness biz cath cLink "cath" "Catherine"
      connectToBusiness biz dan cLink "dan" "Daniel"

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      alice <# "biz> hello everyone"
      bob <# "biz> hello everyone"
      cath <# "#biz biz_1> hello everyone"
      dan <# "#biz biz_1> hello everyone"

      chatItems biz "%1" 10 `shouldReturn` [(1, "hello everyone")]
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello everyone")]
      chatItems biz "@3" 1 `shouldReturn` [(1, "hello everyone")]
      chatItems biz "#1" 1 `shouldReturn` [(1, "hello everyone")]
      chatItems biz "#2" 1 `shouldReturn` [(1, "hello everyone")]

testFeedEditDelete :: HasCallStack => TestParams -> IO ()
testFeedEditDelete =
  testChatCfg4 feedTestCfg businessProfile aliceProfile bobProfile cathProfile $
    \biz alice bob cath -> do
      createCCFeed biz
      connectUsers biz alice
      connectUsers biz bob
      biz ##> "/ad"
      cLink <- getContactLink biz True
      biz ##> "/auto_accept on business"
      biz <## "auto_accept on, business"
      connectToBusiness biz cath cLink "cath" "Catherine"

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      alice <# "biz> hello everyone"
      bob <# "biz> hello everyone"
      cath <# "#biz biz_1> hello everyone"

      biz ##> "! % (hello everyone) hello again"
      biz <# "% [edited] hello again"
      alice <# "biz> [edited] hello again"
      bob <# "biz> [edited] hello again"
      cath <# "#biz biz_1> [edited] hello again"
      chatItems biz "%1" 10 `shouldReturn` [(1, "hello again")]
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello again")]
      chatItems biz "@3" 1 `shouldReturn` [(1, "hello again")]
      chatItems biz "#1" 1 `shouldReturn` [(1, "hello again")]

      biz ##> "\\ % hello again"
      biz <### [ConsoleString "message being deleted", ConsoleString "message deleted"]
      alice <# "biz> [marked deleted] hello again"
      bob <# "biz> [marked deleted] hello again"
      cath <# "#biz biz_1> [marked deleted] hello again"
      chatItems biz "%1" 10 `shouldReturn` []
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello again [marked deleted]")]
      chatItems biz "#1" 1 `shouldReturn` [(1, "hello again [marked deleted]")]

testFeedDropped :: HasCallStack => TestParams -> IO ()
testFeedDropped =
  testChatCfg3 feedTestCfg businessProfile aliceProfile bobProfile $
    \biz alice bob -> do
      createCCFeed biz
      connectUsers biz alice
      connectUsers biz bob

      bob ##> "/feed drop @biz on"
      bob <## "ok"

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      alice <# "biz> hello everyone"
      (bob </)
      chatItems bob "@2" 1 `shouldReturn` [(0, "Audio/video calls: enabled")]

      biz ##> "! % (hello everyone) hello again"
      biz <# "% [edited] hello again"
      alice <# "biz> [edited] hello again"
      (bob </)

      biz ##> "\\ % hello again"
      biz <### [ConsoleString "message being deleted", ConsoleString "message deleted"]
      alice <# "biz> [marked deleted] hello again"
      (bob </)
      chatItems bob "@2" 1 `shouldReturn` [(0, "Audio/video calls: enabled")]

testFeedDetached :: HasCallStack => TestParams -> IO ()
testFeedDetached =
  testChatCfg3 feedTestCfg businessProfile aliceProfile bobProfile $
    \biz alice bob -> do
      createCCFeed biz
      connectUsers biz alice
      connectUsers biz bob

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      alice <# "biz> hello everyone"
      bob <# "biz> hello everyone"

      biz ##> "! @alice (hello everyone) hello alice"
      biz <# "@alice [edited] hello alice"
      alice <# "biz> [edited] hello alice"

      biz ##> "! % (hello everyone) hello again"
      biz <# "% [edited] hello again"
      bob <# "biz> [edited] hello again"
      (alice </)

      chatItems biz "%1" 10 `shouldReturn` [(1, "hello again")]
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello alice")]
      chatItems biz "@3" 1 `shouldReturn` [(1, "hello again")]

testFeedFile :: HasCallStack => TestParams -> IO ()
testFeedFile =
  testChatCfg3 feedTestCfg businessProfile aliceProfile bobProfile $
    \biz alice bob -> withXFTPServer $ do
      createCCFeed biz
      connectUsers biz alice
      connectUsers biz bob

      biz #> "/f % ./tests/fixtures/test.pdf"
      biz <## "use /fc 1 to cancel sending"
      alice <# "biz> sends file test.pdf (266.0 KiB / 272376 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob <# "biz> sends file test.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      biz <## "completed uploading file 1 (test.pdf) for %"

      alice ##> "/fr 1 ./tests/tmp"
      alice
        <### [ "saving file 1 from biz to ./tests/tmp/test.pdf",
               "started receiving file 1 (test.pdf) from biz"
             ]
      alice <## "completed receiving file 1 (test.pdf) from biz"

      bob ##> "/fr 1 ./tests/tmp/bob_test.pdf"
      bob
        <### [ "saving file 1 from biz to ./tests/tmp/bob_test.pdf",
               "started receiving file 1 (test.pdf) from biz"
             ]
      bob <## "completed receiving file 1 (test.pdf) from biz"

      src <- B.readFile "./tests/fixtures/test.pdf"
      B.readFile "./tests/tmp/test.pdf" `shouldReturn` src
      B.readFile "./tests/tmp/bob_test.pdf" `shouldReturn` src

testFeedSkipsIncognito :: HasCallStack => TestParams -> IO ()
testFeedSkipsIncognito =
  testChatCfg3 feedTestCfg businessProfile aliceProfile bobProfile $
    \biz alice bob -> do
      createCCFeed biz
      connectUsers biz alice

      biz ##> "/c i"
      inv <- getInvitation biz
      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      bizIncognito <- getTermLine biz
      concurrentlyN_
        [ bob <## (bizIncognito <> ": contact is connected"),
          do
            biz <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> bizIncognito)
            biz <## "use /i bob to print out this incognito profile again"
        ]

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      alice <# "biz> hello everyone"
      (bob </)

chatItems :: HasCallStack => TestCC -> String -> Int -> IO [(Int, String)]
chatItems cc chatRef count = do
  cc ##> ("/_get chat " <> chatRef <> " count=" <> show count)
  chat <$> getTermLine cc

connectToBusiness :: HasCallStack => TestCC -> TestCC -> String -> String -> String -> IO ()
connectToBusiness biz cc cLink name fullName = do
  cc ##> ("/c " <> cLink)
  cc <## "connection request sent!"
  biz <## ("#" <> name <> " (" <> fullName <> "): accepting business address request...")
  cc <## "#biz: joining the group..."
  biz <## ("#" <> name <> ": " <> name <> "_1 joined the group")
  cc <## "#biz: you joined the group"
