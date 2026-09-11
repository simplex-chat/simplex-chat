{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Feed where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Simplex.Chat.Controller (ChatConfig (..))
import Test.Hspec hiding (it)

chatFeedTests :: SpecWith TestParams
chatFeedTests = do
  describe "feed" $ do
    it "broadcast to contacts and customer groups in several buckets" testFeedBuckets
    it "edit and delete the broadcast in every chat" testFeedEditDelete

-- one recipient per bucket, so each stream runs three buckets for two recipients
feedTestCfg :: ChatConfig
feedTestCfg = testCfg {feedBucketSize = 1}

testFeedBuckets :: HasCallStack => TestParams -> IO ()
testFeedBuckets =
  testChatCfg5 feedTestCfg businessProfile aliceProfile bobProfile cathProfile danProfile $
    \biz alice bob cath dan -> do
      createCCFeed biz
      -- two contacts
      connectUsers biz alice
      connectUsers biz bob
      -- two customer groups
      biz ##> "/ad"
      cLink <- getContactLink biz True
      biz ##> "/auto_accept on business"
      biz <## "auto_accept on, business"
      connectToBusiness biz cath cLink "cath" "Catherine"
      connectToBusiness biz dan cLink "dan" "Daniel"

      biz `send` "/feed hello everyone"
      biz <# "% hello everyone"
      -- the contacts stream and the customer groups stream deliver concurrently
      alice <# "biz> hello everyone"
      bob <# "biz> hello everyone"
      cath <# "#biz biz_1> hello everyone"
      dan <# "#biz biz_1> hello everyone"

      -- the broadcast is one item in the feed and the last item of every recipient chat
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

      -- the edit applies to the instance of every chat
      biz ##> "! % (hello everyone) hello again"
      biz <# "% [edited] hello again"
      alice <# "biz> [edited] hello again"
      bob <# "biz> [edited] hello again"
      cath <# "#biz biz_1> [edited] hello again"
      chatItems biz "%1" 10 `shouldReturn` [(1, "hello again")]
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello again")]
      chatItems biz "@3" 1 `shouldReturn` [(1, "hello again")]
      chatItems biz "#1" 1 `shouldReturn` [(1, "hello again")]

      -- the feed item is removed when every instance is marked deleted
      biz ##> "\\ % hello again"
      -- the response of the command and the event of the last job
      biz <### [ConsoleString "message being deleted", ConsoleString "message deleted"]
      alice <# "biz> [marked deleted] hello again"
      bob <# "biz> [marked deleted] hello again"
      cath <# "#biz biz_1> [marked deleted] hello again"
      chatItems biz "%1" 10 `shouldReturn` []
      chatItems biz "@2" 1 `shouldReturn` [(1, "hello again [marked deleted]")]
      chatItems biz "#1" 1 `shouldReturn` [(1, "hello again [marked deleted]")]

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
