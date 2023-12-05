module ChatTests.ChatList where

import ChatClient
import ChatTests.Utils
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Simplex.Chat.Types (ConnStatus (..))
import Test.Hspec

chatListTests :: SpecWith FilePath
chatListTests = focus $ do
  it "should get last chats" testChatListPaginationLast
  it "should get chats around timestamp" testChatListPaginationTs
  it "should filter chats by search query" testChatListSearch

testChatListPaginationLast :: HasCallStack => FilePath -> IO ()
testChatListPaginationLast tmp =
  withNewTestChatCfg tmp testCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp testCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfg "cath" cathProfile $ \cath -> do
        connectUsers alice bob
        alice <##> bob
        connectUsers alice cath
        cath <##> alice

        alice ##> "/chats 0"
        alice ##> "/chats 1"
        alice <# "@cath hey"
        alice ##> "/chats 2"
        alice <# "bob> hey"
        alice <# "@cath hey"

testChatListPaginationTs :: HasCallStack => FilePath -> IO ()
testChatListPaginationTs tmp =
  withNewTestChatCfg tmp testCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp testCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfg "cath" cathProfile $ \cath -> do
        tsStart <- iso8601Show <$> getCurrentTime
        connectUsers alice bob
        alice <##> bob
        tsAliceBob <- iso8601Show <$> getCurrentTime
        connectUsers alice cath
        cath <##> alice
        tsFinish <- iso8601Show <$> getCurrentTime
        -- syntax smoke check
        getChats_ "count=0" id alice []
        getChats_ ("after=" <> tsFinish <> " count=2") id alice []
        getChats_ ("before=" <> tsFinish <> " count=0") id alice []
        -- limited reads
        getChats_ "count=1" id alice [("@cath", "hey", Just ConnReady)]
        getChats_ ("after=" <> tsStart <> " count=1") id alice [("@cath", "hey", Just ConnReady)]
        getChats_ ("before=" <> tsFinish <> " count=1") id alice [("@cath", "hey", Just ConnReady)]
        -- interval bounds
        getChats_ ("after=" <> tsAliceBob <> " count=10") id alice [("@cath", "hey", Just ConnReady)]
        getChats_ ("before=" <> tsAliceBob <> " count=10") id alice [("@bob", "hey", Just ConnReady)]

testChatListSearch :: HasCallStack => FilePath -> IO ()
testChatListSearch tmp =
  withNewTestChatCfg tmp testCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp testCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfg "cath" cathProfile $ \cath -> do
        connectUsers alice bob
        alice <##> bob
        connectUsers alice cath
        cath <##> alice
        getChats_ "count=1 search=Alice" id alice []
        getChats_ "count=1 search=bob" id alice [("@bob", "hey", Just ConnReady)]
        getChats_ "count=1 search=Bob" id alice [("@bob", "hey", Just ConnReady)]
