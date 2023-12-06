module ChatTests.ChatList where

import ChatClient
import ChatTests.Utils
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Simplex.Chat.Types (ConnStatus (..))
import Test.Hspec

chatListTests :: SpecWith FilePath
chatListTests = do
  it "should get last chats" testPaginationLast
  it "should get chats around timestamp" testPaginationTs
  it "should filter chats by search query" testSearch
  it "should filter chats by fav/unread" testFavUnread
  it "should filter chats by search over fav/unread" testSearchFavUnread

testPaginationLast :: HasCallStack => FilePath -> IO ()
testPaginationLast tmp =
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

testPaginationTs :: HasCallStack => FilePath -> IO ()
testPaginationTs tmp =
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

testSearch :: HasCallStack => FilePath -> IO ()
testSearch tmp =
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

testFavUnread :: HasCallStack => FilePath -> IO ()
testFavUnread tmp =
  withNewTestChatCfg tmp testCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp testCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfg "cath" cathProfile $ \cath -> do
        connectUsers alice bob
        alice <##> bob
        connectUsers alice cath
        cath <##> alice
        getChats_ "unread=on" id alice [] -- everything is read by cli interface
        getChats_ "favorite=on" id alice []
        alice ##> "/_settings @2 {\"enableNtfs\":\"all\",\"favorite\":true}"
        alice <## "ok"
        getChats_ "favorite=on" id alice [("@bob", "hey", Just ConnReady)]
        alice ##> "/_unread chat @3 on"
        alice <## "ok"
        getChats_ "favorite=on unread=on" id alice [("@bob", "hey", Just ConnReady), ("@cath", "hey", Just ConnReady)]
        alice ##> "/_unread chat @3 off"
        alice <## "ok"
        getChats_ "favorite=on unread=on" id alice [("@bob", "hey", Just ConnReady)]

testSearchFavUnread :: HasCallStack => FilePath -> IO ()
testSearchFavUnread tmp =
  withNewTestChatCfg tmp testCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp testCfg "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice <##> bob
      getChats_ "unread=on" id alice []
      getChats_ "favorite=on" id alice []
      getChats_ "favorite=on unread=on search=Bob" id alice [("@bob", "hey", Just ConnReady)]
