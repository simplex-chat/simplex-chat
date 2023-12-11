module ChatTests.ChatList where

import ChatClient
import ChatTests.Utils
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Test.Hspec

chatListTests :: SpecWith FilePath
chatListTests = do
  it "get last chats" testPaginationLast
  it "get chats before/after timestamp" testPaginationTs
  it "filter by search query" testFilterSearch
  it "filter favorite" testFilterFavorite
  it "filter unread" testFilterUnread
  it "filter favorite or unread" testFilterFavoriteOrUnread

testPaginationLast :: HasCallStack => FilePath -> IO ()
testPaginationLast =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
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
testPaginationTs =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      tsStart <- iso8601Show <$> getCurrentTime
      connectUsers alice bob
      alice <##> bob
      tsAliceBob <- iso8601Show <$> getCurrentTime
      connectUsers alice cath
      cath <##> alice
      tsFinish <- iso8601Show <$> getCurrentTime
      -- syntax smoke check
      getChats_ alice "count=0" []
      getChats_ alice ("after=" <> tsFinish <> " count=2") []
      getChats_ alice ("before=" <> tsFinish <> " count=0") []
      -- limited reads
      getChats_ alice "count=1" [("@cath", "hey")]
      getChats_ alice ("after=" <> tsStart <> " count=1") [("@bob", "hey")]
      getChats_ alice ("before=" <> tsFinish <> " count=1") [("@cath", "hey")]
      -- interval bounds
      getChats_ alice ("after=" <> tsAliceBob <> " count=10") [("@cath", "hey")]
      getChats_ alice ("before=" <> tsAliceBob <> " count=10") [("@bob", "hey")]

getChats_ :: HasCallStack => TestCC -> String -> [(String, String)] -> Expectation
getChats_ cc query expected = do
  cc #$> ("/_get chats 1 pcc=on " <> query, chats, expected)

testFilterSearch :: HasCallStack => FilePath -> IO ()
testFilterSearch =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      let query s = "count=1 {\"type\": \"search\", \"search\": \"" <> s <> "\"}"

      getChats_ alice (query "abc") []
      getChats_ alice (query "alice") []
      getChats_ alice (query "bob") [("@bob", "hey")]
      getChats_ alice (query "Bob") [("@bob", "hey")]

testFilterFavorite :: HasCallStack => FilePath -> IO ()
testFilterFavorite =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      let query = "{\"type\": \"filters\", \"favorite\": true, \"unread\": false}"

      -- no favorite chats
      getChats_ alice query []

      -- 1 favorite chat
      alice ##> "/_settings @2 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ alice query [("@bob", "hey")]

      -- 1 favorite chat, unread chat not included
      alice ##> "/_unread chat @3 on"
      alice <## "ok"
      getChats_ alice query [("@bob", "hey")]

testFilterUnread :: HasCallStack => FilePath -> IO ()
testFilterUnread =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      let query = "{\"type\": \"filters\", \"favorite\": false, \"unread\": true}"

      -- no unread chats
      getChats_ alice query []

      -- 1 unread chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ alice query [("@bob", "hey")]

      -- 1 unread chat, favorite chat not included
      alice ##> "/_settings @3 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ alice query [("@bob", "hey")]

testFilterFavoriteOrUnread :: HasCallStack => FilePath -> IO ()
testFilterFavoriteOrUnread =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      let query = "{\"type\": \"filters\", \"favorite\": true, \"unread\": true}"

      -- no favorite or unread chats
      getChats_ alice query []

      -- 1 unread chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ alice query [("@bob", "hey")]

      -- 1 favorite chat
      alice ##> "/_unread chat @2 off"
      alice <## "ok"
      alice ##> "/_settings @3 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ alice query [("@cath", "hey")]

      -- 1 unread chat, 1 favorite chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ alice query [("@cath", "hey"), ("@bob", "hey")]

-- testPaginationAllChatTypes :: HasCallStack => FilePath -> IO ()
-- testPaginationAllChatTypes =
--   testChat3 aliceProfile bobProfile cathProfile $
--     \alice bob cath -> do
--       connectUsers alice bob
--       alice <##> bob
--       connectUsers alice cath
--       cath <##> alice
