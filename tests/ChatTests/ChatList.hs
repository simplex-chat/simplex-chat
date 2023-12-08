module ChatTests.ChatList where

import ChatClient
import ChatTests.Utils
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Simplex.Chat.Types (ConnStatus (..))
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
      getChats_ "count=0" id alice []
      getChats_ ("after=" <> tsFinish <> " count=2") id alice []
      getChats_ ("before=" <> tsFinish <> " count=0") id alice []
      -- limited reads
      getChats_ "count=1" id alice [("@cath", "hey", Just ConnReady)]
      getChats_ ("after=" <> tsStart <> " count=1") id alice [("@bob", "hey", Just ConnReady)]
      getChats_ ("before=" <> tsFinish <> " count=1") id alice [("@cath", "hey", Just ConnReady)]
      -- interval bounds
      getChats_ ("after=" <> tsAliceBob <> " count=10") id alice [("@cath", "hey", Just ConnReady)]
      getChats_ ("before=" <> tsAliceBob <> " count=10") id alice [("@bob", "hey", Just ConnReady)]

testFilterSearch :: HasCallStack => FilePath -> IO ()
testFilterSearch =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      let query s = "count=1 {\"type\": \"search\", \"search\": \"" <> s <> "\"}"

      getChats_ (query "abc") id alice []
      getChats_ (query "alice") id alice []
      getChats_ (query "bob") id alice [("@bob", "hey", Just ConnReady)]
      getChats_ (query "Bob") id alice [("@bob", "hey", Just ConnReady)]

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
      getChats_ query id alice []

      -- 1 favorite chat
      alice ##> "/_settings @2 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady)]

      -- 1 favorite chat, unread chat not included
      alice ##> "/_unread chat @3 on"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady)]

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
      getChats_ query id alice []

      -- 1 unread chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady)]

      -- 1 unread chat, favorite chat not included
      alice ##> "/_settings @3 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady)]

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
      getChats_ query id alice []

      -- 1 unread chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady)]

      -- 1 favorite chat
      alice ##> "/_unread chat @2 off"
      alice <## "ok"
      alice ##> "/_settings @3 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      getChats_ query id alice [("@cath", "hey", Just ConnReady)]

      -- 1 unread chat, 1 favorite chat
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      getChats_ query id alice [("@bob", "hey", Just ConnReady), ("@cath", "hey", Just ConnReady)]
