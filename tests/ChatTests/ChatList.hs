{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module ChatTests.ChatList where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Simplex.Chat.Controller (ChatResponse (CRApiChat, CRApiChats))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.Messages (AChat (..), Chat (..), ChatStats (..))
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Types (IntDef (..))
import Test.Hspec hiding (it)

chatListTests :: SpecWith TestParams
chatListTests = do
  it "get last chats" testPaginationLast
  it "get last chats with max chats option" testMaxChats
  it "get chats before/after timestamp" testPaginationTs
  it "filter by search query" testFilterSearch
  it "filter favorite" testFilterFavorite
  it "filter unread" testFilterUnread
  it "filter unread without chats with only silent unread items" testFilterUnreadSilent
  it "filter favorite or unread" testFilterFavoriteOrUnread
  it "sort and filter chats of all types" testPaginationAllChatTypes

testPaginationLast :: HasCallStack => TestParams -> IO ()
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

testMaxChats :: HasCallStack => TestParams -> IO ()
testMaxChats =
  testChatOpts3 testOpts {coreOptions = testCoreOpts {maxChats = 1}} aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      cath <##> alice

      alice ##> "/chats all"
      alice <# "@cath hey"
      alice ##> "/chats 2"
      alice <# "bob> hey"
      alice <# "@cath hey"
      alice #$> ("/_get chats 1 pcc=on", chats, [("@cath", "hey")])
      getChats_ alice "count=2" [("@cath", "hey"), ("@bob", "hey")]

testPaginationTs :: HasCallStack => TestParams -> IO ()
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

testFilterSearch :: HasCallStack => TestParams -> IO ()
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

testFilterFavorite :: HasCallStack => TestParams -> IO ()
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

testFilterUnread :: HasCallStack => TestParams -> IO ()
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

testFilterUnreadSilent :: HasCallStack => TestParams -> IO ()
testFilterUnreadSilent =
  testChatOpts2 testOpts {markRead = False} aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      let query = "{\"type\": \"filters\", \"favorite\": false, \"unread\": true}"

      bob ##> "/_send @2 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"working\"}, \"priority\": -1}]"
      bob <# "@alice working"
      alice <# "bob> working"
      bob ##> "/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"working\"}, \"priority\": -1}]"
      bob <# "#team working"
      alice <# "#team bob> working"
      getChats_ alice query []
      unreadCounts alice `shouldReturn` [(1, 1), (1, 1)]
      chatUnreadCounts alice "#1" `shouldReturn` (1, 1)
      chatUnreadCounts alice "@2" `shouldReturn` (1, 1)

      bob #> "#team done"
      alice <# "#team bob> done"
      getChats_ alice query [("#team", "done")]
      unreadCounts alice `shouldReturn` [(2, 1), (1, 1)]
      chatUnreadCounts alice "#1" `shouldReturn` (2, 1)

      doneId <- lastItemId alice
      alice #$> ("/_read chat items #1 " <> doneId, id, "items read for chat")
      getChats_ alice query []
      unreadCounts alice `shouldReturn` [(1, 1), (1, 1)]
      alice #$> ("/_read chat @2", id, "ok")
      unreadCounts alice `shouldReturn` [(1, 1), (0, 0)]

unreadCounts :: TestCC -> IO [(Int, Int)]
unreadCounts TestCC {chatController = cc} =
  sendChatCmdStr cc "/_get chats 1" >>= \case
    Right (CRApiChats _ cs) -> pure [(unreadCount, n) | AChat _ Chat {chatStats = ChatStats {unreadCount, unreadSilent = IntDef n}} <- cs]
    r -> fail $ show r

chatUnreadCounts :: TestCC -> String -> IO (Int, Int)
chatUnreadCounts TestCC {chatController = cc} chatRef =
  sendChatCmdStr cc ("/_get chat " <> chatRef <> " initial=10") >>= \case
    Right (CRApiChat _ (AChat _ Chat {chatStats = ChatStats {unreadCount, unreadSilent = IntDef n}}) _) -> pure (unreadCount, n)
    r -> fail $ show r

testFilterFavoriteOrUnread :: HasCallStack => TestParams -> IO ()
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

testPaginationAllChatTypes :: HasCallStack => TestParams -> IO ()
testPaginationAllChatTypes =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      ts1 <- iso8601Show <$> getCurrentTime

      -- @bob
      connectUsers alice bob
      alice <##> bob

      ts2 <- iso8601Show <$> getCurrentTime

      -- <@cath
      alice ##> "/ad"
      cLink <- getContactLink alice True
      cath ##> ("/c " <> cLink)
      alice <#? cath

      ts3 <- iso8601Show <$> getCurrentTime

      -- :3
      alice ##> "/c"
      _ <- getInvitation alice

      ts4 <- iso8601Show <$> getCurrentTime

      -- #team
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"

      ts5 <- iso8601Show <$> getCurrentTime

      -- @dan
      connectUsers alice dan
      alice <##> dan

      _ts6 <- iso8601Show <$> getCurrentTime

      -- \* (notes)
      createCCNoteFolder alice
      alice >* "psst"

      ts7 <- iso8601Show <$> getCurrentTime

      getChats_ alice "count=10" [("*", "psst"), ("@dan", "hey"), ("#team", "Chat with admins: on"), (":3", ""), ("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
      getChats_ alice "count=3" [("*", "psst"), ("@dan", "hey"), ("#team", "Chat with admins: on")]
      getChats_ alice ("after=" <> ts2 <> " count=2") [(":3", ""), ("@cath", "Audio/video calls: enabled")]
      getChats_ alice ("before=" <> ts5 <> " count=2") [("#team", "Chat with admins: on"), (":3", "")]
      getChats_ alice ("after=" <> ts3 <> " count=10") [("*", "psst"), ("@dan", "hey"), ("#team", "Chat with admins: on"), (":3", "")]
      getChats_ alice ("before=" <> ts4 <> " count=10") [(":3", ""), ("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
      getChats_ alice ("after=" <> ts1 <> " count=10") [("*", "psst"), ("@dan", "hey"), ("#team", "Chat with admins: on"), (":3", ""), ("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
      getChats_ alice ("before=" <> ts7 <> " count=10") [("*", "psst"), ("@dan", "hey"), ("#team", "Chat with admins: on"), (":3", ""), ("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
      getChats_ alice ("after=" <> ts7 <> " count=10") []
      getChats_ alice ("before=" <> ts1 <> " count=10") []

      let queryFavorite = "{\"type\": \"filters\", \"favorite\": true, \"unread\": false}"
      getChats_ alice queryFavorite []

      alice ##> "/_settings @2 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"
      alice ##> "/_settings #1 {\"enableNtfs\":\"all\",\"favorite\":true}"
      alice <## "ok"

      getChats_ alice queryFavorite [("#team", "Chat with admins: on"), ("@bob", "hey")]
      getChats_ alice ("before=" <> ts4 <> " count=1 " <> queryFavorite) [("@bob", "hey")]
      getChats_ alice ("before=" <> ts5 <> " count=1 " <> queryFavorite) [("#team", "Chat with admins: on")]
      getChats_ alice ("after=" <> ts1 <> " count=1 " <> queryFavorite) [("@bob", "hey")]
      getChats_ alice ("after=" <> ts4 <> " count=1 " <> queryFavorite) [("#team", "Chat with admins: on")]

      let queryUnread = "{\"type\": \"filters\", \"favorite\": false, \"unread\": true}"

      getChats_ alice queryUnread []
      getChats_ alice ("before=" <> ts2 <> " count=10 " <> queryUnread) []
      getChats_ alice ("after=" <> ts2 <> " count=10 " <> queryUnread) []
