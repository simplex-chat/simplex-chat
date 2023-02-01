{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}

module ChatTests.Direct where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forM_)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Simplex.Chat.Call
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Types (authErrDisableCount, sameVerificationCode, verificationCode)
import qualified Simplex.Messaging.Crypto as C
import System.Directory (copyFile, doesDirectoryExist, doesFileExist)
import Test.Hspec

chatDirectTests :: SpecWith FilePath
chatDirectTests = do
  describe "direct messages" $ do
    describe "add contact and send/receive message" testAddContact
    it "deleting contact deletes profile" testDeleteContactDeletesProfile
    it "direct message quoted replies" testDirectMessageQuotedReply
    it "direct message update" testDirectMessageUpdate
    it "direct message delete" testDirectMessageDelete
    it "direct live message" testDirectLiveMessage
    it "repeat AUTH errors disable contact" testRepeatAuthErrorsDisableContact
  describe "SMP servers" $ do
    it "get and set SMP servers" testGetSetSMPServers
    it "test SMP server connection" testTestSMPServerConnection
  describe "async connection handshake" $ do
    it "connect when initiating client goes offline" testAsyncInitiatingOffline
    it "connect when accepting client goes offline" testAsyncAcceptingOffline
    describe "connect, fully asynchronous (when clients are never simultaneously online)" $ do
      it "v2" testFullAsync
  describe "webrtc calls api" $ do
    it "negotiate call" testNegotiateCall
  describe "maintenance mode" $ do
    it "start/stop/export/import chat" testMaintenanceMode
    it "export/import chat with files" testMaintenanceModeWithFiles
    it "encrypt/decrypt database" testDatabaseEncryption
  describe "mute/unmute messages" $ do
    it "mute/unmute contact" testMuteContact
    it "mute/unmute group" testMuteGroup
  describe "multiple users" $ do
    it "create second user" testCreateSecondUser
    it "multiple users subscribe and receive messages after restart" testUsersSubscribeAfterRestart
    it "both users have contact link" testMultipleUserAddresses
    it "create user with default servers" testCreateUserDefaultServers
    it "create user with same servers" testCreateUserSameServers
    it "delete user" testDeleteUser
    it "users have different chat item TTL configuration, chat items expire" testUsersDifferentCIExpirationTTL
    it "chat items expire after restart for all users according to per user configuration" testUsersRestartCIExpiration
    it "chat items only expire for users who configured expiration" testEnableCIExpirationOnlyForOneUser
    it "disabling chat item expiration doesn't disable it for other users" testDisableCIExpirationOnlyForOneUser
    it "both users have configured timed messages with contacts, messages expire, restart" testUsersTimedMessages
  describe "chat item expiration" $ do
    it "set chat item TTL" testSetChatItemTTL
  describe "queue rotation" $ do
    it "switch contact to a different queue" testSwitchContact
    it "switch group member to a different queue" testSwitchGroupMember
  describe "connection verification code" $ do
    it "verificationCode function converts ByteString to series of digits" $ \_ ->
      verificationCode (C.sha256Hash "abcd") `shouldBe` "61889 38426 63934 09576 96390 79389 84124 85253 63658 69469 70853 37788 95900 68296 20156 25"
    it "sameVerificationCode function should ignore spaces" $ \_ ->
      sameVerificationCode "123 456 789" "12345 6789" `shouldBe` True
    it "mark contact verified" testMarkContactVerified
    it "mark group member verified" testMarkGroupMemberVerified

testAddContact :: HasCallStack => SpecWith FilePath
testAddContact = versionTestMatrix2 runTestAddContact
  where
    runTestAddContact alice bob = do
      alice ##> "/_connect 1"
      inv <- getInvitation alice
      bob ##> ("/_connect 1 " <> inv)
      bob <## "confirmation sent!"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      threadDelay 100000
      chatsEmpty alice bob
      alice #> "@bob hello there 🙂"
      bob <# "alice> hello there 🙂"
      alice ##> "/_unread chat @2 on"
      alice <## "ok"
      alice ##> "/_unread chat @2 off"
      alice <## "ok"
      chatsOneMessage alice bob
      bob #> "@alice hello there"
      alice <# "bob> hello there"
      bob #> "@alice how are you?"
      alice <# "bob> how are you?"
      chatsManyMessages alice bob
      -- test adding the same contact one more time - local name will be different
      alice ##> "/c"
      inv' <- getInvitation alice
      bob ##> ("/c " <> inv')
      bob <## "confirmation sent!"
      concurrently_
        (bob <## "alice_1 (Alice): contact is connected")
        (alice <## "bob_1 (Bob): contact is connected")
      alice #> "@bob_1 hello"
      bob <# "alice_1> hello"
      bob #> "@alice_1 hi"
      alice <# "bob_1> hi"
      alice @@@ [("@bob_1", "hi"), ("@bob", "how are you?")]
      bob @@@ [("@alice_1", "hi"), ("@alice", "how are you?")]
      -- test deleting contact
      alice ##> "/d bob_1"
      alice <## "bob_1: contact is deleted"
      alice ##> "@bob_1 hey"
      alice <## "no contact bob_1"
      alice @@@ [("@bob", "how are you?")]
      alice `hasContactProfiles` ["alice", "bob"]
      bob @@@ [("@alice_1", "hi"), ("@alice", "how are you?")]
      bob `hasContactProfiles` ["alice", "alice", "bob"]
      -- test clearing chat
      alice #$> ("/clear bob", id, "bob: all messages are removed locally ONLY")
      alice #$> ("/_get chat @2 count=100", chat, [])
      bob #$> ("/clear alice", id, "alice: all messages are removed locally ONLY")
      bob #$> ("/_get chat @2 count=100", chat, [])
    chatsEmpty alice bob = do
      alice @@@ [("@bob", "Voice messages: enabled")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures)
      bob @@@ [("@alice", "Voice messages: enabled")]
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures)
    chatsOneMessage alice bob = do
      alice @@@ [("@bob", "hello there 🙂")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hello there 🙂")])
      bob @@@ [("@alice", "hello there 🙂")]
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hello there 🙂")])
    chatsManyMessages alice bob = do
      alice @@@ [("@bob", "how are you?")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hello there 🙂"), (0, "hello there"), (0, "how are you?")])
      bob @@@ [("@alice", "how are you?")]
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hello there 🙂"), (1, "hello there"), (1, "how are you?")])
      -- pagination
      alice #$> ("/_get chat @2 after=" <> itemId 1 <> " count=100", chat, [(0, "hello there"), (0, "how are you?")])
      alice #$> ("/_get chat @2 before=" <> itemId 2 <> " count=100", chat, chatFeatures <> [(1, "hello there 🙂")])
      -- search
      alice #$> ("/_get chat @2 count=100 search=ello ther", chat, [(1, "hello there 🙂"), (0, "hello there")])
      -- read messages
      alice #$> ("/_read chat @2 from=1 to=100", id, "ok")
      bob #$> ("/_read chat @2 from=1 to=100", id, "ok")
      alice #$> ("/_read chat @2", id, "ok")
      bob #$> ("/_read chat @2", id, "ok")

testDeleteContactDeletesProfile :: HasCallStack => FilePath -> IO ()
testDeleteContactDeletesProfile =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob
      -- alice deletes contact, profile is deleted
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      alice ##> "/_contacts 1"
      (alice </)
      alice `hasContactProfiles` ["alice"]
      -- bob deletes contact, profile is deleted
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      bob ##> "/contacts"
      (bob </)
      bob `hasContactProfiles` ["bob"]

testDirectMessageQuotedReply :: HasCallStack => FilePath -> IO ()
testDirectMessageQuotedReply =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 text hello! how are you?"
      alice <# "@bob hello! how are you?"
      bob <# "alice> hello! how are you?"
      bob #> "@alice hi!"
      alice <# "bob> hi!"
      bob `send` "> @alice (hello) all good - you?"
      bob <# "@alice > hello! how are you?"
      bob <## "      all good - you?"
      alice <# "bob> > hello! how are you?"
      alice <## "      all good - you?"
      bob #$> ("/_get chat @2 count=1", chat', [((1, "all good - you?"), Just (0, "hello! how are you?"))])
      alice #$> ("/_get chat @2 count=1", chat', [((0, "all good - you?"), Just (1, "hello! how are you?"))])
      bob `send` ">> @alice (all good) will tell more"
      bob <# "@alice >> all good - you?"
      bob <## "      will tell more"
      alice <# "bob> >> all good - you?"
      alice <## "      will tell more"
      bob #$> ("/_get chat @2 count=1", chat', [((1, "will tell more"), Just (1, "all good - you?"))])
      alice #$> ("/_get chat @2 count=1", chat', [((0, "will tell more"), Just (0, "all good - you?"))])

testDirectMessageUpdate :: HasCallStack => FilePath -> IO ()
testDirectMessageUpdate =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      -- msg id 1
      alice #> "@bob hello 🙂"
      bob <# "alice> hello 🙂"

      -- msg id 2
      bob `send` "> @alice (hello) hi alice"
      bob <# "@alice > hello 🙂"
      bob <## "      hi alice"
      alice <# "bob> > hello 🙂"
      alice <## "      hi alice"

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "hello 🙂"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hello 🙂"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂"))])

      alice ##> ("/_update item @2 " <> itemId 1 <> " text hey 👋")
      alice <# "@bob [edited] hey 👋"
      bob <# "alice> [edited] hey 👋"

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂"))])

      -- msg id 3
      bob `send` "> @alice (hey) hey alice"
      bob <# "@alice > hey 👋"
      bob <## "      hey alice"
      alice <# "bob> > hey 👋"
      alice <## "      hey alice"

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂")), ((0, "hey alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂")), ((1, "hey alice"), Just (0, "hey 👋"))])

      alice ##> ("/_update item @2 " <> itemId 1 <> " text greetings 🤝")
      alice <# "@bob [edited] greetings 🤝"
      bob <# "alice> [edited] greetings 🤝"

      alice #$> ("/_update item @2 " <> itemId 2 <> " text updating bob's message", id, "cannot update this item")

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂")), ((0, "hey alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂")), ((1, "hey alice"), Just (0, "hey 👋"))])

      bob ##> ("/_update item @2 " <> itemId 2 <> " text hey Alice")
      bob <# "@alice [edited] > hello 🙂"
      bob <## "      hey Alice"
      alice <# "bob> [edited] > hello 🙂"
      alice <## "      hey Alice"

      bob ##> ("/_update item @2 " <> itemId 3 <> " text greetings Alice")
      bob <# "@alice [edited] > hey 👋"
      bob <## "      greetings Alice"
      alice <# "bob> [edited] > hey 👋"
      alice <## "      greetings Alice"

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "greetings 🤝"), Nothing), ((0, "hey Alice"), Just (1, "hello 🙂")), ((0, "greetings Alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "greetings 🤝"), Nothing), ((1, "hey Alice"), Just (0, "hello 🙂")), ((1, "greetings Alice"), Just (0, "hey 👋"))])

testDirectMessageDelete :: HasCallStack => FilePath -> IO ()
testDirectMessageDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      -- alice, bob: msg id 1
      alice #> "@bob hello 🙂"
      bob <# "alice> hello 🙂"

      -- alice, bob: msg id 2
      bob `send` "> @alice (hello 🙂) hey alic"
      bob <# "@alice > hello 🙂"
      bob <## "      hey alic"
      alice <# "bob> > hello 🙂"
      alice <## "      hey alic"

      -- alice: deletes msg ids 1,2
      alice #$> ("/_delete item @2 " <> itemId 1 <> " internal", id, "message deleted")
      alice #$> ("/_delete item @2 " <> itemId 2 <> " internal", id, "message deleted")

      alice @@@ [("@bob", "Voice messages: enabled")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures)

      -- alice: msg id 1
      bob ##> ("/_update item @2 " <> itemId 2 <> " text hey alice")
      bob <# "@alice [edited] > hello 🙂"
      bob <## "      hey alice"
      alice <# "bob> [edited] hey alice"
      alice @@@ [("@bob", "hey alice")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hey alice")])

      -- bob: marks deleted msg id 2
      bob #$> ("/_delete item @2 " <> itemId 2 <> " broadcast", id, "message marked deleted")
      bob @@@ [("@alice", "hey alice [marked deleted]")]
      alice <# "bob> [marked deleted] hey alice"
      alice @@@ [("@bob", "hey alice [marked deleted]")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hey alice [marked deleted]")])

      -- alice: deletes msg id 1 that was broadcast deleted by bob
      alice #$> ("/_delete item @2 " <> itemId 1 <> " internal", id, "message deleted")
      alice @@@ [("@bob", "Voice messages: enabled")]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures)

      -- alice: msg id 1, bob: msg id 3 (quoting message alice deleted locally)
      bob `send` "> @alice (hello 🙂) do you receive my messages?"
      bob <# "@alice > hello 🙂"
      bob <## "      do you receive my messages?"
      alice <# "bob> > hello 🙂"
      alice <## "      do you receive my messages?"
      alice @@@ [("@bob", "do you receive my messages?")]
      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "do you receive my messages?"), Just (1, "hello 🙂"))])
      alice #$> ("/_delete item @2 " <> itemId 1 <> " broadcast", id, "cannot delete this item")

      -- alice: msg id 2, bob: msg id 4
      bob #> "@alice how are you?"
      alice <# "bob> how are you?"

      -- alice: deletes msg id 2
      alice #$> ("/_delete item @2 " <> itemId 2 <> " internal", id, "message deleted")

      -- bob: marks deleted msg id 4 (that alice deleted locally)
      bob #$> ("/_delete item @2 " <> itemId 4 <> " broadcast", id, "message marked deleted")
      alice <## "bob> [deleted - original message not found]"

      alice @@@ [("@bob", "do you receive my messages?")]
      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "do you receive my messages?"), Just (1, "hello 🙂"))])
      bob @@@ [("@alice", "how are you? [marked deleted]")]
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hello 🙂"), Nothing), ((1, "hey alice [marked deleted]"), Just (0, "hello 🙂")), ((1, "do you receive my messages?"), Just (0, "hello 🙂")), ((1, "how are you? [marked deleted]"), Nothing)])

      -- bob: deletes msg ids 2,4 (that he has marked deleted)
      bob #$> ("/_delete item @2 " <> itemId 2 <> " internal", id, "message deleted")
      bob #$> ("/_delete item @2 " <> itemId 4 <> " internal", id, "message deleted")
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hello 🙂"), Nothing), ((1, "do you receive my messages?"), Just (0, "hello 🙂"))])

testDirectLiveMessage :: HasCallStack => FilePath -> IO ()
testDirectLiveMessage =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    -- non-empty live message is sent instantly
    alice `send` "/live @bob hello"
    bob <# "alice> [LIVE started] use /show [on/off/4] hello"
    alice ##> ("/_update item @2 " <> itemId 1 <> " text hello there")
    alice <# "@bob [LIVE] hello there"
    bob <# "alice> [LIVE ended] hello there"
    -- empty live message is also sent instantly
    alice `send` "/live @bob"
    bob <# "alice> [LIVE started] use /show [on/off/5]"
    alice ##> ("/_update item @2 " <> itemId 2 <> " text hello 2")
    alice <# "@bob [LIVE] hello 2"
    bob <# "alice> [LIVE ended] hello 2"

testRepeatAuthErrorsDisableContact :: HasCallStack => FilePath -> IO ()
testRepeatAuthErrorsDisableContact =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice <##> bob
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    forM_ [1 .. authErrDisableCount] $ \_ -> sendAuth alice
    alice <## "[bob] connection is disabled, to enable: /enable bob, to delete: /d bob"
    alice ##> "@bob hey"
    alice <## "bob: disabled, to enable: /enable bob, to delete: /d bob"
    alice ##> "/enable bob"
    alice <## "ok"
    sendAuth alice
  where
    sendAuth alice = do
      alice #> "@bob hey"
      alice <## "[bob, contactId: 2, connId: 1] error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"

testGetSetSMPServers :: HasCallStack => FilePath -> IO ()
testGetSetSMPServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/_smp 1", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")
      alice #$> ("/smp smp://1234-w==@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==@smp1.example.im")
      alice #$> ("/smp smp://1234-w==:password@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==:password@smp1.example.im")
      alice #$> ("/smp smp://2345-w==@smp2.example.im;smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")
      alice #$> ("/smp default", id, "ok")
      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")

testTestSMPServerConnection :: HasCallStack => FilePath -> IO ()
testTestSMPServerConnection =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice ##> "/smp test 1 smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:7001"
      alice <## "SMP server test passed"
      -- to test with password:
      -- alice <## "SMP server test failed at CreateQueue, error: SMP AUTH"
      -- alice <## "Server requires authorization to create queues, check password"
      alice ##> "/smp test 1 smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"
      alice <## "SMP server test passed"
      alice ##> "/smp test 1 smp://LcJU@localhost:7001"
      alice <## "SMP server test failed at Connect, error: BROKER smp://LcJU@localhost:7001 NETWORK"
      alice <## "Possibly, certificate fingerprint in server address is incorrect"

testAsyncInitiatingOffline :: HasCallStack => FilePath -> IO ()
testAsyncInitiatingOffline tmp = do
  putStrLn "testAsyncInitiatingOffline"
  inv <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    alice ##> "/c"
    getInvitation alice
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    withTestChat tmp "alice" $ \alice -> do
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

testAsyncAcceptingOffline :: HasCallStack => FilePath -> IO ()
testAsyncAcceptingOffline tmp = do
  putStrLn "testAsyncAcceptingOffline"
  inv <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    alice ##> "/c"
    getInvitation alice
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
  withTestChat tmp "alice" $ \alice -> do
    withTestChat tmp "bob" $ \bob -> do
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

testFullAsync :: HasCallStack => FilePath -> IO ()
testFullAsync tmp = do
  putStrLn "testFullAsync"
  inv <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    alice ##> "/c"
    getInvitation alice
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
  withTestChat tmp "alice" $ \_ -> pure () -- connecting... notification in UI
  withTestChat tmp "bob" $ \_ -> pure () -- connecting... notification in UI
  withTestChat tmp "alice" $ \alice -> do
    alice <## "1 contacts connected (use /cs for the list)"
    alice <## "bob (Bob): contact is connected"
  withTestChat tmp "bob" $ \bob -> do
    bob <## "1 contacts connected (use /cs for the list)"
    bob <## "alice (Alice): contact is connected"

testFullAsyncV1 :: HasCallStack => FilePath -> IO ()
testFullAsyncV1 tmp = do
  putStrLn "testFullAsyncV1"
  inv <- withNewAlice $ \alice -> do
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewBob $ \bob -> do
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
  putStrLn "6"
  withAlice $ \_ -> pure ()
  putStrLn "7"
  withBob $ \_ -> pure ()
  putStrLn "8"
  withAlice $ \alice -> do
    putStrLn "9"
    alice <## "1 contacts connected (use /cs for the list)"
  putStrLn "10"
  withBob $ \_ -> pure ()
  putStrLn "11"
  withAlice $ \alice -> do
    putStrLn "12"
    alice <## "1 contacts connected (use /cs for the list)"
    putStrLn "13"
    alice <## "bob (Bob): contact is connected"
  putStrLn "14"
  withBob $ \bob -> do
    putStrLn "15"
    bob <## "1 contacts connected (use /cs for the list)"
    putStrLn "16"
    bob <## "alice (Alice): contact is connected"
  where
    withNewAlice = withNewTestChatV1 tmp "alice" aliceProfile
    withAlice = withTestChatV1 tmp "alice"
    withNewBob = withNewTestChatV1 tmp "bob" bobProfile
    withBob = withTestChatV1 tmp "bob"

testFullAsyncV1toV2 :: HasCallStack => FilePath -> IO ()
testFullAsyncV1toV2 tmp = do
  putStrLn "testFullAsyncV1toV2"
  inv <- withNewAlice $ \alice -> do
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewBob $ \bob -> do
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
  withAlice $ \_ -> pure ()
  putStrLn "6"
  withBob $ \_ -> pure ()
  putStrLn "7"
  withAlice $ \alice -> do
    putStrLn "8"
    alice <## "1 contacts connected (use /cs for the list)"
  putStrLn "9"
  withBob $ \_ -> pure ()
  putStrLn "10"
  withAlice $ \alice -> do
    putStrLn "11"
    alice <## "1 contacts connected (use /cs for the list)"
    putStrLn "12"
    alice <## "bob (Bob): contact is connected"
  putStrLn "13"
  withBob $ \bob -> do
    putStrLn "14"
    bob <## "1 contacts connected (use /cs for the list)"
    putStrLn "15"
    bob <## "alice (Alice): contact is connected"
  where
    withNewAlice = withNewTestChat tmp "alice" aliceProfile
    withAlice = withTestChat tmp "alice"
    withNewBob = withNewTestChatV1 tmp "bob" bobProfile
    withBob = withTestChatV1 tmp "bob"

testFullAsyncV2toV1 :: HasCallStack => FilePath -> IO ()
testFullAsyncV2toV1 tmp = do
  putStrLn "testFullAsyncV2toV1"
  inv <- withNewAlice $ \alice -> do
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewBob $ \bob -> do
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
  putStrLn "6"
  withAlice $ \_ -> pure ()
  putStrLn "7"
  withBob $ \_ -> pure ()
  putStrLn "8"
  withAlice $ \alice -> do
    putStrLn "9"
    alice <## "1 contacts connected (use /cs for the list)"
  putStrLn "10"
  withBob $ \_ -> pure ()
  putStrLn "11"
  withAlice $ \alice -> do
    putStrLn "12"
    alice <## "1 contacts connected (use /cs for the list)"
    putStrLn "13"
    alice <## "bob (Bob): contact is connected"
  putStrLn "14"
  withBob $ \bob -> do
    putStrLn "15"
    bob <## "1 contacts connected (use /cs for the list)"
    putStrLn "16"
    bob <## "alice (Alice): contact is connected"
  where
    withNewAlice = withNewTestChatV1 tmp "alice" aliceProfile
    {-# INLINE withNewAlice #-}
    withAlice = withTestChatV1 tmp "alice"
    {-# INLINE withAlice #-}
    withNewBob = withNewTestChat tmp "bob" bobProfile
    {-# INLINE withNewBob #-}
    withBob = withTestChat tmp "bob"
    {-# INLINE withBob #-}

testCallType :: CallType
testCallType = CallType {media = CMVideo, capabilities = CallCapabilities {encryption = True}}

testWebRTCSession :: WebRTCSession
testWebRTCSession =
  WebRTCSession
    { rtcSession = "{}",
      rtcIceCandidates = "[]"
    }

testWebRTCCallOffer :: WebRTCCallOffer
testWebRTCCallOffer =
  WebRTCCallOffer
    { callType = testCallType,
      rtcSession = testWebRTCSession
    }

serialize :: ToJSON a => a -> String
serialize = B.unpack . LB.toStrict . J.encode

repeatM_ :: Int -> IO a -> IO ()
repeatM_ n a = forM_ [1 .. n] $ const a

testNegotiateCall :: HasCallStack => FilePath -> IO ()
testNegotiateCall =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    -- just for testing db query
    alice ##> "/_call get"
    -- alice invite bob to call
    alice ##> ("/_call invite @2 " <> serialize testCallType)
    alice <## "ok"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: calling...")])
    bob <## "alice wants to connect with you via WebRTC video call (e2e encrypted)"
    repeatM_ 3 $ getTermLine bob
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: calling...")])
    -- bob accepts call by sending WebRTC offer
    bob ##> ("/_call offer @2 " <> serialize testWebRTCCallOffer)
    bob <## "ok"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: accepted")])
    alice <## "bob accepted your WebRTC video call (e2e encrypted)"
    repeatM_ 3 $ getTermLine alice
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: accepted")])
    -- alice confirms call by sending WebRTC answer
    alice ##> ("/_call answer @2 " <> serialize testWebRTCSession)
    alice <## "ok"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: connecting...")])
    bob <## "alice continued the WebRTC call"
    repeatM_ 3 $ getTermLine bob
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: connecting...")])
    -- participants can update calls as connected
    alice ##> "/_call status @2 connected"
    alice <## "ok"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: in progress (00:00)")])
    bob ##> "/_call status @2 connected"
    bob <## "ok"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: in progress (00:00)")])
    -- either party can end the call
    bob ##> "/_call end @2"
    bob <## "ok"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: ended (00:00)")])
    alice <## "call with bob ended"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: ended (00:00)")])

testMaintenanceMode :: HasCallStack => FilePath -> IO ()
testMaintenanceMode tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatOpts tmp testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
      alice ##> "/c"
      alice <## "error: chat not started"
      alice ##> "/_start"
      alice <## "chat started"
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      alice ##> "/_db export {\"archivePath\": \"./tests/tmp/alice-chat.zip\"}"
      alice <## "error: chat not stopped"
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/_start"
      alice <## "chat started"
      -- chat works after start
      alice <## "1 contacts connected (use /cs for the list)"
      alice #> "@bob hi again"
      bob <# "alice> hi again"
      bob #> "@alice hello"
      alice <# "bob> hello"
      -- export / delete / import
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/_db export {\"archivePath\": \"./tests/tmp/alice-chat.zip\"}"
      alice <## "ok"
      doesFileExist "./tests/tmp/alice-chat.zip" `shouldReturn` True
      alice ##> "/_db import {\"archivePath\": \"./tests/tmp/alice-chat.zip\"}"
      alice <## "ok"
      -- cannot start chat after import
      alice ##> "/_start"
      alice <## "error: chat store changed, please restart chat"
    -- works after full restart
    withTestChat tmp "alice" $ \alice -> testChatWorking alice bob

testChatWorking :: HasCallStack => TestCC -> TestCC -> IO ()
testChatWorking alice bob = do
  alice <## "1 contacts connected (use /cs for the list)"
  alice #> "@bob hello again"
  bob <# "alice> hello again"
  bob #> "@alice hello too"
  alice <# "bob> hello too"

testMaintenanceModeWithFiles :: HasCallStack => FilePath -> IO ()
testMaintenanceModeWithFiles tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatOpts tmp testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      alice ##> "/_files_folder ./tests/tmp/alice_files"
      alice <## "ok"
      connectUsers alice bob
      startFileTransferWithDest' bob alice "test.jpg" "136.5 KiB / 139737 bytes" Nothing
      bob <## "completed sending file 1 (test.jpg) to alice"
      alice <## "completed receiving file 1 (test.jpg) from bob"
      src <- B.readFile "./tests/fixtures/test.jpg"
      B.readFile "./tests/tmp/alice_files/test.jpg" `shouldReturn` src
      threadDelay 500000
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/_db export {\"archivePath\": \"./tests/tmp/alice-chat.zip\"}"
      alice <## "ok"
      alice ##> "/_db delete"
      alice <## "ok"
      -- cannot start chat after delete
      alice ##> "/_start"
      alice <## "error: chat store changed, please restart chat"
      doesDirectoryExist "./tests/tmp/alice_files" `shouldReturn` False
      alice ##> "/_db import {\"archivePath\": \"./tests/tmp/alice-chat.zip\"}"
      alice <## "ok"
      B.readFile "./tests/tmp/alice_files/test.jpg" `shouldReturn` src
    -- works after full restart
    withTestChat tmp "alice" $ \alice -> testChatWorking alice bob

testDatabaseEncryption :: HasCallStack => FilePath -> IO ()
testDatabaseEncryption tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatOpts tmp testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      alice ##> "/db encrypt mykey"
      alice <## "error: chat not stopped"
      alice ##> "/db decrypt mykey"
      alice <## "error: chat not stopped"
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/db decrypt mykey"
      alice <## "error: chat database is not encrypted"
      alice ##> "/db encrypt mykey"
      alice <## "ok"
      alice ##> "/_start"
      alice <## "error: chat store changed, please restart chat"
    withTestChatOpts tmp testOpts {maintenance = True, dbKey = "mykey"} "alice" $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      testChatWorking alice bob
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/db key wrongkey nextkey"
      alice <## "error encrypting database: wrong passphrase or invalid database file"
      alice ##> "/db key mykey nextkey"
      alice <## "ok"
      alice ##> "/_db encryption {\"currentKey\":\"nextkey\",\"newKey\":\"anotherkey\"}"
      alice <## "ok"
    withTestChatOpts tmp testOpts {maintenance = True, dbKey = "anotherkey"} "alice" $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      testChatWorking alice bob
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/db decrypt anotherkey"
      alice <## "ok"
    withTestChat tmp "alice" $ \alice -> testChatWorking alice bob

testMuteContact :: HasCallStack => FilePath -> IO ()
testMuteContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob ##> "/mute alice"
      bob <## "ok"
      alice #> "@bob hi"
      (bob </)
      bob ##> "/contacts"
      bob <## "alice (Alice) (muted, you can /unmute @alice)"
      bob ##> "/unmute alice"
      bob <## "ok"
      bob ##> "/contacts"
      bob <## "alice (Alice)"
      alice #> "@bob hi again"
      bob <# "alice> hi again"

testMuteGroup :: HasCallStack => FilePath -> IO ()
testMuteGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")
      bob ##> "/mute #team"
      bob <## "ok"
      alice #> "#team hi"
      concurrently_
        (bob </)
        (cath <# "#team alice> hi")
      bob ##> "/gs"
      bob <## "#team (muted, you can /unmute #team)"
      bob ##> "/unmute #team"
      bob <## "ok"
      alice #> "#team hi again"
      concurrently_
        (bob <# "#team alice> hi again")
        (cath <# "#team alice> hi again")
      bob ##> "/gs"
      bob <## "#team"

testCreateSecondUser :: HasCallStack => FilePath -> IO ()
testCreateSecondUser =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      -- connect using second user
      connectUsers alice bob
      alice #> "@bob hello"
      bob <# "alisa> hello"
      bob #> "@alisa hey"
      alice <# "bob> hey"

      alice ##> "/user"
      showActiveUser alice "alisa"

      alice ##> "/users"
      alice <## "alice (Alice)"
      alice <## "alisa (active)"

      -- receive message to first user
      bob #> "@alice hey alice"
      (alice, "alice") $<# "bob> hey alice"

      connectUsers alice cath

      -- set active user to first user
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice ##> "/user"
      showActiveUser alice "alice (Alice)"

      alice ##> "/users"
      alice <## "alice (Alice) (active)"
      alice <## "alisa"

      alice <##> bob

      cath #> "@alisa hey alisa"
      (alice, "alisa") $<# "cath> hey alisa"
      alice ##> "@cath hi cath"
      alice <## "no contact cath"

      -- set active user to second user
      alice ##> "/_user 2"
      showActiveUser alice "alisa"

testUsersSubscribeAfterRestart :: HasCallStack => FilePath -> IO ()
testUsersSubscribeAfterRestart tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      connectUsers alice bob
      alice <##> bob

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      connectUsers alice bob
      alice <##> bob

    withTestChat tmp "alice" $ \alice -> do
      -- second user is active
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "[user: alice] 1 contacts connected (use /cs for the list)"

      -- second user receives message
      alice <##> bob

      -- first user receives message
      bob #> "@alice hey alice"
      (alice, "alice") $<# "bob> hey alice"

testMultipleUserAddresses :: HasCallStack => FilePath -> IO ()
testMultipleUserAddresses =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      alice ##> "/ad"
      cLinkAlice <- getContactLink alice True
      bob ##> ("/c " <> cLinkAlice)
      alice <#? bob
      alice @@@ [("<@bob", "")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      threadDelay 100000
      alice @@@ [("@bob", "Voice messages: enabled")]
      alice <##> bob

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      -- connect using second user address
      alice ##> "/ad"
      cLinkAlisa <- getContactLink alice True
      bob ##> ("/c " <> cLinkAlisa)
      alice <#? bob
      alice #$> ("/_get chats 2 pcc=on", chats, [("<@bob", "")])
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alisa: contact is connected")
        (alice <## "bob (Bob): contact is connected")
      threadDelay 100000
      alice #$> ("/_get chats 2 pcc=on", chats, [("@bob", "Voice messages: enabled")])
      alice <##> bob

      bob #> "@alice hey alice"
      (alice, "alice") $<# "bob> hey alice"

      -- delete first user address
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice ##> "/da"
      alice <## "Your chat address is deleted - accepted contacts will remain connected."
      alice <## "To create a new chat address use /ad"

      -- second user receives request when not active
      cath ##> ("/c " <> cLinkAlisa)
      cath <## "connection request sent!"
      alice <## "[user: alisa] cath (Catherine) wants to connect to you!"
      alice <## "to accept: /ac cath"
      alice <## "to reject: /rc cath (the sender will NOT be notified)"

      -- accept request
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alisa: contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      threadDelay 100000
      alice #$> ("/_get chats 2 pcc=on", chats, [("@cath", "Voice messages: enabled"), ("@bob", "hey")])
      alice <##> cath

      -- first user doesn't have cath as contact
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice @@@ [("@bob", "hey alice")]

testCreateUserDefaultServers :: HasCallStack => FilePath -> IO ()
testCreateUserDefaultServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp smp://2345-w==@smp2.example.im;smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")

      -- with same_smp=off
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")

      alice ##> "/create user same_smp=off alisa2"
      showActiveUser alice "alisa2"

      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")

testCreateUserSameServers :: HasCallStack => FilePath -> IO ()
testCreateUserSameServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp smp://2345-w==@smp2.example.im;smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")

      alice ##> "/create user same_smp=on alisa"
      showActiveUser alice "alisa"

      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")

testDeleteUser :: HasCallStack => FilePath -> IO ()
testDeleteUser =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      connectUsers alice bob

      -- cannot delete active user

      alice ##> "/_delete user 1 del_smp=off"
      alice <## "cannot delete active user"

      -- delete user without deleting SMP queues

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      connectUsers alice cath
      alice <##> cath

      alice ##> "/users"
      alice <## "alice (Alice)"
      alice <## "alisa (active)"

      alice ##> "/_delete user 1 del_smp=off"
      alice <## "ok"

      alice ##> "/users"
      alice <## "alisa (active)"

      bob #> "@alice hey"
      -- no connection authorization error - connection wasn't deleted
      (alice </)

      -- cannot delete new active user

      alice ##> "/delete user alisa"
      alice <## "cannot delete active user"

      alice ##> "/users"
      alice <## "alisa (active)"

      alice <##> cath

      -- delete user deleting SMP queues

      alice ##> "/create user alisa2"
      showActiveUser alice "alisa2"

      connectUsers alice dan
      alice <##> dan

      alice ##> "/users"
      alice <## "alisa"
      alice <## "alisa2 (active)"

      alice ##> "/delete user alisa"
      alice <### ["ok", "completed deleting user"]

      alice ##> "/users"
      alice <## "alisa2 (active)"

      cath #> "@alisa hey"
      cath <## "[alisa, contactId: 2, connId: 1] error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"
      (alice </)

      alice <##> dan

testUsersDifferentCIExpirationTTL :: HasCallStack => FilePath -> IO ()
testUsersDifferentCIExpirationTTL tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      -- first user messages
      connectUsers alice bob

      alice #> "@bob alice 1"
      bob <# "alice> alice 1"
      bob #> "@alice alice 2"
      alice <# "bob> alice 2"

      -- second user messages
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      connectUsers alice bob

      alice #> "@bob alisa 1"
      bob <# "alisa> alisa 1"
      bob #> "@alisa alisa 2"
      alice <# "bob> alisa 2"

      -- set ttl for first user
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_ttl 1 1", id, "ok")

      -- set ttl for second user
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_ttl 2 3", id, "ok")

      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 1 second(s)")

      alice #> "@bob alice 3"
      bob <# "alice> alice 3"
      bob #> "@alice alice 4"
      alice <# "bob> alice 4"

      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "alice 1"), (0, "alice 2"), (1, "alice 3"), (0, "alice 4")])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 3 second(s)")

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      -- messages both before and after setting chat item ttl are deleted
      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    cfg = testCfg {ciExpirationInterval = 500000}

testUsersRestartCIExpiration :: HasCallStack => FilePath -> IO ()
testUsersRestartCIExpiration tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      -- set ttl for first user
      alice #$> ("/_ttl 1 1", id, "ok")
      connectUsers alice bob

      -- create second user and set ttl
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_ttl 2 3", id, "ok")
      connectUsers alice bob

      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice #> "@bob alice 1"
      bob <# "alice> alice 1"
      bob #> "@alice alice 2"
      alice <# "bob> alice 2"

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      alice #> "@bob alisa 1"
      bob <# "alisa> alisa 1"
      bob #> "@alisa alisa 2"
      alice <# "bob> alisa 2"

      -- first user will be active on restart
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

    withTestChatCfg tmp cfg "alice" $ \alice -> do
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "[user: alisa] 1 contacts connected (use /cs for the list)"

      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 1 second(s)")

      alice #> "@bob alice 3"
      bob <# "alice> alice 3"
      bob #> "@alice alice 4"
      alice <# "bob> alice 4"

      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "alice 1"), (0, "alice 2"), (1, "alice 3"), (0, "alice 4")])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 3 second(s)")

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      -- messages both before and after restart are deleted
      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    cfg = testCfg {ciExpirationInterval = 500000}

testEnableCIExpirationOnlyForOneUser :: HasCallStack => FilePath -> IO ()
testEnableCIExpirationOnlyForOneUser tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      -- first user messages
      connectUsers alice bob

      alice #> "@bob alice 1"
      bob <# "alice> alice 1"
      bob #> "@alice alice 2"
      alice <# "bob> alice 2"

      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "alice 1"), (0, "alice 2")])

      -- second user messages before first user sets ttl
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      connectUsers alice bob

      alice #> "@bob alisa 1"
      bob <# "alisa> alisa 1"
      bob #> "@alisa alisa 2"
      alice <# "bob> alisa 2"

      -- set ttl for first user
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_ttl 1 1", id, "ok")

      -- second user messages after first user sets ttl
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      -- messages are deleted for first user
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      -- messages are not deleted for second user
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

    withTestChatCfg tmp cfg "alice" $ \alice -> do
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "[user: alice] 1 contacts connected (use /cs for the list)"

      -- messages are not deleted for second user after restart
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      alice #> "@bob alisa 5"
      bob <# "alisa> alisa 5"
      bob #> "@alisa alisa 6"
      alice <# "bob> alisa 6"

      threadDelay 2000000

      -- new messages are not deleted for second user
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4"), (1, "alisa 5"), (0, "alisa 6")])
  where
    cfg = testCfg {ciExpirationInterval = 500000}

testDisableCIExpirationOnlyForOneUser :: HasCallStack => FilePath -> IO ()
testDisableCIExpirationOnlyForOneUser tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      -- set ttl for first user
      alice #$> ("/_ttl 1 1", id, "ok")
      connectUsers alice bob

      -- create second user and set ttl
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_ttl 2 1", id, "ok")
      connectUsers alice bob

      -- first user disables expiration
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/ttl none", id, "ok")
      alice #$> ("/ttl", id, "old messages are not being deleted")

      -- second user still has ttl configured
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 1 second(s)")

      alice #> "@bob alisa 1"
      bob <# "alisa> alisa 1"
      bob #> "@alisa alisa 2"
      alice <# "bob> alisa 2"

      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2")])

      threadDelay 2000000

      -- second user messages are deleted
      alice #$> ("/_get chat @4 count=100", chat, [])

    withTestChatCfg tmp cfg "alice" $ \alice -> do
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "[user: alice] 1 contacts connected (use /cs for the list)"

      -- second user still has ttl configured after restart
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 1 second(s)")

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 3"), (0, "alisa 4")])

      threadDelay 2000000

      -- second user messages are deleted
      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    cfg = testCfg {ciExpirationInterval = 500000}

testUsersTimedMessages :: HasCallStack => FilePath -> IO ()
testUsersTimedMessages tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      connectUsers alice bob
      configureTimedMessages alice bob "2" "1"

      -- create second user and configure timed messages for contact
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      connectUsers alice bob
      configureTimedMessages alice bob "4" "2"

      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice #> "@bob alice 1"
      bob <# "alice> alice 1"
      bob #> "@alice alice 2"
      alice <# "bob> alice 2"

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      alice #> "@bob alisa 1"
      bob <# "alisa> alisa 1"
      bob #> "@alisa alisa 2"
      alice <# "bob> alisa 2"

      -- messages are deleted after ttl
      threadDelay 500000

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [(1, "alice 1"), (0, "alice 2")])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 1"), (0, "alisa 2")])

      threadDelay 1000000

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 1"), (0, "alisa 2")])

      threadDelay 1000000

      alice ##> "/user"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [])

      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice #> "@bob alice 3"
      bob <# "alice> alice 3"
      bob #> "@alice alice 4"
      alice <# "bob> alice 4"

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

    withTestChat tmp "alice" $ \alice -> do
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "[user: alice] 1 contacts connected (use /cs for the list)"

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [(1, "alice 3"), (0, "alice 4")])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 3"), (0, "alisa 4")])

      -- messages are deleted after restart
      threadDelay 1000000

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 3"), (0, "alisa 4")])

      threadDelay 1000000

      alice ##> "/user"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    configureTimedMessages :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
    configureTimedMessages alice bob bobId ttl = do
      aliceName <- userName alice
      alice ##> ("/_set prefs @" <> bobId <> " {\"timedMessages\": {\"allow\": \"yes\", \"ttl\": " <> ttl <> "}}")
      alice <## "you updated preferences for bob:"
      alice <## ("Disappearing messages: off (you allow: yes (" <> ttl <> " sec), contact allows: no)")
      bob <## (aliceName <> " updated preferences for you:")
      bob <## ("Disappearing messages: off (you allow: no, contact allows: yes (" <> ttl <> " sec))")
      bob ##> ("/set disappear @" <> aliceName <> " yes")
      bob <## ("you updated preferences for " <> aliceName <> ":")
      bob <## ("Disappearing messages: enabled (you allow: yes (" <> ttl <> " sec), contact allows: yes (" <> ttl <> " sec))")
      alice <## "bob updated preferences for you:"
      alice <## ("Disappearing messages: enabled (you allow: yes (" <> ttl <> " sec), contact allows: yes (" <> ttl <> " sec))")
      alice #$> ("/clear bob", id, "bob: all messages are removed locally ONLY") -- to remove feature items

testSetChatItemTTL :: HasCallStack => FilePath -> IO ()
testSetChatItemTTL =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "@bob 1"
      bob <# "alice> 1"
      bob #> "@alice 2"
      alice <# "bob> 2"
      -- chat item with file
      alice #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      copyFile "./tests/fixtures/test.jpg" "./tests/tmp/app_files/test.jpg"
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      -- above items should be deleted after we set ttl
      threadDelay 3000000
      alice #> "@bob 3"
      bob <# "alice> 3"
      bob #> "@alice 4"
      alice <# "bob> 4"
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, "1"), Nothing), ((0, "2"), Nothing), ((1, ""), Just "test.jpg"), ((1, "3"), Nothing), ((0, "4"), Nothing)])
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $
        alice #$> ("/_ttl 1 2", id, "ok")
      alice #$> ("/_get chat @2 count=100", chat, [(1, "3"), (0, "4")]) -- when expiration is turned on, first cycle is synchronous
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "1"), (1, "2"), (0, ""), (0, "3"), (1, "4")])
      alice #$> ("/_ttl 1", id, "old messages are set to be deleted after: 2 second(s)")
      alice #$> ("/ttl week", id, "ok")
      alice #$> ("/ttl", id, "old messages are set to be deleted after: one week")
      alice #$> ("/ttl none", id, "ok")
      alice #$> ("/ttl", id, "old messages are not being deleted")

testSwitchContact :: HasCallStack => FilePath -> IO ()
testSwitchContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/switch bob", id, "ok")
      bob <## "alice started changing address for you"
      alice <## "bob: you started changing address"
      bob <## "alice changed address for you"
      alice <## "bob: you changed address"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "started changing address..."), (1, "you changed address")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "started changing address for you..."), (0, "changed address for you")])
      alice <##> bob

testSwitchGroupMember :: HasCallStack => FilePath -> IO ()
testSwitchGroupMember =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice #$> ("/switch #team bob", id, "ok")
      bob <## "#team: alice started changing address for you"
      alice <## "#team: you started changing address for bob"
      bob <## "#team: alice changed address for you"
      alice <## "#team: you changed address for bob"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "started changing address for bob..."), (1, "you changed address for bob")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "started changing address for you..."), (0, "changed address for you")])
      alice #> "#team hey"
      bob <# "#team alice> hey"
      bob #> "#team hi"
      alice <# "#team bob> hi"

testMarkContactVerified :: HasCallStack => FilePath -> IO ()
testMarkContactVerified =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice ##> "/i bob"
    bobInfo alice
    alice <## "connection not verified, use /code command to see security code"
    alice ##> "/code bob"
    bCode <- getTermLine alice
    bob ##> "/code alice"
    aCode <- getTermLine bob
    bCode `shouldBe` aCode
    alice ##> "/verify bob 123"
    alice <##. "connection not verified, current code is "
    alice ##> ("/verify bob " <> aCode)
    alice <## "connection verified"
    alice ##> "/i bob"
    bobInfo alice
    alice <## "connection verified"
    alice ##> "/verify bob"
    alice <##. "connection not verified, current code is "
    alice ##> "/i bob"
    bobInfo alice
    alice <## "connection not verified, use /code command to see security code"
  where
    bobInfo :: HasCallStack => TestCC -> IO ()
    bobInfo alice = do
      alice <## "contact ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"
      alice <## "you've shared main profile with this contact"

testMarkGroupMemberVerified :: HasCallStack => FilePath -> IO ()
testMarkGroupMemberVerified =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    createGroup2 "team" alice bob
    alice ##> "/i #team bob"
    bobInfo alice
    alice <## "connection not verified, use /code command to see security code"
    alice ##> "/code #team bob"
    bCode <- getTermLine alice
    bob ##> "/code #team alice"
    aCode <- getTermLine bob
    bCode `shouldBe` aCode
    alice ##> "/verify #team bob 123"
    alice <##. "connection not verified, current code is "
    alice ##> ("/verify #team bob " <> aCode)
    alice <## "connection verified"
    alice ##> "/i #team bob"
    bobInfo alice
    alice <## "connection verified"
    alice ##> "/verify #team bob"
    alice <##. "connection not verified, current code is "
    alice ##> "/i #team bob"
    bobInfo alice
    alice <## "connection not verified, use /code command to see security code"
  where
    bobInfo :: HasCallStack => TestCC -> IO ()
    bobInfo alice = do
      alice <## "group ID: 1"
      alice <## "member ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"
