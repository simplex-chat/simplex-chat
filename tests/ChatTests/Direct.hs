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
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Store (agentStoreFile, chatStoreFile)
import Simplex.Chat.Types (authErrDisableCount, sameVerificationCode, verificationCode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Version
import System.Directory (copyFile, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

chatDirectTests :: SpecWith FilePath
chatDirectTests = do
  describe "direct messages" $ do
    describe "add contact and send/receive messages" testAddContact
    it "clear chat with contact" testContactClear
    it "deleting contact deletes profile" testDeleteContactDeletesProfile
    it "unused contact is deleted silently" testDeleteUnusedContactSilent
    it "direct message quoted replies" testDirectMessageQuotedReply
    it "direct message update" testDirectMessageUpdate
    it "direct message edit history" testDirectMessageEditHistory
    it "direct message delete" testDirectMessageDelete
    it "direct live message" testDirectLiveMessage
    it "direct timed message" testDirectTimedMessage
    it "repeat AUTH errors disable contact" testRepeatAuthErrorsDisableContact
    it "should send multiline message" testMultilineMessage
  describe "duplicate contacts" $ do
    it "duplicate contacts are separate (contacts don't merge)" testDuplicateContactsSeparate
    it "new contact is separate with multiple duplicate contacts (contacts don't merge)" testDuplicateContactsMultipleSeparate
  describe "invitation link connection plan" $ do
    it "invitation link ok to connect" testPlanInvitationLinkOk
    it "own invitation link" testPlanInvitationLinkOwn
    it "connecting via invitation link" testPlanInvitationLinkConnecting
  describe "SMP servers" $ do
    it "get and set SMP servers" testGetSetSMPServers
    it "test SMP server connection" testTestSMPServerConnection
  describe "XFTP servers" $ do
    it "get and set XFTP servers" testGetSetXFTPServers
    it "test XFTP server connection" testTestXFTPServer
  describe "async connection handshake" $ do
    it "connect when initiating client goes offline" testAsyncInitiatingOffline
    it "connect when accepting client goes offline" testAsyncAcceptingOffline
    describe "connect, fully asynchronous (when clients are never simultaneously online)" $ do
      -- fails in CI
      xit'' "v2" testFullAsync
  describe "webrtc calls api" $ do
    it "negotiate call" testNegotiateCall
  describe "maintenance mode" $ do
    it "start/stop/export/import chat" testMaintenanceMode
    it "export/import chat with files" testMaintenanceModeWithFiles
    it "encrypt/decrypt database" testDatabaseEncryption
  describe "coordination between app and NSE" $ do
    it "should not subscribe in NSE and subscribe in the app" testSubscribeAppNSE
  describe "mute/unmute messages" $ do
    it "mute/unmute contact" testMuteContact
    it "mute/unmute group and member" testMuteGroup
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
    it "user profile privacy: hide profiles and notificaitons" testUserPrivacy
  describe "chat item expiration" $ do
    it "set chat item TTL" testSetChatItemTTL
  describe "connection switch" $ do
    it "switch contact to a different queue" testSwitchContact
    it "stop switching contact to a different queue" testAbortSwitchContact
    it "switch group member to a different queue" testSwitchGroupMember
    it "stop switching group member to a different queue" testAbortSwitchGroupMember
  describe "connection verification code" $ do
    it "verificationCode function converts ByteString to series of digits" $ \_ ->
      verificationCode (C.sha256Hash "abcd") `shouldBe` "61889 38426 63934 09576 96390 79389 84124 85253 63658 69469 70853 37788 95900 68296 20156 25"
    it "sameVerificationCode function should ignore spaces" $ \_ ->
      sameVerificationCode "123 456 789" "12345 6789" `shouldBe` True
    it "mark contact verified" testMarkContactVerified
    it "mark group member verified" testMarkGroupMemberVerified
  describe "message errors" $ do
    it "show message decryption error" testMsgDecryptError
    it "should report ratchet de-synchronization, synchronize ratchets" testSyncRatchet
    it "synchronize ratchets, reset connection code" testSyncRatchetCodeReset
  describe "message reactions" $ do
    it "set message reactions" testSetMessageReactions
  describe "delivery receipts" $ do
    it "should send delivery receipts" testSendDeliveryReceipts
    it "should send delivery receipts depending on configuration" testConfigureDeliveryReceipts
  describe "negotiate connection peer chat protocol version range" $ do
    describe "peer version range correctly set for new connection via invitation" $ do
      testInvVRange supportedChatVRange supportedChatVRange
      testInvVRange supportedChatVRange vr11
      testInvVRange vr11 supportedChatVRange
      testInvVRange vr11 vr11
    describe "peer version range correctly set for new connection via contact request" $ do
      testReqVRange supportedChatVRange supportedChatVRange
      testReqVRange supportedChatVRange vr11
      testReqVRange vr11 supportedChatVRange
      testReqVRange vr11 vr11
    it "update peer version range on received messages" testUpdatePeerChatVRange
  describe "network statuses" $ do
    it "should get network statuses" testGetNetworkStatuses
  where
    testInvVRange vr1 vr2 = it (vRangeStr vr1 <> " - " <> vRangeStr vr2) $ testConnInvChatVRange vr1 vr2
    testReqVRange vr1 vr2 = it (vRangeStr vr1 <> " - " <> vRangeStr vr2) $ testConnReqChatVRange vr1 vr2

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
    chatsEmpty alice bob = do
      alice @@@ [("@bob", lastChatFeature)]
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures)
      bob @@@ [("@alice", lastChatFeature)]
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
      alice #$> ("/read user", id, "ok")
      alice #$> ("/_read user 1", id, "ok")

testDuplicateContactsSeparate :: HasCallStack => FilePath -> IO ()
testDuplicateContactsSeparate =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob

      alice ##> "/c"
      inv' <- getInvitation alice
      bob ##> ("/c " <> inv')
      bob <## "confirmation sent!"
      concurrently_
        (alice <## "bob_1 (Bob): contact is connected")
        (bob <## "alice_1 (Alice): contact is connected")

      alice <##> bob
      alice #> "@bob_1 1"
      bob <# "alice_1> 1"
      bob #> "@alice_1 2"
      alice <# "bob_1> 2"

      alice @@@ [("@bob", "hey"), ("@bob_1", "2")]
      alice `hasContactProfiles` ["alice", "bob", "bob"]
      bob @@@ [("@alice", "hey"), ("@alice_1", "2")]
      bob `hasContactProfiles` ["bob", "alice", "alice"]

testDuplicateContactsMultipleSeparate :: HasCallStack => FilePath -> IO ()
testDuplicateContactsMultipleSeparate =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob

      alice ##> "/c"
      inv' <- getInvitation alice
      bob ##> ("/c " <> inv')
      bob <## "confirmation sent!"
      concurrently_
        (alice <## "bob_1 (Bob): contact is connected")
        (bob <## "alice_1 (Alice): contact is connected")

      alice ##> "/c"
      inv'' <- getInvitation alice
      bob ##> ("/c " <> inv'')
      bob <## "confirmation sent!"
      concurrently_
        (alice <## "bob_2 (Bob): contact is connected")
        (bob <## "alice_2 (Alice): contact is connected")

      alice <##> bob
      alice #> "@bob_1 1"
      bob <# "alice_1> 1"
      bob #> "@alice_1 2"
      alice <# "bob_1> 2"
      alice #> "@bob_2 3"
      bob <# "alice_2> 3"
      bob #> "@alice_2 4"
      alice <# "bob_2> 4"

      alice ##> "/contacts"
      alice <### ["bob (Bob)", "bob_1 (Bob)", "bob_2 (Bob)"]
      bob ##> "/contacts"
      bob <### ["alice (Alice)", "alice_1 (Alice)", "alice_2 (Alice)"]
      alice `hasContactProfiles` ["alice", "bob", "bob", "bob"]
      bob `hasContactProfiles` ["bob", "alice", "alice", "alice"]

testPlanInvitationLinkOk :: HasCallStack => FilePath -> IO ()
testPlanInvitationLinkOk =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/c"
      inv <- getInvitation alice
      bob ##> ("/_connect plan 1 " <> inv)
      bob <## "invitation link: ok to connect"

      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      concurrently_
        (alice <## "bob (Bob): contact is connected")
        (bob <## "alice (Alice): contact is connected")

      bob ##> ("/_connect plan 1 " <> inv)
      bob <## "invitation link: ok to connect" -- conn_req_inv is forgotten after connection
      alice <##> bob

testPlanInvitationLinkOwn :: HasCallStack => FilePath -> IO ()
testPlanInvitationLinkOwn tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    alice ##> "/c"
    inv <- getInvitation alice
    alice ##> ("/_connect plan 1 " <> inv)
    alice <## "invitation link: own link"

    let invSchema2 = linkAnotherSchema inv
    alice ##> ("/_connect plan 1 " <> invSchema2)
    alice <## "invitation link: own link"

    alice ##> ("/c " <> inv)
    alice <## "confirmation sent!"
    alice
      <### [ "alice_1 (Alice): contact is connected",
             "alice_2 (Alice): contact is connected"
           ]

    alice ##> ("/_connect plan 1 " <> inv)
    alice <## "invitation link: ok to connect" -- conn_req_inv is forgotten after connection
    alice @@@ [("@alice_1", lastChatFeature), ("@alice_2", lastChatFeature)]
    alice `send` "@alice_2 hi"
    alice
      <### [ WithTime "@alice_2 hi",
             WithTime "alice_1> hi"
           ]
    alice `send` "@alice_1 hey"
    alice
      <### [ WithTime "@alice_1 hey",
             WithTime "alice_2> hey"
           ]
    alice @@@ [("@alice_1", "hey"), ("@alice_2", "hey")]

testPlanInvitationLinkConnecting :: HasCallStack => FilePath -> IO ()
testPlanInvitationLinkConnecting tmp = do
  inv <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    alice ##> "/c"
    getInvitation alice
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"

    bob ##> ("/_connect plan 1 " <> inv)
    bob <## "invitation link: connecting"

    let invSchema2 = linkAnotherSchema inv
    bob ##> ("/_connect plan 1 " <> invSchema2)
    bob <## "invitation link: connecting"

testContactClear :: HasCallStack => FilePath -> IO ()
testContactClear =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob
      threadDelay 500000
      alice #$> ("/clear bob", id, "bob: all messages are removed locally ONLY")
      alice #$> ("/_get chat @2 count=100", chat, [])
      bob #$> ("/clear alice", id, "alice: all messages are removed locally ONLY")
      bob #$> ("/_get chat @2 count=100", chat, [])

testDeleteContactDeletesProfile :: HasCallStack => FilePath -> IO ()
testDeleteContactDeletesProfile =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob
      -- alice deletes contact, profile is deleted
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"
      alice ##> "/_contacts 1"
      (alice </)
      alice `hasContactProfiles` ["alice"]
      -- bob deletes contact, profile is deleted
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      bob ##> "/contacts"
      (bob </)
      bob `hasContactProfiles` ["bob"]

testDeleteUnusedContactSilent :: HasCallStack => FilePath -> IO ()
testDeleteUnusedContactSilent =
  testChatCfg3 testCfgCreateGroupDirect aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/contacts"
      bob <### ["alice (Alice)", "cath (Catherine)"]
      bob `hasContactProfiles` ["bob", "alice", "cath"]
      cath ##> "/contacts"
      cath <### ["alice (Alice)", "bob (Bob)"]
      cath `hasContactProfiles` ["cath", "alice", "bob"]
      -- bob deletes cath, cath's bob contact is deleted silently
      bob ##> "/d cath"
      bob <## "cath: contact is deleted"
      bob ##> "/contacts"
      bob <## "alice (Alice)"
      threadDelay 50000
      cath ##> "/contacts"
      cath <## "alice (Alice)"
      -- group messages work
      alice #> "#team hello"
      concurrentlyN_
        [ bob <# "#team alice> hello",
          cath <# "#team alice> hello"
        ]
      bob #> "#team hi there"
      concurrentlyN_
        [ alice <# "#team bob> hi there",
          cath <# "#team bob> hi there"
        ]
      cath #> "#team hey"
      concurrentlyN_
        [ alice <# "#team cath> hey",
          bob <# "#team cath> hey"
        ]

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

      alice ##> ("/_update item @2 " <> itemId 1 <> " text hello 🙂")
      alice <## "message didn't change"

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

testDirectMessageEditHistory :: HasCallStack => FilePath -> IO ()
testDirectMessageEditHistory =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "@bob hello!"
      bob <# "alice> hello!"

      alice ##> ("/_get item info @2 " <> itemId 1)
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hello!"
      bob ##> ("/_get item info @2 " <> itemId 1)
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hello!"

      alice ##> ("/_update item @2 " <> itemId 1 <> " text hey 👋")
      alice <# "@bob [edited] hey 👋"
      bob <# "alice> [edited] hey 👋"

      alice ##> ("/_get item info @2 " <> itemId 1)
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> ("/_get item info @2 " <> itemId 1)
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hey 👋"
      bob .<## ": hello!"

      alice ##> ("/_update item @2 " <> itemId 1 <> " text hello there")
      alice <# "@bob [edited] hello there"
      bob <# "alice> [edited] hello there"

      alice ##> "/item info @bob hello"
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hello there"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> "/item info @alice hello"
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hello there"
      bob .<## ": hey 👋"
      bob .<## ": hello!"

      bob #$> ("/_delete item @2 " <> itemId 1 <> " internal", id, "message deleted")

      alice ##> ("/_update item @2 " <> itemId 1 <> " text hey there")
      alice <# "@bob [edited] hey there"
      bob <# "alice> [edited] hey there"

      alice ##> "/item info @bob hey"
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hey there"
      alice .<## ": hello there"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> "/item info @alice hey"
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hey there"

testDirectMessageDelete :: HasCallStack => FilePath -> IO ()
testDirectMessageDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      -- Test for exception not interrupting the delivery - uncomment lines in newContentMessage
      -- alice #> "@bob hello 111"
      -- bob <## "exception: user error (#####################)"
      -- -- bob <## "bad chat command: #####################"
      -- -- bob <# "alice> hello 111"

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

      alice @@@ [("@bob", lastChatFeature)]
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
      alice @@@ [("@bob", lastChatFeature)]
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
    bob <# "alice> [LIVE started] use /show [on/off/6] hello"
    alice ##> ("/_update item @2 " <> itemId 1 <> " text hello there")
    alice <# "@bob [LIVE] hello there"
    bob <# "alice> [LIVE ended] hello there"
    -- empty live message is also sent instantly
    alice `send` "/live @bob"
    bob <# "alice> [LIVE started] use /show [on/off/7]"
    alice ##> ("/_update item @2 " <> itemId 2 <> " text hello 2")
    alice <# "@bob [LIVE] hello 2"
    bob <# "alice> [LIVE ended] hello 2"
    -- live message has edit history
    alice ##> ("/_get item info @2 " <> itemId 2)
    alice <##. "sent at: "
    alice <## "message history:"
    alice .<## ": hello 2"
    alice .<## ":"
    bob ##> ("/_get item info @2 " <> itemId 2)
    bob <##. "sent at: "
    bob <##. "received at: "
    bob <## "message history:"
    bob .<## ": hello 2"
    bob .<## ":"

testDirectTimedMessage :: HasCallStack => FilePath -> IO ()
testDirectTimedMessage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      alice ##> "/_send @2 ttl=1 text hello!"
      alice <# "@bob hello!"
      bob <# "alice> hello!"
      alice <## "timed message deleted: hello!"
      bob <## "timed message deleted: hello!"

      alice ##> "/_send @2 live=off ttl=1 text hey"
      alice <# "@bob hey"
      bob <# "alice> hey"
      alice <## "timed message deleted: hey"
      bob <## "timed message deleted: hey"

      alice ##> "/_send @2 ttl=default text hello"
      alice <# "@bob hello"
      bob <# "alice> hello"

      alice ##> "/_send @2 live=off text hi"
      alice <# "@bob hi"
      bob <# "alice> hi"

testRepeatAuthErrorsDisableContact :: HasCallStack => FilePath -> IO ()
testRepeatAuthErrorsDisableContact =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice <##> bob
    threadDelay 500000
    bob ##> "/_delete @2 notify=off"
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

testMultilineMessage :: HasCallStack => FilePath -> IO ()
testMultilineMessage = testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
  connectUsers alice bob
  connectUsers alice cath
  alice `send` "@bob \"hello\\nthere\"" -- @bob "hello\nthere"
  alice <# "@bob hello"
  alice <## "there"
  bob <# "alice> hello"
  bob <## "there"
  alice `send` "/feed \"hello\\nthere\"" -- /feed "hello\nthere"
  alice <##. "/feed (2)"
  alice <## "there"
  bob <# "alice> hello"
  bob <## "there"
  cath <# "alice> hello"
  cath <## "there"

testGetSetSMPServers :: HasCallStack => FilePath -> IO ()
testGetSetSMPServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/_servers 1 smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")
      alice #$> ("/smp smp://1234-w==@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==@smp1.example.im")
      alice #$> ("/smp smp://1234-w==:password@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==:password@smp1.example.im")
      alice #$> ("/smp smp://2345-w==@smp2.example.im smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice ##> "/smp"
      alice <## "smp://2345-w==@smp2.example.im"
      alice <## "smp://3456-w==@smp3.example.im:5224"
      alice #$> ("/smp default", id, "ok")
      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")

testTestSMPServerConnection :: HasCallStack => FilePath -> IO ()
testTestSMPServerConnection =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice ##> "/smp test smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:7001"
      alice <## "SMP server test passed"
      -- to test with password:
      -- alice <## "SMP server test failed at CreateQueue, error: SMP AUTH"
      -- alice <## "Server requires authorization to create queues, check password"
      alice ##> "/smp test smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"
      alice <## "SMP server test passed"
      alice ##> "/smp test smp://LcJU@localhost:7001"
      alice <## "SMP server test failed at Connect, error: BROKER smp://LcJU@localhost:7001 NETWORK"
      alice <## "Possibly, certificate fingerprint in SMP server address is incorrect"

testGetSetXFTPServers :: HasCallStack => FilePath -> IO ()
testGetSetXFTPServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> withXFTPServer $ do
      alice #$> ("/_servers 1 xftp", id, "xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002")
      alice #$> ("/xftp xftp://1234-w==@xftp1.example.im", id, "ok")
      alice #$> ("/xftp", id, "xftp://1234-w==@xftp1.example.im")
      alice #$> ("/xftp xftp://1234-w==:password@xftp1.example.im", id, "ok")
      alice #$> ("/xftp", id, "xftp://1234-w==:password@xftp1.example.im")
      alice #$> ("/xftp xftp://2345-w==@xftp2.example.im xftp://3456-w==@xftp3.example.im:5224", id, "ok")
      alice ##> "/xftp"
      alice <## "xftp://2345-w==@xftp2.example.im"
      alice <## "xftp://3456-w==@xftp3.example.im:5224"
      alice #$> ("/xftp default", id, "ok")
      alice #$> ("/xftp", id, "xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002")

testTestXFTPServer :: HasCallStack => FilePath -> IO ()
testTestXFTPServer =
  testChat2 aliceProfile bobProfile $
    \alice _ -> withXFTPServer $ do
      alice ##> "/xftp test xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:7002"
      alice <## "XFTP server test passed"
      -- to test with password:
      -- alice <## "XFTP server test failed at CreateFile, error: XFTP AUTH"
      -- alice <## "Server requires authorization to upload files, check password"
      alice ##> "/xftp test xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002"
      alice <## "XFTP server test passed"
      alice ##> "/xftp test xftp://LcJU@localhost:7002"
      alice <## "XFTP server test failed at Connect, error: BROKER xftp://LcJU@localhost:7002 NETWORK"
      alice <## "Possibly, certificate fingerprint in XFTP server address is incorrect"

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
    withTestChatOpts tmp (getTestOpts True "mykey") "alice" $ \alice -> do
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
    withTestChatOpts tmp (getTestOpts True "anotherkey") "alice" $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      testChatWorking alice bob
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/db decrypt anotherkey"
      alice <## "ok"
    withTestChat tmp "alice" $ \alice -> do
      testChatWorking alice bob

testSubscribeAppNSE :: HasCallStack => FilePath -> IO ()
testSubscribeAppNSE tmp =
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChat tmp "alice" aliceProfile $ \alice -> do
      withTestChatOpts tmp testOpts {maintenance = True} "alice" $ \nseAlice -> do
        alice ##> "/_app suspend 1"
        alice <## "ok"
        alice <## "chat suspended"
        nseAlice ##> "/_start subscribe=off expire=off xftp=off"
        nseAlice <## "chat started"
        nseAlice ##> "/ad"
        cLink <- getContactLink nseAlice True
        bob ##> ("/c " <> cLink)
        bob <## "connection request sent!"
        (nseAlice </)
        alice ##> "/_app activate"
        alice <## "ok"
        alice <## "Your address is active! To show: /sa"
        alice <## "bob (Bob) wants to connect to you!"
        alice <## "to accept: /ac bob"
        alice <## "to reject: /rc bob (the sender will NOT be notified)"
        alice ##> "/ac bob"
        alice <## "bob (Bob): accepting contact request..."
        concurrently_
          (bob <## "alice (Alice): contact is connected")
          (alice <## "bob (Bob): contact is connected")
        threadDelay 100000
        alice <##> bob

testMuteContact :: HasCallStack => FilePath -> IO ()
testMuteContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob ##> "/mute @alice"
      bob <## "ok"
      alice #> "@bob hi"
      (bob </)
      bob ##> "/contacts"
      bob <## "alice (Alice) (muted, you can /unmute @alice)"
      bob ##> "/unmute @alice"
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
      bob #> "#team hello"
      concurrently_
        (alice <# "#team bob> hello")
        (cath <# "#team bob> hello")
      cath `send` "> #team (hello) hello too!"
      cath <# "#team > bob hello"
      cath <## "      hello too!"
      concurrentlyN_
        [ (bob </),
          do
            alice <# "#team cath> > bob hello"
            alice <## "      hello too!"
        ]
      bob ##> "/unmute mentions #team"
      bob <## "ok"
      alice `send` "> #team @bob (hello) hey bob!"
      alice <# "#team > bob hello"
      alice <## "      hey bob!"
      concurrentlyN_
        [ do
            bob <# "#team alice> > bob hello"
            bob <## "      hey bob!",
          do
            cath <# "#team alice> > bob hello"
            cath <## "      hey bob!"
        ]
      alice `send` "> #team @cath (hello) hey cath!"
      alice <# "#team > cath hello too!"
      alice <## "      hey cath!"
      concurrentlyN_
        [ (bob </),
          do
            cath <# "#team alice> > cath hello too!"
            cath <## "      hey cath!"
        ]
      bob ##> "/gs"
      bob <## "#team (3 members, mentions only, you can /unmute #team)"
      bob ##> "/unmute #team"
      bob <## "ok"
      alice #> "#team hi again"
      concurrently_
        (bob <# "#team alice> hi again")
        (cath <# "#team alice> hi again")
      bob ##> "/block #team alice"
      bob <## "ok"
      bob ##> "/ms team"
      bob <## "bob (Bob): admin, you, connected"
      bob <## "alice (Alice): owner, host, connected, blocked"
      bob <## "cath (Catherine): admin, connected"
      alice #> "#team test 1"
      concurrently_
        (bob </)
        (cath <# "#team alice> test 1")
      cath #> "#team test 2"
      concurrently_
        (bob <# "#team cath> test 2")
        (alice <# "#team cath> test 2")
      bob ##> "/tail #team 3"
      bob <# "#team alice> hi again"
      bob <# "#team alice> test 1 [blocked]"
      bob <# "#team cath> test 2"
      threadDelay 1000000
      bob ##> "/unblock #team alice"
      bob <## "ok"
      bob ##> "/ms team"
      bob <## "bob (Bob): admin, you, connected"
      bob <## "alice (Alice): owner, host, connected"
      bob <## "cath (Catherine): admin, connected"
      alice #> "#team test 3"
      concurrently_
        (bob <# "#team alice> test 3")
        (cath <# "#team alice> test 3")
      cath #> "#team test 4"
      concurrently_
        (bob <# "#team cath> test 4")
        (alice <# "#team cath> test 4")
      bob ##> "/gs"
      bob <## "#team (3 members)"

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
      alice @@@ [("@bob", lastChatFeature)]
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
      alice #$> ("/_get chats 2 pcc=on", chats, [("@bob", lastChatFeature)])
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
      alice #$> ("/_get chats 2 pcc=on", chats, [("@cath", lastChatFeature), ("@bob", "hey")])
      alice <##> cath

      -- first user doesn't have cath as contact
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice @@@ [("@bob", "hey alice")]

testCreateUserDefaultServers :: HasCallStack => FilePath -> IO ()
testCreateUserDefaultServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp smp://2345-w==@smp2.example.im smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/xftp xftp://2345-w==@xftp2.example.im xftp://3456-w==@xftp3.example.im:5224", id, "ok")
      checkCustomServers alice

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")
      alice #$> ("/xftp", id, "xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002")

      -- with same_servers=off
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      checkCustomServers alice

      alice ##> "/create user same_servers=off alisa2"
      showActiveUser alice "alisa2"

      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001")
      alice #$> ("/xftp", id, "xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002")
  where
    checkCustomServers alice = do
      alice ##> "/smp"
      alice <## "smp://2345-w==@smp2.example.im"
      alice <## "smp://3456-w==@smp3.example.im:5224"
      alice ##> "/xftp"
      alice <## "xftp://2345-w==@xftp2.example.im"
      alice <## "xftp://3456-w==@xftp3.example.im:5224"

testCreateUserSameServers :: HasCallStack => FilePath -> IO ()
testCreateUserSameServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp smp://2345-w==@smp2.example.im smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/xftp xftp://2345-w==@xftp2.example.im xftp://3456-w==@xftp3.example.im:5224", id, "ok")
      checkCustomServers alice

      alice ##> "/create user same_servers=on alisa"
      showActiveUser alice "alisa"

      checkCustomServers alice
  where
    checkCustomServers alice = do
      alice ##> "/smp"
      alice <## "smp://2345-w==@smp2.example.im"
      alice <## "smp://3456-w==@smp3.example.im:5224"
      alice ##> "/xftp"
      alice <## "xftp://2345-w==@xftp2.example.im"
      alice <## "xftp://3456-w==@xftp3.example.im:5224"

testDeleteUser :: HasCallStack => FilePath -> IO ()
testDeleteUser =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      connectUsers alice bob

      alice ##> "/create user alisa"
      showActiveUser alice "alisa"

      -- cannot delete active user when there is another user

      alice ##> "/_delete user 2 del_smp=off"
      alice <## "cannot delete active user"

      -- delete user without deleting SMP queues

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

      -- cannot delete active user when there is another user

      alice ##> "/create user alisa2"
      showActiveUser alice "alisa2"

      connectUsers alice dan
      alice <##> dan

      alice ##> "/delete user alisa2"
      alice <## "cannot delete active user"

      alice ##> "/users"
      alice <## "alisa"
      alice <## "alisa2 (active)"

      alice <##> dan

      -- delete user deleting SMP queues

      alice ##> "/delete user alisa"
      alice <### ["ok", "completed deleting user"]

      alice ##> "/users"
      alice <## "alisa2 (active)"

      cath #> "@alisa hey"
      cath <## "[alisa, contactId: 2, connId: 1] error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"
      (alice </)

      alice <##> dan

      -- delete last active user

      alice ##> "/delete user alisa2 del_smp=off"
      alice <### ["ok", "completed deleting user"]
      alice ##> "/users"
      alice <## "no users"

      alice ##> "/create user alisa3"
      showActiveUser alice "alisa3"

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
    cfg = testCfg {initialCleanupManagerDelay = 0, cleanupManagerStepDelay = 0, ciExpirationInterval = 500000}

testUsersRestartCIExpiration :: HasCallStack => FilePath -> IO ()
testUsersRestartCIExpiration tmp = do
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
      -- set ttl for first user
      alice #$> ("/_ttl 1 2", id, "ok")
      connectUsers alice bob

      -- create second user and set ttl
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_ttl 2 5", id, "ok")
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
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 2 second(s)")

      alice #> "@bob alice 3"
      bob <# "alice> alice 3"
      bob #> "@alice alice 4"
      alice <# "bob> alice 4"

      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "alice 1"), (0, "alice 2"), (1, "alice 3"), (0, "alice 4")])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 5 second(s)")

      alice #> "@bob alisa 3"
      bob <# "alisa> alisa 3"
      bob #> "@alisa alisa 4"
      alice <# "bob> alisa 4"

      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 3000000

      -- messages both before and after restart are deleted
      -- first user messages
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      -- second user messages
      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, chatFeatures <> [(1, "alisa 1"), (0, "alisa 2"), (1, "alisa 3"), (0, "alisa 4")])

      threadDelay 3000000

      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    cfg = testCfg {initialCleanupManagerDelay = 0, cleanupManagerStepDelay = 0, ciExpirationInterval = 500000}

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
    cfg = testCfg {initialCleanupManagerDelay = 0, cleanupManagerStepDelay = 0, ciExpirationInterval = 500000}

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
    cfg = testCfg {initialCleanupManagerDelay = 0, cleanupManagerStepDelay = 0, ciExpirationInterval = 500000}

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

      alice <## "[user: alice] timed message deleted: alice 1"
      alice <## "[user: alice] timed message deleted: alice 2"
      bob <## "timed message deleted: alice 1"
      bob <## "timed message deleted: alice 2"

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 1"), (0, "alisa 2")])

      threadDelay 1000000

      alice <## "timed message deleted: alisa 1"
      alice <## "timed message deleted: alisa 2"
      bob <## "timed message deleted: alisa 1"
      bob <## "timed message deleted: alisa 2"

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

      alice <## "[user: alice] timed message deleted: alice 3"
      alice <## "[user: alice] timed message deleted: alice 4"
      bob <## "timed message deleted: alice 3"
      bob <## "timed message deleted: alice 4"

      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      alice #$> ("/_get chat @2 count=100", chat, [])

      alice ##> "/user alisa"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [(1, "alisa 3"), (0, "alisa 4")])

      threadDelay 1000000

      alice <## "timed message deleted: alisa 3"
      alice <## "timed message deleted: alisa 4"
      bob <## "timed message deleted: alisa 3"
      bob <## "timed message deleted: alisa 4"

      alice ##> "/user"
      showActiveUser alice "alisa"
      alice #$> ("/_get chat @4 count=100", chat, [])
  where
    configureTimedMessages :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
    configureTimedMessages alice bob bobId ttl = do
      aliceName <- userName alice
      alice ##> ("/_set prefs @" <> bobId <> " {\"timedMessages\": {\"allow\": \"yes\", \"ttl\": " <> ttl <> "}}")
      alice <## "you updated preferences for bob:"
      alice <## ("Disappearing messages: enabled (you allow: yes (" <> ttl <> " sec), contact allows: yes)")
      bob <## (aliceName <> " updated preferences for you:")
      bob <## ("Disappearing messages: enabled (you allow: yes (" <> ttl <> " sec), contact allows: yes (" <> ttl <> " sec))")
      alice #$> ("/clear bob", id, "bob: all messages are removed locally ONLY") -- to remove feature items

testUserPrivacy :: HasCallStack => FilePath -> IO ()
testUserPrivacy =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- connect using second user
      connectUsers alice bob
      threadDelay 1000000
      alice #> "@bob hello"
      threadDelay 1000000
      bob <# "alisa> hello"
      bob #> "@alisa hey"
      alice <# "bob> hey"
      -- hide user profile
      alice ##> "/hide user my_password"
      userHidden alice "current "
      -- shows messages when active
      bob #> "@alisa hello again"
      alice <# "bob> hello again"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      -- does not show messages to user
      bob #> "@alisa this won't show"
      (alice </)
      -- does not show hidden user
      alice ##> "/users"
      alice <## "alice (Alice) (active)"
      (alice </)
      -- requires password to switch to the user
      alice ##> "/user alisa"
      alice <## "user does not exist or incorrect password"
      alice ##> "/user alisa wrong_password"
      alice <## "user does not exist or incorrect password"
      alice ##> "/user alisa my_password"
      showActiveUser alice "alisa"
      -- shows hidden user when active
      alice ##> "/users"
      alice <## "alice (Alice)"
      alice <## "alisa (active, hidden, muted, unread: 1)"
      -- hidden message is saved
      alice ##> "/tail"
      alice <##? chatHistory
      alice ##> "/_get items count=10"
      alice <##? chatHistory
      alice ##> "/_get items before=11 count=10"
      alice
        <##? [ "bob> Disappearing messages: allowed",
               "bob> Full deletion: off",
               "bob> Message reactions: enabled",
               "bob> Voice messages: enabled",
               "bob> Audio/video calls: enabled"
             ]
      alice ##> "/_get items after=10 count=10"
      alice
        <##? [ "@bob hello",
               "bob> hey",
               "bob> hello again",
               "bob> this won't show"
             ]
      -- change profile password
      alice ##> "/unmute user"
      alice <## "hidden user always muted when inactive"
      alice ##> "/hide user password"
      alice <## "user is already hidden"
      alice ##> "/unhide user wrong_password"
      alice <## "user does not exist or incorrect password"
      alice ##> "/unhide user my_password"
      userVisible alice "current "
      alice ##> "/hide user new_password"
      userHidden alice "current "
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"
      -- delete last visible active user
      alice ##> "/_delete user 1 del_smp=on"
      alice <### ["ok", "completed deleting user"]
      -- hidden user is not shown
      alice ##> "/users"
      alice <## "no users"
      -- but it is still possible to switch to it
      alice ##> "/user alisa wrong_password"
      alice <## "user does not exist or incorrect password"
      alice ##> "/user alisa new_password"
      showActiveUser alice "alisa"
      alice ##> "/create user alisa2"
      showActiveUser alice "alisa2"
      alice ##> "/_hide user 3 \"password2\""
      alice <## "cannot hide the only not hidden user"
      -- change profile privacy for inactive user via API requires correct password
      alice ##> "/_unmute user 2"
      alice <## "hidden user always muted when inactive"
      alice ##> "/_hide user 2 \"password\""
      alice <## "user is already hidden"
      alice ##> "/_unhide user 2 \"wrong_password\""
      alice <## "user does not exist or incorrect password"
      alice ##> "/_unhide user 2 \"new_password\""
      userVisible alice ""
      alice ##> "/_hide user 2 \"another_password\""
      userHidden alice ""
      alice ##> "/_delete user 2 del_smp=on"
      alice <## "user does not exist or incorrect password"
      alice ##> "/_delete user 2 del_smp=on \"wrong_password\""
      alice <## "user does not exist or incorrect password"
      alice ##> "/_delete user 2 del_smp=on \"another_password\""
      alice <### ["ok", "completed deleting user"]
      alice ##> "/_delete user 3 del_smp=on"
      alice <### ["ok", "completed deleting user"]
  where
    userHidden alice current = do
      alice <## (current <> "user alisa:")
      alice <## "messages are hidden (use /tail to view)"
      alice <## "profile is hidden"
    userVisible alice current = do
      alice <## (current <> "user alisa:")
      alice <## "messages are shown"
      alice <## "profile is visible"
    chatHistory =
      [ "bob> Disappearing messages: allowed",
        "bob> Full deletion: off",
        "bob> Message reactions: enabled",
        "bob> Voice messages: enabled",
        "bob> Audio/video calls: enabled",
        "@bob hello",
        "bob> hey",
        "bob> hello again",
        "bob> this won't show"
      ]

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
      alice #$> ("/switch bob", id, "switch started")
      bob <## "alice started changing address for you"
      alice <## "bob: you started changing address"
      bob <## "alice changed address for you"
      alice <## "bob: you changed address"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "started changing address..."), (1, "you changed address")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "started changing address for you..."), (0, "changed address for you")])
      alice <##> bob

testAbortSwitchContact :: HasCallStack => FilePath -> IO ()
testAbortSwitchContact tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      connectUsers alice bob
    alice #$> ("/switch bob", id, "switch started")
    alice <## "bob: you started changing address"
    -- repeat switch is prohibited
    alice ##> "/switch bob"
    alice <## "error: command is prohibited"
    -- stop switch
    alice #$> ("/abort switch bob", id, "switch aborted")
    -- repeat switch stop is prohibited
    alice ##> "/abort switch bob"
    alice <## "error: command is prohibited"
    withTestChatContactConnected tmp "bob" $ \bob -> do
      bob <## "alice started changing address for you"
      -- alice changes address again
      alice #$> ("/switch bob", id, "switch started")
      alice <## "bob: you started changing address"
      bob <## "alice started changing address for you"
      bob <## "alice changed address for you"
      alice <## "bob: you changed address"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "started changing address..."), (1, "started changing address..."), (1, "you changed address")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "started changing address for you..."), (0, "started changing address for you..."), (0, "changed address for you")])
      alice <##> bob

testSwitchGroupMember :: HasCallStack => FilePath -> IO ()
testSwitchGroupMember =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice #$> ("/switch #team bob", id, "switch started")
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

testAbortSwitchGroupMember :: HasCallStack => FilePath -> IO ()
testAbortSwitchGroupMember tmp = do
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
    alice #$> ("/switch #team bob", id, "switch started")
    alice <## "#team: you started changing address for bob"
    -- repeat switch is prohibited
    alice ##> "/switch #team bob"
    alice <## "error: command is prohibited"
    -- stop switch
    alice #$> ("/abort switch #team bob", id, "switch aborted")
    -- repeat switch stop is prohibited
    alice ##> "/abort switch #team bob"
    alice <## "error: command is prohibited"
    withTestChatContactConnected tmp "bob" $ \bob -> do
      bob <## "#team: connected to server(s)"
      bob <## "#team: alice started changing address for you"
      -- alice changes address again
      alice #$> ("/switch #team bob", id, "switch started")
      alice <## "#team: you started changing address for bob"
      bob <## "#team: alice started changing address for you"
      bob <## "#team: alice changed address for you"
      alice <## "#team: you changed address for bob"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "started changing address for bob..."), (1, "started changing address for bob..."), (1, "you changed address for bob")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "started changing address for you..."), (0, "started changing address for you..."), (0, "changed address for you")])
      alice #> "#team hey"
      bob <# "#team alice> hey"
      bob #> "#team hi"
      alice <# "#team bob> hi"

testMarkContactVerified :: HasCallStack => FilePath -> IO ()
testMarkContactVerified =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice ##> "/i bob"
    bobInfo alice False
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
    bobInfo alice True
    alice ##> "/verify bob"
    alice <##. "connection not verified, current code is "
    alice ##> "/i bob"
    bobInfo alice False
  where
    bobInfo :: HasCallStack => TestCC -> Bool -> IO ()
    bobInfo alice verified = do
      alice <## "contact ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"
      alice <## "you've shared main profile with this contact"
      alice <## connVerified
      alice <## currentChatVRangeInfo
      where
        connVerified
          | verified = "connection verified"
          | otherwise = "connection not verified, use /code command to see security code"

testMarkGroupMemberVerified :: HasCallStack => FilePath -> IO ()
testMarkGroupMemberVerified =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    createGroup2 "team" alice bob
    alice ##> "/i #team bob"
    bobInfo alice False
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
    bobInfo alice True
    alice ##> "/verify #team bob"
    alice <##. "connection not verified, current code is "
    alice ##> "/i #team bob"
    bobInfo alice False
  where
    bobInfo :: HasCallStack => TestCC -> Bool -> IO ()
    bobInfo alice verified = do
      alice <## "group ID: 1"
      alice <## "member ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"
      alice <## connVerified
      alice <## currentChatVRangeInfo
      where
        connVerified
          | verified = "connection verified"
          | otherwise = "connection not verified, use /code command to see security code"

testMsgDecryptError :: HasCallStack => FilePath -> IO ()
testMsgDecryptError tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"
    setupDesynchronizedRatchet tmp alice
    withTestChat tmp "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      alice #> "@bob hello again"
      bob <# "alice> skipped message ID 10..12"
      bob <# "alice> hello again"
      bob #> "@alice received!"
      alice <# "bob> received!"

setupDesynchronizedRatchet :: HasCallStack => FilePath -> TestCC -> IO ()
setupDesynchronizedRatchet tmp alice = do
  copyDb "bob" "bob_old"
  withTestChat tmp "bob" $ \bob -> do
    bob <## "1 contacts connected (use /cs for the list)"
    alice #> "@bob 1"
    bob <# "alice> 1"
    bob #> "@alice 2"
    alice <# "bob> 2"
    alice #> "@bob 3"
    bob <# "alice> 3"
    bob #> "@alice 4"
    alice <# "bob> 4"
    threadDelay 500000
  withTestChat tmp "bob_old" $ \bob -> do
    bob <## "1 contacts connected (use /cs for the list)"
    bob ##> "/sync alice"
    bob <## "error: command is prohibited"
    alice #> "@bob 1"
    bob <## "alice: decryption error (connection out of sync), synchronization required"
    bob <## "use /sync alice to synchronize"
    alice #> "@bob 2"
    alice #> "@bob 3"
    (bob </)
    bob ##> "/tail @alice 1"
    bob <# "alice> decryption error, possibly due to the device change (header, 3 messages)"
    bob ##> "@alice 1"
    bob <## "error: command is prohibited"
    (alice </)
  where
    copyDb from to = do
      copyFile (chatStoreFile $ tmp </> from) (chatStoreFile $ tmp </> to)
      copyFile (agentStoreFile $ tmp </> from) (agentStoreFile $ tmp </> to)

testSyncRatchet :: HasCallStack => FilePath -> IO ()
testSyncRatchet tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"
    setupDesynchronizedRatchet tmp alice
    withTestChat tmp "bob_old" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      bob ##> "/sync alice"
      bob <## "connection synchronization started"
      alice <## "bob: connection synchronization agreed"
      bob <## "alice: connection synchronization agreed"
      alice <## "bob: connection synchronized"
      bob <## "alice: connection synchronized"

      bob #$> ("/_get chat @2 count=3", chat, [(1, "connection synchronization started"), (0, "connection synchronization agreed"), (0, "connection synchronized")])
      alice #$> ("/_get chat @2 count=2", chat, [(0, "connection synchronization agreed"), (0, "connection synchronized")])

      alice #> "@bob hello again"
      bob <# "alice> hello again"
      bob #> "@alice received!"
      alice <# "bob> received!"

testSyncRatchetCodeReset :: HasCallStack => FilePath -> IO ()
testSyncRatchetCodeReset tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      bob #> "@alice hey"
      alice <# "bob> hey"
      -- connection not verified
      bob ##> "/i alice"
      aliceInfo bob False
      -- verify connection
      alice ##> "/code bob"
      bCode <- getTermLine alice
      bob ##> ("/verify alice " <> bCode)
      bob <## "connection verified"
      -- connection verified
      bob ##> "/i alice"
      aliceInfo bob True
    setupDesynchronizedRatchet tmp alice
    withTestChat tmp "bob_old" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"
      bob ##> "/sync alice"
      bob <## "connection synchronization started"
      alice <## "bob: connection synchronization agreed"
      bob <## "alice: connection synchronization agreed"
      bob <## "alice: security code changed"
      alice <## "bob: connection synchronized"
      bob <## "alice: connection synchronized"

      bob #$> ("/_get chat @2 count=4", chat, [(1, "connection synchronization started"), (0, "connection synchronization agreed"), (0, "security code changed"), (0, "connection synchronized")])
      alice #$> ("/_get chat @2 count=2", chat, [(0, "connection synchronization agreed"), (0, "connection synchronized")])

      -- connection not verified
      bob ##> "/i alice"
      aliceInfo bob False

      alice #> "@bob hello again"
      bob <# "alice> hello again"
      bob #> "@alice received!"
      alice <# "bob> received!"
  where
    aliceInfo :: HasCallStack => TestCC -> Bool -> IO ()
    aliceInfo bob verified = do
      bob <## "contact ID: 2"
      bob <## "receiving messages via: localhost"
      bob <## "sending messages via: localhost"
      bob <## "you've shared main profile with this contact"
      bob <## connVerified
      bob <## currentChatVRangeInfo
      where
        connVerified
          | verified = "connection verified"
          | otherwise = "connection not verified, use /code command to see security code"

testSetMessageReactions :: HasCallStack => FilePath -> IO ()
testSetMessageReactions =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "@bob hi"
      bob <# "alice> hi"
      bob ##> "+1 alice hi"
      bob <## "added 👍"
      alice <# "bob> >> hi"
      alice <## "    + 👍"
      bob ##> "+1 alice hi"
      bob <## "bad chat command: reaction already added"
      bob ##> "+^ alice hi"
      bob <## "added 🚀"
      alice <# "bob> >> hi"
      alice <## "    + 🚀"
      alice ##> "/tail @bob 1"
      alice <# "@bob hi"
      alice <## "      👍 1 🚀 1"
      bob ##> "/tail @alice 1"
      bob <# "alice> hi"
      bob <## "      👍 1 🚀 1"
      alice ##> "+1 bob hi"
      alice <## "added 👍"
      bob <# "alice> > hi"
      bob <## "    + 👍"
      alice ##> "/tail @bob 1"
      alice <# "@bob hi"
      alice <## "      👍 2 🚀 1"
      bob ##> "/tail @alice 1"
      bob <# "alice> hi"
      bob <## "      👍 2 🚀 1"
      bob ##> "-1 alice hi"
      bob <## "removed 👍"
      alice <# "bob> >> hi"
      alice <## "    - 👍"
      bob ##> "-^ alice hi"
      bob <## "removed 🚀"
      alice <# "bob> >> hi"
      alice <## "    - 🚀"
      alice ##> "/tail @bob 1"
      alice <# "@bob hi"
      alice <## "      👍 1"
      bob ##> "/tail @alice 1"
      bob <# "alice> hi"
      bob <## "      👍 1"

testSendDeliveryReceipts :: HasCallStack => FilePath -> IO ()
testSendDeliveryReceipts tmp =
  withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp cfg "bob" bobProfile $ \bob -> do
      connectUsers alice bob

      alice #> "@bob hi"
      bob <# "alice> hi"
      alice ⩗ "@bob hi"

      bob #> "@alice hey"
      alice <# "bob> hey"
      bob ⩗ "@alice hey"
  where
    cfg = testCfg {showReceipts = True}

testConfigureDeliveryReceipts :: HasCallStack => FilePath -> IO ()
testConfigureDeliveryReceipts tmp =
  withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp cfg "bob" bobProfile $ \bob -> do
      withNewTestChatCfg tmp cfg "cath" cathProfile $ \cath -> do
        connectUsers alice bob
        connectUsers alice cath

        -- for new users receipts are enabled by default
        receipt bob alice "1"
        receipt cath alice "2"

        -- configure receipts in all chats
        alice ##> "/set receipts all off"
        alice <## "ok"
        noReceipt bob alice "3"
        noReceipt cath alice "4"

        -- configure receipts for user contacts
        alice ##> "/_set receipts contacts 1 on"
        alice <## "ok"
        receipt bob alice "5"
        receipt cath alice "6"

        -- configure receipts for user contacts (terminal api)
        alice ##> "/set receipts contacts off"
        alice <## "ok"
        noReceipt bob alice "7"
        noReceipt cath alice "8"

        -- configure receipts for contact
        alice ##> "/receipts @bob on"
        alice <## "ok"
        receipt bob alice "9"
        noReceipt cath alice "10"

        -- configure receipts for user contacts (don't clear overrides)
        alice ##> "/_set receipts contacts 1 off"
        alice <## "ok"
        receipt bob alice "11"
        noReceipt cath alice "12"

        alice ##> "/_set receipts contacts 1 off clear_overrides=off"
        alice <## "ok"
        receipt bob alice "13"
        noReceipt cath alice "14"

        -- configure receipts for user contacts (clear overrides)
        alice ##> "/set receipts contacts off clear_overrides=on"
        alice <## "ok"
        noReceipt bob alice "15"
        noReceipt cath alice "16"

        -- configure receipts for contact, reset to default
        alice ##> "/receipts @bob on"
        alice <## "ok"
        receipt bob alice "17"
        noReceipt cath alice "18"

        alice ##> "/receipts @bob default"
        alice <## "ok"
        noReceipt bob alice "19"
        noReceipt cath alice "20"
  where
    cfg = testCfg {showReceipts = True}
    receipt cc1 cc2 msg = do
      name1 <- userName cc1
      name2 <- userName cc2
      cc1 #> ("@" <> name2 <> " " <> msg)
      cc2 <# (name1 <> "> " <> msg)
      cc1 ⩗ ("@" <> name2 <> " " <> msg)
    noReceipt cc1 cc2 msg = do
      name1 <- userName cc1
      name2 <- userName cc2
      cc1 #> ("@" <> name2 <> " " <> msg)
      cc2 <# (name1 <> "> " <> msg)
      cc1 <// 50000

testConnInvChatVRange :: HasCallStack => VersionRange -> VersionRange -> FilePath -> IO ()
testConnInvChatVRange ct1VRange ct2VRange tmp =
  withNewTestChatCfg tmp testCfg {chatVRange = ct1VRange} "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp testCfg {chatVRange = ct2VRange} "bob" bobProfile $ \bob -> do
      connectUsers alice bob

      alice ##> "/i bob"
      contactInfoChatVRange alice ct2VRange

      bob ##> "/i alice"
      contactInfoChatVRange bob ct1VRange

testConnReqChatVRange :: HasCallStack => VersionRange -> VersionRange -> FilePath -> IO ()
testConnReqChatVRange ct1VRange ct2VRange tmp =
  withNewTestChatCfg tmp testCfg {chatVRange = ct1VRange} "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp testCfg {chatVRange = ct2VRange} "bob" bobProfile $ \bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True
      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

      alice ##> "/i bob"
      contactInfoChatVRange alice ct2VRange

      bob ##> "/i alice"
      contactInfoChatVRange bob ct1VRange

testUpdatePeerChatVRange :: HasCallStack => FilePath -> IO ()
testUpdatePeerChatVRange tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp cfg11 "bob" bobProfile $ \bob -> do
      connectUsers alice bob

      alice ##> "/i bob"
      contactInfoChatVRange alice vr11

      bob ##> "/i alice"
      contactInfoChatVRange bob supportedChatVRange

    withTestChat tmp "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"

      bob #> "@alice hello 1"
      alice <# "bob> hello 1"

      alice ##> "/i bob"
      contactInfoChatVRange alice supportedChatVRange

      bob ##> "/i alice"
      contactInfoChatVRange bob supportedChatVRange

    withTestChatCfg tmp cfg11 "bob" $ \bob -> do
      bob <## "1 contacts connected (use /cs for the list)"

      bob #> "@alice hello 2"
      alice <# "bob> hello 2"

      alice ##> "/i bob"
      contactInfoChatVRange alice vr11

      bob ##> "/i alice"
      contactInfoChatVRange bob supportedChatVRange
  where
    cfg11 = testCfg {chatVRange = vr11} :: ChatConfig

testGetNetworkStatuses :: HasCallStack => FilePath -> IO ()
testGetNetworkStatuses tmp = do
  withNewTestChatCfg tmp cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg tmp cfg "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice ##> "/_network_statuses"
      alice <## "1 connections connected"
  withTestChatCfg tmp cfg "alice" $ \alice ->
    withTestChatCfg tmp cfg "bob" $ \bob -> do
      alice <## "1 connections connected"
      bob <## "1 connections connected"
  where
    cfg = testCfg {coreApi = True}

vr11 :: VersionRange
vr11 = mkVersionRange 1 1

contactInfoChatVRange :: TestCC -> VersionRange -> IO ()
contactInfoChatVRange cc (VersionRange minVer maxVer) = do
  cc <## "contact ID: 2"
  cc <## "receiving messages via: localhost"
  cc <## "sending messages via: localhost"
  cc <## "you've shared main profile with this contact"
  cc <## "connection not verified, use /code command to see security code"
  cc <## ("peer chat protocol version range: (" <> show minVer <> ", " <> show maxVer <> ")")
