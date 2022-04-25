{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests where

import ChatClient
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Data.ByteString as B
import Data.Char (isDigit)
import qualified Data.Text as T
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Types (ConnStatus (..), ImageData (..), Profile (..), User (..))
import Simplex.Chat.Util (unlessM)
import System.Directory (copyFile, doesFileExist)
import Test.Hspec

aliceProfile :: Profile
aliceProfile = Profile {displayName = "alice", fullName = "Alice", image = Nothing}

bobProfile :: Profile
bobProfile = Profile {displayName = "bob", fullName = "Bob", image = Just (ImageData "data:image/png;base64,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")}

cathProfile :: Profile
cathProfile = Profile {displayName = "cath", fullName = "Catherine", image = Nothing}

danProfile :: Profile
danProfile = Profile {displayName = "dan", fullName = "Daniel", image = Nothing}

chatTests :: Spec
chatTests = do
  describe "direct messages" $ do
    it "add contact and send/receive message" testAddContact
    it "direct message quoted replies" testDirectMessageQuotedReply
    it "direct message update" testDirectMessageUpdate
    it "direct message delete" testDirectMessageDelete
  describe "chat groups" $ do
    it "add contacts, create group and send/receive messages" testGroup
    it "create and join group with 4 members" testGroup2
    it "create and delete group" testGroupDelete
    it "invitee delete group when in status invited" testGroupDeleteWhenInvited
    it "re-add member in status invited" testGroupReAddInvited
    it "remove contact from group and add again" testGroupRemoveAdd
    it "list groups containing group invitations" testGroupList
    it "group message quoted replies" testGroupMessageQuotedReply
    it "group message update" testGroupMessageUpdate
    it "group message delete" testGroupMessageDelete
  describe "user profiles" $ do
    it "update user profiles and notify contacts" testUpdateProfile
    it "update user profile with image" testUpdateProfileImage
  describe "sending and receiving files" $ do
    it "send and receive file" testFileTransfer
    it "send and receive a small file" testSmallFileTransfer
    it "sender cancelled file transfer" testFileSndCancel
    it "recipient cancelled file transfer" testFileRcvCancel
    it "send and receive file to group" testGroupFileTransfer
  describe "sending and receiving files v2" $ do
    it "send and receive file" testFileTransferV2
    it "send and receive a small file" testSmallFileTransferV2
    it "sender cancelled file transfer" testFileSndCancelV2
    it "recipient cancelled file transfer" testFileRcvCancelV2
    it "send and receive file to group" testGroupFileTransferV2
  describe "messages with files" $ do
    it "send and receive message with file" testMessageWithFile
    it "send and receive image" testSendImage
    it "files folder: send and receive image" testFilesFoldersSendImage
    it "files folder: sender deleted file during transfer" testFilesFoldersImageSndDelete
    it "files folder: recipient deleted file during transfer" testFilesFoldersImageRcvDelete
    it "send and receive image with text and quote" testSendImageWithTextAndQuote
    it "send and receive image to group" testGroupSendImage
    it "send and receive image with text and quote to group" testGroupSendImageWithTextAndQuote
  describe "user contact link" $ do
    it "create and connect via contact link" testUserContactLink
    it "auto accept contact requests" testUserContactLinkAutoAccept
    it "deduplicate contact requests" testDeduplicateContactRequests
    it "deduplicate contact requests with profile change" testDeduplicateContactRequestsProfileChange
    it "reject contact and delete contact link" testRejectContactAndDeleteUserContact
    it "delete connection requests when contact link deleted" testDeleteConnectionRequests
  describe "SMP servers" $
    it "get and set SMP servers" testGetSetSMPServers

testAddContact :: IO ()
testAddContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/c"
      inv <- getInvitation alice
      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      chatsEmpty alice bob
      alice #> "@bob hello 🙂"
      bob <# "alice> hello 🙂"
      chatsOneMessage alice bob
      bob #> "@alice hi"
      alice <# "bob> hi"
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
      alice @@@ [("@bob_1", "hi"), ("@bob", "hi")]
      bob @@@ [("@alice_1", "hi"), ("@alice", "hi")]
      -- test deleting contact
      alice ##> "/d bob_1"
      alice <## "bob_1: contact is deleted"
      alice ##> "@bob_1 hey"
      alice <## "no contact bob_1"
      alice @@@ [("@bob", "hi")]
      bob @@@ [("@alice_1", "hi"), ("@alice", "hi")]
  where
    chatsEmpty alice bob = do
      alice @@@ [("@bob", "")]
      alice #$> ("/_get chat @2 count=100", chat, [])
      bob @@@ [("@alice", "")]
      bob #$> ("/_get chat @2 count=100", chat, [])
    chatsOneMessage alice bob = do
      alice @@@ [("@bob", "hello 🙂")]
      alice #$> ("/_get chat @2 count=100", chat, [(1, "hello 🙂")])
      bob @@@ [("@alice", "hello 🙂")]
      bob #$> ("/_get chat @2 count=100", chat, [(0, "hello 🙂")])
    chatsManyMessages alice bob = do
      alice @@@ [("@bob", "hi")]
      alice #$> ("/_get chat @2 count=100", chat, [(1, "hello 🙂"), (0, "hi")])
      bob @@@ [("@alice", "hi")]
      bob #$> ("/_get chat @2 count=100", chat, [(0, "hello 🙂"), (1, "hi")])
      -- pagination
      alice #$> ("/_get chat @2 after=1 count=100", chat, [(0, "hi")])
      alice #$> ("/_get chat @2 before=2 count=100", chat, [(1, "hello 🙂")])
      -- read messages
      alice #$> ("/_read chat @2 from=1 to=100", id, "ok")
      bob #$> ("/_read chat @2 from=1 to=100", id, "ok")

testDirectMessageQuotedReply :: IO ()
testDirectMessageQuotedReply =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"type\": \"text\", \"text\": \"hello! how are you?\"}"
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

testDirectMessageUpdate :: IO ()
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

      alice #$> ("/_get chat @2 count=100", chat', [((1, "hello 🙂"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂"))])
      bob #$> ("/_get chat @2 count=100", chat', [((0, "hello 🙂"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂"))])

      alice #$> ("/_update item @2 1 text hey 👋", id, "message updated")
      bob <# "alice> [edited] hey 👋"

      alice #$> ("/_get chat @2 count=100", chat', [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂"))])
      bob #$> ("/_get chat @2 count=100", chat', [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂"))])

      -- msg id 3
      bob `send` "> @alice (hey) hey alice"
      bob <# "@alice > hey 👋"
      bob <## "      hey alice"
      alice <# "bob> > hey 👋"
      alice <## "      hey alice"

      alice #$> ("/_get chat @2 count=100", chat', [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂")), ((0, "hey alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂")), ((1, "hey alice"), Just (0, "hey 👋"))])

      alice #$> ("/_update item @2 1 text greetings 🤝", id, "message updated")
      bob <# "alice> [edited] greetings 🤝"

      alice #$> ("/_update item @2 2 text updating bob's message", id, "cannot update this item")

      alice #$> ("/_get chat @2 count=100", chat', [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂")), ((0, "hey alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂")), ((1, "hey alice"), Just (0, "hey 👋"))])

      bob #$> ("/_update item @2 2 text hey Alice", id, "message updated")
      alice <# "bob> [edited] > hello 🙂"
      alice <## "      hey Alice"

      bob #$> ("/_update item @2 3 text greetings Alice", id, "message updated")
      alice <# "bob> [edited] > hey 👋"
      alice <## "      greetings Alice"

      alice #$> ("/_get chat @2 count=100", chat', [((1, "greetings 🤝"), Nothing), ((0, "hey Alice"), Just (1, "hello 🙂")), ((0, "greetings Alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', [((0, "greetings 🤝"), Nothing), ((1, "hey Alice"), Just (0, "hello 🙂")), ((1, "greetings Alice"), Just (0, "hey 👋"))])

testDirectMessageDelete :: IO ()
testDirectMessageDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      -- msg id 1
      alice #> "@bob hello 🙂"
      bob <# "alice> hello 🙂"

      -- msg id 2
      bob `send` "> @alice (hello) hey alic"
      bob <# "@alice > hello 🙂"
      bob <## "      hey alic"
      alice <# "bob> > hello 🙂"
      alice <## "      hey alic"

      alice #$> ("/_delete item @2 1 internal", id, "message deleted")
      alice #$> ("/_delete item @2 2 internal", id, "message deleted")

      alice @@@ [("@bob", "")]
      alice #$> ("/_get chat @2 count=100", chat, [])

      alice #$> ("/_update item @2 1 text updating deleted message", id, "cannot update this item")
      alice #$> ("/_send @2 quoted 1 text quoting deleted message", id, "cannot reply to this message")

      bob #$> ("/_update item @2 2 text hey alice", id, "message updated")
      alice <# "bob> [edited] hey alice"

      alice @@@ [("@bob", "hey alice")]
      alice #$> ("/_get chat @2 count=100", chat, [(0, "hey alice")])

      -- msg id 3
      bob #> "@alice how are you?"
      alice <# "bob> how are you?"

      bob #$> ("/_delete item @2 3 broadcast", id, "message deleted")
      alice <# "bob> [deleted] how are you?"

      alice #$> ("/_delete item @2 1 broadcast", id, "message deleted")
      bob <# "alice> [deleted] hello 🙂"

      alice #$> ("/_delete item @2 2 broadcast", id, "cannot delete this item")
      alice #$> ("/_delete item @2 2 internal", id, "message deleted")

      alice @@@ [("@bob", "this item is deleted (broadcast)")]
      alice #$> ("/_get chat @2 count=100", chat, [(0, "this item is deleted (broadcast)")])
      bob @@@ [("@alice", "hey alice")]
      bob #$> ("/_get chat @2 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((1, "hey alice"), (Just (0, "hello 🙂")))])

testGroup :: IO ()
testGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "use /a team <name> to add members"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice ##> "/a team cath"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to cath",
          do
            cath <## "#team: alice invites you to join the group as admin"
            cath <## "use /j team to accept"
        ]
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          do
            cath <## "#team: you joined the group"
            cath <## "#team: member bob (Bob) is connected",
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      threadDelay 1000000 -- server assigns timestamps with one second precision
      bob #> "#team hi there"
      concurrently_
        (alice <# "#team bob> hi there")
        (cath <# "#team bob> hi there")
      threadDelay 1000000
      cath #> "#team hey team"
      concurrently_
        (alice <# "#team cath> hey team")
        (bob <# "#team cath> hey team")
      bob <##> cath
      getReadChats alice bob cath
      -- list groups
      alice ##> "/gs"
      alice <## "#team"
      -- list group members
      alice ##> "/ms team"
      alice
        <### [ "alice (Alice): owner, you, created group",
               "bob (Bob): admin, invited, connected",
               "cath (Catherine): admin, invited, connected"
             ]
      -- list contacts
      alice ##> "/cs"
      alice <## "bob (Bob)"
      alice <## "cath (Catherine)"
      -- remove member
      bob ##> "/rm team cath"
      concurrentlyN_
        [ bob <## "#team: you removed cath from the group",
          alice <## "#team: bob removed cath from the group",
          do
            cath <## "#team: bob removed you from the group"
            cath <## "use /d #team to delete the group"
        ]
      bob #> "#team hi"
      concurrently_
        (alice <# "#team bob> hi")
        (cath </)
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath </)
      cath ##> "#team hello"
      cath <## "you are no longer a member of the group"
      bob <##> cath
  where
    getReadChats alice bob cath = do
      alice @@@ [("#team", "hey team"), ("@cath", ""), ("@bob", "")]
      alice #$> ("/_get chat #1 count=100", chat, [(1, "hello"), (0, "hi there"), (0, "hey team")])
      alice #$> ("/_get chat #1 after=1 count=100", chat, [(0, "hi there"), (0, "hey team")])
      alice #$> ("/_get chat #1 before=3 count=100", chat, [(1, "hello"), (0, "hi there")])
      bob @@@ [("@cath", "hey"), ("#team", "hey team"), ("@alice", "")]
      bob #$> ("/_get chat #1 count=100", chat, [(0, "hello"), (1, "hi there"), (0, "hey team")])
      cath @@@ [("@bob", "hey"), ("#team", "hey team"), ("@alice", "")]
      cath #$> ("/_get chat #1 count=100", chat, [(0, "hello"), (0, "hi there"), (1, "hey team")])
      alice #$> ("/_read chat #1 from=1 to=100", id, "ok")
      bob #$> ("/_read chat #1 from=1 to=100", id, "ok")
      cath #$> ("/_read chat #1 from=1 to=100", id, "ok")

testGroup2 :: IO ()
testGroup2 =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob dan
      connectUsers alice dan
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "use /a club <name> to add members"
      alice ##> "/a club bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to bob",
          do
            bob <## "#club: alice invites you to join the group as admin"
            bob <## "use /j club to accept"
        ]
      alice ##> "/a club cath"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to cath",
          do
            cath <## "#club: alice invites you to join the group as admin"
            cath <## "use /j club to accept"
        ]
      bob ##> "/j club"
      concurrently_
        (alice <## "#club: bob joined the group")
        (bob <## "#club: you joined the group")
      cath ##> "/j club"
      concurrentlyN_
        [ alice <## "#club: cath joined the group",
          do
            cath <## "#club: you joined the group"
            cath <## "#club: member bob (Bob) is connected",
          do
            bob <## "#club: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#club: new member cath is connected"
        ]
      bob ##> "/a club dan"
      concurrentlyN_
        [ bob <## "invitation to join the group #club sent to dan",
          do
            dan <## "#club: bob invites you to join the group as admin"
            dan <## "use /j club to accept"
        ]
      dan ##> "/j club"
      concurrentlyN_
        [ bob <## "#club: dan joined the group",
          do
            dan <## "#club: you joined the group"
            dan
              <### [ "#club: member alice_1 (Alice) is connected",
                     "contact alice_1 is merged into alice",
                     "use @alice <message> to send messages",
                     "#club: member cath (Catherine) is connected"
                   ],
          do
            alice <## "#club: bob added dan_1 (Daniel) to the group (connecting...)"
            alice <## "#club: new member dan_1 is connected"
            alice <## "contact dan_1 is merged into dan"
            alice <## "use @dan <message> to send messages",
          do
            cath <## "#club: bob added dan (Daniel) to the group (connecting...)"
            cath <## "#club: new member dan is connected"
        ]
      alice #> "#club hello"
      concurrentlyN_
        [ bob <# "#club alice> hello",
          cath <# "#club alice> hello",
          dan <# "#club alice> hello"
        ]
      bob #> "#club hi there"
      concurrentlyN_
        [ alice <# "#club bob> hi there",
          cath <# "#club bob> hi there",
          dan <# "#club bob> hi there"
        ]
      cath #> "#club hey"
      concurrentlyN_
        [ alice <# "#club cath> hey",
          bob <# "#club cath> hey",
          dan <# "#club cath> hey"
        ]
      dan #> "#club how is it going?"
      concurrentlyN_
        [ alice <# "#club dan> how is it going?",
          bob <# "#club dan> how is it going?",
          cath <# "#club dan> how is it going?"
        ]
      bob <##> cath
      dan <##> cath
      dan <##> alice
      -- remove member
      cath ##> "/rm club dan"
      concurrentlyN_
        [ cath <## "#club: you removed dan from the group",
          alice <## "#club: cath removed dan from the group",
          bob <## "#club: cath removed dan from the group",
          do
            dan <## "#club: cath removed you from the group"
            dan <## "use /d #club to delete the group"
        ]
      alice #> "#club hello"
      concurrentlyN_
        [ bob <# "#club alice> hello",
          cath <# "#club alice> hello",
          (dan </)
        ]
      bob #> "#club hi there"
      concurrentlyN_
        [ alice <# "#club bob> hi there",
          cath <# "#club bob> hi there",
          (dan </)
        ]
      cath #> "#club hey"
      concurrentlyN_
        [ alice <# "#club cath> hey",
          bob <# "#club cath> hey",
          (dan </)
        ]
      dan ##> "#club how is it going?"
      dan <## "you are no longer a member of the group"
      dan ##> "/d #club"
      dan <## "#club: you deleted the group"
      dan <##> cath
      dan <##> alice
      -- member leaves
      bob ##> "/l club"
      concurrentlyN_
        [ do
            bob <## "#club: you left the group"
            bob <## "use /d #club to delete the group",
          alice <## "#club: bob left the group",
          cath <## "#club: bob left the group"
        ]
      alice #> "#club hello"
      concurrently_
        (cath <# "#club alice> hello")
        (bob </)
      cath #> "#club hey"
      concurrently_
        (alice <# "#club cath> hey")
        (bob </)
      bob ##> "#club how is it going?"
      bob <## "you are no longer a member of the group"
      bob ##> "/d #club"
      bob <## "#club: you deleted the group"
      bob <##> cath
      bob <##> alice

testGroupDelete :: IO ()
testGroupDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/d #team"
      concurrentlyN_
        [ alice <## "#team: you deleted the group",
          do
            bob <## "#team: alice deleted the group"
            bob <## "use /d #team to delete the local copy of the group",
          do
            cath <## "#team: alice deleted the group"
            cath <## "use /d #team to delete the local copy of the group"
        ]
      alice ##> "#team hi"
      alice <## "no group #team"
      bob ##> "/d #team"
      bob <## "#team: you deleted the group"
      cath ##> "#team hi"
      cath <## "you are no longer a member of the group"
      cath ##> "/d #team"
      cath <## "#team: you deleted the group"

testGroupDeleteWhenInvited :: IO ()
testGroupDeleteWhenInvited =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "use /a team <name> to add members"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      bob ##> "/d #team"
      bob <## "#team: you deleted the group"
      -- alice doesn't receive notification that bob deleted group,
      -- but she can re-add bob
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]

testGroupReAddInvited :: IO ()
testGroupReAddInvited =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "use /a team <name> to add members"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      -- alice re-adds bob, he sees it as the same group
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      -- if alice removes bob and then re-adds him, she uses a new connection request
      -- and he sees it as a new group with a different local display name
      alice ##> "/rm team bob"
      alice <## "#team: you removed bob from the group"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team_1 (team): alice invites you to join the group as admin"
            bob <## "use /j team_1 to accept"
        ]

testGroupRemoveAdd :: IO ()
testGroupRemoveAdd =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- remove member
      alice ##> "/rm team bob"
      concurrentlyN_
        [ alice <## "#team: you removed bob from the group",
          do
            bob <## "#team: alice removed you from the group"
            bob <## "use /d #team to delete the group",
          cath <## "#team: alice removed bob from the group"
        ]
      alice ##> "/a team bob"
      alice <## "invitation to join the group #team sent to bob"
      bob <## "#team_1 (team): alice invites you to join the group as admin"
      bob <## "use /j team_1 to accept"
      bob ##> "/j team_1"
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team_1: you joined the group"
            bob <## "#team_1: member cath_1 (Catherine) is connected"
            bob <## "contact cath_1 is merged into cath"
            bob <## "use @cath <message> to send messages",
          do
            cath <## "#team: alice added bob_1 (Bob) to the group (connecting...)"
            cath <## "#team: new member bob_1 is connected"
            cath <## "contact bob_1 is merged into bob"
            cath <## "use @bob <message> to send messages"
        ]
      alice #> "#team hi"
      concurrently_
        (bob <# "#team_1 alice> hi")
        (cath <# "#team alice> hi")
      bob #> "#team_1 hey"
      concurrently_
        (alice <# "#team bob> hey")
        (cath <# "#team bob> hey")
      cath #> "#team hello"
      concurrently_
        (alice <# "#team cath> hello")
        (bob <# "#team_1 cath> hello")

testGroupList :: IO ()
testGroupList =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice ##> "/g tennis"
      alice <## "group #tennis is created"
      alice <## "use /a tennis <name> to add members"
      alice ##> "/a tennis bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #tennis sent to bob",
          do
            bob <## "#tennis: alice invites you to join the group as admin"
            bob <## "use /j tennis to accept"
        ]
      -- alice sees both groups
      alice ##> "/gs"
      alice <### ["#team", "#tennis"]
      -- bob sees #tennis as invitation
      bob ##> "/gs"
      bob
        <### [ "#team",
               "#tennis - you are invited (/j tennis to join, /d #tennis to delete invitation)"
             ]
      -- after deleting invitation bob sees only one group
      bob ##> "/d #tennis"
      bob <## "#tennis: you deleted the group"
      bob ##> "/gs"
      bob <## "#team"

testGroupMessageQuotedReply :: IO ()
testGroupMessageQuotedReply =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice #> "#team hello! how are you?"
      concurrently_
        (bob <# "#team alice> hello! how are you?")
        (cath <# "#team alice> hello! how are you?")
      threadDelay 1000000
      bob `send` "> #team @alice (hello) hello, all good, you?"
      bob <# "#team > alice hello! how are you?"
      bob <## "      hello, all good, you?"
      concurrently_
        ( do
            alice <# "#team bob> > alice hello! how are you?"
            alice <## "      hello, all good, you?"
        )
        ( do
            cath <# "#team bob> > alice hello! how are you?"
            cath <## "      hello, all good, you?"
        )
      bob #$> ("/_get chat #1 count=100", chat', [((0, "hello! how are you?"), Nothing), ((1, "hello, all good, you?"), Just (0, "hello! how are you?"))])
      alice #$> ("/_get chat #1 count=100", chat', [((1, "hello! how are you?"), Nothing), ((0, "hello, all good, you?"), Just (1, "hello! how are you?"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "hello! how are you?"), Nothing), ((0, "hello, all good, you?"), Just (0, "hello! how are you?"))])
      bob `send` "> #team bob (hello, all good) will tell more"
      bob <# "#team > bob hello, all good, you?"
      bob <## "      will tell more"
      concurrently_
        ( do
            alice <# "#team bob> > bob hello, all good, you?"
            alice <## "      will tell more"
        )
        ( do
            cath <# "#team bob> > bob hello, all good, you?"
            cath <## "      will tell more"
        )
      bob #$> ("/_get chat #1 count=1", chat', [((1, "will tell more"), Just (1, "hello, all good, you?"))])
      alice #$> ("/_get chat #1 count=1", chat', [((0, "will tell more"), Just (0, "hello, all good, you?"))])
      cath #$> ("/_get chat #1 count=1", chat', [((0, "will tell more"), Just (0, "hello, all good, you?"))])
      threadDelay 1000000
      cath `send` "> #team bob (hello) hi there!"
      cath <# "#team > bob hello, all good, you?"
      cath <## "      hi there!"
      concurrently_
        ( do
            alice <# "#team cath> > bob hello, all good, you?"
            alice <## "      hi there!"
        )
        ( do
            bob <# "#team cath> > bob hello, all good, you?"
            bob <## "      hi there!"
        )
      cath #$> ("/_get chat #1 count=1", chat', [((1, "hi there!"), Just (0, "hello, all good, you?"))])
      alice #$> ("/_get chat #1 count=1", chat', [((0, "hi there!"), Just (0, "hello, all good, you?"))])
      bob #$> ("/_get chat #1 count=1", chat', [((0, "hi there!"), Just (1, "hello, all good, you?"))])
      alice `send` "> #team (will tell) go on"
      alice <# "#team > bob will tell more"
      alice <## "      go on"
      concurrently_
        ( do
            bob <# "#team alice> > bob will tell more"
            bob <## "      go on"
        )
        ( do
            cath <# "#team alice> > bob will tell more"
            cath <## "      go on"
        )

testGroupMessageUpdate :: IO ()
testGroupMessageUpdate =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- msg id 1
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")

      alice #$> ("/_update item #1 1 text hey 👋", id, "message updated")
      concurrently_
        (bob <# "#team alice> [edited] hey 👋")
        (cath <# "#team alice> [edited] hey 👋")

      alice #$> ("/_get chat #1 count=100", chat', [((1, "hey 👋"), Nothing)])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "hey 👋"), Nothing)])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "hey 👋"), Nothing)])

      threadDelay 1000000
      -- msg id 2
      bob `send` "> #team @alice (hey) hi alice"
      bob <# "#team > alice hey 👋"
      bob <## "      hi alice"
      concurrently_
        ( do
            alice <# "#team bob> > alice hey 👋"
            alice <## "      hi alice"
        )
        ( do
            cath <# "#team bob> > alice hey 👋"
            cath <## "      hi alice"
        )

      alice #$> ("/_get chat #1 count=100", chat', [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hey 👋"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "hey 👋"), Nothing), ((0, "hi alice"), Just (0, "hey 👋"))])

      alice #$> ("/_update item #1 1 text greetings 🤝", id, "message updated")
      concurrently_
        (bob <# "#team alice> [edited] greetings 🤝")
        (cath <# "#team alice> [edited] greetings 🤝")

      alice #$> ("/_update item #1 2 text updating bob's message", id, "cannot update this item")

      threadDelay 1000000
      cath `send` "> #team @alice (greetings) greetings!"
      cath <# "#team > alice greetings 🤝"
      cath <## "      greetings!"
      concurrently_
        ( do
            alice <# "#team cath> > alice greetings 🤝"
            alice <## "      greetings!"
        )
        ( do
            bob <# "#team cath> > alice greetings 🤝"
            bob <## "      greetings!"
        )

      alice #$> ("/_get chat #1 count=100", chat', [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hey 👋")), ((0, "greetings!"), Just (1, "greetings 🤝"))])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hey 👋")), ((0, "greetings!"), Just (0, "greetings 🤝"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (0, "hey 👋")), ((1, "greetings!"), Just (0, "greetings 🤝"))])

testGroupMessageDelete :: IO ()
testGroupMessageDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- msg id 1
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")

      alice #$> ("/_delete item #1 1 internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=100", chat, [])
      bob #$> ("/_get chat #1 count=100", chat, [(0, "hello!")])
      cath #$> ("/_get chat #1 count=100", chat, [(0, "hello!")])

      alice #$> ("/_update item #1 1 text updating deleted message", id, "cannot update this item")
      alice #$> ("/_send #1 quoted 1 text quoting deleted message", id, "cannot reply to this message")

      threadDelay 1000000
      -- msg id 2
      bob `send` "> #team @alice (hello) hi alic"
      bob <# "#team > alice hello!"
      bob <## "      hi alic"
      concurrently_
        ( do
            alice <# "#team bob> > alice hello!"
            alice <## "      hi alic"
        )
        ( do
            cath <# "#team bob> > alice hello!"
            cath <## "      hi alic"
        )

      alice #$> ("/_get chat #1 count=100", chat', [((0, "hi alic"), Just (1, "hello!"))])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "hello!"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "hello!"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      alice #$> ("/_delete item #1 1 broadcast", id, "message deleted")
      concurrently_
        (bob <# "#team alice> [deleted] hello!")
        (cath <# "#team alice> [deleted] hello!")

      alice #$> ("/_delete item #1 2 internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=100", chat', [])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      bob #$> ("/_update item #1 2 text hi alice", id, "message updated")
      concurrently_
        (alice <# "#team bob> [edited] hi alice")
        ( do
            cath <# "#team bob> [edited] > alice hello!"
            cath <## "      hi alice"
        )

      alice #$> ("/_get chat #1 count=100", chat', [((0, "hi alice"), Nothing)])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((1, "hi alice"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((0, "hi alice"), Just (0, "hello!"))])

      threadDelay 1000000
      -- msg id 3
      cath #> "#team how are you?"
      concurrently_
        (alice <# "#team cath> how are you?")
        (bob <# "#team cath> how are you?")

      cath #$> ("/_delete item #1 3 broadcast", id, "message deleted")
      concurrently_
        (alice <# "#team cath> [deleted] how are you?")
        (bob <# "#team cath> [deleted] how are you?")

      alice #$> ("/_delete item #1 2 broadcast", id, "cannot delete this item")
      alice #$> ("/_delete item #1 2 internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing)])
      bob #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((1, "hi alice"), Just (0, "hello!")), ((0, "this item is deleted (broadcast)"), Nothing)])
      cath #$> ("/_get chat #1 count=100", chat', [((0, "this item is deleted (broadcast)"), Nothing), ((0, "hi alice"), Just (0, "hello!"))])

testUpdateProfile :: IO ()
testUpdateProfile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <display name> [<full name>] to change it"
      alice <## "(the updated profile will be sent to all your contacts)"
      alice ##> "/p alice"
      concurrentlyN_
        [ alice <## "user full name removed (your contacts are notified)",
          bob <## "contact alice removed full name",
          cath <## "contact alice removed full name"
        ]
      alice ##> "/p alice Alice Jones"
      concurrentlyN_
        [ alice <## "user full name changed to Alice Jones (your contacts are notified)",
          bob <## "contact alice updated full name: Alice Jones",
          cath <## "contact alice updated full name: Alice Jones"
        ]
      cath ##> "/p cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cate (your contacts are notified)",
          do
            alice <## "contact cath changed to cate"
            alice <## "use @cate <message> to send messages",
          do
            bob <## "contact cath changed to cate"
            bob <## "use @cate <message> to send messages"
        ]
      cath ##> "/p cat Cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cat (Cate) (your contacts are notified)",
          do
            alice <## "contact cate changed to cat (Cate)"
            alice <## "use @cat <message> to send messages",
          do
            bob <## "contact cate changed to cat (Cate)"
            bob <## "use @cat <message> to send messages"
        ]

testUpdateProfileImage :: IO ()
testUpdateProfileImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/profile_image data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="
      alice <## "profile image updated"
      alice ##> "/profile_image"
      alice <## "profile image removed"
      alice ##> "/_profile {\"displayName\": \"alice2\", \"fullName\": \"\"}"
      alice <## "user profile is changed to alice2 (your contacts are notified)"
      bob <## "contact alice changed to alice2"
      bob <## "use @alice2 <message> to send messages"
      (bob </)

testFileTransfer :: IO ()
testFileTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      concurrentlyN_
        [ do
            bob #> "@alice receiving here..."
            bob <## "completed receiving file 1 (test.jpg) from alice",
          do
            alice <# "bob> receiving here..."
            alice <## "completed sending file 1 (test.jpg) to bob"
        ]
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testSmallFileTransfer :: IO ()
testSmallFileTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #> "/f @bob ./tests/fixtures/test.txt"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.txt (11 bytes / 11 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.txt"
      concurrentlyN_
        [ do
            bob <## "started receiving file 1 (test.txt) from alice"
            bob <## "completed receiving file 1 (test.txt) from alice",
          do
            alice <## "started sending file 1 (test.txt) to bob"
            alice <## "completed sending file 1 (test.txt) to bob"
        ]
      src <- B.readFile "./tests/fixtures/test.txt"
      dest <- B.readFile "./tests/tmp/test.txt"
      dest `shouldBe` src

testFileSndCancel :: IO ()
testFileSndCancel =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      alice ##> "/fc 1"
      concurrentlyN_
        [ do
            alice <## "cancelled sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled: bob"
            alice <## "file transfer cancelled",
          do
            bob <## "alice cancelled sending file 1 (test.jpg)"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg"
        ]
      checkPartialTransfer

testFileRcvCancel :: IO ()
testFileRcvCancel =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      bob ##> "/fs 1"
      getTermLine bob >>= (`shouldStartWith` "receiving file 1 (test.jpg) progress")
      waitFileExists "./tests/tmp/test.jpg"
      bob ##> "/fc 1"
      concurrentlyN_
        [ do
            bob <## "cancelled receiving file 1 (test.jpg) from alice"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg",
          do
            alice <## "bob cancelled receiving file 1 (test.jpg)"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled: bob"
        ]
      checkPartialTransfer

testGroupFileTransfer :: IO ()
testGroupFileTransfer =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice #> "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice ##> "/fs 1"
      getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) not accepted")
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg):"
            alice <### ["  complete: bob", "  not accepted: cath"],
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath"
            alice ##> "/fs 1"
            getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) complete"),
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]

testFileTransferV2 :: IO ()
testFileTransferV2 =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransferV2 alice bob
      concurrentlyN_
        [ do
            bob #> "@alice receiving here..."
            bob <## "completed receiving file 1 (test.jpg) from alice",
          do
            alice <# "bob> receiving here..."
            alice <## "completed sending file 1 (test.jpg) to bob"
        ]
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testSmallFileTransferV2 :: IO ()
testSmallFileTransferV2 =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice `send` "/f_v2 @bob ./tests/fixtures/test.txt"
      alice <# "/f @bob ./tests/fixtures/test.txt"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.txt (11 bytes / 11 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.txt"
      concurrentlyN_
        [ do
            bob <## "started receiving file 1 (test.txt) from alice"
            bob <## "completed receiving file 1 (test.txt) from alice",
          do
            alice <## "started sending file 1 (test.txt) to bob"
            alice <## "completed sending file 1 (test.txt) to bob"
        ]
      src <- B.readFile "./tests/fixtures/test.txt"
      dest <- B.readFile "./tests/tmp/test.txt"
      dest `shouldBe` src

testFileSndCancelV2 :: IO ()
testFileSndCancelV2 =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransferV2 alice bob
      alice ##> "/fc 1"
      concurrentlyN_
        [ do
            alice <## "cancelled sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled: bob"
            alice <## "file transfer cancelled",
          do
            bob <## "alice cancelled sending file 1 (test.jpg)"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg"
        ]
      checkPartialTransfer

testFileRcvCancelV2 :: IO ()
testFileRcvCancelV2 =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransferV2 alice bob
      bob ##> "/fs 1"
      getTermLine bob >>= (`shouldStartWith` "receiving file 1 (test.jpg) progress")
      waitFileExists "./tests/tmp/test.jpg"
      bob ##> "/fc 1"
      concurrentlyN_
        [ do
            bob <## "cancelled receiving file 1 (test.jpg) from alice"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg",
          do
            alice <## "bob cancelled receiving file 1 (test.jpg)"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled: bob"
        ]
      checkPartialTransfer

testGroupFileTransferV2 :: IO ()
testGroupFileTransferV2 =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice `send` "/f_v2 #team ./tests/fixtures/test.jpg"
      alice <# "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice ##> "/fs 1"
      getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg): no file transfers")
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) complete: bob",
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath"
            alice ##> "/fs 1"
            getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) complete"),
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]

testMessageWithFile :: IO ()
testMessageWithFile =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 file ./tests/fixtures/test.jpg text hi, sending a file"
      alice <# "@bob hi, sending a file"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> hi, sending a file"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, [((1, "hi, sending a file"), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, [((0, "hi, sending a file"), Just "./tests/tmp/test.jpg")])

testSendImage :: IO ()
testSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 file ./tests/fixtures/test.jpg json {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, [((0, ""), Just "./tests/tmp/test.jpg")])
      -- deleting contact without files folder set should not remove file
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      fileExists <- doesFileExist "./tests/tmp/test.jpg"
      fileExists `shouldBe` True

testFilesFoldersSendImage :: IO ()
testFilesFoldersSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 file test.jpg json {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/app_files/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chatF, [((1, ""), Just "test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, [((0, ""), Just "test.jpg")])
      -- deleting contact with files folder set should remove file
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageSndDelete :: IO ()
testFilesFoldersImageSndDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/tmp/alice_app_files", id, "ok")
      copyFile "./tests/fixtures/test.jpg" "./tests/tmp/alice_app_files/test.jpg"
      bob #$> ("/_files_folder ./tests/tmp/bob_app_files", id, "ok")
      alice ##> "/_send @2 file test.jpg json {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      -- deleting contact should cancel and remove file
      checkActionDeletesFile "./tests/tmp/alice_app_files/test.jpg" $ do
        alice ##> "/d bob"
        alice <## "bob: contact is deleted"
        bob <## "alice cancelled sending file 1 (test.jpg)"
        bob ##> "/fs 1"
        bob <## "receiving file 1 (test.jpg) cancelled, received part path: test.jpg"
      -- deleting contact should remove cancelled file
      checkActionDeletesFile "./tests/tmp/bob_app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageRcvDelete :: IO ()
testFilesFoldersImageRcvDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 file test.jpg json {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "/f @bob test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      -- deleting contact should cancel and remove file
      waitFileExists "./tests/tmp/app_files/test.jpg"
      checkActionDeletesFile "./tests/tmp/app_files/test.jpg" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"
        alice <## "bob cancelled receiving file 1 (test.jpg)"
        alice ##> "/fs 1"
        alice <## "sending file 1 (test.jpg) cancelled: bob"

testSendImageWithTextAndQuote :: IO ()
testSendImageWithTextAndQuote =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      bob #> "@alice hi alice"
      alice <# "bob> hi alice"
      alice ##> "/_send @2 file ./tests/fixtures/test.jpg quoted 1 json {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "@bob > hi alice"
      alice <## "      hey bob"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> > hi alice"
      bob <## "      hey bob"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1 ./tests/tmp"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrently_
        (bob <## "started receiving file 1 (test.jpg) from alice")
        (alice <## "started sending file 1 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 1 (test.jpg) from alice")
        (alice <## "completed sending file 1 (test.jpg) to bob")
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      alice #$> ("/_get chat @2 count=100", chat'', [((0, "hi alice"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi alice"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("@bob", "hey bob")]
      bob #$> ("/_get chat @2 count=100", chat'', [((1, "hi alice"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi alice"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("@alice", "hey bob")]

testGroupSendImage :: IO ()
testGroupSendImage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/_send #1 file ./tests/fixtures/test.jpg json {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob",
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath",
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      dest2 <- B.readFile "./tests/tmp/test_1.jpg"
      dest2 `shouldBe` src
      alice #$> ("/_get chat #1 count=100", chatF, [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat #1 count=100", chatF, [((0, ""), Just "./tests/tmp/test.jpg")])
      cath #$> ("/_get chat #1 count=100", chatF, [((0, ""), Just "./tests/tmp/test_1.jpg")])

testGroupSendImageWithTextAndQuote :: IO ()
testGroupSendImageWithTextAndQuote =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob #> "#team hi team"
      concurrently_
        (alice <# "#team bob> hi team")
        (cath <# "#team bob> hi team")
      threadDelay 1000000
      alice ##> "/_send #1 file ./tests/fixtures/test.jpg quoted 1 json {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}"
      alice <# "#team > bob hi team"
      alice <## "      hey bob"
      alice <# "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> > bob hi team"
            bob <## "      hey bob"
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> > bob hi team"
            cath <## "      hey bob"
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob",
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath",
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src
      dest2 <- B.readFile "./tests/tmp/test_1.jpg"
      dest2 `shouldBe` src
      alice #$> ("/_get chat #1 count=100", chat'', [((0, "hi team"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi team"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("#team", "hey bob"), ("@bob", ""), ("@cath", "")]
      bob #$> ("/_get chat #1 count=100", chat'', [((1, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi team"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("#team", "hey bob"), ("@alice", ""), ("@cath", "")]
      cath #$> ("/_get chat #1 count=100", chat'', [((0, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (0, "hi team"), Just "./tests/tmp/test_1.jpg")])
      cath @@@ [("#team", "hey bob"), ("@alice", ""), ("@bob", "")]

testUserContactLink :: IO ()
testUserContactLink = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request..."
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")
    alice @@@ [("@bob", "")]
    alice <##> bob

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("<@cath", ""), ("@bob", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request..."
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    alice @@@ [("@cath", ""), ("@bob", "hey")]
    alice <##> cath

testUserContactLinkAutoAccept :: IO ()
testUserContactLinkAutoAccept =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice @@@ [("<@bob", "")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice @@@ [("@bob", "")]
      alice <##> bob

      alice ##> "/auto_accept on"
      alice <## "auto_accept on"

      cath ##> ("/c " <> cLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice @@@ [("@cath", ""), ("@bob", "hey")]
      alice <##> cath

      alice ##> "/auto_accept off"
      alice <## "auto_accept off"

      dan ##> ("/c " <> cLink)
      alice <#? dan
      alice @@@ [("<@dan", ""), ("@cath", "hey"), ("@bob", "hey")]
      alice ##> "/ac dan"
      alice <## "dan (Daniel): accepting contact request..."
      concurrently_
        (dan <## "alice (Alice): contact is connected")
        (alice <## "dan (Daniel): contact is connected")
      alice @@@ [("@dan", ""), ("@cath", "hey"), ("@bob", "hey")]
      alice <##> dan

testDeduplicateContactRequests :: IO ()
testDeduplicateContactRequests = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]
    bob @@@! [(":1", "", Just ConnJoined)]

    bob ##> ("/c " <> cLink)
    alice <#? bob
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]
    bob @@@! [(":3", "", Just ConnJoined), (":2", "", Just ConnJoined), (":1", "", Just ConnJoined)]

    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request..."
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")

    bob ##> ("/c " <> cLink)
    bob <## "alice (Alice): contact already exists"
    alice @@@ [("@bob", "")]
    bob @@@ [("@alice", ""), (":2", ""), (":1", "")]
    bob ##> "/_delete :1"
    bob <## "connection :1 deleted"
    bob ##> "/_delete :2"
    bob <## "connection :2 deleted"

    alice <##> bob
    alice @@@ [("@bob", "hey")]
    bob @@@ [("@alice", "hey")]

    bob ##> ("/c " <> cLink)
    bob <## "alice (Alice): contact already exists"

    alice <##> bob
    alice #$> ("/_get chat @2 count=100", chat, [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("<@cath", ""), ("@bob", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request..."
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    alice @@@ [("@cath", ""), ("@bob", "hey")]
    alice <##> cath

testDeduplicateContactRequestsProfileChange :: IO ()
testDeduplicateContactRequestsProfileChange = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]

    bob ##> "/p bob"
    bob <## "user full name removed (your contacts are notified)"
    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice @@@ [("<@bob", "")]

    bob ##> "/p bob Bob Ross"
    bob <## "user full name changed to Bob Ross (your contacts are notified)"
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]

    bob ##> "/p robert Robert"
    bob <## "user profile is changed to robert (Robert) (your contacts are notified)"
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@robert", "")]

    alice ##> "/ac bob"
    alice <## "no contact request from bob"
    alice ##> "/ac robert"
    alice <## "robert (Robert): accepting contact request..."
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "robert (Robert): contact is connected")

    bob ##> ("/c " <> cLink)
    bob <## "alice (Alice): contact already exists"
    alice @@@ [("@robert", "")]
    bob @@@ [("@alice", ""), (":3", ""), (":2", ""), (":1", "")]
    bob ##> "/_delete :1"
    bob <## "connection :1 deleted"
    bob ##> "/_delete :2"
    bob <## "connection :2 deleted"
    bob ##> "/_delete :3"
    bob <## "connection :3 deleted"

    alice <##> bob
    alice @@@ [("@robert", "hey")]
    bob @@@ [("@alice", "hey")]

    bob ##> ("/c " <> cLink)
    bob <## "alice (Alice): contact already exists"

    alice <##> bob
    alice #$> ("/_get chat @2 count=100", chat, [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("<@cath", ""), ("@robert", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request..."
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    alice @@@ [("@cath", ""), ("@robert", "hey")]
    alice <##> cath

testRejectContactAndDeleteUserContact :: IO ()
testRejectContactAndDeleteUserContact = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice ##> "/rc bob"
    alice <## "bob: contact request rejected"
    (bob </)

    alice ##> "/sa"
    cLink' <- getContactLink alice False
    cLink' `shouldBe` cLink

    alice ##> "/da"
    alice <## "Your chat address is deleted - accepted contacts will remain connected."
    alice <## "To create a new chat address use /ad"

    cath ##> ("/c " <> cLink)
    cath <## "error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"

testDeleteConnectionRequests :: IO ()
testDeleteConnectionRequests = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    cath ##> ("/c " <> cLink)
    alice <#? cath

    alice ##> "/da"
    alice <## "Your chat address is deleted - accepted contacts will remain connected."
    alice <## "To create a new chat address use /ad"

    alice ##> "/ad"
    cLink' <- getContactLink alice True
    bob ##> ("/c " <> cLink')
    -- same names are used here, as they were released at /da
    alice <#? bob
    cath ##> ("/c " <> cLink')
    alice <#? cath

testGetSetSMPServers :: IO ()
testGetSetSMPServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp_servers", id, "no custom SMP servers saved")
      alice #$> ("/smp_servers smp://1234-w==@smp1.example.im", id, "ok")
      alice #$> ("/smp_servers", id, "smp://1234-w==@smp1.example.im")
      alice #$> ("/smp_servers smp://2345-w==@smp2.example.im,smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/smp_servers", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")
      alice #$> ("/smp_servers default", id, "ok")
      alice #$> ("/smp_servers", id, "no custom SMP servers saved")

startFileTransfer :: TestCC -> TestCC -> IO ()
startFileTransfer alice bob = do
  alice #> "/f @bob ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  concurrently_
    (bob <## "started receiving file 1 (test.jpg) from alice")
    (alice <## "started sending file 1 (test.jpg) to bob")

startFileTransferV2 :: TestCC -> TestCC -> IO ()
startFileTransferV2 alice bob = do
  alice `send` "/f_v2 @bob ./tests/fixtures/test.jpg"
  alice <# "/f @bob ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  concurrently_
    (bob <## "started receiving file 1 (test.jpg) from alice")
    (alice <## "started sending file 1 (test.jpg) to bob")

checkPartialTransfer :: IO ()
checkPartialTransfer = do
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  B.unpack src `shouldStartWith` B.unpack dest
  B.length src > B.length dest `shouldBe` True

checkActionDeletesFile :: FilePath -> IO () -> IO ()
checkActionDeletesFile file action = do
  fileExistsBefore <- doesFileExist file
  fileExistsBefore `shouldBe` True
  action
  fileExistsAfter <- doesFileExist file
  fileExistsAfter `shouldBe` False

waitFileExists :: FilePath -> IO ()
waitFileExists f = unlessM (doesFileExist f) $ waitFileExists f

connectUsers :: TestCC -> TestCC -> IO ()
connectUsers cc1 cc2 = do
  name1 <- showName cc1
  name2 <- showName cc2
  cc1 ##> "/c"
  inv <- getInvitation cc1
  cc2 ##> ("/c " <> inv)
  cc2 <## "confirmation sent!"
  concurrently_
    (cc2 <## (name1 <> ": contact is connected"))
    (cc1 <## (name2 <> ": contact is connected"))

showName :: TestCC -> IO String
showName (TestCC ChatController {currentUser} _ _ _ _) = do
  Just User {localDisplayName, profile = Profile {fullName}} <- readTVarIO currentUser
  pure . T.unpack $ localDisplayName <> " (" <> fullName <> ")"

createGroup2 :: String -> TestCC -> TestCC -> IO ()
createGroup2 gName cc1 cc2 = do
  connectUsers cc1 cc2
  name2 <- userName cc2
  cc1 ##> ("/g " <> gName)
  cc1 <## ("group #" <> gName <> " is created")
  cc1 <## ("use /a " <> gName <> " <name> to add members")
  addMember gName cc1 cc2
  cc2 ##> ("/j " <> gName)
  concurrently_
    (cc1 <## ("#" <> gName <> ": " <> name2 <> " joined the group"))
    (cc2 <## ("#" <> gName <> ": you joined the group"))

createGroup3 :: String -> TestCC -> TestCC -> TestCC -> IO ()
createGroup3 gName cc1 cc2 cc3 = do
  createGroup2 gName cc1 cc2
  connectUsers cc1 cc3
  name3 <- userName cc3
  sName2 <- showName cc2
  sName3 <- showName cc3
  addMember gName cc1 cc3
  cc3 ##> ("/j " <> gName)
  concurrentlyN_
    [ cc1 <## ("#" <> gName <> ": " <> name3 <> " joined the group"),
      do
        cc3 <## ("#" <> gName <> ": you joined the group")
        cc3 <## ("#" <> gName <> ": member " <> sName2 <> " is connected"),
      do
        cc2 <## ("#" <> gName <> ": alice added " <> sName3 <> " to the group (connecting...)")
        cc2 <## ("#" <> gName <> ": new member " <> name3 <> " is connected")
    ]

addMember :: String -> TestCC -> TestCC -> IO ()
addMember gName inviting invitee = do
  name1 <- userName inviting
  memName <- userName invitee
  inviting ##> ("/a " <> gName <> " " <> memName)
  concurrentlyN_
    [ inviting <## ("invitation to join the group #" <> gName <> " sent to " <> memName),
      do
        invitee <## ("#" <> gName <> ": " <> name1 <> " invites you to join the group as admin")
        invitee <## ("use /j " <> gName <> " to accept")
    ]

-- | test sending direct messages
(<##>) :: TestCC -> TestCC -> IO ()
cc1 <##> cc2 = do
  name1 <- userName cc1
  name2 <- userName cc2
  cc1 #> ("@" <> name2 <> " hi")
  cc2 <# (name1 <> "> hi")
  cc2 #> ("@" <> name1 <> " hey")
  cc1 <# (name2 <> "> hey")

(##>) :: TestCC -> String -> IO ()
cc ##> cmd = do
  cc `send` cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
cc #> cmd = do
  cc `send` cmd
  cc <# cmd

(#$>) :: (Eq a, Show a) => TestCC -> (String, String -> a, a) -> Expectation
cc #$> (cmd, f, res) = do
  cc ##> cmd
  (f <$> getTermLine cc) `shouldReturn` res

chat :: String -> [(Int, String)]
chat = map (\(a, _, _) -> a) . chat''

chat' :: String -> [((Int, String), Maybe (Int, String))]
chat' = map (\(a, b, _) -> (a, b)) . chat''

chatF :: String -> [((Int, String), Maybe String)]
chatF = map (\(a, _, c) -> (a, c)) . chat''

chat'' :: String -> [((Int, String), Maybe (Int, String), Maybe String)]
chat'' = read

(@@@) :: TestCC -> [(String, String)] -> Expectation
(@@@) = getChats . map $ \(ldn, msg, _) -> (ldn, msg)

(@@@!) :: TestCC -> [(String, String, Maybe ConnStatus)] -> Expectation
(@@@!) = getChats id

getChats :: (Eq a, Show a) => ([(String, String, Maybe ConnStatus)] -> [a]) -> TestCC -> [a] -> Expectation
getChats f cc res = do
  cc ##> "/_get chats pcc=on"
  line <- getTermLine cc
  f (read line) `shouldMatchList` res

send :: TestCC -> String -> IO ()
send TestCC {chatController = cc} cmd = atomically $ writeTBQueue (inputQ cc) cmd

(<##) :: TestCC -> String -> Expectation
cc <## line = getTermLine cc `shouldReturn` line

(<###) :: TestCC -> [String] -> Expectation
_ <### [] = pure ()
cc <### ls = do
  line <- getTermLine cc
  if line `elem` ls
    then cc <### filter (/= line) ls
    else error $ "unexpected output: " <> line

(<#) :: TestCC -> String -> Expectation
cc <# line = (dropTime <$> getTermLine cc) `shouldReturn` line

(</) :: TestCC -> Expectation
(</) = (<// 500000)

(<#?) :: TestCC -> TestCC -> Expectation
cc1 <#? cc2 = do
  name <- userName cc2
  sName <- showName cc2
  cc2 <## "connection request sent!"
  cc1 <## (sName <> " wants to connect to you!")
  cc1 <## ("to accept: /ac " <> name)
  cc1 <## ("to reject: /rc " <> name <> " (the sender will NOT be notified)")

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getInvitation :: TestCC -> IO String
getInvitation cc = do
  cc <## "pass this invitation link to your contact (via another channel):"
  cc <## ""
  inv <- getTermLine cc
  cc <## ""
  cc <## "and ask them to connect: /c <invitation_link_above>"
  pure inv

getContactLink :: TestCC -> Bool -> IO String
getContactLink cc created = do
  cc <## if created then "Your new chat address is created!" else "Your chat address:"
  cc <## ""
  link <- getTermLine cc
  cc <## ""
  cc <## "Anybody can send you contact requests with: /c <contact_link_above>"
  cc <## "to show it again: /sa"
  cc <## "to delete it: /da (accepted contacts will remain connected)"
  pure link
