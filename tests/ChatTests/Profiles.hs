{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Profiles where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad
import Control.Monad.Except
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Chat.Store.Shared (createContact)
import Simplex.Chat.Types (ConnStatus (..), GroupMemberRole (..), Profile (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import System.Directory (copyFile, createDirectoryIfMissing)
import Test.Hspec

chatProfileTests :: SpecWith FilePath
chatProfileTests = do
  describe "user profiles" $ do
    it "update user profile and notify contacts" testUpdateProfile
    it "update user profile with image" testUpdateProfileImage
    it "use multiword profile names" testMultiWordProfileNames
  describe "user contact link" $ do
    it "create and connect via contact link" testUserContactLink
    it "add contact link to profile" testProfileLink
    it "auto accept contact requests" testUserContactLinkAutoAccept
    it "deduplicate contact requests" testDeduplicateContactRequests
    it "deduplicate contact requests with profile change" testDeduplicateContactRequestsProfileChange
    it "reject contact and delete contact link" testRejectContactAndDeleteUserContact
    it "delete connection requests when contact link deleted" testDeleteConnectionRequests
    it "auto-reply message" testAutoReplyMessage
    it "auto-reply message in incognito" testAutoReplyMessageInIncognito
  describe "contact address connection plan" $ do
    it "contact address ok to connect; known contact" testPlanAddressOkKnown
    it "own contact address" testPlanAddressOwn
    it "connecting via contact address" testPlanAddressConnecting
    it "re-connect with deleted contact" testPlanAddressContactDeletedReconnected
    it "contact via address" testPlanAddressContactViaAddress
  describe "incognito" $ do
    it "connect incognito via invitation link" testConnectIncognitoInvitationLink
    it "connect incognito via contact address" testConnectIncognitoContactAddress
    it "accept contact request incognito" testAcceptContactRequestIncognito
    it "set connection incognito" testSetConnectionIncognito
    it "reset connection incognito" testResetConnectionIncognito
    it "set connection incognito prohibited during negotiation" testSetConnectionIncognitoProhibitedDuringNegotiation
    it "connection incognito unchanged errors" testConnectionIncognitoUnchangedErrors
    it "set, reset, set connection incognito" testSetResetSetConnectionIncognito
    it "join group incognito" testJoinGroupIncognito
    it "can't invite contact to whom user connected incognito to a group" testCantInviteContactIncognito
    it "can't see global preferences update" testCantSeeGlobalPrefsUpdateIncognito
    it "deleting contact first, group second deletes incognito profile" testDeleteContactThenGroupDeletesIncognitoProfile
    it "deleting group first, contact second deletes incognito profile" testDeleteGroupThenContactDeletesIncognitoProfile
  describe "contact aliases" $ do
    it "set contact alias" testSetAlias
    it "set connection alias" testSetConnectionAlias
  describe "preferences" $ do
    it "set contact preferences" testSetContactPrefs
    it "feature offers" testFeatureOffers
    it "update group preferences" testUpdateGroupPrefs
    it "allow full deletion to contact" testAllowFullDeletionContact
    it "allow full deletion to group" testAllowFullDeletionGroup
    it "prohibit direct messages to group members" testProhibitDirectMessages
    xit'' "enable timed messages with contact" testEnableTimedMessagesContact
    it "enable timed messages in group" testEnableTimedMessagesGroup
    xit'' "timed messages enabled globally, contact turns on" testTimedMessagesEnabledGlobally

testUpdateProfile :: HasCallStack => FilePath -> IO ()
testUpdateProfile =
  testChatCfg3 testCfgCreateGroupDirect aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <display name> to change it"
      alice <## "(the updated profile will be sent to all your contacts)"
      alice ##> "/p alice"
      concurrentlyN_
        [ alice <## "user full name removed (your 2 contacts are notified)",
          bob <## "contact alice removed full name",
          cath <## "contact alice removed full name"
        ]
      alice ##> "/p alice Alice Jones"
      concurrentlyN_
        [ alice <## "user full name changed to Alice Jones (your 2 contacts are notified)",
          bob <## "contact alice updated full name: Alice Jones",
          cath <## "contact alice updated full name: Alice Jones"
        ]
      cath ##> "/p cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cate (your 2 contacts are notified)",
          do
            alice <## "contact cath changed to cate"
            alice <## "use @cate <message> to send messages",
          do
            bob <## "contact cath changed to cate"
            bob <## "use @cate <message> to send messages"
        ]
      cath ##> "/p cat Cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cat (Cate) (your 2 contacts are notified)",
          do
            alice <## "contact cate changed to cat (Cate)"
            alice <## "use @cat <message> to send messages",
          do
            bob <## "contact cate changed to cat (Cate)"
            bob <## "use @cat <message> to send messages"
        ]

testUpdateProfileImage :: HasCallStack => FilePath -> IO ()
testUpdateProfileImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/set profile image data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="
      alice <## "profile image updated"
      alice ##> "/show profile image"
      alice <## "Profile image:"
      alice <## "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="
      alice ##> "/delete profile image"
      alice <## "profile image removed"
      alice ##> "/show profile image"
      alice <## "No profile image"
      alice ##> "/_profile 1 {\"displayName\": \"alice2\", \"fullName\": \"\", \"preferences\": {\"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
      alice <## "user profile is changed to alice2 (your 1 contacts are notified)"
      bob <## "contact alice changed to alice2"
      bob <## "use @alice2 <message> to send messages"
      (bob </)

testMultiWordProfileNames :: HasCallStack => FilePath -> IO ()
testMultiWordProfileNames =
  testChat3 aliceProfile' bobProfile' cathProfile' $
    \alice bob cath -> do
      alice ##> "/c"
      inv <- getInvitation alice
      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      concurrently_
        (bob <## "'Alice Jones': contact is connected")
        (alice <## "'Bob James': contact is connected")
      alice #> "@'Bob James' hi"
      bob <# "'Alice Jones'> hi"
      alice ##> "/g 'Our Team'"
      alice <## "group #'Our Team' is created"
      alice <## "to add members use /a 'Our Team' <name> or /create link #'Our Team'"
      alice ##> "/a 'Our Team' 'Bob James' admin"
      alice <## "invitation to join the group #'Our Team' sent to 'Bob James'"
      bob <## "#'Our Team': 'Alice Jones' invites you to join the group as admin"
      bob <## "use /j 'Our Team' to accept"
      bob ##> "/j 'Our Team'"
      bob <## "#'Our Team': you joined the group"
      alice <## "#'Our Team': 'Bob James' joined the group"
      bob ##> "/c"
      inv' <- getInvitation bob
      cath ##> ("/c " <> inv')
      cath <## "confirmation sent!"
      concurrently_
        (cath <## "'Bob James': contact is connected")
        (bob <## "'Cath Johnson': contact is connected")
      bob ##> "/a 'Our Team' 'Cath Johnson'"
      bob <## "invitation to join the group #'Our Team' sent to 'Cath Johnson'"
      cath <## "#'Our Team': 'Bob James' invites you to join the group as member"
      cath <## "use /j 'Our Team' to accept"
      cath ##> "/j 'Our Team'"
      concurrentlyN_
        [ bob <## "#'Our Team': 'Cath Johnson' joined the group",
          do
            cath <## "#'Our Team': you joined the group"
            cath <## "#'Our Team': member 'Alice Jones' is connected",
          do
            alice <## "#'Our Team': 'Bob James' added 'Cath Johnson' to the group (connecting...)"
            alice <## "#'Our Team': new member 'Cath Johnson' is connected"
        ]
      bob #> "#'Our Team' hi"
      alice <# "#'Our Team' 'Bob James'> hi"
      cath <# "#'Our Team' 'Bob James'> hi"
      alice `send` "@'Cath Johnson' hello"
      alice <## "member #'Our Team' 'Cath Johnson' does not have direct connection, creating"
      alice <## "contact for member #'Our Team' 'Cath Johnson' is created"
      alice <## "sent invitation to connect directly to member #'Our Team' 'Cath Johnson'"
      alice <# "@'Cath Johnson' hello"
      cath <## "#'Our Team' 'Alice Jones' is creating direct contact 'Alice Jones' with you"
      cath <# "'Alice Jones'> hello"
      cath <## "'Alice Jones': contact is connected"
      alice <## "'Cath Johnson': contact is connected"
      cath ##> "/p 'Cath J'"
      cath <## "user profile is changed to 'Cath J' (your 2 contacts are notified)"
      alice <## "contact 'Cath Johnson' changed to 'Cath J'"
      alice <## "use @'Cath J' <message> to send messages"
      bob <## "contact 'Cath Johnson' changed to 'Cath J'"
      bob <## "use @'Cath J' <message> to send messages"
      alice #> "@'Cath J' hi"
      cath <# "'Alice Jones'> hi"
  where
    aliceProfile' = baseProfile {displayName = "Alice Jones"}
    bobProfile' = baseProfile {displayName = "Bob James"}
    cathProfile' = baseProfile {displayName = "Cath Johnson"}
    baseProfile = Profile {displayName = "", fullName = "", image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

testUserContactLink :: HasCallStack => FilePath -> IO ()
testUserContactLink =
  testChat3 aliceProfile bobProfile cathProfile $
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
      threadDelay 100000
      alice @@@ [("@bob", lastChatFeature)]
      alice <##> bob

      cath ##> ("/c " <> cLink)
      alice <#? cath
      alice @@@ [("<@cath", ""), ("@bob", "hey")]
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      threadDelay 100000
      alice @@@ [("@cath", lastChatFeature), ("@bob", "hey")]
      alice <##> cath

testProfileLink :: HasCallStack => FilePath -> IO ()
testProfileLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

      alice ##> "/pa on"
      alice <## "new contact address set"

      bob <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink bob cLink

      cath ##> ("/c " <> cLink)
      alice <#? cath
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice <##> cath

      checkAliceProfileLink cath cLink

      alice ##> "/pa off"
      alice <## "contact address removed"

      bob <## "alice removed contact address"
      checkAliceNoProfileLink bob

      cath <## "alice removed contact address"
      checkAliceNoProfileLink cath

      alice ##> "/pa on"
      alice <## "new contact address set"

      bob <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink bob cLink

      cath <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink cath cLink

      alice ##> "/da"
      alice <## "Your chat address is deleted - accepted contacts will remain connected."
      alice <## "To create a new chat address use /ad"

      bob <## "alice removed contact address"
      checkAliceNoProfileLink bob

      cath <## "alice removed contact address"
      checkAliceNoProfileLink cath
  where
    checkAliceProfileLink cc cLink = do
      cc ##> "/info alice"
      cc <## "contact ID: 2"
      cc <##. "receiving messages via"
      cc <##. "sending messages via"
      cc <## ("contact address: " <> cLink)
      cc <## "you've shared main profile with this contact"
      cc <## "connection not verified, use /code command to see security code"
      cc <## currentChatVRangeInfo
    checkAliceNoProfileLink cc = do
      cc ##> "/info alice"
      cc <## "contact ID: 2"
      cc <##. "receiving messages via"
      cc <##. "sending messages via"
      cc <## "you've shared main profile with this contact"
      cc <## "connection not verified, use /code command to see security code"
      cc <## currentChatVRangeInfo

testUserContactLinkAutoAccept :: HasCallStack => FilePath -> IO ()
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
      threadDelay 100000
      alice @@@ [("@bob", lastChatFeature)]
      alice <##> bob

      alice ##> "/auto_accept on"
      alice <## "auto_accept on"

      cath ##> ("/c " <> cLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      threadDelay 100000
      alice @@@ [("@cath", lastChatFeature), ("@bob", "hey")]
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
      threadDelay 100000
      alice @@@ [("@dan", lastChatFeature), ("@cath", "hey"), ("@bob", "hey")]
      alice <##> dan

testDeduplicateContactRequests :: HasCallStack => FilePath -> IO ()
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
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"
    alice @@@ [("@bob", lastChatFeature)]
    bob @@@ [("@alice", lastChatFeature), (":2", ""), (":1", "")]
    bob ##> "/_delete :1"
    bob <## "connection :1 deleted"
    bob ##> "/_delete :2"
    bob <## "connection :2 deleted"

    alice <##> bob
    alice @@@ [("@bob", "hey")]
    bob @@@ [("@alice", "hey")]

    bob ##> ("/c " <> cLink)
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"

    alice <##> bob
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("<@cath", ""), ("@bob", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request..."
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    threadDelay 100000
    alice @@@ [("@cath", lastChatFeature), ("@bob", "hey")]
    alice <##> cath

testDeduplicateContactRequestsProfileChange :: HasCallStack => FilePath -> IO ()
testDeduplicateContactRequestsProfileChange = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]

    bob ##> "/p bob"
    bob <## "user full name removed (your 0 contacts are notified)"
    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice @@@ [("<@bob", "")]

    bob ##> "/p bob Bob Ross"
    bob <## "user full name changed to Bob Ross (your 0 contacts are notified)"
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("<@bob", "")]

    bob ##> "/p robert Robert"
    bob <## "user profile is changed to robert (Robert) (your 0 contacts are notified)"
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
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"
    alice @@@ [("@robert", lastChatFeature)]
    bob @@@ [("@alice", lastChatFeature), (":3", ""), (":2", ""), (":1", "")]
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
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"

    alice <##> bob
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("<@cath", ""), ("@robert", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request..."
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    threadDelay 100000
    alice @@@ [("@cath", lastChatFeature), ("@robert", "hey")]
    alice <##> cath

testRejectContactAndDeleteUserContact :: HasCallStack => FilePath -> IO ()
testRejectContactAndDeleteUserContact = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/_address 1"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice ##> "/rc bob"
    alice <## "bob: contact request rejected"
    (bob </)

    alice ##> "/_show_address 1"
    cLink' <- getContactLink alice False
    alice <## "auto_accept off"
    cLink' `shouldBe` cLink

    alice ##> "/_delete_address 1"
    alice <## "Your chat address is deleted - accepted contacts will remain connected."
    alice <## "To create a new chat address use /ad"

    cath ##> ("/c " <> cLink)
    cath <## "error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"

testDeleteConnectionRequests :: HasCallStack => FilePath -> IO ()
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

testAutoReplyMessage :: HasCallStack => FilePath -> IO ()
testAutoReplyMessage = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    alice ##> "/_auto_accept 1 on incognito=off text hello!"
    alice <## "auto_accept on"
    alice <## "auto reply:"
    alice <## "hello!"

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob (Bob): accepting contact request..."
    concurrentlyN_
      [ do
          bob <## "alice (Alice): contact is connected"
          bob <# "alice> hello!",
        do
          alice <## "bob (Bob): contact is connected"
          alice <# "@bob hello!"
      ]

testAutoReplyMessageInIncognito :: HasCallStack => FilePath -> IO ()
testAutoReplyMessageInIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    alice ##> "/auto_accept on incognito=on text hello!"
    alice <## "auto_accept on, incognito"
    alice <## "auto reply:"
    alice <## "hello!"

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob (Bob): accepting contact request..."
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ do
          bob <## (aliceIncognito <> ": contact is connected")
          bob <# (aliceIncognito <> "> hello!"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice
            <### [ "use /i bob to print out this incognito profile again",
                   WithTime "i @bob hello!"
                 ]
      ]

testPlanAddressOkKnown :: HasCallStack => FilePath -> IO ()
testPlanAddressOkKnown =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: ok to connect"

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice @@@ [("<@bob", "")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"

      let cLinkSchema2 = linkAnotherSchema cLink
      bob ##> ("/_connect plan 1 " <> cLinkSchema2)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"

      bob ##> ("/c " <> cLink)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"

testPlanAddressOwn :: HasCallStack => FilePath -> IO ()
testPlanAddressOwn tmp =
  withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    alice ##> ("/_connect plan 1 " <> cLink)
    alice <## "contact address: own address"

    let cLinkSchema2 = linkAnotherSchema cLink
    alice ##> ("/_connect plan 1 " <> cLinkSchema2)
    alice <## "contact address: own address"

    alice ##> ("/c " <> cLink)
    alice <## "connection request sent!"
    alice <## "alice_1 (Alice) wants to connect to you!"
    alice <## "to accept: /ac alice_1"
    alice <## "to reject: /rc alice_1 (the sender will NOT be notified)"
    alice @@@ [("<@alice_1", ""), (":2", "")]
    alice ##> "/ac alice_1"
    alice <## "alice_1 (Alice): accepting contact request..."
    alice
      <### [ "alice_1 (Alice): contact is connected",
             "alice_2 (Alice): contact is connected"
           ]

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

    alice ##> ("/_connect plan 1 " <> cLink)
    alice <## "contact address: own address"

    alice ##> ("/c " <> cLink)
    alice <## "alice_2 (Alice): contact already exists"

testPlanAddressConnecting :: HasCallStack => FilePath -> IO ()
testPlanAddressConnecting tmp = do
  cLink <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    alice ##> "/ad"
    getContactLink alice True
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    threadDelay 100000

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"

    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting, allowed to reconnect"

    let cLinkSchema2 = linkAnotherSchema cLink
    bob ##> ("/_connect plan 1 " <> cLinkSchema2)
    bob <## "contact address: connecting, allowed to reconnect"

    threadDelay 100000
  withTestChat tmp "alice" $ \alice -> do
    alice <## "Your address is active! To show: /sa"
    alice <## "bob (Bob) wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request..."
  withTestChat tmp "bob" $ \bob -> do
    threadDelay 500000
    bob @@@ [("@alice", "")]
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting to contact alice"

    let cLinkSchema2 = linkAnotherSchema cLink
    bob ##> ("/_connect plan 1 " <> cLinkSchema2)
    bob <## "contact address: connecting to contact alice"

    bob ##> ("/c " <> cLink)
    bob <## "contact address: connecting to contact alice"

testPlanAddressContactDeletedReconnected :: HasCallStack => FilePath -> IO ()
testPlanAddressContactDeletedReconnected =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"

      bob ##> ("/c " <> cLink)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"

      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: ok to connect"

      let cLinkSchema2 = linkAnotherSchema cLink
      bob ##> ("/_connect plan 1 " <> cLinkSchema2)
      bob <## "contact address: ok to connect"

      bob ##> ("/c " <> cLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice_1 (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

      alice #> "@bob hi"
      bob <# "alice_1> hi"
      bob #> "@alice_1 hey"
      alice <# "bob> hey"

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: known contact alice_1"
      bob <## "use @alice_1 <message> to send messages"

      bob ##> ("/_connect plan 1 " <> cLinkSchema2)
      bob <## "contact address: known contact alice_1"
      bob <## "use @alice_1 <message> to send messages"

      bob ##> ("/c " <> cLink)
      bob <## "contact address: known contact alice_1"
      bob <## "use @alice_1 <message> to send messages"

testPlanAddressContactViaAddress :: HasCallStack => FilePath -> IO ()
testPlanAddressContactViaAddress =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      alice ##> "/pa on" -- not necessary, without it bob would receive profile update removing contact link
      alice <## "new contact address set"

      case A.parseOnly strP (B.pack cLink) of
        Left _ -> error "error parsing contact link"
        Right cReq -> do
          let profile = aliceProfile {contactLink = Just cReq}
          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          bob ##> "/delete @alice"
          bob <## "alice: contact is deleted"

          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          bob ##> ("/_connect plan 1 " <> cLink)
          bob <## "contact address: known contact without connection alice"

          let cLinkSchema2 = linkAnotherSchema cLink
          bob ##> ("/_connect plan 1 " <> cLinkSchema2)
          bob <## "contact address: known contact without connection alice"

          -- terminal api
          bob ##> ("/c " <> cLink)
          connecting alice bob

          bob ##> "/_delete @2 notify=off"
          bob <## "alice: contact is deleted"
          alice ##> "/_delete @2 notify=off"
          alice <## "bob: contact is deleted"

          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          -- GUI api
          bob ##> "/_connect contact 1 2"
          connecting alice bob
  where
    connecting alice bob = do
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request..."
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

      alice <##> bob
      bob @@@ [("@alice", "hey")]

testConnectIncognitoInvitationLink :: HasCallStack => FilePath -> IO ()
testConnectIncognitoInvitationLink = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/connect incognito"
    inv <- getInvitation alice
    bob ##> ("/connect incognito " <> inv)
    bob <## "confirmation sent!"
    bobIncognito <- getTermLine bob
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ do
          bob <## (aliceIncognito <> ": contact is connected, your incognito profile for this contact is " <> bobIncognito)
          bob <## ("use /i " <> aliceIncognito <> " to print out this incognito profile again"),
        do
          alice <## (bobIncognito <> ": contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## ("use /i " <> bobIncognito <> " to print out this incognito profile again")
      ]
    alice ?#> ("@" <> bobIncognito <> " psst, I'm incognito")
    bob ?<# (aliceIncognito <> "> psst, I'm incognito")
    bob ?#> ("@" <> aliceIncognito <> " <whispering> me too")
    alice ?<# (bobIncognito <> "> <whispering> me too")
    -- new contact is connected non incognito
    connectUsers alice cath
    alice <##> cath
    -- bob is not notified on profile change
    alice ##> "/p alice"
    concurrentlyN_
      [ alice <## "user full name removed (your 1 contacts are notified)",
        cath <## "contact alice removed full name"
      ]
    alice ?#> ("@" <> bobIncognito <> " do you see that I've changed profile?")
    bob ?<# (aliceIncognito <> "> do you see that I've changed profile?")
    bob ?#> ("@" <> aliceIncognito <> " no")
    alice ?<# (bobIncognito <> "> no")
    alice ##> "/_set prefs @2 {}"
    alice <## ("your preferences for " <> bobIncognito <> " did not change")
    (bob </)
    alice ##> "/_set prefs @2 {\"fullDelete\": {\"allow\": \"always\"}}"
    alice <## ("you updated preferences for " <> bobIncognito <> ":")
    alice <## "Full deletion: enabled for contact (you allow: always, contact allows: no)"
    bob <## (aliceIncognito <> " updated preferences for you:")
    bob <## "Full deletion: enabled for you (you allow: no, contact allows: always)"
    bob ##> "/_set prefs @2 {}"
    bob <## ("your preferences for " <> aliceIncognito <> " did not change")
    (alice </)
    alice ##> "/_set prefs @2 {\"fullDelete\": {\"allow\": \"no\"}}"
    alice <## ("you updated preferences for " <> bobIncognito <> ":")
    alice <## "Full deletion: off (you allow: no, contact allows: no)"
    bob <## (aliceIncognito <> " updated preferences for you:")
    bob <## "Full deletion: off (you allow: no, contact allows: no)"
    -- list contacts
    alice ##> "/contacts"
    alice
      <### [ ConsoleString $ "i " <> bobIncognito,
             "cath (Catherine)"
           ]
    alice `hasContactProfiles` ["alice", T.pack aliceIncognito, T.pack bobIncognito, "cath"]
    bob ##> "/contacts"
    bob <## ("i " <> aliceIncognito)
    bob `hasContactProfiles` ["bob", T.pack aliceIncognito, T.pack bobIncognito]
    -- alice deletes contact, incognito profile is deleted
    alice ##> ("/d " <> bobIncognito)
    alice <## (bobIncognito <> ": contact is deleted")
    bob <## (aliceIncognito <> " deleted contact with you")
    alice ##> "/contacts"
    alice <## "cath (Catherine)"
    alice `hasContactProfiles` ["alice", "cath"]
    -- bob deletes contact, incognito profile is deleted
    bob ##> ("/d " <> aliceIncognito)
    bob <## (aliceIncognito <> ": contact is deleted")
    bob ##> "/contacts"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testConnectIncognitoContactAddress :: HasCallStack => FilePath -> IO ()
testConnectIncognitoContactAddress = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c i " <> cLink)
    bobIncognito <- getTermLine bob
    bob <## "connection request sent incognito!"
    alice <## (bobIncognito <> " wants to connect to you!")
    alice <## ("to accept: /ac " <> bobIncognito)
    alice <## ("to reject: /rc " <> bobIncognito <> " (the sender will NOT be notified)")
    alice ##> ("/ac " <> bobIncognito)
    alice <## (bobIncognito <> ": accepting contact request...")
    _ <- getTermLine bob
    concurrentlyN_
      [ do
          bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
          bob <## "use /i alice to print out this incognito profile again",
        alice <## (bobIncognito <> ": contact is connected")
      ]
    -- conversation is incognito
    alice #> ("@" <> bobIncognito <> " who are you?")
    bob ?<# "alice> who are you?"
    bob ?#> "@alice I'm Batman"
    alice <# (bobIncognito <> "> I'm Batman")
    -- list contacts
    bob ##> "/contacts"
    bob <## "i alice (Alice)"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    threadDelay 500000
    -- delete contact, incognito profile is deleted
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    alice <## (bobIncognito <> " deleted contact with you")
    bob ##> "/contacts"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testAcceptContactRequestIncognito :: HasCallStack => FilePath -> IO ()
testAcceptContactRequestIncognito = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice ##> "/accept incognito bob"
    alice <## "bob (Bob): accepting contact request..."
    aliceIncognitoBob <- getTermLine alice
    concurrentlyN_
      [ bob <## (aliceIncognitoBob <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognitoBob)
          alice <## "use /i bob to print out this incognito profile again"
      ]
    -- conversation is incognito
    alice ?#> "@bob my profile is totally inconspicuous"
    bob <# (aliceIncognitoBob <> "> my profile is totally inconspicuous")
    bob #> ("@" <> aliceIncognitoBob <> " I know!")
    alice ?<# "bob> I know!"
    -- list contacts
    alice ##> "/contacts"
    alice <## "i bob (Bob)"
    alice `hasContactProfiles` ["alice", "bob", T.pack aliceIncognitoBob]
    -- delete contact, incognito profile is deleted
    alice ##> "/d bob"
    alice <## "bob: contact is deleted"
    bob <## (aliceIncognitoBob <> " deleted contact with you")
    alice ##> "/contacts"
    (alice </)
    alice `hasContactProfiles` ["alice"]
    -- /_accept api
    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice ##> "/_accept incognito=on 1"
    alice <## "cath (Catherine): accepting contact request..."
    aliceIncognitoCath <- getTermLine alice
    concurrentlyN_
      [ cath <## (aliceIncognitoCath <> ": contact is connected"),
        do
          alice <## ("cath (Catherine): contact is connected, your incognito profile for this contact is " <> aliceIncognitoCath)
          alice <## "use /i cath to print out this incognito profile again"
      ]
    alice `hasContactProfiles` ["alice", "cath", T.pack aliceIncognitoCath]
    cath `hasContactProfiles` ["cath", T.pack aliceIncognitoCath]

testSetConnectionIncognito :: HasCallStack => FilePath -> IO ()
testSetConnectionIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/connect"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 on"
    alice <## "connection 1 changed to incognito"
    bob ##> ("/connect " <> inv)
    bob <## "confirmation sent!"
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## ("use /i bob to print out this incognito profile again")
      ]
    alice ?#> ("@bob hi")
    bob <# (aliceIncognito <> "> hi")
    bob #> ("@" <> aliceIncognito <> " hey")
    alice ?<# ("bob> hey")
    alice `hasContactProfiles` ["alice", "bob", T.pack aliceIncognito]
    bob `hasContactProfiles` ["bob", T.pack aliceIncognito]

testResetConnectionIncognito :: HasCallStack => FilePath -> IO ()
testResetConnectionIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/_connect 1 incognito=on"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 off"
    alice <## "connection 1 changed to non incognito"
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")
    alice <##> bob
    alice `hasContactProfiles` ["alice", "bob"]
    bob `hasContactProfiles` ["alice", "bob"]

testSetConnectionIncognitoProhibitedDuringNegotiation :: HasCallStack => FilePath -> IO ()
testSetConnectionIncognitoProhibitedDuringNegotiation tmp = do
  inv <- withNewTestChat tmp "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    alice ##> "/connect"
    getInvitation alice
  withNewTestChat tmp "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
  withTestChat tmp "alice" $ \alice -> do
    threadDelay 250000
    alice ##> "/_set incognito :1 on"
    alice <## "chat db error: SEPendingConnectionNotFound {connId = 1}"
    withTestChat tmp "bob" $ \bob -> do
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      alice `hasContactProfiles` ["alice", "bob"]
      bob `hasContactProfiles` ["alice", "bob"]

testConnectionIncognitoUnchangedErrors :: HasCallStack => FilePath -> IO ()
testConnectionIncognitoUnchangedErrors = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/connect"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 off"
    alice <## "incognito mode change prohibited"
    alice ##> "/_set incognito :1 on"
    alice <## "connection 1 changed to incognito"
    alice ##> "/_set incognito :1 on"
    alice <## "incognito mode change prohibited"
    alice ##> "/_set incognito :1 off"
    alice <## "connection 1 changed to non incognito"
    alice ##> "/_set incognito :1 off"
    alice <## "incognito mode change prohibited"
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")
    alice <##> bob
    alice `hasContactProfiles` ["alice", "bob"]
    bob `hasContactProfiles` ["alice", "bob"]

testSetResetSetConnectionIncognito :: HasCallStack => FilePath -> IO ()
testSetResetSetConnectionIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/_connect 1 incognito=off"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 on"
    alice <## "connection 1 changed to incognito"
    alice ##> "/_set incognito :1 off"
    alice <## "connection 1 changed to non incognito"
    alice ##> "/_set incognito :1 on"
    alice <## "connection 1 changed to incognito"
    bob ##> ("/_connect 1 incognito=off " <> inv)
    bob <## "confirmation sent!"
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## ("use /i bob to print out this incognito profile again")
      ]
    alice ?#> ("@bob hi")
    bob <# (aliceIncognito <> "> hi")
    bob #> ("@" <> aliceIncognito <> " hey")
    alice ?<# ("bob> hey")
    alice `hasContactProfiles` ["alice", "bob", T.pack aliceIncognito]
    bob `hasContactProfiles` ["bob", T.pack aliceIncognito]

testJoinGroupIncognito :: HasCallStack => FilePath -> IO ()
testJoinGroupIncognito =
  testChatCfg4 testCfgCreateGroupDirect aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      -- non incognito connections
      connectUsers alice bob
      connectUsers alice dan
      connectUsers bob cath
      connectUsers bob dan
      connectUsers cath dan
      -- cath connected incognito to alice
      alice ##> "/c"
      inv <- getInvitation alice
      cath ##> ("/c i " <> inv)
      cath <## "confirmation sent!"
      cathIncognito <- getTermLine cath
      concurrentlyN_
        [ do
            cath <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> cathIncognito)
            cath <## "use /i alice to print out this incognito profile again",
          alice <## (cathIncognito <> ": contact is connected")
        ]
      -- alice creates group
      alice ##> "/g secret_club"
      alice <## "group #secret_club is created"
      alice <## "to add members use /a secret_club <name> or /create link #secret_club"
      -- alice invites bob
      alice ##> "/a secret_club bob admin"
      concurrentlyN_
        [ alice <## "invitation to join the group #secret_club sent to bob",
          do
            bob <## "#secret_club: alice invites you to join the group as admin"
            bob <## "use /j secret_club to accept"
        ]
      bob ##> "/j secret_club"
      concurrently_
        (alice <## "#secret_club: bob joined the group")
        (bob <## "#secret_club: you joined the group")
      -- alice invites cath
      alice ##> ("/a secret_club " <> cathIncognito <> " admin")
      concurrentlyN_
        [ alice <## ("invitation to join the group #secret_club sent to " <> cathIncognito),
          do
            cath <## "#secret_club: alice invites you to join the group as admin"
            cath <## ("use /j secret_club to join incognito as " <> cathIncognito)
        ]
      -- cath uses the same incognito profile when joining group, cath and bob don't merge contacts
      cath ##> "/j secret_club"
      concurrentlyN_
        [ alice <## ("#secret_club: " <> cathIncognito <> " joined the group"),
          do
            cath <## ("#secret_club: you joined the group incognito as " <> cathIncognito)
            cath <## "#secret_club: member bob_1 (Bob) is connected",
          do
            bob <## ("#secret_club: alice added " <> cathIncognito <> " to the group (connecting...)")
            bob <## ("#secret_club: new member " <> cathIncognito <> " is connected")
        ]
      -- cath cannot invite to the group because her membership is incognito
      cath ##> "/a secret_club dan"
      cath <## "you are using an incognito profile for this group - prohibited to invite contacts"
      -- alice invites dan
      alice ##> "/a secret_club dan admin"
      concurrentlyN_
        [ alice <## "invitation to join the group #secret_club sent to dan",
          do
            dan <## "#secret_club: alice invites you to join the group as admin"
            dan <## "use /j secret_club to accept"
        ]
      dan ##> "/j secret_club"
      -- cath and dan don't merge contacts
      concurrentlyN_
        [ alice <## "#secret_club: dan joined the group",
          do
            dan <## "#secret_club: you joined the group"
            dan
              <### [ ConsoleString $ "#secret_club: member " <> cathIncognito <> " is connected",
                     "#secret_club: member bob_1 (Bob) is connected",
                     "contact bob_1 is merged into bob",
                     "use @bob <message> to send messages"
                   ],
          do
            bob <## "#secret_club: alice added dan_1 (Daniel) to the group (connecting...)"
            bob <## "#secret_club: new member dan_1 is connected"
            bob <## "contact dan_1 is merged into dan"
            bob <## "use @dan <message> to send messages",
          do
            cath <## "#secret_club: alice added dan_1 (Daniel) to the group (connecting...)"
            cath <## "#secret_club: new member dan_1 is connected"
        ]
      -- send messages - group is incognito for cath
      alice #> "#secret_club hello"
      concurrentlyN_
        [ bob <# "#secret_club alice> hello",
          cath ?<# "#secret_club alice> hello",
          dan <# "#secret_club alice> hello"
        ]
      bob #> "#secret_club hi there"
      concurrentlyN_
        [ alice <# "#secret_club bob> hi there",
          cath ?<# "#secret_club bob_1> hi there",
          dan <# "#secret_club bob> hi there"
        ]
      cath ?#> "#secret_club hey"
      concurrentlyN_
        [ alice <# ("#secret_club " <> cathIncognito <> "> hey"),
          bob <# ("#secret_club " <> cathIncognito <> "> hey"),
          dan <# ("#secret_club " <> cathIncognito <> "> hey")
        ]
      dan #> "#secret_club how is it going?"
      concurrentlyN_
        [ alice <# "#secret_club dan> how is it going?",
          bob <# "#secret_club dan> how is it going?",
          cath ?<# "#secret_club dan_1> how is it going?"
        ]
      -- cath and bob can send messages via new direct connection, cath is incognito
      bob #> ("@" <> cathIncognito <> " hi, I'm bob")
      cath ?<# "bob_1> hi, I'm bob"
      cath ?#> "@bob_1 hey, I'm incognito"
      bob <# (cathIncognito <> "> hey, I'm incognito")
      -- cath and dan can send messages via new direct connection, cath is incognito
      dan #> ("@" <> cathIncognito <> " hi, I'm dan")
      cath ?<# "dan_1> hi, I'm dan"
      cath ?#> "@dan_1 hey, I'm incognito"
      dan <# (cathIncognito <> "> hey, I'm incognito")
      -- non incognito connections are separate
      bob <##> cath
      dan <##> cath
      -- list groups
      cath ##> "/gs"
      cath <## "i #secret_club (4 members)"
      -- list group members
      alice ##> "/ms secret_club"
      alice
        <### [ "alice (Alice): owner, you, created group",
               "bob (Bob): admin, invited, connected",
               ConsoleString $ cathIncognito <> ": admin, invited, connected",
               "dan (Daniel): admin, invited, connected"
             ]
      bob ##> "/ms secret_club"
      bob
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): admin, you, connected",
               ConsoleString $ cathIncognito <> ": admin, connected",
               "dan (Daniel): admin, connected"
             ]
      cath ##> "/ms secret_club"
      cath
        <### [ "alice (Alice): owner, host, connected",
               "bob_1 (Bob): admin, connected",
               ConsoleString $ "i " <> cathIncognito <> ": admin, you, connected",
               "dan_1 (Daniel): admin, connected"
             ]
      dan ##> "/ms secret_club"
      dan
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): admin, connected",
               ConsoleString $ cathIncognito <> ": admin, connected",
               "dan (Daniel): admin, you, connected"
             ]
      -- remove member
      bob ##> ("/rm secret_club " <> cathIncognito)
      concurrentlyN_
        [ bob <## ("#secret_club: you removed " <> cathIncognito <> " from the group"),
          alice <## ("#secret_club: bob removed " <> cathIncognito <> " from the group"),
          dan <## ("#secret_club: bob removed " <> cathIncognito <> " from the group"),
          do
            cath <## "#secret_club: bob_1 removed you from the group"
            cath <## "use /d #secret_club to delete the group"
        ]
      bob #> "#secret_club hi"
      concurrentlyN_
        [ alice <# "#secret_club bob> hi",
          dan <# "#secret_club bob> hi",
          (cath </)
        ]
      alice #> "#secret_club hello"
      concurrentlyN_
        [ bob <# "#secret_club alice> hello",
          dan <# "#secret_club alice> hello",
          (cath </)
        ]
      cath ##> "#secret_club hello"
      cath <## "you are no longer a member of the group"
      -- cath can still message members directly
      bob #> ("@" <> cathIncognito <> " I removed you from group")
      cath ?<# "bob_1> I removed you from group"
      cath ?#> "@bob_1 ok"
      bob <# (cathIncognito <> "> ok")

testCantInviteContactIncognito :: HasCallStack => FilePath -> IO ()
testCantInviteContactIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- alice connected incognito to bob
    alice ##> "/c i"
    inv <- getInvitation alice
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## "use /i bob to print out this incognito profile again"
      ]
    -- alice creates group non incognito
    alice ##> "/g club"
    alice <## "group #club is created"
    alice <## "to add members use /a club <name> or /create link #club"
    alice ##> "/a club bob"
    alice <## "you're using your main profile for this group - prohibited to invite contacts to whom you are connected incognito"
    -- bob doesn't receive invitation
    (bob </)

testCantSeeGlobalPrefsUpdateIncognito :: HasCallStack => FilePath -> IO ()
testCantSeeGlobalPrefsUpdateIncognito = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/c i"
    invIncognito <- getInvitation alice
    alice ##> "/c"
    inv <- getInvitation alice
    bob ##> ("/c " <> invIncognito)
    bob <## "confirmation sent!"
    aliceIncognito <- getTermLine alice
    cath ##> ("/c " <> inv)
    cath <## "confirmation sent!"
    concurrentlyN_
      [ bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## "use /i bob to print out this incognito profile again",
        do
          cath <## "alice (Alice): contact is connected"
      ]
    alice <## "cath (Catherine): contact is connected"
    alice ##> "/_profile 1 {\"displayName\": \"alice\", \"fullName\": \"\", \"preferences\": {\"fullDelete\": {\"allow\": \"always\"}, \"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
    alice <## "user full name removed (your 1 contacts are notified)"
    alice <## "updated preferences:"
    alice <## "Full deletion allowed: always"
    (alice </)
    -- bob doesn't receive profile update
    (bob </)
    cath <## "contact alice removed full name"
    cath <## "alice updated preferences for you:"
    cath <## "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
    (cath </)
    bob ##> "/_set prefs @2 {\"fullDelete\": {\"allow\": \"always\"}}"
    bob <## ("you updated preferences for " <> aliceIncognito <> ":")
    bob <## "Full deletion: enabled for contact (you allow: always, contact allows: no)"
    alice <## "bob updated preferences for you:"
    alice <## "Full deletion: enabled for you (you allow: no, contact allows: always)"
    alice ##> "/_set prefs @2 {\"fullDelete\": {\"allow\": \"yes\"}}"
    alice <## "you updated preferences for bob:"
    alice <## "Full deletion: enabled (you allow: yes, contact allows: always)"
    bob <## (aliceIncognito <> " updated preferences for you:")
    bob <## "Full deletion: enabled (you allow: always, contact allows: yes)"
    (cath </)
    alice ##> "/_set prefs @3 {\"fullDelete\": {\"allow\": \"always\"}}"
    alice <## "your preferences for cath did not change"
    alice ##> "/_set prefs @3 {\"fullDelete\": {\"allow\": \"yes\"}}"
    alice <## "you updated preferences for cath:"
    alice <## "Full deletion: off (you allow: yes, contact allows: no)"
    cath <## "alice updated preferences for you:"
    cath <## "Full deletion: off (you allow: default (no), contact allows: yes)"

testDeleteContactThenGroupDeletesIncognitoProfile :: HasCallStack => FilePath -> IO ()
testDeleteContactThenGroupDeletesIncognitoProfile = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- bob connects incognito to alice
    alice ##> "/c"
    inv <- getInvitation alice
    bob ##> ("/c i " <> inv)
    bob <## "confirmation sent!"
    bobIncognito <- getTermLine bob
    concurrentlyN_
      [ alice <## (bobIncognito <> ": contact is connected"),
        do
          bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
          bob <## "use /i alice to print out this incognito profile again"
      ]
    -- bob joins group using incognito profile
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> ("/a team " <> bobIncognito)
    concurrentlyN_
      [ alice <## ("invitation to join the group #team sent to " <> bobIncognito),
        do
          bob <## "#team: alice invites you to join the group as member"
          bob <## ("use /j team to join incognito as " <> bobIncognito)
      ]
    bob ##> "/j team"
    concurrently_
      (alice <## ("#team: " <> bobIncognito <> " joined the group"))
      (bob <## ("#team: you joined the group incognito as " <> bobIncognito))
    bob ##> "/contacts"
    bob <## "i alice (Alice)"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete contact
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    alice <## (bobIncognito <> " deleted contact with you")
    bob ##> "/contacts"
    (bob </)
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete group
    bob ##> "/l team"
    concurrentlyN_
      [ do
          bob <## "#team: you left the group"
          bob <## "use /d #team to delete the group",
        alice <## ("#team: " <> bobIncognito <> " left the group")
      ]
    bob ##> "/d #team"
    bob <## "#team: you deleted the group"
    bob `hasContactProfiles` ["bob"]

testDeleteGroupThenContactDeletesIncognitoProfile :: HasCallStack => FilePath -> IO ()
testDeleteGroupThenContactDeletesIncognitoProfile = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- bob connects incognito to alice
    alice ##> "/c"
    inv <- getInvitation alice
    bob ##> ("/c i " <> inv)
    bob <## "confirmation sent!"
    bobIncognito <- getTermLine bob
    concurrentlyN_
      [ alice <## (bobIncognito <> ": contact is connected"),
        do
          bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
          bob <## "use /i alice to print out this incognito profile again"
      ]
    -- bob joins group using incognito profile
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> ("/a team " <> bobIncognito)
    concurrentlyN_
      [ alice <## ("invitation to join the group #team sent to " <> bobIncognito),
        do
          bob <## "#team: alice invites you to join the group as member"
          bob <## ("use /j team to join incognito as " <> bobIncognito)
      ]
    bob ##> "/j team"
    concurrently_
      (alice <## ("#team: " <> bobIncognito <> " joined the group"))
      (bob <## ("#team: you joined the group incognito as " <> bobIncognito))
    bob ##> "/contacts"
    bob <## "i alice (Alice)"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete group
    bob ##> "/l team"
    concurrentlyN_
      [ do
          bob <## "#team: you left the group"
          bob <## "use /d #team to delete the group",
        alice <## ("#team: " <> bobIncognito <> " left the group")
      ]
    bob ##> "/d #team"
    bob <## "#team: you deleted the group"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete contact
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    alice <## (bobIncognito <> " deleted contact with you")
    bob ##> "/contacts"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testSetAlias :: HasCallStack => FilePath -> IO ()
testSetAlias = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    connectUsers alice bob
    alice #$> ("/_set alias @2 my friend bob", id, "contact bob alias updated: my friend bob")
    alice ##> "/contacts"
    alice <## "bob (Bob) (alias: my friend bob)"
    alice #$> ("/_set alias @2", id, "contact bob alias removed")
    alice ##> "/contacts"
    alice <## "bob (Bob)"

testSetConnectionAlias :: HasCallStack => FilePath -> IO ()
testSetConnectionAlias = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/c"
    inv <- getInvitation alice
    alice @@@ [(":1", "")]
    alice ##> "/_set alias :1 friend"
    alice <## "connection 1 alias updated: friend"
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (alice <## "bob (Bob): contact is connected")
      (bob <## "alice (Alice): contact is connected")
    threadDelay 100000
    alice @@@ [("@bob", lastChatFeature)]
    alice ##> "/contacts"
    alice <## "bob (Bob) (alias: friend)"

testSetContactPrefs :: HasCallStack => FilePath -> IO ()
testSetContactPrefs = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice #$> ("/_files_folder ./tests/tmp/alice", id, "ok")
    bob #$> ("/_files_folder ./tests/tmp/bob", id, "ok")
    createDirectoryIfMissing True "./tests/tmp/alice"
    createDirectoryIfMissing True "./tests/tmp/bob"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/alice/test.txt"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/bob/test.txt"
    bob ##> "/_profile 1 {\"displayName\": \"bob\", \"fullName\": \"Bob\", \"preferences\": {\"voice\": {\"allow\": \"no\"}, \"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
    bob <## "profile image removed"
    bob <## "updated preferences:"
    bob <## "Voice messages allowed: no"
    (bob </)
    connectUsers alice bob
    alice ##> "/_set prefs @2 {}"
    alice <## "your preferences for bob did not change"
    (bob </)
    let startFeatures = [(0, "Disappearing messages: allowed"), (0, "Full deletion: off"), (0, "Message reactions: enabled"), (0, "Voice messages: off"), (0, "Audio/video calls: enabled")]
    alice #$> ("/_get chat @2 count=100", chat, startFeatures)
    bob #$> ("/_get chat @2 count=100", chat, startFeatures)
    let sendVoice = "/_send @2 json {\"filePath\": \"test.txt\", \"msgContent\": {\"type\": \"voice\", \"text\": \"\", \"duration\": 10}}"
        voiceNotAllowed = "bad chat command: feature not allowed Voice messages"
    alice ##> sendVoice
    alice <## voiceNotAllowed
    bob ##> sendVoice
    bob <## voiceNotAllowed
    -- alice ##> "/_set prefs @2 {\"voice\": {\"allow\": \"always\"}}"
    alice ##> "/set voice @bob always"
    alice <## "you updated preferences for bob:"
    alice <## "Voice messages: enabled for contact (you allow: always, contact allows: no)"
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact")])
    bob <## "alice updated preferences for you:"
    bob <## "Voice messages: enabled for you (you allow: default (no), contact allows: always)"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you")])
    alice ##> sendVoice
    alice <## voiceNotAllowed
    bob ##> sendVoice
    bob <# "@alice voice message (00:10)"
    bob <# "/f @alice test.txt"
    bob <## "completed sending file 1 (test.txt) to alice"
    alice <# "bob> voice message (00:10)"
    alice <# "bob> sends file test.txt (11 bytes / 11 bytes)"
    alice <## "started receiving file 1 (test.txt) from bob"
    alice <## "completed receiving file 1 (test.txt) from bob"
    (bob </)
    -- alice ##> "/_profile 1 {\"displayName\": \"alice\", \"fullName\": \"Alice\", \"preferences\": {\"voice\": {\"allow\": \"no\"}}}"
    alice ##> "/set voice no"
    alice <## "updated preferences:"
    alice <## "Voice messages allowed: no"
    (alice </)
    alice ##> "/_set prefs @2 {\"voice\": {\"allow\": \"yes\"}}"
    alice <## "you updated preferences for bob:"
    alice <## "Voice messages: off (you allow: yes, contact allows: no)"
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact"), (0, "voice message (00:10)"), (1, "Voice messages: off")])
    bob <## "alice updated preferences for you:"
    bob <## "Voice messages: off (you allow: default (no), contact allows: yes)"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off")])
    (bob </)
    bob ##> "/_profile 1 {\"displayName\": \"bob\", \"fullName\": \"\", \"preferences\": {\"voice\": {\"allow\": \"yes\"}, \"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
    bob <## "user full name removed (your 1 contacts are notified)"
    bob <## "updated preferences:"
    bob <## "Voice messages allowed: yes"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off"), (1, "Voice messages: enabled")])
    (bob </)
    alice <## "contact bob removed full name"
    alice <## "bob updated preferences for you:"
    alice <## "Voice messages: enabled (you allow: yes, contact allows: yes)"
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact"), (0, "voice message (00:10)"), (1, "Voice messages: off"), (0, "Voice messages: enabled")])
    (alice </)
    bob ##> "/_set prefs @2 {}"
    bob <## "your preferences for alice did not change"
    -- no change
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off"), (1, "Voice messages: enabled")])
    (bob </)
    (alice </)
    alice ##> "/_set prefs @2 {\"voice\": {\"allow\": \"no\"}}"
    alice <## "you updated preferences for bob:"
    alice <## "Voice messages: off (you allow: no, contact allows: yes)"
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact"), (0, "voice message (00:10)"), (1, "Voice messages: off"), (0, "Voice messages: enabled"), (1, "Voice messages: off")])
    bob <## "alice updated preferences for you:"
    bob <## "Voice messages: off (you allow: default (yes), contact allows: no)"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off"), (1, "Voice messages: enabled"), (0, "Voice messages: off")])

testFeatureOffers :: HasCallStack => FilePath -> IO ()
testFeatureOffers = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    connectUsers alice bob
    alice ##> "/set delete @bob yes"
    alice <## "you updated preferences for bob:"
    alice <## "Full deletion: off (you allow: yes, contact allows: no)"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "you offered Full deletion")])
    bob <## "alice updated preferences for you:"
    bob <## "Full deletion: off (you allow: default (no), contact allows: yes)"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "offered Full deletion")])
    alice ##> "/set delete @bob no"
    alice <## "you updated preferences for bob:"
    alice <## "Full deletion: off (you allow: no, contact allows: no)"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "you offered Full deletion"), (1, "you cancelled Full deletion")])
    bob <## "alice updated preferences for you:"
    bob <## "Full deletion: off (you allow: default (no), contact allows: no)"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "offered Full deletion"), (0, "cancelled Full deletion")])

testUpdateGroupPrefs :: HasCallStack => FilePath -> IO ()
testUpdateGroupPrefs =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected")])
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected")])
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: on"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on")])
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"off\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion: off"
      alice <## "Voice messages: off"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: off"
      bob <## "Voice messages: off"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off")])
      alice ##> "/set voice #team on"
      alice <## "updated group preferences:"
      alice <## "Voice messages: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Voice messages: on"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on")])
      threadDelay 500000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      -- no update
      threadDelay 500000
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      alice #> "#team hey"
      bob <# "#team alice> hey"
      threadDelay 1000000
      bob #> "#team hi"
      alice <# "#team bob> hi"
      threadDelay 500000
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on"), (1, "hey"), (0, "hi")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on"), (0, "hey"), (1, "hi")])

testAllowFullDeletionContact :: HasCallStack => FilePath -> IO ()
testAllowFullDeletionContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob
      alice ##> "/set delete @bob always"
      alice <## "you updated preferences for bob:"
      alice <## "Full deletion: enabled for contact (you allow: always, contact allows: no)"
      bob <## "alice updated preferences for you:"
      bob <## "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (0, "hey"), (1, "Full deletion: enabled for contact")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (1, "hey"), (0, "Full deletion: enabled for you")])
      bob #$> ("/_delete item @2 " <> itemId 2 <> " broadcast", id, "message deleted")
      alice <# "bob> [deleted] hey"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (1, "Full deletion: enabled for contact")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (0, "Full deletion: enabled for you")])

testAllowFullDeletionGroup :: HasCallStack => FilePath -> IO ()
testAllowFullDeletionGroup =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      threadDelay 1000000
      alice #> "#team hi"
      bob <# "#team alice> hi"
      threadDelay 1000000
      bob #> "#team hey"
      bob ##> "/last_item_id #team"
      msgItemId <- getTermLine bob
      alice <# "#team bob> hey"
      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "hi"), (0, "hey"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (1, "hey"), (0, "Full deletion: on")])
      bob #$> ("/_delete item #1 " <> msgItemId <> " broadcast", id, "message deleted")
      alice <# "#team bob> [deleted] hey"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "hi"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (0, "Full deletion: on")])

testProhibitDirectMessages :: HasCallStack => FilePath -> IO ()
testProhibitDirectMessages =
  testChatCfg4 testCfgCreateGroupDirect aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice ##> "/set direct #team off"
      alice <## "updated group preferences:"
      alice <## "Direct messages: off"
      directProhibited bob
      directProhibited cath
      threadDelay 1000000
      -- still can send direct messages to direct contacts
      alice #> "@bob hello again"
      bob <# "alice> hello again"
      alice #> "@cath hello again"
      cath <# "alice> hello again"
      bob ##> "@cath hello again"
      bob <## "direct messages to indirect contact cath are prohibited"
      (cath </)
      connectUsers cath dan
      addMember "team" cath dan GRMember
      dan ##> "/j #team"
      concurrentlyN_
        [ cath <## "#team: dan joined the group",
          do
            dan <## "#team: you joined the group"
            dan
              <### [ "#team: member alice (Alice) is connected",
                     "#team: member bob (Bob) is connected"
                   ],
          do
            alice <## "#team: cath added dan (Daniel) to the group (connecting...)"
            alice <## "#team: new member dan is connected",
          do
            bob <## "#team: cath added dan (Daniel) to the group (connecting...)"
            bob <## "#team: new member dan is connected"
        ]
      alice ##> "@dan hi"
      alice <## "direct messages to indirect contact dan are prohibited"
      bob ##> "@dan hi"
      bob <## "direct messages to indirect contact dan are prohibited"
      (dan </)
      dan ##> "@alice hi"
      dan <## "direct messages to indirect contact alice are prohibited"
      dan ##> "@bob hi"
      dan <## "direct messages to indirect contact bob are prohibited"
      dan #> "@cath hi"
      cath <# "dan> hi"
      cath #> "@dan hi"
      dan <# "cath> hi"
  where
    directProhibited :: HasCallStack => TestCC -> IO ()
    directProhibited cc = do
      cc <## "alice updated group #team:"
      cc <## "updated group preferences:"
      cc <## "Direct messages: off"

testEnableTimedMessagesContact :: HasCallStack => FilePath -> IO ()
testEnableTimedMessagesContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_set prefs @2 {\"timedMessages\": {\"allow\": \"yes\", \"ttl\": 1}}"
      alice <## "you updated preferences for bob:"
      alice <## "Disappearing messages: enabled (you allow: yes (1 sec), contact allows: yes)"
      bob <## "alice updated preferences for you:"
      bob <## "Disappearing messages: enabled (you allow: yes (1 sec), contact allows: yes (1 sec))"
      bob ##> "/set disappear @alice yes"
      bob <## "your preferences for alice did not change"
      alice <##> bob
      threadDelay 500000
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "Disappearing messages: enabled (1 sec)"), (1, "hi"), (0, "hey")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "Disappearing messages: enabled (1 sec)"), (0, "hi"), (1, "hey")])
      threadDelay 1000000
      alice <## "timed message deleted: hi"
      alice <## "timed message deleted: hey"
      bob <## "timed message deleted: hi"
      bob <## "timed message deleted: hey"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "Disappearing messages: enabled (1 sec)")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "Disappearing messages: enabled (1 sec)")])
      -- turn off, messages are not disappearing
      bob ##> "/set disappear @alice no"
      bob <## "you updated preferences for alice:"
      bob <## "Disappearing messages: off (you allow: no, contact allows: yes (1 sec))"
      alice <## "bob updated preferences for you:"
      alice <## "Disappearing messages: off (you allow: yes (1 sec), contact allows: no)"
      alice <##> bob
      threadDelay 1500000
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "Disappearing messages: enabled (1 sec)"), (0, "Disappearing messages: off"), (1, "hi"), (0, "hey")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "Disappearing messages: enabled (1 sec)"), (1, "Disappearing messages: off"), (0, "hi"), (1, "hey")])
      -- test api
      bob ##> "/set disappear @alice yes 30s"
      bob <## "you updated preferences for alice:"
      bob <## "Disappearing messages: enabled (you allow: yes (30 sec), contact allows: yes (1 sec))"
      alice <## "bob updated preferences for you:"
      alice <## "Disappearing messages: enabled (you allow: yes (30 sec), contact allows: yes (30 sec))"
      bob ##> "/set disappear @alice week" -- "yes" is optional
      bob <## "you updated preferences for alice:"
      bob <## "Disappearing messages: enabled (you allow: yes (1 week), contact allows: yes (1 sec))"
      alice <## "bob updated preferences for you:"
      alice <## "Disappearing messages: enabled (you allow: yes (1 week), contact allows: yes (1 week))"

testEnableTimedMessagesGroup :: HasCallStack => FilePath -> IO ()
testEnableTimedMessagesGroup =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      threadDelay 1000000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"timedMessages\": {\"enable\": \"on\", \"ttl\": 1}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: on (1 sec)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: on (1 sec)"
      threadDelay 1000000
      alice #> "#team hi"
      bob <# "#team alice> hi"
      threadDelay 500000
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Disappearing messages: on (1 sec)"), (1, "hi")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Disappearing messages: on (1 sec)"), (0, "hi")])
      threadDelay 1000000
      alice <## "timed message deleted: hi"
      bob <## "timed message deleted: hi"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Disappearing messages: on (1 sec)")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Disappearing messages: on (1 sec)")])
      -- turn off, messages are not disappearing
      alice ##> "/set disappear #team off"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: off"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: off"
      threadDelay 1000000
      alice #> "#team hey"
      bob <# "#team alice> hey"
      threadDelay 1500000
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Disappearing messages: on (1 sec)"), (1, "Disappearing messages: off"), (1, "hey")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Disappearing messages: on (1 sec)"), (0, "Disappearing messages: off"), (0, "hey")])
      -- test api
      alice ##> "/set disappear #team on 30s"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: on (30 sec)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: on (30 sec)"
      alice ##> "/set disappear #team week" -- "on" is optional
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: on (1 week)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: on (1 week)"

testTimedMessagesEnabledGlobally :: HasCallStack => FilePath -> IO ()
testTimedMessagesEnabledGlobally =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/set disappear yes"
      alice <## "user profile did not change"
      connectUsers alice bob
      bob ##> "/_set prefs @2 {\"timedMessages\": {\"allow\": \"yes\", \"ttl\": 1}}"
      bob <## "you updated preferences for alice:"
      bob <## "Disappearing messages: enabled (you allow: yes (1 sec), contact allows: yes)"
      alice <## "bob updated preferences for you:"
      alice <## "Disappearing messages: enabled (you allow: yes (1 sec), contact allows: yes (1 sec))"
      alice <##> bob
      threadDelay 500000
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "Disappearing messages: enabled (1 sec)"), (1, "hi"), (0, "hey")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "Disappearing messages: enabled (1 sec)"), (0, "hi"), (1, "hey")])
      threadDelay 1000000
      alice <## "timed message deleted: hi"
      bob <## "timed message deleted: hi"
      alice <## "timed message deleted: hey"
      bob <## "timed message deleted: hey"
      alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "Disappearing messages: enabled (1 sec)")])
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "Disappearing messages: enabled (1 sec)")])
