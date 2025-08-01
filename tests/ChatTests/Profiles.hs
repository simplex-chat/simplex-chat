{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatTests.Profiles where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad
import Control.Monad.Except
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Chat.Controller (ChatConfig (..), ChatHooks (..), defaultChatHooks)
import Simplex.Chat.Options
import Simplex.Chat.Protocol (currentChatVersion)
import Simplex.Chat.Store.Shared (createContact)
import Simplex.Chat.Types (ConnStatus (..), Profile (..), GroupRejectionReason (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Server.Env.STM hiding (subscriptions)
import Simplex.Messaging.Transport
import Simplex.Messaging.Util (encodeJSON)
import System.Directory (copyFile, createDirectoryIfMissing)
import Test.Hspec hiding (it)

chatProfileTests :: SpecWith TestParams
chatProfileTests = do
  describe "user profiles" $ do
    it "update user profile and notify contacts" testUpdateProfile
    it "update user profile with image" testUpdateProfileImage
    it "use multiword profile names" testMultiWordProfileNames
  describe "user contact link" $ do
    it "create and connect via contact link" testUserContactLink
    it "retry connecting via contact link" testRetryConnectingViaContactLink
    it "add contact link to profile" testProfileLink
    it "auto accept contact requests" testUserContactLinkAutoAccept
    it "deduplicate contact requests" testDeduplicateContactRequests
    it "deduplicate contact requests with profile change" testDeduplicateContactRequestsProfileChange
    it "reject contact and delete contact link" testRejectContactAndDeleteUserContact
    it "keep connection requests when contact link deleted" testKeepConnectionRequests
    it "connected contact works when contact link deleted" testContactLinkDeletedConnectedContactWorks
    -- TODO [short links] test auto-reply with current version, with connecting client not preparing contact
    it "auto-reply message" testAutoReplyMessage
    it "auto-reply message in incognito" testAutoReplyMessageInIncognito
    describe "business address" $ do
      it "create and connect via business address" testBusinessAddress
      -- TODO [short links] test business auto-reply with current version, with connecting client not preparing contact
      it "update profiles with business address" testBusinessUpdateProfiles
  describe "contact address connection plan" $ do
    it "contact address ok to connect; known contact" testPlanAddressOkKnown
    it "own contact address" testPlanAddressOwn
    it "connecting via contact address" testPlanAddressConnecting
    it "connecting via contact address (slow handshake)" testPlanAddressConnectingSlow
    it "re-connect with deleted contact" testPlanAddressContactDeletedReconnected
    it "contact via address" testPlanAddressContactViaAddress
    it "contact via short address" testPlanAddressContactViaShortAddress
  describe "incognito" $ do
    it "connect incognito via invitation link" testConnectIncognitoInvitationLink
    it "connect incognito via contact address" testConnectIncognitoContactAddress
    it "accept contact request incognito" testAcceptContactRequestIncognito
    it "set connection incognito" testSetConnectionIncognito
    it "reset connection incognito" testResetConnectionIncognito
    it "set connection incognito prohibited during negotiation" testSetConnectionIncognitoProhibitedDuringNegotiation
    it "set connection incognito prohibited during negotiation (slow handshake)" testSetConnectionIncognitoProhibitedDuringNegotiationSlow
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
  describe "group aliases" $ do
    it "set group alias" testSetGroupAlias
  describe "pending connection users" $ do
    it "change user for pending connection" testChangePCCUser
    it "change from incognito profile connects as new user" testChangePCCUserFromIncognito
    it "change user for pending connection and later set incognito connects as incognito in changed profile" testChangePCCUserAndThenIncognito
    it "change user for user without matching servers creates new connection" testChangePCCUserDiffSrv
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
    it "update multiple user preferences for multiple contacts" testUpdateMultipleUserPrefs
    describe "group preferences for specific member role" $ do
      it "direct messages" testGroupPrefsDirectForRole
      it "files & media" testGroupPrefsFilesForRole
      it "SimpleX links" testGroupPrefsSimplexLinksForRole
    it "set user, contact and group UI theme" testSetUITheme
  describe "short links" $ do
    it "should connect via one-time invitation" testShortLinkInvitation
    it "should plan and connect via one-time invitation" testPlanShortLinkInvitation
    it "should connect via contact address" testShortLinkContactAddress
    it "should join group" testShortLinkJoinGroup
  describe "short links with attached data" shortLinkTests

shortLinkTests :: SpecWith TestParams
shortLinkTests = do
  it "prepare contact using invitation short link data and connect" testShortLinkInvitationPrepareContact
  it "prepare contact with image in profile" testShortLinkInvitationImage
  it "prepare contact via invitation and retry connecting" testShortLinkInvitationConnectRetry
  it "prepare contact using address short link data and connect" testShortLinkAddressPrepareContact
  it "address connect plan after contact is deleted but conversation kept" testShortLinkAddressDeleteContact
  it "prepare contact via invitation and connect after it is deleted" testShortLinkDeletedInvitation
  it "prepare contact via address and connect after it is deleted" testShortLinkDeletedAddress
  it "prepare contact via address and connect with retry after error" testShortLinkAddressConnectRetry
  it "prepare contact via address and connect incognito with retry" testShortLinkAddressConnectRetryIncognito
  it "prepare business chat using address short link data and connect" testShortLinkAddressPrepareBusiness
  it "connect to business address with request message" testBusinessAddressRequestMessage
  it "prepare group using group short link data and connect" testShortLinkPrepareGroup
  it "prepare group using group short link data and connect, host rejects" testShortLinkPrepareGroupReject
  it "connect to group with welcome message via short link" testGroupShortLinkWelcome
  it "retry connecting to group via short link" testShortLinkGroupRetry
  it "connect to prepared contact incognito (via invitation)" testShortLinkInvitationConnectPreparedContactIncognito
  it "connect to prepared contact incognito (via address)" testShortLinkAddressConnectPreparedContactIncognito
  it "change prepared contact user" testShortLinkChangePreparedContactUser
  it "change prepared contact user, new user has contact with the same name" testShortLinkChangePreparedContactUserDuplicate
  it "connect to prepared group incognito" testShortLinkConnectPreparedGroupIncognito
  it "change prepared group user" testShortLinkChangePreparedGroupUser
  it "change prepared group user, new user has group with the same name" testShortLinkChangePreparedGroupUserDuplicate
  it "setting incognito for invitation should update short link data" testShortLinkInvitationSetIncognito
  it "changing user for invitation should update short link data" testShortLinkInvitationChangeUser
  it "changing profile should update address short link data" testShortLinkAddressChangeProfile
  it "changing auto-reply message should update address short link data" testShortLinkAddressChangeAutoReply
  it "changing group profile should update short link data" testShortLinkGroupChangeProfile

testUpdateProfile :: HasCallStack => TestParams -> IO ()
testUpdateProfile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob cath
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"
      alice <## "(the updated profile will be sent to all your contacts)"
      alice ##> "/p alice"
      concurrentlyN_
        [ alice <## "user bio removed (your 2 contacts are notified)",
          bob <## "contact alice removed bio",
          cath <## "contact alice removed bio"
        ]
      alice ##> "/p alice Alice Jones"
      concurrentlyN_
        [ alice <## "user bio changed to Alice Jones (your 2 contacts are notified)",
          bob <## "contact alice updated bio: Alice Jones",
          cath <## "contact alice updated bio: Alice Jones"
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

testUpdateProfileImage :: HasCallStack => TestParams -> IO ()
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

testMultiWordProfileNames :: HasCallStack => TestParams -> IO ()
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
      alice
        <### [ "member #'Our Team' 'Cath Johnson' does not have direct connection, creating",
               "contact for member #'Our Team' 'Cath Johnson' is created",
               "sent invitation to connect directly to member #'Our Team' 'Cath Johnson'",
               WithTime "@'Cath Johnson' hello"
             ]
      cath <## "#'Our Team' 'Alice Jones' is creating direct contact 'Alice Jones' with you"
      cath <# "'Alice Jones'> hello"
      cath <## "'Alice Jones': you can send messages to contact"
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
    baseProfile = Profile {displayName = "", fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

testUserContactLink :: HasCallStack => TestParams -> IO ()
testUserContactLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True
      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice @@@ [("@bob", "Audio/video calls: enabled")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      threadDelay 100000
      alice @@@ [("@bob", lastChatFeature)]
      alice <##> bob

      cath ##> ("/c " <> cLink)
      alice <#? cath
      alice @@@ [("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      threadDelay 100000
      alice @@@ [("@cath", lastChatFeature), ("@bob", "hey")]
      alice <##> cath

testRetryConnectingViaContactLink :: HasCallStack => TestParams -> IO ()
testRetryConnectingViaContactLink ps = testChatCfgOpts2 cfg' opts' aliceProfile bobProfile test ps
  where
    tmp = tmpPath ps
    test alice bob = do
      cLink <- withSmpServer' serverCfg' $ do
        alice ##> "/ad"
        getContactLink alice True
      alice <## "server disconnected localhost ()"
      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: ok to connect"
      _sLinkData <- getTermLine bob
      bob ##> ("/_connect 1 " <> cLink)
      bob <##. "smp agent error: BROKER"
      withSmpServer' serverCfg' $ do
        alice <## "server connected localhost ()"
        threadDelay 250000
        bob ##> ("/_connect plan 1 " <> cLink)
        bob <## "contact address: ok to connect"
        _sLinkData <- getTermLine bob
        bob ##> ("/_connect 1 " <> cLink)
        alice <#? bob
      alice <## "server disconnected localhost ()"
      bob <## "server disconnected localhost ()"
      alice ##> "/ac bob"
      alice <##. "smp agent error: BROKER"
      withSmpServer' serverCfg' $ do
        alice <## "server connected localhost ()"
        bob <## "server connected localhost ()"
        alice ##> "/ac bob"
        alice <## "bob (Bob): accepting contact request, you can send messages to contact"
        concurrently_
          (bob <## "alice (Alice): contact is connected")
          (alice <## "bob (Bob): contact is connected")
        alice #> "@bob message 1"
        bob <# "alice> message 1"
        bob #> "@alice message 2"
        alice <# "bob> message 2"
      alice <## "server disconnected localhost (@bob)"
      bob <## "server disconnected localhost (@alice)"
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          msgQueueQuota = 2,
          serverStoreCfg = persistentServerStoreCfg tmp
        }
    fastRetryInterval = defaultReconnectInterval {initialInterval = 50000} -- same as in agent tests
    cfg' =
      testCfg
        { agentConfig =
            testAgentCfg
              { quotaExceededTimeout = 1,
                messageRetryInterval = RetryInterval2 {riFast = fastRetryInterval, riSlow = fastRetryInterval}
              }
        }
    opts' =
      testOpts
        { coreOptions =
            testCoreOpts
              { smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7003"]
              }
        }

testProfileLink :: HasCallStack => TestParams -> IO ()
testProfileLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      alice ##> "/ad"
      (sLink, _cLink) <- getContactLinks alice True

      bob ##> ("/c " <> sLink)
      alice <#? bob
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

      alice ##> "/pa on"
      alice <## "new contact address set"

      bob <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink bob sLink

      cath ##> ("/c " <> sLink)
      alice <#? cath
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice <##> cath

      checkAliceProfileLink cath sLink

      alice ##> "/pa off"
      alice <## "contact address removed"

      bob <## "alice removed contact address"
      checkAliceNoProfileLink bob

      cath <## "alice removed contact address"
      checkAliceNoProfileLink cath

      alice ##> "/pa on"
      alice <## "new contact address set"

      bob <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink bob sLink

      cath <## "alice set new contact address, use /info alice to view"
      checkAliceProfileLink cath sLink

      alice ##> "/da"
      alice <## "Your chat address is deleted - accepted contacts will remain connected."
      alice <## "To create a new chat address use /ad"

      bob <## "alice removed contact address"
      checkAliceNoProfileLink bob

      cath <## "alice removed contact address"
      checkAliceNoProfileLink cath
  where
    checkAliceProfileLink cc sLink = do
      cc ##> "/info alice"
      cc <## "contact ID: 2"
      cc <##. "receiving messages via"
      cc <##. "sending messages via"
      cc <## ("contact address: " <> sLink)
      cc <## "you've shared main profile with this contact"
      cc <## "connection not verified, use /code command to see security code"
      cc <## "quantum resistant end-to-end encryption"
      cc <## currentChatVRangeInfo
    checkAliceNoProfileLink cc = do
      cc ##> "/info alice"
      cc <## "contact ID: 2"
      cc <##. "receiving messages via"
      cc <##. "sending messages via"
      cc <## "you've shared main profile with this contact"
      cc <## "connection not verified, use /code command to see security code"
      cc <## "quantum resistant end-to-end encryption"
      cc <## currentChatVRangeInfo

testUserContactLinkAutoAccept :: HasCallStack => TestParams -> IO ()
testUserContactLinkAutoAccept =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice @@@ [("@bob", "Audio/video calls: enabled")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
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
      alice <## "cath (Catherine): you can send messages to contact"
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
      alice @@@ [("@dan", "Audio/video calls: enabled"), ("@cath", "hey"), ("@bob", "hey")]
      alice ##> "/ac dan"
      alice <## "dan (Daniel): accepting contact request, you can send messages to contact"
      concurrently_
        (dan <## "alice (Alice): contact is connected")
        (alice <## "dan (Daniel): contact is connected")
      threadDelay 100000
      alice @@@ [("@dan", lastChatFeature), ("@cath", "hey"), ("@bob", "hey")]
      alice <##> dan

testDeduplicateContactRequests :: HasCallStack => TestParams -> IO ()
testDeduplicateContactRequests = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@bob", "Audio/video calls: enabled")]
    bob @@@! [(":1", "", Just ConnJoined)]

    bob ##> ("/c " <> cLink)
    alice <#? bob
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@bob", "Audio/video calls: enabled")]
    bob @@@! [(":3", "", Just ConnJoined), (":2", "", Just ConnJoined), (":1", "", Just ConnJoined)]

    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request, you can send messages to contact"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")

    bob ##> ("/c " <> cLink)
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"
    threadDelay 100000
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
    -- TODO [short links] test falls here because alice has 2 sets of feature items
    -- alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("@cath", "Audio/video calls: enabled"), ("@bob", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    threadDelay 100000
    alice @@@ [("@cath", lastChatFeature), ("@bob", "hey")]
    alice <##> cath

testDeduplicateContactRequestsProfileChange :: HasCallStack => TestParams -> IO ()
testDeduplicateContactRequestsProfileChange = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True

    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@bob", "Audio/video calls: enabled")]

    bob ##> "/p bob"
    bob <## "user bio removed (your 0 contacts are notified)"
    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice @@@ [("@bob", "Audio/video calls: enabled")]

    bob ##> "/p bob Bob Ross"
    bob <## "user bio changed to Bob Ross (your 0 contacts are notified)"
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@bob", "Audio/video calls: enabled")]

    bob ##> "/p robert Robert"
    bob <## "user profile is changed to robert (Robert) (your 0 contacts are notified)"
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@robert", "Audio/video calls: enabled")]

    alice ##> "/ac bob"
    alice <## "no contact request from bob"
    alice ##> "/ac robert"
    alice <## "robert (Robert): accepting contact request, you can send messages to contact"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "robert (Robert): contact is connected")

    bob ##> ("/c " <> cLink)
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"
    threadDelay 100000
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
    threadDelay 100000
    -- TODO [short links] test falls here because alice has 2 sets of feature items
    -- alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi"), (0, "hey"), (1, "hi"), (0, "hey")])
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "hi"), (1, "hey"), (0, "hi"), (1, "hey")])

    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice @@@ [("@cath", "Audio/video calls: enabled"), ("@robert", "hey")]
    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    threadDelay 100000
    alice @@@ [("@cath", lastChatFeature), ("@robert", "hey")]
    alice <##> cath

testRejectContactAndDeleteUserContact :: HasCallStack => TestParams -> IO ()
testRejectContactAndDeleteUserContact = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/_address 1"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice @@@ [("@bob", "Audio/video calls: enabled")]

    alice ##> "/rc bob"
    alice <## "bob: contact request rejected"
    alice @@@ []
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

testKeepConnectionRequests :: HasCallStack => TestParams -> IO ()
testKeepConnectionRequests = testChat3 aliceProfile bobProfile cathProfile $
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

    -- can accept and reject requests after address deletion
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request, you can send messages to contact"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")
    alice <##> bob

    alice ##> "/rc cath"
    alice <## "cath: contact request rejected"

    alice @@@ [("@bob", "hey")]

    -- bob's request to new address uses different name
    alice ##> "/ad"
    cLink' <- getContactLink alice True

    bob ##> ("/c " <> cLink')
    bob <## "connection request sent!"
    alice <## "bob_1 (Bob) wants to connect to you!"
    alice <## "to accept: /ac bob_1"
    alice <## "to reject: /rc bob_1 (the sender will NOT be notified)"

    alice ##> "/ac bob_1"
    alice <## "bob_1 (Bob): accepting contact request, you can send messages to contact"
    concurrently_
      (bob <## "alice_1 (Alice): contact is connected")
      (alice <## "bob_1 (Bob): contact is connected")

    alice #> "@bob_1 hi"
    bob <# "alice_1> hi"
    bob #> "@alice_1 hey"
    alice <# "bob_1> hey"

    cath ##> ("/c " <> cLink')
    alice <#? cath

    alice ##> "/ac cath"
    alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
    concurrently_
      (cath <## "alice (Alice): contact is connected")
      (alice <## "cath (Catherine): contact is connected")
    alice <##> cath

    alice @@@ [("@cath", "hey"), ("@bob_1", "hey"), ("@bob", "hey")]

testContactLinkDeletedConnectedContactWorks :: HasCallStack => TestParams -> IO ()
testContactLinkDeletedConnectedContactWorks = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob

    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request, you can send messages to contact"
    concurrently_
      (bob <## "alice (Alice): contact is connected")
      (alice <## "bob (Bob): contact is connected")
    alice @@@ [("@bob", "Audio/video calls: enabled")]
    bob @@@ [("@alice", "Audio/video calls: enabled")]

    alice ##> "/da"
    alice <## "Your chat address is deleted - accepted contacts will remain connected."
    alice <## "To create a new chat address use /ad"

    alice <##> bob
    alice @@@ [("@bob", "hey")]
    bob @@@ [("@alice", "hey")]

testAutoReplyMessage :: HasCallStack => TestParams -> IO ()
testAutoReplyMessage = testChatCfg2 testCfgNoShortLinks aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLinkNoShortLink alice True
    alice ##> "/auto_accept on incognito=off text hello!"
    alice <## "auto_accept on"
    alice <## "auto reply:"
    alice <## "hello!"

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob (Bob): accepting contact request..."
    alice <## "bob (Bob): you can send messages to contact"
    alice <# "@bob hello!"
    concurrentlyN_
      [ do
          bob <# "alice> hello!"
          bob <## "alice (Alice): contact is connected",
        alice <## "bob (Bob): contact is connected"
      ]

testAutoReplyMessageInIncognito :: HasCallStack => TestParams -> IO ()
testAutoReplyMessageInIncognito = testChatCfg2 testCfgNoShortLinks aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLinkNoShortLink alice True
    alice ##> "/auto_accept on incognito=on text hello!"
    alice <## "auto_accept on, incognito"
    alice <## "auto reply:"
    alice <## "hello!"

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    alice <## "bob (Bob): accepting contact request..."
    alice <## "bob (Bob): you can send messages to contact"
    alice <# "i @bob hello!"
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ do
          bob <# (aliceIncognito <> "> hello!")
          bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## "use /i bob to print out this incognito profile again"
      ]

testBusinessAddress :: HasCallStack => TestParams -> IO ()
testBusinessAddress = testChat3 businessProfile aliceProfile {fullName = "Alice @ Biz"} bobProfile $
  \biz alice bob -> do
    biz ##> "/ad"
    cLink <- getContactLink biz True
    biz ##> "/auto_accept on business"
    biz <## "auto_accept on, business"
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: ok to connect"
    _sLinkData <- getTermLine bob
    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting, allowed to reconnect"
    biz <## "#bob (Bob): accepting business address request..."
    bob <## "#biz: joining the group..."
    -- the next command can be prone to race conditions
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "business address: connecting to business #biz"
    biz <## "#bob: bob_1 joined the group"
    bob <## "#biz: you joined the group"
    biz #> "#bob hi"
    bob <# "#biz biz_1> hi"
    bob #> "#biz hello"
    biz <# "#bob bob_1> hello"
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "business address: known business #biz"
    bob <## "use #biz <message> to send messages"
    connectUsers biz alice
    biz <##> alice
    biz ##> "/a #bob alice"
    biz <## "invitation to join the group #bob sent to alice"
    alice <## "#bob (Bob): biz invites you to join the group as member"
    alice <## "use /j bob to accept"
    alice ##> "/j bob"
    concurrentlyN_
      [ do
          alice <## "#bob: you joined the group"
          alice <### [WithTime "#bob biz> hi [>>]", WithTime "#bob bob_1> hello [>>]"]
          alice <## "#bob: member bob_1 (Bob) is connected",
        biz <## "#bob: alice joined the group",
        do
          bob <## "#biz: biz_1 added alice (Alice @ Biz) to the group (connecting...)"
          bob <## "#biz: new member alice is connected"
      ]
    alice #> "#bob hey"
    concurrently_
      (bob <# "#biz alice> hey")
      (biz <# "#bob alice> hey")
    bob #> "#biz hey there"
    concurrently_
      (alice <# "#bob bob_1> hey there")
      (biz <# "#bob bob_1> hey there")

testBusinessUpdateProfiles :: HasCallStack => TestParams -> IO ()
testBusinessUpdateProfiles = testChatCfg4 testCfgNoShortLinks businessProfile aliceProfile bobProfile cathProfile $
  \biz alice bob cath -> do
    biz ##> "/ad"
    cLink <- getContactLinkNoShortLink biz True
    biz ##> "/auto_accept on business text Welcome"
    biz <## "auto_accept on, business"
    biz <## "auto reply:"
    biz <## "Welcome"
    alice ##> ("/c " <> cLink)
    alice <## "connection request sent!"
    biz <## "#alice (Alice): accepting business address request..."
    alice <## "#biz: joining the group..."
    biz <# "#alice Welcome" -- auto reply
    biz <## "#alice: alice_1 joined the group"
    alice
      <###
        [ WithTime "#biz biz_1> Welcome",
          "#biz: you joined the group"
        ]
    biz #> "#alice hi"
    alice <# "#biz biz_1> hi"
    alice #> "#biz hello"
    biz <# "#alice alice_1> hello"
    alice ##> "/p alisa"
    alice <## "user profile is changed to alisa (your 0 contacts are notified)"
    alice #> "#biz hello again" -- profile update is sent with message
    biz <## "alice_1 updated group #alice:"
    biz <## "changed to #alisa"
    biz <# "#alisa alisa_1> hello again"
    -- customer can invite members too, if business allows
    biz ##> "/mr alisa alisa_1 admin"
    biz <## "#alisa: you changed the role of alisa_1 to admin"
    alice <## "#biz: biz_1 changed your role from member to admin"
    connectUsersNoShortLink alice bob
    alice ##> "/a #biz bob"
    alice <## "invitation to join the group #biz sent to bob"
    bob <## "#biz (Biz Inc): alisa invites you to join the group as member"
    bob <## "use /j biz to accept"
    bob ##> "/j biz"
    concurrentlyN_
      [ do
          bob <## "#biz: you joined the group"
          bob
            <###
              [ WithTime "#biz biz_1> Welcome [>>]",
                WithTime "#biz biz_1> hi [>>]",
                WithTime "#biz alisa> hello [>>]",
                WithTime "#biz alisa> hello again [>>]"
              ]
          bob <## "#biz: member biz_1 (Biz Inc) is connected",
        alice <## "#biz: bob joined the group",
        do
          biz <## "#alisa: alisa_1 added bob (Bob) to the group (connecting...)"
          biz <## "#alisa: new member bob is connected"
      ]
    -- changing other member profiles does not change group profile
    bob ##> "/p robert"
    bob <## "user profile is changed to robert (your 1 contacts are notified)"
    alice <## "contact bob changed to robert" -- only alice receives profile update
    alice <## "use @robert <message> to send messages"
    bob #> "#biz hi there" -- profile update is sent to group with message
    alice <# "#biz robert> hi there"
    biz <# "#alisa robert> hi there"
    -- add business team member
    connectUsersNoShortLink biz cath
    biz ##> "/a #alisa cath"
    biz <## "invitation to join the group #alisa sent to cath"
    cath <## "#alisa: biz invites you to join the group as member"
    cath <## "use /j alisa to accept"
    cath ##> "/j alisa"
    concurrentlyN_
      [ do
          cath <## "#alisa: you joined the group"
          cath
            <###
              [ WithTime "#alisa biz> Welcome [>>]",
                WithTime "#alisa biz> hi [>>]",
                WithTime "#alisa alisa_1> hello [>>]",
                WithTime "#alisa alisa_1> hello again [>>]",
                WithTime "#alisa robert> hi there [>>]"
              ]
          cath <## "#alisa: member alisa_1 is connected"
          cath <## "#alisa: member robert is connected",
        biz <## "#alisa: cath joined the group",
        do
          alice <## "#biz: biz_1 added cath (Catherine) to the group (connecting...)"
          alice <## "#biz: new member cath is connected",
        do
          bob <## "#biz: biz_1 added cath (Catherine) to the group (connecting...)"
          bob <## "#biz: new member cath is connected"
      ]
    -- both customers receive business profile change
    biz ##> "/p business"
    biz <## "user profile is changed to business (your 1 contacts are notified)"
    biz #> "#alisa hey"
    concurrentlyN_
      [ do
          alice <## "biz_1 updated group #biz:"
          alice <## "changed to #business"
          alice <# "#business business_1> hey",
        do
          bob <## "biz_1 updated group #biz:"
          bob <## "changed to #business"
          bob <# "#business business_1> hey",
        do
          cath <## "contact biz changed to business"
          cath <## "use @business <message> to send messages"
          cath <# "#alisa business> hey"
      ]
    biz ##> "/set voice #alisa on"
    biz <## "updated group preferences:"
    biz <## "Voice messages: on"
    concurrentlyN_
      [ do
          alice <## "business_1 updated group #business:"
          alice <## "updated group preferences:"
          alice <## "Voice messages: on",
        do
          bob <## "business_1 updated group #business:"
          bob <## "updated group preferences:"
          bob <## "Voice messages: on",
        do
          cath <## "business updated group #alisa:"
          cath <## "updated group preferences:"
          cath <## "Voice messages: on"
      ]
    biz #$> ("/_get chat #1 count=1", chat, [(1, "Voice messages: on")])
    alice #$> ("/_get chat #1 count=1", chat, [(0, "Voice messages: on")])
    bob #$> ("/_get chat #1 count=1", chat, [(0, "Voice messages: on")])
    cath #$> ("/_get chat #1 count=1", chat, [(0, "Voice messages: on")])

testPlanAddressOkKnown :: HasCallStack => TestParams -> IO ()
testPlanAddressOkKnown =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/_connect plan 1 " <> cLink)
      bob <## "contact address: ok to connect"
      _sLinkData <- getTermLine bob

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice @@@ [("@bob", "Audio/video calls: enabled")]
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
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

testPlanAddressOwn :: HasCallStack => TestParams -> IO ()
testPlanAddressOwn ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
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
    alice @@@ [("@alice_1", "Audio/video calls: enabled"), (":2", "")]
    alice ##> "/ac alice_1"
    alice <## "alice_1 (Alice): accepting contact request, you can send messages to contact"
    alice
      <### [ "alice_1 (Alice): contact is connected",
             "alice_2 (Alice): contact is connected"
           ]

    threadDelay 100000
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

testPlanAddressConnecting :: HasCallStack => TestParams -> IO ()
testPlanAddressConnecting ps = do
  cLink <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/ad"
    getContactLink alice True
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    threadDelay 100000

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"

    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting, allowed to reconnect"

    let cLinkSchema2 = linkAnotherSchema cLink
    bob ##> ("/_connect plan 1 " <> cLinkSchema2)
    bob <## "contact address: connecting, allowed to reconnect"

    threadDelay 100000
  withTestChat ps "alice" $ \alice -> do
    alice <## "Your address is active! To show: /sa"
    alice <## "bob (Bob) wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request, you can send messages to contact"
  withTestChat ps "bob" $ \bob -> do
    threadDelay 500000
    bob <## "alice (Alice): contact is connected"
    bob @@@ [("@alice", "Audio/video calls: enabled")]
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

testPlanAddressConnectingSlow :: HasCallStack => TestParams -> IO ()
testPlanAddressConnectingSlow ps = do
  cLink <- withNewTestChatCfg ps testCfgSlow "alice" aliceProfile $ \alice -> do
    alice ##> "/ad"
    getContactLinkNoShortLink alice True
  withNewTestChatCfg ps testCfgSlow "bob" bobProfile $ \bob -> do
    threadDelay 100000

    bob ##> ("/c " <> cLink)
    bob <## "connection request sent!"

    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting, allowed to reconnect"

    let cLinkSchema2 = linkAnotherSchema cLink
    bob ##> ("/_connect plan 1 " <> cLinkSchema2)
    bob <## "contact address: connecting, allowed to reconnect"

    threadDelay 100000
  withTestChatCfg ps testCfgSlow "alice" $ \alice -> do
    alice <## "Your address is active! To show: /sa"
    alice <## "bob (Bob) wants to connect to you!"
    alice <## "to accept: /ac bob"
    alice <## "to reject: /rc bob (the sender will NOT be notified)"
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request..."
  withTestChatCfg ps testCfgSlow "bob" $ \bob -> do
    threadDelay 500000
    bob @@@ [("@alice", "")]
    bob ##> ("/_connect plan 1 " <> cLink)
    bob <## "contact address: connecting to contact alice"

    let cLinkSchema2 = linkAnotherSchema cLink
    bob ##> ("/_connect plan 1 " <> cLinkSchema2)
    bob <## "contact address: connecting to contact alice"

    bob ##> ("/c " <> cLink)
    bob <## "contact address: connecting to contact alice"

testPlanAddressContactDeletedReconnected :: HasCallStack => TestParams -> IO ()
testPlanAddressContactDeletedReconnected =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      cLink <- getContactLink alice True

      bob ##> ("/c " <> cLink)
      alice <#? bob
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
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
      _sLinkData <- getTermLine bob

      let cLinkSchema2 = linkAnotherSchema cLink
      bob ##> ("/_connect plan 1 " <> cLinkSchema2)
      bob <## "contact address: ok to connect"
      _sLinkData <- getTermLine bob

      bob ##> ("/c " <> cLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
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

testPlanAddressContactViaAddress :: HasCallStack => TestParams -> IO ()
testPlanAddressContactViaAddress =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      (_sLink, cLink) <- getContactLinks alice True

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

          -- terminal api
          bob ##> ("/c " <> cLink)
          connecting alice bob

          bob ##> "/delete @alice"
          bob <## "alice: contact is deleted"
          alice ##> "/delete @bob"
          alice <## "bob: contact is deleted"

          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          -- GUI api
#if defined(dbPostgres)
          bob ##> "/_connect contact 1 4"
#else
          bob ##> "/_connect contact 1 2"
#endif
          connecting alice bob
  where
    connecting alice bob = do
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrentlyN_
        [ do
            bob <## "alice set new contact address, use /info alice to view"
            bob <## "alice (Alice): contact is connected",
          alice <## "bob (Bob): contact is connected"
        ]
      alice <##> bob
      bob @@@ [("@alice", "hey")]

testPlanAddressContactViaShortAddress :: HasCallStack => TestParams -> IO ()
testPlanAddressContactViaShortAddress =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/ad"
      (sLink, _) <- getContactLinks alice True

      alice ##> "/pa on" -- not necessary, without it bob would receive profile update removing contact link
      alice <## "new contact address set"

      case A.parseOnly strP (B.pack sLink) of
        Left _ -> error "error parsing contact link"
        Right shortLink -> do
          let profile = aliceProfile {contactLink = Just shortLink}
          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          bob ##> "/delete @alice"
          bob <## "alice: contact is deleted"

          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          bob ##> ("/_connect plan 1 " <> sLink)
          bob <## "contact address: known contact without connection alice"

          -- terminal api
          bob ##> ("/c " <> sLink)
          connecting alice bob

          bob ##> "/delete @alice"
          bob <## "alice: contact is deleted"
          alice ##> "/delete @bob"
          alice <## "bob: contact is deleted"

          void $ withCCUser bob $ \user -> withCCTransaction bob $ \db -> runExceptT $ createContact db user profile
          bob @@@ [("@alice", "")]

          -- GUI api
#if defined(dbPostgres)
          bob ##> "/_connect contact 1 4"
#else
          bob ##> "/_connect contact 1 2"
#endif
          connecting alice bob
  where
    connecting alice bob = do
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      bob @@@ [("@alice", "hey")]

testConnectIncognitoInvitationLink :: HasCallStack => TestParams -> IO ()
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
      [ alice <## "user bio removed (your 1 contacts are notified)",
        cath <## "contact alice removed bio"
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

testConnectIncognitoContactAddress :: HasCallStack => TestParams -> IO ()
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
    alice <## (bobIncognito <> ": accepting contact request, you can send messages to contact")
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

testAcceptContactRequestIncognito :: HasCallStack => TestParams -> IO ()
testAcceptContactRequestIncognito = testChatCfg3 testCfgNoShortLinks aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice ##> "/ad"
    cLink <- getContactLinkNoShortLink alice True
    -- GUI /_accept api
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice ##> "/_accept incognito=on 1"
    alice <## "bob (Bob): accepting contact request, you can send messages to contact"
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
    -- terminal /accept api
    cath ##> ("/c " <> cLink)
    alice <#? cath
    alice ##> "/accept incognito cath"
    alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
    aliceIncognitoCath <- getTermLine alice
    concurrentlyN_
      [ cath <## (aliceIncognitoCath <> ": contact is connected"),
        do
          alice <## ("cath (Catherine): contact is connected, your incognito profile for this contact is " <> aliceIncognitoCath)
          alice <## "use /i cath to print out this incognito profile again"
      ]
    alice `hasContactProfiles` ["alice", "cath", T.pack aliceIncognitoCath]
    cath `hasContactProfiles` ["cath", T.pack aliceIncognitoCath]

testSetConnectionIncognito :: HasCallStack => TestParams -> IO ()
testSetConnectionIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/connect"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
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

testResetConnectionIncognito :: HasCallStack => TestParams -> IO ()
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

testSetConnectionIncognitoProhibitedDuringNegotiation :: HasCallStack => TestParams -> IO ()
testSetConnectionIncognitoProhibitedDuringNegotiation ps = do
  inv <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    alice ##> "/connect"
    getInvitation alice
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
  withTestChat ps "alice" $ \alice -> do
    threadDelay 250000
    alice <## "bob (Bob): contact is connected"
    alice ##> "/_set incognito :1 on"
    alice <## "chat db error: SEPendingConnectionNotFound {connId = 1}"
    withTestChat ps "bob" $ \bob -> do
      bob <## "alice (Alice): contact is connected"
      alice <##> bob
      alice `hasContactProfiles` ["alice", "bob"]
      bob `hasContactProfiles` ["alice", "bob"]

testSetConnectionIncognitoProhibitedDuringNegotiationSlow :: HasCallStack => TestParams -> IO ()
testSetConnectionIncognitoProhibitedDuringNegotiationSlow ps = do
  inv <- withNewTestChatCfg ps testCfgSlow "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    alice ##> "/connect"
    getInvitationNoShortLink alice
  withNewTestChatCfg ps testCfgSlow "bob" bobProfile $ \bob -> do
    threadDelay 250000
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
  withTestChatCfg ps testCfgSlow "alice" $ \alice -> do
    threadDelay 250000
    alice ##> "/_set incognito :1 on"
    alice <## "chat db error: SEPendingConnectionNotFound {connId = 1}"
    withTestChatCfg ps testCfgSlow "bob" $ \bob -> do
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      alice `hasContactProfiles` ["alice", "bob"]
      bob `hasContactProfiles` ["alice", "bob"]

testConnectionIncognitoUnchangedErrors :: HasCallStack => TestParams -> IO ()
testConnectionIncognitoUnchangedErrors = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/connect"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 off"
    alice <## "incognito mode change prohibited"
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
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

testSetResetSetConnectionIncognito :: HasCallStack => TestParams -> IO ()
testSetResetSetConnectionIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/_connect 1 incognito=off"
    inv <- getInvitation alice
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
    alice <## "connection 1 changed to incognito"
    alice ##> "/_set incognito :1 off"
    alice <## "connection 1 changed to non incognito"
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
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

testJoinGroupIncognito :: HasCallStack => TestParams -> IO ()
testJoinGroupIncognito =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
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
                     "contact and member are merged: bob, #secret_club bob_1",
                     "use @bob <message> to send messages"
                   ],
          do
            bob <## "#secret_club: alice added dan_1 (Daniel) to the group (connecting...)"
            bob <## "#secret_club: new member dan_1 is connected"
            bob <## "contact and member are merged: dan, #secret_club dan_1"
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
      -- non incognito direct connections are separate
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
      cath <## "bad chat command: not current member"

testCantInviteContactIncognito :: HasCallStack => TestParams -> IO ()
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

testCantSeeGlobalPrefsUpdateIncognito :: HasCallStack => TestParams -> IO ()
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
    alice <## "user bio removed (your 1 contacts are notified)"
    alice <## "updated preferences:"
    alice <## "Full deletion allowed: always"
    (alice </)
    -- bob doesn't receive profile update
    (bob </)
    cath <## "contact alice removed bio"
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

testDeleteContactThenGroupDeletesIncognitoProfile :: HasCallStack => TestParams -> IO ()
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

testDeleteGroupThenContactDeletesIncognitoProfile :: HasCallStack => TestParams -> IO ()
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

testSetAlias :: HasCallStack => TestParams -> IO ()
testSetAlias = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    connectUsers alice bob
    alice #$> ("/_set alias @2 my friend bob", id, "contact bob alias updated: my friend bob")
    alice ##> "/contacts"
    alice <## "bob (Bob) (alias: my friend bob)"
    alice #$> ("/_set alias @2", id, "contact bob alias removed")
    alice ##> "/contacts"
    alice <## "bob (Bob)"

testChangePCCUser :: HasCallStack => TestParams -> IO ()
testChangePCCUser = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- Create a new invite
    alice ##> "/connect"
    _ <- getInvitation alice
    -- Create new user and go back to original user
    alice ##> "/create user alisa"
    showActiveUser alice "alisa"
    alice ##> "/create user alisa2"
    showActiveUser alice "alisa2"
    alice ##> "/user alice"
    showActiveUser alice "alice (Alice)"
    -- Change connection to newly created user
    alice ##> "/_set conn user :1 2"
    alice <## "connection 1 changed from user alice to user alisa, new link:"
    alice <## ""
    _ <- getTermLine alice
    alice <## ""
    alice <## "The invitation link for old clients:"
    _ <- getTermLine alice
    alice ##> "/user alisa"
    showActiveUser alice "alisa"
    -- Change connection back to other user
    alice ##> "/_set conn user :1 3"
    alice <## "connection 1 changed from user alisa to user alisa2, new link:"
    alice <## ""
    _shortInv <- getTermLine alice
    alice <## ""
    alice <## "The invitation link for old clients:"
    inv <- getTermLine alice
    alice ##> "/user alisa2"
    showActiveUser alice "alisa2"
    -- Connect
    bob ##> ("/connect " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (alice <## "bob (Bob): contact is connected")
      (bob <## "alisa2: contact is connected")
    alice <##> bob

testChangePCCUserFromIncognito :: HasCallStack => TestParams -> IO ()
testChangePCCUserFromIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- Create a new invite and set as incognito
    alice ##> "/connect"
    _ <- getInvitation alice
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
    alice <## "connection 1 changed to incognito"
    -- Create new user and go back to original user
    alice ##> "/create user alisa"
    showActiveUser alice "alisa"
    alice ##> "/user alice"
    showActiveUser alice "alice (Alice)"
    -- Change connection to newly created user
    alice ##> "/_set conn user :1 2"
    alice <## "connection 1 changed from user alice to user alisa, new link:"
    alice <## ""
    _ <- getTermLine alice
    alice <## ""
    alice <## "The invitation link for old clients:"
    _ <- getTermLine alice
    alice `hasContactProfiles` ["alice"]
    alice ##> "/user alisa"
    showActiveUser alice "alisa"
    -- Change connection back to initial user
    alice ##> "/_set conn user :1 1"
    alice <## "connection 1 changed from user alisa to user alice, new link:"
    alice <## ""
    _shortInv <- getTermLine alice
    alice <## ""
    alice <## "The invitation link for old clients:"
    inv <- getTermLine alice
    alice ##> "/user alice"
    showActiveUser alice "alice (Alice)"
    -- Connect
    bob ##> ("/connect " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (alice <## "bob (Bob): contact is connected")
      (bob <## "alice (Alice): contact is connected")
    alice <##> bob

testChangePCCUserAndThenIncognito :: HasCallStack => TestParams -> IO ()
testChangePCCUserAndThenIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- Create a new invite and set as incognito
    alice ##> "/connect"
    _ <- getInvitation alice
    -- Create new user and go back to original user
    alice ##> "/create user alisa"
    showActiveUser alice "alisa"
    alice ##> "/user alice"
    showActiveUser alice "alice (Alice)"
    -- Change connection to newly created user
    alice ##> "/_set conn user :1 2"
    alice <## "connection 1 changed from user alice to user alisa, new link:"
    alice <## ""
    _shortInv <- getTermLine alice
    alice <## ""
    alice <## "The invitation link for old clients:"
    inv <- getTermLine alice
    alice ##> "/user alisa"
    showActiveUser alice "alisa"
    -- Change connection to incognito and make sure it's attached to the newly created user profile
    alice ##> "/_set incognito :1 on"
    _ <- getTermLine alice
    alice <## "connection 1 changed to incognito"
    bob ##> ("/connect " <> inv)
    bob <## "confirmation sent!"
    alisaIncognito <- getTermLine alice
    concurrentlyN_
      [ bob <## (alisaIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> alisaIncognito)
          alice <## ("use /i bob to print out this incognito profile again")
      ]
    alice ?#> "@bob hi"
    bob <# (alisaIncognito <> "> hi")
    bob #> ("@" <> alisaIncognito <> " hey")
    alice ?<# "bob> hey"

testChangePCCUserDiffSrv :: HasCallStack => TestParams -> IO ()
testChangePCCUserDiffSrv ps = do
  withSmpServer' serverCfg' $ do
    withNewTestChatCfgOpts ps testCfg testOpts "alice" aliceProfile $ \alice -> do
      withNewTestChatCfgOpts ps testCfg testOpts "bob" bobProfile $ \bob -> do
        -- Create a new invite
        alice ##> "/connect"
        _ <- getInvitation alice
        alice ##> "/_set incognito :1 on"
        _ <- getTermLine alice
        alice <## "connection 1 changed to incognito"
        -- Create new user with different servers
        alice ##> "/create user alisa"
        showActiveUser alice "alisa"
        alice ##> "/smp"
        alice <## "Your servers"
        alice <## "  SMP servers"
        alice <## "    smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"
        alice #$> ("/smp smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@127.0.0.1:7003", id, "ok")
        alice ##> "/smp"
        alice <## "Your servers"
        alice <## "  SMP servers"
        alice <## "    smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@127.0.0.1:7003"
        alice ##> "/user alice"
        showActiveUser alice "alice (Alice)"
        -- Change connection to newly created user and use the newly created connection
        alice ##> "/_set conn user :1 2"
        alice <## "connection 1 changed from user alice to user alisa, new link:"
        alice <## ""
        _shortInv <- getTermLine alice
        alice <## ""
        alice <## "The invitation link for old clients:"
        inv <- getTermLine alice
        alice `hasContactProfiles` ["alice"]
        alice ##> "/user alisa"
        showActiveUser alice "alisa"
        -- Connect
        bob ##> ("/connect " <> inv)
        bob <## "confirmation sent!"
        concurrently_
          (alice <## "bob (Bob): contact is connected")
          (bob <## "alisa: contact is connected")
        alice <##> bob
  where
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False), ("7002", transport @TLS, False)],
          msgQueueQuota = 2
        }

testSetConnectionAlias :: HasCallStack => TestParams -> IO ()
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

testSetGroupAlias :: HasCallStack => TestParams -> IO ()
testSetGroupAlias = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    createGroup2 "team" alice bob
    threadDelay 1500000
    alice ##> "/_set alias #1 friends"
    alice <## "group #team alias updated: friends"
    alice ##> "/groups"
    alice <## "#team (2 members) (alias: friends)"
    alice ##> "/_set alias #1"
    alice <## "group #team alias removed"
    alice ##> "/groups"
    alice <## "#team (2 members)"

testSetContactPrefs :: HasCallStack => TestParams -> IO ()
testSetContactPrefs = testChat2 aliceProfile bobProfile $
  \alice bob -> withXFTPServer $ do
    alice #$> ("/_files_folder ./tests/tmp/alice", id, "ok")
    bob #$> ("/_files_folder ./tests/tmp/bob", id, "ok")
    createDirectoryIfMissing True "./tests/tmp/alice"
    createDirectoryIfMissing True "./tests/tmp/bob"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/alice/test.txt"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/bob/test.txt"
    bob ##> "/_profile 1 {\"displayName\": \"bob\", \"fullName\": \"\", \"shortDescr\": \"Bob\", \"preferences\": {\"voice\": {\"allow\": \"no\"}, \"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
    bob <## "profile image removed"
    bob <## "updated preferences:"
    bob <## "Voice messages allowed: no"
    (bob </)
    connectUsers alice bob
    alice ##> "/_set prefs @2 {}"
    alice <## "your preferences for bob did not change"
    (bob </)
    let startFeatures = [(1, "chat banner"), (0, e2eeInfoPQStr), (0, "Disappearing messages: allowed"), (0, "Full deletion: off"), (0, "Message reactions: enabled"), (0, "Voice messages: off"), (0, "Audio/video calls: enabled")]
    alice #$> ("/_get chat @2 count=100", chat, startFeatures)
    bob #$> ("/_get chat @2 count=100", chat, startFeatures)
    let sendVoice = "/_send @2 json [{\"filePath\": \"test.txt\", \"msgContent\": {\"type\": \"voice\", \"text\": \"\", \"duration\": 10}}]"
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

    -- sending voice message allowed
    bob ##> sendVoice
    bob <# "@alice voice message (00:10)"
    bob <# "/f @alice test.txt"
    bob <## "use /fc 1 to cancel sending"
    alice <# "bob> voice message (00:10)"
    alice <# "bob> sends file test.txt (11 bytes / 11 bytes)"
    alice <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "completed uploading file 1 (test.txt) for alice"
    alice ##> "/fr 1"
    alice
      <### [ "saving file 1 from bob to test_1.txt",
             "started receiving file 1 (test.txt) from bob"
           ]
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
    bob <## "user bio removed (your 1 contacts are notified)"
    bob <## "updated preferences:"
    bob <## "Voice messages allowed: yes"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off"), (1, "Voice messages: enabled")])
    (bob </)
    alice <## "contact bob removed bio"
    alice <## "bob updated preferences for you:"
    alice <## "Voice messages: enabled (you allow: yes, contact allows: yes)"
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact"), (0, "voice message (00:10)"), (1, "Voice messages: off"), (0, "updated profile"), (0, "Voice messages: enabled")])
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
    alice #$> ("/_get chat @2 count=100", chat, startFeatures <> [(1, "Voice messages: enabled for contact"), (0, "voice message (00:10)"), (1, "Voice messages: off"), (0, "updated profile"), (0, "Voice messages: enabled"), (1, "Voice messages: off")])
    bob <## "alice updated preferences for you:"
    bob <## "Voice messages: off (you allow: default (yes), contact allows: no)"
    bob #$> ("/_get chat @2 count=100", chat, startFeatures <> [(0, "Voice messages: enabled for you"), (1, "voice message (00:10)"), (0, "Voice messages: off"), (1, "Voice messages: enabled"), (0, "Voice messages: off")])

testFeatureOffers :: HasCallStack => TestParams -> IO ()
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

testUpdateGroupPrefs :: HasCallStack => TestParams -> IO ()
testUpdateGroupPrefs =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected")])
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected")])
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Full deletion: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: on"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on")])
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"off\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion: off"
      alice <## "Voice messages: off"
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: off"
      bob <## "Voice messages: off"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off")])
      alice ##> "/set voice #team on"
      alice <## "updated group preferences:"
      alice <## "Voice messages: on"
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Voice messages: on"
      threadDelay 500000
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on")])
      threadDelay 500000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}"
      -- no update
      threadDelay 500000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      alice #> "#team hey"
      bob <# "#team alice> hey"
      threadDelay 1000000
      bob #> "#team hi"
      alice <# "#team bob> hi"
      threadDelay 500000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on"), (1, "hey"), (0, "hi")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on"), (0, "hey"), (1, "hi")])

testAllowFullDeletionContact :: HasCallStack => TestParams -> IO ()
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

testAllowFullDeletionGroup :: HasCallStack => TestParams -> IO ()
testAllowFullDeletionGroup =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      threadDelay 1500000
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
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "hi"), (0, "hey"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (1, "hey"), (0, "Full deletion: on")])
      bob #$> ("/_delete item #1 " <> msgItemId <> " broadcast", id, "message deleted")
      alice <# "#team bob> [deleted] hey"
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "hi"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (0, "Full deletion: on")])

testProhibitDirectMessages :: HasCallStack => TestParams -> IO ()
testProhibitDirectMessages =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
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
      bob <## "bad chat command: direct messages not allowed"
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
      alice <## "bad chat command: direct messages not allowed"
      bob ##> "@dan hi"
      bob <## "bad chat command: direct messages not allowed"
      (dan </)
      dan ##> "@alice hi"
      dan <## "bad chat command: direct messages not allowed"
      dan ##> "@bob hi"
      dan <## "bad chat command: direct messages not allowed"
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

testEnableTimedMessagesContact :: HasCallStack => TestParams -> IO ()
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

testEnableTimedMessagesGroup :: HasCallStack => TestParams -> IO ()
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
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Disappearing messages: on (1 sec)"), (1, "hi")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Disappearing messages: on (1 sec)"), (0, "hi")])
      threadDelay 1000000
      alice <## "timed message deleted: hi"
      bob <## "timed message deleted: hi"
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Disappearing messages: on (1 sec)")])
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
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (1, "Disappearing messages: on (1 sec)"), (1, "Disappearing messages: off"), (1, "hey")])
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

testTimedMessagesEnabledGlobally :: HasCallStack => TestParams -> IO ()
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

testUpdateMultipleUserPrefs :: HasCallStack => TestParams -> IO ()
testUpdateMultipleUserPrefs = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    connectUsers alice bob
    alice #> "@bob hi bob"
    bob <# "alice> hi bob"

    connectUsers alice cath
    alice #> "@cath hi cath"
    cath <# "alice> hi cath"

    alice ##> "/_profile 1 {\"displayName\": \"alice\", \"fullName\": \"\", \"shortDescr\": \"Alice\", \"preferences\": {\"fullDelete\": {\"allow\": \"always\"}, \"reactions\": {\"allow\": \"no\"}, \"receipts\": {\"allow\": \"yes\", \"activated\": true}}}"
    alice <## "updated preferences:"
    alice <## "Full deletion allowed: always"
    alice <## "Message reactions allowed: no"

    bob <## "alice updated preferences for you:"
    bob <## "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
    bob <## "Message reactions: off (you allow: default (yes), contact allows: no)"

    cath <## "alice updated preferences for you:"
    cath <## "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
    cath <## "Message reactions: off (you allow: default (yes), contact allows: no)"

    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "hi bob"), (1, "Full deletion: enabled for contact"), (1, "Message reactions: off")])
    alice #$> ("/_get chat @3 count=100", chat, chatFeatures <> [(1, "hi cath"), (1, "Full deletion: enabled for contact"), (1, "Message reactions: off")])

testGroupPrefsDirectForRole :: HasCallStack => TestParams -> IO ()
testGroupPrefsDirectForRole = testChat4 aliceProfile bobProfile cathProfile danProfile $
  \alice bob cath dan -> do
    createGroup3 "team" alice bob cath
    threadDelay 1000000
    alice ##> "/set direct #team on owner"
    alice <## "updated group preferences:"
    alice <## "Direct messages: on for owners"
    directForOwners bob
    directForOwners cath
    threadDelay 1000000
    bob ##> "@cath hello again"
    bob <## "bad chat command: direct messages not allowed"
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

    -- dan cannot send direct messages to alice
    dan ##> "@alice hello alice"
    dan <## "bad chat command: direct messages not allowed"
    (alice </)

    -- alice (owner) can send direct messages to dan
    alice `send` "@dan hello dan"
    alice
      <### [ "member #team dan does not have direct connection, creating",
             "contact for member #team dan is created",
             "sent invitation to connect directly to member #team dan",
             WithTime "@dan hello dan"
           ]
    dan
      <### [ "#team alice is creating direct contact alice with you",
             WithTime "alice> hello dan"
           ]
    dan <## "alice (Alice): you can send messages to contact"
    concurrently_
      (alice <## "dan (Daniel): contact is connected")
      (dan <## "alice (Alice): contact is connected")

    -- now dan can send messages to alice
    dan #> "@alice hi alice"
    alice <# "dan> hi alice"
  where
    directForOwners :: HasCallStack => TestCC -> IO ()
    directForOwners cc = do
      cc <## "alice updated group #team:"
      cc <## "updated group preferences:"
      cc <## "Direct messages: on for owners"

testGroupPrefsFilesForRole :: HasCallStack => TestParams -> IO ()
testGroupPrefsFilesForRole = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> withXFTPServer $ do
    alice #$> ("/_files_folder ./tests/tmp/alice", id, "ok")
    bob #$> ("/_files_folder ./tests/tmp/bob", id, "ok")
    createDirectoryIfMissing True "./tests/tmp/alice"
    createDirectoryIfMissing True "./tests/tmp/bob"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/alice/test1.txt"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/bob/test2.txt"
    createGroup3 "team" alice bob cath
    threadDelay 1000000
    alice ##> "/set files #team on owner"
    alice <## "updated group preferences:"
    alice <## "Files and media: on for owners"
    filesForOwners bob
    filesForOwners cath
    threadDelay 1000000
    bob ##> "/f #team test2.txt"
    bob <## "bad chat command: feature not allowed Files and media"
    (alice </)
    (cath </)
    alice #> "/f #team test1.txt"
    alice <## "use /fc 1 to cancel sending"
    alice <## "completed uploading file 1 (test1.txt) for #team"
    bob <# "#team alice> sends file test1.txt (11 bytes / 11 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    cath <# "#team alice> sends file test1.txt (11 bytes / 11 bytes)"
    cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
  where
    filesForOwners :: HasCallStack => TestCC -> IO ()
    filesForOwners cc = do
      cc <## "alice updated group #team:"
      cc <## "updated group preferences:"
      cc <## "Files and media: on for owners"

testGroupPrefsSimplexLinksForRole :: HasCallStack => TestParams -> IO ()
testGroupPrefsSimplexLinksForRole = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> withXFTPServer $ do
    createGroup3 "team" alice bob cath
    threadDelay 1000000
    alice ##> "/set links #team on owner"
    alice <## "updated group preferences:"
    alice <## "SimpleX links: on for owners"
    linksForOwners bob
    linksForOwners cath
    threadDelay 1000000
    bob ##> "/c"
    inv <- getInvitation bob
    bob ##> ("#team \"" <> inv <> "\\ntest\"")
    bob <## "bad chat command: feature not allowed SimpleX links"
    bob ##> ("/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"" <> inv <> "\\ntest\"}}]")
    bob <## "bad chat command: feature not allowed SimpleX links"
    (alice </)
    (cath </)
    bob `send` ("@alice \"" <> inv <> "\\ntest\"")
    bob <# ("@alice " <> inv)
    bob <## "test"
    alice <# ("bob> " <> inv)
    alice <## "test"
    bob ##> "#team <- @alice https://simplex.chat"
    bob <## "bad chat command: feature not allowed SimpleX links"
    alice #> ("#team " <> inv)
    bob <# ("#team alice> " <> inv)
    cath <# ("#team alice> " <> inv)
  where
    linksForOwners :: HasCallStack => TestCC -> IO ()
    linksForOwners cc = do
      cc <## "alice updated group #team:"
      cc <## "updated group preferences:"
      cc <## "SimpleX links: on for owners"

testSetUITheme :: HasCallStack => TestParams -> IO ()
testSetUITheme =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice #$> ("/_set theme user 1 " <> theme UCMDark, id, "ok")
    alice #$> ("/_set theme @2 " <> theme UCMDark, id, "ok")
    alice #$> ("/_set theme #1 " <> theme UCMDark, id, "ok")
    alice ##> "/u"
    userInfo alice "alice (Alice)"
    alice <## ("UI themes: " <> theme UCMDark)
    alice ##> "/create user alice2"
    userInfo alice "alice2"
    alice ##> "/u alice"
    userInfo alice "alice (Alice)"
    alice <## ("UI themes: " <> theme UCMDark)
    alice ##> "/i @bob"
    contactInfo alice
    alice <## ("UI themes: " <> theme UCMDark)
    alice ##> "/i #team"
    groupInfo alice
    alice <## ("UI themes: " <> theme UCMDark)
    alice #$> ("/_set theme user 1", id, "ok")
    alice #$> ("/_set theme @2", id, "ok")
    alice #$> ("/_set theme #1", id, "ok")
    alice ##> "/u"
    userInfo alice "alice (Alice)"
    alice ##> "/i @bob"
    contactInfo alice
    alice ##> "/i #team"
    groupInfo alice
  where
    theme cm = T.unpack $ encodeJSON UIThemeEntityOverrides {light = Nothing, dark = Just $ UIThemeEntityOverride cm Nothing defaultUIColors}
    userInfo a name = do
      a <## ("user profile: " <> name)
      a <## "use /p <name> [<bio>] to change it"
      a <## "(the updated profile will be sent to all your contacts)"
    contactInfo a = do
      a <## "contact ID: 2"
      a <## "receiving messages via: localhost"
      a <## "sending messages via: localhost"
      a <## "you've shared main profile with this contact"
      a <## "connection not verified, use /code command to see security code"
      a <## "quantum resistant end-to-end encryption"
      a <## ("peer chat protocol version range: (Version 1, " <> show currentChatVersion <> ")")
    groupInfo a = do
      a <## "group ID: 1"
      a <## "current members: 1"

testShortLinkInvitation :: HasCallStack => TestParams -> IO ()
testShortLinkInvitation =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    alice ##> "/c"
    (inv, _) <- getInvitations alice
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (alice <## "bob (Bob): contact is connected")
      (bob <## "alice (Alice): contact is connected")
    alice #> "@bob hi"
    bob <# "alice> hi"
    bob #> "@alice hey"
    alice <# "bob> hey"

testPlanShortLinkInvitation :: HasCallStack => TestParams -> IO ()
testPlanShortLinkInvitation =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    alice ##> "/c"
    (inv, _) <- getInvitations alice
    alice ##> ("/_connect plan 1 " <> inv)
    alice <## "invitation link: own link"
    alice ##> ("/_connect plan 1 " <> slSimplexScheme inv)
    alice <## "invitation link: own link"
    bob ##> ("/_connect plan 1 " <> inv)
    bob <## "invitation link: ok to connect"
    _sLinkData <- getTermLine bob
    -- nobody else can connect
    cath ##> ("/_connect plan 1 " <> inv)
    cath <##. "error: connection authorization failed"
    cath ##> ("/c " <> inv)
    cath <##. "error: connection authorization failed"
    -- bob can retry "plan"
    bob ##> ("/_connect plan 1 " <> inv)
    bob <## "invitation link: ok to connect"
    _sLinkData <- getTermLine bob
    -- with simplex: scheme too
    bob ##> ("/_connect plan 1 " <> slSimplexScheme inv)
    bob <## "invitation link: ok to connect"
    _sLinkData <- getTermLine bob
    bob ##> ("/c " <> inv)
    bob <## "confirmation sent!"
    concurrently_
      (alice <## "bob (Bob): contact is connected")
      (bob <## "alice (Alice): contact is connected")
    alice #> "@bob hi"
    bob <# "alice> hi"
    bob #> "@alice hey"
    alice <# "bob> hey"
    bob ##> ("/_connect plan 1 " <> inv)
    bob <##. "error: connection authorization failed"
    alice ##> ("/_connect plan 1 " <> inv)
    alice <##. "error: connection authorization failed" -- short_link_inv and conn_req_inv are removed after connection

slSimplexScheme :: String -> String
slSimplexScheme sl = T.unpack $ T.replace "https://localhost/" "simplex:/" (T.pack sl) <> "?h=localhost"

testShortLinkContactAddress :: HasCallStack => TestParams -> IO ()
testShortLinkContactAddress =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    alice ##> "/ad"
    (shortLink, fullLink) <- getContactLinks alice True
    alice ##> ("/_connect plan 1 " <> shortLink)
    alice <## "contact address: own address"
    alice ##> ("/_connect plan 1 " <> slSimplexScheme shortLink)
    alice <## "contact address: own address"
    alice ##> ("/_connect plan 1 " <> fullLink)
    alice <## "contact address: own address"
    (alice, bob) `connectVia` shortLink
    bob ##> ("/_connect plan 1 " <> slSimplexScheme shortLink)
    bob <## "contact address: known contact alice"
    bob <## "use @alice <message> to send messages"
    (alice, cath) `connectVia` slSimplexScheme shortLink
    cath ##> ("/_connect plan 1 " <> shortLink)
    cath <## "contact address: known contact alice"
    cath <## "use @alice <message> to send messages"
    (alice, dan) `connectVia` fullLink
  where
    (alice, cc) `connectVia` cLink = do
      name <- userName cc
      sName <- showName cc
      cc ##> ("/_connect plan 1 " <> cLink)
      cc <## "contact address: ok to connect"
      _sLinkData <- getTermLine cc
      cc ##> ("/c " <> cLink)
      alice <#? cc
      alice ##> ("/ac " <> name)
      alice <## (sName <> ": accepting contact request, you can send messages to contact")
      concurrently_
        (cc <## "alice (Alice): contact is connected")
        (alice <## (sName <> ": contact is connected"))
      cc ##> ("/_connect plan 1 " <> cLink)
      cc <## "contact address: known contact alice"
      cc <## "use @alice <message> to send messages"

testShortLinkJoinGroup :: HasCallStack => TestParams -> IO ()
testShortLinkJoinGroup =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    threadDelay 100000
    alice ##> "/ad" -- create the address to test that it can co-exist with group link
    _ <- getContactLinks alice True
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> "/create link #team"
    (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
    alice ##> ("/_connect plan 1 " <> shortLink)
    alice <## "group link: own link for group #team"
    alice ##> ("/_connect plan 1 " <> slSimplexScheme shortLink)
    alice <## "group link: own link for group #team"
    alice ##> ("/_connect plan 1 " <> fullLink)
    alice <## "group link: own link for group #team"
    joinGroup alice bob shortLink
    bob ##> ("/_connect plan 1 " <> shortLink)
    bob <## "group link: known group #team"
    bob <## "use #team <message> to send messages"
    bob ##> ("/_connect plan 1 " <> slSimplexScheme shortLink)
    bob <## "group link: known group #team"
    bob <## "use #team <message> to send messages"
    joinGroup alice cath $ slSimplexScheme shortLink
    concurrentlyN_
      [ do
          bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
          bob <## "#team: new member cath is connected",
        cath <## "#team: member bob (Bob) is connected"
      ]
    cath ##> ("/_connect plan 1 " <> slSimplexScheme shortLink)
    cath <## "group link: known group #team"
    cath <## "use #team <message> to send messages"
    cath ##> ("/_connect plan 1 " <> shortLink)
    cath <## "group link: known group #team"
    cath <## "use #team <message> to send messages"
    joinGroup alice dan fullLink
    concurrentlyN_
      [ do
          bob <## "#team: alice added dan (Daniel) to the group (connecting...)"
          bob <## "#team: new member dan is connected",
        do
          cath <## "#team: alice added dan (Daniel) to the group (connecting...)"
          cath <## "#team: new member dan is connected",
        do
          dan <## "#team: member bob (Bob) is connected"
          dan <## "#team: member cath (Catherine) is connected"
      ]
    dan ##> ("/_connect plan 1 " <> fullLink)
    dan <## "group link: known group #team"
    dan <## "use #team <message> to send messages"
  where
    joinGroup alice cc link = do
      name <- userName cc
      sName <- showName cc
      cc ##> ("/_connect plan 1 " <> link)
      cc <## "group link: ok to connect"
      _sLinkData <- getTermLine cc
      cc ##> ("/c " <> link)
      cc <## "connection request sent!"
      alice <## (sName <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> name <> " joined the group"),
          do
            cc <## "#team: joining the group..."
            cc <## "#team: you joined the group"
        ]

testShortLinkInvitationPrepareContact :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationPrepareContact = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: known prepared contact alice"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice <# "bob> hello"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: known contact alice"
      bob <## "use @alice <message> to send messages"
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: known deleted contact alice"

testShortLinkInvitationImage :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationImage = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      bob ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations bob
      alice ##> ("/_connect plan 1 " <> shortLink)
      alice <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine alice
      alice ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      alice <## "bob: contact is prepared"
      alice ##> "/_connect contact @2 text hello"
      alice
        <### [ "bob: connection started",
                WithTime "@bob hello"
              ]
      bob <# "alice> hello"
      concurrently_
        (alice <## "bob (Bob): contact is connected")
        (bob <## "alice (Alice): contact is connected")
      bob <##> alice

testShortLinkInvitationConnectRetry :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationConnectRetry ps = testChatOpts2 opts' aliceProfile bobProfile test ps
  where
    test alice bob = do
      shortLink <- withSmpServer' serverCfg' $ do
        alice ##> "/_connect 1"
        (shortLink, fullLink) <- getInvitations alice
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "invitation link: ok to connect"
        contactSLinkData <- getTermLine bob
        bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
        bob <## "alice: contact is prepared"
        pure shortLink
      alice <## "server disconnected localhost ()"
      bob ##> "/_connect contact @2 text hello"
      bob <##. "smp agent error: BROKER"
      withSmpServer' serverCfg' $ do
        alice <## "server connected localhost ()"
        threadDelay 250000
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "invitation link: known prepared contact alice"
        bob ##> "/_connect contact @2 text hello"
        bob
          <### [ "alice: connection started",
                WithTime "@alice hello"
              ]
        alice <# "bob> hello"
        concurrently_
          (bob <## "alice (Alice): contact is connected")
          (alice <## "bob (Bob): contact is connected")
        alice <##> bob
      alice <## "server disconnected localhost (@bob)"
      bob <## "server disconnected localhost (@alice)"
    tmp = tmpPath ps
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          serverStoreCfg = persistentServerStoreCfg tmp
        }
    opts' =
      testOpts
        { coreOptions =
            testCoreOpts
              { smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7003"]
              }
        }

testShortLinkAddressPrepareContact :: HasCallStack => TestParams -> IO ()
testShortLinkAddressPrepareContact = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: known prepared contact alice"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice
        <### [ "bob (Bob) wants to connect to you!",
               WithTime "bob> hello"
             ]
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac i bob"
      alice <## "bad chat command: incognito not allowed for address with short link data"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: known contact alice"
      bob <## "use @alice <message> to send messages"
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      void $ getTermLine bob

testShortLinkAddressDeleteContact :: HasCallStack => TestParams -> IO ()
testShortLinkAddressDeleteContact = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True
      alice ##> "/pa on"
      alice <## "new contact address set"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice
        <### [ "bob (Bob) wants to connect to you!",
               WithTime "bob> hello"
             ]
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      threadDelay 250000
      bob ##> "/d alice entity"
      bob <## "alice: contact is deleted"
      alice <## "bob (Bob) deleted contact with you"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      void $ getTermLine bob

testShortLinkDeletedInvitation :: HasCallStack => TestParams -> IO ()
testShortLinkDeletedInvitation = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      alice @@@ [(":1","")]
      alice ##> "/_delete :1"
      alice <## "connection :1 deleted"
      bob ##> "/_connect contact @2"
      bob <##. "error: connection authorization failed"
      bob ##> "/_connect contact @2"
      bob <##. "error: connection authorization failed"

testShortLinkDeletedAddress :: HasCallStack => TestParams -> IO ()
testShortLinkDeletedAddress = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      alice ##> "/da"
      alice <## "Your chat address is deleted - accepted contacts will remain connected."
      alice <## "To create a new chat address use /ad"
      bob ##> "/_connect contact @2"
      bob <##. "error: connection authorization failed"
      bob ##> "/_connect contact @2"
      bob <##. "error: connection authorization failed"

testShortLinkAddressConnectRetry :: HasCallStack => TestParams -> IO ()
testShortLinkAddressConnectRetry ps =
  withNewTestChatOpts ps opts' "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps opts' "bob" bobProfile $ \bob -> do
      shortLink <- withSmpServer' serverCfg' $ do
        alice ##> "/ad"
        (shortLink, fullLink) <- getContactLinks alice True
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "contact address: ok to connect"
        contactSLinkData <- getTermLine bob
        bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
        bob <## "alice: contact is prepared"
        pure shortLink
      alice <## "server disconnected localhost ()"
      bob ##> "/_connect contact @2 text hello"
      bob <##. "smp agent error: BROKER"
      withSmpServer' serverCfg' $ do
        alice <## "server connected localhost ()"
        threadDelay 250000
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "contact address: known prepared contact alice"
        bob ##> "/_connect contact @2 text hello"
        bob
          <### [ "alice: connection started",
                WithTime "@alice hello"
              ]
        alice
          <### [ "bob (Bob) wants to connect to you!",
                WithTime "bob> hello"
              ]
        alice <## "to accept: /ac bob"
        alice <## "to reject: /rc bob (the sender will NOT be notified)"
        alice ##> "/ac bob"
        alice <## "bob (Bob): accepting contact request, you can send messages to contact"
        concurrently_
          (bob <## "alice (Alice): contact is connected")
          (alice <## "bob (Bob): contact is connected")
        alice <##> bob
      alice <## "server disconnected localhost (@bob)"
      bob <## "server disconnected localhost (@alice)"
  where
    tmp = tmpPath ps
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          serverStoreCfg = persistentServerStoreCfg tmp
        }
    opts' =
      testOpts
        { coreOptions =
            testCoreOpts
              { smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7003"]
              }
        }

testShortLinkAddressConnectRetryIncognito :: HasCallStack => TestParams -> IO ()
testShortLinkAddressConnectRetryIncognito ps =
  withNewTestChatOpts ps opts' "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps opts' "bob" bobProfile $ \bob -> do
      shortLink <- withSmpServer' serverCfg' $ do
        alice ##> "/ad"
        (shortLink, fullLink) <- getContactLinks alice True
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "contact address: ok to connect"
        contactSLinkData <- getTermLine bob
        bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
        bob <## "alice: contact is prepared"
        pure shortLink
      alice <## "server disconnected localhost ()"
      bob ##> "/_connect contact @2 incognito=on text hello"
      bob <##. "smp agent error: BROKER"
      bobIncognito <- withSmpServer' serverCfg' $ do
        alice <## "server connected localhost ()"
        threadDelay 250000
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "contact address: known prepared contact alice"
        bob ##> "/_connect contact @2 incognito=on text hello"
        bobIncognito <- getTermLine bob
        bob
          <### [ "alice: connection started incognito",
                 WithTime "i @alice hello"
               ]
        alice
          <### [ ConsoleString (bobIncognito <> " wants to connect to you!"),
                 WithTime (bobIncognito <> "> hello")
               ]
        alice <## ("to accept: /ac " <> bobIncognito)
        alice <## ("to reject: /rc " <> bobIncognito <> " (the sender will NOT be notified)")
        alice ##> ("/ac " <> bobIncognito)
        alice <## (bobIncognito <> ": accepting contact request, you can send messages to contact")
        concurrentlyN_
          [ do
              _ <- getTermLine bob
              bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
              bob <## "use /i alice to print out this incognito profile again",
            alice <## (bobIncognito <> ": contact is connected")
          ]
        alice #> ("@" <> bobIncognito <> " hi")
        bob ?<# "alice> hi"
        bob ?#> "@alice hey"
        alice <# (bobIncognito <> "> hey")
        pure bobIncognito
      alice <## ("server disconnected localhost (@" <> bobIncognito <> ")")
      bob <## "server disconnected localhost (@alice)"
  where
    tmp = tmpPath ps
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          serverStoreCfg = persistentServerStoreCfg tmp
        }
    opts' =
      testOpts
        { coreOptions =
            testCoreOpts
              { smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7003"]
              }
        }

testShortLinkAddressPrepareBusiness :: HasCallStack => TestParams -> IO ()
testShortLinkAddressPrepareBusiness = testChat3 businessProfile aliceProfile {fullName = "Alice @ Biz"} bobProfile test
  where
    test biz alice bob = do
      biz ##> "/ad"
      (shortLink, fullLink) <- getContactLinks biz True
      biz ##> "/auto_accept on business"
      biz <## "auto_accept on, business"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "#biz: group is prepared"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: known prepared business #biz"
      bob ##> "/_connect group #1"
      bob <## "#biz: connection started"
      biz <## "#bob (Bob): accepting business address request..."
      bob <## "#biz: joining the group..."
      -- the next command can be prone to race conditions
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: connecting to business #biz"
      biz <## "#bob: bob_1 joined the group"
      bob <## "#biz: you joined the group"
      biz #> "#bob hi"
      bob <# "#biz biz_1> hi"
      bob #> "#biz hello"
      biz <# "#bob bob_1> hello"

      connectUsers biz alice
      biz <##> alice
      biz ##> "/a #bob alice"
      biz <## "invitation to join the group #bob sent to alice"
      alice <## "#bob (Bob): biz invites you to join the group as member"
      alice <## "use /j bob to accept"
      alice ##> "/j bob"
      concurrentlyN_
        [ do
            alice <## "#bob: you joined the group"
            alice <### [WithTime "#bob biz> hi [>>]", WithTime "#bob bob_1> hello [>>]"]
            alice <## "#bob: member bob_1 (Bob) is connected",
          biz <## "#bob: alice joined the group",
          do
            bob <## "#biz: biz_1 added alice (Alice @ Biz) to the group (connecting...)"
            bob <## "#biz: new member alice is connected"
        ]
      alice #> "#bob hey"
      concurrently_
        (bob <# "#biz alice> hey")
        (biz <# "#bob alice> hey")
      bob #> "#biz hey there"
      concurrently_
        (alice <# "#bob bob_1> hey there")
        (biz <# "#bob bob_1> hey there")
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: known business #biz"
      bob <## "use #biz <message> to send messages"
      biz ##> "/d #bob"
      biz <## "#bob: you deleted the group"
      alice <## "#bob: biz deleted the group"
      alice <## "use /d #bob to delete the local copy of the group"
      bob <## "#biz: biz_1 deleted the group"
      bob <## "use /d #biz to delete the local copy of the group"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: ok to connect"
      void $ getTermLine bob

testBusinessAddressRequestMessage :: HasCallStack => TestParams -> IO ()
testBusinessAddressRequestMessage = testChat3 businessProfile aliceProfile {fullName = "Alice @ Biz"} bobProfile test
  where
    test biz alice bob = do
      biz ##> "/ad"
      (shortLink, fullLink) <- getContactLinks biz True
      biz ##> "/auto_accept on business text Welcome!"
      biz <## "auto_accept on, business"
      biz <## "auto reply:"
      biz <## "Welcome!"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "business address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "#biz: group is prepared"
      bob #$> ("/_get chat #1 count=100", chat, businessGroupFeatures <> [(0, "Welcome!")])
      bob ##> "/_connect group #1 text Hello!"
      bob
        <###
          [ "#biz: connection started",
            WithTime "#biz Hello!"
          ]
      biz <# "#bob bob_1> Hello!"
      biz <## "#bob (Bob): accepting business address request..."
      bob <## "#biz: joining the group..."
      biz <### ["#bob: bob_1 joined the group"]
      bob <### ["#biz: you joined the group"]
      -- Another member should receive history
      connectUsers biz alice
      biz ##> "/a bob alice"
      biz <## "invitation to join the group #bob sent to alice"
      alice <## "#bob (Bob): biz invites you to join the group as member"
      alice <## "use /j bob to accept"
      alice ##> "/j bob"
      concurrentlyN_
        [ alice
            <###
              [ "#bob: you joined the group",
                WithTime "#bob biz> Welcome! [>>]",
                WithTime "#bob bob_1> Hello! [>>]",
                "#bob: member bob_1 (Bob) is connected"
              ],
          biz <## "#bob: alice joined the group",
          do
            bob <## "#biz: biz_1 added alice (Alice @ Biz) to the group (connecting...)"
            bob <## "#biz: new member alice is connected"
        ]

testShortLinkPrepareGroup :: HasCallStack => TestParams -> IO ()
testShortLinkPrepareGroup = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#team: group is prepared"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: known prepared group #team"
      bob ##> "/_connect group #1"
      bob <## "#team: connection started"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
            bob <## "#team: member cath (Catherine) is connected",
          do
            cath <## "#team: alice added bob (Bob) to the group (connecting...)"
            cath <## "#team: new member bob is connected"
        ]
      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"
      bob ##> "/l #team"
      bob <## "#team: you left the group"
      bob <## "use /d #team to delete the group"
      alice <## "#team: bob left the group"
      cath <## "#team: bob left the group"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      void $ getTermLine bob

testShortLinkPrepareGroupReject :: HasCallStack => TestParams -> IO ()
testShortLinkPrepareGroupReject = testChatCfg3 cfg aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#team: group is prepared"
      bob ##> "/_connect group #1"
      bob <## "#team: connection started"
      alice <## "bob (Bob): rejecting request to join group #team, reason: GRRBlockedName"
      bob <## "#team: joining the group..."
      bob <## "#team: join rejected, reason: GRRBlockedName"

      alice #> "#team 1"
      cath <# "#team alice> 1"
      cath #> "#team 2"
      alice <# "#team cath> 2"

      -- rejected member can't send messages to group
      bob ##> "#team hello"
      bob <## "bad chat command: not current member"
    cfg = testCfg {chatHooks = defaultChatHooks {acceptMember = Just (\_ _ _ -> pure $ Left GRRBlockedName)}}

testGroupShortLinkWelcome :: HasCallStack => TestParams -> IO ()
testGroupShortLinkWelcome = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/set welcome #team Welcome!"
      alice <## "welcome message changed to:"
      alice <## "Welcome!"
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#team: group is prepared"
      bob #$> ("/_get chat #1 count=100", chat, groupFeaturesNoE2E <> [(0, "Welcome!")])
      threadDelay 1000000 -- TODO [short links] to compensate for rounding of timestamps of received messages
      bob ##> "/_connect group #1"
      bob <## "#team: connection started"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <### ["#team: you joined the group"]
        ]
      bob #$> ("/_get chat #1 count=100", chat, groupFeaturesNoE2E <> [(0, "Welcome!"), (0, e2eeInfoNoPQStr), (0, "connected")])
      alice #> "#team 1"
      bob <# "#team alice> 1"
      bob #> "#team 2"
      alice <# "#team bob> 2"

testShortLinkGroupRetry :: HasCallStack => TestParams -> IO ()
testShortLinkGroupRetry ps = testChatOpts2 opts' aliceProfile bobProfile test ps
  where
    test alice bob = do
      shortLink <- withSmpServer' serverCfg' $ do
        connectUsers alice bob
        alice ##> "/g team"
        alice <## "group #team is created"
        alice <## "to add members use /a team <name> or /create link #team"
        alice ##> "/create link #team"
        (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "group link: ok to connect"
        groupSLinkData <- getTermLine bob
        bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
        bob <## "#team: group is prepared"
        pure shortLink
      alice <## "server disconnected localhost (@bob)"
      bob <## "server disconnected localhost (@alice)"
      bob ##> "/_connect group #1"
      bob <##. "smp agent error: BROKER"
      withSmpServer' serverCfg' $ do
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "group link: known prepared group #team"
        alice <## "server connected localhost (@bob)"
        bob <## "server connected localhost (@alice)"
        threadDelay 250000
        bob ##> "/_connect group #1"
        bob <## "#team: connection started"
        alice <## "bob_1 (Bob): accepting request to join group #team..."
        concurrentlyN_
          [ alice <## "#team: bob_1 joined the group",
            do
              bob <## "#team: joining the group..."
              bob <## "#team: you joined the group"
          ]
        alice <## "contact and member are merged: bob, #team bob_1"
        alice <## "use @bob <message> to send messages"
        bob <## "contact and member are merged: alice, #team alice_1"
        bob <## "use @alice <message> to send messages"
        alice #> "#team 1"
        bob <# "#team alice> 1"
        bob #> "#team 2"
        alice <# "#team bob> 2"
      alice <## "server disconnected localhost (@bob)"
      bob <## "server disconnected localhost (@alice)"
    tmp = tmpPath ps
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          serverStoreCfg = persistentServerStoreCfg tmp
        }
    opts' =
      testOpts
        { coreOptions =
            testCoreOpts
              { smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7003"]
              }
        }

testShortLinkInvitationConnectPreparedContactIncognito :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationConnectPreparedContactIncognito = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> "/_connect contact @2 incognito=on"
      bobIncognito <- getTermLine bob
      bob <## "alice: connection started incognito"
      _ <- getTermLine bob
      concurrentlyN_
        [ alice <## (bobIncognito <> ": contact is connected"),
          do
            bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i alice to print out this incognito profile again")
        ]
      alice #> ("@" <> bobIncognito <> " hi")
      bob ?<# "alice> hi"
      bob ?#> "@alice hey"
      alice <# (bobIncognito <> "> hey")

testShortLinkAddressConnectPreparedContactIncognito :: HasCallStack => TestParams -> IO ()
testShortLinkAddressConnectPreparedContactIncognito = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> "/_connect contact @2 incognito=on"
      bobIncognito <- getTermLine bob
      bob <## "alice: connection started incognito"
      alice <## (bobIncognito <> " wants to connect to you!")
      alice <## ("to accept: /ac " <> bobIncognito)
      alice <## ("to reject: /rc " <> bobIncognito <> " (the sender will NOT be notified)")
      alice ##> ("/ac " <> bobIncognito)
      alice <## (bobIncognito <> ": accepting contact request, you can send messages to contact")
      _ <- getTermLine bob
      concurrentlyN_
        [ alice <## (bobIncognito <> ": contact is connected"),
          do
            bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i alice to print out this incognito profile again")
        ]
      alice #> ("@" <> bobIncognito <> " hi")
      bob ?<# "alice> hi"
      bob ?#> "@alice hey"
      alice <# (bobIncognito <> "> hey")

testShortLinkChangePreparedContactUser :: HasCallStack => TestParams -> IO ()
testShortLinkChangePreparedContactUser = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      bob ##> "/create user robert"
      showActiveUser bob "robert"
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"

      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"

      -- 2 ids are for "user contacts", 2 ids are for second user contact cards, so alice is id 5
      bob ##> "/_set contact user @5 2"
      bob <## "contact alice changed from user bob to user robert"

      bob ##> "/user robert"
      showActiveUser bob "robert"

      bob ##> "/_connect contact @5 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice <# "robert> hello"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "robert: contact is connected")

      alice <##> bob

      alice @@@ [("@robert", "hey")]
      alice `hasContactProfiles` ["alice", "robert"]
      bob #$> ("/_get chats 2 pcc=on", chats, [("@alice", "hey"), ("@Ask SimpleX Team", ""), ("@SimpleX Status", ""), ("*", "")])
      bob `hasContactProfiles` ["robert", "alice", "Ask SimpleX Team", "SimpleX Status"]
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"
      bob @@@ []
      bob `hasContactProfiles` ["bob"]

testShortLinkChangePreparedContactUserDuplicate :: HasCallStack => TestParams -> IO ()
testShortLinkChangePreparedContactUserDuplicate = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      bob ##> "/create user robert"
      showActiveUser bob "robert"

      connectUsers alice bob
      alice <##> bob

      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"

      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alice: contact is prepared"

      -- 2 ids are for "user contacts"
      -- 2 ids are for second user contact cards
      -- 1 for second user's alice
      -- so this alice is id 6
      bob ##> "/_set contact user @6 2"
      bob <## "contact alice changed from user bob to user robert, new local name: alice_1"

      bob ##> "/user robert"
      showActiveUser bob "robert"

      bob ##> "/_connect contact @6 text hello"
      bob
        <### [ "alice_1: connection started",
               WithTime "@alice_1 hello"
             ]
      alice <# "robert_1> hello"
      concurrently_
        (bob <## "alice_1 (Alice): contact is connected")
        (alice <## "robert_1: contact is connected")

      alice #> "@robert_1 hi"
      bob <# "alice_1> hi"
      bob #> "@alice_1 hey"
      alice <# "robert_1> hey"

      alice <##> bob

      alice @@@ [("@robert", "hey"), ("@robert_1", "hey")]
      alice `hasContactProfiles` ["alice", "robert", "robert"]
      bob #$> ("/_get chats 2 pcc=on", chats, [("@alice", "hey"), ("@alice_1", "hey"), ("@Ask SimpleX Team", ""), ("@SimpleX Status", ""), ("*", "")])
      bob `hasContactProfiles` ["robert", "alice", "alice", "Ask SimpleX Team", "SimpleX Status"]
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"
      bob @@@ []
      bob `hasContactProfiles` ["bob"]

testShortLinkConnectPreparedGroupIncognito :: HasCallStack => TestParams -> IO ()
testShortLinkConnectPreparedGroupIncognito = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#team: group is prepared"
      bob ##> "/_connect group #1 incognito=on"
      bobIncognito <- getTermLine bob
      bob <## "#team: connection started incognito"
      alice <## (bobIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> bobIncognito <> " joined the group"),
          do
            bob <## "#team: joining the group..."
            bob <## ("#team: you joined the group incognito as " <> bobIncognito)
            bob <## "#team: member cath (Catherine) is connected",
          do
            cath <## ("#team: alice added " <> bobIncognito <> " to the group (connecting...)")
            cath <## ("#team: new member " <> bobIncognito <> " is connected")
        ]

      alice #> "#team 1"
      bob ?<# "#team alice> 1"
      cath <# "#team alice> 1"

      bob ?#> "#team 2"
      [alice, cath] *<# ("#team " <> bobIncognito <> "> 2")

      cath #> "#team 3"
      alice <# "#team cath> 3"
      bob ?<# "#team cath> 3"

testShortLinkChangePreparedGroupUser :: HasCallStack => TestParams -> IO ()
testShortLinkChangePreparedGroupUser = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True

      bob ##> "/create user robert"
      showActiveUser bob "robert"
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#team: group is prepared"

      bob ##> "/_set group user #1 2"
      bob <## "group #team changed from user bob to user robert"

      bob ##> "/user robert"
      showActiveUser bob "robert"

      bob ##> "/_connect group #1"
      bob <## "#team: connection started"
      alice <## "robert: accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: robert joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
            bob <## "#team: member cath (Catherine) is connected",
          do
            cath <## "#team: alice added robert to the group (connecting...)"
            cath <## "#team: new member robert is connected"
        ]

      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team robert> 2"
      threadDelay 1000000
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

      alice @@@ [("#team", "3"), ("@cath","sent invitation to join group team as admin")]
      alice `hasContactProfiles` ["alice", "cath", "robert"]
      bob #$> ("/_get chats 2 pcc=on", chats, [("#team", "3"), ("@Ask SimpleX Team", ""), ("@SimpleX Status", ""), ("*", "")])
      bob `hasContactProfiles` ["robert", "alice", "cath", "Ask SimpleX Team", "SimpleX Status"]
      cath @@@ [("#team", "3"), ("@alice","received invitation to join group team as admin")]
      cath `hasContactProfiles` ["cath", "alice", "robert"]
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"
      bob @@@ []
      bob `hasContactProfiles` ["bob"]

testShortLinkChangePreparedGroupUserDuplicate :: HasCallStack => TestParams -> IO ()
testShortLinkChangePreparedGroupUserDuplicate = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True

      bob ##> "/create user robert"
      showActiveUser bob "robert"

      bob ##> ("/_connect plan 2 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData1 <- getTermLine bob
      bob ##> ("/_prepare group 2 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData1)
      bob <## "#team: group is prepared"

      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData2 <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData2)
      bob <## "#team: group is prepared"

      bob ##> "/_set group user #2 2"
      bob <## "group #team changed from user bob to user robert, new local name: #team_1"

      bob ##> "/user robert"
      showActiveUser bob "robert"

      bob ##> "/_connect group #2"
      bob <## "#team_1: connection started"
      alice <## "robert: accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: robert joined the group",
          do
            bob <## "#team_1: joining the group..."
            bob <## "#team_1: you joined the group"
            bob <## "#team_1: member cath (Catherine) is connected",
          do
            cath <## "#team: alice added robert to the group (connecting...)"
            cath <## "#team: new member robert is connected"
        ]

      alice #> "#team 1"
      bob <# "#team_1 alice> 1"
      cath <# "#team alice> 1"

      bob #> "#team_1 2"
      [alice, cath] *<# "#team robert> 2"

      cath #> "#team 3"
      alice <# "#team cath> 3"
      bob <# "#team_1 cath> 3"

      -- also connect to the first prepared instance of group
      bob ##> "/_connect group #1"
      bob <## "#team: connection started"
      alice <## "robert_1: accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: robert_1 joined the group",
          bob
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   "#team: member cath_1 (Catherine) is connected",
                   "#team: member robert_2 is connected",
                   WithTime "#team alice_1> 1 [>>]",
                   WithTime "#team robert_2> 2 [>>]",
                   WithTime "#team cath_1> 3 [>>]",
                   -- for previously joined instance of group:
                   "#team_1: alice added robert_1 to the group (connecting...)",
                   "#team_1: new member robert_1 is connected"
                 ],
          do
            cath <## "#team: alice added robert_1 to the group (connecting...)"
            cath <## "#team: new member robert_1 is connected"
        ]

      alice #> "#team 4"
      bob
        <### [ WithTime "#team_1 alice> 4",
               WithTime "#team alice_1> 4"
             ]
      cath <# "#team alice> 4"

      bob #> "#team_1 5"
      [alice, cath] *<# "#team robert> 5"
      bob <# "#team robert_2> 5"

      bob #> "#team 6"
      [alice, cath] *<# "#team robert_1> 6"
      bob <# "#team_1 robert_1> 6"

      threadDelay 1000000
      cath #> "#team 7"
      alice <# "#team cath> 7"
      bob
        <### [ WithTime "#team_1 cath> 7",
               WithTime "#team cath_1> 7"
             ]

      alice @@@ [("#team", "7"), ("@cath","sent invitation to join group team as admin")]
      alice `hasContactProfiles` ["alice", "cath", "robert", "robert"]
      bob `hasContactProfiles` ["robert", "robert", "robert", "alice", "alice", "cath", "cath", "Ask SimpleX Team", "SimpleX Status"]
      cath @@@ [("#team", "7"), ("@alice","received invitation to join group team as admin")]
      cath `hasContactProfiles` ["cath", "alice", "robert", "robert"]
      bob ##> "/user bob"
      showActiveUser bob "bob (Bob)"
      bob @@@ []
      bob `hasContactProfiles` ["bob"]

testShortLinkInvitationSetIncognito :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationSetIncognito = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/_connect 1"
      (shortLink, fullLink) <- getInvitations alice

      alice ##> "/_set incognito :1 on"
      aliceIncognito <- getTermLine alice
      alice <## "connection 1 changed to incognito"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## (aliceIncognito <> ": contact is prepared")
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ ConsoleString (aliceIncognito <> ": connection started"),
               WithTime ("@" <> aliceIncognito <> " hello")
             ]
      alice ?<# "bob> hello"
      _ <- getTermLine alice
      concurrentlyN_
        [ bob <## (aliceIncognito <> ": contact is connected"),
          do
            alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
            alice <## "use /i bob to print out this incognito profile again"
        ]
      alice ?#> ("@bob hi")
      bob <# (aliceIncognito <> "> hi")
      bob #> ("@" <> aliceIncognito <> " hey")
      alice ?<# ("bob> hey")

testShortLinkInvitationChangeUser :: HasCallStack => TestParams -> IO ()
testShortLinkInvitationChangeUser = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice)"

      alice ##> "/_connect 1"
      _ <- getInvitations alice

      alice ##> "/_set conn user :1 2"
      alice <## "connection 1 changed from user alice to user alisa, new link:"
      alice <## ""
      shortLink <- getTermLine alice
      alice <## ""
      alice <## "The invitation link for old clients:"
      fullLink <- getTermLine alice
      alice ##> "/user alisa"
      showActiveUser alice "alisa"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "invitation link: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alisa: contact is prepared"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alisa: connection started",
               WithTime "@alisa hello"
             ]
      alice <# "bob> hello"
      concurrently_
        (bob <## "alisa: contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

testShortLinkAddressChangeProfile :: HasCallStack => TestParams -> IO ()
testShortLinkAddressChangeProfile = testChat2 aliceProfile bobProfile test
  where
    test alice bob = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> contactSLinkData)
      bob <## "alisa: contact is prepared"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alisa: connection started",
               WithTime "@alisa hello"
             ]
      alice
        <### [ "bob (Bob) wants to connect to you!",
               WithTime "bob> hello"
             ]
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac i bob"
      alice <## "bad chat command: incognito not allowed for address with short link data"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alisa: contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob

testShortLinkAddressChangeAutoReply :: HasCallStack => TestParams -> IO ()
testShortLinkAddressChangeAutoReply = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True

      alice ##> "/auto_accept on incognito=off text welcome!"
      alice <## "auto_accept on"
      alice <## "auto reply:"
      alice <## "welcome!"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      bobContactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> bobContactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice <# "bob> hello"
      alice <## "bob (Bob): accepting contact request..."
      alice <## "bob (Bob): you can send messages to contact"
      concurrently_
        (bob <### ["alice (Alice): contact is connected"])
        (alice <### ["bob (Bob): contact is connected"])
      alice <##> bob
      alice ##> "/auto_accept on incognito=off"
      alice <## "auto_accept on"

      cath ##> ("/_connect plan 1 " <> shortLink)
      cath <## "contact address: ok to connect"
      cathContactSLinkData <- getTermLine cath
      cath ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " " <> cathContactSLinkData)
      cath <## "alice: contact is prepared"
      cath ##> "/_connect contact @2 text hello"
      cath
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice <# "cath> hello"
      alice <## "cath (Catherine): accepting contact request..."
      alice <## "cath (Catherine): you can send messages to contact"
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice <##> cath

testShortLinkGroupChangeProfile :: HasCallStack => TestParams -> IO ()
testShortLinkGroupChangeProfile = testChat3 aliceProfile bobProfile cathProfile test
  where
    test alice bob cath = do
      createGroup2 "team" alice cath
      alice ##> "/create link #team"
      (shortLink, fullLink) <- getGroupLinks alice "team" GRMember True

      alice ##> "/gp team club"
      alice <## "changed to #club"
      cath <## "alice updated group #team:"
      cath <## "changed to #club"

      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "group link: ok to connect"
      groupSLinkData <- getTermLine bob
      bob ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " " <> groupSLinkData)
      bob <## "#club: group is prepared"
      bob ##> "/_connect group #1"
      bob <## "#club: connection started"
      alice <## "bob (Bob): accepting request to join group #club..."
      concurrentlyN_
        [ alice <## "#club: bob joined the group",
          do
            bob <## "#club: joining the group..."
            bob <## "#club: you joined the group"
            bob <## "#club: member cath (Catherine) is connected",
          do
            cath <## "#club: alice added bob (Bob) to the group (connecting...)"
            cath <## "#club: new member bob is connected"
        ]
      alice #> "#club 1"
      [bob, cath] *<# "#club alice> 1"
      bob #> "#club 2"
      [alice, cath] *<# "#club bob> 2"
      cath #> "#club 3"
      [alice, bob] *<# "#club cath> 3"
