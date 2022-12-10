{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChatTests where

import ChatClient
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String
import qualified Data.Text as T
import Simplex.Chat.Call
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), InlineFilesConfig (..), defaultInlineFilesConfig)
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Store (getUserContactProfiles)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.SQLite (withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (unlessM)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

defaultPrefs :: Maybe Preferences
defaultPrefs = Just $ toChatPrefs defaultChatPrefs

aliceProfile :: Profile
aliceProfile = Profile {displayName = "alice", fullName = "Alice", image = Nothing, preferences = defaultPrefs}

bobProfile :: Profile
bobProfile = Profile {displayName = "bob", fullName = "Bob", image = Just (ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAKHGlDQ1BJQ0MgUHJvZmlsZQAASImFVgdUVNcWve9Nb7QZeu9NehtAem/Sq6gMQ28OQxWxgAQjEFFEREARNFQFg1KjiIhiIQgoYA9IEFBisCAq6OQNJNH4//r/zDpz9ttzz7n73ffWmg0A6QCDxYqD+QCIT0hmezlYywQEBsngngEYCAIy0AC6DGYSy8rDwxUg8Xf9d7wbAxC33tHgzvrP3/9nCISFJzEBgIIRTGey2MkILkawT1oyi4tnEUxjI6IQvMLFkauYqxjQQtewwuoaHy8bBNMBwJMZDHYkAERbhJdJZUYic4hhCNZOCItOQDB3vjkzioFwxLsIXhcRl5IOAImrRzs+fivCk7QRrIL0shAcwNUW+tX8yH/tFfrPXgxG5D84Pi6F+dc9ck+HHJ7g641UMSQlQATQBHEgBaQDGcACbLAVYaIRJhx5Dv+9j77aZ4OsZIFtSEc0iARRIBnpt/9qlvfqpGSQBhjImnCEcUU+NtxnujZy4fbqVEiU/wuXdQyA9S0cDqfzC+e2F4DzyLkSB79wyi0A8KoBcL2GmcJOXePQ3C8MIAJeQAOiQArIAxXuWwMMgSmwBHbAGbgDHxAINgMmojceUZUGMkEWyAX54AA4DMpAJTgJ6sAZ0ALawQVwGVwDt8AQGAUPwQSYBi/AAngHliEIwkEUiAqJQtKQIqQO6UJ0yByyg1whLygQCoEioQQoBcqE9kD5UBFUBlVB9dBPUCd0GboBDUP3oUloDnoNfYRRMBmmwZKwEqwF02Er2AX2gTfBkXAinAHnwPvhUrgaPg23wZfhW/AoPAG/gBdRAEVCCaFkURooOsoG5Y4KQkWg2KidqDxUCaoa1YTqQvWj7qAmUPOoD2gsmoqWQWugTdGOaF80E52I3okuQJeh69Bt6D70HfQkegH9GUPBSGDUMSYYJ0wAJhKThsnFlGBqMK2Yq5hRzDTmHRaLFcIqY42wjthAbAx2O7YAewzbjO3BDmOnsIs4HE4Up44zw7njGLhkXC7uKO407hJuBDeNe48n4aXxunh7fBA+AZ+NL8E34LvxI/gZ/DKBj6BIMCG4E8II2wiFhFOELsJtwjRhmchPVCaaEX2IMcQsYimxiXiV+Ij4hkQiyZGMSZ6kaNJuUinpLOk6aZL0gSxAViPbkIPJKeT95FpyD/k++Q2FQlGiWFKCKMmU/ZR6yhXKE8p7HiqPJo8TTxjPLp5ynjaeEZ6XvAReRV4r3s28GbwlvOd4b/PO8xH4lPhs+Bh8O/nK+Tr5xvkW+an8Ovzu/PH8BfwN/Df4ZwVwAkoCdgJhAjkCJwWuCExRUVR5qg2VSd1DPUW9Sp2mYWnKNCdaDC2fdoY2SFsQFBDUF/QTTBcsF7woOCGEElISchKKEyoUahEaE/ooLClsJRwuvE+4SXhEeElEXMRSJFwkT6RZZFTko6iMqJ1orOhB0XbRx2JoMTUxT7E0seNiV8XmxWnipuJM8TzxFvEHErCEmoSXxHaJkxIDEouSUpIOkizJo5JXJOelhKQspWKkiqW6peakqdLm0tHSxdKXpJ/LCMpYycTJlMr0ySzISsg6yqbIVskOyi7LKcv5ymXLNcs9lifK0+Uj5Ivle+UXFKQV3BQyFRoVHigSFOmKUYpHFPsVl5SUlfyV9iq1K80qiyg7KWcoNyo/UqGoWKgkqlSr3FXFqtJVY1WPqQ6pwWoGalFq5Wq31WF1Q/Vo9WPqw+sw64zXJayrXjeuQdaw0kjVaNSY1BTSdNXM1mzXfKmloBWkdVCrX+uztoF2nPYp7Yc6AjrOOtk6XTqvddV0mbrlunf1KHr2erv0OvRe6avrh+sf179nQDVwM9hr0GvwydDIkG3YZDhnpGAUYlRhNE6n0T3oBfTrxhhja+NdxheMP5gYmiSbtJj8YaphGmvaYDq7Xnl9+PpT66fM5MwYZlVmE+Yy5iHmJ8wnLGQtGBbVFk8t5S3DLGssZ6xUrWKsTlu9tNa2Zlu3Wi/ZmNjssOmxRdk62ObZDtoJ2Pnaldk9sZezj7RvtF9wMHDY7tDjiHF0cTzoOO4k6cR0qndacDZy3uHc50J28XYpc3nqqubKdu1yg92c3Q65PdqguCFhQ7s7cHdyP+T+2EPZI9HjZ0+sp4dnueczLx2vTK9+b6r3Fu8G73c+1j6FPg99VXxTfHv9eP2C/er9lvxt/Yv8JwK0AnYE3AoUC4wO7AjCBfkF1QQtbrTbeHjjdLBBcG7w2CblTembbmwW2xy3+eIW3i2MLedCMCH+IQ0hKwx3RjVjMdQptCJ0gWnDPMJ8EWYZVhw2F24WXhQ+E2EWURQxG2kWeShyLsoiqiRqPtomuiz6VYxjTGXMUqx7bG0sJ84/rjkeHx8S35kgkBCb0LdVamv61mGWOiuXNZFokng4cYHtwq5JgpI2JXUk05A/0oEUlZTvUiZTzVPLU9+n+aWdS+dPT0gf2Ka2bd+2mQz7jB+3o7czt/dmymZmZU7usNpRtRPaGbqzd5f8rpxd07sddtdlEbNis37J1s4uyn67x39PV45kzu6cqe8cvmvM5cll547vNd1b+T36++jvB/fp7Tu673NeWN7NfO38kvyVAmbBzR90fij9gbM/Yv9goWHh8QPYAwkHxg5aHKwr4i/KKJo65HaorVimOK/47eEth2+U6JdUHiEeSTkyUepa2nFU4eiBoytlUWWj5dblzRUSFfsqlo6FHRs5bnm8qVKyMr/y44noE/eqHKraqpWqS05iT6aefHbK71T/j/Qf62vEavJrPtUm1E7UedX11RvV1zdINBQ2wo0pjXOng08PnbE909Gk0VTVLNScfxacTTn7/KeQn8ZaXFp6z9HPNZ1XPF/RSm3Na4PatrUttEe1T3QEdgx3Onf2dpl2tf6s+XPtBdkL5RcFLxZ2E7tzujmXMi4t9rB65i9HXp7q3dL78ErAlbt9nn2DV12uXr9mf+1Kv1X/petm1y/cMLnReZN+s/2W4a22AYOB1l8MfmkdNBxsu210u2PIeKhreP1w94jFyOU7tneu3XW6e2t0w+jwmO/YvfHg8Yl7Yfdm78fdf/Ug9cHyw92PMI/yHvM9Lnki8aT6V9VfmycMJy5O2k4OPPV++nCKOfXit6TfVqZznlGelcxIz9TP6s5emLOfG3q+8fn0C9aL5fnc3/l/r3ip8vL8H5Z/DCwELEy/Yr/ivC54I/qm9q3+295Fj8Un7+LfLS/lvRd9X/eB/qH/o//HmeW0FdxK6SfVT12fXT4/4sRzOCwGm7FqBVBIwhERALyuBYASCAB1CPEPG9f8119+BvrK2fyNwVndL5jhvubRVsMQgCakeCFp04OsQ1LJEgAe5NodqT6WANbT+yf/iqQIPd21PXgaAcDJcjivtwJAQHLFgcNZ9uBwPlUgYhHf1z37f7V9g9e8ITewiP88wfWIYET6HPg21nzjV2fybQVcxfrg2/onng/F50lD/ccAAAA4ZVhJZk1NACoAAAAIAAGHaQAEAAAAAQAAABoAAAAAAAKgAgAEAAAAAQAAABigAwAEAAAAAQAAABgAAAAAwf1XlwAAAaNJREFUSA3FlT1LA0EQQBN/gYUYRTksJZVgEbCR/D+7QMr8ABtttBBCsLGzsLG2sxaxED/ie4d77u0dyaE5HHjczn7MzO7M7nU6/yXz+bwLhzCCjTQO+rZhDH3opuNLdRYN4RHe4RIKJ7R34Ro+4AEGSw2mE1iUwT18gpI74WvkGlccu4XNdH0jnYU7cAUacidn37qR23cOxc4aGU0nYUAn7iSWEHkz46w0ocdQu1X6B/AMQZ5o7KfBqNOfwRH8JB7FajGhnmcpKvQe3MEbvILiDm5gPXaCHnZr4vvFGMoEKudKn8YvQIOOe+YzCPop7dwJ3zRfJ7GDuso4YJGRa0yZgg4tUaNXdGrbuZWKKxzYYEJc2xp9AUUjGt8KC2jvgYadF8+10vJyDnNLXwbdiWUZi0fUK01Eoc+AZhCLZVzK4Vq6sDUdz+0dEcbbTTIOJmAyTVhx/WmvrExbv2jtPhWLKodjCtefZiEeZeVZWWSndgwj6fVf3XON8Qwq15++uoqrfYVrow6dGBpCq79ME291jaB0/Q2CPncyht/99MNO/vr9AqW/CGi8sJqbAAAAAElFTkSuQmCC"), preferences = defaultPrefs}

cathProfile :: Profile
cathProfile = Profile {displayName = "cath", fullName = "Catherine", image = Nothing, preferences = defaultPrefs}

danProfile :: Profile
danProfile = Profile {displayName = "dan", fullName = "Daniel", image = Nothing, preferences = defaultPrefs}

chatTests :: Spec
chatTests = do
  describe "direct messages" $ do
    describe "add contact and send/receive message" testAddContact
    it "deleting contact deletes profile" testDeleteContactDeletesProfile
    it "direct message quoted replies" testDirectMessageQuotedReply
    it "direct message update" testDirectMessageUpdate
    it "direct message delete" testDirectMessageDelete
  describe "chat groups" $ do
    describe "add contacts, create group and send/receive messages" testGroup
    it "add contacts, create group and send/receive messages, check messages" testGroupCheckMessages
    it "create and join group with 4 members" testGroup2
    it "create and delete group" testGroupDelete
    it "create group with the same displayName" testGroupSameName
    it "invitee delete group when in status invited" testGroupDeleteWhenInvited
    it "re-add member in status invited" testGroupReAddInvited
    it "re-add member in status invited, change role" testGroupReAddInvitedChangeRole
    it "delete contact before they accept group invitation, contact joins group" testGroupDeleteInvitedContact
    it "member profile is kept when deleting group if other groups have this member" testDeleteGroupMemberProfileKept
    it "remove contact from group and add again" testGroupRemoveAdd
    it "list groups containing group invitations" testGroupList
    it "group message quoted replies" testGroupMessageQuotedReply
    it "group message update" testGroupMessageUpdate
    it "group message delete" testGroupMessageDelete
    it "update group profile" testUpdateGroupProfile
    it "update member role" testUpdateMemberRole
    it "unused contacts are deleted after all their groups are deleted" testGroupDeleteUnusedContacts
    it "group description is shown as the first message to new members" testGroupDescription
  describe "async group connections" $ do
    xit "create and join group when clients go offline" testGroupAsync
  describe "user profiles" $ do
    it "update user profile and notify contacts" testUpdateProfile
    it "update user profile with image" testUpdateProfileImage
  describe "sending and receiving files" $ do
    describe "send and receive file" $ fileTestMatrix2 runTestFileTransfer
    it "send and receive file inline (without accepting)" testInlineFileTransfer
    it "send and receive small file inline (default config)" testSmallInlineFileTransfer
    it "small file sent without acceptance is ignored in terminal by default" testSmallInlineFileIgnored
    it "receive file inline with inline=on option" testReceiveInline
    describe "send and receive a small file" $ fileTestMatrix2 runTestSmallFileTransfer
    describe "sender cancelled file transfer before transfer" $ fileTestMatrix2 runTestFileSndCancelBeforeTransfer
    it "sender cancelled file transfer during transfer" testFileSndCancelDuringTransfer
    it "recipient cancelled file transfer" testFileRcvCancel
    describe "send and receive file to group" $ fileTestMatrix3 runTestGroupFileTransfer
    it "send and receive file inline to group (without accepting)" testInlineGroupFileTransfer
    it "send and receive small file inline to group (default config)" testSmallInlineGroupFileTransfer
    it "small file sent without acceptance is ignored in terminal by default" testSmallInlineGroupFileIgnored
    describe "sender cancelled group file transfer before transfer" $ fileTestMatrix3 runTestGroupFileSndCancelBeforeTransfer
  describe "messages with files" $ do
    describe "send and receive message with file" $ fileTestMatrix2 runTestMessageWithFile
    it "send and receive image" testSendImage
    it "files folder: send and receive image" testFilesFoldersSendImage
    it "files folder: sender deleted file during transfer" testFilesFoldersImageSndDelete
    it "files folder: recipient deleted file during transfer" testFilesFoldersImageRcvDelete
    it "send and receive image with text and quote" testSendImageWithTextAndQuote
    describe "send and receive image to group" testGroupSendImage
    it "send and receive image with text and quote to group" testGroupSendImageWithTextAndQuote
  describe "user contact link" $ do
    describe "create and connect via contact link" testUserContactLink
    it "auto accept contact requests" testUserContactLinkAutoAccept
    it "deduplicate contact requests" testDeduplicateContactRequests
    it "deduplicate contact requests with profile change" testDeduplicateContactRequestsProfileChange
    it "reject contact and delete contact link" testRejectContactAndDeleteUserContact
    it "delete connection requests when contact link deleted" testDeleteConnectionRequests
    it "auto-reply message" testAutoReplyMessage
    it "auto-reply message in incognito" testAutoReplyMessageInIncognito
  describe "incognito mode" $ do
    it "connect incognito via invitation link" testConnectIncognitoInvitationLink
    it "connect incognito via contact address" testConnectIncognitoContactAddress
    it "accept contact request incognito" testAcceptContactRequestIncognito
    it "join group incognito" testJoinGroupIncognito
    it "can't invite contact to whom user connected incognito to a group" testCantInviteContactIncognito
    it "can't see global preferences update" testCantSeeGlobalPrefsUpdateIncognito
    it "deleting contact first, group second deletes incognito profile" testDeleteContactThenGroupDeletesIncognitoProfile
    it "deleting group first, contact second deletes incognito profile" testDeleteGroupThenContactDeletesIncognitoProfile
  describe "group links" $ do
    it "create group link, join via group link" testGroupLink
    it "delete group, re-join via same link" testGroupLinkDeleteGroupRejoin
    it "sending message to contact created via group link marks it used" testGroupLinkContactUsed
    it "create group link, join via group link - incognito membership" testGroupLinkIncognitoMembership
    it "unused host contact is deleted after all groups with it are deleted" testGroupLinkUnusedHostContactDeleted
    it "leaving groups with unused host contacts deletes incognito profiles" testGroupLinkIncognitoUnusedHostContactsDeleted
  describe "contact aliases" $ do
    it "set contact alias" testSetAlias
    it "set connection alias" testSetConnectionAlias
  describe "preferences" $ do
    it "set contact preferences" testSetContactPrefs
    it "update group preferences" testUpdateGroupPrefs
    it "allow full deletion to contact" testAllowFullDeletionContact
    it "allow full deletion to group" testAllowFullDeletionGroup
    it "prohibit direct messages to group members" testProhibitDirectMessages
  describe "SMP servers" $ do
    it "get and set SMP servers" testGetSetSMPServers
    it "test SMP server connection" testTestSMPServerConnection
  describe "async connection handshake" $ do
    it "connect when initiating client goes offline" testAsyncInitiatingOffline
    it "connect when accepting client goes offline" testAsyncAcceptingOffline
    describe "connect, fully asynchronous (when clients are never simultaneously online)" $ do
      it "v2" testFullAsync
  -- it "v1" testFullAsyncV1
  -- it "v1 to v2" testFullAsyncV1toV2
  -- it "v2 to v1" testFullAsyncV2toV1
  describe "async sending and receiving files" $ do
    xdescribe "send and receive file, fully asynchronous" $ do
      it "v2" testAsyncFileTransfer
      it "v1" testAsyncFileTransferV1
    xit "send and receive file to group, fully asynchronous" testAsyncGroupFileTransfer
  describe "webrtc calls api" $ do
    it "negotiate call" testNegotiateCall
  describe "maintenance mode" $ do
    it "start/stop/export/import chat" testMaintenanceMode
    it "export/import chat with files" testMaintenanceModeWithFiles
    it "encrypt/decrypt database" testDatabaseEncryption
  describe "mute/unmute messages" $ do
    it "mute/unmute contact" testMuteContact
    it "mute/unmute group" testMuteGroup
  describe "chat item expiration" $ do
    it "set chat item TTL" testSetChatItemTTL
  describe "queue rotation" $ do
    it "switch contact to a different queue" testSwitchContact
    it "switch group member to a different queue" testSwitchGroupMember
  describe "connection verification code" $ do
    it "verificationCode function converts ByteString to series of digits" $
      verificationCode (C.sha256Hash "abcd") `shouldBe` "61889 38426 63934 09576 96390 79389 84124 85253 63658 69469 70853 37788 95900 68296 20156 25"
    it "sameVerificationCode function should ignore spaces" $
      sameVerificationCode "123 456 789" "12345 6789" `shouldBe` True
    it "mark contact verified" testMarkContactVerified
    it "mark group member verified" testMarkGroupMemberVerified

versionTestMatrix2 :: (TestCC -> TestCC -> IO ()) -> Spec
versionTestMatrix2 runTest = do
  it "v2" $ testChat2 aliceProfile bobProfile runTest
  it "v1" $ testChatCfg2 testCfgV1 aliceProfile bobProfile runTest
  it "v1 to v2" $ runTestCfg2 testCfg testCfgV1 runTest
  it "v2 to v1" $ runTestCfg2 testCfgV1 testCfg runTest

versionTestMatrix3 :: (TestCC -> TestCC -> TestCC -> IO ()) -> Spec
versionTestMatrix3 runTest = do
  it "v2" $ testChat3 aliceProfile bobProfile cathProfile runTest

-- it "v1" $ testChatCfg3 testCfgV1 aliceProfile bobProfile cathProfile runTest
-- it "v1 to v2" $ runTestCfg3 testCfg testCfgV1 testCfgV1 runTest
-- it "v2+v1 to v2" $ runTestCfg3 testCfg testCfg testCfgV1 runTest
-- it "v2 to v1" $ runTestCfg3 testCfgV1 testCfg testCfg runTest
-- it "v2+v1 to v1" $ runTestCfg3 testCfgV1 testCfg testCfgV1 runTest

inlineCfg :: Integer -> ChatConfig
inlineCfg n = testCfg {inlineFiles = defaultInlineFilesConfig {sendChunks = 0, offerChunks = n, receiveChunks = n}}

fileTestMatrix2 :: (TestCC -> TestCC -> IO ()) -> Spec
fileTestMatrix2 runTest = do
  it "via connection" $ runTestCfg2 viaConn viaConn runTest
  it "inline (accepting)" $ runTestCfg2 inline inline runTest
  it "via connection (inline offered)" $ runTestCfg2 inline viaConn runTest
  it "via connection (inline supported)" $ runTestCfg2 viaConn inline runTest
  where
    inline = inlineCfg 100
    viaConn = inlineCfg 0

fileTestMatrix3 :: (TestCC -> TestCC -> TestCC -> IO ()) -> Spec
fileTestMatrix3 runTest = do
  it "via connection" $ runTestCfg3 viaConn viaConn viaConn runTest
  it "inline" $ runTestCfg3 inline inline inline runTest
  it "via connection (inline offered)" $ runTestCfg3 inline viaConn viaConn runTest
  it "via connection (inline supported)" $ runTestCfg3 viaConn inline inline runTest
  where
    inline = inlineCfg 100
    viaConn = inlineCfg 0

runTestCfg2 :: ChatConfig -> ChatConfig -> (TestCC -> TestCC -> IO ()) -> IO ()
runTestCfg2 aliceCfg bobCfg runTest =
  withTmpFiles $
    withNewTestChatCfg aliceCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg bobCfg "bob" bobProfile $ \bob ->
        runTest alice bob

runTestCfg3 :: ChatConfig -> ChatConfig -> ChatConfig -> (TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
runTestCfg3 aliceCfg bobCfg cathCfg runTest =
  withTmpFiles $
    withNewTestChatCfg aliceCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg bobCfg "bob" bobProfile $ \bob ->
        withNewTestChatCfg cathCfg "cath" cathProfile $ \cath ->
          runTest alice bob cath

testAddContact :: Spec
testAddContact = versionTestMatrix2 runTestAddContact
  where
    runTestAddContact alice bob = do
      alice ##> "/c"
      inv <- getInvitation alice
      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
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

testDeleteContactDeletesProfile :: IO ()
testDeleteContactDeletesProfile =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice <##> bob
      -- alice deletes contact, profile is deleted
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      alice ##> "/cs"
      (alice </)
      alice `hasContactProfiles` ["alice"]
      -- bob deletes contact, profile is deleted
      bob ##> "/d alice"
      bob <## "alice: contact is deleted"
      bob ##> "/cs"
      (bob </)
      bob `hasContactProfiles` ["bob"]

testDirectMessageQuotedReply :: IO ()
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

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "hello 🙂"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "hello 🙂"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂"))])

      alice #$> ("/_update item @2 " <> itemId 1 <> " text hey 👋", id, "message updated")
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

      alice #$> ("/_update item @2 " <> itemId 1 <> " text greetings 🤝", id, "message updated")
      bob <# "alice> [edited] greetings 🤝"

      alice #$> ("/_update item @2 " <> itemId 2 <> " text updating bob's message", id, "cannot update this item")

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hello 🙂")), ((0, "hey alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hello 🙂")), ((1, "hey alice"), Just (0, "hey 👋"))])

      bob #$> ("/_update item @2 " <> itemId 2 <> " text hey Alice", id, "message updated")
      alice <# "bob> [edited] > hello 🙂"
      alice <## "      hey Alice"

      bob #$> ("/_update item @2 " <> itemId 3 <> " text greetings Alice", id, "message updated")
      alice <# "bob> [edited] > hey 👋"
      alice <## "      greetings Alice"

      alice #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((1, "greetings 🤝"), Nothing), ((0, "hey Alice"), Just (1, "hello 🙂")), ((0, "greetings Alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat @2 count=100", chat', chatFeatures' <> [((0, "greetings 🤝"), Nothing), ((1, "hey Alice"), Just (0, "hello 🙂")), ((1, "greetings Alice"), Just (0, "hey 👋"))])

testDirectMessageDelete :: IO ()
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
      bob #$> ("/_update item @2 " <> itemId 2 <> " text hey alice", id, "message updated")
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

testGroup :: Spec
testGroup = versionTestMatrix3 runTestGroup
  where
    runTestGroup alice bob cath = testGroupShared alice bob cath False

testGroupCheckMessages :: IO ()
testGroupCheckMessages =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> testGroupShared alice bob cath True

testGroupShared :: TestCC -> TestCC -> TestCC -> Bool -> IO ()
testGroupShared alice bob cath checkMessages = do
  connectUsers alice bob
  connectUsers alice cath
  alice ##> "/g team"
  alice <## "group #team is created"
  alice <## "to add members use /a team <name> or /create link #team"
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
  when checkMessages $ threadDelay 1000000 -- for deterministic order of messages and "connected" events
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
  when checkMessages $ threadDelay 1000000 -- for deterministic order of messages and "connected" events
  alice #> "#team hello"
  concurrently_
    (bob <# "#team alice> hello")
    (cath <# "#team alice> hello")
  when checkMessages $ threadDelay 1000000 -- server assigns timestamps with one second precision
  bob #> "#team hi there"
  concurrently_
    (alice <# "#team bob> hi there")
    (cath <# "#team bob> hi there")
  when checkMessages $ threadDelay 1000000
  cath #> "#team hey team"
  concurrently_
    (alice <# "#team cath> hey team")
    (bob <# "#team cath> hey team")
  bob <##> cath
  when checkMessages getReadChats
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
  -- delete contact
  alice ##> "/d bob"
  alice <## "bob: contact is deleted"
  alice ##> "@bob hey"
  alice <## "no contact bob"
  when checkMessages $ threadDelay 1000000
  alice #> "#team checking connection"
  bob <# "#team alice> checking connection"
  when checkMessages $ threadDelay 1000000
  bob #> "#team received"
  alice <# "#team bob> received"
  when checkMessages $ do
    alice @@@ [("@cath", "sent invitation to join group team as admin"), ("#team", "received")]
    bob @@@ [("@alice", "received invitation to join group team as admin"), ("@cath", "hey"), ("#team", "received")]
  -- test clearing chat
  alice #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  alice #$> ("/_get chat #1 count=100", chat, [])
  bob #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  bob #$> ("/_get chat #1 count=100", chat, [])
  cath #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  cath #$> ("/_get chat #1 count=100", chat, [])
  where
    getReadChats :: IO ()
    getReadChats = do
      alice @@@ [("#team", "hey team"), ("@cath", "sent invitation to join group team as admin"), ("@bob", "sent invitation to join group team as admin")]
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (0, "connected"), (1, "hello"), (0, "hi there"), (0, "hey team")])
      -- "before" and "after" define a chat item id across all chats,
      -- so we take into account group event items as well as sent group invitations in direct chats
      alice #$> ("/_get chat #1 after=" <> groupItemId 2 5 <> " count=100", chat, [(0, "hi there"), (0, "hey team")])
      alice #$> ("/_get chat #1 before=" <> groupItemId 2 7 <> " count=100", chat, [(0, "connected"), (0, "connected"), (1, "hello"), (0, "hi there")])
      alice #$> ("/_get chat #1 count=100 search=team", chat, [(0, "hey team")])
      bob @@@ [("@cath", "hey"), ("#team", "hey team"), ("@alice", "received invitation to join group team as admin")]
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "added cath (Catherine)"), (0, "connected"), (0, "hello"), (1, "hi there"), (0, "hey team")])
      cath @@@ [("@bob", "hey"), ("#team", "hey team"), ("@alice", "received invitation to join group team as admin")]
      cath #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "connected"), (0, "hello"), (0, "hi there"), (1, "hey team")])
      alice #$> ("/_read chat #1 from=1 to=100", id, "ok")
      bob #$> ("/_read chat #1 from=1 to=100", id, "ok")
      cath #$> ("/_read chat #1 from=1 to=100", id, "ok")
      alice #$> ("/_read chat #1", id, "ok")
      bob #$> ("/_read chat #1", id, "ok")
      cath #$> ("/_read chat #1", id, "ok")
      alice #$> ("/_unread chat #1 on", id, "ok")
      alice #$> ("/_unread chat #1 off", id, "ok")

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
      alice <## "to add members use /a club <name> or /create link #club"
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
      -- show last messages
      alice ##> "/t #club 8"
      alice -- these strings are expected in any order because of sorting by time and rounding of time for sent
        <##? [ "#club bob> connected",
               "#club cath> connected",
               "#club bob> added dan (Daniel)",
               "#club dan> connected",
               "#club hello",
               "#club bob> hi there",
               "#club cath> hey",
               "#club dan> how is it going?"
             ]
      alice ##> "/t @dan 2"
      alice
        <##? [ "dan> hi",
               "@dan hey"
             ]
      alice ##> "/t 18"
      alice
        <##? [ "@bob sent invitation to join group club as admin",
               "@cath sent invitation to join group club as admin",
               "#club bob> connected",
               "#club cath> connected",
               "#club bob> added dan (Daniel)",
               "#club dan> connected",
               "#club hello",
               "#club bob> hi there",
               "#club cath> hey",
               "#club dan> how is it going?",
               "dan> hi",
               "@dan hey",
               "dan> Full deletion: off",
               "dan> Voice messages: enabled",
               "bob> Full deletion: off",
               "bob> Voice messages: enabled",
               "cath> Full deletion: off",
               "cath> Voice messages: enabled"
             ]
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
      alice <##> bob
      alice <##> cath
      -- unused group contacts are deleted
      bob ##> "@cath hi"
      bob <## "no contact cath"
      (cath </)
      cath ##> "@bob hi"
      cath <## "no contact bob"
      (bob </)

testGroupSameName :: IO ()
testGroupSameName =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/g team"
      alice <## "group #team_1 (team) is created"
      alice <## "to add members use /a team_1 <name> or /create link #team_1"

testGroupDeleteWhenInvited :: IO ()
testGroupDeleteWhenInvited =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
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
      alice <## "to add members use /a team <name> or /create link #team"
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

testGroupReAddInvitedChangeRole :: IO ()
testGroupReAddInvitedChangeRole =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      -- alice re-adds bob, he sees it as the same group
      alice ##> "/a team bob owner"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as owner"
            bob <## "use /j team to accept"
        ]
      -- bob joins as owner
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      bob ##> "/d #team"
      concurrentlyN_
        [ bob <## "#team: you deleted the group",
          do
            alice <## "#team: bob deleted the group"
            alice <## "use /d #team to delete the local copy of the group"
        ]
      bob ##> "#team hi"
      bob <## "no group #team"
      alice ##> "/d #team"
      alice <## "#team: you deleted the group"

testGroupDeleteInvitedContact :: IO ()
testGroupDeleteInvitedContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      alice ##> "@bob hey"
      alice <## "no contact bob"
      bob #> "@alice hey"
      (alice </)

testDeleteGroupMemberProfileKept :: IO ()
testDeleteGroupMemberProfileKept =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      -- group 1
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
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
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      -- group 2
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "to add members use /a club <name> or /create link #club"
      alice ##> "/a club bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to bob",
          do
            bob <## "#club: alice invites you to join the group as admin"
            bob <## "use /j club to accept"
        ]
      bob ##> "/j club"
      concurrently_
        (alice <## "#club: bob joined the group")
        (bob <## "#club: you joined the group")
      alice #> "#club hello"
      bob <# "#club alice> hello"
      bob #> "#club hi there"
      alice <# "#club bob> hi there"
      -- delete contact
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      alice ##> "@bob hey"
      alice <## "no contact bob"
      bob #> "@alice hey"
      (alice </)
      -- delete group 1
      alice ##> "/d #team"
      concurrentlyN_
        [ alice <## "#team: you deleted the group",
          do
            bob <## "#team: alice deleted the group"
            bob <## "use /d #team to delete the local copy of the group"
        ]
      alice ##> "#team hi"
      alice <## "no group #team"
      bob ##> "/d #team"
      bob <## "#team: you deleted the group"
      -- group 2 still works
      alice #> "#club checking connection"
      bob <# "#club alice> checking connection"
      bob #> "#club received"
      alice <# "#club bob> received"

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
      alice <## "to add members use /a tennis <name> or /create link #tennis"
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
      threadDelay 1000000
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
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello! how are you?"), Nothing), ((1, "hello, all good, you?"), Just (0, "hello! how are you?"))])
      alice #$> ("/_get chat #1 count=2", chat', [((1, "hello! how are you?"), Nothing), ((0, "hello, all good, you?"), Just (1, "hello! how are you?"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello! how are you?"), Nothing), ((0, "hello, all good, you?"), Just (0, "hello! how are you?"))])
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
      threadDelay 1000000
      -- alice, bob: msg id 5, cath: msg id 4 (after group invitations & group events)
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")

      alice #$> ("/_update item #1 " <> groupItemId 2 5 <> " text hey 👋", id, "message updated")
      concurrently_
        (bob <# "#team alice> [edited] hey 👋")
        (cath <# "#team alice> [edited] hey 👋")

      alice #$> ("/_get chat #1 count=1", chat', [((1, "hey 👋"), Nothing)])
      bob #$> ("/_get chat #1 count=1", chat', [((0, "hey 👋"), Nothing)])
      cath #$> ("/_get chat #1 count=1", chat', [((0, "hey 👋"), Nothing)])

      threadDelay 1000000
      -- alice, bob: msg id 6, cath: msg id 5
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

      alice #$> ("/_get chat #1 count=2", chat', [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hey 👋"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hey 👋"), Nothing), ((0, "hi alice"), Just (0, "hey 👋"))])

      alice #$> ("/_update item #1 " <> groupItemId 2 5 <> " text greetings 🤝", id, "message updated")
      concurrently_
        (bob <# "#team alice> [edited] greetings 🤝")
        (cath <# "#team alice> [edited] greetings 🤝")

      alice #$> ("/_update item #1 " <> groupItemId 2 6 <> " text updating bob's message", id, "cannot update this item")

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

      alice #$> ("/_get chat #1 count=3", chat', [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hey 👋")), ((0, "greetings!"), Just (1, "greetings 🤝"))])
      bob #$> ("/_get chat #1 count=3", chat', [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hey 👋")), ((0, "greetings!"), Just (0, "greetings 🤝"))])
      cath #$> ("/_get chat #1 count=3", chat', [((0, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (0, "hey 👋")), ((1, "greetings!"), Just (0, "greetings 🤝"))])

testGroupMessageDelete :: IO ()
testGroupMessageDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      -- alice, bob: msg id 5, cath: msg id 4 (after group invitations & group events)
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")

      -- alice: deletes msg id 5
      alice #$> ("/_delete item #1 " <> groupItemId 2 5 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=1", chat, [(0, "connected")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "hello!")])
      cath #$> ("/_get chat #1 count=1", chat, [(0, "hello!")])

      threadDelay 1000000
      -- alice: msg id 5, bob: msg id 6, cath: msg id 5
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

      alice #$> ("/_get chat #1 count=1", chat', [((0, "hi alic"), Just (1, "hello!"))])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      -- alice: deletes msg id 5
      alice #$> ("/_delete item #1 " <> groupItemId 2 5 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=1", chat', [((0, "connected"), Nothing)])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      -- alice: msg id 5
      bob #$> ("/_update item #1 " <> groupItemId 2 7 <> " text hi alice", id, "message updated")
      concurrently_
        (alice <# "#team bob> [edited] hi alice")
        ( do
            cath <# "#team bob> [edited] > alice hello!"
            cath <## "      hi alice"
        )

      alice #$> ("/_get chat #1 count=1", chat', [((0, "hi alice"), Nothing)])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((1, "hi alice"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((0, "hi alice"), Just (0, "hello!"))])

      threadDelay 1000000
      -- alice: msg id 6, bob: msg id 7, cath: msg id 6
      cath #> "#team how are you?"
      concurrently_
        (alice <# "#team cath> how are you?")
        (bob <# "#team cath> how are you?")

      cath #$> ("/_delete item #1 " <> groupItemId 2 7 <> " broadcast", id, "message marked deleted")
      concurrently_
        (alice <# "#team cath> [marked deleted] how are you?")
        (bob <# "#team cath> [marked deleted] how are you?")

      alice #$> ("/_delete item #1 " <> groupItemId 2 5 <> " broadcast", id, "cannot delete this item")
      alice #$> ("/_delete item #1 " <> groupItemId 2 5 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=1", chat', [((0, "how are you? [marked deleted]"), Nothing)])
      bob #$> ("/_get chat #1 count=3", chat', [((0, "hello!"), Nothing), ((1, "hi alice"), Just (0, "hello!")), ((0, "how are you? [marked deleted]"), Nothing)])
      cath #$> ("/_get chat #1 count=3", chat', [((0, "hello!"), Nothing), ((0, "hi alice"), Just (0, "hello!")), ((1, "how are you? [marked deleted]"), Nothing)])

testUpdateGroupProfile :: IO ()
testUpdateGroupProfile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")
      bob ##> "/gp team my_team"
      bob <## "you have insufficient permissions for this group command"
      alice ##> "/gp team my_team"
      alice <## "changed to #my_team"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed to #my_team",
          do
            cath <## "alice updated group #team:"
            cath <## "changed to #my_team"
        ]
      bob #> "#my_team hi"
      concurrently_
        (alice <# "#my_team bob> hi")
        (cath <# "#my_team bob> hi")

testUpdateMemberRole :: IO ()
testUpdateMemberRole =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      addMember "team" alice bob GRAdmin
      alice ##> "/mr team bob member"
      alice <## "#team: you changed the role of bob from admin to member"
      bob <## "#team: alice invites you to join the group as member"
      bob <## "use /j team to accept"
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      connectUsers bob cath
      bob ##> "/a team cath"
      bob <## "you have insufficient permissions for this group command"
      alice ##> "/mr team bob admin"
      concurrently_
        (alice <## "#team: you changed the role of bob from member to admin")
        (bob <## "#team: alice changed your role from member to admin")
      bob ##> "/a team cath owner"
      bob <## "you have insufficient permissions for this group command"
      addMember "team" bob cath GRMember
      cath ##> "/j team"
      concurrentlyN_
        [ bob <## "#team: cath joined the group",
          do
            cath <## "#team: you joined the group"
            cath <## "#team: member alice (Alice) is connected",
          do
            alice <## "#team: bob added cath (Catherine) to the group (connecting...)"
            alice <## "#team: new member cath is connected"
        ]
      alice ##> "/mr team alice admin"
      concurrentlyN_
        [ alice <## "#team: you changed your role from owner to admin",
          bob <## "#team: alice changed the role from owner to admin",
          cath <## "#team: alice changed the role from owner to admin"
        ]
      alice ##> "/d #team"
      alice <## "you have insufficient permissions for this group command"

testGroupDeleteUnusedContacts :: IO ()
testGroupDeleteUnusedContacts =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      -- create group 1
      createGroup3 "team" alice bob cath
      -- create group 2
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "to add members use /a club <name> or /create link #club"
      alice ##> "/a club bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to bob",
          do
            bob <## "#club: alice invites you to join the group as admin"
            bob <## "use /j club to accept"
        ]
      bob ##> "/j club"
      concurrently_
        (alice <## "#club: bob joined the group")
        (bob <## "#club: you joined the group")
      alice ##> "/a club cath"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to cath",
          do
            cath <## "#club: alice invites you to join the group as admin"
            cath <## "use /j club to accept"
        ]
      cath ##> "/j club"
      concurrentlyN_
        [ alice <## "#club: cath joined the group",
          do
            cath <## "#club: you joined the group"
            cath <## "#club: member bob_1 (Bob) is connected"
            cath <## "contact bob_1 is merged into bob"
            cath <## "use @bob <message> to send messages",
          do
            bob <## "#club: alice added cath_1 (Catherine) to the group (connecting...)"
            bob <## "#club: new member cath_1 is connected"
            bob <## "contact cath_1 is merged into cath"
            bob <## "use @cath <message> to send messages"
        ]
      -- list contacts
      bob ##> "/cs"
      bob <## "alice (Alice)"
      bob <## "cath (Catherine)"
      cath ##> "/cs"
      cath <## "alice (Alice)"
      cath <## "bob (Bob)"
      -- delete group 1, contacts and profiles are kept
      deleteGroup alice bob cath "team"
      bob ##> "/cs"
      bob <## "alice (Alice)"
      bob <## "cath (Catherine)"
      bob `hasContactProfiles` ["alice", "bob", "cath"]
      cath ##> "/cs"
      cath <## "alice (Alice)"
      cath <## "bob (Bob)"
      cath `hasContactProfiles` ["alice", "bob", "cath"]
      -- delete group 2, unused contacts and profiles are deleted
      deleteGroup alice bob cath "club"
      bob ##> "/cs"
      bob <## "alice (Alice)"
      bob `hasContactProfiles` ["alice", "bob"]
      cath ##> "/cs"
      cath <## "alice (Alice)"
      cath `hasContactProfiles` ["alice", "cath"]
  where
    deleteGroup alice bob cath group = do
      alice ##> ("/d #" <> group)
      concurrentlyN_
        [ alice <## ("#" <> group <> ": you deleted the group"),
          do
            bob <## ("#" <> group <> ": alice deleted the group")
            bob <## ("use /d #" <> group <> " to delete the local copy of the group"),
          do
            cath <## ("#" <> group <> ": alice deleted the group")
            cath <## ("use /d #" <> group <> " to delete the local copy of the group")
        ]
      bob ##> ("/d #" <> group)
      bob <## ("#" <> group <> ": you deleted the group")
      cath ##> ("/d #" <> group)
      cath <## ("#" <> group <> ": you deleted the group")

testGroupDescription :: IO ()
testGroupDescription = testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
  connectUsers alice bob
  alice ##> "/g team"
  alice <## "group #team is created"
  alice <## "to add members use /a team <name> or /create link #team"
  addMember "team" alice bob GRAdmin
  bob ##> "/j team"
  concurrentlyN_
    [ alice <## "#team: bob joined the group",
      bob <## "#team: you joined the group"
    ]
  alice ##> "/group_profile team"
  alice <## "#team"
  groupInfo alice
  alice ##> "/group_descr team Welcome to the team!"
  alice <## "description changed to:"
  alice <## "Welcome to the team!"
  bob <## "alice updated group #team:"
  bob <## "description changed to:"
  bob <## "Welcome to the team!"
  alice ##> "/group_profile team"
  alice <## "#team"
  alice <## "description:"
  alice <## "Welcome to the team!"
  groupInfo alice
  connectUsers alice cath
  addMember "team" alice cath GRMember
  cath ##> "/j team"
  concurrentlyN_
    [ alice <## "#team: cath joined the group",
      do
        cath <## "#team: you joined the group"
        cath <# "#team alice> Welcome to the team!"
        cath <## "#team: member bob (Bob) is connected",
      do
        bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
        bob <## "#team: new member cath is connected"
    ]
  connectUsers bob dan
  addMember "team" bob dan GRMember
  dan ##> "/j team"
  concurrentlyN_
    [ bob <## "#team: dan joined the group",
      do
        dan <## "#team: you joined the group"
        dan <# "#team bob> Welcome to the team!"
        dan
          <### [ "#team: member alice (Alice) is connected",
                 "#team: member cath (Catherine) is connected"
               ],
      bobAddedDan alice,
      bobAddedDan cath
    ]
  where
    groupInfo alice = do
      alice <## "group preferences:"
      alice <## "Direct messages enabled: on"
      alice <## "Full deletion enabled: off"
      alice <## "Voice messages enabled: on"
    bobAddedDan cc = do
      cc <## "#team: bob added dan (Daniel) to the group (connecting...)"
      cc <## "#team: new member dan is connected"

testGroupAsync :: IO ()
testGroupAsync = withTmpFiles $ do
  print (0 :: Integer)
  withNewTestChat "alice" aliceProfile $ \alice -> do
    withNewTestChat "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
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
      alice #> "#team hello bob"
      bob <# "#team alice> hello bob"
  print (1 :: Integer)
  withTestChat "alice" $ \alice -> do
    withNewTestChat "cath" cathProfile $ \cath -> do
      alice <## "1 contacts connected (use /cs for the list)"
      alice <## "#team: connected to server(s)"
      connectUsers alice cath
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
          cath <## "#team: you joined the group"
        ]
      alice #> "#team hello cath"
      cath <# "#team alice> hello cath"
  print (2 :: Integer)
  withTestChat "bob" $ \bob -> do
    withTestChat "cath" $ \cath -> do
      concurrentlyN_
        [ do
            bob <## "1 contacts connected (use /cs for the list)"
            bob <## "#team: connected to server(s)"
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <# "#team alice> hello cath"
            bob <## "#team: new member cath is connected",
          do
            cath <## "2 contacts connected (use /cs for the list)"
            cath <## "#team: connected to server(s)"
            cath <## "#team: member bob (Bob) is connected"
        ]
  threadDelay 500000
  print (3 :: Integer)
  withTestChat "bob" $ \bob -> do
    withNewTestChat "dan" danProfile $ \dan -> do
      bob <## "2 contacts connected (use /cs for the list)"
      bob <## "#team: connected to server(s)"
      connectUsers bob dan
      bob ##> "/a team dan"
      concurrentlyN_
        [ bob <## "invitation to join the group #team sent to dan",
          do
            dan <## "#team: bob invites you to join the group as admin"
            dan <## "use /j team to accept"
        ]
      dan ##> "/j team"
      concurrentlyN_
        [ bob <## "#team: dan joined the group",
          dan <## "#team: you joined the group"
        ]
      threadDelay 1000000
  threadDelay 1000000
  print (4 :: Integer)
  withTestChat "alice" $ \alice -> do
    withTestChat "cath" $ \cath -> do
      withTestChat "dan" $ \dan -> do
        concurrentlyN_
          [ do
              alice <## "2 contacts connected (use /cs for the list)"
              alice <## "#team: connected to server(s)"
              alice <## "#team: bob added dan (Daniel) to the group (connecting...)"
              alice <## "#team: new member dan is connected",
            do
              cath <## "2 contacts connected (use /cs for the list)"
              cath <## "#team: connected to server(s)"
              cath <## "#team: bob added dan (Daniel) to the group (connecting...)"
              cath <## "#team: new member dan is connected",
            do
              dan <## "3 contacts connected (use /cs for the list)"
              dan <## "#team: connected to server(s)"
              dan <## "#team: member alice (Alice) is connected"
              dan <## "#team: member cath (Catherine) is connected"
          ]
        threadDelay 1000000
  print (5 :: Integer)
  withTestChat "alice" $ \alice -> do
    withTestChat "bob" $ \bob -> do
      withTestChat "cath" $ \cath -> do
        withTestChat "dan" $ \dan -> do
          concurrentlyN_
            [ do
                alice <## "3 contacts connected (use /cs for the list)"
                alice <## "#team: connected to server(s)",
              do
                bob <## "3 contacts connected (use /cs for the list)"
                bob <## "#team: connected to server(s)",
              do
                cath <## "3 contacts connected (use /cs for the list)"
                cath <## "#team: connected to server(s)",
              do
                dan <## "3 contacts connected (use /cs for the list)"
                dan <## "#team: connected to server(s)"
            ]
          alice #> "#team hello"
          concurrentlyN_
            [ bob <# "#team alice> hello",
              cath <# "#team alice> hello",
              dan <# "#team alice> hello"
            ]
          bob #> "#team hi there"
          concurrentlyN_
            [ alice <# "#team bob> hi there",
              cath <# "#team bob> hi there",
              dan <# "#team bob> hi there"
            ]
          cath #> "#team hey"
          concurrentlyN_
            [ alice <# "#team cath> hey",
              bob <# "#team cath> hey",
              dan <# "#team cath> hey"
            ]
          dan #> "#team how is it going?"
          concurrentlyN_
            [ alice <# "#team dan> how is it going?",
              bob <# "#team dan> how is it going?",
              cath <# "#team dan> how is it going?"
            ]
          bob <##> cath
          dan <##> cath
          dan <##> alice

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

runTestFileTransfer :: TestCC -> TestCC -> IO ()
runTestFileTransfer alice bob = do
  connectUsers alice bob
  startFileTransfer' alice bob "test.pdf" "266.0 KiB / 272376 bytes"
  concurrentlyN_
    [ do
        bob #> "@alice receiving here..."
        bob <## "completed receiving file 1 (test.pdf) from alice",
      alice
        <### [ WithTime "bob> receiving here...",
               "completed sending file 1 (test.pdf) to bob"
             ]
    ]
  src <- B.readFile "./tests/fixtures/test.pdf"
  dest <- B.readFile "./tests/tmp/test.pdf"
  dest `shouldBe` src

testInlineFileTransfer :: IO ()
testInlineFileTransfer =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/test.jpg\"}"
    alice <# "@bob voice message (00:10)"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    -- below is not shown in "sent" mode
    -- alice <## "use /fc 1 to cancel sending"
    bob <# "alice> voice message (00:10)"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    -- below is not shown in "sent" mode
    -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "started receiving file 1 (test.jpg) from alice"
    concurrently_
      (alice <## "completed sending file 1 (test.jpg) to bob")
      (bob <## "completed receiving file 1 (test.jpg) from alice")
    src <- B.readFile "./tests/fixtures/test.jpg"
    dest <- B.readFile "./tests/tmp/test.jpg"
    dest `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, sendChunks = 100, receiveChunks = 100}}

testSmallInlineFileTransfer :: IO ()
testSmallInlineFileTransfer =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    bob ##> "/_files_folder ./tests/tmp/"
    bob <## "ok"
    alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
    alice <# "@bob voice message (00:10)"
    alice <# "/f @bob ./tests/fixtures/logo.jpg"
    -- below is not shown in "sent" mode
    -- alice <## "use /fc 1 to cancel sending"
    bob <# "alice> voice message (00:10)"
    bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
    -- below is not shown in "sent" mode
    -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob <## "started receiving file 1 (logo.jpg) from alice"
    concurrently_
      (alice <## "completed sending file 1 (logo.jpg) to bob")
      (bob <## "completed receiving file 1 (logo.jpg) from alice")
    src <- B.readFile "./tests/fixtures/logo.jpg"
    dest <- B.readFile "./tests/tmp/logo.jpg"
    dest `shouldBe` src

testSmallInlineFileIgnored :: IO ()
testSmallInlineFileIgnored = withTmpFiles $ do
  withNewTestChat "alice" aliceProfile $ \alice ->
    withNewTestChatOpts testOpts {allowInstantFiles = False} "bob" bobProfile $ \bob -> do
      connectUsers alice bob
      bob ##> "/_files_folder ./tests/tmp/"
      bob <## "ok"
      alice ##> "/_send @2 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "@bob voice message (00:10)"
      alice <# "/f @bob ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      bob <# "alice> voice message (00:10)"
      bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob <## "A small file sent without acceptance - you can enable receiving such files with -f option."
      -- below is not shown in "sent" mode
      -- bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      alice <## "completed sending file 1 (logo.jpg) to bob"
      bob ##> "/fr 1"
      bob <## "file is already being received: logo.jpg"

testReceiveInline :: IO ()
testReceiveInline =
  testChatCfg2 cfg aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice #> "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 inline=on ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
    src <- B.readFile "./tests/fixtures/test.jpg"
    dest <- B.readFile "./tests/tmp/test.jpg"
    dest `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 10, receiveChunks = 5}}

runTestSmallFileTransfer :: TestCC -> TestCC -> IO ()
runTestSmallFileTransfer alice bob = do
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

runTestFileSndCancelBeforeTransfer :: TestCC -> TestCC -> IO ()
runTestFileSndCancelBeforeTransfer alice bob = do
  connectUsers alice bob
  alice #> "/f @bob ./tests/fixtures/test.txt"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.txt (11 bytes / 11 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  alice ##> "/fc 1"
  concurrentlyN_
    [ alice <##. "cancelled sending file 1 (test.txt)",
      bob <## "alice cancelled sending file 1 (test.txt)"
    ]
  alice ##> "/fs 1"
  alice
    <##.. [ "sending file 1 (test.txt): no file transfers",
            "sending file 1 (test.txt) cancelled: bob"
          ]
  alice <## "file transfer cancelled"
  bob ##> "/fs 1"
  bob <## "receiving file 1 (test.txt) cancelled"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "file cancelled: test.txt"

testFileSndCancelDuringTransfer :: IO ()
testFileSndCancelDuringTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer' alice bob "test_1MB.pdf" "1017.7 KiB / 1042157 bytes"
      alice ##> "/fc 1"
      concurrentlyN_
        [ do
            alice <## "cancelled sending file 1 (test_1MB.pdf) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test_1MB.pdf) cancelled: bob"
            alice <## "file transfer cancelled",
          do
            bob <## "alice cancelled sending file 1 (test_1MB.pdf)"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test_1MB.pdf) cancelled, received part path: ./tests/tmp/test_1MB.pdf"
        ]
      checkPartialTransfer "test_1MB.pdf"

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
      checkPartialTransfer "test.jpg"

runTestGroupFileTransfer :: TestCC -> TestCC -> TestCC -> IO ()
runTestGroupFileTransfer alice bob cath = do
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
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest1 <- B.readFile "./tests/tmp/test.jpg"
  dest2 <- B.readFile "./tests/tmp/test_1.jpg"
  dest1 `shouldBe` src
  dest2 `shouldBe` src

testInlineGroupFileTransfer :: IO ()
testInlineGroupFileTransfer =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/_files_folder ./tests/tmp/bob/"
      bob <## "ok"
      cath ##> "/_files_folder ./tests/tmp/cath/"
      cath <## "ok"
      alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "#team voice message (00:10)"
      alice <# "/f #team ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            alice
              <### [ "completed sending file 1 (logo.jpg) to bob",
                     "completed sending file 1 (logo.jpg) to cath"
                   ]
            alice ##> "/fs 1"
            alice <##. "sending file 1 (logo.jpg) complete",
          do
            bob <# "#team alice> voice message (00:10)"
            bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            bob <## "started receiving file 1 (logo.jpg) from alice"
            bob <## "completed receiving file 1 (logo.jpg) from alice",
          do
            cath <# "#team alice> voice message (00:10)"
            cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            cath <## "started receiving file 1 (logo.jpg) from alice"
            cath <## "completed receiving file 1 (logo.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/logo.jpg"
      dest1 <- B.readFile "./tests/tmp/bob/logo.jpg"
      dest2 <- B.readFile "./tests/tmp/cath/logo.jpg"
      dest1 `shouldBe` src
      dest2 `shouldBe` src
  where
    cfg = testCfg {inlineFiles = defaultInlineFilesConfig {offerChunks = 100, sendChunks = 100, totalSendChunks = 100, receiveChunks = 100}}

testSmallInlineGroupFileTransfer :: IO ()
testSmallInlineGroupFileTransfer =
  testChatCfg3 testCfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/_files_folder ./tests/tmp/bob/"
      bob <## "ok"
      cath ##> "/_files_folder ./tests/tmp/cath/"
      cath <## "ok"
      alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
      alice <# "#team voice message (00:10)"
      alice <# "/f #team ./tests/fixtures/logo.jpg"
      -- below is not shown in "sent" mode
      -- alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            alice
              <### [ "completed sending file 1 (logo.jpg) to bob",
                     "completed sending file 1 (logo.jpg) to cath"
                   ]
            alice ##> "/fs 1"
            alice <##. "sending file 1 (logo.jpg) complete",
          do
            bob <# "#team alice> voice message (00:10)"
            bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            bob <## "started receiving file 1 (logo.jpg) from alice"
            bob <## "completed receiving file 1 (logo.jpg) from alice",
          do
            cath <# "#team alice> voice message (00:10)"
            cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
            cath <## "started receiving file 1 (logo.jpg) from alice"
            cath <## "completed receiving file 1 (logo.jpg) from alice"
        ]
      src <- B.readFile "./tests/fixtures/logo.jpg"
      dest1 <- B.readFile "./tests/tmp/bob/logo.jpg"
      dest2 <- B.readFile "./tests/tmp/cath/logo.jpg"
      dest1 `shouldBe` src
      dest2 `shouldBe` src

testSmallInlineGroupFileIgnored :: IO ()
testSmallInlineGroupFileIgnored = withTmpFiles $ do
  withNewTestChat "alice" aliceProfile $ \alice ->
    withNewTestChatOpts testOpts {allowInstantFiles = False} "bob" bobProfile $ \bob -> do
      withNewTestChatOpts testOpts {allowInstantFiles = False} "cath" cathProfile $ \cath -> do
        createGroup3 "team" alice bob cath
        bob ##> "/_files_folder ./tests/tmp/bob/"
        bob <## "ok"
        cath ##> "/_files_folder ./tests/tmp/cath/"
        cath <## "ok"
        alice ##> "/_send #1 json {\"msgContent\":{\"type\":\"voice\", \"duration\":10, \"text\":\"\"}, \"filePath\":\"./tests/fixtures/logo.jpg\"}"
        alice <# "#team voice message (00:10)"
        alice <# "/f #team ./tests/fixtures/logo.jpg"
        -- below is not shown in "sent" mode
        -- alice <## "use /fc 1 to cancel sending"
        concurrentlyN_
          [ do
              alice
                <### [ "completed sending file 1 (logo.jpg) to bob",
                       "completed sending file 1 (logo.jpg) to cath"
                     ]
              alice ##> "/fs 1"
              alice <##. "sending file 1 (logo.jpg) complete",
            do
              bob <# "#team alice> voice message (00:10)"
              bob <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
              bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
              bob <## "A small file sent without acceptance - you can enable receiving such files with -f option."
              bob ##> "/fr 1"
              bob <## "file is already being received: logo.jpg",
            do
              cath <# "#team alice> voice message (00:10)"
              cath <# "#team alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
              cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
              cath <## "A small file sent without acceptance - you can enable receiving such files with -f option."
              cath ##> "/fr 1"
              cath <## "file is already being received: logo.jpg"
          ]

runTestGroupFileSndCancelBeforeTransfer :: TestCC -> TestCC -> TestCC -> IO ()
runTestGroupFileSndCancelBeforeTransfer alice bob cath = do
  createGroup3 "team" alice bob cath
  alice #> "/f #team ./tests/fixtures/test.txt"
  alice <## "use /fc 1 to cancel sending"
  concurrentlyN_
    [ do
        bob <# "#team alice> sends file test.txt (11 bytes / 11 bytes)"
        bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
      do
        cath <# "#team alice> sends file test.txt (11 bytes / 11 bytes)"
        cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
    ]
  alice ##> "/fc 1"
  concurrentlyN_
    [ alice <## "cancelled sending file 1 (test.txt)",
      bob <## "alice cancelled sending file 1 (test.txt)",
      cath <## "alice cancelled sending file 1 (test.txt)"
    ]
  alice ##> "/fs 1"
  alice <## "sending file 1 (test.txt): no file transfers"
  alice <## "file transfer cancelled"
  bob ##> "/fs 1"
  bob <## "receiving file 1 (test.txt) cancelled"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "file cancelled: test.txt"

runTestMessageWithFile :: TestCC -> TestCC -> IO ()
runTestMessageWithFile alice bob = do
  connectUsers alice bob
  alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
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
  alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, "hi, sending a file"), Just "./tests/fixtures/test.jpg")])
  bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, "hi, sending a file"), Just "./tests/tmp/test.jpg")])

testSendImage :: IO ()
testSendImage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
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
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "./tests/tmp/test.jpg")])
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
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
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
      alice #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((1, ""), Just "test.jpg")])
      bob #$> ("/_get chat @2 count=100", chatF, chatFeaturesF <> [((0, ""), Just "test.jpg")])
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
      copyFile "./tests/fixtures/test_1MB.pdf" "./tests/tmp/alice_app_files/test_1MB.pdf"
      bob #$> ("/_files_folder ./tests/tmp/bob_app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test_1MB.pdf\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
      alice <# "/f @bob test_1MB.pdf"
      alice <## "use /fc 1 to cancel sending"
      bob <# "alice> sends file test_1MB.pdf (1017.7 KiB / 1042157 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      bob <## "saving file 1 from alice to test_1MB.pdf"
      concurrently_
        (bob <## "started receiving file 1 (test_1MB.pdf) from alice")
        (alice <## "started sending file 1 (test_1MB.pdf) to bob")
      -- deleting contact should cancel and remove file
      checkActionDeletesFile "./tests/tmp/alice_app_files/test_1MB.pdf" $ do
        alice ##> "/d bob"
        alice <## "bob: contact is deleted"
        bob <## "alice cancelled sending file 1 (test_1MB.pdf)"
        bob ##> "/fs 1"
        bob <## "receiving file 1 (test_1MB.pdf) cancelled, received part path: test_1MB.pdf"
      -- deleting contact should remove cancelled file
      checkActionDeletesFile "./tests/tmp/bob_app_files/test_1MB.pdf" $ do
        bob ##> "/d alice"
        bob <## "alice: contact is deleted"

testFilesFoldersImageRcvDelete :: IO ()
testFilesFoldersImageRcvDelete =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #$> ("/_files_folder ./tests/fixtures", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/app_files", id, "ok")
      alice ##> "/_send @2 json {\"filePath\": \"test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
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
      alice ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> itemId 1 <> ", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
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
      B.readFile "./tests/tmp/test.jpg" `shouldReturn` src
      alice #$> ("/_get chat @2 count=100", chat'', chatFeatures'' <> [((0, "hi alice"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi alice"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("@bob", "hey bob")]
      bob #$> ("/_get chat @2 count=100", chat'', chatFeatures'' <> [((1, "hi alice"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi alice"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("@alice", "hey bob")]
      -- quoting (file + text) with file uses quoted text
      bob ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.pdf\", \"quotedItemId\": " <> itemId 2 <> ", \"msgContent\": {\"text\":\"\",\"type\":\"file\"}}")
      bob <# "@alice > hey bob"
      bob <## "      test.pdf"
      bob <# "/f @alice ./tests/fixtures/test.pdf"
      bob <## "use /fc 2 to cancel sending"
      alice <# "bob> > hey bob"
      alice <## "      test.pdf"
      alice <# "bob> sends file test.pdf (266.0 KiB / 272376 bytes)"
      alice <## "use /fr 2 [<dir>/ | <path>] to receive it"
      alice ##> "/fr 2 ./tests/tmp"
      alice <## "saving file 2 from bob to ./tests/tmp/test.pdf"
      concurrently_
        (alice <## "started receiving file 2 (test.pdf) from bob")
        (bob <## "started sending file 2 (test.pdf) to alice")
      concurrently_
        (alice <## "completed receiving file 2 (test.pdf) from bob")
        (bob <## "completed sending file 2 (test.pdf) to alice")
      txtSrc <- B.readFile "./tests/fixtures/test.pdf"
      B.readFile "./tests/tmp/test.pdf" `shouldReturn` txtSrc
      -- quoting (file without text) with file uses file name
      alice ##> ("/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> itemId 3 <> ", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
      alice <# "@bob > test.pdf"
      alice <## "      test.jpg"
      alice <# "/f @bob ./tests/fixtures/test.jpg"
      alice <## "use /fc 3 to cancel sending"
      bob <# "alice> > test.pdf"
      bob <## "      test.jpg"
      bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 3 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 3 ./tests/tmp"
      bob <## "saving file 3 from alice to ./tests/tmp/test_1.jpg"
      concurrently_
        (bob <## "started receiving file 3 (test.jpg) from alice")
        (alice <## "started sending file 3 (test.jpg) to bob")
      concurrently_
        (bob <## "completed receiving file 3 (test.jpg) from alice")
        (alice <## "completed sending file 3 (test.jpg) to bob")
      B.readFile "./tests/tmp/test_1.jpg" `shouldReturn` src

testGroupSendImage :: Spec
testGroupSendImage = versionTestMatrix3 runTestGroupSendImage
  where
    runTestGroupSendImage alice bob cath = do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice ##> "/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}"
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
      alice #$> ("/_get chat #1 count=1", chatF, [((1, ""), Just "./tests/fixtures/test.jpg")])
      bob #$> ("/_get chat #1 count=1", chatF, [((0, ""), Just "./tests/tmp/test.jpg")])
      cath #$> ("/_get chat #1 count=1", chatF, [((0, ""), Just "./tests/tmp/test_1.jpg")])

testGroupSendImageWithTextAndQuote :: IO ()
testGroupSendImageWithTextAndQuote =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      bob #> "#team hi team"
      concurrently_
        (alice <# "#team bob> hi team")
        (cath <# "#team bob> hi team")
      threadDelay 1000000
      alice ##> ("/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"quotedItemId\": " <> groupItemId 2 5 <> ", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}")
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
      alice #$> ("/_get chat #1 count=2", chat'', [((0, "hi team"), Nothing, Nothing), ((1, "hey bob"), Just (0, "hi team"), Just "./tests/fixtures/test.jpg")])
      alice @@@ [("#team", "hey bob"), ("@bob", "sent invitation to join group team as admin"), ("@cath", "sent invitation to join group team as admin")]
      bob #$> ("/_get chat #1 count=2", chat'', [((1, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (1, "hi team"), Just "./tests/tmp/test.jpg")])
      bob @@@ [("#team", "hey bob"), ("@alice", "received invitation to join group team as admin")]
      cath #$> ("/_get chat #1 count=2", chat'', [((0, "hi team"), Nothing, Nothing), ((0, "hey bob"), Just (0, "hi team"), Just "./tests/tmp/test_1.jpg")])
      cath @@@ [("#team", "hey bob"), ("@alice", "received invitation to join group team as admin")]

testUserContactLink :: Spec
testUserContactLink = versionTestMatrix3 $ \alice bob cath -> do
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
  alice @@@ [("@bob", "Voice messages: enabled")]
  alice <##> bob

  cath ##> ("/c " <> cLink)
  alice <#? cath
  alice @@@ [("<@cath", ""), ("@bob", "hey")]
  alice ##> "/ac cath"
  alice <## "cath (Catherine): accepting contact request..."
  concurrently_
    (cath <## "alice (Alice): contact is connected")
    (alice <## "cath (Catherine): contact is connected")
  alice @@@ [("@cath", "Voice messages: enabled"), ("@bob", "hey")]
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
      threadDelay 100000
      alice @@@ [("@bob", "Voice messages: enabled")]
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
      alice @@@ [("@cath", "Voice messages: enabled"), ("@bob", "hey")]
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
      alice @@@ [("@dan", "Voice messages: enabled"), ("@cath", "hey"), ("@bob", "hey")]
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
    alice @@@ [("@bob", "Voice messages: enabled")]
    bob @@@ [("@alice", "Voice messages: enabled"), (":2", ""), (":1", "")]
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
    alice @@@ [("@cath", "Voice messages: enabled"), ("@bob", "hey")]
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
    alice @@@ [("@robert", "Voice messages: enabled")]
    bob @@@ [("@alice", "Voice messages: enabled"), (":3", ""), (":2", ""), (":1", "")]
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
    alice @@@ [("@cath", "Voice messages: enabled"), ("@robert", "hey")]
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
    alice <## "auto_accept off"
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

testAutoReplyMessage :: IO ()
testAutoReplyMessage = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    alice ##> "/auto_accept on incognito=off text hello!"
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

testAutoReplyMessageInIncognito :: IO ()
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

testConnectIncognitoInvitationLink :: IO ()
testConnectIncognitoInvitationLink = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice #$> ("/incognito on", id, "ok")
    bob #$> ("/incognito on", id, "ok")
    alice ##> "/c"
    inv <- getInvitation alice
    bob ##> ("/c " <> inv)
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
    -- after turning incognito mode off conversation is incognito
    alice #$> ("/incognito off", id, "ok")
    bob #$> ("/incognito off", id, "ok")
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
      [ alice <## "user full name removed (your contacts are notified)",
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
    alice ##> "/cs"
    alice
      <### [ ConsoleString $ "i " <> bobIncognito,
             "cath (Catherine)"
           ]
    alice `hasContactProfiles` ["alice", T.pack aliceIncognito, T.pack bobIncognito, "cath"]
    bob ##> "/cs"
    bob <## ("i " <> aliceIncognito)
    bob `hasContactProfiles` ["bob", T.pack aliceIncognito, T.pack bobIncognito]
    -- alice deletes contact, incognito profile is deleted
    alice ##> ("/d " <> bobIncognito)
    alice <## (bobIncognito <> ": contact is deleted")
    alice ##> "/cs"
    alice <## "cath (Catherine)"
    alice `hasContactProfiles` ["alice", "cath"]
    -- bob deletes contact, incognito profile is deleted
    bob ##> ("/d " <> aliceIncognito)
    bob <## (aliceIncognito <> ": contact is deleted")
    bob ##> "/cs"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testConnectIncognitoContactAddress :: IO ()
testConnectIncognitoContactAddress = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob #$> ("/incognito on", id, "ok")
    bob ##> ("/c " <> cLink)
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
    -- after turning incognito mode off conversation is incognito
    alice #$> ("/incognito off", id, "ok")
    bob #$> ("/incognito off", id, "ok")
    alice #> ("@" <> bobIncognito <> " who are you?")
    bob ?<# "alice> who are you?"
    bob ?#> "@alice I'm Batman"
    alice <# (bobIncognito <> "> I'm Batman")
    -- list contacts
    bob ##> "/cs"
    bob <## "i alice (Alice)"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete contact, incognito profile is deleted
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    bob ##> "/cs"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testAcceptContactRequestIncognito :: IO ()
testAcceptContactRequestIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice ##> "/ad"
    cLink <- getContactLink alice True
    bob ##> ("/c " <> cLink)
    alice <#? bob
    alice #$> ("/incognito on", id, "ok")
    alice ##> "/ac bob"
    alice <## "bob (Bob): accepting contact request..."
    aliceIncognito <- getTermLine alice
    concurrentlyN_
      [ bob <## (aliceIncognito <> ": contact is connected"),
        do
          alice <## ("bob (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
          alice <## "use /i bob to print out this incognito profile again"
      ]
    -- after turning incognito mode off conversation is incognito
    alice #$> ("/incognito off", id, "ok")
    bob #$> ("/incognito off", id, "ok")
    alice ?#> "@bob my profile is totally inconspicuous"
    bob <# (aliceIncognito <> "> my profile is totally inconspicuous")
    bob #> ("@" <> aliceIncognito <> " I know!")
    alice ?<# "bob> I know!"
    -- list contacts
    alice ##> "/cs"
    alice <## "i bob (Bob)"
    alice `hasContactProfiles` ["alice", "bob", T.pack aliceIncognito]
    -- delete contact, incognito profile is deleted
    alice ##> "/d bob"
    alice <## "bob: contact is deleted"
    alice ##> "/cs"
    (alice </)
    alice `hasContactProfiles` ["alice"]

testJoinGroupIncognito :: IO ()
testJoinGroupIncognito = testChat4 aliceProfile bobProfile cathProfile danProfile $
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
    cath #$> ("/incognito on", id, "ok")
    cath ##> ("/c " <> inv)
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
    alice ##> "/a secret_club bob"
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
    alice ##> ("/a secret_club " <> cathIncognito)
    concurrentlyN_
      [ alice <## ("invitation to join the group #secret_club sent to " <> cathIncognito),
        do
          cath <## "#secret_club: alice invites you to join the group as admin"
          cath <## ("use /j secret_club to join incognito as " <> cathIncognito)
      ]
    -- cath uses the same incognito profile when joining group, disabling incognito mode doesn't affect it
    cath #$> ("/incognito off", id, "ok")
    cath ##> "/j secret_club"
    -- cath and bob don't merge contacts
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
    cath <## "you've connected to this group using an incognito profile - prohibited to invite contacts"
    -- alice invites dan
    alice ##> "/a secret_club dan"
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
    cath <## "i #secret_club"
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

testCantInviteContactIncognito :: IO ()
testCantInviteContactIncognito = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- alice connected incognito to bob
    alice #$> ("/incognito on", id, "ok")
    alice ##> "/c"
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
    alice #$> ("/incognito off", id, "ok")
    alice ##> "/g club"
    alice <## "group #club is created"
    alice <## "to add members use /a club <name> or /create link #club"
    alice ##> "/a club bob"
    alice <## "you're using your main profile for this group - prohibited to invite contacts to whom you are connected incognito"
    -- bob doesn't receive invitation
    (bob </)

testCantSeeGlobalPrefsUpdateIncognito :: IO ()
testCantSeeGlobalPrefsUpdateIncognito = testChat3 aliceProfile bobProfile cathProfile $
  \alice bob cath -> do
    alice #$> ("/incognito on", id, "ok")
    alice ##> "/c"
    invIncognito <- getInvitation alice
    alice #$> ("/incognito off", id, "ok")
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
    alice ##> "/_profile {\"displayName\": \"alice\", \"fullName\": \"\", \"preferences\": {\"fullDelete\": {\"allow\": \"always\"}}}"
    alice <## "user full name removed (your contacts are notified)"
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

testDeleteContactThenGroupDeletesIncognitoProfile :: IO ()
testDeleteContactThenGroupDeletesIncognitoProfile = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- bob connects incognito to alice
    alice ##> "/c"
    inv <- getInvitation alice
    bob #$> ("/incognito on", id, "ok")
    bob ##> ("/c " <> inv)
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
          bob <## "#team: alice invites you to join the group as admin"
          bob <## ("use /j team to join incognito as " <> bobIncognito)
      ]
    bob ##> "/j team"
    concurrently_
      (alice <## ("#team: " <> bobIncognito <> " joined the group"))
      (bob <## ("#team: you joined the group incognito as " <> bobIncognito))
    bob ##> "/cs"
    bob <## "i alice (Alice)"
    bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognito]
    -- delete contact
    bob ##> "/d alice"
    bob <## "alice: contact is deleted"
    bob ##> "/cs"
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

testDeleteGroupThenContactDeletesIncognitoProfile :: IO ()
testDeleteGroupThenContactDeletesIncognitoProfile = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    -- bob connects incognito to alice
    alice ##> "/c"
    inv <- getInvitation alice
    bob #$> ("/incognito on", id, "ok")
    bob ##> ("/c " <> inv)
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
          bob <## "#team: alice invites you to join the group as admin"
          bob <## ("use /j team to join incognito as " <> bobIncognito)
      ]
    bob ##> "/j team"
    concurrently_
      (alice <## ("#team: " <> bobIncognito <> " joined the group"))
      (bob <## ("#team: you joined the group incognito as " <> bobIncognito))
    bob ##> "/cs"
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
    bob ##> "/cs"
    (bob </)
    bob `hasContactProfiles` ["bob"]

testSetAlias :: IO ()
testSetAlias = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    connectUsers alice bob
    alice #$> ("/_set alias @2 my friend bob", id, "contact bob alias updated: my friend bob")
    alice ##> "/cs"
    alice <## "bob (Bob) (alias: my friend bob)"
    alice #$> ("/_set alias @2", id, "contact bob alias removed")
    alice ##> "/cs"
    alice <## "bob (Bob)"

testSetConnectionAlias :: IO ()
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
    alice @@@ [("@bob", "Voice messages: enabled")]
    alice ##> "/cs"
    alice <## "bob (Bob) (alias: friend)"

testSetContactPrefs :: IO ()
testSetContactPrefs = testChat2 aliceProfile bobProfile $
  \alice bob -> do
    alice #$> ("/_files_folder ./tests/tmp/alice", id, "ok")
    bob #$> ("/_files_folder ./tests/tmp/bob", id, "ok")
    createDirectoryIfMissing True "./tests/tmp/alice"
    createDirectoryIfMissing True "./tests/tmp/bob"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/alice/test.txt"
    copyFile "./tests/fixtures/test.txt" "./tests/tmp/bob/test.txt"
    bob ##> "/_profile {\"displayName\": \"bob\", \"fullName\": \"Bob\", \"preferences\": {\"voice\": {\"allow\": \"no\"}}}"
    bob <## "profile image removed"
    bob <## "updated preferences:"
    bob <## "Voice messages allowed: no"
    (bob </)
    connectUsers alice bob
    alice ##> "/_set prefs @2 {}"
    alice <## "your preferences for bob did not change"
    (bob </)
    let startFeatures = [(0, "Full deletion: off"), (0, "Voice messages: off")]
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
    -- alice ##> "/_profile {\"displayName\": \"alice\", \"fullName\": \"Alice\", \"preferences\": {\"voice\": {\"allow\": \"no\"}}}"
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
    bob ##> "/_profile {\"displayName\": \"bob\", \"fullName\": \"\", \"preferences\": {\"voice\": {\"allow\": \"yes\"}}}"
    bob <## "user full name removed (your contacts are notified)"
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

testUpdateGroupPrefs :: IO ()
testUpdateGroupPrefs =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected")])
      threadDelay 1000000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"team\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion enabled: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion enabled: on"
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on")])
      threadDelay 1000000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"team\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"off\"}, \"directMessages\": {\"enable\": \"on\"}}}"
      alice <## "updated group preferences:"
      alice <## "Full deletion enabled: off"
      alice <## "Voice messages enabled: off"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion enabled: off"
      bob <## "Voice messages enabled: off"
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off")])
      threadDelay 1000000
      -- alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"team\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"on\"}}}"
      alice ##> "/set voice #team on"
      alice <## "updated group preferences:"
      alice <## "Voice messages enabled: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Voice messages enabled: on"
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on")])
      threadDelay 1000000
      alice ##> "/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"team\", \"groupPreferences\": {\"fullDelete\": {\"enable\": \"off\"}, \"voice\": {\"enable\": \"on\"}, \"directMessages\": {\"enable\": \"on\"}}}"
      -- no update
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on")])
      threadDelay 1000000
      alice #> "#team hey"
      bob <# "#team alice> hey"
      threadDelay 1000000
      bob #> "#team hi"
      alice <# "#team bob> hi"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "Full deletion: on"), (1, "Full deletion: off"), (1, "Voice messages: off"), (1, "Voice messages: on"), (1, "hey"), (0, "hi")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "Full deletion: on"), (0, "Full deletion: off"), (0, "Voice messages: off"), (0, "Voice messages: on"), (0, "hey"), (1, "hi")])

testAllowFullDeletionContact :: IO ()
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

testAllowFullDeletionGroup :: IO ()
testAllowFullDeletionGroup =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      threadDelay 1000000
      alice #> "#team hi"
      bob <# "#team alice> hi"
      threadDelay 1000000
      bob #> "#team hey"
      alice <# "#team bob> hey"
      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion enabled: on"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion enabled: on"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "hi"), (0, "hey"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (1, "hey"), (0, "Full deletion: on")])
      bob #$> ("/_delete item #1 " <> groupItemId 2 5 <> " broadcast", id, "message deleted")
      alice <# "#team bob> [deleted] hey"
      alice #$> ("/_get chat #1 count=100", chat, [(0, "connected"), (1, "hi"), (1, "Full deletion: on")])
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "hi"), (0, "Full deletion: on")])

testProhibitDirectMessages :: IO ()
testProhibitDirectMessages =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup3 "team" alice bob cath
    threadDelay 1000000
    alice ##> "/set direct #team off"
    alice <## "updated group preferences:"
    alice <## "Direct messages enabled: off"
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
    directProhibited cc = do
      cc <## "alice updated group #team:"
      cc <## "updated group preferences:"
      cc <## "Direct messages enabled: off"

testGetSetSMPServers :: IO ()
testGetSetSMPServers =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:5001")
      alice #$> ("/smp smp://1234-w==@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==@smp1.example.im")
      alice #$> ("/smp smp://1234-w==:password@smp1.example.im", id, "ok")
      alice #$> ("/smp", id, "smp://1234-w==:password@smp1.example.im")
      alice #$> ("/smp smp://2345-w==@smp2.example.im;smp://3456-w==@smp3.example.im:5224", id, "ok")
      alice #$> ("/smp", id, "smp://2345-w==@smp2.example.im, smp://3456-w==@smp3.example.im:5224")
      alice #$> ("/smp default", id, "ok")
      alice #$> ("/smp", id, "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:5001")

testTestSMPServerConnection :: IO ()
testTestSMPServerConnection =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      alice ##> "/smp test smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:5001"
      alice <## "SMP server test passed"
      -- to test with password:
      -- alice <## "SMP server test failed at CreateQueue, error: SMP AUTH"
      -- alice <## "Server requires authorization to create queues, check password"
      alice ##> "/smp test smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:5001"
      alice <## "SMP server test passed"
      alice ##> "/smp test smp://LcJU@localhost:5001"
      alice <## "SMP server test failed at Connect, error: BROKER smp://LcJU@localhost:5001 NETWORK"
      alice <## "Possibly, certificate fingerprint in server address is incorrect"

testAsyncInitiatingOffline :: IO ()
testAsyncInitiatingOffline = withTmpFiles $ do
  putStrLn "testAsyncInitiatingOffline"
  inv <- withNewTestChat "alice" aliceProfile $ \alice -> do
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewTestChat "bob" bobProfile $ \bob -> do
    threadDelay 250000
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
    putStrLn "6"
    withTestChat "alice" $ \alice -> do
      putStrLn "7"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

testAsyncAcceptingOffline :: IO ()
testAsyncAcceptingOffline = withTmpFiles $ do
  putStrLn "testAsyncAcceptingOffline"
  inv <- withNewTestChat "alice" aliceProfile $ \alice -> do
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewTestChat "bob" bobProfile $ \bob -> do
    threadDelay 250000
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
  putStrLn "6"
  withTestChat "alice" $ \alice -> do
    putStrLn "7"
    withTestChat "bob" $ \bob -> do
      putStrLn "8"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")

testFullAsync :: IO ()
testFullAsync = withTmpFiles $ do
  putStrLn "testFullAsync"
  inv <- withNewTestChat "alice" aliceProfile $ \alice -> do
    threadDelay 250000
    putStrLn "1"
    alice ##> "/c"
    putStrLn "2"
    getInvitation alice
  putStrLn "3"
  withNewTestChat "bob" bobProfile $ \bob -> do
    threadDelay 250000
    putStrLn "4"
    bob ##> ("/c " <> inv)
    putStrLn "5"
    bob <## "confirmation sent!"
  putStrLn "6"
  withTestChat "alice" $ \_ -> pure () -- connecting... notification in UI
  putStrLn "7"
  withTestChat "bob" $ \_ -> pure () -- connecting... notification in UI
  putStrLn "8"
  withTestChat "alice" $ \alice -> do
    putStrLn "9"
    alice <## "1 contacts connected (use /cs for the list)"
    putStrLn "10"
    alice <## "bob (Bob): contact is connected"
  putStrLn "11"
  withTestChat "bob" $ \bob -> do
    putStrLn "12"
    bob <## "1 contacts connected (use /cs for the list)"
    putStrLn "13"
    bob <## "alice (Alice): contact is connected"

testFullAsyncV1 :: IO ()
testFullAsyncV1 = withTmpFiles $ do
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
    withNewAlice = withNewTestChatV1 "alice" aliceProfile
    withAlice = withTestChatV1 "alice"
    withNewBob = withNewTestChatV1 "bob" bobProfile
    withBob = withTestChatV1 "bob"

testFullAsyncV1toV2 :: IO ()
testFullAsyncV1toV2 = withTmpFiles $ do
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
    withNewAlice = withNewTestChat "alice" aliceProfile
    withAlice = withTestChat "alice"
    withNewBob = withNewTestChatV1 "bob" bobProfile
    withBob = withTestChatV1 "bob"

testFullAsyncV2toV1 :: IO ()
testFullAsyncV2toV1 = withTmpFiles $ do
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
    withNewAlice = withNewTestChatV1 "alice" aliceProfile
    withAlice = withTestChatV1 "alice"
    withNewBob = withNewTestChat "bob" bobProfile
    withBob = withTestChat "bob"

testAsyncFileTransfer :: IO ()
testAsyncFileTransfer = withTmpFiles $ do
  withNewTestChat "alice" aliceProfile $ \alice ->
    withNewTestChat "bob" bobProfile $ \bob ->
      connectUsers alice bob
  withTestChatContactConnected "alice" $ \alice -> do
    alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\":\"text\", \"text\": \"hi, sending a file\"}}"
    alice <# "@bob hi, sending a file"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatContactConnected "bob" $ \bob -> do
    bob <# "alice> hi, sending a file"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  -- withTestChatContactConnected' "alice" -- TODO not needed in v2
  -- withTestChatContactConnected' "bob" -- TODO not needed in v2
  withTestChatContactConnected' "alice"
  withTestChatContactConnected' "bob"
  withTestChatContactConnected "alice" $ \alice -> do
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
  withTestChatContactConnected "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src

testAsyncFileTransferV1 :: IO ()
testAsyncFileTransferV1 = withTmpFiles $ do
  withNewTestChatV1 "alice" aliceProfile $ \alice ->
    withNewTestChatV1 "bob" bobProfile $ \bob ->
      connectUsers alice bob
  withTestChatContactConnectedV1 "alice" $ \alice -> do
    alice ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\":\"text\", \"text\": \"hi, sending a file\"}}"
    alice <# "@bob hi, sending a file"
    alice <# "/f @bob ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatContactConnectedV1 "bob" $ \bob -> do
    bob <# "alice> hi, sending a file"
    bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  withTestChatContactConnectedV1' "alice" -- TODO not needed in v2
  withTestChatContactConnectedV1' "bob" -- TODO not needed in v2
  withTestChatContactConnectedV1' "alice"
  withTestChatContactConnectedV1' "bob"
  withTestChatContactConnectedV1 "alice" $ \alice -> do
    alice <## "started sending file 1 (test.jpg) to bob"
    alice <## "completed sending file 1 (test.jpg) to bob"
  withTestChatContactConnectedV1 "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
    bob <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src

testAsyncGroupFileTransfer :: IO ()
testAsyncGroupFileTransfer = withTmpFiles $ do
  withNewTestChat "alice" aliceProfile $ \alice ->
    withNewTestChat "bob" bobProfile $ \bob ->
      withNewTestChat "cath" cathProfile $ \cath ->
        createGroup3 "team" alice bob cath
  withTestChatGroup3Connected "alice" $ \alice -> do
    alice ##> "/_send #1 json {\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"text\":\"\",\"type\":\"text\"}}"
    alice <# "/f #team ./tests/fixtures/test.jpg"
    alice <## "use /fc 1 to cancel sending"
  withTestChatGroup3Connected "bob" $ \bob -> do
    bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
    bob ##> "/fr 1 ./tests/tmp/"
    bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  withTestChatGroup3Connected "cath" $ \cath -> do
    cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
    cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
    cath ##> "/fr 1 ./tests/tmp/"
    cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
  withTestChatGroup3Connected' "alice"
  withTestChatGroup3Connected' "bob"
  withTestChatGroup3Connected' "cath"
  -- withTestChatGroup3Connected' "alice" -- TODO not needed in v2
  -- withTestChatGroup3Connected' "bob" -- TODO not needed in v2
  -- withTestChatGroup3Connected' "cath" -- TODO not needed in v2
  withTestChatGroup3Connected' "alice"
  withTestChatGroup3Connected "bob" $ \bob -> do
    bob <## "started receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected "cath" $ \cath -> do
    cath <## "started receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected "alice" $ \alice -> do
    alice
      <### [ "started sending file 1 (test.jpg) to bob",
             "completed sending file 1 (test.jpg) to bob",
             "started sending file 1 (test.jpg) to cath",
             "completed sending file 1 (test.jpg) to cath"
           ]
  withTestChatGroup3Connected "bob" $ \bob -> do
    bob <## "completed receiving file 1 (test.jpg) from alice"
  withTestChatGroup3Connected "cath" $ \cath -> do
    cath <## "completed receiving file 1 (test.jpg) from alice"
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  dest `shouldBe` src
  dest2 <- B.readFile "./tests/tmp/test_1.jpg"
  dest2 `shouldBe` src

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

testNegotiateCall :: IO ()
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
    alice <## "message updated" -- call chat item updated
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: accepted")])
    -- alice confirms call by sending WebRTC answer
    alice ##> ("/_call answer @2 " <> serialize testWebRTCSession)
    alice
      <### [ "ok",
             "message updated"
           ]
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: connecting...")])
    bob <## "alice continued the WebRTC call"
    repeatM_ 3 $ getTermLine bob
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: connecting...")])
    -- participants can update calls as connected
    alice ##> "/_call status @2 connected"
    alice
      <### [ "ok",
             "message updated"
           ]
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: in progress (00:00)")])
    bob ##> "/_call status @2 connected"
    bob <## "ok"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: in progress (00:00)")])
    -- either party can end the call
    bob ##> "/_call end @2"
    bob <## "ok"
    bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "incoming call: ended (00:00)")])
    alice <## "call with bob ended"
    alice <## "message updated"
    alice #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(1, "outgoing call: ended (00:00)")])

testMaintenanceMode :: IO ()
testMaintenanceMode = withTmpFiles $ do
  withNewTestChat "bob" bobProfile $ \bob -> do
    withNewTestChatOpts testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
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
    withTestChat "alice" $ \alice -> testChatWorking alice bob

testChatWorking :: TestCC -> TestCC -> IO ()
testChatWorking alice bob = do
  alice <## "1 contacts connected (use /cs for the list)"
  alice #> "@bob hello again"
  bob <# "alice> hello again"
  bob #> "@alice hello too"
  alice <# "bob> hello too"

testMaintenanceModeWithFiles :: IO ()
testMaintenanceModeWithFiles = withTmpFiles $ do
  withNewTestChat "bob" bobProfile $ \bob -> do
    withNewTestChatOpts testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
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
    withTestChat "alice" $ \alice -> testChatWorking alice bob

testDatabaseEncryption :: IO ()
testDatabaseEncryption = withTmpFiles $ do
  withNewTestChat "bob" bobProfile $ \bob -> do
    withNewTestChatOpts testOpts {maintenance = True} "alice" aliceProfile $ \alice -> do
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
    withTestChatOpts testOpts {maintenance = True, dbKey = "mykey"} "alice" $ \alice -> do
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
    withTestChatOpts testOpts {maintenance = True, dbKey = "anotherkey"} "alice" $ \alice -> do
      alice ##> "/_start"
      alice <## "chat started"
      testChatWorking alice bob
      alice ##> "/_stop"
      alice <## "chat stopped"
      alice ##> "/db decrypt anotherkey"
      alice <## "ok"
    withTestChat "alice" $ \alice -> testChatWorking alice bob

testMuteContact :: IO ()
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
      bob ##> "/cs"
      bob <## "alice (Alice) (muted, you can /unmute @alice)"
      bob ##> "/unmute alice"
      bob <## "ok"
      bob ##> "/cs"
      bob <## "alice (Alice)"
      alice #> "@bob hi again"
      bob <# "alice> hi again"

testMuteGroup :: IO ()
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

testSetChatItemTTL :: IO ()
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
        alice #$> ("/_ttl 2", id, "ok")
      alice #$> ("/_get chat @2 count=100", chat, [(1, "3"), (0, "4")]) -- when expiration is turned on, first cycle is synchronous
      bob #$> ("/_get chat @2 count=100", chat, chatFeatures <> [(0, "1"), (1, "2"), (0, ""), (0, "3"), (1, "4")])
      alice #$> ("/ttl", id, "old messages are set to be deleted after: 2 second(s)")
      alice #$> ("/ttl week", id, "ok")
      alice #$> ("/ttl", id, "old messages are set to be deleted after: one week")
      alice #$> ("/ttl none", id, "ok")
      alice #$> ("/ttl", id, "old messages are not being deleted")

testGroupLink :: IO ()
testGroupLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/show link #team"
      alice <## "no group link, to create: /create link #team"
      alice ##> "/create link #team"
      _ <- getGroupLink alice "team" True
      alice ##> "/delete link #team"
      alice <## "Group link is deleted - joined members will remain connected."
      alice <## "To create a new group link use /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" True
      alice ##> "/show link #team"
      _ <- getGroupLink alice "team" False
      alice ##> "/create link #team"
      alice <## "you already have link for this group, to show: /show link #team"
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "bob (Bob): contact is connected"
            alice <## "bob invited to group #team via your group link"
            alice <## "#team: bob joined the group",
          do
            bob <## "alice (Alice): contact is connected"
            bob <## "#team: you joined the group"
        ]
      alice #$> ("/_get chat #1 count=100", chat, [(0, "invited via your group link"), (0, "connected")])
      -- contacts connected via group link are not in chat previews
      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected")]
      alice <##> bob
      alice @@@ [("@bob", "hey"), ("#team", "connected")]

      -- user address doesn't interfere
      alice ##> "/ad"
      cLink <- getContactLink alice True
      cath ##> ("/c " <> cLink)
      alice <#? cath
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request..."
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice <##> cath

      -- third member
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      alice <## "cath_1 (Catherine): accepting request to join group #team..."
      -- if contact existed it is merged
      concurrentlyN_
        [ alice
            <### [ "cath_1 (Catherine): contact is connected",
                   "contact cath_1 is merged into cath",
                   "use @cath <message> to send messages",
                   EndsWith "invited to group #team via your group link",
                   EndsWith "joined the group"
                 ],
          cath
            <### [ "alice_1 (Alice): contact is connected",
                   "contact alice_1 is merged into alice",
                   "use @alice <message> to send messages",
                   "#team: you joined the group",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      bob #> "#team hi there"
      concurrently_
        (alice <# "#team bob> hi there")
        (cath <# "#team bob> hi there")
      cath #> "#team hey team"
      concurrently_
        (alice <# "#team cath> hey team")
        (bob <# "#team cath> hey team")

      -- leaving team removes link
      alice ##> "/l team"
      concurrentlyN_
        [ do
            alice <## "#team: you left the group"
            alice <## "use /d #team to delete the group",
          bob <## "#team: alice left the group",
          cath <## "#team: alice left the group"
        ]
      alice ##> "/show link #team"
      alice <## "no group link, to create: /create link #team"

testGroupLinkDeleteGroupRejoin :: IO ()
testGroupLinkDeleteGroupRejoin =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "bob (Bob): contact is connected"
            alice <## "bob invited to group #team via your group link"
            alice <## "#team: bob joined the group",
          do
            bob <## "alice (Alice): contact is connected"
            bob <## "#team: you joined the group"
        ]
      -- use contact so it's not deleted when deleting group
      bob <##> alice
      bob ##> "/l team"
      concurrentlyN_
        [ do
            bob <## "#team: you left the group"
            bob <## "use /d #team to delete the group",
          alice <## "#team: bob left the group"
        ]
      bob ##> "/d #team"
      bob <## "#team: you deleted the group"
      -- re-join via same link
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice
            <### [ "bob_1 (Bob): contact is connected",
                   "contact bob_1 is merged into bob",
                   "use @bob <message> to send messages",
                   EndsWith "invited to group #team via your group link",
                   EndsWith "joined the group"
                 ],
          bob
            <### [ "alice_1 (Alice): contact is connected",
                   "contact alice_1 is merged into alice",
                   "use @alice <message> to send messages",
                   "#team: you joined the group"
                 ]
        ]
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

testGroupLinkContactUsed :: IO ()
testGroupLinkContactUsed =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "bob (Bob): contact is connected"
            alice <## "bob invited to group #team via your group link"
            alice <## "#team: bob joined the group",
          do
            bob <## "alice (Alice): contact is connected"
            bob <## "#team: you joined the group"
        ]
      -- sending/receiving a message marks contact as used
      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected")]
      alice #> "@bob hello"
      bob <# "alice> hello"
      alice #$> ("/clear bob", id, "bob: all messages are removed locally ONLY")
      alice @@@ [("@bob", ""), ("#team", "connected")]
      bob #$> ("/clear alice", id, "alice: all messages are removed locally ONLY")
      bob @@@ [("@alice", ""), ("#team", "connected")]

testGroupLinkIncognitoMembership :: IO ()
testGroupLinkIncognitoMembership =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      -- bob connected incognito to alice
      alice ##> "/c"
      inv <- getInvitation alice
      bob #$> ("/incognito on", id, "ok")
      bob ##> ("/c " <> inv)
      bob <## "confirmation sent!"
      bobIncognito <- getTermLine bob
      concurrentlyN_
        [ do
            bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## "use /i alice to print out this incognito profile again",
          alice <## (bobIncognito <> ": contact is connected")
        ]
      bob #$> ("/incognito off", id, "ok")
      -- alice creates group
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      -- alice invites bob
      alice ##> ("/a team " <> bobIncognito)
      concurrentlyN_
        [ alice <## ("invitation to join the group #team sent to " <> bobIncognito),
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## ("use /j team to join incognito as " <> bobIncognito)
        ]
      bob ##> "/j team"
      concurrently_
        (alice <## ("#team: " <> bobIncognito <> " joined the group"))
        (bob <## ("#team: you joined the group incognito as " <> bobIncognito))
      -- bob creates group link, cath joins
      bob ##> "/create link #team"
      gLink <- getGroupLink bob "team" True
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      bob <## "cath (Catherine): accepting request to join group #team..."
      _ <- getTermLine bob
      concurrentlyN_
        [ do
            bob <## ("cath (Catherine): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## "use /i cath to print out this incognito profile again"
            bob <## "cath invited to group #team via your group link"
            bob <## "#team: cath joined the group",
          do
            cath <## (bobIncognito <> ": contact is connected")
            cath <## "#team: you joined the group"
            cath <## "#team: member alice (Alice) is connected",
          do
            alice <## ("#team: " <> bobIncognito <> " added cath (Catherine) to the group (connecting...)")
            alice <## "#team: new member cath is connected"
        ]
      bob ?#> "@cath hi, I'm incognito"
      cath <# (bobIncognito <> "> hi, I'm incognito")
      cath #> ("@" <> bobIncognito <> " hey, I'm cath")
      bob ?<# "cath> hey, I'm cath"
      -- dan joins incognito
      dan #$> ("/incognito on", id, "ok")
      dan ##> ("/c " <> gLink)
      danIncognito <- getTermLine dan
      dan <## "connection request sent incognito!"
      bob <## (danIncognito <> ": accepting request to join group #team...")
      _ <- getTermLine bob
      _ <- getTermLine dan
      concurrentlyN_
        [ do
            bob <## (danIncognito <> ": contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i " <> danIncognito <> " to print out this incognito profile again")
            bob <## (danIncognito <> " invited to group #team via your group link")
            bob <## ("#team: " <> danIncognito <> " joined the group"),
          do
            dan <## (bobIncognito <> ": contact is connected, your incognito profile for this contact is " <> danIncognito)
            dan <## ("use /i " <> bobIncognito <> " to print out this incognito profile again")
            dan <## ("#team: you joined the group incognito as " <> danIncognito)
            dan
              <### [ "#team: member alice (Alice) is connected",
                     "#team: member cath (Catherine) is connected"
                   ],
          do
            alice <## ("#team: " <> bobIncognito <> " added " <> danIncognito <> " to the group (connecting...)")
            alice <## ("#team: new member " <> danIncognito <> " is connected"),
          do
            cath <## ("#team: " <> bobIncognito <> " added " <> danIncognito <> " to the group (connecting...)")
            cath <## ("#team: new member " <> danIncognito <> " is connected")
        ]
      dan #$> ("/incognito off", id, "ok")
      bob ?#> ("@" <> danIncognito <> " hi, I'm incognito")
      dan ?<# (bobIncognito <> "> hi, I'm incognito")
      dan ?#> ("@" <> bobIncognito <> " hey, me too")
      bob ?<# (danIncognito <> "> hey, me too")
      alice #> "#team hello"
      concurrentlyN_
        [ bob ?<# "#team alice> hello",
          cath <# "#team alice> hello",
          dan ?<# "#team alice> hello"
        ]
      bob ?#> "#team hi there"
      concurrentlyN_
        [ alice <# ("#team " <> bobIncognito <> "> hi there"),
          cath <# ("#team " <> bobIncognito <> "> hi there"),
          dan ?<# ("#team " <> bobIncognito <> "> hi there")
        ]
      cath #> "#team hey"
      concurrentlyN_
        [ alice <# "#team cath> hey",
          bob ?<# "#team cath> hey",
          dan ?<# "#team cath> hey"
        ]
      dan ?#> "#team how is it going?"
      concurrentlyN_
        [ alice <# ("#team " <> danIncognito <> "> how is it going?"),
          bob ?<# ("#team " <> danIncognito <> "> how is it going?"),
          cath <# ("#team " <> danIncognito <> "> how is it going?")
        ]

testGroupLinkUnusedHostContactDeleted :: IO ()
testGroupLinkUnusedHostContactDeleted =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      -- create group 1
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLinkTeam <- getGroupLink alice "team" True
      bob ##> ("/c " <> gLinkTeam)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "bob (Bob): contact is connected"
            alice <## "bob invited to group #team via your group link"
            alice <## "#team: bob joined the group",
          do
            bob <## "alice (Alice): contact is connected"
            bob <## "#team: you joined the group"
        ]
      -- create group 2
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "to add members use /a club <name> or /create link #club"
      alice ##> "/create link #club"
      gLinkClub <- getGroupLink alice "club" True
      bob ##> ("/c " <> gLinkClub)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #club..."
      concurrentlyN_
        [ alice
            <### [ "bob_1 (Bob): contact is connected",
                   "contact bob_1 is merged into bob",
                   "use @bob <message> to send messages",
                   EndsWith "invited to group #club via your group link",
                   EndsWith "joined the group"
                 ],
          bob
            <### [ "alice_1 (Alice): contact is connected",
                   "contact alice_1 is merged into alice",
                   "use @alice <message> to send messages",
                   "#club: you joined the group"
                 ]
        ]
      -- list contacts
      bob ##> "/cs"
      bob <## "alice (Alice)"
      -- delete group 1, host contact and profile are kept
      bobLeaveDeleteGroup alice bob "team"
      bob ##> "/cs"
      bob <## "alice (Alice)"
      bob `hasContactProfiles` ["alice", "bob"]
      -- delete group 2, unused host contact and profile are deleted
      bobLeaveDeleteGroup alice bob "club"
      bob ##> "/cs"
      (bob </)
      bob `hasContactProfiles` ["bob"]
  where
    bobLeaveDeleteGroup alice bob group = do
      bob ##> ("/l " <> group)
      concurrentlyN_
        [ do
            bob <## ("#" <> group <> ": you left the group")
            bob <## ("use /d #" <> group <> " to delete the group"),
          alice <## ("#" <> group <> ": bob left the group")
        ]
      bob ##> ("/d #" <> group)
      bob <## ("#" <> group <> ": you deleted the group")

testGroupLinkIncognitoUnusedHostContactsDeleted :: IO ()
testGroupLinkIncognitoUnusedHostContactsDeleted =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      bob #$> ("/incognito on", id, "ok")
      bobIncognitoTeam <- createGroupBobIncognito alice bob "team" "alice"
      bobIncognitoClub <- createGroupBobIncognito alice bob "club" "alice_1"
      bobIncognitoTeam `shouldNotBe` bobIncognitoClub
      -- list contacts
      bob ##> "/cs"
      bob <## "i alice (Alice)"
      bob <## "i alice_1 (Alice)"
      bob `hasContactProfiles` ["alice", "alice", "bob", T.pack bobIncognitoTeam, T.pack bobIncognitoClub]
      -- delete group 1, unused host contact and profile are deleted
      bobLeaveDeleteGroup alice bob "team" bobIncognitoTeam
      bob ##> "/cs"
      bob <## "i alice_1 (Alice)"
      bob `hasContactProfiles` ["alice", "bob", T.pack bobIncognitoClub]
      -- delete group 2, unused host contact and profile are deleted
      bobLeaveDeleteGroup alice bob "club" bobIncognitoClub
      bob ##> "/cs"
      (bob </)
      bob `hasContactProfiles` ["bob"]
  where
    createGroupBobIncognito alice bob group bobsAliceContact = do
      alice ##> ("/g " <> group)
      alice <## ("group #" <> group <> " is created")
      alice <## ("to add members use /a " <> group <> " <name> or /create link #" <> group)
      alice ##> ("/create link #" <> group)
      gLinkTeam <- getGroupLink alice group True
      bob ##> ("/c " <> gLinkTeam)
      bobIncognito <- getTermLine bob
      bob <## "connection request sent incognito!"
      alice <## (bobIncognito <> ": accepting request to join group #" <> group <> "...")
      _ <- getTermLine bob
      concurrentlyN_
        [ do
            alice <## (bobIncognito <> ": contact is connected")
            alice <## (bobIncognito <> " invited to group #" <> group <> " via your group link")
            alice <## ("#" <> group <> ": " <> bobIncognito <> " joined the group"),
          do
            bob <## (bobsAliceContact <> " (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i " <> bobsAliceContact <> " to print out this incognito profile again")
            bob <## ("#" <> group <> ": you joined the group incognito as " <> bobIncognito)
        ]
      pure bobIncognito
    bobLeaveDeleteGroup alice bob group bobIncognito = do
      bob ##> ("/l " <> group)
      concurrentlyN_
        [ do
            bob <## ("#" <> group <> ": you left the group")
            bob <## ("use /d #" <> group <> " to delete the group"),
          alice <## ("#" <> group <> ": " <> bobIncognito <> " left the group")
        ]
      bob ##> ("/d #" <> group)
      bob <## ("#" <> group <> ": you deleted the group")

testSwitchContact :: IO ()
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

testSwitchGroupMember :: IO ()
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

testMarkContactVerified :: IO ()
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
    bobInfo alice = do
      alice <## "contact ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"
      alice <## "you've shared main profile with this contact"

testMarkGroupMemberVerified :: IO ()
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
    bobInfo alice = do
      alice <## "group ID: 1"
      alice <## "member ID: 2"
      alice <## "receiving messages via: localhost"
      alice <## "sending messages via: localhost"

withTestChatContactConnected :: String -> (TestCC -> IO a) -> IO a
withTestChatContactConnected dbPrefix action =
  withTestChat dbPrefix $ \cc -> do
    cc <## "1 contacts connected (use /cs for the list)"
    action cc

withTestChatContactConnected' :: String -> IO ()
withTestChatContactConnected' dbPrefix = withTestChatContactConnected dbPrefix $ \_ -> pure ()

withTestChatContactConnectedV1 :: String -> (TestCC -> IO a) -> IO a
withTestChatContactConnectedV1 dbPrefix action =
  withTestChatV1 dbPrefix $ \cc -> do
    cc <## "1 contacts connected (use /cs for the list)"
    action cc

withTestChatContactConnectedV1' :: String -> IO ()
withTestChatContactConnectedV1' dbPrefix = withTestChatContactConnectedV1 dbPrefix $ \_ -> pure ()

withTestChatGroup3Connected :: String -> (TestCC -> IO a) -> IO a
withTestChatGroup3Connected dbPrefix action = do
  withTestChat dbPrefix $ \cc -> do
    cc <## "2 contacts connected (use /cs for the list)"
    cc <## "#team: connected to server(s)"
    action cc

withTestChatGroup3Connected' :: String -> IO ()
withTestChatGroup3Connected' dbPrefix = withTestChatGroup3Connected dbPrefix $ \_ -> pure ()

startFileTransfer :: TestCC -> TestCC -> IO ()
startFileTransfer alice bob =
  startFileTransfer' alice bob "test.jpg" "136.5 KiB / 139737 bytes"

startFileTransfer' :: TestCC -> TestCC -> String -> String -> IO ()
startFileTransfer' cc1 cc2 fileName fileSize = startFileTransferWithDest' cc1 cc2 fileName fileSize $ Just "./tests/tmp"

startFileTransferWithDest' :: TestCC -> TestCC -> String -> String -> Maybe String -> IO ()
startFileTransferWithDest' cc1 cc2 fileName fileSize fileDest_ = do
  name1 <- userName cc1
  name2 <- userName cc2
  cc1 #> ("/f @" <> name2 <> " ./tests/fixtures/" <> fileName)
  cc1 <## "use /fc 1 to cancel sending"
  cc2 <# (name1 <> "> sends file " <> fileName <> " (" <> fileSize <> ")")
  cc2 <## "use /fr 1 [<dir>/ | <path>] to receive it"
  cc2 ##> ("/fr 1" <> maybe "" (" " <>) fileDest_)
  cc2 <## ("saving file 1 from " <> name1 <> " to " <> maybe id (</>) fileDest_ fileName)
  concurrently_
    (cc2 <## ("started receiving file 1 (" <> fileName <> ") from " <> name1))
    (cc1 <## ("started sending file 1 (" <> fileName <> ") to " <> name2))

checkPartialTransfer :: String -> IO ()
checkPartialTransfer fileName = do
  src <- B.readFile $ "./tests/fixtures/" <> fileName
  dest <- B.readFile $ "./tests/tmp/" <> fileName
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
  Just User {localDisplayName, profile = LocalProfile {fullName}} <- readTVarIO currentUser
  pure . T.unpack $ localDisplayName <> " (" <> fullName <> ")"

createGroup2 :: String -> TestCC -> TestCC -> IO ()
createGroup2 gName cc1 cc2 = do
  connectUsers cc1 cc2
  name2 <- userName cc2
  cc1 ##> ("/g " <> gName)
  cc1 <## ("group #" <> gName <> " is created")
  cc1 <## ("to add members use /a " <> gName <> " <name> or /create link #" <> gName)
  addMember gName cc1 cc2 GRAdmin
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
  addMember gName cc1 cc3 GRAdmin
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

addMember :: String -> TestCC -> TestCC -> GroupMemberRole -> IO ()
addMember gName inviting invitee role = do
  name1 <- userName inviting
  memName <- userName invitee
  inviting ##> ("/a " <> gName <> " " <> memName <> " " <> B.unpack (strEncode role))
  concurrentlyN_
    [ inviting <## ("invitation to join the group #" <> gName <> " sent to " <> memName),
      do
        invitee <## ("#" <> gName <> ": " <> name1 <> " invites you to join the group as " <> B.unpack (strEncode role))
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

(?#>) :: TestCC -> String -> IO ()
cc ?#> cmd = do
  cc `send` cmd
  cc <# ("i " <> cmd)

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

chatFeatures :: [(Int, String)]
chatFeatures = map (\(a, _, _) -> a) chatFeatures''

chatFeatures' :: [((Int, String), Maybe (Int, String))]
chatFeatures' = map (\(a, b, _) -> (a, b)) chatFeatures''

chatFeaturesF :: [((Int, String), Maybe String)]
chatFeaturesF = map (\(a, _, c) -> (a, c)) chatFeatures''

chatFeatures'' :: [((Int, String), Maybe (Int, String), Maybe String)]
chatFeatures'' = [((0, "Full deletion: off"), Nothing, Nothing), ((0, "Voice messages: enabled"), Nothing, Nothing)]

groupFeatures :: [(Int, String)]
groupFeatures = map (\(a, _, _) -> a) groupFeatures''

groupFeatures'' :: [((Int, String), Maybe (Int, String), Maybe String)]
groupFeatures'' = [((0, "Direct messages: on"), Nothing, Nothing), ((0, "Full deletion: off"), Nothing, Nothing), ((0, "Voice messages: on"), Nothing, Nothing)]

itemId :: Int -> String
itemId i = show $ length chatFeatures + i

groupItemId :: Int -> Int -> String
groupItemId n i = show $ length chatFeatures * n + i

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
cc <## line = do
  l <- getTermLine cc
  when (l /= line) $ print ("expected: " <> line, ", got: " <> l)
  l `shouldBe` line

(<##.) :: TestCC -> String -> Expectation
cc <##. line = do
  l <- getTermLine cc
  let prefix = line `isPrefixOf` l
  unless prefix $ print ("expected to start from: " <> line, ", got: " <> l)
  prefix `shouldBe` True

(<##..) :: TestCC -> [String] -> Expectation
cc <##.. ls = do
  l <- getTermLine cc
  let prefix = any (`isPrefixOf` l) ls
  unless prefix $ print ("expected to start from one of: " <> show ls, ", got: " <> l)
  prefix `shouldBe` True

data ConsoleResponse = ConsoleString String | WithTime String | EndsWith String
  deriving (Show)

instance IsString ConsoleResponse where fromString = ConsoleString

-- this assumes that the string can only match one option
getInAnyOrder :: (String -> String) -> TestCC -> [ConsoleResponse] -> Expectation
getInAnyOrder _ _ [] = pure ()
getInAnyOrder f cc ls = do
  line <- f <$> getTermLine cc
  let rest = filter (not . expected line) ls
  if length rest < length ls
    then getInAnyOrder f cc rest
    else error $ "unexpected output: " <> line
  where
    expected :: String -> ConsoleResponse -> Bool
    expected l = \case
      ConsoleString s -> l == s
      WithTime s -> dropTime_ l == Just s
      EndsWith s -> s `isSuffixOf` l

(<###) :: TestCC -> [ConsoleResponse] -> Expectation
(<###) = getInAnyOrder id

(<##?) :: TestCC -> [ConsoleResponse] -> Expectation
(<##?) = getInAnyOrder dropTime

(<#) :: TestCC -> String -> Expectation
cc <# line = (dropTime <$> getTermLine cc) `shouldReturn` line

(?<#) :: TestCC -> String -> Expectation
cc ?<# line = (dropTime <$> getTermLine cc) `shouldReturn` "i " <> line

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
dropTime msg = fromMaybe err $ dropTime_ msg
  where
    err = error $ "invalid time: " <> msg

dropTime_ :: String -> Maybe String
dropTime_ msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then Just text else Nothing
  _ -> Nothing

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

getGroupLink :: TestCC -> String -> Bool -> IO String
getGroupLink cc gName created = do
  cc <## if created then "Group link is created!" else "Group link:"
  cc <## ""
  link <- getTermLine cc
  cc <## ""
  cc <## "Anybody can connect to you and join group with: /c <group_link_above>"
  cc <## ("to show it again: /show link #" <> gName)
  cc <## ("to delete it: /delete link #" <> gName <> " (joined members will remain connected to you)")
  pure link

hasContactProfiles :: TestCC -> [ContactName] -> Expectation
hasContactProfiles cc names =
  getContactProfiles cc >>= \ps -> ps `shouldMatchList` names

getContactProfiles :: TestCC -> IO [ContactName]
getContactProfiles cc = do
  user_ <- readTVarIO (currentUser $ chatController cc)
  case user_ of
    Nothing -> pure []
    Just user -> do
      profiles <- withTransaction (chatStore $ chatController cc) $ \db -> getUserContactProfiles db user
      pure $ map (\Profile {displayName} -> displayName) profiles
