{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatTests.Groups where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (runExceptT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Time (UTCTime, getCurrentTime)
import Data.Int (Int64)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Simplex.Chat.Controller (ChatController (ChatController, smpAgent), ChatConfig (..), ChatHooks (..), ChatLogLevel (..), defaultChatHooks)
import Simplex.Chat.Library.Internal (uniqueMsgMentions, updatedMentionNames)
import Simplex.Chat.Markdown (parseMaybeMarkdownList)
import Simplex.Chat.Messages (CIMention (..), CIMentionMember (..), ChatItemId)
import Simplex.Chat.Messages.Batch (encodeBinaryBatch, encodeFwdElement)
import Simplex.Chat.Messages.CIContent (publicGroupNoE2EText)
import Simplex.Chat.Options
import Simplex.Chat.Protocol (ChatMessage (ChatMessage), ChatMsgEvent (XGrpMemNew, XMsgUpdate, XMsgNew, XMsgDel), FwdSender (FwdMember), GrpMsgForward (GrpMsgForward), MsgContainer (..), MsgMention (..), MsgContent (..), VerifiedMsg (VMUnsigned), mcSimple, msgContentText)
import Simplex.Chat.Types
import Simplex.Chat.Types.MemberRelations (MemberRelation (..), getRelation, setRelation)
import Simplex.Chat.Types.Shared (GroupMemberRole (..), GroupAcceptance (..))
import Simplex.Messaging.Agent (sendMessages, vrValue)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.RetryInterval
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet (pattern PQEncOff)
import Simplex.Messaging.Protocol (MsgFlags (..))
import Simplex.Messaging.Server.Env.STM hiding (subscriptions)
import Simplex.Messaging.Transport
import Simplex.Messaging.Version
import System.Directory (copyFile, doesFileExist)
import Test.Hspec hiding (it)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Options.DB
import System.FilePath ((</>))
#endif

chatGroupTests :: SpecWith TestParams
chatGroupTests = do
  describe "chat groups" $ do
    describe "add contacts, create group and send/receive messages" testGroupMatrix
    it "add contacts, create group and send/receive messages, check messages" testGroupCheckMessages
    it "mark multiple messages as read" testMarkReadGroup
    it "initial chat pagination" testChatPaginationInitial
    it "send large message" testGroupLargeMessage
    it "create group with incognito membership" testNewGroupIncognito
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
    it "group message edit history" testGroupMessageEditHistory
    it "group message delete" testGroupMessageDelete
    it "group message delete multiple" testGroupMessageDeleteMultiple
    it "group message delete multiple (many chat batches)" testGroupMessageDeleteMultipleManyBatches
    it "group live message" testGroupLiveMessage
    it "update group profile" testUpdateGroupProfile
    it "update member role" testUpdateMemberRole
    it "check owner role change" testOwnerRoleChange
    it "group description is shown as the first message to new members" testGroupDescription
    it "moderate message of another group member" testGroupModerate
    it "moderate own message (should process as deletion)" testGroupModerateOwn
    it "moderate multiple messages" testGroupModerateMultiple
    it "moderate message of another group member (full delete)" testGroupModerateFullDelete
    it "moderate message that arrives after the event of moderation" testGroupDelayedModeration
    it "moderate message that arrives after the event of moderation (full delete)" testGroupDelayedModerationFullDelete
    it "remove member with messages (full deletion is enabled)" testDeleteMemberWithMessages
    it "remove member with messages mark deleted" testDeleteMemberMarkMessagesDeleted
    it "remove member - delete messages of left/removed members" testDeleteMemberMessagesLeftRemoved
  describe "batch send messages" $ do
    it "send multiple messages api" testSendMulti
    it "send multiple timed messages" testSendMultiTimed
    it "send multiple messages (many chat batches)" testSendMultiManyBatches
    it "shared message body is reused" testSharedMessageBody
    it "shared batch body is reused" testSharedBatchBody
  describe "async group connections" $ do
    xit "create and join group when clients go offline" testGroupAsync
  describe "group links" $ do
    it "create group link, join via group link" testGroupLink
    it "invitees were previously connected as contacts" testGroupLinkInviteesWereConnected
    it "all members were previously connected as contacts" testGroupLinkAllMembersWereConnected
    it "delete group, re-join via same link" testGroupLinkDeleteGroupRejoin
    it "host incognito" testGroupLinkHostIncognito
    it "invitee incognito" testGroupLinkInviteeIncognito
    it "incognito - join/invite" testGroupLinkIncognitoJoinInvite
    it "group link member role" testGroupLinkMemberRole
    it "demotion does not remove group link" testGroupLinkDemotedAdmin
    it "host profile received" testGroupLinkHostProfileReceived
    it "existing contact merged" testGroupLinkExistingContactMerged
  describe "group links - member screening" $ do
    it "reject member - blocked name" testGLinkRejectBlockedName
    it "accept member - only host approval" testGLinkApproveMember
    it "accept member - only moderators review" testGLinkReviewMember
    it "accept member - host approval, then moderators review" testGLinkApproveThenReviewMember
    it "delete pending approval member" testGLinkDeletePendingApprovalMember
    it "admin that joined via link introduces member for moderator review" testGLinkReviewIntroduce
  describe "group link connection plan" $ do
    it "ok to connect; known group" testPlanGroupLinkKnown
    it "own group link" testPlanGroupLinkOwn
    it "group link without contact - connecting" testPlanGroupLinkConnecting
    it "re-join existing group after leaving" testPlanGroupLinkLeaveRejoin
#if !defined(dbPostgres)
  -- TODO [postgres] restore from outdated db backup (same as in agent)
  describe "group message errors" $ do
    it "show message decryption error" testGroupMsgDecryptError
    it "should report ratchet de-synchronization, synchronize ratchets" testGroupSyncRatchet
    it "synchronize ratchets, reset connection code" testGroupSyncRatchetCodeReset
#endif
  describe "group message reactions" $ do
    it "set group message reactions" testSetGroupMessageReactions
  describe "group delivery receipts" $ do
    it "should send delivery receipts in group" testSendGroupDeliveryReceipts
    it "should send delivery receipts in group depending on configuration" testConfigureGroupDeliveryReceipts
  describe "link content filter" $ do
    it "filter group chat by link content" testGroupLinkContentFilter
  describe "direct connections in group are not established based on chat protocol version" $ do
    it "direct contacts are not created" testNoGroupDirectConns
    it "members have different local display names in different groups" testNoDirectDifferentLDNs
  describe "merge members and contacts" $ do
    it "new member should merge with existing contact" testMergeMemberExistingContact
    it "new contact should merge with existing member" testMergeContactExistingMember
    it "new contact should merge with multiple existing members" testMergeContactMultipleMembers
    it "new group link host contact should merge with single existing contact out of multiple" testMergeGroupLinkHostMultipleContacts
  describe "create member contact" $ do
    it "create contact with group member with invitation message" testMemberContactMessage
    it "create contact with group member without invitation message" testMemberContactNoMessage
    it "prohibited to create contact with group member if it already exists" testMemberContactProhibitedContactExists
    it "prohibited to repeat sending x.grp.direct.inv" testMemberContactProhibitedRepeatInv
    it "invited member replaces member contact reference if it already exists" testMemberContactInvitedConnectionReplaced
    it "share incognito profile" testMemberContactIncognito
    it "sends and updates profile when creating contact" testMemberContactProfileUpdate
    it "re-create member contact after deletion, many groups" testRecreateMemberContactManyGroups
    it "manually accept contact with group member" testMemberContactAccept
    it "manually accept contact with group member incognito" testMemberContactAcceptIncognito
  describe "group message forwarding" $ do
    it "forward messages between invitee and introduced (x.msg.new)" testGroupMsgForwardMessage
    it "forward batched messages" testGroupMsgForwardBatched
    it "forward reports to moderators, don't forward to members (x.msg.new, MCReport)" testGroupMsgForwardReport
    it "deduplicate forwarded messages" testGroupMsgForwardDeduplicate
    it "forward message edit (x.msg.update)" testGroupMsgForwardEdit
    it "forward message reaction (x.msg.react)" testGroupMsgForwardReaction
    it "forward message deletion (x.msg.del)" testGroupMsgForwardDeletion
    it "forward file (x.msg.file.descr)" testGroupMsgForwardFile
    it "forward role change (x.grp.mem.role)" testGroupMsgForwardChangeRole
    it "forward new member announcement (x.grp.mem.new)" testGroupMsgForwardNewMember
    it "forward member leaving (x.grp.leave)" testGroupMsgForwardLeave
    it "forward member removal (x.grp.mem.del)" testGroupMsgForwardMemberRemoval
    it "forward admin removal (x.grp.mem.del, relay forwards it was removed)" testGroupMsgForwardAdminRemoval
    it "forward group deletion (x.grp.del)" testGroupMsgForwardGroupDeletion
  describe "group history" $ do
    it "text messages" testGroupHistory
    it "history is sent when joining via group link" testGroupHistoryGroupLink
    it "history is not sent if preference is disabled" testGroupHistoryPreferenceOff
    it "host's file" testGroupHistoryHostFile
    it "member's file" testGroupHistoryMemberFile
    it "large file with text" testGroupHistoryLargeFile
    it "multiple files" testGroupHistoryMultipleFiles
    it "cancelled files are not attached (text message is still sent)" testGroupHistoryFileCancel
    it "cancelled files without text are excluded" testGroupHistoryFileCancelNoText
    it "quoted messages" testGroupHistoryQuotes
    it "deleted message is not included" testGroupHistoryDeletedMessage
    it "disappearing message is sent as disappearing" testGroupHistoryDisappearingMessage
    it "welcome message (group description) is sent after history" testGroupHistoryWelcomeMessage
    it "unknown member messages are processed" testGroupHistoryUnknownMember
  describe "membership profile updates" $ do
    it "send profile update on next message to group" testMembershipProfileUpdateNextGroupMessage
    it "multiple groups with same member, update is applied only once" testMembershipProfileUpdateSameMember
    it "member contact is active" testMembershipProfileUpdateContactActive
    it "member contact is deleted" testMembershipProfileUpdateContactDeleted
    it "member contact is deleted silently, then considered disabled" testMembershipProfileUpdateContactDisabled
    it "profile update without change is ignored" testMembershipProfileUpdateNoChangeIgnored
    it "change of profile contact link is ignored" testMembershipProfileUpdateContactLinkIgnored
  describe "block member for all" $ do
    it "messages are marked blocked" testBlockForAllMarkedBlocked
    it "messages are fully deleted" testBlockForAllFullDelete
    it "another admin can unblock" testBlockForAllAnotherAdminUnblocks
    it "member was blocked before joining group" testBlockForAllBeforeJoining
    it "repeat block, unblock" testBlockForAllRepeat
    it "block multiple members" testBlockForAllMultipleMembers
    it "block left/removed members" testBlockForAllLeftRemoved
  describe "group member inactivity" $ do
    it "mark member inactive on reaching quota" testGroupMemberInactive
  describe "group member reports" $ do
    it "should send report to group owner, admins and moderators, but not other users" testGroupMemberReports
  describe "group member mentions" $ do
    it "should send and edit messages with member mentions" testMemberMention
    it "should forward and quote message updating mentioned member name" testForwardQuoteMention
    it "should send updated mentions in history" testGroupHistoryWithMentions
    describe "uniqueMsgMentions" testUniqueMsgMentions
    describe "updatedMentionNames" testUpdatedMentionNames
  describe "group scoped messages" $ do
    it "should send scoped messages to support (single moderator)" testScopedSupportSingleModerator
    it "should send scoped messages to support (many moderators)" testScopedSupportManyModerators
    it "should forward messages inside support scope" testScopedSupportForward
    it "should forward messages inside support scope while member is in review" testScopedSupportForwardWhileReview
    it "should not forward messages from support to main scope" testScopedSupportDontForward
    it "should forward group wide message (x.grp.info) to all members, including in review" testScopedSupportForwardAll
    it "should not forward messages between support scopes" testScopedSupportDontForwardBetweenScopes
    it "should forward file inside support scope" testScopedSupportForwardFile
    it "should forward member removal in support scope in review (x.grp.mem.del)" testScopedSupportForwardMemberRemoval
    it "should forward admin removal in support scope in review (x.grp.mem.del, relay forwards it was removed)" testScopedSupportForwardAdminRemoval
    it "should forward pending member leaving in support scope in review (x.grp.leave)" testScopedSupportForwardLeave
    it "should forward group deletion in support scope in review (x.grp.del)" testScopedSupportForwardGroupDeletion
    it "should send messages to admins and members" testSupportCLISendCommand
    it "should correctly maintain unread stats for support chats on reading chat items" testScopedSupportUnreadStatsOnRead
    it "should correctly maintain unread stats for support chats on deleting chat items" testScopedSupportUnreadStatsOnDelete
    it "should correct member attention stat for support chat on opening it" testScopedSupportUnreadStatsCorrectOnOpen
    it "should remove support chat with member when member is removed" testScopedSupportMemberRemoved
    it "should remove support chat with member when user removes member" testScopedSupportUserRemovesMember
    it "should remove support chat with member when member leaves" testScopedSupportMemberLeaves
    it "should respect support preference in group" testSupportPreferenceGroup
    it "should respect support preference in channel" testSupportPreferenceChannel
  -- TODO [relays] add tests for channels
  -- TODO   - tests with delivery loop over members restored after restart
  -- TODO   - connect plans for relay groups
  -- TODO   - cancellation on failure to create relay group (for owner)
  -- TODO   - async retry connecting to relay (for members)
  -- TODO   - test relay privileges
  describe "channels" $ do
    describe "relay delivery" $ do
      describe "single relay" $ do
        it "should deliver messages to members" testChannels1RelayDeliver
        describe "should deliver messages in a loop over members" $ do
          it "number of recipients is multiple of bucket size (3/1)" (testChannels1RelayDeliverLoop 1)
          it "number of recipients is NOT multiple of bucket size (3/2)" (testChannels1RelayDeliverLoop 2)
          it "number of recipients is equal to bucket size (3/3)" (testChannels1RelayDeliverLoop 3)
        it "sender should deduplicate their own messages" testChannelsSenderDeduplicateOwn
      describe "multiple relays" $ do
        it "2 relays: should deliver messages to members" testChannels2RelaysDeliver
        it "should share same incognito profile with all relays" testChannels2RelaysIncognito
      it "should connect to channel via /c (CLI)" testConnectChannelCLI
      it "should connect to channel via /c incognito (CLI)" testConnectChannelCLIIncognito
    describe "deliver member profiles via relay" $ do
      it "late joiner (no prior history) learns sender on first forward" testChannelLateJoinerReceivesProfile
      it "2 relays: deduplicate member announcement" testChannel2RelaysDeduplicateProfile
      it "multi senders disseminate independently" testChannelMultiSendersIndependent
      it "large profile fits in body" testChannelLargeProfileFits
      it "multiple large profiles pack across batches in one multi-sender job" testChannelMultipleLargeProfiles
      it "profile update reuses existing announcement (no re-prepend)" testChannelProfileUpdateNoRePrepend
    describe "channel operations" $ do
      it "should update channel profile (signed)" testChannelUpdateProfileSigned
      it "should preserve working link after profile update" testChannelLinkAfterProfileUpdate
      it "should preserve working link after welcome message update" testChannelLinkAfterWelcomeUpdate
      it "should preserve owner key in link data after profile update" testChannelOwnerKeyAfterLinkUpdate
      it "should update channel preferences (signed)" testChannelUpdatePrefsSigned
      it "should change member role (signed)" testChannelChangeRoleSigned
      it "should block member for all (signed)" testChannelBlockMemberSigned
      it "should remove member (signed)" testChannelRemoveMemberSigned
      it "should delete channel (signed)" testChannelDeleteGroupSigned
      it "should delete channel and clean up relay connections" testChannelDeleteGroupCleanup
      it "owner should leave channel (signed)" testChannelOwnerLeave
      it "subscriber should leave channel (signed)" testChannelSubscriberLeave
      it "relay should leave channel" testChannelRelayLeave
      it "owner should update profile in channel (signed)" testChannelOwnerProfileUpdate
      it "subscriber should update profile in channel (signed)" testChannelSubscriberProfileUpdate
      it "should report relay results when one relay deleted its address" testChannelCreateDeletedRelay
      it "should deliver support scope messages via relay" testChannelSupportScope
      it "should add relay to existing channel" testChannelAddRelay
      it "should remove relay from channel" testChannelRemoveRelay
      it "should remove left relay from channel" testChannelRemoveLeftRelay
      describe "relay rejection" $ do
        it "relay rejects fresh invitation after leaving the same channel" testRelayRejectAfterLeave
        it "operator allow clears rejection and relay accepts again" testRelayAllowAcceptsAgain
        it "rejection on channel A does not affect unrelated channel B" testRelayDoesNotRejectUnrelatedChannel
        it "concurrent fresh invitations both rejected" testRelayRejectRaceConcurrentInvitations
      describe "promoted members roster" $ do
        it "moderator action verifies via owner-signed roster" testChannelModeratorActionViaRoster
        it "subscriber recovers a missed roster member after a version gap" testChannelSubscriberRosterCatchUp
        it "2 relays: subscriber recovers a missed roster member after a version gap" testChannel2RelaysSubscriberRosterCatchUp
        it "removed moderator drops from the roster cache" testChannelRemovedModeratorRefreshesRoster
        it "role transitions update the roster (mod <-> admin, admin -> non-roster)" testChannelRoleTransitionsUpdateRoster
        it "malicious relay cannot downgrade or re-key a roster-established moderator via XGrpMemNew" testChannelRelayCannotDowngradeRosterMember
        it "malicious relay cannot forge a privileged member via XGrpMemNew forwarded as the owner" testChannelRelayCannotForgePrivilegedMember
        it "should add relay to channel with roster (relay caches roster before joinable)" testChannelAddRelayWithRoster
        it "roster blob spanning multiple chunks reassembles" testChannelRosterMultipartReassembly
        it "corrupted roster blob is rejected on digest mismatch" testChannelRosterDigestMismatchRejected
        it "promoted member enters the roster and can post" testChannelPromotedMemberCanPost
        it "observer cannot post until promoted" testChannelObserverCannotPost
        it "promoted member re-connecting via a new relay is accepted via the roster-pinned key" testChannelPromotedMemberRejoinViaRelay
        it "2 relays: multi-chunk roster reassembles per source (no stream interleaving)" testChannelRosterMultiRelayMultipart
    describe "channel message operations" $ do
      it "should update channel message" testChannelMessageUpdate
      it "should delete channel message" testChannelMessageDelete
      it "should delete channel message from history" testChannelMessageDeleteFromHistory
      it "should send and receive channel message file" testChannelMessageFile
      it "should cancel channel message file" testChannelMessageFileCancel
      it "should quote channel message" testChannelMessageQuote
      it "should not leak owner identity in channel reaction" testChannelOwnerReaction
      it "should not leak owner identity in channel quote" testChannelOwnerQuote
      it "should update channel message sent as member" testChannelOwnerUpdateAsMember
      it "should delete channel message sent as member" testChannelOwnerDeleteAsMember
      it "should send and receive file sent as member" testChannelOwnerFileTransferAsMember
      it "should cancel file sent as member" testChannelOwnerFileCancelAsMember
      it "should attribute reactions to member" testChannelReactionAttribution
      it "should recreate deleted item with correct sendAsGroup from update" testChannelUpdateFallbackSendAsGroup
      it "should respect sendAsGroup parameter in forward API" testForwardAPIUsesParameter
      it "should compute sendAsGroup in CLI forward" testForwardCLISendAsGroup
      it "should update member message in channel" testChannelMemberMessageUpdate
      it "should delete member message in channel" testChannelMemberMessageDelete
      describe "channel message signing" $ do
        it "should sign member message and reuse signature on edit" testChannelMemberMessageSign
        it "should reject unsigned update of a signed item" testChannelMemberUpdateEnforcement
        it "should sign as-channel post and keep it displayed as the channel" testChannelAsGroupSign
        it "should reject a non-owner posting as the channel" testChannelAsGroupSpoof
        it "should sign self-delete of a signed item" testChannelMemberSelfDeleteSign
        it "should reject unsigned delete of a signed item" testChannelMemberDeleteEnforcement
        it "should always sign moderation delete" testChannelModerationDeleteSign
        it "should verify signed file digest" testChannelSignedFile
        it "should warn on missing signature when signing is required" testChannelSignMessagesRequired

testGroupCheckMessages :: HasCallStack => TestParams -> IO ()
testGroupCheckMessages =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> testGroupShared alice bob cath True

testGroupMatrix :: SpecWith TestParams
testGroupMatrix =
  versionTestMatrix3 $ \alice bob cath -> testGroupShared alice bob cath False

testGroupShared :: HasCallStack => TestCC -> TestCC -> TestCC -> Bool -> IO ()
testGroupShared alice bob cath checkMessages = do
  connectUsers alice bob
  connectUsers alice cath
  alice ##> "/g team"
  alice <## "group #team is created"
  alice <## "to add members use /a team <name> or /create link #team"
  alice ##> "/a team bob admin"
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
  alice ##> "/a team cath admin"
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
  msgItem1 <- lastItemId alice
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
  msgItem2 <- lastItemId alice
  when checkMessages $ getReadChats msgItem1 msgItem2
  -- list groups
  alice ##> "/gs"
  alice <## "#team (3 members)"
  -- list group members
  alice ##> "/ms team"
  alice
    <### [ "alice (Alice): owner, you, created group",
           "bob (Bob): admin, invited, connected",
           "cath (Catherine): admin, invited, connected"
         ]
  -- list contacts
  alice ##> "/contacts"
  alice <## "bob (Bob)"
  alice <## "cath (Catherine)"
  -- test observer role
  alice ##> "/mr team bob observer"
  concurrentlyN_
    [ alice <## "#team: you changed the role of bob to observer",
      bob <## "#team: alice changed your role from admin to observer",
      cath <## "#team: alice changed the role of bob from admin to observer"
    ]
  bob ##> "#team hello"
  bob <## "#team: you don't have permission to send messages"
  bob ##> "/rm team cath"
  bob <## "#team: you have insufficient permissions for this action, the required role is admin"
  alice ##> "/mr team bob admin"
  concurrentlyN_
    [ alice <## "#team: you changed the role of bob to admin",
      bob <## "#team: alice changed your role from observer to admin",
      cath <## "#team: alice changed the role of bob from observer to admin"
    ]
  -- delete contact
  alice ##> "/d bob"
  alice <## "bob: contact is deleted"
  bob <## "alice (Alice) deleted contact with you"
  when checkMessages $ threadDelay 1000000
  alice #> "#team checking connection"
  concurrently_
    (bob <# "#team alice> checking connection")
    (cath <# "#team alice> checking connection")
  when checkMessages $ threadDelay 1000000
  bob #> "#team received"
  concurrently_
    (alice <# "#team bob> received")
    (cath <# "#team bob> received")
  when checkMessages $ do
    alice @@@ [("@cath", "sent invitation to join group team as admin"), ("#team", "received")]
    bob @@@ [("@alice", "contact deleted"), ("#team", "received")]
  -- test clearing chat
  threadDelay 1000000
  alice #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  alice #$> ("/_get chat #1 count=100", chat, [(1,"chat banner")])
  bob #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  bob #$> ("/_get chat #1 count=100", chat, [(1,"chat banner")])
  cath #$> ("/clear #team", id, "#team: all messages are removed locally ONLY")
  cath #$> ("/_get chat #1 count=100", chat, [(1,"chat banner")])
  where
    getReadChats :: HasCallStack => String -> String -> IO ()
    getReadChats msgItem1 msgItem2 = do
      alice @@@ [("#team", "hey team"), ("@cath", "sent invitation to join group team as admin"), ("@bob", "sent invitation to join group team as admin")]
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "connected"), (0, "connected"), (1, "hello"), (0, "hi there"), (0, "hey team")])
      -- "before" and "after" define a chat item id across all chats,
      -- so we take into account group event items as well as sent group invitations in direct chats
      alice #$> ("/_get chat #1 after=" <> msgItem1 <> " count=100", chat, [(0, "hi there"), (0, "hey team")])
      alice #$> ("/_get chat #1 before=" <> msgItem2 <> " count=100", chat, sndGroupFeatures <> [(0, "connected"), (0, "connected"), (1, "hello"), (0, "hi there")])
      alice #$> ("/_get chat #1 around=" <> msgItem1 <> " count=2", chat, [(0, "connected"), (0, "connected"), (1, "hello"), (0, "hi there"), (0, "hey team")])
      alice #$> ("/_get chat #1 count=100 search=team", chat, [(0, "hey team")])
      bob @@@ [("#team", "hey team"), ("@alice", "received invitation to join group team as admin")]
      bob #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "added cath"), (0, "connected"), (0, "hello"), (1, "hi there"), (0, "hey team")])
      cath @@@ [("#team", "hey team"), ("@alice", "received invitation to join group team as admin")]
      cath #$> ("/_get chat #1 count=100", chat, groupFeatures <> [(0, "connected"), (0, "connected"), (0, "hello"), (0, "hi there"), (1, "hey team")])
      alice #$> ("/_read chat #1", id, "ok")
      bob #$> ("/_read chat #1", id, "ok")
      cath #$> ("/_read chat #1", id, "ok")
      alice #$> ("/_unread chat #1 on", id, "ok")
      alice #$> ("/_unread chat #1 off", id, "ok")

testMarkReadGroup :: HasCallStack => TestParams -> IO ()
testMarkReadGroup = testChat2 aliceProfile bobProfile $ \alice bob -> do
  createGroup2 "team" alice bob
  alice #> "#team 1"
  alice #> "#team 2"
  alice #> "#team 3"
  alice #> "#team 4"
  bob <# "#team alice> 1"
  bob <# "#team alice> 2"
  bob <# "#team alice> 3"
  bob <# "#team alice> 4"
  bob ##> "/last_item_id"
  i :: ChatItemId <- read <$> getTermLine bob
  let itemIds = intercalate "," $ map show [i - 3 .. i]
  bob #$> ("/_read chat items #1 " <> itemIds, id, "items read for chat")

testChatPaginationInitial :: HasCallStack => TestParams -> IO ()
testChatPaginationInitial = testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> do
  createGroup2 "team" alice bob
  -- Wait, otherwise ids are going to be wrong.
  threadDelay 1000000
  lastEventId <- (read :: String -> Int) <$> lastItemId bob
  let groupItemId n = show $ lastEventId + n

  -- Send messages from alice to bob
  forM_ ([1 .. 10] :: [Int]) $ \n -> alice #> ("#team " <> show n)

  -- Bob receives the messages.
  forM_ ([1 .. 10] :: [Int]) $ \n -> bob <# ("#team alice> " <> show n)

  -- All messages are unread for bob, should return area around unread
  bob #$> ("/_get chat #1 initial=2", chat, [(0, "Chat with admins: on"), (0, "connected"), (0, "1"), (0, "2"), (0, "3")])

  -- Read next 2 items
  let itemIds = intercalate "," $ map groupItemId [1 .. 2]
  bob #$> ("/_read chat items #1 " <> itemIds, id, "items read for chat")
  bob #$> ("/_get chat #1 initial=2", chat, [(0, "connected"), (0, "1"), (0, "2"), (0, "3"), (0, "4")])

  -- Read all items
  bob #$> ("/_read chat #1", id, "ok")
  bob #$> ("/_get chat #1 initial=3", chat, [(0, "8"), (0, "9"), (0, "10")])
  bob #$> ("/_get chat #1 initial=5", chat, [(0, "6"), (0, "7"), (0, "8"), (0, "9"), (0, "10")])
  where
    opts =
      testOpts
        { markRead = False
        }

testGroupLargeMessage :: HasCallStack => TestParams -> IO ()
testGroupLargeMessage =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      img <- genProfileImg
      let profileImage = "data:image/png;base64," <> B.unpack img
      alice `send` ("/_group_profile #1 {\"displayName\": \"team\", \"fullName\": \"\", \"image\": \"" <> profileImage <> "\", \"groupPreferences\": {\"directMessages\": {\"enable\": \"on\"}, \"history\": {\"enable\": \"on\"}}}")
      _trimmedCmd1 <- getTermLine alice
      alice <## "profile image updated"
      bob <## "alice updated group #team:"
      bob <## "profile image updated"

testNewGroupIncognito :: HasCallStack => TestParams -> IO ()
testNewGroupIncognito =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      bob ##> "/set accept member contacts on"
      bob <## "ok"

      connectUsers alice bob

      -- alice creates group with incognito membership
      alice ##> "/g i team"
      aliceIncognito <- getTermLine alice
      alice <## ("group #team is created, your incognito profile for this group is " <> aliceIncognito)
      alice <## "to add members use /create link #team"

      -- alice invites bob
      alice ##> "/a team bob"
      alice <## "you are using an incognito profile for this group - prohibited to invite contacts"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob_1 joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      alice <##> bob

      alice ##> "@#team bob_1 hi, I'm incognito"
      alice
        <### [ "member #team bob_1 does not have direct connection, creating",
                "contact for member #team bob_1 is created",
                "sent invitation to connect directly to member #team bob_1",
                WithTime "i @bob_1 hi, I'm incognito"
              ]
      bob
        <### [ ConsoleString ("#team " <> aliceIncognito <> " is creating direct contact " <> aliceIncognito <> " with you"),
                WithTime (aliceIncognito <> "> hi, I'm incognito")
              ]
      bob <## (aliceIncognito <> ": you can send messages to contact")
      _ <- getTermLine alice
      concurrentlyN_
        [ do
            alice <## ("bob_1 (Bob): contact is connected, your incognito profile for this contact is " <> aliceIncognito)
            alice <## "use /i bob_1 to print out this incognito profile again",
          bob <## (aliceIncognito <> ": contact is connected")
        ]

      bob #> ("@" <> aliceIncognito <> " hey, I'm bob")
      alice ?<# "bob_1> hey, I'm bob"

      alice ?#> "#team hello"
      bob <# ("#team " <> aliceIncognito <> "> hello")
      bob #> "#team hi there"
      alice ?<# "#team bob_1> hi there"

      alice ##> "/gs"
      alice <## "i #team (2 members)"
      bob ##> "/gs"
      bob <## "#team (2 members)"

testGroup2 :: HasCallStack => TestParams -> IO ()
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
      alice ##> "/a club bob admin"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to bob",
          do
            bob <## "#club: alice invites you to join the group as admin"
            bob <## "use /j club to accept"
        ]
      alice ##> "/a club cath admin"
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
            dan <## "#club: bob invites you to join the group as member"
            dan <## "use /j club to accept"
        ]
      dan ##> "/j club"
      concurrentlyN_
        [ bob <## "#club: dan joined the group",
          do
            dan <## "#club: you joined the group"
            dan
              <### [ "#club: member alice_1 (Alice) is connected",
                     "contact and member are merged: alice, #club alice_1",
                     "use @alice <message> to send messages",
                     "#club: member cath (Catherine) is connected"
                   ],
          do
            alice <## "#club: bob added dan_1 (Daniel) to the group (connecting...)"
            alice <## "#club: new member dan_1 is connected"
            alice <## "contact and member are merged: dan, #club dan_1"
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
      dan <##> alice
      -- show last messages
      alice ##> "/t #club 21"
      alice -- these strings are expected in any order because of sorting by time and rounding of time for sent
        <##?
          ( map (ConsoleString . ("#club " <> )) groupFeatureStrs
              <>
                [ "#club bob> connected",
                  "#club cath> connected",
                  "#club bob> added dan",
                  "#club dan> connected",
                  "#club hello",
                  "#club bob> hi there",
                  "#club cath> hey",
                  "#club dan> how is it going?"
                ]
          )
      alice ##> "/t @dan 2"
      alice
        <##? [ "dan> hi",
               "@dan hey"
             ]
      -- TODO this fails returning only 23 lines out of 24
      -- alice ##> "/t 24"
      -- alice
      --   <##? [ "@bob sent invitation to join group club as admin",
      --          "@cath sent invitation to join group club as admin",
      --          "#club bob> connected",
      --          "#club cath> connected",
      --          "#club bob> added dan (Daniel)", -- either this is missing
      --          "#club dan> connected",
      --          "#club hello",
      --          "#club bob> hi there",
      --          "#club cath> hey",
      --          "#club dan> how is it going?",
      --          "dan> hi",
      --          "@dan hey",
      --          "dan> Disappearing messages: off",
      --          "dan> Full deletion: off",
      --          "dan> Voice messages: enabled",
      --          "dan> Audio/video calls: enabled",
      --          "bob> Disappearing messages: off", -- or this one
      --          "bob> Full deletion: off",
      --          "bob> Voice messages: enabled",
      --          "bob> Audio/video calls: enabled",
      --          "cath> Disappearing messages: off",
      --          "cath> Full deletion: off",
      --          "cath> Voice messages: enabled",
      --          "cath> Audio/video calls: enabled"
      --        ]
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
      dan <## "bad chat command: not current member"
      dan ##> "/d #club"
      dan <## "#club: you deleted the group"
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
      bob <## "bad chat command: not current member"
      bob ##> "/d #club"
      bob <## "#club: you deleted the group"
      bob <##> alice

testGroupDelete :: HasCallStack => TestParams -> IO ()
testGroupDelete =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GRMember) (cath, GRMember)
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
      cath <## "bad chat command: not current member"
      cath ##> "/d #team"
      cath <## "#team: you deleted the group"
      alice <##> bob
      alice <##> cath
      -- unused group contacts are deleted
      threadDelay 3000000
      bob ##> "@cath hi"
      bob <## "no contact cath"
      (cath </)
      cath ##> "@bob hi"
      cath <## "no contact bob"
      (bob </)
  where
    cfg = testCfg {initialCleanupManagerDelay = 0, cleanupManagerInterval = 1, cleanupManagerStepDelay = 0}

testGroupSameName :: HasCallStack => TestParams -> IO ()
testGroupSameName =
  testChat2 aliceProfile bobProfile $
    \alice _ -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/g team"
      alice <## "group #team_1 is created"
      alice <## "to add members use /a team_1 <name> or /create link #team_1"

testGroupDeleteWhenInvited :: HasCallStack => TestParams -> IO ()
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
            bob <## "#team: alice invites you to join the group as member"
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
            bob <## "#team: alice invites you to join the group as member"
            bob <## "use /j team to accept"
        ]

testGroupReAddInvited :: HasCallStack => TestParams -> IO ()
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
            bob <## "#team: alice invites you to join the group as member"
            bob <## "use /j team to accept"
        ]
      -- alice re-adds bob, he sees it as the same group
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as member"
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
            bob <## "#team_1: alice invites you to join the group as member"
            bob <## "use /j team_1 to accept"
        ]

testGroupReAddInvitedChangeRole :: HasCallStack => TestParams -> IO ()
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
            bob <## "#team: alice invites you to join the group as member"
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

testGroupDeleteInvitedContact :: HasCallStack => TestParams -> IO ()
testGroupDeleteInvitedContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      bob ##> "/set accept member contacts on"
      bob <## "ok"

      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as member"
            bob <## "use /j team to accept"
        ]
      threadDelay 500000
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      alice `send` "@bob hey"
      alice
        <### [ WithTime "@bob hey",
               "member #team bob does not have direct connection, creating",
               "contact for member #team bob is created",
               "sent invitation to connect directly to member #team bob"
             ]
      bob
        <### [ "#team alice is creating direct contact alice with you",
               WithTime "alice> hey",
               "alice: security code changed"
             ]
      bob <## "alice (Alice): you can send messages to contact"
      concurrently_
        (alice <## "bob (Bob): contact is connected")
        (bob <## "alice (Alice): contact is connected")
      alice <##> bob

testDeleteGroupMemberProfileKept :: HasCallStack => TestParams -> IO ()
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
            bob <## "#team: alice invites you to join the group as member"
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
            bob <## "#club: alice invites you to join the group as member"
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
      bob <## "alice (Alice) deleted contact with you"
      alice ##> "@bob hey"
      alice <## "no contact bob, use @#club bob <your message>"
      bob ##> "@alice hey"
      bob <## "alice: not ready"
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

testGroupRemoveAdd :: HasCallStack => TestParams -> IO ()
testGroupRemoveAdd =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GRMember) (cath, GRMember)

      threadDelay 100000

      -- remove member
      alice ##> "/rm team bob"
      concurrentlyN_
        [ alice <## "#team: you removed bob from the group",
          do
            bob <## "#team: alice removed you from the group"
            bob <## "use /d #team to delete the group",
          cath <## "#team: alice removed bob from the group"
        ]

      threadDelay 100000

      alice ##> "/a team bob"
      alice <## "invitation to join the group #team sent to bob"
      bob <## "#team_1: alice invites you to join the group as member"
      bob <## "use /j team_1 to accept"
      bob ##> "/j team_1"
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team_1: you joined the group"
            bob <## "#team_1: member cath_1 (Catherine) is connected",
          do
            cath <## "#team: alice added bob_1 (Bob) to the group (connecting...)"
            cath <## "#team: new member bob_1 is connected"
        ]
      alice #> "#team hi"
      concurrently_
        (bob <# "#team_1 alice> hi")
        (cath <# "#team alice> hi")
      bob #> "#team_1 hey"
      concurrently_
        (alice <# "#team bob> hey")
        (cath <# "#team bob_1> hey")
      cath #> "#team hello"
      concurrently_
        (alice <# "#team cath> hello")
        (bob <# "#team_1 cath_1> hello")

testGroupList :: HasCallStack => TestParams -> IO ()
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
            bob <## "#tennis: alice invites you to join the group as member"
            bob <## "use /j tennis to accept"
        ]
      -- alice sees both groups
      alice ##> "/gs"
      alice <### ["#team (2 members)", "#tennis (1 member)"]
      -- bob sees #tennis as invitation
      bob ##> "/gs"
      bob
        <### [ "#team (2 members)",
               "#tennis - you are invited (/j tennis to join, /d #tennis to delete invitation)"
             ]
      -- after deleting invitation bob sees only one group
      bob ##> "/d #tennis"
      bob <## "#tennis: you deleted the group"
      bob ##> "/gs"
      bob <## "#team (2 members)"

testGroupMessageQuotedReply :: HasCallStack => TestParams -> IO ()
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
            alice <# "#team bob!> > alice hello! how are you?"
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
            bob <# "#team cath!> > bob hello, all good, you?"
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
            bob <# "#team alice!> > bob will tell more"
            bob <## "      go on"
        )
        ( do
            cath <# "#team alice> > bob will tell more"
            cath <## "      go on"
        )

testGroupMessageUpdate :: HasCallStack => TestParams -> IO ()
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

      msgItemId1 <- lastItemId alice
      alice ##> ("/_update item #1 " <> msgItemId1 <> " text hello!")
      alice <## "message didn't change"

      alice ##> ("/_update item #1 " <> msgItemId1 <> " text hey 👋")
      alice <# "#team [edited] hey 👋"
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
            alice <# "#team bob!> > alice hey 👋"
            alice <## "      hi alice"
        )
        ( do
            cath <# "#team bob> > alice hey 👋"
            cath <## "      hi alice"
        )

      alice #$> ("/_get chat #1 count=2", chat', [((1, "hey 👋"), Nothing), ((0, "hi alice"), Just (1, "hey 👋"))])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hey 👋"), Nothing), ((1, "hi alice"), Just (0, "hey 👋"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hey 👋"), Nothing), ((0, "hi alice"), Just (0, "hey 👋"))])

      alice ##> ("/_update item #1 " <> msgItemId1 <> " text greetings 🤝")
      alice <# "#team [edited] greetings 🤝"
      concurrently_
        (bob <# "#team alice> [edited] greetings 🤝")
        (cath <# "#team alice> [edited] greetings 🤝")

      msgItemId2 <- lastItemId alice
      alice #$> ("/_update item #1 " <> msgItemId2 <> " text updating bob's message", id, "cannot update this item")

      threadDelay 1000000
      cath `send` "> #team @alice (greetings) greetings!"
      cath <# "#team > alice greetings 🤝"
      cath <## "      greetings!"
      concurrently_
        ( do
            alice <# "#team cath!> > alice greetings 🤝"
            alice <## "      greetings!"
        )
        ( do
            bob <# "#team cath> > alice greetings 🤝"
            bob <## "      greetings!"
        )

      alice #$> ("/_get chat #1 count=3", chat', [((1, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (1, "hey 👋")), ((0, "greetings!"), Just (1, "greetings 🤝"))])
      bob #$> ("/_get chat #1 count=3", chat', [((0, "greetings 🤝"), Nothing), ((1, "hi alice"), Just (0, "hey 👋")), ((0, "greetings!"), Just (0, "greetings 🤝"))])
      cath #$> ("/_get chat #1 count=3", chat', [((0, "greetings 🤝"), Nothing), ((0, "hi alice"), Just (0, "hey 👋")), ((1, "greetings!"), Just (0, "greetings 🤝"))])

testGroupMessageEditHistory :: HasCallStack => TestParams -> IO ()
testGroupMessageEditHistory =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      threadDelay 1000000
      alice #> "#team hello!"
      bob <# "#team alice> hello!"
      aliceItemId <- lastItemId alice
      bobItemId <- lastItemId bob

      alice ##> ("/_get item info #1 " <> aliceItemId)
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hello!"
      bob ##> ("/_get item info #1 " <> bobItemId)
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hello!"

      alice ##> ("/_update item #1 " <> aliceItemId <> " text hey 👋")
      alice <# "#team [edited] hey 👋"
      bob <# "#team alice> [edited] hey 👋"

      alice ##> ("/_get item info #1 " <> aliceItemId)
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> ("/_get item info #1 " <> bobItemId)
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hey 👋"
      bob .<## ": hello!"

      alice ##> ("/_update item #1 " <> aliceItemId <> " text hello there")
      alice <# "#team [edited] hello there"
      bob <# "#team alice> [edited] hello there"

      alice ##> "/item info #team hello"
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hello there"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> "/item info #team hello"
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hello there"
      bob .<## ": hey 👋"
      bob .<## ": hello!"

      bob #$> ("/_delete item #1 " <> bobItemId <> " internal", id, "message deleted")

      alice ##> ("/_update item #1 " <> aliceItemId <> " text hey there")
      alice <# "#team [edited] hey there"
      bob <# "#team alice> [edited] hey there"

      alice ##> "/item info #team hey"
      alice <##. "sent at: "
      alice <## "message history:"
      alice .<## ": hey there"
      alice .<## ": hello there"
      alice .<## ": hey 👋"
      alice .<## ": hello!"
      bob ##> "/item info #team hey"
      bob <##. "sent at: "
      bob <##. "received at: "
      bob <## "message history:"
      bob .<## ": hey there"

testGroupMessageDelete :: HasCallStack => TestParams -> IO ()
testGroupMessageDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath
      threadDelay 1000000
      -- alice, bob: msg id 5, cath: msg id 4 (after group invitations & group events)
      alice #> "#team hello!"
      concurrently_
        (bob <# "#team alice> hello!")
        (cath <# "#team alice> hello!")

      threadDelay 1000000
      msgItemId1 <- lastItemId alice
      alice #$> ("/_delete item #1 " <> msgItemId1 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=2", chat, [(0, "connected"), (0, "connected")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "hello!")])
      cath #$> ("/_get chat #1 count=1", chat, [(0, "hello!")])

      threadDelay 1000000
      -- alice: msg id 5, bob: msg id 6, cath: msg id 5
      bob `send` "> #team @alice (hello) hi alic"
      bob <# "#team > alice hello!"
      bob <## "      hi alic"
      concurrently_
        ( do
            alice <# "#team bob!> > alice hello!"
            alice <## "      hi alic"
        )
        ( do
            cath <# "#team bob> > alice hello!"
            cath <## "      hi alic"
        )

      alice #$> ("/_get chat #1 count=1", chat', [((0, "hi alic"), Just (1, "hello!"))])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      msgItemId2 <- lastItemId alice
      alice #$> ("/_delete item #1 " <> msgItemId2 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=2", chat', [((0, "connected"), Nothing), ((0, "connected"), Nothing)])
      bob #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((1, "hi alic"), Just (0, "hello!"))])
      cath #$> ("/_get chat #1 count=2", chat', [((0, "hello!"), Nothing), ((0, "hi alic"), Just (0, "hello!"))])

      -- alice: msg id 5
      msgItemId3 <- lastItemId bob
      bob ##> ("/_update item #1 " <> msgItemId3 <> " text hi alice")
      bob <# "#team [edited] > alice hello!"
      bob <## "      hi alice"
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

      msgItemId4 <- lastItemId cath
      cath #$> ("/_delete item #1 " <> msgItemId4 <> " broadcast", id, "message marked deleted")
      concurrently_
        (alice <# "#team cath> [marked deleted] how are you?")
        (bob <# "#team cath> [marked deleted] how are you?")

      alice ##> "/last_item_id 1"
      msgItemId6 <- getTermLine alice
      alice #$> ("/_delete item #1 " <> msgItemId6 <> " broadcast", id, "cannot delete this item")
      alice #$> ("/_delete item #1 " <> msgItemId6 <> " internal", id, "message deleted")

      alice #$> ("/_get chat #1 count=1", chat', [((0, "how are you? [marked deleted]"), Nothing)])
      bob #$> ("/_get chat #1 count=3", chat', [((0, "hello!"), Nothing), ((1, "hi alice"), Just (0, "hello!")), ((0, "how are you? [marked deleted]"), Nothing)])
      cath #$> ("/_get chat #1 count=3", chat', [((0, "hello!"), Nothing), ((0, "hi alice"), Just (0, "hello!")), ((1, "how are you? [marked deleted]"), Nothing)])

testGroupMessageDeleteMultiple :: HasCallStack => TestParams -> IO ()
testGroupMessageDeleteMultiple =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      threadDelay 1000000
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      msgId1 <- lastItemId alice

      threadDelay 1000000
      alice #> "#team hey"
      concurrently_
        (bob <# "#team alice> hey")
        (cath <# "#team alice> hey")
      msgId2 <- lastItemId alice

      threadDelay 1000000
      alice ##> ("/_delete item #1 " <> msgId1 <> "," <> msgId2 <> " broadcast")
      alice <## "2 messages deleted"
      concurrentlyN_
        [ do
            bob <# "#team alice> [marked deleted] hello"
            bob <# "#team alice> [marked deleted] hey",
          do
            cath <# "#team alice> [marked deleted] hello"
            cath <# "#team alice> [marked deleted] hey"
        ]

      alice #$> ("/_get chat #1 count=2", chat, [(1, "hello [marked deleted]"), (1, "hey [marked deleted]")])
      bob #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted]"), (0, "hey [marked deleted]")])
      cath #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted]"), (0, "hey [marked deleted]")])

testGroupMessageDeleteMultipleManyBatches :: HasCallStack => TestParams -> IO ()
testGroupMessageDeleteMultipleManyBatches =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      bob ##> "/set receipts all off"
      bob <## "ok"
      cath ##> "/set receipts all off"
      cath <## "ok"

      msgIdZero <- lastItemId alice

      let cm i = "{\"msgContent\": {\"type\": \"text\", \"text\": \"message " <> show i <> "\"}}"
          cms = intercalate ", " (map cm [1 .. 300 :: Int])

      alice `send` ("/_send #1 json [" <> cms <> "]")
      _ <- getTermLine alice

      alice <## "300 messages sent"

      forM_ [(1 :: Int) .. 300] $ \i -> do
        concurrently_
          (bob <# ("#team alice> message " <> show i))
          (cath <# ("#team alice> message " <> show i))
      msgIdLast <- lastItemId alice

      let mIdFirst = (read msgIdZero :: Int) + 1
          mIdLast = read msgIdLast :: Int
          deleteIds = intercalate "," (map show [mIdFirst .. mIdLast])
      alice `send` ("/_delete item #1 " <> deleteIds <> " broadcast")
      _ <- getTermLine alice
      alice <## "300 messages deleted"
      forM_ [(1 :: Int) .. 300] $ \i ->
        concurrently_
          (bob <# ("#team alice> [marked deleted] message " <> show i))
          (cath <# ("#team alice> [marked deleted] message " <> show i))

testGroupLiveMessage :: HasCallStack => TestParams -> IO ()
testGroupLiveMessage =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    createGroup3 "team" alice bob cath
    threadDelay 500000
    -- non-empty live message is sent instantly
    alice `send` "/live #team hello"
    msgItemId1 <- lastItemId alice
    bob <#. "#team alice> [LIVE started]"
    cath <#. "#team alice> [LIVE started]"
    alice ##> ("/_update item #1 " <> msgItemId1 <> " text hello there")
    alice <# "#team [LIVE] hello there"
    bob <# "#team alice> [LIVE ended] hello there"
    cath <# "#team alice> [LIVE ended] hello there"
    -- empty live message is also sent instantly
    threadDelay 1000000
    alice `send` "/live #team"
    msgItemId2 <- lastItemId alice
    bob <#. "#team alice> [LIVE started]"
    cath <#. "#team alice> [LIVE started]"
    alice ##> ("/_update item #1 " <> msgItemId2 <> " text hello 2")
    alice <# "#team [LIVE] hello 2"
    bob <# "#team alice> [LIVE ended] hello 2"
    cath <# "#team alice> [LIVE ended] hello 2"
    -- live message has edit history
    alice ##> ("/_get item info #1 " <> msgItemId2)
    alice <##. "sent at: "
    alice <## "message history:"
    alice .<## ": hello 2"
    alice .<## ":"
    bobItemId <- lastItemId bob
    bob ##> ("/_get item info #1 " <> bobItemId)
    bob <##. "sent at: "
    bob <##. "received at: "
    bob <## "message history:"
    bob .<## ": hello 2"
    bob .<## ":"

testUpdateGroupProfile :: HasCallStack => TestParams -> IO ()
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
      bob <## "#team: you have insufficient permissions for this action, the required role is owner"
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
      alice ##> "/gp my_team my_team My team"
      alice <## "description changed to: My team"
      concurrentlyN_
        [ do
            bob <## "alice updated group #my_team:"
            bob <## "description changed to: My team",
          do
            cath <## "alice updated group #my_team:"
            cath <## "description changed to: My team"
        ]
      alice ##> "/gp my_team my_team My team updated"
      alice <## "description changed to: My team updated"
      concurrentlyN_
        [ do
            bob <## "alice updated group #my_team:"
            bob <## "description changed to: My team updated",
          do
            cath <## "alice updated group #my_team:"
            cath <## "description changed to: My team updated"
        ]

testUpdateMemberRole :: HasCallStack => TestParams -> IO ()
testUpdateMemberRole =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      addMember "team" alice bob GRAdmin
      alice ##> "/mr team bob member"
      alice <## "#team: you changed the role of bob to member"
      bob <## "#team: alice invites you to join the group as member"
      bob <## "use /j team to accept"
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      connectUsers bob cath
      bob ##> "/a team cath"
      bob <## "#team: you have insufficient permissions for this action, the required role is admin"
      alice ##> "/mr team bob admin"
      concurrently_
        (alice <## "#team: you changed the role of bob to admin")
        (bob <## "#team: alice changed your role from member to admin")
      bob ##> "/a team cath owner"
      bob <## "#team: you have insufficient permissions for this action, the required role is owner"
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
      alice <## "bad chat command: can't change role for self"

testOwnerRoleChange :: HasCallStack => TestParams -> IO ()
testOwnerRoleChange =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      void $ withCCTransaction cath $ \db ->
        DB.execute_
          db
          [sql|
            UPDATE group_members
            SET member_role = 'owner'
            WHERE member_category = 'user'
              AND group_id IN (
                SELECT group_id FROM groups WHERE local_display_name = 'team'
              )
          |]

      cath ##> "/mr #team bob owner"
      cath <## "#team: you changed the role of bob to owner"
      concurrentlyN_
        [ alice <## "error: x.grp.mem.role with insufficient member permissions",
          bob <## "error: x.grp.mem.role with insufficient member permissions"
        ]

      bob ##> "/ms team"
      bob
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): admin, you, connected",
               "cath (Catherine): admin, connected"
             ]

testGroupDescription :: HasCallStack => TestParams -> IO ()
testGroupDescription = testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
  connectUsers alice bob
  alice ##> "/g team"
  alice <## "group #team is created"
  alice <## "to add members use /a team <name> or /create link #team"
  -- alice ##> "/set delete #team off"
  -- alice <## "updated group preferences:"
  -- alice <## "Full deletion: off"
  addMember "team" alice bob GRAdmin
  bob ##> "/j team"
  concurrentlyN_
    [ alice <## "#team: bob joined the group",
      bob <## "#team: you joined the group"
    ]
  alice ##> "/group_profile team"
  alice <## "#team"
  groupInfo' alice
  alice ##> "/set welcome team Welcome to the team!"
  alice <## "welcome message changed to:"
  alice <## "Welcome to the team!"
  bob <## "alice updated group #team:"
  bob <## "welcome message changed to:"
  bob <## "Welcome to the team!"
  alice ##> "/group_profile team"
  alice <## "#team"
  alice <## "welcome message:"
  alice <## "Welcome to the team!"
  groupInfo' alice
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
    groupInfo' :: HasCallStack => TestCC -> IO ()
    groupInfo' alice = do
      alice <## "group preferences:"
      alice <## "Disappearing messages: off"
      alice <## "Direct messages: on"
      alice <## "Full deletion: off"
      alice <## "Message reactions: on"
      alice <## "Voice messages: on"
      alice <## "Files and media: on"
      alice <## "SimpleX links: on"
      alice <## "Member reports: on"
      alice <## "Recent history: on"
      alice <## "Chat with admins: on"
    bobAddedDan :: HasCallStack => TestCC -> IO ()
    bobAddedDan cc = do
      cc <## "#team: bob added dan (Daniel) to the group (connecting...)"
      cc <## "#team: new member dan is connected"

testGroupModerate :: HasCallStack => TestParams -> IO ()
testGroupModerate =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath
      alice ##> "/mr team cath member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of cath to member",
          bob <## "#team: alice changed the role of cath from admin to member",
          cath <## "#team: alice changed your role from admin to member"
        ]
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      bob ##> "\\\\ #team @alice hello"
      bob <## "cannot delete this item"
      threadDelay 1000000
      cath #> "#team hi"
      concurrently_
        (alice <# "#team cath> hi")
        (bob <# "#team cath> hi")
      bob ##> "\\\\ #team @cath hi"
      bob <## "message marked deleted by you"
      concurrently_
        (alice <# "#team cath> [marked deleted by bob] hi")
        (cath <# "#team cath> [marked deleted by bob] hi")
      alice #$> ("/_get chat #1 count=1", chat, [(0, "hi [marked deleted by bob]")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "hi [marked deleted by you]")])
      cath #$> ("/_get chat #1 count=1", chat, [(1, "hi [marked deleted by bob]")])

testGroupModerateOwn :: HasCallStack => TestParams -> IO ()
testGroupModerateOwn =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      -- disableFullDeletion2 "team" alice bob
      threadDelay 1000000
      alice #> "#team hello"
      bob <# "#team alice> hello"
      alice ##> "\\\\ #team @alice hello"
      alice <## "message marked deleted by you"
      bob <# "#team alice> [marked deleted by alice] hello"
      alice #$> ("/_get chat #1 count=1", chat, [(1, "hello [marked deleted by you]")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "hello [marked deleted by alice]")])

testGroupModerateMultiple :: HasCallStack => TestParams -> IO ()
testGroupModerateMultiple =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      threadDelay 1000000
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      msgId1 <- lastItemId alice

      threadDelay 1000000
      bob #> "#team hey"
      concurrently_
        (alice <# "#team bob> hey")
        (cath <# "#team bob> hey")
      msgId2 <- lastItemId alice

      alice ##> ("/_delete member item #1 " <> msgId1 <> "," <> msgId2)
      alice <## "2 messages deleted"
      concurrentlyN_
        [ do
            bob <# "#team alice> [marked deleted by alice] hello"
            bob <# "#team bob> [marked deleted by alice] hey",
          do
            cath <# "#team alice> [marked deleted by alice] hello"
            cath <# "#team bob> [marked deleted by alice] hey"
        ]

      alice #$> ("/_get chat #1 count=2", chat, [(1, "hello [marked deleted by you]"), (0, "hey [marked deleted by you]")])
      bob #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted by alice]"), (1, "hey [marked deleted by alice]")])
      cath #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted by alice]"), (0, "hey [marked deleted by alice]")])

testGroupModerateFullDelete :: HasCallStack => TestParams -> IO ()
testGroupModerateFullDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath
      alice ##> "/mr team cath member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of cath to member",
          bob <## "#team: alice changed the role of cath from admin to member",
          cath <## "#team: alice changed your role from admin to member"
        ]
      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "updated group preferences:"
            bob <## "Full deletion: on",
          do
            cath <## "alice updated group #team:"
            cath <## "updated group preferences:"
            cath <## "Full deletion: on"
        ]
      threadDelay 1000000
      cath #> "#team hi"
      concurrently_
        (alice <# "#team cath> hi")
        (bob <# "#team cath> hi")
      bob ##> "\\\\ #team @cath hi"
      bob <## "message deleted by you"
      concurrently_
        (alice <# "#team cath> [deleted by bob] hi")
        (cath <# "#team cath> [deleted by bob] hi")
      alice #$> ("/_get chat #1 count=1", chat, [(0, "moderated [deleted by bob]")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "moderated [deleted by you]")])
      cath #$> ("/_get chat #1 count=1", chat, [(1, "moderated [deleted by bob]")])

testGroupDelayedModeration :: HasCallStack => TestParams -> IO ()
testGroupDelayedModeration ps = do
  withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg ps cfg "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
      -- disableFullDeletion2 "team" alice bob
    withNewTestChatCfg ps cfg "cath" cathProfile $ \cath -> do
      connectUsers alice cath
      addMember "team" alice cath GRMember
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath <## "#team: you joined the group"
        ]
      threadDelay 1000000

      -- imitate not implemented group forwarding
      -- (real client wouldn't have forwarding code, but tests use "current code" with configured version,
      -- and forwarding client doesn't check compatibility)
      updateGroupForwardingVectors alice "bob" "cath" MRConnected

      cath #> "#team hi" -- message is pending for bob
      alice <# "#team cath> hi"
      alice ##> "\\\\ #team @cath hi"
      alice <## "message marked deleted by you"
      cath <# "#team cath> [marked deleted by alice] hi"
    withTestChatCfg ps cfg "bob" $ \bob -> do
      bob <## "subscribed 2 connections on server localhost"
      bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
      withTestChatCfg ps cfg "cath" $ \cath -> do
        cath <## "subscribed 3 connections on server localhost"
        cath <## "#team: member bob (Bob) is connected"
        bob
          <### [ "#team: new member cath is connected",
                 EndsWith "#team cath> [marked deleted by alice] hi"
               ]
        alice #$> ("/_get chat #1 count=1", chat, [(0, "hi [marked deleted by you]")])
        cath #$> ("/_get chat #1 count=2", chat, [(1, "hi [marked deleted by alice]"), (0, "connected")])
        bob ##> "/_get chat #1 count=2"
        r <- chat <$> getTermLine bob
        r `shouldMatchList` [(0, "connected"), (0, "hi [marked deleted by alice]")]
  where
    -- version before forwarding, so cath doesn't expect alice to forward messages (groupForwardVersion = 4)
    cfg = testCfg {chatVRange = mkVersionRange (VersionChat 1) (VersionChat 3)}

testGroupDelayedModerationFullDelete :: HasCallStack => TestParams -> IO ()
testGroupDelayedModerationFullDelete ps = do
  withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg ps cfg "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
      -- disableFullDeletion2 "team" alice bob
    withNewTestChatCfg ps cfg "cath" cathProfile $ \cath -> do
      connectUsers alice cath
      addMember "team" alice cath GRMember
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath <## "#team: you joined the group"
        ]
      threadDelay 1000000

      -- imitate not implemented group forwarding
      -- (real client wouldn't have forwarding code, but tests use "current code" with configured version,
      -- and forwarding client doesn't check compatibility)
      updateGroupForwardingVectors alice "bob" "cath" MRConnected

      cath #> "#team hi" -- message is pending for bob
      alice <# "#team cath> hi"
      alice ##> "\\\\ #team @cath hi"
      alice <## "message marked deleted by you"
      cath <# "#team cath> [marked deleted by alice] hi"
      -- if full deletion was enabled at time of moderation, cath would delete pending message as well,
      -- that's why we set it afterwards to test delayed moderation for bob
      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      cath <## "alice updated group #team:"
      cath <## "updated group preferences:"
      cath <## "Full deletion: on"
    withTestChatCfg ps cfg "bob" $ \bob -> do
      bob <## "subscribed 2 connections on server localhost"
      bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Full deletion: on"
      withTestChatCfg ps cfg "cath" $ \cath -> do
        cath <## "subscribed 3 connections on server localhost"
        cath <## "#team: member bob (Bob) is connected"
        bob
          <### [ "#team: new member cath is connected",
                 EndsWith "#team cath> moderated [deleted by alice]"
               ]
        alice #$> ("/_get chat #1 count=2", chat, [(0, "hi [marked deleted by you]"), (1, "Full deletion: on")])
        cath #$> ("/_get chat #1 count=3", chat, [(1, "hi [marked deleted by alice]"), (0, "Full deletion: on"), (0, "connected")])
        bob ##> "/_get chat #1 count=3"
        r <- chat <$> getTermLine bob
        r `shouldMatchList` [(0, "Full deletion: on"), (0, "connected"), (0, "moderated [deleted by alice]")]
  where
    -- version before forwarding, so cath doesn't expect alice to forward messages (groupForwardVersion = 4)
    cfg = testCfg {chatVRange = mkVersionRange (VersionChat 1) (VersionChat 3)}

testDeleteMemberWithMessages :: HasCallStack => TestParams -> IO ()
testDeleteMemberWithMessages =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup3' "team" alice (bob, GRMember) (cath, GRMember)
      threadDelay 750000
      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      threadDelay 750000
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "updated group preferences:"
            bob <## "Full deletion: on",
          do
            cath <## "alice updated group #team:"
            cath <## "updated group preferences:"
            cath <## "Full deletion: on"
        ]
      threadDelay 750000

      alice #$> ("/_files_folder ./tests/tmp/alice_app_files", id, "ok")
      bob #$> ("/_files_folder ./tests/tmp/bob_app_files", id, "ok")
      cath #$> ("/_files_folder ./tests/tmp/cath_app_files", id, "ok")
      copyFile "./tests/fixtures/test.jpg" "./tests/tmp/bob_app_files/test.jpg"

      bob ##> "/_send #1 json [{\"filePath\": \"test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"file from bob\"}}]"
      bob <# "#team file from bob"
      bob <# "/f #team test.jpg"
      bob <## "use /fc 1 to cancel sending"

      alice <# "#team bob> file from bob"
      alice <# "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      cath <# "#team bob> file from bob"
      cath <# "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
      cath <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob <## "completed uploading file 1 (test.jpg) for #team"

      alice ##> "/fr 1"
      alice
        <### [ "saving file 1 from bob to test.jpg",
               "started receiving file 1 (test.jpg) from bob"
             ]
      alice <## "completed receiving file 1 (test.jpg) from bob"

      cath ##> "/fr 1"
      cath
        <### [ "saving file 1 from bob to test.jpg",
               "started receiving file 1 (test.jpg) from bob"
             ]
      cath <## "completed receiving file 1 (test.jpg) from bob"

      src <- B.readFile "./tests/fixtures/test.jpg"
      B.readFile "./tests/tmp/alice_app_files/test.jpg" `shouldReturn` src
      B.readFile "./tests/tmp/bob_app_files/test.jpg" `shouldReturn` src
      B.readFile "./tests/tmp/cath_app_files/test.jpg" `shouldReturn` src

      threadDelay 1000000
      alice ##> "/rm #team bob messages=on"
      alice <## "#team: you removed bob from the group with all messages"
      bob <## "#team: alice removed you from the group with all messages"
      bob <## "use /d #team to delete the group"
      cath <## "#team: alice removed bob from the group with all messages"

      doesFileExist "./tests/tmp/alice_app_files/test.jpg" `shouldReturn` False
      doesFileExist "./tests/tmp/bob_app_files/test.jpg" `shouldReturn` False
      doesFileExist "./tests/tmp/cath_app_files/test.jpg" `shouldReturn` False

      -- Under fullDelete, bob's items are physically deleted on all sides; only the system event remains.
      alice #$> ("/_get chat #1 count=1", chat, [(1, "removed bob")])
      bob #$> ("/_get chat #1 count=1", chat, [(0, "removed you")])
      cath #$> ("/_get chat #1 count=1", chat, [(0, "removed bob")])

testDeleteMemberMarkMessagesDeleted :: HasCallStack => TestParams -> IO ()
testDeleteMemberMarkMessagesDeleted =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GRMember) (cath, GRMember)
      threadDelay 1000000
      bob #> "#team hello"
      concurrently_
        (alice <# "#team bob> hello")
        (cath <# "#team bob> hello")
      alice #$> ("/_get chat #1 count=1", chat, [(0, "hello")])
      bob #$> ("/_get chat #1 count=1", chat, [(1, "hello")])
      cath #$> ("/_get chat #1 count=1", chat, [(0, "hello")])
      threadDelay 1000000
      alice ##> "/rm #team bob messages=on"
      alice <## "#team: you removed bob from the group with all messages"
      bob <## "#team: alice removed you from the group with all messages"
      bob <## "use /d #team to delete the group"
      cath <## "#team: alice removed bob from the group with all messages"
      alice #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted by you]"), (1, "removed bob")])
      bob #$> ("/_get chat #1 count=2", chat, [(1, "hello [marked deleted by alice]"), (0, "removed you")])
      cath #$> ("/_get chat #1 count=2", chat, [(0, "hello [marked deleted by alice]"), (0, "removed bob")])

testDeleteMemberMessagesLeftRemoved :: HasCallStack => TestParams -> IO ()
testDeleteMemberMessagesLeftRemoved =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRMember)

      threadDelay 1000000
      cath #> "#team 1"
      [alice, bob, dan] *<# "#team cath> 1"

      threadDelay 1000000
      dan #> "#team 2"
      [alice, bob, cath] *<# "#team dan> 2"

      alice #$> ("/_get chat #1 count=2", chat, [(0, "1"), (0, "2")])
      bob #$> ("/_get chat #1 count=2", chat, [(0, "1"), (0, "2")])
      cath #$> ("/_get chat #1 count=2", chat, [(1, "1"), (0, "2")])
      dan #$> ("/_get chat #1 count=2", chat, [(0, "1"), (1, "2")])

      threadDelay 1000000
      cath ##> "/leave #team"
      concurrentlyN_
        [ do
            cath <## "#team: you left the group"
            cath <## "use /d #team to delete the group",
          alice <## "#team: cath left the group",
          bob <## "#team: cath left the group",
          dan <## "#team: cath left the group"
        ]

      threadDelay 1000000
      alice ##> "/rm team dan"
      concurrentlyN_
        [ alice <## "#team: you removed dan from the group",
          do
            dan <## "#team: alice removed you from the group"
            dan <## "use /d #team to delete the group",
          bob <## "#team: alice removed dan from the group"
        ]

      alice ##> "/rm #team cath messages=on"
      alice <## "#team: you removed cath from the group with all messages"
      bob <## "#team: alice removed cath from the group with all messages"

      alice ##> "/rm #team dan messages=on"
      alice <## "#team: you removed dan from the group with all messages"
      bob <## "#team: alice removed dan from the group with all messages"

      alice #$> ("/_get chat #1 count=4", chat, [(0, "1 [marked deleted by you]"), (0, "2 [marked deleted by you]"), (0, "left [marked deleted by you]"), (1, "removed dan")])
      bob #$> ("/_get chat #1 count=4", chat, [(0, "1 [marked deleted by alice]"), (0, "2 [marked deleted by alice]"), (0, "left [marked deleted by alice]"), (0, "removed dan")])
      cath #$> ("/_get chat #1 count=3", chat, [(1, "1"), (0, "2"), (1, "left")])
      dan #$> ("/_get chat #1 count=4", chat, [(0, "1"), (1, "2"), (0, "left"), (0, "removed you")])

testSendMulti :: HasCallStack => TestParams -> IO ()
testSendMulti =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      alice ##> "/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"test 1\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 2\"}}]"
      alice <# "#team test 1"
      alice <# "#team test 2"
      bob <# "#team alice> test 1"
      bob <# "#team alice> test 2"
      cath <# "#team alice> test 1"
      cath <# "#team alice> test 2"

testSendMultiTimed :: HasCallStack => TestParams -> IO ()
testSendMultiTimed =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      alice ##> "/set disappear #team on 1"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: on (1 sec)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: on (1 sec)"
      cath <## "alice updated group #team:"
      cath <## "updated group preferences:"
      cath <## "Disappearing messages: on (1 sec)"

      alice ##> "/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"test 1\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 2\"}}]"
      alice <# "#team test 1"
      alice <# "#team test 2"
      bob <# "#team alice> test 1"
      bob <# "#team alice> test 2"
      cath <# "#team alice> test 1"
      cath <# "#team alice> test 2"

      alice
        <### [ "timed message deleted: test 1",
               "timed message deleted: test 2"
             ]
      bob
        <### [ "timed message deleted: test 1",
               "timed message deleted: test 2"
             ]
      cath
        <### [ "timed message deleted: test 1",
               "timed message deleted: test 2"
             ]

testSendMultiManyBatches :: HasCallStack => TestParams -> IO ()
testSendMultiManyBatches =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      msgIdAlice <- lastItemId alice
      msgIdBob <- lastItemId bob
      msgIdCath <- lastItemId cath

      let cm i = "{\"msgContent\": {\"type\": \"text\", \"text\": \"message " <> show i <> "\"}}"
          cms = intercalate ", " (map cm [1 .. 300 :: Int])

      alice `send` ("/_send #1 json [" <> cms <> "]")
      _ <- getTermLine alice

      alice <## "300 messages sent"

      forM_ [(1 :: Int) .. 300] $ \i -> do
        concurrently_
          (bob <# ("#team alice> message " <> show i))
          (cath <# ("#team alice> message " <> show i))

      checkItemCount alice msgIdAlice 300
      checkItemCount bob msgIdBob 300
      checkItemCount cath msgIdCath 300
  where
    checkItemCount c msgId n = do
      itemsCount <- withCCTransaction c $ \db ->
        DB.query db "SELECT count(1) FROM chat_items WHERE chat_item_id > ?" (Only msgId) :: IO [[Int]]
      itemsCount `shouldBe` [[n]]

testSharedMessageBody :: HasCallStack => TestParams -> IO ()
testSharedMessageBody ps' =
  withNewTestChatOpts ps opts' "alice" aliceProfile $ \alice -> do
    withSmpServer' serverCfg' $
      withNewTestChatOpts ps opts' "bob" bobProfile $ \bob ->
        withNewTestChatOpts ps opts' "cath" cathProfile $ \cath -> do
          createGroup3 "team" alice bob cath

    alice <## "disconnected 4 connections on server localhost"
    alice #> "#team hello"
    checkMsgBodyCount alice 1

    withSmpServer' serverCfg' $
      withTestChatOpts ps opts' "bob" $ \bob ->
        withTestChatOpts ps opts' "cath" $ \cath -> do
          concurrentlyN_
            [ alice <## "subscribed 4 connections on server localhost",
              bob <## "subscribed 3 connections on server localhost",
              cath <## "subscribed 3 connections on server localhost"
            ]
          bob <# "#team alice> hello"
          cath <# "#team alice> hello"
          threadDelay 500000
          checkMsgBodyCount alice 0

    alice <## "disconnected 4 connections on server localhost"
  where
    ps = ps' {printOutput = True} :: TestParams
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

checkMsgBodyCount :: TestCC -> Int -> IO ()
checkMsgBodyCount c n = do
  bodiesCount <- withCCAgentTransaction c $ \db ->
    DB.query_ db "SELECT count(1) FROM snd_message_bodies"
  bodiesCount `shouldBe` [[n]]

testSharedBatchBody :: HasCallStack => TestParams -> IO ()
testSharedBatchBody ps =
  withNewTestChatOpts ps opts' "alice" aliceProfile $ \alice -> do
    withSmpServer' serverCfg' $
      withNewTestChatOpts ps opts' "bob" bobProfile $ \bob ->
        withNewTestChatOpts ps opts' "cath" cathProfile $ \cath -> do
          createGroup3 "team" alice bob cath

    alice <## "disconnected 4 connections on server localhost"

    let cm i = "{\"msgContent\": {\"type\": \"text\", \"text\": \"message " <> show i <> "\"}}"
        cms = intercalate ", " (map cm [1 .. 300 :: Int])
    alice `send` ("/_send #1 json [" <> cms <> "]")
    _ <- getTermLine alice
    alice <## "300 messages sent"

    checkMsgBodyCount alice 3

    withSmpServer' serverCfg' $
      withTestChatOpts ps opts' "bob" $ \bob ->
        withTestChatOpts ps opts' "cath" $ \cath -> do
          concurrentlyN_
            [ alice <## "subscribed 4 connections on server localhost",
              bob <## "subscribed 3 connections on server localhost",
              cath <## "subscribed 3 connections on server localhost"
            ]
          forM_ [(1 :: Int) .. 300] $ \i -> do
            concurrently_
              (bob <# ("#team alice> message " <> show i))
              (cath <# ("#team alice> message " <> show i))
          checkMsgBodyCount alice 0

    alice <## "disconnected 4 connections on server localhost"
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

testGroupAsync :: HasCallStack => TestParams -> IO ()
testGroupAsync ps = do
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChat ps "bob" bobProfile $ \bob -> do
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
  withTestChat ps "alice" $ \alice -> do
    withNewTestChat ps "cath" cathProfile $ \cath -> do
      alice <## "subscribed 2 connections on server localhost"
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
  withTestChat ps "bob" $ \bob -> do
    withTestChat ps "cath" $ \cath -> do
      concurrentlyN_
        [ do
            bob <## "subscribed 2 connections on server localhost"
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <# "#team alice> hello cath"
            bob <## "#team: new member cath is connected",
          do
            cath <## "subscribed 3 connections on server localhost"
            cath <## "#team: member bob (Bob) is connected"
        ]
  threadDelay 500000
  withTestChat ps "bob" $ \bob -> do
    withNewTestChat ps "dan" danProfile $ \dan -> do
      bob <## "subscribed 4 connections on server localhost"
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
  withTestChat ps "alice" $ \alice -> do
    withTestChat ps "cath" $ \cath -> do
      withTestChat ps "dan" $ \dan -> do
        concurrentlyN_
          [ do
              alice <## "subscribed 4 connections on server localhost"
              alice <## "#team: bob added dan (Daniel) to the group (connecting...)"
              alice <## "#team: new member dan is connected",
            do
              cath <## "subscribed 4 connections on server localhost"
              cath <## "#team: bob added dan (Daniel) to the group (connecting...)"
              cath <## "#team: new member dan is connected",
            do
              dan <## "subscribed 5 connections on server localhost"
              dan <## "#team: member alice (Alice) is connected"
              dan <## "#team: member cath (Catherine) is connected"
          ]
        threadDelay 1000000
  withTestChat ps "alice" $ \alice -> do
    withTestChat ps "bob" $ \bob -> do
      withTestChat ps "cath" $ \cath -> do
        withTestChat ps "dan" $ \dan -> do
          concurrentlyN_
            [ alice <## "subscribed 6 connections server localhost",
              bob <## "subscribed 6 connections server localhost",
              cath <## "subscribed 6 connections server localhost",
              dan <## "subscribed 6 connections server localhost"
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

testGroupLinkDeleteGroupRejoin :: HasCallStack => TestParams -> IO ()
testGroupLinkDeleteGroupRejoin =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]
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
        [ alice <## "#team: bob_1 joined the group",
          bob
            <### [ "#team: joining the group...",
                   "#team: you joined the group"
                 ]
        ]
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob_1> hi there"

testGroupLinkIncognitoJoinInvite :: HasCallStack => TestParams -> IO ()
testGroupLinkIncognitoJoinInvite =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      -- bob connected incognito to alice
      alice ##> "/c"
      inv <- getInvitation alice
      bob ##> ("/c i " <> inv)
      bob <## "confirmation sent!"
      bobIncognito <- getTermLine bob
      concurrentlyN_
        [ do
            bob <## ("alice (Alice): contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## "use /i alice to print out this incognito profile again",
          alice <## (bobIncognito <> ": contact is connected")
        ]
      -- alice creates group
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      -- alice invites bob
      alice ##> ("/a team " <> bobIncognito <> " admin")
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
      gLink <- getGroupLink bob "team" GRMember True
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      bob <## "cath (Catherine): accepting request to join group #team..."
      concurrentlyN_
        [ bob <## "#team: cath joined the group",
          do
            cath <## "#team: joining the group..."
            cath <## "#team: you joined the group"
            cath <## "#team: member alice (Alice) is connected",
          do
            alice <## ("#team: " <> bobIncognito <> " added cath (Catherine) to the group (connecting...)")
            alice <## "#team: new member cath is connected"
        ]
      -- dan joins incognito
      dan ##> ("/c i " <> gLink)
      danIncognito <- getTermLine dan
      dan <## "connection request sent incognito!"
      bob <## (danIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ bob <## ("#team: " <> danIncognito <> " joined the group"),
          do
            dan <## "#team: joining the group..."
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

testPlanGroupLinkOwn :: HasCallStack => TestParams -> IO ()
testPlanGroupLinkOwn ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    threadDelay 100000
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> "/create link #team"
    gLink <- getGroupLink alice "team" GRMember True

    alice ##> ("/_connect plan 1 " <> gLink)
    alice <## "group link: own link for group #team"

    let gLinkSchema2 = linkAnotherSchema gLink
    alice ##> ("/_connect plan 1 " <> gLinkSchema2)
    alice <## "group link: own link for group #team"

    alice ##> ("/c " <> gLink)
    alice <## "connection request sent!"
    alice <## "alice_1 (Alice): accepting request to join group #team..."
    alice
      <### [ "#team: alice_1 joined the group",
             "#team_1: joining the group...",
             "#team_1: you joined the group"
           ]
    alice `send` "#team 1"
    alice
      <### [ WithTime "#team 1",
             WithTime "#team_1 alice_2> 1"
           ]
    alice `send` "#team_1 2"
    alice
      <### [ WithTime "#team_1 2",
             WithTime "#team alice_1> 2"
           ]

    alice ##> ("/_connect plan 1 " <> gLink)
    alice <## "group link: own link for group #team"

    alice ##> ("/_connect plan 1 " <> gLinkSchema2)
    alice <## "group link: own link for group #team"

testPlanGroupLinkLeaveRejoin :: HasCallStack => TestParams -> IO ()
testPlanGroupLinkLeaveRejoin =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True

      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000

      bob ##> ("/_connect plan 1 " <> gLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"

      bob ##> ("/c " <> gLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"

      threadDelay 100000

      bob ##> "/leave #team"
      concurrentlyN_
        [ do
            bob <## "#team: you left the group"
            bob <## "use /d #team to delete the group",
          alice <## "#team: bob left the group"
        ]

      threadDelay 100000

      bob ##> ("/_connect plan 1 " <> gLink)
      bob <## "group link: ok to connect directly"
      _sLinkData <- getTermLine bob

      let gLinkSchema2 = linkAnotherSchema gLink
      bob ##> ("/_connect plan 1 " <> gLinkSchema2)
      bob <## "group link: ok to connect directly"
      _sLinkData <- getTermLine bob

      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob_1 joined the group",
          bob
            <### [ "#team_1: joining the group...",
                   "#team_1: you joined the group"
                 ]
        ]

      alice #> "#team hi"
      bob <# "#team_1 alice_1> hi"
      bob #> "#team_1 hey"
      alice <# "#team bob_1> hey"

      bob ##> ("/_connect plan 1 " <> gLink)
      bob <## "group link: known group #team_1"
      bob <## "use #team_1 <message> to send messages"

      bob ##> ("/_connect plan 1 " <> gLinkSchema2)
      bob <## "group link: known group #team_1"
      bob <## "use #team_1 <message> to send messages"

      bob ##> ("/c " <> gLink)
      bob <## "group link: known group #team_1"
      bob <## "use #team_1 <message> to send messages"

testGroupLink :: HasCallStack => TestParams -> IO ()
testGroupLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"

      alice ##> "/set history #team off"
      alice <## "updated group preferences:"
      alice <## "Recent history: off"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(1, "Recent history: off"), (0, "invited via your group link"), (0, "connected")])

      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected")]
      alice ##> "/contacts"
      bob ##> "/contacts"

      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

      -- user address doesn't interfere
      alice ##> "/ad"
      cLink <- getContactLink alice True
      cath ##> ("/c " <> cLink)
      alice <#? cath
      alice ##> "/ac cath"
      alice <## "cath (Catherine): accepting contact request, you can send messages to contact"
      concurrently_
        (cath <## "alice (Alice): contact is connected")
        (alice <## "cath (Catherine): contact is connected")
      alice <##> cath

      -- third member
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      concurrentlyN_
        [ do
            alice <## "cath_1 (Catherine): accepting request to join group #team..."
            alice <## "#team: cath_1 joined the group"
            alice <## "contact and member are merged: cath, #team cath_1"
            alice <## "use @cath <message> to send messages",
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   "#team: member bob (Bob) is connected",
                   "contact and member are merged: alice, #team alice_1",
                   "use @alice <message> to send messages"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath #> "#team hey"
      alice <# "#team cath> hey"
      bob <# "#team cath> hey"

      bob #> "#team hi cath"
      alice <# "#team bob> hi cath"
      cath <# "#team bob> hi cath"

      -- leaving group removes link
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

      -- deleting group keeps contacts
      alice ##> "/contacts"
      alice <## "cath (Catherine)"
      alice ##> "/d #team"
      alice <## "#team: you deleted the group"
      alice ##> "/contacts"
      alice <## "cath (Catherine)"

testGroupLinkInviteesWereConnected :: HasCallStack => TestParams -> IO ()
testGroupLinkInviteesWereConnected =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers bob cath
      bob <##> cath

      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"

      alice ##> "/set history #team off"
      alice <## "updated group preferences:"
      alice <## "Recent history: off"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(1, "Recent history: off"), (0, "invited via your group link"), (0, "connected")])

      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected"), ("@cath", "hey")]
      alice ##> "/contacts"
      bob ##> "/contacts"
      bob <## "cath (Catherine)"

      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      concurrentlyN_
        [ do
            alice <## "cath (Catherine): accepting request to join group #team..."
            alice <## "#team: cath joined the group",
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   "#team: member bob_1 (Bob) is connected",
                   "contact and member are merged: bob, #team bob_1",
                   "use @bob <message> to send messages"
                 ],
          bob
            <### [ "#team: alice added cath_1 (Catherine) to the group (connecting...)",
                   "#team: new member cath_1 is connected",
                   "contact and member are merged: cath, #team cath_1",
                   "use @cath <message> to send messages"
                 ]
        ]

      -- message delivery works
      bob <##> cath

      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

testGroupLinkAllMembersWereConnected :: HasCallStack => TestParams -> IO ()
testGroupLinkAllMembersWereConnected =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      alice <##> bob
      connectUsers alice cath
      alice <##> cath
      connectUsers bob cath
      bob <##> cath

      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"

      alice ##> "/set history #team off"
      alice <## "updated group preferences:"
      alice <## "Recent history: off"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "#team: bob_1 joined the group"
            alice <## "contact and member are merged: bob, #team bob_1"
            alice <## "use @bob <message> to send messages",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
            bob <## "contact and member are merged: alice, #team alice_1"
            bob <## "use @alice <message> to send messages"
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(1, "Recent history: off"), (0, "invited via your group link"), (0, "connected")])

      alice @@@ [("#team", "connected"), ("@bob", "hey"), ("@cath", "hey")]
      bob @@@ [("#team", "connected"), ("@alice", "hey"), ("@cath", "hey")]
      alice ##> "/contacts"
      alice <## "bob (Bob)"
      alice <## "cath (Catherine)"
      bob ##> "/contacts"
      bob <## "alice (Alice)"
      bob <## "cath (Catherine)"

      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      concurrentlyN_
        [ alice
            <### [ "cath_1 (Catherine): accepting request to join group #team...",
                   "#team: cath_1 joined the group",
                   "contact and member are merged: cath, #team cath_1",
                   "use @cath <message> to send messages"
                 ],
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   "#team: member bob_1 (Bob) is connected",
                   "contact and member are merged: bob, #team bob_1",
                   "use @bob <message> to send messages",
                   "contact and member are merged: alice, #team alice_1",
                   "use @alice <message> to send messages"
                 ],
          bob
            <### [ "#team: alice added cath_1 (Catherine) to the group (connecting...)",
                   "#team: new member cath_1 is connected",
                   "contact and member are merged: cath, #team cath_1",
                   "use @cath <message> to send messages"
                 ]
        ]

      -- message delivery works
      alice <##> bob
      alice <##> cath
      bob <##> cath

      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

testGroupLinkMemberRole :: HasCallStack => TestParams -> IO ()
testGroupLinkMemberRole =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team observer"
      gLink <- getGroupLink alice "team" GRObserver True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000

      alice ##> "/ms team"
      alice
        <### [ "alice (Alice): owner, you, created group",
               "bob (Bob): observer, invited, connected"
             ]

      bob ##> "/ms team"
      bob
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): observer, you, connected"
             ]

      bob ##> "#team hi there"
      bob <## "#team: you don't have permission to send messages"

      alice ##> "/mr #team bob member"
      alice <## "#team: you changed the role of bob to member"
      bob <## "#team: alice changed your role from observer to member"

      bob #> "#team hey now"
      alice <# "#team bob> hey now"

      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      concurrentlyN_
        [ do
            alice <## "cath (Catherine): accepting request to join group #team..."
            alice <## "#team: cath joined the group",
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   WithTime "#team bob> hey now [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]
      bob #> "#team hi cath"
      alice <# "#team bob> hi cath"
      cath <# "#team bob> hi cath"

      cath ##> "#team hey"
      cath <## "#team: you don't have permission to send messages"

      alice ##> "/mr #team cath admin"
      alice <## "#team: you changed the role of cath to admin"
      cath <## "#team: alice changed your role from observer to admin"
      bob <## "#team: alice changed the role of cath from observer to admin"

      cath #> "#team hey"
      alice <# "#team cath> hey"
      bob <# "#team cath> hey"

      cath ##> "/mr #team bob admin"
      cath <## "#team: you changed the role of bob to admin"
      bob <## "#team: cath changed your role from member to admin"
      alice <## "#team: cath changed the role of bob from member to admin"

testGroupLinkDemotedAdmin :: HasCallStack => TestParams -> IO ()
testGroupLinkDemotedAdmin =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob _cath -> do
      createGroup2' "team" alice (bob, GRAdmin) True

      bob ##> "/create link #team member"
      _gLink <- getGroupLink bob "team" GRMember True

      alice ##> "/mr #team bob member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of bob to member",
          bob <## "#team: alice changed your role from admin to member"
        ]

      -- demotion does not remove bob's group link (it is preserved, usable again on re-promotion)
      bob ##> "/show link #team"
      void $ getGroupLink bob "team" GRMember False

testGroupLinkHostIncognito :: HasCallStack => TestParams -> IO ()
testGroupLinkHostIncognito =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/g i team"
      aliceIncognito <- getTermLine alice
      alice <## ("group #team is created, your incognito profile for this group is " <> aliceIncognito)
      alice <## "to add members use /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "invited via your group link"), (0, "connected")])

      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected")]
      alice ##> "/contacts"
      bob ##> "/contacts"

      alice ?#> "#team hello"
      bob <# ("#team " <> aliceIncognito <> "> hello")
      bob #> "#team hi there"
      alice ?<# "#team bob> hi there"

testGroupLinkInviteeIncognito :: HasCallStack => TestParams -> IO ()
testGroupLinkInviteeIncognito =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c i " <> gLink)
      bobIncognito <- getTermLine bob
      bob <## "connection request sent incognito!"
      alice <## (bobIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> bobIncognito <> " joined the group"),
          do
            bob <## "#team: joining the group..."
            bob <## ("#team: you joined the group incognito as " <> bobIncognito)
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "invited via your group link"), (0, "connected")])

      alice @@@ [("#team", "connected")]
      bob @@@ [("#team", "connected")]
      alice ##> "/contacts"
      bob ##> "/contacts"

      alice #> "#team hello"
      bob ?<# "#team alice> hello"
      bob ?#> "#team hi there"
      alice <# ("#team " <> bobIncognito <> "> hi there")

testGroupLinkHostProfileReceived :: HasCallStack => TestParams -> IO ()
testGroupLinkHostProfileReceived =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      let profileImage = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="
      alice ##> ("/set profile image " <> profileImage)
      alice <## "profile image updated"

      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      threadDelay 100000

      aliceImage <- getProfilePictureByName bob "alice"
      aliceImage `shouldBe` Just profileImage

testGroupLinkExistingContactMerged :: HasCallStack => TestParams -> IO ()
testGroupLinkExistingContactMerged =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob

      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob_1 (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ do
            alice <## "#team: bob_1 joined the group"
            alice <## "contact and member are merged: bob, #team bob_1"
            alice <## "use @bob <message> to send messages",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
            bob <## "contact and member are merged: alice, #team alice_1"
            bob <## "use @alice <message> to send messages"
        ]

      threadDelay 100000
      alice #$> ("/_get chat #1 count=100", chat, sndGroupFeatures <> [(0, "invited via your group link"), (0, "connected")])

      alice <##> bob

      alice @@@ [("#team", "connected"), ("@bob", "hey")]
      bob @@@ [("#team", "connected"), ("@alice", "hey")]
      alice ##> "/contacts"
      alice <## "bob (Bob)"
      bob ##> "/contacts"
      bob <## "alice (Alice)"

      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

testGLinkRejectBlockedName :: HasCallStack => TestParams -> IO ()
testGLinkRejectBlockedName =
  testChatCfg2 cfg aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): rejecting request to join group #team, reason: GRRBlockedName"
      bob <## "#team: joining the group..."
      bob <## "#team: join rejected, reason: GRRBlockedName"

      threadDelay 100000

      alice `hasContactProfiles` ["alice"]
      memCount <- withCCTransaction alice $ \db ->
        DB.query_ db "SELECT count(1) FROM group_members" :: IO [[Int]]
      memCount `shouldBe` [[1]]

      -- rejected member can't send messages to group
      bob ##> "#team hello"
      bob <## "bad chat command: not current member"

      bob ##> ("/c " <> gLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"
  where
    cfg = testCfg {chatHooks = defaultChatHooks {acceptMember = Just (\_ _ _ -> pure $ Left GRRBlockedName)}}

testGLinkApproveMember :: HasCallStack => TestParams -> IO ()
testGLinkApproveMember =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: cath connected and pending approval, use /_accept member #1 3 <role> to accept member",
          do
            cath <## "#team: joining the group..."
            cath <## "#team: you joined the group, pending approval"
        ]

      -- pending member doesn't see messages sent in group
      alice #> "#team hi group"
      bob <# "#team alice> hi group"

      bob #> "#team hey"
      alice <# "#team bob> hey"

      -- pending member can't send messages to group
      cath ##> "#team hello"
      cath <## "bad chat command: not current member"

      -- pending member and host can send messages to each other
      alice ##> "/_send #1(_support:3) text send me proofs"
      alice <# "#team (support: cath) send me proofs"
      cath <# "#team (support) alice> send me proofs"

      cath ##> "/_send #1(_support) text proofs"
      cath <# "#team (support) proofs"
      alice <# "#team (support: cath) cath> proofs"

      -- accept member
      alice ##> "/accept member #team cath"
      concurrentlyN_
        [ alice <## "#team: cath accepted",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hi group [>>]",
                   WithTime "#team bob> hey [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      alice #> "#team welcome cath"
      [bob, cath] *<# "#team alice> welcome cath"

      bob #> "#team hi cath"
      [alice, cath] *<# "#team bob> hi cath"

      cath #> "#team hi group"
      [alice, bob] *<# "#team cath> hi group"
  where
    cfg = testCfg {chatHooks = defaultChatHooks {acceptMember = Just (\_ _ _ -> pure $ Right (GAPendingApproval, GRObserver))}}

testGLinkReviewMember :: HasCallStack => TestParams -> IO ()
testGLinkReviewMember =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRModerator) (dan, GRModerator)

      alice ##> "/set admission review #team all"
      alice <## "changed member admission rules"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed member admission rules",
          do
            cath <## "alice updated group #team:"
            cath <## "changed member admission rules",
          do
            dan <## "alice updated group #team:"
            dan <## "changed member admission rules"
        ]

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      eve ##> ("/c " <> gLink)
      eve <## "connection request sent!"
      alice <## "eve (Eve): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: eve connected and pending review",
          eve
            <### [ "#team: alice accepted you to the group, pending review",
                   "#team: joining the group...",
                   "#team: you joined the group, connecting to group moderators for admission to group",
                   "#team: member cath (Catherine) is connected",
                   "#team: member dan (Daniel) is connected"
                 ],
          do
            cath <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            cath <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member",
          do
            dan <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            dan <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
        ]

      -- pending member doesn't see messages sent in group
      alice #> "#team 1"
      [bob, cath, dan] *<# "#team alice> 1"

      bob #> "#team 2"
      [alice, cath, dan] *<# "#team bob> 2"

      cath #> "#team 3"
      [alice, bob, dan] *<# "#team cath> 3"

      dan #> "#team 4"
      [alice, bob, cath] *<# "#team dan> 4"

      (eve </)

      -- pending member can't send messages to group
      eve ##> "#team hello"
      eve <## "bad chat command: not current member"

      -- pending member and moderators can send messages to each other
      alice ##> "/_send #1(_support:5) text 5"
      alice <# "#team (support: eve) 5"
      [cath, dan] *<# "#team (support: eve) alice> 5"
      eve <# "#team (support) alice> 5"

      cath ##> "/_send #1(_support:5) text 6"
      cath <# "#team (support: eve) 6"
      [alice, dan] *<# "#team (support: eve) cath> 6"
      eve <# "#team (support) cath> 6"

      dan ##> "/_send #1(_support:5) text 7"
      dan <# "#team (support: eve) 7"
      [alice, cath] *<# "#team (support: eve) dan> 7"
      eve <# "#team (support) dan> 7"

      eve ##> "/_send #1(_support) text 8"
      eve <# "#team (support) 8"
      [alice, cath, dan] *<# "#team (support: eve) eve> 8"

      (bob </)

      -- deleting support chat with pending member is prohibited
      alice ##> "/_delete member chat #1 5"
      alice <## "bad chat command: member is pending"

      -- moderator can't accept member with a role higher than their own
      dan ##> "/_accept member #1 5 admin"
      dan <## "#team: you have insufficient permissions for this action, the required role is admin"
      dan ##> "/_accept member #1 5 owner"
      dan <## "#team: you have insufficient permissions for this action, the required role is owner"

      -- accept member
      dan ##> "/_accept member #1 5 member"
      concurrentlyN_
        [ dan <## "#team: eve accepted",
          alice <## "#team: dan accepted eve to the group (will introduce remaining members)",
          cath <## "#team: dan accepted eve to the group",
          eve
            <### [ "#team: you joined the group",
                   WithTime "#team alice> 1 [>>]",
                   WithTime "#team bob> 2 [>>]",
                   WithTime "#team cath> 3 [>>]",
                   WithTime "#team dan> 4 [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added eve (Eve) to the group (connecting...)"
            bob <## "#team: new member eve is connected"
        ]

      alice #> "#team 9"
      [bob, cath, dan, eve] *<# "#team alice> 9"

      bob #> "#team 10"
      [alice, cath, dan, eve] *<# "#team bob> 10"

      cath #> "#team 11"
      [alice, bob, dan, eve] *<# "#team cath> 11"

      dan #> "#team 12"
      [alice, bob, cath, eve] *<# "#team dan> 12"

      eve #> "#team 13"
      [alice, bob, cath, dan] *<# "#team eve> 13"

testGLinkApproveThenReviewMember :: HasCallStack => TestParams -> IO ()
testGLinkApproveThenReviewMember =
  testChatCfg5 cfg aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRModerator) (dan, GRModerator)

      alice ##> "/set admission review #team all"
      alice <## "changed member admission rules"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed member admission rules",
          do
            cath <## "alice updated group #team:"
            cath <## "changed member admission rules",
          do
            dan <## "alice updated group #team:"
            dan <## "changed member admission rules"
        ]

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      eve ##> ("/c " <> gLink)
      eve <## "connection request sent!"
      alice <## "eve (Eve): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: eve connected and pending approval, use /_accept member #1 5 <role> to accept member",
          do
            eve <## "#team: joining the group..."
            eve <## "#team: you joined the group, pending approval"
        ]

      -- pending member doesn't see messages sent in group
      alice #> "#team 1"
      [bob, cath, dan] *<# "#team alice> 1"

      bob #> "#team 2"
      [alice, cath, dan] *<# "#team bob> 2"

      cath #> "#team 3"
      [alice, bob, dan] *<# "#team cath> 3"

      dan #> "#team 4"
      [alice, bob, cath] *<# "#team dan> 4"

      (eve </)

      -- pending member can't send messages to group
      eve ##> "#team hello"
      eve <## "bad chat command: not current member"

      -- pending member and host can send messages to each other
      alice ##> "/_send #1(_support:5) text 5"
      alice <# "#team (support: eve) 5"
      eve <# "#team (support) alice> 5"

      eve ##> "/_send #1(_support) text 6"
      eve <# "#team (support) 6"
      alice <# "#team (support: eve) eve> 6"

      (bob </)
      (cath </)
      (dan </)

      -- accept member
      alice ##> "/_accept member #1 5 member"
      concurrentlyN_
        [ alice <## "#team: eve accepted and pending review (will introduce moderators)",
          eve
            <### [ "#team: alice accepted you to the group, pending review",
                   "#team: member cath (Catherine) is connected",
                   "#team: member dan (Daniel) is connected"
                 ],
          do
            cath <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            cath <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member",
          do
            dan <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            dan <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
        ]

      -- pending member doesn't see messages sent in group
      alice #> "#team 7"
      [bob, cath, dan] *<# "#team alice> 7"

      bob #> "#team 8"
      [alice, cath, dan] *<# "#team bob> 8"

      cath #> "#team 9"
      [alice, bob, dan] *<# "#team cath> 9"

      dan #> "#team 10"
      [alice, bob, cath] *<# "#team dan> 10"

      (eve </)

      -- pending member can't send messages to group
      eve ##> "#team hello"
      eve <## "bad chat command: not current member"

      -- pending member and moderators can send messages to each other
      alice ##> "/_send #1(_support:5) text 11"
      alice <# "#team (support: eve) 11"
      [cath, dan] *<# "#team (support: eve) alice> 11"
      eve <# "#team (support) alice> 11"

      cath ##> "/_send #1(_support:5) text 12"
      cath <# "#team (support: eve) 12"
      [alice, dan] *<# "#team (support: eve) cath> 12"
      eve <# "#team (support) cath> 12"

      dan ##> "/_send #1(_support:5) text 13"
      dan <# "#team (support: eve) 13"
      [alice, cath] *<# "#team (support: eve) dan> 13"
      eve <# "#team (support) dan> 13"

      eve ##> "/_send #1(_support) text 14"
      eve <# "#team (support) 14"
      [alice, cath, dan] *<# "#team (support: eve) eve> 14"

      (bob </)

      -- accept member
      dan ##> "/_accept member #1 5 member"
      concurrentlyN_
        [ dan <## "#team: eve accepted",
          alice <## "#team: dan accepted eve to the group (will introduce remaining members)",
          cath <## "#team: dan accepted eve to the group",
          eve
            <### [ "#team: you joined the group",
                   WithTime "#team alice> 1 [>>]",
                   WithTime "#team bob> 2 [>>]",
                   WithTime "#team cath> 3 [>>]",
                   WithTime "#team dan> 4 [>>]",
                   WithTime "#team alice> 7 [>>]",
                   WithTime "#team bob> 8 [>>]",
                   WithTime "#team cath> 9 [>>]",
                   WithTime "#team dan> 10 [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added eve (Eve) to the group (connecting...)"
            bob <## "#team: new member eve is connected"
        ]

      alice #> "#team 15"
      [bob, cath, dan, eve] *<# "#team alice> 15"

      bob #> "#team 16"
      [alice, cath, dan, eve] *<# "#team bob> 16"

      cath #> "#team 17"
      [alice, bob, dan, eve] *<# "#team cath> 17"

      dan #> "#team 18"
      [alice, bob, cath, eve] *<# "#team dan> 18"

      eve #> "#team 19"
      [alice, bob, cath, dan] *<# "#team eve> 19"
  where
    cfg = testCfg {chatHooks = defaultChatHooks {acceptMember = Just (\_ _ _ -> pure $ Right (GAPendingApproval, GRObserver))}}

testGLinkDeletePendingApprovalMember :: HasCallStack => TestParams -> IO ()
testGLinkDeletePendingApprovalMember =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: cath connected and pending approval, use /_accept member #1 3 <role> to accept member",
          do
            cath <## "#team: joining the group..."
            cath <## "#team: you joined the group, pending approval"
        ]

      alice ##> "/rm team cath"
      alice <## "#team: you removed cath from the group"
      cath <## "#team: alice removed you from the group"
      cath <## "use /d #team to delete the group"
  where
    cfg = testCfg {chatHooks = defaultChatHooks {acceptMember = Just (\_ _ _ -> pure $ Right (GAPendingApproval, GRObserver))}}

testGLinkReviewIntroduce :: HasCallStack => TestParams -> IO ()
testGLinkReviewIntroduce =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup3' "team" alice (bob, GRMember) (cath, GRModerator)

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      dan ##> ("/c " <> gLink)
      dan <## "connection request sent!"
      alice <## "dan (Daniel): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: dan joined the group",
          do
            dan <## "#team: joining the group..."
            dan <## "#team: you joined the group"
            dan <###
              [ "#team: member bob (Bob) is connected",
                "#team: member cath (Catherine) is connected"
              ],
          do
            bob <## "#team: alice added dan (Daniel) to the group (connecting...)"
            bob <## "#team: new member dan is connected",
          do
            cath <## "#team: alice added dan (Daniel) to the group (connecting...)"
            cath <## "#team: new member dan is connected"
        ]

      alice ##> "/mr team dan admin"
      concurrentlyN_
        [ alice <## "#team: you changed the role of dan to admin",
          bob <## "#team: alice changed the role of dan from member to admin",
          cath <## "#team: alice changed the role of dan from member to admin",
          dan <## "#team: alice changed your role from member to admin"
        ]

      alice ##> "/set admission review #team all"
      alice <## "changed member admission rules"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed member admission rules",
          do
            cath <## "alice updated group #team:"
            cath <## "changed member admission rules",
          do
            dan <## "alice updated group #team:"
            dan <## "changed member admission rules"
        ]

      dan ##> "/create link #team"
      gLinkDan <- getGroupLink dan "team" GRMember True
      eve ##> ("/c " <> gLinkDan)
      eve <## "connection request sent!"
      dan <## "eve (Eve): accepting request to join group #team..."
      concurrentlyN_
        [ dan <## "#team: eve connected and pending review",
          eve
            <### [ "#team: dan accepted you to the group, pending review",
                   "#team: joining the group...",
                   "#team: you joined the group, connecting to group moderators for admission to group",
                   "#team: member alice (Alice) is connected",
                   "#team: member cath (Catherine) is connected"
                 ],
          do
            alice <## "#team: dan added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            alice <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member",
          do
            cath <## "#team: dan added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            cath <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
        ]

      -- owner accepts new member
      alice ##> "/_accept member #1 5 member"
      concurrentlyN_
        [ alice <## "#team: eve accepted",
          dan <## "#team: alice accepted eve to the group (will introduce remaining members)",
          cath <## "#team: alice accepted eve to the group",
          eve
            <### [ "#team: you joined the group",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: dan added eve (Eve) to the group (connecting...)"
            bob <## "#team: new member eve is connected"
        ]

      alice #> "#team 1"
      [bob, cath, dan, eve] *<# "#team alice> 1"

      bob #> "#team 2"
      [alice, cath, dan, eve] *<# "#team bob> 2"

      cath #> "#team 3"
      [alice, bob, dan, eve] *<# "#team cath> 3"

      dan #> "#team 4"
      [alice, bob, cath, eve] *<# "#team dan> 4"

      eve #> "#team 5"
      [alice, bob, cath, dan] *<# "#team eve> 5"

testPlanGroupLinkKnown :: HasCallStack => TestParams -> IO ()
testPlanGroupLinkKnown =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True

      bob ##> ("/_connect plan 1 " <> gLink)
      bob <## "group link: ok to connect directly"
      _sLinkData <- getTermLine bob

      bob ##> ("/c " <> gLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      bob ##> ("/_connect plan 1 " <> gLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"

      let gLinkSchema2 = linkAnotherSchema gLink
      bob ##> ("/_connect plan 1 " <> gLinkSchema2)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"

      bob ##> ("/c " <> gLink)
      bob <## "group link: known group #team"
      bob <## "use #team <message> to send messages"

testPlanGroupLinkConnecting :: HasCallStack => TestParams -> IO ()
testPlanGroupLinkConnecting ps = do
  gLink <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    threadDelay 100000
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> "/create link #team"
    getGroupLink alice "team" GRMember True
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    threadDelay 100000

    bob ##> ("/c " <> gLink)
    bob <## "connection request sent!"

    bob ##> ("/_connect plan 1 " <> gLink)
    bob <## "group link: connecting, allowed to reconnect"

    let gLinkSchema2 = linkAnotherSchema gLink
    bob ##> ("/_connect plan 1 " <> gLinkSchema2)
    bob <## "group link: connecting, allowed to reconnect"

    threadDelay 100000
  withTestChat ps "alice" $ \alice -> do
    alice
      <### [ "subscribed 1 connections on server localhost",
             "bob (Bob): accepting request to join group #team..."
           ]
  withTestChat ps "bob" $ \bob -> do
    threadDelay 500000
    bob <## "subscribed 1 connections on server localhost"
    bob <## "#team: joining the group..."
    bob <## "#team: you joined the group"

    bob ##> ("/_connect plan 1 " <> gLink)
    bob <## "group link: known group #team"
    bob <## "use #team <message> to send messages"

    let gLinkSchema2 = linkAnotherSchema gLink
    bob ##> ("/_connect plan 1 " <> gLinkSchema2)
    bob <## "group link: known group #team"
    bob <## "use #team <message> to send messages"

    bob ##> ("/c " <> gLink)
    bob <## "group link: known group #team"
    bob <## "use #team <message> to send messages"

#if !defined(dbPostgres)
testGroupMsgDecryptError :: HasCallStack => TestParams -> IO ()
testGroupMsgDecryptError ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"
    setupDesynchronizedRatchet ps alice
    withTestChat ps "bob" $ \bob -> do
      bob <## "subscribed 2 connections on server localhost"
      alice #> "#team hello again"
      bob <# "#team alice> skipped message ID 8..10"
      bob <# "#team alice> hello again"
      bob #> "#team received!"
      alice <# "#team bob> received!"

setupDesynchronizedRatchet :: HasCallStack => TestParams -> TestCC -> IO ()
setupDesynchronizedRatchet ps alice = do
  copyDb "bob" "bob_old"
  withTestChat ps "bob" $ \bob -> do
    bob <## "subscribed 2 connections on server localhost"
    alice #> "#team 1"
    bob <# "#team alice> 1"
    bob #> "#team 2"
    alice <# "#team bob> 2"
    alice #> "#team 3"
    bob <# "#team alice> 3"
    bob #> "#team 4"
    alice <# "#team bob> 4"
  withTestChat ps "bob_old" $ \bob -> do
    bob <## "subscribed 2 connections on server localhost"
    bob ##> "/sync #team alice"
    bob <## "error: command is prohibited, synchronizeRatchet: not allowed"
    alice #> "#team 1"
    bob <## "#team alice: decryption error (connection out of sync), synchronization required"
    bob <## "use /sync #team alice to synchronize"
    alice #> "#team 2"
    alice #> "#team 3"
    (bob </)
    bob ##> "/tail #team 1"
    bob <# "#team alice> decryption error, possibly due to the device change (header, 3 messages)"
  where
    tmp = tmpPath ps
    copyDb from to = do
      copyFile (tmp </> (from <> chatSuffix)) (tmp </> (to <> chatSuffix))
      copyFile (tmp </> (from <> agentSuffix)) (tmp </> (to <> agentSuffix))

testGroupSyncRatchet :: HasCallStack => TestParams -> IO ()
testGroupSyncRatchet ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"
    setupDesynchronizedRatchet ps alice
    withTestChat ps "bob_old" $ \bob -> do
      bob <## "subscribed 2 connections on server localhost"
      bob `send` "#team 1"
      -- "send prohibited" error is not printed in group as SndMessage is created,
      -- but it should be displayed in per member snd statuses
      bob <# "#team 1"
      (alice </)
      -- synchronize bob and alice
      bob ##> "/sync #team alice"
      bob <## "connection synchronization started"
      alice <## "#team bob: connection synchronization agreed"
      bob <## "#team alice: connection synchronization agreed"
      alice <## "#team bob: connection synchronized"
      bob <## "#team alice: connection synchronized"

      threadDelay 100000
      bob #$> ("/_get chat #1 count=3", chat, [(1, "connection synchronization started for alice"), (0, "connection synchronization agreed"), (0, "connection synchronized")])
      alice #$> ("/_get chat #1 count=2", chat, [(0, "connection synchronization agreed"), (0, "connection synchronized")])

      alice #> "#team hello again"
      bob <# "#team alice> hello again"
      bob #> "#team received!"
      alice <# "#team bob> received!"

testGroupSyncRatchetCodeReset :: HasCallStack => TestParams -> IO ()
testGroupSyncRatchetCodeReset ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      createGroup2 "team" alice bob
      alice #> "#team hi"
      bob <# "#team alice> hi"
      bob #> "#team hey"
      alice <# "#team bob> hey"
      -- connection not verified
      bob ##> "/i #team alice"
      aliceInfo bob False
      -- verify connection
      alice ##> "/code #team bob"
      bCode <- getTermLine alice
      bob ##> ("/verify #team alice " <> bCode)
      bob <## "connection verified"
      -- connection verified
      bob ##> "/i #team alice"
      aliceInfo bob True
    setupDesynchronizedRatchet ps alice
    withTestChat ps "bob_old" $ \bob -> do
      bob <## "subscribed 2 connections on server localhost"
      bob ##> "/sync #team alice"
      bob <## "connection synchronization started"
      alice <## "#team bob: connection synchronization agreed"
      bob <## "#team alice: connection synchronization agreed"
      bob <## "#team alice: security code changed"
      alice <## "#team bob: connection synchronized"
      bob <## "#team alice: connection synchronized"

      threadDelay 100000
      bob #$> ("/_get chat #1 count=4", chat, [(1, "connection synchronization started for alice"), (0, "connection synchronization agreed"), (0, "security code changed"), (0, "connection synchronized")])
      alice #$> ("/_get chat #1 count=2", chat, [(0, "connection synchronization agreed"), (0, "connection synchronized")])

      -- connection not verified
      bob ##> "/i #team alice"
      aliceInfo bob False

      alice #> "#team hello again"
      bob <# "#team alice> hello again"
      bob #> "#team received!"
      alice <# "#team bob> received!"
  where
    aliceInfo :: HasCallStack => TestCC -> Bool -> IO ()
    aliceInfo bob verified = do
      bob <## "group ID: 1"
      bob <## "member ID: 1"
      bob <## "receiving messages via: localhost"
      bob <## "sending messages via: localhost"
      bob <## connVerified
      bob <## currentChatVRangeInfo
      where
        connVerified
          | verified = "connection verified"
          | otherwise = "connection not verified, use /code command to see security code"
#endif

testSetGroupMessageReactions :: HasCallStack => TestParams -> IO ()
testSetGroupMessageReactions =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      threadDelay 1000000
      alice #> "#team hi"
      bob <# "#team alice> hi"
      cath <# "#team alice> hi"
      bob ##> "+1 #team hi"
      bob <## "added 👍"
      alice <# "#team bob> > alice hi"
      alice <## "    + 👍"
      cath <# "#team bob> > alice hi"
      cath <## "    + 👍"
      bob ##> "+1 #team hi"
      bob <## "bad chat command: reaction already added"
      bob ##> "+^ #team hi"
      bob <## "added 🚀"
      alice <# "#team bob> > alice hi"
      alice <## "    + 🚀"
      cath <# "#team bob> > alice hi"
      cath <## "    + 🚀"
      alice ##> "/tail #team 1"
      alice <# "#team hi"
      alice <## "      👍 1 🚀 1"
      bob ##> "/tail #team 1"
      bob <# "#team alice> hi"
      bob <## "      👍 1 🚀 1"
      bob ##> "/tail #team 1"
      bob <# "#team alice> hi"
      bob <## "      👍 1 🚀 1"
      alice ##> "+1 #team hi"
      alice <## "added 👍"
      bob <# "#team alice> > alice hi"
      bob <## "    + 👍"
      cath <# "#team alice> > alice hi"
      cath <## "    + 👍"
      alice ##> "/tail #team 1"
      alice <# "#team hi"
      alice <## "      👍 2 🚀 1"
      bob ##> "/tail #team 1"
      bob <# "#team alice> hi"
      bob <## "      👍 2 🚀 1"
      cath ##> "/tail #team 1"
      cath <# "#team alice> hi"
      cath <## "      👍 2 🚀 1"
      itemId' <- lastItemId alice
      alice ##> ("/_reaction members 1 #1 " <> itemId' <> " {\"type\": \"emoji\", \"emoji\": \"👍\"}")
      alice <## "2 member(s) reacted"
      bob ##> "-1 #team hi"
      bob <## "removed 👍"
      alice <# "#team bob> > alice hi"
      alice <## "    - 👍"
      cath <# "#team bob> > alice hi"
      cath <## "    - 👍"
      bob ##> "-^ #team hi"
      bob <## "removed 🚀"
      alice <# "#team bob> > alice hi"
      alice <## "    - 🚀"
      cath <# "#team bob> > alice hi"
      cath <## "    - 🚀"
      alice ##> "/tail #team 1"
      alice <# "#team hi"
      alice <## "      👍 1"
      bob ##> "/tail #team 1"
      bob <# "#team alice> hi"
      bob <## "      👍 1"
      cath ##> "/tail #team 1"
      cath <# "#team alice> hi"
      cath <## "      👍 1"

testSendGroupDeliveryReceipts :: HasCallStack => TestParams -> IO ()
testSendGroupDeliveryReceipts ps =
  withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg ps cfg "bob" bobProfile $ \bob -> do
      withNewTestChatCfg ps cfg "cath" cathProfile $ \cath -> do
        -- turn off contacts receipts for tests
        alice ##> "/_set receipts contacts 1 off"
        alice <## "ok"
        bob ##> "/_set receipts contacts 1 off"
        bob <## "ok"
        cath ##> "/_set receipts contacts 1 off"
        cath <## "ok"

        createGroup3 "team" alice bob cath
        threadDelay 1000000

        alice #> "#team hi"
        bob <# "#team alice> hi"
        cath <# "#team alice> hi"
        alice % "#team hi"
        alice ⩗ "#team hi"

        bob #> "#team hey"
        alice <# "#team bob> hey"
        cath <# "#team bob> hey"
        bob % "#team hey"
        bob ⩗ "#team hey"
  where
    cfg = testCfg {showReceipts = True}

testConfigureGroupDeliveryReceipts :: HasCallStack => TestParams -> IO ()
testConfigureGroupDeliveryReceipts ps =
  withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
    withNewTestChatCfg ps cfg "bob" bobProfile $ \bob -> do
      withNewTestChatCfg ps cfg "cath" cathProfile $ \cath -> do
        -- turn off contacts receipts for tests
        alice ##> "/_set receipts contacts 1 off"
        alice <## "ok"
        bob ##> "/_set receipts contacts 1 off"
        bob <## "ok"
        cath ##> "/_set receipts contacts 1 off"
        cath <## "ok"

        -- create group 1
        createGroup3 "team" alice bob cath
        threadDelay 1000000

        -- create group 2
        alice ##> "/g club"
        alice <## "group #club is created"
        alice <## "to add members use /a club <name> or /create link #club"
        alice ##> "/a club bob"
        concurrentlyN_
          [ alice <## "invitation to join the group #club sent to bob",
            do
              bob <## "#club: alice invites you to join the group as member"
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
              cath <## "#club: alice invites you to join the group as member"
              cath <## "use /j club to accept"
          ]
        cath ##> "/j club"
        concurrentlyN_
          [ alice <## "#club: cath joined the group",
            do
              cath <## "#club: you joined the group"
              cath <## "#club: member bob_1 (Bob) is connected",
            do
              bob <## "#club: alice added cath_1 (Catherine) to the group (connecting...)"
              bob <## "#club: new member cath_1 is connected"
          ]
        threadDelay 1000000

        -- for new users receipts are enabled by default
        receipt bob alice cath "team" "1"
        receipt bob alice cath "club" "2"

        -- configure receipts in all chats
        alice ##> "/set receipts all off"
        alice <## "ok"
        partialReceipt bob alice cath "team" "3"
        partialReceipt bob alice cath "club" "4"

        -- configure receipts for user groups
        alice ##> "/_set receipts groups 1 on"
        alice <## "ok"
        receipt bob alice cath "team" "5"
        receipt bob alice cath "club" "6"

        -- configure receipts for user groups (terminal api)
        alice ##> "/set receipts groups off"
        alice <## "ok"
        partialReceipt bob alice cath "team" "7"
        partialReceipt bob alice cath "club" "8"

        -- configure receipts for group
        alice ##> "/receipts #team on"
        alice <## "ok"
        receipt bob alice cath "team" "9"
        partialReceipt bob alice cath "club" "10"

        -- configure receipts for user groups (don't clear overrides)
        alice ##> "/_set receipts groups 1 off"
        alice <## "ok"
        receipt bob alice cath "team" "11"
        partialReceipt bob alice cath "club" "12"

        alice ##> "/_set receipts groups 1 off clear_overrides=off"
        alice <## "ok"
        receipt bob alice cath "team" "13"
        partialReceipt bob alice cath "club" "14"

        -- configure receipts for user groups (clear overrides)
        alice ##> "/set receipts groups off clear_overrides=on"
        alice <## "ok"
        partialReceipt bob alice cath "team" "15"
        partialReceipt bob alice cath "club" "16"

        -- configure receipts for group, reset to default
        alice ##> "/receipts #team on"
        alice <## "ok"
        receipt bob alice cath "team" "17"
        partialReceipt bob alice cath "club" "18"

        alice ##> "/receipts #team default"
        alice <## "ok"
        partialReceipt bob alice cath "team" "19"
        partialReceipt bob alice cath "club" "20"

        -- cath - disable receipts for user groups
        cath ##> "/_set receipts groups 1 off"
        cath <## "ok"
        noReceipt bob alice cath "team" "21"
        noReceipt bob alice cath "club" "22"

        -- partial, all receipts in one group; no receipts in other group
        cath ##> "/receipts #team on"
        cath <## "ok"
        partialReceipt bob alice cath "team" "23"
        noReceipt bob alice cath "club" "24"

        alice ##> "/receipts #team on"
        alice <## "ok"
        receipt bob alice cath "team" "25"
        noReceipt bob alice cath "club" "26"
  where
    cfg = testCfg {showReceipts = True}
    receipt cc1 cc2 cc3 gName msg = do
      cc1 #> ("#" <> gName <> " " <> msg)
      cc2 .<## ("> " <> msg)
      cc3 .<## ("> " <> msg)
      cc1 % ("#" <> gName <> " " <> msg)
      cc1 ⩗ ("#" <> gName <> " " <> msg)
    partialReceipt cc1 cc2 cc3 gName msg = do
      cc1 #> ("#" <> gName <> " " <> msg)
      cc2 .<## ("> " <> msg)
      cc3 .<## ("> " <> msg)
      cc1 % ("#" <> gName <> " " <> msg)
    noReceipt cc1 cc2 cc3 gName msg = do
      cc1 #> ("#" <> gName <> " " <> msg)
      cc2 .<## ("> " <> msg)
      cc3 .<## ("> " <> msg)
      cc1 <// 50000

testNoGroupDirectConns :: HasCallStack => TestParams -> IO ()
testNoGroupDirectConns =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob ##> "/contacts"
      bob <## "alice (Alice)"
      cath ##> "/contacts"
      cath <## "alice (Alice)"

testNoDirectDifferentLDNs :: HasCallStack => TestParams -> IO ()
testNoDirectDifferentLDNs =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "to add members use /a club <name> or /create link #club"
      addMember "club" alice bob GRAdmin
      bob ##> "/j club"
      concurrently_
        (alice <## "#club: bob joined the group")
        (bob <## "#club: you joined the group")
      addMember "club" alice cath GRAdmin
      cath ##> "/j club"
      concurrentlyN_
        [ alice <## "#club: cath joined the group",
          do
            cath <## "#club: you joined the group"
            cath <## "#club: member bob_1 (Bob) is connected",
          do
            bob <## "#club: alice added cath_1 (Catherine) to the group (connecting...)"
            bob <## "#club: new member cath_1 is connected"
        ]

      testGroupLDNs alice bob cath "team" "bob" "cath"
      testGroupLDNs alice bob cath "club" "bob_1" "cath_1"

      alice `hasContactProfiles` ["alice", "bob", "cath"]
      bob `hasContactProfiles` ["bob", "alice", "cath", "cath"]
      cath `hasContactProfiles` ["cath", "alice", "bob", "bob"]
  where
    testGroupLDNs alice bob cath gName bobLDN cathLDN = do
      alice ##> ("/ms " <> gName)
      alice
        <### [ "alice (Alice): owner, you, created group",
               "bob (Bob): admin, invited, connected",
               "cath (Catherine): admin, invited, connected"
             ]

      bob ##> ("/ms " <> gName)
      bob
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): admin, you, connected",
               ConsoleString (cathLDN <> " (Catherine): admin, connected")
             ]

      cath ##> ("/ms " <> gName)
      cath
        <### [ "alice (Alice): owner, host, connected",
               ConsoleString (bobLDN <> " (Bob): admin, connected"),
               "cath (Catherine): admin, you, connected"
             ]

      alice #> ("#" <> gName <> " hello")
      concurrentlyN_
        [ bob <# ("#" <> gName <> " alice> hello"),
          cath <# ("#" <> gName <> " alice> hello")
        ]
      bob #> ("#" <> gName <> " hi there")
      concurrentlyN_
        [ alice <# ("#" <> gName <> " bob> hi there"),
          cath <# ("#" <> gName <> " " <> bobLDN <> "> hi there")
        ]
      cath #> ("#" <> gName <> " hey")
      concurrentlyN_
        [ alice <# ("#" <> gName <> " cath> hey"),
          bob <# ("#" <> gName <> " " <> cathLDN <> "> hey")
        ]

testMergeMemberExistingContact :: HasCallStack => TestParams -> IO ()
testMergeMemberExistingContact =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      createGroup2 "team" bob cath
      bob ##> "/a #team alice"
      bob <## "invitation to join the group #team sent to alice"
      alice <## "#team: bob invites you to join the group as member"
      alice <## "use /j team to accept"
      alice ##> "/j team"
      concurrentlyN_
        [ do
            alice <## "#team: you joined the group"
            alice <## "#team: member cath_1 (Catherine) is connected"
            alice <## "contact and member are merged: cath, #team cath_1"
            alice <## "use @cath <message> to send messages",
          do
            bob <## "#team: alice joined the group",
          do
            cath <## "#team: bob added alice_1 (Alice) to the group (connecting...)"
            cath <## "#team: new member alice_1 is connected"
            cath <## "contact and member are merged: alice, #team alice_1"
            cath <## "use @alice <message> to send messages"
        ]
      alice <##> cath
      alice #> "#team hello"
      bob <# "#team alice> hello"
      cath <# "#team alice> hello"
      cath #> "#team hello too"
      bob <# "#team cath> hello too"
      alice <# "#team cath> hello too"

      alice ##> "/contacts"
      alice
        <### [ "bob (Bob)",
               "cath (Catherine)"
             ]
      cath ##> "/contacts"
      cath
        <### [ "alice (Alice)",
               "bob (Bob)"
             ]
      alice `hasContactProfiles` ["alice", "bob", "cath"]
      cath `hasContactProfiles` ["cath", "alice", "bob"]

testMergeContactExistingMember :: HasCallStack => TestParams -> IO ()
testMergeContactExistingMember =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      bob ##> "/c"
      inv' <- getInvitation bob
      cath ##> ("/c " <> inv')
      cath <## "confirmation sent!"
      concurrentlyN_
        [ bob
            <### [ "cath_1 (Catherine): contact is connected",
                   "contact and member are merged: cath_1, #team cath",
                   "use @cath <message> to send messages"
                 ],
          cath
            <### [ "bob_1 (Bob): contact is connected",
                   "contact and member are merged: bob_1, #team bob",
                   "use @bob <message> to send messages"
                 ]
        ]
      bob <##> cath

      bob ##> "/contacts"
      bob <### ["alice (Alice)", "cath (Catherine)"]
      cath ##> "/contacts"
      cath <### ["alice (Alice)", "bob (Bob)"]
      bob `hasContactProfiles` ["alice", "bob", "cath"]
      cath `hasContactProfiles` ["cath", "alice", "bob"]

testMergeContactMultipleMembers :: HasCallStack => TestParams -> IO ()
testMergeContactMultipleMembers =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      create2Groups3 "team" "club" alice bob cath

      bob `hasContactProfiles` ["alice", "bob", "cath", "cath"]
      cath `hasContactProfiles` ["cath", "alice", "bob", "bob"]

      bob ##> "/c"
      inv' <- getInvitation bob
      cath ##> ("/c " <> inv')
      cath <## "confirmation sent!"
      concurrentlyN_
        [ bob
            <### [ "cath_2 (Catherine): contact is connected",
                   StartsWith "contact and member are merged: cath",
                   StartsWith "use @cath",
                   StartsWith "contact and member are merged: cath",
                   StartsWith "use @cath"
                 ],
          cath
            <### [ "bob_2 (Bob): contact is connected",
                   StartsWith "contact and member are merged: bob",
                   StartsWith "use @bob",
                   StartsWith "contact and member are merged: bob",
                   StartsWith "use @bob"
                 ]
        ]
      bob <##> cath

      bob ##> "/contacts"
      bob <### ["alice (Alice)", "cath (Catherine)"]
      cath ##> "/contacts"
      cath <### ["alice (Alice)", "bob (Bob)"]
      bob `hasContactProfiles` ["alice", "bob", "cath"]
      cath `hasContactProfiles` ["cath", "alice", "bob"]

testMergeGroupLinkHostMultipleContacts :: HasCallStack => TestParams -> IO ()
testMergeGroupLinkHostMultipleContacts =
  testChat2 bobProfile cathProfile $
    \bob cath -> do
      connectUsers bob cath

      bob ##> "/c"
      inv' <- getInvitation bob
      cath ##> ("/c " <> inv')
      cath <## "confirmation sent!"
      concurrently_
        (bob <## "cath_1 (Catherine): contact is connected")
        (cath <## "bob_1 (Bob): contact is connected")

      bob `hasContactProfiles` ["bob", "cath", "cath"]
      cath `hasContactProfiles` ["cath", "bob", "bob"]

      bob ##> "/g party"
      bob <## "group #party is created"
      bob <## "to add members use /a party <name> or /create link #party"
      bob ##> "/create link #party"
      gLink <- getGroupLink bob "party" GRMember True
      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      bob <## "cath_2 (Catherine): accepting request to join group #party..."
      concurrentlyN_
        [ bob
            <### [ EndsWith "joined the group",
                   "contact and member are merged: cath, #party cath_2",
                   StartsWith "use @cath"
                 ],
          cath
            <### [ "#party: joining the group...",
                   "#party: you joined the group",
                   "contact and member are merged: bob, #party bob_2",
                   StartsWith "use @bob"
                 ]
        ]
      bob <##> cath

      bob ##> "/contacts"
      bob <### ["cath (Catherine)", "cath_1 (Catherine)"]
      cath ##> "/contacts"
      cath <### ["bob (Bob)", "bob_1 (Bob)"]
      bob `hasContactProfiles` ["bob", "cath", "cath"]
      cath `hasContactProfiles` ["cath", "bob", "bob"]

testMemberContactMessage :: HasCallStack => TestParams -> IO ()
testMemberContactMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      bob ##> "/set accept member contacts on"
      bob <## "ok"
      cath ##> "/set accept member contacts on"
      cath <## "ok"

      createGroup3 "team" alice bob cath

      -- alice and bob delete contacts, connect
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      bob ##> "/d alice"
      bob <## "alice: contact is deleted"

      alice ##> "@#team bob hi"
      alice
        <### [ "member #team bob does not have direct connection, creating",
               "contact for member #team bob is created",
               "sent invitation to connect directly to member #team bob",
               WithTime "@bob hi"
             ]
      bob
        <### [ "#team alice is creating direct contact alice with you",
               WithTime "alice> hi"
             ]
      bob <## "alice (Alice): you can send messages to contact"
      concurrently_
        (alice <## "bob (Bob): contact is connected")
        (bob <## "alice (Alice): contact is connected")

      bob #$> ("/_get chat #1 count=1", chat, [(0, "started direct connection with you")])

      -- exchanging messages will enable PQ (see Chat "TODO PQ" - perhaps connection should be negotiated with PQ on)
      alice <##> bob
      alice <##> bob

      alice `send` "@bob hi"
      alice <## "bob: quantum resistant end-to-end encryption enabled"
      alice <# "@bob hi"
      bob <## "alice: quantum resistant end-to-end encryption enabled"
      bob <# "alice> hi"

      bob #> "@alice hey"
      alice <# "bob> hey"

      alice <##> bob

      -- bob and cath connect
      bob ##> "@#team cath hi"
      bob
        <### [ "member #team cath does not have direct connection, creating",
               "contact for member #team cath is created",
               "sent invitation to connect directly to member #team cath",
               WithTime "@cath hi"
             ]
      cath
        <### [ "#team bob is creating direct contact bob with you",
               WithTime "bob> hi"
             ]
      cath <## "bob (Bob): you can send messages to contact"
      concurrently_
        (bob <## "cath (Catherine): contact is connected")
        (cath <## "bob (Bob): contact is connected")

      cath #$> ("/_get chat #1 count=1", chat, [(0, "started direct connection with you")])
      bob <##> cath

testMemberContactNoMessage :: HasCallStack => TestParams -> IO ()
testMemberContactNoMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      cath ##> "/set accept member contacts on"
      cath <## "ok"

      createGroup3 "team" alice bob cath

      -- bob and cath connect
      bob ##> "/_create member contact #1 3"
      bob <## "contact for member #team cath is created"

      bob ##> "/_invite member contact @3"
      bob <## "sent invitation to connect directly to member #team cath"
      cath <## "#team bob is creating direct contact bob with you"
      cath <## "bob (Bob): you can send messages to contact"
      concurrently_
        (bob <## "cath (Catherine): contact is connected")
        (cath <## "bob (Bob): contact is connected")

      cath #$> ("/_get chat #1 count=1", chat, [(0, "started direct connection with you")])
      bob <##> cath

testMemberContactProhibitedContactExists :: HasCallStack => TestParams -> IO ()
testMemberContactProhibitedContactExists =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      alice ##> "/_create member contact #1 2"
      alice <## "bad chat command: member contact already exists"

      alice ##> "@#team bob hi"
      alice <# "@bob hi"
      bob <# "alice> hi"

testMemberContactProhibitedRepeatInv :: HasCallStack => TestParams -> IO ()
testMemberContactProhibitedRepeatInv =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      cath ##> "/set accept member contacts on"
      cath <## "ok"

      createGroup3 "team" alice bob cath

      bob ##> "/_create member contact #1 3"
      bob <## "contact for member #team cath is created"

      bob ##> "/_invite member contact @3 text hi"
      bob
        <### [ "sent invitation to connect directly to member #team cath",
               WithTime "@cath hi"
             ]
      bob ##> "/_invite member contact @3 text hey"
      bob <## "bad chat command: x.grp.direct.inv already sent"
      cath
        <### [ "#team bob is creating direct contact bob with you",
               WithTime "bob> hi"
             ]
      cath <## "bob (Bob): you can send messages to contact"
      concurrently_
        (bob <## "cath (Catherine): contact is connected")
        (cath <## "bob (Bob): contact is connected")

      bob <##> cath

testMemberContactInvitedConnectionReplaced :: HasCallStack => TestParams -> IO ()
testMemberContactInvitedConnectionReplaced ps = do
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob ##> "/set accept member contacts on"
        bob <## "ok"

        createGroup3 "team" alice bob cath

        alice ##> "/d bob"
        alice <## "bob: contact is deleted"
        bob <## "alice (Alice) deleted contact with you"

        alice ##> "@#team bob hi"
        alice
          <### [ "member #team bob does not have direct connection, creating",
                 "contact for member #team bob is created",
                 "sent invitation to connect directly to member #team bob",
                 WithTime "@bob hi"
               ]
        bob
          <### [ "#team alice is creating direct contact alice with you",
                 WithTime "alice> hi",
                 "alice: security code changed"
               ]
        bob <## "alice (Alice): you can send messages to contact"
        concurrently_
          (alice <## "bob (Bob): contact is connected")
          (bob <## "alice (Alice): contact is connected")

        bob ##> "/_get chat @2 count=100"
        items <- chat <$> getTermLine bob
        items `shouldContain` [(0, "security code changed")]

    withTestChat ps "bob" $ \bob -> do
      subscriptions bob 3

      checkConnectionsWork alice bob

  withTestChat ps "alice" $ \alice -> do
    subscriptions alice 4

    withTestChat ps "bob" $ \bob -> do
      subscriptions bob 3

      checkConnectionsWork alice bob

      withTestChat ps "cath" $ \cath -> do
        subscriptions cath 3

        -- group messages work
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
  where
    subscriptions :: TestCC -> Int -> IO ()
    subscriptions cc n =
      cc <## ("subscribed " <> show n <> " connections on server localhost")
    checkConnectionsWork alice bob = do
      alice <##> bob
      alice @@@ [("@bob", "hey"), ("@cath", "sent invitation to join group team as admin"), ("#team", "connected")]
      bob @@@ [("@alice", "hey"), ("#team", "started direct connection with you")]

testMemberContactIncognito :: HasCallStack => TestParams -> IO ()
testMemberContactIncognito =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      cath ##> "/set accept member contacts on"
      cath <## "ok"

      -- create group, bob joins incognito
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c i " <> gLink)
      bobIncognito <- getTermLine bob
      bob <## "connection request sent incognito!"
      alice <## (bobIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> bobIncognito <> " joined the group"),
          do
            bob <## "#team: joining the group..."
            bob <## ("#team: you joined the group incognito as " <> bobIncognito)
        ]
      -- cath joins incognito
      cath ##> ("/c i " <> gLink)
      cathIncognito <- getTermLine cath
      cath <## "connection request sent incognito!"
      alice <## (cathIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> cathIncognito <> " joined the group"),
          do
            cath <## "#team: joining the group..."
            cath <## ("#team: you joined the group incognito as " <> cathIncognito)
            cath <## ("#team: member " <> bobIncognito <> " is connected"),
          do
            bob <## ("#team: alice added " <> cathIncognito <> " to the group (connecting...)")
            bob <## ("#team: new member " <> cathIncognito <> " is connected")
        ]

      alice `hasContactProfiles` ["alice", T.pack bobIncognito, T.pack cathIncognito]
      bob `hasContactProfiles` ["bob", "alice", T.pack bobIncognito, T.pack cathIncognito]
      cath `hasContactProfiles` ["cath", "alice", T.pack bobIncognito, T.pack cathIncognito]

      -- bob creates member contact with cath - both share incognito profile
      bob ##> ("@#team " <> cathIncognito <> " hi")
      bob
        <### [ ConsoleString ("member #team " <> cathIncognito <> " does not have direct connection, creating"),
               ConsoleString ("contact for member #team " <> cathIncognito <> " is created"),
               ConsoleString ("sent invitation to connect directly to member #team " <> cathIncognito),
               WithTime ("i @" <> cathIncognito <> " hi")
             ]
      cath
        <### [ ConsoleString ("#team " <> bobIncognito <> " is creating direct contact " <> bobIncognito <> " with you"),
               WithTime ("i " <> bobIncognito <> "> hi")
             ]
      cath <## (bobIncognito <> ": you can send messages to contact")
      _ <- getTermLine bob
      _ <- getTermLine cath
      concurrentlyN_
        [ do
            bob <## (cathIncognito <> ": contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i " <> cathIncognito <> " to print out this incognito profile again"),
          do
            cath <## (bobIncognito <> ": contact is connected, your incognito profile for this contact is " <> cathIncognito)
            cath <## ("use /i " <> bobIncognito <> " to print out this incognito profile again")
        ]

      bob `hasContactProfiles` ["bob", "alice", T.pack bobIncognito, T.pack cathIncognito]
      cath `hasContactProfiles` ["cath", "alice", T.pack bobIncognito, T.pack cathIncognito]

      bob ?#> ("@" <> cathIncognito <> " hi, I'm incognito")
      cath ?<# (bobIncognito <> "> hi, I'm incognito")
      cath ?#> ("@" <> bobIncognito <> " hey, me too")
      bob ?<# (cathIncognito <> "> hey, me too")

      -- members still use incognito profile for group
      alice #> "#team hello"
      concurrentlyN_
        [ bob ?<# "#team alice> hello",
          cath ?<# "#team alice> hello"
        ]
      bob ?#> "#team hi there"
      concurrentlyN_
        [ alice <# ("#team " <> bobIncognito <> "> hi there"),
          cath ?<# ("#team " <> bobIncognito <> "> hi there")
        ]
      cath ?#> "#team hey"
      concurrentlyN_
        [ alice <# ("#team " <> cathIncognito <> "> hey"),
          bob ?<# ("#team " <> cathIncognito <> "> hey")
        ]

testMemberContactProfileUpdate :: HasCallStack => TestParams -> IO ()
testMemberContactProfileUpdate =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      cath ##> "/set accept member contacts on"
      cath <## "ok"

      createGroup3 "team" alice bob cath

      bob ##> "/p rob Rob"
      bob <## "user profile is changed to rob (Rob) (your 1 contacts are notified)"
      alice <## "contact bob changed to rob (Rob)"
      alice <## "use @rob <message> to send messages"

      cath ##> "/p kate Kate"
      cath <## "user profile is changed to kate (Kate) (your 1 contacts are notified)"
      alice <## "contact cath changed to kate (Kate)"
      alice <## "use @kate <message> to send messages"

      alice #> "#team hello"
      bob <# "#team alice> hello"
      cath <# "#team alice> hello"

      alice `hasContactProfiles` ["alice", "rob", "kate"]
      bob `hasContactProfiles` ["rob", "alice", "cath"]
      cath `hasContactProfiles` ["kate", "alice", "bob"]

      bob `send` "@cath hi"
      bob
        <### [ "member #team cath does not have direct connection, creating",
               "contact for member #team cath is created",
               "sent invitation to connect directly to member #team cath",
               WithTime "@cath hi"
             ]
      cath
        <### [ "#team bob is creating direct contact bob with you",
               WithTime "bob> hi"
             ]
      cath <## "bob (Bob): you can send messages to contact"
      concurrentlyN_
        [ do
            bob <## "contact cath changed to kate (Kate)"
            bob <## "use @kate <message> to send messages"
            bob <## "kate (Kate): contact is connected",
          do
            cath <## "contact bob changed to rob (Rob)"
            cath <## "use @rob <message> to send messages"
            cath <## "rob (Rob): contact is connected"
        ]

      bob ##> "/contacts"
      bob
        <### [ "alice (Alice)",
               "kate (Kate)"
             ]
      cath ##> "/contacts"
      cath
        <### [ "alice (Alice)",
               "rob (Rob)"
             ]
      alice `hasContactProfiles` ["alice", "rob", "kate"]
      bob `hasContactProfiles` ["rob", "alice", "kate"]
      cath `hasContactProfiles` ["kate", "alice", "rob"]

      bob #> "#team hello too"
      alice <# "#team rob> hello too"
      cath <# "#team rob> hello too" -- updated profile
      cath #> "#team hello there"
      alice <# "#team kate> hello there"
      bob <# "#team kate> hello there" -- updated profile

testRecreateMemberContactManyGroups :: HasCallStack => TestParams -> IO ()
testRecreateMemberContactManyGroups =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      bob ##> "/set accept member contacts on"
      bob <## "ok"

      connectUsers alice bob
      createGroup2' "team" alice (bob, GRAdmin) False
      createGroup2' "club" alice (bob, GRAdmin) False

      -- alice can message bob via team and via club
      alice ##> "@#team bob 1"
      alice <# "@bob 1"
      bob <# "alice> 1"

      bob ##> "@#team alice 2"
      bob <# "@alice 2"
      alice <# "bob> 2"

      alice ##> "@#club bob 3"
      alice <# "@bob 3"
      bob <# "alice> 3"

      bob ##> "@#club alice 4"
      bob <# "@alice 4"
      alice <# "bob> 4"

      -- alice deletes contact with bob
      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      bob ##> "/d alice"
      bob <## "alice: contact is deleted"

      -- group messages work
      alice #> "#team hello"
      bob <# "#team alice> hello"
      bob #> "#team hi there"
      alice <# "#team bob> hi there"

      -- alice creates member contact with bob
      alice ##> "@#team bob hi"
      alice
        <### [ "member #team bob does not have direct connection, creating",
               "contact for member #team bob is created",
               "sent invitation to connect directly to member #team bob",
               WithTime "@bob hi"
             ]
      bob
        <### [ "#team alice is creating direct contact alice with you",
               WithTime "alice> hi"
             ]
      bob <## "alice (Alice): you can send messages to contact"
      concurrently_
        (alice <## "bob (Bob): contact is connected")
        (bob <## "alice (Alice): contact is connected")

      -- alice can message bob via team and via club
      alice ##> "@#team bob 1"
      alice <# "@bob 1"
      bob <# "alice> 1"

      bob ##> "@#team alice 2"
      bob <# "@alice 2"
      alice <# "bob> 2"

      alice ##> "@#club bob 3"
      alice <# "@bob 3"
      bob <# "alice> 3"

      bob ##> "@#club alice 4"
      bob <# "@alice 4"
      alice <# "bob> 4"

testMemberContactAccept :: HasCallStack => TestParams -> IO ()
testMemberContactAccept =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      -- bob and cath connect
      bob ##> "/_create member contact #1 3"
      bob <## "contact for member #team cath is created"

      bob ##> "/_invite member contact @3"
      bob <## "sent invitation to connect directly to member #team cath"
      cath <## "#team bob requests to create direct contact with you"
      cath <## "to accept: /accept_member_contact @bob"
      cath <## "to reject: /delete @bob (the sender will NOT be notified)"

      cath #$> ("/_get chat @3 count=1", chat, [(0, "requested connection from group team")])

      cath ##> "/accept_member_contact @bob"
      cath <## "contact bob is accepted, starting connection"
      concurrently_
        (bob <## "cath (Catherine): contact is connected")
        (cath <## "bob (Bob): contact is connected")

      bob <##> cath

      -- if group is deleted, bob and cath keep contact with each other
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

      bob <##> cath

testMemberContactAcceptIncognito :: HasCallStack => TestParams -> IO ()
testMemberContactAcceptIncognito =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      -- create group, bob joins incognito
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      bob ##> ("/c i " <> gLink)
      bobIncognito <- getTermLine bob
      bob <## "connection request sent incognito!"
      alice <## (bobIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> bobIncognito <> " joined the group"),
          do
            bob <## "#team: joining the group..."
            bob <## ("#team: you joined the group incognito as " <> bobIncognito)
        ]
      -- cath joins incognito
      cath ##> ("/c i " <> gLink)
      cathIncognito <- getTermLine cath
      cath <## "connection request sent incognito!"
      alice <## (cathIncognito <> ": accepting request to join group #team...")
      concurrentlyN_
        [ alice <## ("#team: " <> cathIncognito <> " joined the group"),
          do
            cath <## "#team: joining the group..."
            cath <## ("#team: you joined the group incognito as " <> cathIncognito)
            cath <## ("#team: member " <> bobIncognito <> " is connected"),
          do
            bob <## ("#team: alice added " <> cathIncognito <> " to the group (connecting...)")
            bob <## ("#team: new member " <> cathIncognito <> " is connected")
        ]

      threadDelay 1000000

      -- bob and cath connect
      bob ##> "/_create member contact #1 3"
      bob <## ("contact for member #team " <> cathIncognito <> " is created")

      bob ##> "/_invite member contact @2"
      bob <## ("sent invitation to connect directly to member #team " <> cathIncognito)
      cath <## ("#team " <> bobIncognito <> " requests to create direct contact with you")
      cath <## ("to accept: /accept_member_contact @" <> bobIncognito)
      cath <## ("to reject: /delete @" <> bobIncognito <> " (the sender will NOT be notified)")

      -- check correct incognito profiles are used
      bob @@@ [("@" <> cathIncognito, "chat banner"), ("#team", "connected")]

      bob ##> ("/i " <> cathIncognito)
      bob <## "contact ID: 2"
      bob <##. "receiving messages via"
      bob <## ("you've shared incognito profile with this contact: " <> bobIncognito)
      bob <## "connection not verified, use /code command to see security code"
      bob <## currentChatVRangeInfo

      cath @@@ [("@" <> bobIncognito, "requested connection from group team"), ("#team", "started direct connection with you")]
      cath #$> ("/_get chat @2 count=1", chat, [(0, "requested connection from group team")])

      cath ##> ("/i " <> bobIncognito)
      cath <## "contact ID: 2"
      cath <## ("you've shared incognito profile with this contact: " <> cathIncognito)
      cath <## "connection not verified, use /code command to see security code"
      cath <## currentChatVRangeInfo

      -- accept connection
      cath ##> ("/accept_member_contact @" <> bobIncognito)
      cath <## ("contact " <> bobIncognito <> " is accepted, starting connection")
      _ <- getTermLine bob
      _ <- getTermLine cath
      concurrentlyN_
        [ do
            bob <## (cathIncognito <> ": contact is connected, your incognito profile for this contact is " <> bobIncognito)
            bob <## ("use /i " <> cathIncognito <> " to print out this incognito profile again"),
          do
            cath <## (bobIncognito <> ": contact is connected, your incognito profile for this contact is " <> cathIncognito)
            cath <## ("use /i " <> bobIncognito <> " to print out this incognito profile again")
        ]

      bob ?#> ("@" <> cathIncognito <> " hi")
      cath ?<# (bobIncognito <> "> hi")
      cath ?#> ("@" <> bobIncognito <> " hey")
      bob ?<# (cathIncognito <> "> hey")

      -- if group is deleted, bob and cath keep contact with each other
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

      bob ?#> ("@" <> cathIncognito <> " hi")
      cath ?<# (bobIncognito <> "> hi")
      cath ?#> ("@" <> bobIncognito <> " hey")
      bob ?<# (cathIncognito <> "> hey")

testGroupMsgForwardMessage :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      threadDelay 1000000

      cath #> "#team hey team"
      alice <# "#team cath> hey team"
      bob <# "#team cath> hey team [>>]"

      alice ##> "/tail #team 2"
      alice <# "#team bob> hi there"
      alice <# "#team cath> hey team"

      bob ##> "/tail #team 2"
      bob <# "#team hi there"
      bob <# "#team cath> hey team [>>]"

      cath ##> "/tail #team 2"
      cath <# "#team bob> hi there [>>]"
      cath <# "#team hey team"

testGroupMsgForwardBatched :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardBatched =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob ##> "/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"test 1\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 2\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 3\"}}]"
      bob <# "#team test 1"
      bob <# "#team test 2"
      bob <# "#team test 3"
      alice <# "#team bob> test 1"
      alice <# "#team bob> test 2"
      alice <# "#team bob> test 3"
      cath <# "#team bob> test 1 [>>]"
      cath <# "#team bob> test 2 [>>]"
      cath <# "#team bob> test 3 [>>]"

      threadDelay 1000000

      cath ##> "/_send #1 json [{\"msgContent\": {\"type\": \"text\", \"text\": \"test 4\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 5\"}}, {\"msgContent\": {\"type\": \"text\", \"text\": \"test 6\"}}]"
      cath <# "#team test 4"
      cath <# "#team test 5"
      cath <# "#team test 6"
      alice <# "#team cath> test 4"
      alice <# "#team cath> test 5"
      alice <# "#team cath> test 6"
      bob <# "#team cath> test 4 [>>]"
      bob <# "#team cath> test 5 [>>]"
      bob <# "#team cath> test 6 [>>]"

      alice ##> "/tail #team 6"
      alice <# "#team bob> test 1"
      alice <# "#team bob> test 2"
      alice <# "#team bob> test 3"
      alice <# "#team cath> test 4"
      alice <# "#team cath> test 5"
      alice <# "#team cath> test 6"

      bob ##> "/tail #team 6"
      bob <# "#team test 1"
      bob <# "#team test 2"
      bob <# "#team test 3"
      bob <# "#team cath> test 4 [>>]"
      bob <# "#team cath> test 5 [>>]"
      bob <# "#team cath> test 6 [>>]"

      cath ##> "/tail #team 6"
      cath <# "#team bob> test 1 [>>]"
      cath <# "#team bob> test 2 [>>]"
      cath <# "#team bob> test 3 [>>]"
      cath <# "#team test 4"
      cath <# "#team test 5"
      cath <# "#team test 6"

testGroupMsgForwardReport :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardReport =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      alice ##> "/mr team bob moderator"
      concurrentlyN_
        [ alice <## "#team: you changed the role of bob to moderator",
          bob <## "#team: alice changed your role from admin to moderator",
          cath <## "#team: alice changed the role of bob from admin to moderator"
        ]

      alice ##> "/mr team cath member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of cath to member",
          bob <## "#team: alice changed the role of cath from admin to member",
          cath <## "#team: alice changed your role from admin to member"
        ]
      cath ##> "/report #team content hi there"
      cath <# "#team (support) > bob hi there"
      cath <## "      report content"
      concurrentlyN_
        [ do
            alice <# "#team (support: cath) cath> > bob hi there"
            alice <## "      report content",
          do
            bob <# "#team (support: cath) cath!> > bob hi there [>>]"
            bob <## "      report content [>>]"
        ]

      alice ##> "/mr team bob member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of bob to member",
          bob <## "#team: alice changed your role from moderator to member",
          cath <## "#team: alice changed the role of bob from moderator to member"
        ]

      cath ##> "/report #team content hi there"
      cath <# "#team (support) > bob hi there"
      cath <## "      report content"
      concurrentlyN_
        [ do
            alice <# "#team (support: cath) cath> > bob hi there"
            alice <## "      report content",
          (bob </)
        ]

      -- regular messages are still forwarded

      cath #> "#team hey team"
      alice <# "#team cath> hey team"
      bob <# "#team cath> hey team [>>]"

setupGroupForwarding :: TestCC -> TestCC -> TestCC -> IO ()
setupGroupForwarding host invitee1 invitee2 = do
  threadDelay 1000000 -- delay so member relations don't get overwritten to connected

  invitee1Name <- userName invitee1
  invitee2Name <- userName invitee2

  -- set up test: break connections between invitee1 and invitee2 to enable group forwarding
  void $ withCCTransaction invitee1 $ \db ->
    DB.execute
      db
      [sql|
        UPDATE connections SET conn_status='deleted'
        WHERE group_member_id IN (SELECT group_member_id FROM group_members WHERE local_display_name = ?)
      |]
      (Only invitee2Name)
  void $ withCCTransaction invitee2 $ \db ->
    DB.execute
      db
      [sql|
        UPDATE connections SET conn_status='deleted'
        WHERE group_member_id IN (SELECT group_member_id FROM group_members WHERE local_display_name = ?)
      |]
      (Only invitee1Name)

  setupGroupForwardingVectors host invitee1 invitee2

setupGroupForwardingVectors :: TestCC -> TestCC -> TestCC -> IO ()
setupGroupForwardingVectors host invitee1 invitee2 = do
  invitee1Name <- userName invitee1
  invitee2Name <- userName invitee2
  updateGroupForwardingVectors host invitee1Name invitee2Name MRIntroduced

updateGroupForwardingVectors :: TestCC -> String -> String -> MemberRelation -> IO ()
updateGroupForwardingVectors host invitee1Name invitee2Name relation = do
  void $ withCCTransaction host $ \db -> do
    [(invitee1Index, invitee1Vec)] <- DB.query db
      [sql|
        SELECT index_in_group, member_relations_vector
        FROM group_members
        WHERE local_display_name = ?
      |]
      (Only invitee1Name)
    [(invitee2Index, invitee2Vec)] <- DB.query db
      [sql|
        SELECT index_in_group, member_relations_vector
        FROM group_members
        WHERE local_display_name = ?
      |]
      (Only invitee2Name)

    let invitee1Vec' = setRelation invitee2Index relation (fromMaybe B.empty invitee1Vec)
    DB.execute db
      [sql|
        UPDATE group_members
        SET member_relations_vector = ?
        WHERE local_display_name = ?
      |]
      (Binary invitee1Vec', invitee1Name)

    let invitee2Vec' = setRelation invitee1Index relation (fromMaybe B.empty invitee2Vec)
    DB.execute db
      [sql|
        UPDATE group_members
        SET member_relations_vector = ?
        WHERE local_display_name = ?
      |]
      (Binary invitee2Vec', invitee2Name)

testGroupMsgForwardDeduplicate :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardDeduplicate =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      threadDelay 1000000 -- delay so member relations don't get overwritten to connected
      setupGroupForwardingVectors alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath
        <### [ Predicate ("#team bob> hi there" `isInfixOf`),
               StartsWith "duplicate group message, group id: 1"
             ]

      threadDelay 1000000

      -- cath sends x.grp.mem.con on deduplication, so alice doesn't forward anymore

      cath #> "#team hey team"
      alice <# "#team cath> hey team"
      bob <# "#team cath> hey team"

      alice ##> "/tail #team 2"
      alice <# "#team bob> hi there"
      alice <# "#team cath> hey team"

      bob ##> "/tail #team 2"
      bob <# "#team hi there"
      bob <# "#team cath> hey team"

      cath ##> "/tail #team 2"
      cath <#. "#team bob> hi there"
      cath <# "#team hey team"

testGroupMsgForwardEdit :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardEdit =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      bob ##> "! #team hello there"
      bob <# "#team [edited] hello there"
      alice <# "#team bob> [edited] hello there"
      cath <# "#team bob> [edited] hello there" -- TODO show as forwarded
      alice ##> "/tail #team 1"
      alice <# "#team bob> hello there"

      bob ##> "/tail #team 1"
      bob <# "#team hello there"

      cath ##> "/tail #team 1"
      cath <# "#team bob> hello there [>>]"

testGroupMsgForwardReaction :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardReaction =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      cath ##> "+1 #team hi there"
      cath <## "added 👍"
      alice <# "#team cath> > bob hi there"
      alice <## "    + 👍"
      bob <# "#team cath> > bob hi there"
      bob <## "    + 👍"

testGroupMsgForwardDeletion :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardDeletion =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      bob ##> "\\ #team hi there"
      bob <## "message marked deleted"
      alice <# "#team bob> [marked deleted] hi there"
      cath <# "#team bob> [marked deleted] hi there" -- TODO show as forwarded

testGroupMsgForwardFile :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardFile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob #> "/f #team ./tests/fixtures/test.jpg"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (test.jpg) for #team"
      concurrentlyN_
        [ do
            alice <# "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
            alice <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it [>>]"
        ]
      cath ##> "/fr 1 ./tests/tmp"
      cath <## "saving file 1 from bob to ./tests/tmp/test.jpg"
      cath <## "started receiving file 1 (test.jpg) from bob"
      cath <## "completed receiving file 1 (test.jpg) from bob"
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testGroupMsgForwardChangeRole :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardChangeRole =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      cath ##> "/mr #team bob member"
      cath <## "#team: you changed the role of bob to member"
      alice <## "#team: cath changed the role of bob from admin to member"
      bob <## "#team: cath changed your role from admin to member" -- TODO show as forwarded

testGroupMsgForwardNewMember :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardNewMember =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      connectUsers cath dan
      cath ##> "/a #team dan"
      cath <## "invitation to join the group #team sent to dan"
      dan <## "#team: cath invites you to join the group as member"
      dan <## "use /j team to accept"
      dan ##> "/j #team"
      dan <## "#team: you joined the group"
      concurrentlyN_
        [ cath <## "#team: dan joined the group",
          do
            alice <## "#team: cath added dan (Daniel) to the group (connecting...)"
            alice <## "#team: new member dan is connected",
          -- bob will not connect to dan, as introductions are not forwarded (yet?)
          bob <## "#team: cath added dan (Daniel) to the group (connecting...)", -- TODO show as forwarded
          dan <## "#team: member alice (Alice) is connected"
        ]

      dan #> "#team hello all"
      alice <# "#team dan> hello all"
      -- bob <# "#team dan> hello all [>>]"
      cath <# "#team dan> hello all"

      bob #> "#team hi all"
      alice <# "#team bob> hi all"
      cath <# "#team bob> hi all [>>]"
      -- dan <# "#team bob> hi all [>>]"

      bob ##> "/ms team"
      bob
        <### [ "alice (Alice): owner, host, connected",
               "bob (Bob): admin, you, connected",
               "cath (Catherine): admin, connected",
               "dan (Daniel): member"
             ]

testGroupMsgForwardLeave :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardLeave =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      setupGroupForwarding alice bob cath

      bob ##> "/leave #team"
      bob <## "#team: you left the group"
      bob <## "use /d #team to delete the group"
      alice <## "#team: bob left the group"
      cath <## "#team: bob left the group"

testGroupMsgForwardMemberRemoval :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardMemberRemoval =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GRAdmin) (cath, GRMember)
      setupGroupForwarding alice bob cath

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
      cath <## "bad chat command: not current member"

testGroupMsgForwardAdminRemoval :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardAdminRemoval =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GROwner) (cath, GRMember)
      setupGroupForwarding alice bob cath

      -- alice forwards messages between bob and cath
      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      cath #> "#team hey"
      alice <# "#team cath> hey"
      bob <# "#team cath> hey [>>]"

      -- if alice is removed, she forwards message of her own removal
      bob ##> "/rm team alice"
      concurrentlyN_
        [ bob <## "#team: you removed alice from the group",
          do
            alice <## "#team: bob removed you from the group"
            alice <## "use /d #team to delete the group",
          cath <## "#team: bob removed alice from the group"
        ]

      -- there is no forwarding admin anymore between bob and cath, so messages don't get delivered
      -- (this is not a desired behavior, just a test demonstration/proof of current implementation)
      bob #> "#team hi"
      concurrently_
        (cath </)
        (alice </)
      cath #> "#team hello"
      concurrently_
        (bob </)
        (alice </)
      alice ##> "#team hello"
      alice <## "bad chat command: not current member"

testGroupMsgForwardGroupDeletion :: HasCallStack => TestParams -> IO ()
testGroupMsgForwardGroupDeletion =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3' "team" alice (bob, GROwner) (cath, GRMember)
      setupGroupForwarding alice bob cath

      -- alice forwards messages between bob and cath
      bob #> "#team hi there"
      alice <# "#team bob> hi there"
      cath <# "#team bob> hi there [>>]"

      cath #> "#team hey"
      alice <# "#team cath> hey"
      bob <# "#team cath> hey [>>]"

      -- if bob deletes the group, alice forwards it to cath
      bob ##> "/d #team"
      concurrentlyN_
        [ bob <## "#team: you deleted the group",
          do
            alice <## "#team: bob deleted the group"
            alice <## "use /d #team to delete the local copy of the group",
          do
            cath <## "#team: bob deleted the group"
            cath <## "use /d #team to delete the local copy of the group"
        ]

      alice ##> "/groups"
      alice <## "#team (group deleted, delete local copy: /d #team)"
      bob ##> "/groups"
      bob <## "you have no groups!"
      bob <## "to create: /g <name>"
      cath ##> "/groups"
      cath <## "#team (group deleted, delete local copy: /d #team)"

testGroupHistory :: HasCallStack => TestParams -> IO ()
testGroupHistory =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team hello"
      bob <# "#team alice> hello"

      threadDelay 1000000

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   WithTime "#team bob> hey! [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine cath
      r `shouldContain` [(0, "hello"), (0, "hey!")]

      -- message delivery works after sending history
      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

testGroupHistoryGroupLink :: HasCallStack => TestParams -> IO ()
testGroupHistoryGroupLink =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team hello"
      bob <# "#team alice> hello"

      threadDelay 1000000

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True

      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   WithTime "#team bob> hey! [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine cath
      r `shouldContain` [(0, "hello"), (0, "hey!")]

      -- message delivery works after sending history
      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

testGroupHistoryPreferenceOff :: HasCallStack => TestParams -> IO ()
testGroupHistoryPreferenceOff =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team hello"
      bob <# "#team alice> hello"

      threadDelay 1000000

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   WithTime "#team bob> hey! [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine cath
      r `shouldContain` [(0, "hello"), (0, "hey!")]

      alice ##> "/set history #team off"
      alice <## "updated group preferences:"
      alice <## "Recent history: off"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "updated group preferences:"
            bob <## "Recent history: off",
          do
            cath <## "alice updated group #team:"
            cath <## "updated group preferences:"
            cath <## "Recent history: off"
        ]

      connectUsers alice dan
      addMember "team" alice dan GRAdmin
      dan ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: dan joined the group",
          do
            dan <## "#team: you joined the group"
            dan
              <### [ "#team: member bob (Bob) is connected",
                     "#team: member cath (Catherine) is connected"
                   ],
          aliceAddedDan bob,
          aliceAddedDan cath
        ]

      dan ##> "/_get chat #1 count=100"
      r' <- chat <$> getTermLine dan
      r' `shouldNotContain` [(0, "hello")]
      r' `shouldNotContain` [(0, "hey!")]

      -- message delivery works
      alice #> "#team 1"
      [bob, cath, dan] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath, dan] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob, dan] *<# "#team cath> 3"
      dan #> "#team 4"
      [alice, bob, cath] *<# "#team dan> 4"
  where
    aliceAddedDan :: HasCallStack => TestCC -> IO ()
    aliceAddedDan cc = do
      cc <## "#team: alice added dan (Daniel) to the group (connecting...)"
      cc <## "#team: new member dan is connected"

testGroupHistoryHostFile :: HasCallStack => TestParams -> IO ()
testGroupHistoryHostFile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup2 "team" alice bob

      alice #> "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      alice <## "completed uploading file 1 (test.jpg) for #team"

      bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]",
                   "use /fr 1 [<dir>/ | <path>] to receive it [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from alice to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from alice"
             ]
      cath <## "completed receiving file 1 (test.jpg) from alice"
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testGroupHistoryMemberFile :: HasCallStack => TestParams -> IO ()
testGroupHistoryMemberFile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      createGroup2 "team" alice bob

      bob #> "/f #team ./tests/fixtures/test.jpg"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (test.jpg) for #team"

      alice <# "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team bob> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]",
                   "use /fr 1 [<dir>/ | <path>] to receive it [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from bob to ./tests/tmp/test.jpg",
               "started receiving file 1 (test.jpg) from bob"
             ]
      cath <## "completed receiving file 1 (test.jpg) from bob"
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testGroupHistoryLargeFile :: HasCallStack => TestParams -> IO ()
testGroupHistoryLargeFile =
  testChatCfg3 cfg aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile", "17mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile"]

      createGroup2 "team" alice bob

      bob ##> "/_send #1 json [{\"filePath\": \"./tests/tmp/testfile\", \"msgContent\": {\"text\":\"hello\",\"type\":\"file\"}}]"
      bob <# "#team hello"
      bob <# "/f #team ./tests/tmp/testfile"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (testfile) for #team"

      alice <# "#team bob> hello"
      alice <# "#team bob> sends file testfile (17.0 MiB / 17825792 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      -- admin receiving file does not prevent the new member from receiving it later
      alice ##> "/fr 1 ./tests/tmp"
      alice
        <### [ "saving file 1 from bob to ./tests/tmp/testfile_1",
               "started receiving file 1 (testfile) from bob"
             ]
      alice <## "completed receiving file 1 (testfile) from bob"
      src <- B.readFile "./tests/tmp/testfile"
      destAlice <- B.readFile "./tests/tmp/testfile_1"
      destAlice `shouldBe` src

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team bob> hello [>>]",
                   WithTime "#team bob> sends file testfile (17.0 MiB / 17825792 bytes) [>>]",
                   "use /fr 1 [<dir>/ | <path>] to receive it [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from bob to ./tests/tmp/testfile_2",
               "started receiving file 1 (testfile) from bob"
             ]
      cath <## "completed receiving file 1 (testfile) from bob"

      destCath <- B.readFile "./tests/tmp/testfile_2"
      destCath `shouldBe` src
  where
    cfg = testCfg {xftpDescrPartSize = 200}

testGroupHistoryMultipleFiles :: HasCallStack => TestParams -> IO ()
testGroupHistoryMultipleFiles =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile_bob", "2mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_bob"]
      xftpCLI ["rand", "./tests/tmp/testfile_alice", "1mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_alice"]

      createGroup2 "team" alice bob

      threadDelay 1000000

      bob ##> "/_send #1 json [{\"filePath\": \"./tests/tmp/testfile_bob\", \"msgContent\": {\"text\":\"hi alice\",\"type\":\"file\"}}]"
      bob <# "#team hi alice"
      bob <# "/f #team ./tests/tmp/testfile_bob"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (testfile_bob) for #team"

      alice <# "#team bob> hi alice"
      alice <# "#team bob> sends file testfile_bob (2.0 MiB / 2097152 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      threadDelay 1000000

      alice ##> "/_send #1 json [{\"filePath\": \"./tests/tmp/testfile_alice\", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"file\"}}]"
      alice <# "#team hey bob"
      alice <# "/f #team ./tests/tmp/testfile_alice"
      alice <## "use /fc 2 to cancel sending"
      alice <## "completed uploading file 2 (testfile_alice) for #team"

      bob <# "#team alice> hey bob"
      bob <# "#team alice> sends file testfile_alice (1.0 MiB / 1048576 bytes)"
      bob <## "use /fr 2 [<dir>/ | <path>] to receive it"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team bob> hi alice [>>]",
                   WithTime "#team bob> sends file testfile_bob (2.0 MiB / 2097152 bytes) [>>]",
                   "use /fr 1 [<dir>/ | <path>] to receive it [>>]",
                   WithTime "#team alice> hey bob [>>]",
                   WithTime "#team alice> sends file testfile_alice (1.0 MiB / 1048576 bytes) [>>]",
                   "use /fr 2 [<dir>/ | <path>] to receive it [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/fr 1 ./tests/tmp"
      cath
        <### [ "saving file 1 from bob to ./tests/tmp/testfile_bob_1",
               "started receiving file 1 (testfile_bob) from bob"
             ]
      cath <## "completed receiving file 1 (testfile_bob) from bob"
      srcBob <- B.readFile "./tests/tmp/testfile_bob"
      destBob <- B.readFile "./tests/tmp/testfile_bob_1"
      destBob `shouldBe` srcBob

      cath ##> "/fr 2 ./tests/tmp"
      cath
        <### [ "saving file 2 from alice to ./tests/tmp/testfile_alice_1",
               "started receiving file 2 (testfile_alice) from alice"
             ]
      cath <## "completed receiving file 2 (testfile_alice) from alice"
      srcAlice <- B.readFile "./tests/tmp/testfile_alice"
      destAlice <- B.readFile "./tests/tmp/testfile_alice_1"
      destAlice `shouldBe` srcAlice

      cath ##> "/_get chat #1 count=100"
      r <- chatF <$> getTermLine cath
      r
        `shouldContain` [ ((0, "hi alice"), Just "./tests/tmp/testfile_bob_1"),
                          ((0, "hey bob"), Just "./tests/tmp/testfile_alice_1")
                        ]

testGroupHistoryFileCancel :: HasCallStack => TestParams -> IO ()
testGroupHistoryFileCancel =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile_bob", "2mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_bob"]
      xftpCLI ["rand", "./tests/tmp/testfile_alice", "1mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_alice"]

      createGroup2 "team" alice bob

      bob ##> "/_send #1 json [{\"filePath\": \"./tests/tmp/testfile_bob\", \"msgContent\": {\"text\":\"hi alice\",\"type\":\"file\"}}]"
      bob <# "#team hi alice"
      bob <# "/f #team ./tests/tmp/testfile_bob"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (testfile_bob) for #team"

      alice <# "#team bob> hi alice"
      alice <# "#team bob> sends file testfile_bob (2.0 MiB / 2097152 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob ##> "/fc 1"
      bob <## "cancelled sending file 1 (testfile_bob) to alice"
      alice <## "bob cancelled sending file 1 (testfile_bob)"

      threadDelay 1000000

      alice ##> "/_send #1 json [{\"filePath\": \"./tests/tmp/testfile_alice\", \"msgContent\": {\"text\":\"hey bob\",\"type\":\"file\"}}]"
      alice <# "#team hey bob"
      alice <# "/f #team ./tests/tmp/testfile_alice"
      alice <## "use /fc 2 to cancel sending"
      alice <## "completed uploading file 2 (testfile_alice) for #team"

      bob <# "#team alice> hey bob"
      bob <# "#team alice> sends file testfile_alice (1.0 MiB / 1048576 bytes)"
      bob <## "use /fr 2 [<dir>/ | <path>] to receive it"

      alice ##> "/fc 2"
      alice <## "cancelled sending file 2 (testfile_alice) to bob"
      bob <## "alice cancelled sending file 2 (testfile_alice)"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team bob> hi alice [>>]",
                   WithTime "#team alice> hey bob [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

testGroupHistoryFileCancelNoText :: HasCallStack => TestParams -> IO ()
testGroupHistoryFileCancelNoText =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> withXFTPServer $ do
      xftpCLI ["rand", "./tests/tmp/testfile_bob", "2mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_bob"]
      xftpCLI ["rand", "./tests/tmp/testfile_alice", "1mb"] `shouldReturn` ["File created: " <> "./tests/tmp/testfile_alice"]

      createGroup2 "team" alice bob

      alice #> "#team hello"
      bob <# "#team alice> hello"

      -- bob file

      bob #> "/f #team ./tests/tmp/testfile_bob"
      bob <## "use /fc 1 to cancel sending"
      bob <## "completed uploading file 1 (testfile_bob) for #team"

      alice <# "#team bob> sends file testfile_bob (2.0 MiB / 2097152 bytes)"
      alice <## "use /fr 1 [<dir>/ | <path>] to receive it"

      bob ##> "/fc 1"
      bob <## "cancelled sending file 1 (testfile_bob) to alice"
      alice <## "bob cancelled sending file 1 (testfile_bob)"

      -- alice file

      alice #> "/f #team ./tests/tmp/testfile_alice"
      alice <## "use /fc 2 to cancel sending"
      alice <## "completed uploading file 2 (testfile_alice) for #team"

      bob <# "#team alice> sends file testfile_alice (1.0 MiB / 1048576 bytes)"
      bob <## "use /fr 2 [<dir>/ | <path>] to receive it"

      alice ##> "/fc 2"
      alice <## "cancelled sending file 2 (testfile_alice) to bob"
      bob <## "alice cancelled sending file 2 (testfile_alice)"

      -- other messages are sent

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   WithTime "#team bob> hey! [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

testGroupHistoryQuotes :: HasCallStack => TestParams -> IO ()
testGroupHistoryQuotes =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team ALICE"
      bob <# "#team alice> ALICE"

      threadDelay 1000000

      bob #> "#team BOB"
      alice <# "#team bob> BOB"

      threadDelay 1000000

      alice `send` "> #team @alice (ALICE) 1"
      alice <# "#team > alice ALICE"
      alice <## "      1"
      bob <# "#team alice> > alice ALICE"
      bob <## "      1"

      threadDelay 1000000

      alice `send` "> #team @bob (BOB) 2"
      alice <# "#team > bob BOB"
      alice <## "      2"
      bob <# "#team alice!> > bob BOB"
      bob <## "      2"

      threadDelay 1000000

      bob `send` "> #team @alice (ALICE) 3"
      bob <# "#team > alice ALICE"
      bob <## "      3"
      alice <# "#team bob!> > alice ALICE"
      alice <## "      3"

      threadDelay 1000000

      bob `send` "> #team @bob (BOB) 4"
      bob <# "#team > bob BOB"
      bob <## "      4"
      alice <# "#team bob> > bob BOB"
      alice <## "      4"

      alice
        #$> ( "/_get chat #1 count=6",
              chat',
              [ ((1, "ALICE"), Nothing),
                ((0, "BOB"), Nothing),
                ((1, "1"), Just (1, "ALICE")),
                ((1, "2"), Just (0, "BOB")),
                ((0, "3"), Just (1, "ALICE")),
                ((0, "4"), Just (0, "BOB"))
              ]
            )
      bob
        #$> ( "/_get chat #1 count=6",
              chat',
              [ ((0, "ALICE"), Nothing),
                ((1, "BOB"), Nothing),
                ((0, "1"), Just (0, "ALICE")),
                ((0, "2"), Just (1, "BOB")),
                ((1, "3"), Just (0, "ALICE")),
                ((1, "4"), Just (1, "BOB"))
              ]
            )

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> ALICE [>>]",
                   WithTime "#team bob> BOB [>>]",
                   WithTime "#team alice> > alice ALICE [>>]",
                   "      1 [>>]",
                   WithTime "#team alice> > bob BOB [>>]",
                   "      2 [>>]",
                   WithTime "#team bob> > alice ALICE [>>]",
                   "      3 [>>]",
                   WithTime "#team bob> > bob BOB [>>]",
                   "      4 [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat' <$> getTermLine cath
      r
        `shouldContain` [ ((0, "ALICE"), Nothing),
                          ((0, "BOB"), Nothing),
                          ((0, "1"), Just (0, "ALICE")),
                          ((0, "2"), Just (0, "BOB")),
                          ((0, "3"), Just (0, "ALICE")),
                          ((0, "4"), Just (0, "BOB"))
                        ]

testGroupHistoryDeletedMessage :: HasCallStack => TestParams -> IO ()
testGroupHistoryDeletedMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob
      -- disableFullDeletion2 "team" alice bob

      alice #> "#team hello"
      bob <# "#team alice> hello"

      threadDelay 1000000

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      bobMsgId <- lastItemId bob
      bob #$> ("/_delete item #1 " <> bobMsgId <> " broadcast", id, "message marked deleted")
      alice <# "#team bob> [marked deleted] hey!"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine cath
      r `shouldContain` [(0, "hello")]
      r `shouldNotContain` [(0, "hey!")]

testGroupHistoryDisappearingMessage :: HasCallStack => TestParams -> IO ()
testGroupHistoryDisappearingMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team 1"
      bob <# "#team alice> 1"

      threadDelay 1000000

      -- 3 seconds so that messages 2 and 3 are not deleted for alice before sending history to cath
      alice ##> "/set disappear #team on 4"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: on (4 sec)"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: on (4 sec)"

      bob #> "#team 2"
      alice <# "#team bob> 2"

      threadDelay 1000000

      alice #> "#team 3"
      bob <# "#team alice> 3"

      threadDelay 1000000

      alice ##> "/set disappear #team off"
      alice <## "updated group preferences:"
      alice <## "Disappearing messages: off"
      bob <## "alice updated group #team:"
      bob <## "updated group preferences:"
      bob <## "Disappearing messages: off"

      bob #> "#team 4"
      alice <# "#team bob> 4"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> 1 [>>]",
                   WithTime "#team bob> 2 [>>]",
                   WithTime "#team alice> 3 [>>]",
                   WithTime "#team bob> 4 [>>]",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r1 <- chat <$> getTermLine cath
      r1 `shouldContain` [(0, "1"), (0, "2"), (0, "3"), (0, "4")]

      concurrentlyN_
        [ alice
            <### [ "timed message deleted: 2",
                   "timed message deleted: 3"
                 ],
          bob
            <### [ "timed message deleted: 2",
                   "timed message deleted: 3"
                 ],
          cath
            <### [ "timed message deleted: 2",
                   "timed message deleted: 3"
                 ]
        ]

      cath ##> "/_get chat #1 count=100"
      r2 <- chat <$> getTermLine cath
      r2 `shouldContain` [(0, "1"), (0, "4")]
      r2 `shouldNotContain` [(0, "2")]
      r2 `shouldNotContain` [(0, "3")]

testGroupHistoryWelcomeMessage :: HasCallStack => TestParams -> IO ()
testGroupHistoryWelcomeMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      alice ##> "/set welcome #team welcome to team"
      alice <## "welcome message changed to:"
      alice <## "welcome to team"

      bob <## "alice updated group #team:"
      bob <## "welcome message changed to:"
      bob <## "welcome to team"

      threadDelay 1000000

      alice #> "#team hello"
      bob <# "#team alice> hello"

      threadDelay 1000000

      bob #> "#team hey!"
      alice <# "#team bob> hey!"

      connectUsers alice cath
      addMember "team" alice cath GRAdmin
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hello [>>]",
                   WithTime "#team bob> hey! [>>]",
                   WithTime "#team alice> welcome to team",
                   "#team: member bob (Bob) is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

      cath ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine cath
      -- sometimes there are "connected" and feature items in between,
      -- so we filter them out; `shouldContain` then checks order is correct
      let expected = [(0, "hello"), (0, "hey!"), (0, "welcome to team")]
          r' = filter (`elem` expected) r
      r' `shouldContain` expected

      -- message delivery works after sending history
      alice #> "#team 1"
      [bob, cath] *<# "#team alice> 1"
      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2"
      cath #> "#team 3"
      [alice, bob] *<# "#team cath> 3"

testGroupHistoryUnknownMember :: HasCallStack => TestParams -> IO ()
testGroupHistoryUnknownMember =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "team" alice bob cath

      threadDelay 1000000

      alice #> "#team hi from alice"
      [bob, cath] *<# "#team alice> hi from alice"

      threadDelay 1000000

      bob #> "#team hi from bob"
      [alice, cath] *<# "#team bob> hi from bob"

      threadDelay 1000000

      cath #> "#team hi from cath"
      [alice, bob] *<# "#team cath> hi from cath"

      bob ##> "/l team"
      concurrentlyN_
        [ do
            bob <## "#team: you left the group"
            bob <## "use /d #team to delete the group",
          alice <## "#team: bob left the group",
          cath <## "#team: bob left the group"
        ]

      connectUsers alice dan
      addMember "team" alice dan GRAdmin
      dan ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: dan joined the group",
          dan
            <### [ "#team: you joined the group",
                   WithTime "#team alice> hi from alice [>>]",
                   StartsWith "#team: alice forwarded a message from an unknown member, creating unknown member record",
                   EndsWith "hi from bob [>>]",
                   WithTime "#team cath> hi from cath [>>]",
                   "#team: member cath (Catherine) is connected"
                 ],
          do
            cath <## "#team: alice added dan (Daniel) to the group (connecting...)"
            cath <## "#team: new member dan is connected"
        ]

      dan ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine dan
      r `shouldContain` [(0, "hi from alice"), (0, "hi from bob"), (0, "hi from cath")]

      dan ##> "/ms team"
      dan
        <### [ "dan (Daniel): admin, you, connected",
               "alice (Alice): owner, host, connected",
               "cath (Catherine): admin, connected",
               EndsWith "author, status unknown"
             ]

      -- message delivery works after sending history
      alice #> "#team 1"
      [cath, dan] *<# "#team alice> 1"
      cath #> "#team 2"
      [alice, dan] *<# "#team cath> 2"
      dan #> "#team 3"
      [alice, cath] *<# "#team dan> 3"

testMembershipProfileUpdateNextGroupMessage :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateNextGroupMessage =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      -- create group 1
      threadDelay 100000
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "to add members use /a team <name> or /create link #team"
      alice ##> "/create link #team"
      gLinkTeam <- getGroupLink alice "team" GRMember True
      bob ##> ("/c " <> gLinkTeam)
      bob <## "connection request sent!"
      alice <## "bob (Bob): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team: joining the group..."
            bob <## "#team: you joined the group"
        ]

      -- create group 2
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "to add members use /a club <name> or /create link #club"
      alice ##> "/create link #club"
      gLinkClub <- getGroupLink alice "club" GRMember True
      cath ##> ("/c " <> gLinkClub)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting request to join group #club..."
      concurrentlyN_
        [ alice <## "#club: cath joined the group",
          do
            cath <## "#club: joining the group..."
            cath <## "#club: you joined the group"
        ]

      -- alice has no contacts
      alice ##> "/contacts"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice #> "#club hello club"
      cath <# "#club alice> hello club"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      -- update profile in group 1

      bob ##> "/ms team"
      bob
        <### [ "bob (Bob): member, you, connected",
               "alice (Alice): owner, host, connected"
             ]

      alice #> "#team team 1"
      bob <# "#team alisa> team 1"
      cath <// 50000

      bob ##> "/ms team"
      bob
        <### [ "bob (Bob): member, you, connected",
               "alisa: owner, host, connected"
             ]

      alice #> "#team team 2"
      bob <# "#team alisa> team 2"

      bob ##> "/_get chat #1 count=100"
      rb <- chat <$> getTermLine bob
      rb `shouldContain` [(0, "updated profile")]

      -- update profile in group 2

      cath ##> "/ms club"
      cath
        <### [ "cath (Catherine): member, you, connected",
               "alice (Alice): owner, host, connected"
             ]

      alice #> "#club club 1"
      cath <# "#club alisa> club 1"

      cath ##> "/ms club"
      cath
        <### [ "cath (Catherine): member, you, connected",
               "alisa: owner, host, connected"
             ]

      alice #> "#club club 2"
      cath <# "#club alisa> club 2"

      cath ##> "/_get chat #1 count=100"
      rc <- chat <$> getTermLine cath
      rc `shouldContain` [(0, "updated profile")]

testMembershipProfileUpdateSameMember :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateSameMember =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob
      createGroup2' "club" alice (bob, GRAdmin) False

      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alice", "bob"]

      alice #> "#team team 1"
      bob <## "contact alice changed to alisa"
      bob <## "use @alisa <message> to send messages"
      bob <# "#team alisa> team 1"

      -- since members were related to the same contact, both member records are updated
      bob `hasContactProfiles` ["alisa", "bob"]
      checkMembers bob
      checkItems bob

      -- profile update is not processed in second group, since it hasn't changed
      alice #> "#club club 1"
      bob <# "#club alisa> club 1"

      bob `hasContactProfiles` ["alisa", "bob"]
      checkMembers bob
      checkItems bob
  where
    checkMembers bob = do
      bob ##> "/ms team"
      bob
        <### [ "bob (Bob): admin, you, connected",
               "alisa: owner, host, connected"
             ]
      bob ##> "/ms club"
      bob
        <### [ "bob (Bob): admin, you, connected",
               "alisa: owner, host, connected"
             ]
    checkItems bob = do
      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldNotContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rTeam <- chat <$> getTermLine bob
      rTeam `shouldContain` [(0, "updated profile")]

      bob ##> "/_get chat #2 count=100"
      rClub <- chat <$> getTermLine bob
      rClub `shouldNotContain` [(0, "updated profile")]

testMembershipProfileUpdateContactActive :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateContactActive =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      alice ##> "/contacts"
      alice <## "bob (Bob)"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 1 contacts are notified)"
      bob <## "contact alice changed to alisa"
      bob <## "use @alisa <message> to send messages"

      bob `hasContactProfiles` ["alisa", "bob"]

      alice #> "#team team 1"
      bob <# "#team alisa> team 1"

      bob `hasContactProfiles` ["alisa", "bob"]

      checkItems bob

      alice ##> "/ad"
      (sLink, _cLink) <- getContactLinks alice True
      alice ##> "/pa on"
      alice <## "new contact address set"
      bob <## "alisa set new contact address, use /info alisa to view"

      bob `hasContactProfiles` ["alisa", "bob"]
      checkAliceProfileLink bob "alisa" sLink

      -- profile update does not remove contact address from profile
      alice ##> "/p 'Alice Smith'"
      alice <## "user profile is changed to 'Alice Smith' (your 1 contacts are notified)"
      bob <## "contact alisa changed to 'Alice Smith'"
      bob <## "use @'Alice Smith' <message> to send messages"

      bob `hasContactProfiles` ["Alice Smith", "bob"]
      checkAliceProfileLink bob "'Alice Smith'" sLink

      -- receiving group message does not remove contact address from profile
      alice #> "#team team 2"
      bob <# "#team 'Alice Smith'> team 2"

      bob `hasContactProfiles` ["Alice Smith", "bob"]
      checkAliceProfileLink bob "'Alice Smith'" sLink

      checkItems bob
  where
    checkItems bob = do
      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rGrp <- chat <$> getTermLine bob
      rGrp `shouldNotContain` [(0, "updated profile")]
    checkAliceProfileLink bob name sLink = do
      bob ##> ("/info #team " <> name)
      bob <## "group ID: 1"
      bob <## "member ID: 1"
      bob <##. "receiving messages via"
      bob <##. "sending messages via"
      bob <## ("contact address: " <> sLink)
      bob <## "connection not verified, use /code command to see security code"
      bob <## currentChatVRangeInfo

testMembershipProfileUpdateContactDeleted :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateContactDeleted =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      alice ##> "/contacts"
      alice <## "bob (Bob)"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alice", "bob"]

      alice #> "#team team 1"
      bob <## "contact alice changed to alisa"
      bob <## "use @alisa <message> to send messages"
      bob <# "#team alisa> team 1"

      bob `hasContactProfiles` ["alisa", "bob"]

      checkItems bob

      -- adding contact address to profile does not share it with member
      alice ##> "/ad"
      _ <- getContactLink alice True
      alice ##> "/pa on"
      alice <## "new contact address set"

      bob `hasContactProfiles` ["alisa", "bob"]
      checkAliceNoProfileLink bob "alisa"

      alice #> "#team team 2"
      bob <# "#team alisa> team 2"

      bob `hasContactProfiles` ["alisa", "bob"]
      checkAliceNoProfileLink bob "alisa"

      -- profile update does not add contact address to member profile
      alice ##> "/p 'Alice Smith'"
      alice <## "user profile is changed to 'Alice Smith' (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alisa", "bob"]
      checkAliceNoProfileLink bob "alisa"

      alice #> "#team team 3"
      bob <## "contact alisa changed to 'Alice Smith'"
      bob <## "use @'Alice Smith' <message> to send messages"
      bob <# "#team 'Alice Smith'> team 3"

      bob `hasContactProfiles` ["Alice Smith", "bob"]
      checkAliceNoProfileLink bob "'Alice Smith'"

      checkItems bob
  where
    checkItems bob = do
      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldNotContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rGrp <- chat <$> getTermLine bob
      rGrp `shouldContain` [(0, "updated profile")]
    checkAliceNoProfileLink bob name = do
      bob ##> ("/info #team " <> name)
      bob <## "group ID: 1"
      bob <## "member ID: 1"
      bob <##. "receiving messages via"
      bob <##. "sending messages via"
      bob <## "connection not verified, use /code command to see security code"
      bob <## currentChatVRangeInfo

testMembershipProfileUpdateContactDisabled :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateContactDisabled =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      alice ##> "/contacts"
      alice <## "bob (Bob)"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice ##> "/_delete @2 notify=off"
      alice <## "bob: contact is deleted"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alice", "bob"]

      -- bob expects update from contact, so he doesn't update profile
      alice #> "#team team 1"
      bob <# "#team alice> team 1"

      bob `hasContactProfiles` ["alice", "bob"]

      -- bob sends any message to alice, increases auth err counter
      bob `send` "/feed hi all"
      bob <##. "/feed (1)"
      bob <## "[alice, contactId: 2, connId: 1] error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"

      -- on next profile update from alice member, bob considers contact disabled for purposes of profile update
      alice #> "#team team 2"
      bob <# "#team alice> team 2"

      bob `hasContactProfiles` ["alice", "bob"]

      alice ##> "/p 'Alice Smith'"
      alice <## "user profile is changed to 'Alice Smith' (your 0 contacts are notified)"

      alice #> "#team team 3"
      bob <## "contact alice changed to 'Alice Smith'"
      bob <## "use @'Alice Smith' <message> to send messages"
      bob <# "#team 'Alice Smith'> team 3"

      bob `hasContactProfiles` ["Alice Smith", "bob"]

      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldNotContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rGrp <- chat <$> getTermLine bob
      rGrp `shouldContain` [(0, "updated profile")]

testMembershipProfileUpdateNoChangeIgnored :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateNoChangeIgnored =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      alice ##> "/contacts"
      alice <## "bob (Bob)"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      alice ##> "/p alisa"
      alice <## "user profile is changed to alisa (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alice", "bob"]

      alice ##> "/p alice Alice"
      alice <## "user profile is changed to alice (Alice) (your 0 contacts are notified)"

      bob `hasContactProfiles` ["alice", "bob"]

      alice #> "#team team 1"
      bob <# "#team alice> team 1"

      bob `hasContactProfiles` ["alice", "bob"]

      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldNotContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rGrp <- chat <$> getTermLine bob
      rGrp `shouldNotContain` [(0, "updated profile")]

testMembershipProfileUpdateContactLinkIgnored :: HasCallStack => TestParams -> IO ()
testMembershipProfileUpdateContactLinkIgnored =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      createGroup2 "team" alice bob

      alice ##> "/contacts"
      alice <## "bob (Bob)"

      alice #> "#team hello team"
      bob <# "#team alice> hello team"

      alice ##> "/d bob"
      alice <## "bob: contact is deleted"
      bob <## "alice (Alice) deleted contact with you"

      alice ##> "/ad"
      _ <- getContactLink alice True
      alice ##> "/pa on"
      alice <## "new contact address set"

      bob `hasContactProfiles` ["alice", "bob"]

      alice #> "#team team 1"
      bob <# "#team alice> team 1"

      bob ##> "/_get chat @2 count=100"
      rCt <- chat <$> getTermLine bob
      rCt `shouldNotContain` [(0, "updated profile")]

      bob ##> "/_get chat #1 count=100"
      rGrp <- chat <$> getTermLine bob
      rGrp `shouldNotContain` [(0, "updated profile")]

      bob ##> "/info #team alice"
      bob <## "group ID: 1"
      bob <## "member ID: 1"
      bob <##. "receiving messages via"
      bob <##. "sending messages via"
      bob <## "connection not verified, use /code command to see security code"
      bob <## currentChatVRangeInfo

testBlockForAllMarkedBlocked :: HasCallStack => TestParams -> IO ()
testBlockForAllMarkedBlocked =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      threadDelay 1000000

      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"

      threadDelay 1000000

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"
      cath <## "#team: alice blocked bob"
      bob <// 50000

      alice ##> "/ms team"
      alice
        <### [ "alice (Alice): owner, you, created group",
               "bob (Bob): admin, invited, connected, blocked by admin",
               "cath (Catherine): admin, invited, connected"
             ]

      cath ##> "/ms team"
      cath
        <### [ "cath (Catherine): admin, you, connected",
               "alice (Alice): owner, host, connected",
               "bob (Bob): admin, connected, blocked by admin"
             ]

      bob ##> "/ms team"
      bob
        <### [ "bob (Bob): admin, you, connected",
               "alice (Alice): owner, host, connected",
               "cath (Catherine): admin, connected"
             ]

      threadDelay 1000000

      bob #> "#team 2"
      alice <# "#team bob> 2 [blocked by admin] <muted>"
      cath <# "#team bob> 2 [blocked by admin] <muted>"

      threadDelay 1000000

      bob #> "#team 3"
      alice <# "#team bob> 3 [blocked by admin] <muted>"
      cath <# "#team bob> 3 [blocked by admin] <muted>"

      threadDelay 1000000

      alice ##> "/unblock for all #team bob"
      alice <## "#team: you unblocked bob"
      cath <## "#team: alice unblocked bob"
      bob <// 50000

      threadDelay 1000000

      bob #> "#team 4"
      [alice, cath] *<# "#team bob> 4"

      alice
        #$> ( "/_get chat #1 count=6",
              chat,
              [ (0, "1"),
                (1, "blocked bob"),
                (0, "2 [blocked by admin]"),
                (0, "3 [blocked by admin]"),
                (1, "unblocked bob"),
                (0, "4")
              ]
            )
      cath
        #$> ( "/_get chat #1 count=6",
              chat,
              [ (0, "1"),
                (0, "blocked bob"),
                (0, "2 [blocked by admin]"),
                (0, "3 [blocked by admin]"),
                (0, "unblocked bob"),
                (0, "4")
              ]
            )
      bob #$> ("/_get chat #1 count=4", chat, [(1, "1"), (1, "2"), (1, "3"), (1, "4")])

testBlockForAllFullDelete :: HasCallStack => TestParams -> IO ()
testBlockForAllFullDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      alice ##> "/set delete #team on"
      alice <## "updated group preferences:"
      alice <## "Full deletion: on"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "updated group preferences:"
            bob <## "Full deletion: on",
          do
            cath <## "alice updated group #team:"
            cath <## "updated group preferences:"
            cath <## "Full deletion: on"
        ]

      threadDelay 1000000

      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"

      threadDelay 1000000

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"
      cath <## "#team: alice blocked bob"
      bob <// 50000

      threadDelay 1000000

      bob #> "#team 2"
      alice <# "#team bob> blocked [blocked by admin] <muted>"
      cath <# "#team bob> blocked [blocked by admin] <muted>"

      threadDelay 1000000

      bob #> "#team 3"
      alice <# "#team bob> blocked [blocked by admin] <muted>"
      cath <# "#team bob> blocked [blocked by admin] <muted>"

      threadDelay 1000000

      alice ##> "/unblock for all #team bob"
      alice <## "#team: you unblocked bob"
      cath <## "#team: alice unblocked bob"
      bob <// 50000

      threadDelay 1000000

      bob #> "#team 4"
      [alice, cath] *<# "#team bob> 4"

      alice
        #$> ( "/_get chat #1 count=6",
              chat,
              [ (0, "1"),
                (1, "blocked bob"),
                (0, "blocked [blocked by admin]"),
                (0, "blocked [blocked by admin]"),
                (1, "unblocked bob"),
                (0, "4")
              ]
            )
      cath
        #$> ( "/_get chat #1 count=6",
              chat,
              [ (0, "1"),
                (0, "blocked bob"),
                (0, "blocked [blocked by admin]"),
                (0, "blocked [blocked by admin]"),
                (0, "unblocked bob"),
                (0, "4")
              ]
            )
      bob #$> ("/_get chat #1 count=4", chat, [(1, "1"), (1, "2"), (1, "3"), (1, "4")])

testBlockForAllAnotherAdminUnblocks :: HasCallStack => TestParams -> IO ()
testBlockForAllAnotherAdminUnblocks =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"
      cath <## "#team: alice blocked bob"
      bob <// 50000

      bob #> "#team 2"
      alice <# "#team bob> 2 [blocked by admin] <muted>"
      cath <# "#team bob> 2 [blocked by admin] <muted>"

      cath ##> "/unblock for all #team bob"
      cath <## "#team: you unblocked bob"
      alice <## "#team: cath unblocked bob"
      bob <// 50000

      bob #> "#team 3"
      [alice, cath] *<# "#team bob> 3"

      bob #$> ("/_get chat #1 count=3", chat, [(1, "1"), (1, "2"), (1, "3")])

testBlockForAllBeforeJoining :: HasCallStack => TestParams -> IO ()
testBlockForAllBeforeJoining =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"
      cath <## "#team: alice blocked bob"
      bob <// 50000

      bob #> "#team 2"
      [alice, cath] *<# "#team bob> 2 [blocked by admin] <muted>"

      connectUsers alice dan
      addMember "team" alice dan GRAdmin
      dan ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: dan joined the group",
          do
            dan <## "#team: you joined the group"
            dan
              <### [ "#team: member bob (Bob) is connected",
                     "#team: member cath (Catherine) is connected"
                   ],
          aliceAddedDan bob,
          aliceAddedDan cath
        ]

      threadDelay 1000000

      bob #> "#team 3"
      [alice, cath, dan] *<# "#team bob> 3 [blocked by admin] <muted>"

      threadDelay 1000000

      bob #> "#team 4"
      [alice, cath, dan] *<# "#team bob> 4 [blocked by admin] <muted>"

      threadDelay 1000000

      alice ##> "/unblock for all #team bob"
      alice <## "#team: you unblocked bob"
      cath <## "#team: alice unblocked bob"
      dan <## "#team: alice unblocked bob"
      bob <// 50000

      threadDelay 1000000

      bob #> "#team 5"
      [alice, cath, dan] *<# "#team bob> 5"

      dan ##> "/_get chat #1 count=100"
      r <- chat <$> getTermLine dan
      r `shouldContain` [(0, "3 [blocked by admin]"), (0, "4 [blocked by admin]"), (0, "unblocked bob"), (0, "5")]
      r `shouldNotContain` [(0, "1")]
      r `shouldNotContain` [(0, "1 [blocked by admin]")]
      r `shouldNotContain` [(0, "2")]
      r `shouldNotContain` [(0, "2 [blocked by admin]")]
  where
    aliceAddedDan :: HasCallStack => TestCC -> IO ()
    aliceAddedDan cc = do
      cc <## "#team: alice added dan (Daniel) to the group (connecting...)"
      cc <## "#team: new member dan is connected"

testBlockForAllRepeat :: HasCallStack => TestParams -> IO ()
testBlockForAllRepeat =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- disableFullDeletion3 "team" alice bob cath

      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"
      cath <## "#team: alice blocked bob"
      bob <// 50000

      alice ##> "/block for all #team bob"
      alice <## "#team: you blocked bob"

      cath ##> "/block for all #team bob"
      cath <## "#team: you blocked bob"

      bob #> "#team 2"
      alice <# "#team bob> 2 [blocked by admin] <muted>"
      cath <# "#team bob> 2 [blocked by admin] <muted>"

      cath ##> "/unblock for all #team bob"
      cath <## "#team: you unblocked bob"
      alice <## "#team: cath unblocked bob"
      bob <// 50000

      alice ##> "/unblock for all #team bob"
      alice <## "#team: you unblocked bob"

      cath ##> "/unblock for all #team bob"
      cath <## "#team: you unblocked bob"

      bob #> "#team 3"
      [alice, cath] *<# "#team bob> 3"

      bob #$> ("/_get chat #1 count=3", chat, [(1, "1"), (1, "2"), (1, "3")])

testBlockForAllMultipleMembers :: HasCallStack => TestParams -> IO ()
testBlockForAllMultipleMembers =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "team" alice bob cath

      connectUsers alice dan
      addMember "team" alice dan GRMember
      dan ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: dan joined the group",
          do
            dan <## "#team: you joined the group"
            dan
              <### [ "#team: member bob (Bob) is connected",
                     "#team: member cath (Catherine) is connected"
                   ],
          do
            bob <## "#team: alice added dan (Daniel) to the group (connecting...)"
            bob <## "#team: new member dan is connected",
          do
            cath <## "#team: alice added dan (Daniel) to the group (connecting...)"
            cath <## "#team: new member dan is connected"
        ]

      -- lower roles to for batch block to be allowed (can't batch block if admins are selected)
      alice ##> "/mr team bob member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of bob to member",
          bob <## "#team: alice changed your role from admin to member",
          cath <## "#team: alice changed the role of bob from admin to member",
          dan <## "#team: alice changed the role of bob from admin to member"
        ]
      alice ##> "/mr team cath member"
      concurrentlyN_
        [ alice <## "#team: you changed the role of cath to member",
          bob <## "#team: alice changed the role of cath from admin to member",
          cath <## "#team: alice changed your role from admin to member",
          dan <## "#team: alice changed the role of cath from admin to member"
        ]

      bob #> "#team 1"
      [alice, cath, dan] *<# "#team bob> 1"

      cath #> "#team 2"
      [alice, bob, dan] *<# "#team cath> 2"

      alice ##> "/_block #1 2,3 blocked=on"
      alice <## "#team: you blocked 2 members"
      dan <## "#team: alice blocked bob"
      dan <## "#team: alice blocked cath"
      bob <// 50000
      cath <// 50000

      -- bob and cath don't know they are blocked and receive each other's messages
      bob #> "#team 3"
      [alice, dan] *<# "#team bob> 3 [blocked by admin] <muted>"
      cath <# "#team bob> 3"

      cath #> "#team 4"
      [alice, dan] *<# "#team cath> 4 [blocked by admin] <muted>"
      bob <# "#team cath> 4"

      alice ##> "/_block #1 2,3 blocked=off"
      alice <## "#team: you unblocked 2 members"
      dan <## "#team: alice unblocked bob"
      dan <## "#team: alice unblocked cath"
      bob <// 50000
      cath <// 50000

      bob #> "#team 5"
      [alice, cath, dan] *<# "#team bob> 5"

      cath #> "#team 6"
      [alice, bob, dan] *<# "#team cath> 6"

testBlockForAllLeftRemoved :: HasCallStack => TestParams -> IO ()
testBlockForAllLeftRemoved =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRMember)

      cath ##> "/leave #team"
      concurrentlyN_
        [ do
            cath <## "#team: you left the group"
            cath <## "use /d #team to delete the group",
          alice <## "#team: cath left the group",
          bob <## "#team: cath left the group",
          dan <## "#team: cath left the group"
        ]

      alice ##> "/rm team dan"
      concurrentlyN_
        [ alice <## "#team: you removed dan from the group",
          do
            dan <## "#team: alice removed you from the group"
            dan <## "use /d #team to delete the group",
          bob <## "#team: alice removed dan from the group"
        ]

      alice ##> "/block for all #team cath"
      alice <## "#team: you blocked cath"
      bob <## "#team: alice blocked cath"

      alice ##> "/block for all #team dan"
      alice <## "#team: you blocked dan"
      bob <## "#team: alice blocked dan"

testGroupMemberInactive :: HasCallStack => TestParams -> IO ()
testGroupMemberInactive ps = do
  withSmpServer' serverCfg' $ do
    withNewTestChatCfgOpts ps cfg' opts' "alice" aliceProfile $ \alice -> do
      withNewTestChatCfgOpts ps cfg' opts' "bob" bobProfile $ \bob -> do
        createGroup2 "team" alice bob

        alice #> "#team hi"
        bob <# "#team alice> hi"
        bob #> "#team hey"
        alice <# "#team bob> hey"

      -- bob is offline
      alice #> "#team 1"
      alice #> "#team 2"
      alice #> "#team 3"
      alice <## "[#team bob] connection is marked as inactive"
      -- 4 and 5 will be sent to bob as pending messages
      alice #> "#team 4"
      alice #> "#team 5"

      pgmCount <- withCCTransaction alice $ \db ->
        DB.query_ db "SELECT count(1) FROM pending_group_messages" :: IO [[Int]]
      pgmCount `shouldBe` [[2]]

      threadDelay 1500000

      withTestChatCfgOpts ps cfg' opts' "bob" $ \bob -> do
        bob <## "subscribed 2 connections on server localhost"
        bob <# "#team alice> 1"
        bob <# "#team alice> 2"
        bob <#. "#team alice> skipped message ID"
        alice <## "[#team bob] inactive connection is marked as active"

        bob <# "#team alice> 4"
        bob <# "#team alice> 5"

        pgmCount' <- withCCTransaction alice $ \db ->
          DB.query_ db "SELECT count(1) FROM pending_group_messages" :: IO [[Int]]
        pgmCount' `shouldBe` [[0]]

        -- delivery works
        alice #> "#team hi"
        bob <# "#team alice> hi"
        bob #> "#team hey"
        alice <# "#team bob> hey"
  where
    serverCfg' =
      smpServerCfg
        { transports = [("7003", transport @TLS, False)],
          msgQueueQuota = 2
        }
    fastRetryInterval = defaultReconnectInterval {initialInterval = 50_000} -- same as in agent tests
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

testGroupMemberReports :: HasCallStack => TestParams -> IO ()
testGroupMemberReports =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      createGroup3 "jokes" alice bob cath
      -- disableFullDeletion3 "jokes" alice bob cath
      alice ##> "/mr jokes bob moderator"
      concurrentlyN_
        [ alice <## "#jokes: you changed the role of bob to moderator",
          bob <## "#jokes: alice changed your role from admin to moderator",
          cath <## "#jokes: alice changed the role of bob from admin to moderator"
        ]
      alice ##> "/mr jokes cath member"
      concurrentlyN_
        [ alice <## "#jokes: you changed the role of cath to member",
          bob <## "#jokes: alice changed the role of cath from admin to member",
          cath <## "#jokes: alice changed your role from admin to member"
        ]
      alice ##> "/create link #jokes"
      gLink <- getGroupLink alice "jokes" GRMember True
      dan ##> ("/c " <> gLink)
      dan <## "connection request sent!"
      concurrentlyN_
        [ do
            alice <## "dan (Daniel): accepting request to join group #jokes..."
            alice <## "#jokes: dan joined the group",
          do
            dan <## "#jokes: joining the group..."
            dan <## "#jokes: you joined the group"
            dan <###
              [ "#jokes: member bob (Bob) is connected",
                "#jokes: member cath (Catherine) is connected"
              ],
          do
            bob <## "#jokes: alice added dan (Daniel) to the group (connecting...)"
            bob <## "#jokes: new member dan is connected",
          do
            cath <## "#jokes: alice added dan (Daniel) to the group (connecting...)"
            cath <## "#jokes: new member dan is connected"
        ]
      cath #> "#jokes inappropriate joke"
      concurrentlyN_
        [ alice <# "#jokes cath> inappropriate joke",
          bob <# "#jokes cath> inappropriate joke",
          dan <# "#jokes cath> inappropriate joke"
        ]
      dan ##> "/report #jokes content inappropriate joke"
      dan <# "#jokes (support) > cath inappropriate joke"
      dan <## "      report content"
      concurrentlyN_
        [ do
            alice <# "#jokes (support: dan) dan> > cath inappropriate joke"
            alice <## "      report content",
          do
            bob <# "#jokes (support: dan) dan> > cath inappropriate joke"
            bob <## "      report content",
          (cath </)
        ]
      alice #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content")])
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content")])
      dan #$> ("/_get chat #1 content=report count=100", chat, [(1, "report content")])
      alice ##> "\\\\ #jokes cath inappropriate joke"
      concurrentlyN_
        [ do
            alice <## "#jokes: 1 messages deleted by user"
            alice <## "message marked deleted by you",
          do
            bob <# "#jokes cath> [marked deleted by alice] inappropriate joke"
            bob <## "#jokes: 1 messages deleted by member alice",
          cath <# "#jokes cath> [marked deleted by alice] inappropriate joke",
          do
            dan <# "#jokes cath> [marked deleted by alice] inappropriate joke"
            dan <## "#jokes: 1 messages deleted by member alice"
        ]
      alice #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by you]")])
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by alice]")])
      dan #$> ("/_get chat #1 content=report count=100", chat, [(1, "report content [marked deleted by alice]")])
      -- delete all reports locally
      alice #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      bob #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      dan #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      cath #> "#jokes ok joke"
      concurrentlyN_
        [ alice <# "#jokes cath> ok joke",
          bob <# "#jokes cath> ok joke",
          dan <# "#jokes cath> ok joke"
        ]
      dan ##> "/report #jokes content ok joke"
      dan <# "#jokes (support) > cath ok joke"
      dan <## "      report content"
      dan ##> "/report #jokes spam ok joke"
      dan <# "#jokes (support) > cath ok joke"
      dan <## "      report spam"
      concurrentlyN_
        [ do
            alice <# "#jokes (support: dan) dan> > cath ok joke"
            alice <## "      report content"
            alice <# "#jokes (support: dan) dan> > cath ok joke"
            alice <## "      report spam",
          do
            bob <# "#jokes (support: dan) dan> > cath ok joke"
            bob <## "      report content"
            bob <# "#jokes (support: dan) dan> > cath ok joke"
            bob <## "      report spam",
          (cath </)
        ]
      alice #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content"), (0, "report spam")])
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content"), (0, "report spam")])
      cath #$> ("/_get chat #1 content=report count=100", chat, [])
      dan #$> ("/_get chat #1 content=report count=100", chat, [(1, "report content"), (1, "report spam")])
      alice ##> "/_archive reports #1"
      alice <## "#jokes: 2 messages deleted by user"
      (bob </)
      alice #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by you]"), (0, "report spam [marked deleted by you]")])
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content"), (0, "report spam")])
      bob ##> "/_archive reports #1"
      bob <## "#jokes: 2 messages deleted by user"
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by you]"), (0, "report spam [marked deleted by you]")])
      -- delete reports for all admins
      alice #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      bob #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      dan #$> ("/clear #jokes", id, "#jokes: all messages are removed locally ONLY")
      cath #> "#jokes ok joke 2"
      concurrentlyN_
        [ alice <# "#jokes cath> ok joke 2",
          bob <# "#jokes cath> ok joke 2",
          dan <# "#jokes cath> ok joke 2"
        ]
      dan ##> "/report #jokes content ok joke 2"
      dan <# "#jokes (support) > cath ok joke 2"
      dan <## "      report content"
      concurrentlyN_
        [ do
            alice <# "#jokes (support: dan) dan> > cath ok joke 2"
            alice <## "      report content",
          do
            bob <# "#jokes (support: dan) dan> > cath ok joke 2"
            bob <## "      report content",
          (cath </)
        ]
      alice ##> "/last_item_id"
      i :: ChatItemId <- read <$> getTermLine alice
      alice ##> ("/_delete reports #1 " <> show i <> " broadcast")
      alice <## "message marked deleted by you"
      bob <# "#jokes (support: dan) dan> [marked deleted by alice] report content"
      alice #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by you]")])
      bob #$> ("/_get chat #1 content=report count=100", chat, [(0, "report content [marked deleted by alice]")])
      dan #$> ("/_get chat #1 content=report count=100", chat, [(1, "report content")])

testMemberMention :: HasCallStack => TestParams -> IO ()
testMemberMention =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice #> "#team hello!"
      concurrentlyN_
        [ bob <# "#team alice> hello!",
          cath <# "#team alice> hello!"
        ]
      bob #> "#team hello @alice"
      concurrentlyN_
        [ alice <# "#team bob!> hello @alice",
          cath <# "#team bob> hello @alice"
        ]
      alice #> "#team hello @bob @bob @cath"
      concurrentlyN_
        [ bob <# "#team alice!> hello @bob @bob @cath",
          cath <# "#team alice!> hello @bob @bob @cath"
        ]
      cath #> "#team hello @Alice" -- not a mention
      concurrentlyN_
        [ alice <# "#team cath> hello @Alice",
          bob <# "#team cath> hello @Alice"
        ]
      cath ##> "! #team hello @alice" -- make it a mention
      cath <# "#team [edited] hello @alice"
      concurrentlyN_
        [ alice <# "#team cath> [edited] hello @alice",
          bob <# "#team cath> [edited] hello @alice"
        ]
      cath ##> "! #team hello @alice @bob" -- add a mention
      cath <# "#team [edited] hello @alice @bob"
      concurrentlyN_
        [ alice <# "#team cath> [edited] hello @alice @bob",
          bob <# "#team cath> [edited] hello @alice @bob"
        ]

testForwardQuoteMention :: HasCallStack => TestParams -> IO ()
testForwardQuoteMention =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      bob #> "#team hello @alice @cath"
      concurrentlyN_
        [ alice <# "#team bob!> hello @alice @cath",
          cath <# "#team bob!> hello @alice @cath"
        ]
      -- quote mentions
      alice `send` "> #team @bob (hello) hi there!"
      alice <# "#team > bob hello @alice @cath"
      alice <## "      hi there!"
      concurrently_
        ( do
            bob <# "#team alice!> > bob hello @alice @cath"
            bob <## "      hi there!"
        )
        ( do
            cath <# "#team alice> > bob hello @alice @cath"
            cath <## "      hi there!"
        )
      -- forward mentions to the same group
      alice `send` "#team <- #team hello"
      alice <# "#team <- #team"
      alice <## "      hello @alice @cath"
      concurrentlyN_
        [ do
            bob <# "#team alice> -> forwarded"
            bob <## "      hello @alice @cath",
          do
            cath <# "#team alice!> -> forwarded"
            cath <## "      hello @alice @cath"
        ]
      -- forward mentions
      alice `send` "@bob <- #team hello"
      alice <# "@bob <- #team"
      alice <## "      hello @alice @cath"
      bob <# "alice> -> forwarded"
      bob <## "      hello @alice @cath"
      -- member renamed to duplicate name
      cath ##> "/p alice_1"
      cath <## "user profile is changed to alice_1 (your 1 contacts are notified)"
      alice <## "contact cath changed to alice_1"
      alice <## "use @alice_1 <message> to send messages"
      -- mention changed in quoted mentions
      alice `send` "> #team @bob (hello) hi there!"
      alice <# "#team > bob hello @alice @alice_1"
      alice <## "      hi there!"
      concurrently_
        ( do
            bob <# "#team alice!> > bob hello @alice @alice_1"
            bob <## "      hi there!"
        )
        ( do
            cath <# "#team alice> > bob hello @alice @alice_1"
            cath <## "      hi there!"
        )
      -- mention changed in forwarded message
      alice `send` "@bob <- #team hello"
      alice <# "@bob <- #team"
      alice <## "      hello @alice @alice_1"
      bob <# "alice> -> forwarded"
      bob <## "      hello @alice @alice_1"

testGroupHistoryWithMentions :: HasCallStack => TestParams -> IO ()
testGroupHistoryWithMentions =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup2 "team" alice bob

      threadDelay 1000000

      alice #> "#team hello @bob"
      bob <# "#team alice!> hello @bob"

      bob ##> "/p robert"
      bob <## "user profile is changed to robert (your 1 contacts are notified)"
      alice <## "contact bob changed to robert"
      alice <## "use @robert <message> to send messages"

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True

      cath ##> ("/c " <> gLink)
      cath <## "connection request sent!"
      alice <## "cath (Catherine): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          cath
            <### [ "#team: joining the group...",
                   "#team: you joined the group",
                   WithTime "#team alice> hello @robert [>>]",
                   "#team: member robert is connected"
                 ],
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]

testUniqueMsgMentions :: SpecWith TestParams
testUniqueMsgMentions = do
  it "1 correct mention" $ \_ ->
    uniqueMsgMentions 2 (mm [("alice", "abcd")]) ["alice"]
      `shouldBe` (mm [("alice", "abcd")])
  it "2 correct mentions" $ \_ ->
    uniqueMsgMentions 2 (mm [("alice", "abcd"), ("bob", "efgh")]) ["alice", "bob"]
      `shouldBe` (mm [("alice", "abcd"), ("bob", "efgh")])
  it "2 correct mentions with repetition" $ \_ ->
    uniqueMsgMentions 2 (mm [("alice", "abcd"), ("bob", "efgh")]) ["alice", "alice", "alice", "bob", "bob", "bob"]
      `shouldBe` (mm [("alice", "abcd"), ("bob", "efgh")])
  it "too many mentions - drop extras" $ \_ ->
    uniqueMsgMentions 3 (mm [("a", "abcd"), ("b", "efgh"), ("c", "1234"), ("d", "5678")]) ["a", "a", "a", "b", "b", "c", "d"]
      `shouldBe` (mm [("a", "abcd"), ("b", "efgh"), ("c", "1234")])
  it "repeated-with-different name - drop extras" $ \_ ->
    uniqueMsgMentions 2 (mm [("alice", "abcd"), ("alice2", "abcd"), ("bob", "efgh"), ("bob2", "efgh")]) ["alice", "alice2", "bob", "bob2"]
      `shouldBe` (mm [("alice", "abcd"), ("bob", "efgh")])
  where
    mm = M.fromList . map (second $ MsgMention . MemberId)

testUpdatedMentionNames :: SpecWith TestParams
testUpdatedMentionNames = do
  it "keep mentions" $ \_ -> do
    test (mm [("alice", Just "alice"), ("bob", Nothing)]) "hello @alice @bob"
      `shouldBe` "hello @alice @bob"
    test (mm [("alice_1", Just "alice"), ("alice", Just "alice")]) "hello @alice @alice_1"
      `shouldBe` "hello @alice @alice_1"
  it "keep non-mentions" $ \_ -> do
    test (mm []) "hello @alice @bob"
      `shouldBe` "hello @alice @bob"
    test (mm [("alice", Just "alice")]) "hello @alice @bob"
      `shouldBe` "hello @alice @bob"
  it "replace changed names" $ \_ -> do
    test (mm [("alice", Just "Alice Jones"), ("bob", Just "robert")]) "hello @alice @bob"
      `shouldBe` "hello @'Alice Jones' @robert"
    test (mm [("alice", Just "alice"), ("cath", Just "alice")]) "hello @alice @cath"
      `shouldBe` "hello @alice @alice_1"
  where
    test mentionsMap t =
      let (mc', _, _) = updatedMentionNames (MCText t) (parseMaybeMarkdownList t) mentionsMap
       in msgContentText mc'
    mm = M.fromList . map (second mentionedMember)
    mentionedMember name_ = CIMention {memberId = MemberId "abcd", memberRef = ciMentionMember <$> name_}
      where
        ciMentionMember name = CIMentionMember {groupMemberId = 1, displayName = name, localAlias = Nothing, memberRole = GRMember}

testScopedSupportSingleModerator :: HasCallStack => TestParams -> IO ()
testScopedSupportSingleModerator =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    createGroup3' "team" alice (bob, GRMember) (cath, GRMember)

    alice #> "#team 1"
    [bob, cath] *<# "#team alice> 1"

    bob #> "#team 2"
    [alice, cath] *<# "#team bob> 2"

    alice ##> "/_send #1(_support:2) text 3"
    alice <# "#team (support: bob) 3"
    bob <# "#team (support) alice> 3"

    bob ##> "/_send #1(_support) text 4"
    bob <# "#team (support) 4"
    alice <# "#team (support: bob) bob> 4"

    cath ##> "/_send #1(_support:3) text 5"
    cath <## "#team: you have insufficient permissions for this action, the required role is moderator"

    alice ##> "/_delete member chat #1 2"
    alice <## "#team: bob support chat deleted"

testScopedSupportManyModerators :: HasCallStack => TestParams -> IO ()
testScopedSupportManyModerators =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)

    threadDelay 1000000

    alice #> "#team 1"
    [bob, cath, dan] *<# "#team alice> 1"

    threadDelay 1000000

    bob #> "#team 2"
    [alice, cath, dan] *<# "#team bob> 2"

    threadDelay 1000000

    alice ##> "/_send #1(_support:2) text 3"
    alice <# "#team (support: bob) 3"
    bob <# "#team (support) alice> 3"
    dan <# "#team (support: bob) alice> 3"

    threadDelay 1000000

    bob ##> "/_send #1(_support) text 4"
    bob <# "#team (support) 4"
    [alice, dan] *<# "#team (support: bob) bob> 4"

    threadDelay 1000000

    dan ##> "/_send #1(_support:3) text 5"
    dan <# "#team (support: bob) 5"
    alice <# "#team (support: bob) dan> 5"
    bob <# "#team (support) dan> 5"

    alice @@@ [("#team","2"), ("@dan","sent invitation to join group team as moderator"), ("@cath","sent invitation to join group team as member"), ("@bob","sent invitation to join group team as member")]
    bob @@@ [("#team","2"), ("@alice","received invitation to join group team as member")]
    dan @@@ [("#team","2"), ("@alice","received invitation to join group team as moderator")]
    cath @@@ [("#team","2"), ("@alice","received invitation to join group team as member")]

    alice #$> ("/_get chat #1 count=3", chat, [(0, "connected"), (1, "1"), (0, "2")])
    alice #$> ("/_get chat #1(_support:2) count=100", chat, [(1, "3"), (0, "4"), (0, "5")])
    bob #$> ("/_get chat #1 count=3", chat, [(0, "connected"), (0, "1"), (1, "2")])
    bob #$> ("/_get chat #1(_support) count=100", chat, [(0, "3"), (1, "4"), (0, "5")])
    dan #$> ("/_get chat #1 count=3", chat, [(0, "connected"), (0, "1"), (0, "2")])
    dan #$> ("/_get chat #1(_support:3) count=100", chat, [(0, "3"), (0, "4"), (1, "5")])
    cath #$> ("/_get chat #1 count=3", chat, [(0, "connected"), (0, "1"), (0, "2")])
    cath #$> ("/_get chat #1(_support:3) count=100", chat, [])

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 0"
    dan <## "bob (Bob) (id 3): unread: 0, require attention: 0, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 0, require attention: 0, mentions: 0"
    cath ##> "/member support chats #team"
    cath <## "bob (Bob) (id 3): unread: 0, require attention: 0, mentions: 0"

testScopedSupportForward :: HasCallStack => TestParams -> IO ()
testScopedSupportForward =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)
    setupGroupForwarding alice bob dan

    -- messages are forwarded in main scope
    bob #> "#team 1"
    [alice, cath] *<# "#team bob> 1"
    dan <# "#team bob> 1 [>>]"

    dan #> "#team 2"
    [alice, cath] *<# "#team dan> 2"
    bob <# "#team dan> 2 [>>]"

    -- messages are forwarded inside support scope
    bob #> "#team (support) 3"
    alice <# "#team (support: bob) bob> 3"
    dan <# "#team (support: bob) bob> 3 [>>]"

    dan #> "#team (support: bob) 4"
    alice <# "#team (support: bob) dan> 4"
    bob <# "#team (support) dan> 4 [>>]"

testScopedSupportForwardWhileReview :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardWhileReview =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRModerator) (dan, GRModerator)

      alice ##> "/set admission review #team all"
      alice <## "changed member admission rules"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed member admission rules",
          do
            cath <## "alice updated group #team:"
            cath <## "changed member admission rules",
          do
            dan <## "alice updated group #team:"
            dan <## "changed member admission rules"
        ]

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      eve ##> ("/c " <> gLink)
      eve <## "connection request sent!"
      alice <## "eve (Eve): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: eve connected and pending review",
          eve
            <### [ "#team: alice accepted you to the group, pending review",
                   "#team: joining the group...",
                   "#team: you joined the group, connecting to group moderators for admission to group",
                   "#team: member cath (Catherine) is connected",
                   "#team: member dan (Daniel) is connected"
                 ],
          do
            cath <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            cath <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member",
          do
            dan <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            dan <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
        ]

      setupGroupForwarding alice cath eve

      -- message from cath is not forwarded to eve in group scope
      bob #> "#team 1"
      [alice, cath, dan] *<# "#team bob> 1"

      -- message from cath is not forwarded to eve in group scope
      cath #> "#team 2"
      [alice, bob, dan] *<# "#team cath> 2"

      -- messages are forwarded in support scope
      eve #> "#team (support) 3"
      [alice, dan] *<# "#team (support: eve) eve> 3"
      cath <# "#team (support: eve) eve> 3 [>>]"

      cath #> "#team (support: eve) 4"
      [alice, dan] *<# "#team (support: eve) cath> 4"
      eve <# "#team (support) cath> 4 [>>]"

testScopedSupportDontForward :: HasCallStack => TestParams -> IO ()
testScopedSupportDontForward =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)
    setupGroupForwarding alice bob cath

    -- messages are forwarded in main scope
    bob #> "#team 1"
    [alice, dan] *<# "#team bob> 1"
    cath <# "#team bob> 1 [>>]"

    cath #> "#team 2"
    [alice, dan] *<# "#team cath> 2"
    bob <# "#team cath> 2 [>>]"

    -- messages are not forwarded from support to main scope
    bob #> "#team (support) 3"
    [alice, dan] *<# "#team (support: bob) bob> 3"

    cath #> "#team (support) 4"
    [alice, dan] *<# "#team (support: cath) cath> 4"

testScopedSupportForwardAll :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardAll =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GROwner)

      alice ##> "/set admission review #team all"
      alice <## "changed member admission rules"
      concurrentlyN_
        [ do
            bob <## "alice updated group #team:"
            bob <## "changed member admission rules",
          do
            cath <## "alice updated group #team:"
            cath <## "changed member admission rules",
          do
            dan <## "alice updated group #team:"
            dan <## "changed member admission rules"
        ]

      alice ##> "/create link #team"
      gLink <- getGroupLink alice "team" GRMember True
      eve ##> ("/c " <> gLink)
      eve <## "connection request sent!"
      alice <## "eve (Eve): accepting request to join group #team..."
      concurrentlyN_
        [ alice <## "#team: eve connected and pending review",
          eve
            <### [ "#team: alice accepted you to the group, pending review",
                   "#team: joining the group...",
                   "#team: you joined the group, connecting to group moderators for admission to group",
                   "#team: member dan (Daniel) is connected"
                 ],
          do
            dan <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
            dan <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
        ]

      setupGroupForwarding alice bob dan
      setupGroupForwarding alice dan eve

      -- messages are forwarded in main scope between bob and dan
      bob #> "#team 1"
      [alice, cath] *<# "#team bob> 1"
      dan <# "#team bob> 1 [>>]"

      dan #> "#team 2"
      [alice, cath] *<# "#team dan> 2"
      bob <# "#team dan> 2 [>>]"

      -- messages are forwarded in support scope between dan and eve
      eve #> "#team (support) 3"
      alice <# "#team (support: eve) eve> 3"
      dan <# "#team (support: eve) eve> 3 [>>]"

      dan #> "#team (support: eve) 4"
      alice <# "#team (support: eve) dan> 4"
      eve <# "#team (support) dan> 4 [>>]"

      -- x.grp.info is forwarded from dan to both bob and eve
      dan ##> "/gp team my_team"
      dan <## "changed to #my_team"
      concurrentlyN_
        [ do
            alice <## "dan updated group #team:"
            alice <## "changed to #my_team",
          do
            bob <## "dan updated group #team:"
            bob <## "changed to #my_team",
          do
            cath <## "dan updated group #team:"
            cath <## "changed to #my_team",
          do
            eve <## "dan updated group #team:"
            eve <## "changed to #my_team"
        ]

testScopedSupportDontForwardBetweenScopes :: HasCallStack => TestParams -> IO ()
testScopedSupportDontForwardBetweenScopes =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)
    setupGroupForwarding alice bob cath

    -- messages are forwarded in main scope
    bob #> "#team 1"
    [alice, dan] *<# "#team bob> 1"
    cath <# "#team bob> 1 [>>]"

    cath #> "#team 2"
    [alice, dan] *<# "#team cath> 2"
    bob <# "#team cath> 2 [>>]"

    -- messages not forwarded between support scopes
    bob #> "#team (support) 3"
    alice <# "#team (support: bob) bob> 3"
    dan <# "#team (support: bob) bob> 3"

    cath #> "#team (support) 4"
    alice <# "#team (support: cath) cath> 4"
    dan <# "#team (support: cath) cath> 4"

    bob #> "#team (support) 5"
    alice <# "#team (support: bob) bob> 5"
    dan <# "#team (support: bob) bob> 5"

testScopedSupportForwardFile :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardFile =
  testChat4 aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> withXFTPServer $ do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)
    setupGroupForwarding alice bob dan

    -- files are forwarded inside support scope
    bob ##> "/_send #1(_support) json [{\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}]"
    bob <# "#team (support) hi, sending a file"
    bob <# "/f #team (support) ./tests/fixtures/test.jpg"
    bob <## "use /fc 1 to cancel sending"

    concurrentlyN_
        [ do
            alice <# "#team (support: bob) bob> hi, sending a file"
            alice <# "#team (support: bob) bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
            alice <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            dan <# "#team (support: bob) bob> hi, sending a file [>>]"
            dan <# "#team (support: bob) bob> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
            dan <## "use /fr 1 [<dir>/ | <path>] to receive it [>>]"
        ]

    bob <## "completed uploading file 1 (test.jpg) for #team"

    dan ##> "/fr 1 ./tests/tmp"
    dan
      <### [ "saving file 1 from bob to ./tests/tmp/test.jpg",
              "started receiving file 1 (test.jpg) from bob"
            ]
    dan <## "completed receiving file 1 (test.jpg) from bob"

testScopedSupportForwardMemberRemoval :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardMemberRemoval =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRAdmin) (cath, GRMember) (dan, GRModerator)
      setupReviewForward alice bob cath dan eve

      -- bob removes eve, eve and dan receive member removal message
      bob ##> "/_remove #1 5"
      concurrentlyN_
        [ bob <## "#team: you removed eve from the group",
          alice <## "#team: bob removed eve from the group",
          dan <## "#team: bob removed eve from the group",
          do
            eve <## "#team: bob removed you from the group"
            eve <## "use /d #team to delete the group"
        ]

      alice ##> "#team (support: eve) hi"
      alice <## "bad chat command: support member not current or pending"
      bob ##> "#team (support: eve) hi"
      bob <##. "chat db error: SEGroupMemberNameNotFound"
      dan ##> "#team (support: eve) hi"
      dan <##. "chat db error: SEGroupMemberNameNotFound"
      eve ##> "/groups"
      eve <## "#team (you are removed, delete local copy: /d #team)"

setupReviewForward :: TestCC -> TestCC -> TestCC -> TestCC -> TestCC -> IO ()
setupReviewForward alice bob cath dan eve = do
  alice ##> "/set admission review #team all"
  alice <## "changed member admission rules"
  concurrentlyN_
    [ do
        bob <## "alice updated group #team:"
        bob <## "changed member admission rules",
      do
        cath <## "alice updated group #team:"
        cath <## "changed member admission rules",
      do
        dan <## "alice updated group #team:"
        dan <## "changed member admission rules"
    ]

  alice ##> "/create link #team"
  gLink <- getGroupLink alice "team" GRMember True
  eve ##> ("/c " <> gLink)
  eve <## "connection request sent!"
  alice <## "eve (Eve): accepting request to join group #team..."
  concurrentlyN_
    [ alice <## "#team: eve connected and pending review",
      eve
        <### [ "#team: alice accepted you to the group, pending review",
                "#team: joining the group...",
                "#team: you joined the group, connecting to group moderators for admission to group",
                "#team: member bob (Bob) is connected",
                "#team: member dan (Daniel) is connected"
              ],
      do
        bob <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
        bob <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member",
      do
        dan <## "#team: alice added eve (Eve) to the group (connecting and pending review...), use /_accept member #1 5 <role> to accept member"
        dan <## "#team: new member eve is connected and pending review, use /_accept member #1 5 <role> to accept member"
    ]

  setupGroupForwarding alice bob eve
  setupGroupForwarding alice bob dan

  -- alice forwards messages between bob and eve, bob and dan
  eve #> "#team (support) 3"
  [alice, dan] *<# "#team (support: eve) eve> 3"
  bob <# "#team (support: eve) eve> 3 [>>]"

  dan #> "#team (support: eve) 4"
  alice <# "#team (support: eve) dan> 4"
  bob <# "#team (support: eve) dan> 4 [>>]"
  eve <# "#team (support) dan> 4"

  bob #> "#team (support: eve) 5"
  alice <# "#team (support: eve) bob> 5"
  dan <# "#team (support: eve) bob> 5 [>>]"
  eve <# "#team (support) bob> 5 [>>]"

testScopedSupportForwardAdminRemoval :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardAdminRemoval =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GROwner) (cath, GRMember) (dan, GRModerator)
      setupReviewForward alice bob cath dan eve

      -- bob removes eve, eve and dan receive member removal message
      bob ##> "/rm team alice"
      concurrentlyN_
        [ bob <## "#team: you removed alice from the group",
          do
            alice <## "#team: bob removed you from the group"
            alice <## "use /d #team to delete the group",
          cath <## "#team: bob removed alice from the group",
          dan <## "#team: bob removed alice from the group",
          eve <## "#team: bob removed alice from the group"
        ]

      -- there is no forwarding admin anymore between bob and cath,
      -- so messages between bob and eve, bob and dan don't get delivered
      -- (this is not a desired behavior, just a test demonstration/proof of current implementation)
      eve #> "#team (support) hi"
      concurrentlyN_
        [ dan <# "#team (support: eve) eve> hi",
          (bob </),
          (alice </)
        ]

      dan #> "#team (support: eve) hey"
      concurrentlyN_
        [ eve <# "#team (support) dan> hey",
          (bob </),
          (alice </)
        ]

      bob #> "#team (support: eve) hello"
      concurrentlyN_
        [ (eve </),
          (dan </),
          (alice </)
        ]

      alice ##> "/groups"
      alice <## "#team (you are removed, delete local copy: /d #team)"

testScopedSupportForwardLeave :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardLeave =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GRAdmin) (cath, GRMember) (dan, GRModerator)
      setupReviewForward alice bob cath dan eve

      -- eve leaves group, bob and dan receive member leave message
      eve ##> "/leave #team"
      eve <## "#team: you left the group"
      eve <## "use /d #team to delete the group"
      alice <## "#team: eve left the group"
      bob <## "#team: eve left the group"
      dan <## "#team: eve left the group"

      alice ##> "#team (support: eve) hi"
      alice <## "bad chat command: support member not current or pending"
      bob ##> "#team (support: eve) hi"
      bob <##. "bad chat command: support member not current or pending"
      dan ##> "#team (support: eve) hi"
      dan <##. "bad chat command: support member not current or pending"
      eve ##> "/groups"
      eve <## "#team (you left, delete local copy: /d #team)"

testScopedSupportForwardGroupDeletion :: HasCallStack => TestParams -> IO ()
testScopedSupportForwardGroupDeletion =
  testChat5 aliceProfile bobProfile cathProfile danProfile eveProfile $
    \alice bob cath dan eve -> do
      createGroup4 "team" alice (bob, GROwner) (cath, GRMember) (dan, GRModerator)
      setupReviewForward alice bob cath dan eve

      -- if bob deletes the group, alice forwards it to eve and dan
      bob ##> "/d #team"
      concurrentlyN_
        [ bob <## "#team: you deleted the group",
          do
            alice <## "#team: bob deleted the group"
            alice <## "use /d #team to delete the local copy of the group",
          do
            cath <## "#team: bob deleted the group"
            cath <## "use /d #team to delete the local copy of the group",
          do
            dan <## "#team: bob deleted the group"
            dan <## "use /d #team to delete the local copy of the group",
          do
            eve <## "#team: bob deleted the group"
            eve <## "use /d #team to delete the local copy of the group"
        ]

      alice ##> "/groups"
      alice <## "#team (group deleted, delete local copy: /d #team)"
      bob ##> "/groups"
      bob <## "you have no groups!"
      bob <## "to create: /g <name>"
      cath ##> "/groups"
      cath <## "#team (group deleted, delete local copy: /d #team)"
      dan ##> "/groups"
      dan <## "#team (group deleted, delete local copy: /d #team)"
      eve ##> "/groups"
      eve <## "#team (group deleted, delete local copy: /d #team)"

testSupportCLISendCommand :: HasCallStack => TestParams -> IO ()
testSupportCLISendCommand =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    createGroup2' "team" alice (bob, GRObserver) True

    alice #> "#team 1"
    bob <# "#team alice> 1"

    bob ##> "#team 2"
    bob <## "#team: you don't have permission to send messages"
    (alice </)

    alice #> "#team (support: bob) 3"
    bob <# "#team (support) alice> 3"

    bob #> "#team (support) 4"
    alice <# "#team (support: bob) bob> 4"

    bob ##> "#team (support 4"
    bob <## "bad chat command: Failed reading: empty"

testScopedSupportUnreadStatsOnRead :: HasCallStack => TestParams -> IO ()
testScopedSupportUnreadStatsOnRead =
  testChatOpts4 opts aliceProfile bobProfile cathProfile danProfile $ \alice bob cath dan -> do
    createGroup4 "team" alice (bob, GRMember) (cath, GRMember) (dan, GRModerator)

    alice #> "#team 1"
    [bob, cath, dan] *<# "#team alice> 1"

    bob #> "#team 2"
    [alice, cath, dan] *<# "#team bob> 2"

    alice ##> "/_send #1(_support:2) text 3"
    alice <# "#team (support: bob) 3"
    bob <# "#team (support) alice> 3"
    dan <# "#team (support: bob) alice> 3"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 0"
    dan <## "bob (Bob) (id 3): unread: 1, require attention: 0, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 1, require attention: 0, mentions: 0"

    bob ##> "/_send #1(_support) text 4"
    bob <# "#team (support) 4"
    [alice, dan] *<# "#team (support: bob) bob> 4"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 1, require attention: 1, mentions: 0"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 2, require attention: 1, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 1, require attention: 0, mentions: 0"

    threadDelay 1000000

    dan ##> "/_send #1(_support:3) text 5"
    dan <# "#team (support: bob) 5"
    alice <# "#team (support: bob) dan> 5"
    bob <# "#team (support) dan> 5"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 2, require attention: 0, mentions: 0"
    -- In test "answering" doesn't reset unanswered, but in UI items would be marked read on opening chat
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 2, require attention: 1, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 2, require attention: 0, mentions: 0"

    threadDelay 1000000

    dan ##> "/_send #1(_support:3) json [{\"msgContent\": {\"type\": \"text\", \"text\": \"@alice 6\"}, \"mentions\": {\"alice\": 1}}]"
    dan <# "#team (support: bob) @alice 6"
    alice <# "#team (support: bob) dan!> @alice 6"
    bob <# "#team (support) dan> @alice 6"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 3, require attention: 0, mentions: 1"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 2, require attention: 1, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 3, require attention: 0, mentions: 0"

    aliceMentionedByDanItemId <- lastItemId alice

    threadDelay 1000000

    bob ##> "/_send #1(_support) json [{\"msgContent\": {\"type\": \"text\", \"text\": \"@alice 7\"}, \"mentions\": {\"alice\": 1}}]"
    bob <# "#team (support) @alice 7"
    alice <# "#team (support: bob) bob!> @alice 7"
    dan <# "#team (support: bob) bob> @alice 7"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 4, require attention: 1, mentions: 2"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 3, require attention: 2, mentions: 0"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 3, require attention: 0, mentions: 0"

    aliceMentionedByBobItemId <- lastItemId alice

    bob ##> "/_send #1(_support) json [{\"msgContent\": {\"type\": \"text\", \"text\": \"@dan 8\"}, \"mentions\": {\"dan\": 4}}]"
    bob <# "#team (support) @dan 8"
    alice <# "#team (support: bob) bob> @dan 8"
    dan <# "#team (support: bob) bob!> @dan 8"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 5, require attention: 2, mentions: 2"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 4, require attention: 3, mentions: 1"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 3, require attention: 0, mentions: 0"

    alice #$> ("/_read chat items #1(_support:2) " <> aliceMentionedByDanItemId, id, "items read for chat")

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 4, require attention: 2, mentions: 1"

    alice #$> ("/_read chat items #1(_support:2) " <> aliceMentionedByBobItemId, id, "items read for chat")

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 3, require attention: 1, mentions: 0"

    threadDelay 1000000

    dan ##> "/_send #1(_support:3) json [{\"msgContent\": {\"type\": \"text\", \"text\": \"@bob 9\"}, \"mentions\": {\"bob\": 3}}]"
    dan <# "#team (support: bob) @bob 9"
    alice <# "#team (support: bob) dan> @bob 9"
    bob <# "#team (support) dan!> @bob 9"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 4, require attention: 0, mentions: 0"
    dan ##> "/member support chats #team"
    dan <## "members require attention: 1"
    dan <## "bob (Bob) (id 3): unread: 4, require attention: 3, mentions: 1"
    bob ##> "/member support chats #team"
    bob <## "support: unread: 4, require attention: 0, mentions: 1"

    alice ##> "/_read chat #1(_support:2)"
    alice <## "#team: bob support chat read"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"

    dan ##> "/_read chat #1(_support:3)"
    dan <## "#team: bob support chat read"

    dan ##> "/member support chats #team"
    dan <## "members require attention: 0"
    dan <## "bob (Bob) (id 3): unread: 0, require attention: 0, mentions: 0"

    bob ##> "/_read chat #1(_support)"
    bob <## "#team: support chat read"

    bob ##> "/member support chats #team"
    bob <## "support: unread: 0, require attention: 0, mentions: 0"

    cath ##> "/member support chats #team"
    cath <// 50000
  where
    opts =
      testOpts
        { markRead = False
        }

testScopedSupportUnreadStatsOnDelete :: HasCallStack => TestParams -> IO ()
testScopedSupportUnreadStatsOnDelete =
  testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> do
    createGroup2 "team" alice bob

    alice ##> "/set delete #team on"
    alice <## "updated group preferences:"
    alice <## "Full deletion: on"
    bob <## "alice updated group #team:"
    bob <## "updated group preferences:"
    bob <## "Full deletion: on"

    bob #> "#team (support) 1"
    alice <# "#team (support: bob) bob> 1"

    msgIdBob <- lastItemId bob

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 1, require attention: 1, mentions: 0"

    bob #$> ("/_delete item #1(_support) " <> msgIdBob <> " broadcast", id, "message deleted")
    alice <# "#team (support: bob) bob> [deleted] 1"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"
  where
    opts =
      testOpts
        { markRead = False
        }

testScopedSupportUnreadStatsCorrectOnOpen :: HasCallStack => TestParams -> IO ()
testScopedSupportUnreadStatsCorrectOnOpen =
  testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> do
    createGroup2 "team" alice bob

    bob #> "#team (support) 1"
    alice <# "#team (support: bob) bob> 1"

    bob #> "#team (support) 2"
    alice <# "#team (support: bob) bob> 2"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 2, require attention: 2, mentions: 0"

    alice ##> "/_read chat #1(_support:2)"
    alice <## "#team: bob support chat read"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"

    bob #> "#team (support) 3"
    alice <# "#team (support: bob) bob> 3"

    bob #> "#team (support) 4"
    alice <# "#team (support: bob) bob> 4"

    bob #> "#team (support) 5"
    alice <# "#team (support: bob) bob> 5"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 3, require attention: 3, mentions: 0"

    -- opening chat should correct group_members.support_chat_items_member_attention value if it got out of sync
    void $ withCCTransaction alice $ \db ->
      DB.execute db "UPDATE group_members SET support_chat_items_member_attention=100 WHERE group_member_id=?" (Only (2 :: Int64))

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 3, require attention: 100, mentions: 0"

    alice #$> ("/_get chat #1(_support:2) count=100", chat, [(0, "1"), (0, "2"), (0, "3"), (0, "4"), (0, "5")])

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 3, require attention: 3, mentions: 0"

    alice ##> "/_read chat #1(_support:2)"
    alice <## "#team: bob support chat read"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"

    -- opening chat should also correct groups.members_require_attention value if corrected member no longer requires attention
    void $ withCCTransaction alice $ \db -> do
      DB.execute db "UPDATE group_members SET support_chat_items_member_attention=100 WHERE group_member_id=?" (Only (2 :: Int64))
      DB.execute db "UPDATE groups SET members_require_attention=1 WHERE group_id=?" (Only (1 :: Int64))

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 100, mentions: 0"

    alice #$> ("/_get chat #1(_support:2) count=100", chat, [(0, "1"), (0, "2"), (0, "3"), (0, "4"), (0, "5")])

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
    alice <## "bob (Bob) (id 2): unread: 0, require attention: 0, mentions: 0"
  where
    opts =
      testOpts
        { markRead = False
        }

testScopedSupportMemberRemoved :: HasCallStack => TestParams -> IO ()
testScopedSupportMemberRemoved =
  testChatOpts3 opts aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    createGroup3' "team" alice (bob, GRMember) (cath, GRAdmin)

    bob #> "#team (support) 1"
    [alice, cath] *<# "#team (support: bob) bob> 1"

    bob #> "#team (support) 2"
    [alice, cath] *<# "#team (support: bob) bob> 2"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 2, require attention: 2, mentions: 0"

    cath ##> "/rm team bob"
    concurrentlyN_
      [ cath <## "#team: you removed bob from the group",
        do
          bob <## "#team: cath removed you from the group"
          bob <## "use /d #team to delete the group",
        alice <## "#team: cath removed bob from the group"
      ]

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
  where
    opts =
      testOpts
        { markRead = False
        }

testScopedSupportUserRemovesMember :: HasCallStack => TestParams -> IO ()
testScopedSupportUserRemovesMember =
  testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> do
    createGroup2' "team" alice (bob, GRMember) True

    bob #> "#team (support) 1"
    alice <# "#team (support: bob) bob> 1"

    bob #> "#team (support) 2"
    alice <# "#team (support: bob) bob> 2"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 2, require attention: 2, mentions: 0"

    alice ##> "/rm team bob"
    concurrentlyN_
      [ alice <## "#team: you removed bob from the group",
        do
          bob <## "#team: alice removed you from the group"
          bob <## "use /d #team to delete the group"
      ]

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
  where
    opts =
      testOpts
        { markRead = False
        }

testScopedSupportMemberLeaves :: HasCallStack => TestParams -> IO ()
testScopedSupportMemberLeaves =
  testChatOpts2 opts aliceProfile bobProfile $ \alice bob -> do
    createGroup2' "team" alice (bob, GRMember) True

    bob #> "#team (support) 1"
    alice <# "#team (support: bob) bob> 1"

    bob #> "#team (support) 2"
    alice <# "#team (support: bob) bob> 2"

    alice ##> "/member support chats #team"
    alice <## "members require attention: 1"
    alice <## "bob (Bob) (id 2): unread: 2, require attention: 2, mentions: 0"

    bob ##> "/l team"
    concurrentlyN_
      [ do
          bob <## "#team: you left the group"
          bob <## "use /d #team to delete the group",
        alice <## "#team: bob left the group"
      ]

    alice ##> "/member support chats #team"
    alice <## "members require attention: 0"
  where
    opts =
      testOpts
        { markRead = False
        }

testSupportPreferenceGroup :: HasCallStack => TestParams -> IO ()
testSupportPreferenceGroup =
  testChat3 aliceProfile bobProfile cathProfile $ \alice bob cath -> do
    createGroup3' "team" alice (bob, GRMember) (cath, GRMember)

    threadDelay 1000000

    -- support enabled by default, bob sends to support
    bob #> "#team (support) hello"
    alice <# "#team (support: bob) bob> hello"

    -- alice replies
    alice #> "#team (support: bob) hi"
    bob <# "#team (support) alice> hi"

    -- alice disables support
    alice ##> "/set support #team off"
    alice <## "updated group preferences:"
    alice <## "Chat with admins: off"
    concurrentlyN_
      [ do
          bob <## "alice updated group #team:"
          bob <## "updated group preferences:"
          bob <## "Chat with admins: off",
        do
          cath <## "alice updated group #team:"
          cath <## "updated group preferences:"
          cath <## "Chat with admins: off"
      ]

    threadDelay 500000

    -- cath can't send support (no existing chat)
    cath ##> "#team (support) hey"
    cath <## "bad chat command: feature not allowed Chat with admins"

    -- alice can't send to cath's support (no existing chat)
    alice ##> "#team (support: cath) hey"
    alice <## "bad chat command: feature not allowed Chat with admins"

    -- bob can still send (existing chat)
    bob #> "#team (support) still here"
    alice <# "#team (support: bob) bob> still here"

    -- alice can still send to bob (existing chat)
    alice #> "#team (support: bob) yes"
    bob <# "#team (support) alice> yes"

testSupportPreferenceChannel :: HasCallStack => TestParams -> IO ()
testSupportPreferenceChannel ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "relay" chatRelayProfile $ \relay ->
      withNewTestChat ps "bob" bobProfile $ \bob ->
        withNewTestChat ps "cath" cathProfile $ \cath -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice relay
          memberJoinChannel "team" [relay] [alice] shortLink fullLink bob
          memberJoinChannel "team" [relay] [alice] shortLink fullLink cath

          threadDelay 1000000

          alice ##> "/set support #team on"
          alice <## "updated group preferences:"
          alice <## "Chat with admins: on"
          toggledSupport relay "alice" "team" "on"
          concurrentlyN_
            [ toggledSupport bob "alice" "team" "on",
              toggledSupport cath "alice" "team" "on"
            ]

          -- support enabled by default, bob sends to support
          bob #> "#team (support) hello"
          relay <# "#team (support: bob) bob> hello"
          alice <# "#team (support: bob) bob> hello [>>]"

          -- alice replies
          alice #> "#team (support: bob) hi"
          relay <# "#team (support: bob) alice> hi"
          bob <# "#team (support) alice> hi [>>]"

          -- alice disables support

          threadDelay 1000000

          alice ##> "/set support #team off"
          alice <## "updated group preferences:"
          alice <## "Chat with admins: off"
          toggledSupport relay "alice" "team" "off"
          concurrentlyN_
            [ toggledSupport bob "alice" "team" "off",
              toggledSupport cath "alice" "team" "off"
            ]

          threadDelay 500000

          -- cath can't send support (no existing chat)
          cath ##> "#team (support) hey"
          cath <## "bad chat command: feature not allowed Chat with admins"
          alice ##> "#team (support: cath) hey too"
          alice <## "bad chat command: feature not allowed Chat with admins"

          -- bob can still send (existing chat)
          bob #> "#team (support) still here"
          concurrentlyN_
            [ relay <# "#team (support: bob) bob> still here",
              alice <# "#team (support: bob) bob> still here [>>]"
            ]

          -- alice can still send to bob (existing chat)
          alice #> "#team (support: bob) yes"
          concurrentlyN_
            [ relay <# "#team (support: bob) alice> yes",
              bob <# "#team (support) alice> yes [>>]"
            ]

testConnectChannelCLI :: HasCallStack => TestParams -> IO ()
testConnectChannelCLI ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, _fullLink) <- prepareChannel2Relays "team" alice bob cath
          relayNames <- mapM userName [bob, cath]
          mName <- userName dan
          mFullName <- showName dan
          dan ##> ("/c " <> shortLink)
          dan <## "#team: connection started"
          concurrentlyN_ $
            [ dan
                <### concat
                  [ [ ConsoleString ("#team: joining the group (connecting to relay " <> rName <> ")..."),
                      ConsoleString ("#team: you joined the group (connected to relay " <> rName <> ")")
                    ]
                  | rName <- relayNames
                  ]
            ]
              <> [ do
                     relay <## (mFullName <> ": accepting request to join group #team...")
                     relay <## ("#team: " <> mName <> " joined the group")
                 | relay <- [bob, cath]
                 ]
              <> [alice <### [EndsWith ("introduced " <> mFullName <> " in the channel")]]

testConnectChannelCLIIncognito :: HasCallStack => TestParams -> IO ()
testConnectChannelCLIIncognito ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, _fullLink) <- prepareChannel2Relays "team" alice bob cath
          relayNames <- mapM userName [bob, cath]
          dan ##> ("/c i " <> shortLink)
          danIncognito <- getTermLine dan
          dan <## "#team: connection started incognito"
          concurrentlyN_ $
            [ dan
                <### concat
                  [ [ ConsoleString ("#team: joining the group (connecting to relay " <> rName <> ")..."),
                      ConsoleString ("#team: you joined the group (connected to relay " <> rName <> ") incognito as " <> danIncognito)
                    ]
                  | rName <- relayNames
                  ]
            ]
              <> [ do
                     relay <## (danIncognito <> ": accepting request to join group #team...")
                     relay <## ("#team: " <> danIncognito <> " joined the group")
                 | relay <- [bob, cath]
                 ]
              <> [alice <### [EndsWith ("introduced " <> danIncognito <> " in the channel")]]

testChannels1RelayDeliver :: HasCallStack => TestParams -> IO ()
testChannels1RelayDeliver ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            alice #> "#team hi"
            bob <# "#team> hi"
            [cath, dan, eve] *<# "#team> hi [>>]"

            cath ##> "+1 #team hi"
            cath <## "added 👍"
            bob <# "#team cath> > hi"
            bob <## "    + 👍"
            -- alice knows cath via XGrpMemNew announcement from relay
            alice <# "#team cath> > hi"
            alice <## "    + 👍"
            -- dan/eve learn cath via prepended XGrpMemNew before the forwarded reaction
            dan <## "#team: bob introduced cath (Catherine) in the channel"
            dan <# "#team cath> > hi"
            dan <## "    + 👍"
            eve <## "#team: bob introduced cath (Catherine) in the channel"
            eve <# "#team cath> > hi"
            eve <## "    + 👍"

            -- owner's public member count is maintained automatically
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 4"
            -- subscriber refreshes count via short link
            threadDelay 100000 -- wait for async short link data update
            cath ##> "/_get group link data #1"
            cath <## "group ID: 1"
            cath <## "subscribers: 4"

createChannel1Relay :: String -> TestCC -> TestCC -> TestCC -> TestCC -> TestCC -> IO ()
createChannel1Relay gName owner relay cath dan eve = do
  (shortLink, fullLink) <- prepareChannel1Relay gName owner relay
  forM_ [cath, dan, eve] $ \member ->
    memberJoinChannel gName [relay] [owner] shortLink fullLink member

-- Promote a fresh channel subscriber (observer default) to member so it can post; the roster bump
-- re-serves to the other (still-unknown) subscribers, who see the change rendered by member id hash.
promoteChannelMember :: HasCallStack => String -> TestCC -> TestCC -> TestCC -> [TestCC] -> IO ()
promoteChannelMember gName owner relay member others = do
  mName <- userName member
  oName <- userName owner
  owner ##> ("/mr #" <> gName <> " " <> mName <> " member")
  owner <## ("#" <> gName <> ": you changed the role of " <> mName <> " to member (signed)")
  concurrentlyN_ $
    [ relay <## ("#" <> gName <> ": " <> oName <> " changed the role of " <> mName <> " from observer to member (signed)"),
      member <## ("#" <> gName <> ": " <> oName <> " changed your role from observer to member (signed)")
    ]
      <> [o <### [EndsWith "from observer to member (signed)"] | o <- others]

setupRelay :: TestCC -> TestCC -> IO String
setupRelay owner relay = do
  rName <- userName relay
  relay ##> "/ad"
  (relaySLink, _cLink) <- getContactLinks relay True
  owner ##> ("/relays name=" <> rName <> " " <> relaySLink)
  owner <## "ok"
  pure relaySLink

prepareChannel1Relay :: String -> TestCC -> TestCC -> IO (String, String)
prepareChannel1Relay gName owner relay = do
  _ <- setupRelay owner relay
  prepareChannel gName owner relay

prepareChannel :: String -> TestCC -> TestCC -> IO (String, String)
prepareChannel = prepareChannel' 1

prepareChannel' :: Int -> String -> TestCC -> TestCC -> IO (String, String)
prepareChannel' relayId gName owner relay = do
  owner ##> ("/public group relays=1 #" <> gName)
  owner <## ("group #" <> gName <> " is created")
  owner <## "wait for selected relay(s) to join, then you can invite members via group link"

  concurrentlyN_
    [ do
        owner <## ("#" <> gName <> ": group link relays updated, current relays:")
        owner <## ("  - relay id " <> show relayId <> ": active")
        owner <## "group link:"
        _ <- getTermLine owner
        pure (),
      relay <## ("#" <> gName <> ": you joined the group as relay")
    ]

  owner ##> ("/show link #" <> gName)
  getGroupLinks owner gName GRObserver False

createChannel2Relays :: String -> TestCC -> TestCC -> TestCC -> TestCC -> TestCC -> TestCC -> IO ()
createChannel2Relays gName owner relay1 relay2 dan eve frank = do
  (shortLink, fullLink) <- prepareChannel2Relays gName owner relay1 relay2
  forM_ [dan, eve, frank] $ \member ->
    memberJoinChannel gName [relay1, relay2] [owner] shortLink fullLink member

prepareChannel2Relays :: String -> TestCC -> TestCC -> TestCC -> IO (String, String)
prepareChannel2Relays gName owner relay1 relay2 = do
  r1Name <- userName relay1
  r2Name <- userName relay2

  relay1 ##> "/ad"
  (r1SLink, _cLink) <- getContactLinks relay1 True
  relay2 ##> "/ad"
  (r2SLink, _cLink) <- getContactLinks relay2 True

  owner ##> ("/relays name=" <> r1Name <> " " <> r1SLink <> " name=" <> r2Name <> " " <> r2SLink)
  owner <## "ok"

  owner ##> ("/public group relays=1,2 #" <> gName)
  owner <## ("group #" <> gName <> " is created")
  owner <## "wait for selected relay(s) to join, then you can invite members via group link"

  concurrentlyN_
    [ do
        -- one relay connects
        owner <## ("#" <> gName <> ": group link relays updated, current relays:")
        owner
          <### [ EndsWith ": active",
                 Predicate (\l -> ": invited" `isSuffixOf` l || ": accepted" `isSuffixOf` l || ": acknowledged_roster" `isSuffixOf` l)
               ]
        owner <## "group link:"
        void $ getTermLine owner -- consume group link line
        -- second relay connects
        owner <## ("#" <> gName <> ": group link relays updated, current relays:")
        owner
          <### [ "  - relay id 1: active",
                 "  - relay id 2: active"
               ]
        owner <## "group link:"
        void $ getTermLine owner, -- consume group link line
      relay1 <## ("#" <> gName <> ": you joined the group as relay"),
      relay2 <## ("#" <> gName <> ": you joined the group as relay")
    ]

  owner ##> ("/show link #" <> gName)
  getGroupLinks owner gName GRObserver False

memberJoinChannel :: String -> [TestCC] -> [TestCC] -> String -> String -> TestCC -> IO ()
memberJoinChannel gName = memberJoinChannel' gName 1 0 0 0

-- | sfx params: relaySfx - how relay/owner see the member, memberRelaySfx - how member sees relay
memberJoinChannel' :: String -> Int -> Int -> Int -> Int -> [TestCC] -> [TestCC] -> String -> String -> TestCC -> IO ()
memberJoinChannel' gName gId relaySfx ownerSfx memberRelaySfx relays owners shortLink fullLink member = do
  mName <- userName member
  mFullName <- showName member
  let sfxMName s = if s == 0 then mName else mName <> "_" <> show s
      sfxName s = if s == 0 then mFullName else sfxMName s <> drop (length mName) mFullName
      sfxRelayName rn = if memberRelaySfx == 0 then rn else rn <> "_" <> show memberRelaySfx
  relayNames <- mapM (\r -> sfxRelayName <$> userName r) relays

  member ##> ("/_connect plan 1 " <> shortLink)
  member <## "group link: ok to connect via relays"
  groupSLinkData <- getTermLine member

  member ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " direct=off " <> groupSLinkData)
  member <## ("#" <> gName <> ": group is prepared")

  member ##> ("/_connect group #" <> show gId)
  member <## ("#" <> gName <> ": connection started")
  concurrentlyN_ $
    [ member
        <### concat
          [ [ ConsoleString ("#" <> gName <> ": joining the group (connecting to relay " <> rName <> ")..."),
              ConsoleString ("#" <> gName <> ": you joined the group (connected to relay " <> rName <> ")")
            ]
          | rName <- relayNames
          ]
    ]
      <> [ do
             relay <## (sfxName relaySfx <> ": accepting request to join group #" <> gName <> "...")
             relay <## ("#" <> gName <> ": " <> sfxMName relaySfx <> " joined the group")
         | relay <- relays
         ]
      <> [ owner <### [EndsWith ("introduced " <> sfxName ownerSfx <> " in the channel")]
         | owner <- owners
         ]

memberJoinChannelIncognito :: String -> [TestCC] -> [TestCC] -> String -> String -> TestCC -> IO String
memberJoinChannelIncognito gName relays owners shortLink fullLink member = do
  relayNames <- mapM userName relays

  member ##> ("/_connect plan 1 " <> shortLink)
  member <## "group link: ok to connect via relays"
  groupSLinkData <- getTermLine member

  member ##> ("/_prepare group 1 " <> fullLink <> " " <> shortLink <> " direct=off " <> groupSLinkData)
  member <## ("#" <> gName <> ": group is prepared")

  member ##> "/_connect group #1 incognito=on"
  memIncognito <- getTermLine member
  member <## ("#" <> gName <> ": connection started incognito")
  concurrentlyN_ $
    [ member
        <### concat
          [ [ ConsoleString ("#" <> gName <> ": joining the group (connecting to relay " <> rName <> ")..."),
              ConsoleString ("#" <> gName <> ": you joined the group (connected to relay " <> rName <> ") incognito as " <> memIncognito)
            ]
          | rName <- relayNames
          ]
    ]
      <> [ do
             relay <## (memIncognito <> ": accepting request to join group #" <> gName <> "...")
             relay <## ("#" <> gName <> ": " <> memIncognito <> " joined the group")
         | relay <- relays
         ]
      <> [ owner <### [EndsWith ("introduced " <> memIncognito <> " in the channel")]
         | owner <- owners
         ]
  pure memIncognito

-- | Assert that sender's member_relations_vector has 'MRIntroduced' at
-- the recipient's index, looked up by display name on the same DB.
memberIntroducedTo :: HasCallStack => TestCC -> T.Text -> T.Text -> IO ()
memberIntroducedTo cc senderName recipientName = do
  rows <- withCCTransaction cc $ \db ->
    DB.query
      db
      [sql|
        SELECT s.member_relations_vector, r.index_in_group
        FROM group_members s, group_members r
        WHERE s.local_display_name = ? AND r.local_display_name = ?
      |]
      (senderName, recipientName) ::
      IO [(Maybe ByteString, Int64)]
  case rows of
    [(mv, idx)] -> getRelation idx (fromMaybe B.empty mv) `shouldBe` MRIntroduced
    _ -> expectationFailure $ "memberIntroducedTo: expected exactly one row for " <> show (senderName, recipientName) <> ", got " <> show (length rows)

testChannels1RelayDeliverLoop :: HasCallStack => Int -> TestParams -> IO ()
testChannels1RelayDeliverLoop deliveryBucketSize ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatCfgOpts ps cfg relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            alice #> "#team hi"
            bob <# "#team> hi"
            [cath, dan, eve] *<# "#team> hi [>>]"

            cath ##> "+1 #team hi"
            cath <## "added 👍"
            bob <# "#team cath> > hi"
            bob <## "    + 👍"
            alice <# "#team cath> > hi"
            alice <## "    + 👍"
            dan <## "#team: bob introduced cath (Catherine) in the channel"
            dan <# "#team cath> > hi"
            dan <## "    + 👍"
            eve <## "#team: bob introduced cath (Catherine) in the channel"
            eve <# "#team cath> > hi"
            eve <## "    + 👍"
  where
    cfg = testCfg {deliveryBucketSize}

testChannelsSenderDeduplicateOwn :: HasCallStack => TestParams -> IO ()
testChannelsSenderDeduplicateOwn ps = do
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChat ps "cath" cathProfile $ \cath ->
      withNewTestChat ps "dan" danProfile $ \dan ->
        withNewTestChat ps "eve" eveProfile $ \eve -> do
          withNewTestChatCfgOpts ps cfg relayTestOpts "bob" bobProfile $ \bob -> do
            createChannel1Relay "team" alice bob cath dan eve
            -- promote cath and dan while the relay is online, so their buffered posts replay as members
            promoteChannelMember "team" alice bob cath [dan, eve]
            promoteChannelMember "team" alice bob dan [cath, eve]

          -- chat relay bob is offline
          alice #> "#team 1"
          alice #> "#team 2"
          alice #> "#team 3"
          cath #> "#team 4"
          cath #> "#team 5"
          dan #> "#team 6"

          withTestChatCfgOpts ps cfg relayTestOpts "bob" $ \bob -> do
            bob <## "subscribed 6 connections on server localhost"
            bob
              <### [ WithTime "#team> 1",
                     WithTime "#team> 2",
                     WithTime "#team> 3",
                     WithTime "#team cath> 4",
                     WithTime "#team cath> 5",
                     WithTime "#team dan> 6"
                   ]
            alice
              <### [ WithTime "#team cath> 4 [>>]",
                     WithTime "#team cath> 5 [>>]",
                     WithTime "#team dan> 6 [>>]"
                   ]
            cath
              <### [ EndsWith "updated to dan",
                     "#team: bob introduced dan (Daniel) in the channel",
                     WithTime "#team> 1 [>>]",
                     WithTime "#team> 2 [>>]",
                     WithTime "#team> 3 [>>]",
                     WithTime "#team dan> 6 [>>]"
                   ]
            dan
              <### [ EndsWith "updated to cath",
                     "#team: bob introduced cath (Catherine) in the channel",
                     WithTime "#team> 1 [>>]",
                     WithTime "#team> 2 [>>]",
                     WithTime "#team> 3 [>>]",
                     WithTime "#team cath> 4 [>>]",
                     WithTime "#team cath> 5 [>>]"
                   ]
            eve
              <### [ EndsWith "updated to cath",
                     EndsWith "updated to dan",
                     "#team: bob introduced cath (Catherine) in the channel",
                     "#team: bob introduced dan (Daniel) in the channel",
                     WithTime "#team> 1 [>>]",
                     WithTime "#team> 2 [>>]",
                     WithTime "#team> 3 [>>]",
                     WithTime "#team cath> 4 [>>]",
                     WithTime "#team cath> 5 [>>]",
                     WithTime "#team dan> 6 [>>]"
                   ]
  where
    cfg = testCfg {deliveryWorkerDelay = 250000}

testChannelLateJoinerReceivesProfile :: HasCallStack => TestParams -> IO ()
testChannelLateJoinerReceivesProfile ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink dan
          promoteChannelMember "team" alice bob cath [dan]

          -- first forward: dan resolves cath (roster-known by id hash) on the prepended XGrpMemNew.
          cath #> "#team hi"
          bob <# "#team cath> hi"
          alice <# "#team cath> hi [>>]"
          dan <### [EndsWith "updated to cath"]
          dan <## "#team: bob introduced cath (Catherine) in the channel"
          dan <# "#team cath> hi [>>]"

          -- second forward: dan's bit is set, no prepend, no view event.
          cath #> "#team hi again"
          bob <# "#team cath> hi again"
          alice <# "#team cath> hi again [>>]"
          dan <# "#team cath> hi again [>>]"

          memberIntroducedTo bob "cath" "alice"
          memberIntroducedTo bob "cath" "dan"

          -- profile update: rename piggybacks on next send; no re-prepend, bits stay set.
          cath ##> "/p kate Kate"
          cath <## "user profile is changed to kate (Kate) (your 0 contacts are notified)"

          cath #> "#team renamed"
          bob <# "#team kate> renamed"
          alice <# "#team kate> renamed [>>]"
          dan <# "#team kate> renamed [>>]"
          threadDelay 500000
          memberIntroducedTo bob "kate" "alice"
          memberIntroducedTo bob "kate" "dan"

testChannel2RelaysDeduplicateProfile :: HasCallStack => TestParams -> IO ()
testChannel2RelaysDeduplicateProfile ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
            memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink dan
            memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink eve

            -- promote dan (observer default) so it can post; eve learns dan via the roster (id hash)
            alice ##> "/mr #team dan member"
            alice <## "#team: you changed the role of dan to member (signed)"
            concurrentlyN_
              [ bob <## "#team: alice changed the role of dan from observer to member (signed)",
                cath <## "#team: alice changed the role of dan from observer to member (signed)",
                dan <## "#team: alice changed your role from observer to member (signed)",
                eve <### [EndsWith "from observer to member (signed)"]
              ]

            -- first forward: both relays prepend XGrpMemNew(dan) for eve;
            -- second hits xGrpMemNew's "already created via another relay" branch.
            dan #> "#team hi"
            bob <# "#team dan> hi"
            cath <# "#team dan> hi"
            alice <# "#team dan> hi [>>]"
            eve <### [EndsWith "updated to dan"]
            eve .<## " introduced dan (Daniel) in the channel"
            eve <# "#team dan> hi [>>]"

            -- second forward: eve's bit is set on both relays, no prepend.
            dan #> "#team hi again"
            bob <# "#team dan> hi again"
            cath <# "#team dan> hi again"
            alice <# "#team dan> hi again [>>]"
            eve <# "#team dan> hi again [>>]"

            -- both relays independently mark eve in dan's vector;
            -- alice's bit was set at join via introduceInChannel and stays set.
            memberIntroducedTo bob "dan" "alice"
            memberIntroducedTo bob "dan" "eve"
            memberIntroducedTo cath "dan" "alice"
            memberIntroducedTo cath "dan" "eve"

            -- profile update: rename piggybacks on next send; no re-prepend, bits stay set.
            dan ##> "/p dean Dean"
            dan <## "user profile is changed to dean (Dean) (your 0 contacts are notified)"

            dan #> "#team renamed"
            bob <# "#team dean> renamed"
            cath <# "#team dean> renamed"
            alice <# "#team dean> renamed [>>]"
            eve <# "#team dean> renamed [>>]"
            threadDelay 500000
            memberIntroducedTo bob "dean" "alice"
            memberIntroducedTo bob "dean" "eve"
            memberIntroducedTo cath "dean" "alice"
            memberIntroducedTo cath "dean" "eve"

testChannelLargeProfileFits :: HasCallStack => TestParams -> IO ()
testChannelLargeProfileFits ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink dan
          promoteChannelMember "team" alice bob cath [dan]

          -- ~14000 chars: profile fits in a singleton batch AND packs
          -- inline with the forwarded body (exercises the in-body path).
          let bigImage = T.pack ("data:image/png;base64," <> replicate 14000 'A')
          withCCTransaction bob $ \db ->
            DB.execute db "UPDATE contact_profiles SET image = ? WHERE display_name = ?" (bigImage, "cath" :: T.Text)

          cath #> "#team hi"
          bob <# "#team cath> hi"
          alice <# "#team cath> hi [>>]"
          dan <### [EndsWith "updated to cath"]
          dan <## "#team: bob introduced cath (Catherine) in the channel"
          dan <# "#team cath> hi [>>]"

          memberIntroducedTo bob "cath" "dan"

testChannelMultipleLargeProfiles :: HasCallStack => TestParams -> IO ()
testChannelMultipleLargeProfiles ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatCfgOpts ps cfg relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]
            promoteChannelMember "team" alice bob dan [cath, eve]

            -- ~14500 chars each: one rides inline with the body,
            -- the other spills into a standalone overflow batch.
            let cathImage = T.pack ("data:image/png;base64," <> replicate 14500 'A')
                danImage = T.pack ("data:image/png;base64," <> replicate 14500 'B')
            withCCTransaction bob $ \db -> do
              DB.execute db "UPDATE contact_profiles SET image = ? WHERE display_name = ?" (cathImage, "cath" :: T.Text)
              DB.execute db "UPDATE contact_profiles SET image = ? WHERE display_name = ?" (danImage, "dan" :: T.Text)

            -- deliveryWorkerDelay=250ms lets the relay coalesce cath's and
            -- dan's sends into one multi-sender job.
            cath #> "#team from cath"
            bob <# "#team cath> from cath"
            dan #> "#team from dan"
            bob <# "#team dan> from dan"

            alice
              <### [ WithTime "#team cath> from cath [>>]",
                     WithTime "#team dan> from dan [>>]"
                   ]
            cath
              <### [ EndsWith "updated to dan",
                     "#team: bob introduced dan (Daniel) in the channel",
                     WithTime "#team dan> from dan [>>]"
                   ]
            dan
              <### [ EndsWith "updated to cath",
                     "#team: bob introduced cath (Catherine) in the channel",
                     WithTime "#team cath> from cath [>>]"
                   ]
            eve
              <### [ EndsWith "updated to cath",
                     EndsWith "updated to dan",
                     "#team: bob introduced dan (Daniel) in the channel",
                     "#team: bob introduced cath (Catherine) in the channel",
                     WithTime "#team cath> from cath [>>]",
                     WithTime "#team dan> from dan [>>]"
                   ]

            memberIntroducedTo bob "cath" "eve"
            memberIntroducedTo bob "dan" "eve"
  where
    cfg = testCfg {deliveryWorkerDelay = 250000}

-- Asserted via SQL on the relay's DB rather than terminal output: the
-- "updated profile" chat item rendering on relays/owners is order-sensitive.
testChannelProfileUpdateNoRePrepend :: HasCallStack => TestParams -> IO ()
testChannelProfileUpdateNoRePrepend ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink dan
          promoteChannelMember "team" alice bob cath [dan]

          cath #> "#team hi"
          bob <# "#team cath> hi"
          alice <# "#team cath> hi [>>]"
          dan <### [EndsWith "updated to cath"]
          dan <## "#team: bob introduced cath (Catherine) in the channel"
          dan <# "#team cath> hi [>>]"

          memberIntroducedTo bob "cath" "dan"

          -- /p only delivers XInfo to direct contacts; for group members it
          -- piggybacks on the next group send via shouldSendProfileUpdate.
          cath ##> "/p kate Kate"
          cath <## "user profile is changed to kate (Kate) (your 0 contacts are notified)"

          cath #> "#team hi again"
          bob <# "#team kate> hi again"
          alice <# "#team kate> hi again [>>]"
          dan <# "#team kate> hi again [>>]"
          threadDelay 500000
          memberIntroducedTo bob "kate" "dan"

testChannelMultiSendersIndependent :: HasCallStack => TestParams -> IO ()
testChannelMultiSendersIndependent ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]
            promoteChannelMember "team" alice bob dan [cath, eve]

            -- cath posts: dan and eve resolve cath on the prepended XGrpMemNew
            cath #> "#team from cath"
            bob <# "#team cath> from cath"
            alice <# "#team cath> from cath [>>]"
            dan <### [EndsWith "updated to cath"]
            dan <## "#team: bob introduced cath (Catherine) in the channel"
            dan <# "#team cath> from cath [>>]"
            eve <### [EndsWith "updated to cath"]
            eve <## "#team: bob introduced cath (Catherine) in the channel"
            eve <# "#team cath> from cath [>>]"

            -- dan posts: cath and eve resolve dan independently of cath's vector
            dan #> "#team from dan"
            bob <# "#team dan> from dan"
            alice <# "#team dan> from dan [>>]"
            cath <### [EndsWith "updated to dan"]
            cath <## "#team: bob introduced dan (Daniel) in the channel"
            cath <# "#team dan> from dan [>>]"
            eve <### [EndsWith "updated to dan"]
            eve <## "#team: bob introduced dan (Daniel) in the channel"
            eve <# "#team dan> from dan [>>]"

            -- second post from cath: all recipients have cath marked, no prepend
            cath #> "#team again from cath"
            bob <# "#team cath> again from cath"
            alice <# "#team cath> again from cath [>>]"
            dan <# "#team cath> again from cath [>>]"
            eve <# "#team cath> again from cath [>>]"

testChannels2RelaysDeliver :: HasCallStack => TestParams -> IO ()
testChannels2RelaysDeliver ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              createChannel2Relays "team" alice bob cath dan eve frank

              -- promote dan (observer default) so it can send; eve/frank learn dan via the roster
              alice ##> "/mr #team dan member"
              alice <## "#team: you changed the role of dan to member (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of dan from observer to member (signed)",
                  cath <## "#team: alice changed the role of dan from observer to member (signed)",
                  dan <## "#team: alice changed your role from observer to member (signed)",
                  eve <### [EndsWith "from observer to member (signed)"],
                  frank <### [EndsWith "from observer to member (signed)"]
                ]

              alice #> "#team hi"
              [bob, cath] *<# "#team> hi"
              [dan, eve, frank] *<# "#team> hi [>>]"

              dan ##> "+1 #team hi"
              dan <## "added 👍"
              bob <# "#team dan> > hi"
              bob <## "    + 👍"
              cath <# "#team dan> > hi"
              cath <## "    + 👍"
              alice <# "#team dan> > hi"
              alice <## "    + 👍"
              eve .<##. ("#team: unknown member ", " updated to dan")
              eve .<## " introduced dan (Daniel) in the channel"
              eve <# "#team dan> > hi"
              eve <## "    + 👍"
              frank .<##. ("#team: unknown member ", " updated to dan")
              frank .<## " introduced dan (Daniel) in the channel"
              frank <# "#team dan> > hi"
              frank <## "    + 👍"

testChannels2RelaysIncognito :: HasCallStack => TestParams -> IO ()
testChannels2RelaysIncognito ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
              danIncognito <- memberJoinChannelIncognito "team" [bob, cath] [alice] shortLink fullLink dan
              forM_ [eve, frank] $ \member ->
                memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink member

              -- promote dan (observer default) so it can send; eve/frank learn dan via the roster
              alice ##> ("/mr #team " <> danIncognito <> " member")
              alice <## ("#team: you changed the role of " <> danIncognito <> " to member (signed)")
              concurrentlyN_
                [ bob <## ("#team: alice changed the role of " <> danIncognito <> " from observer to member (signed)"),
                  cath <## ("#team: alice changed the role of " <> danIncognito <> " from observer to member (signed)"),
                  dan <## "#team: alice changed your role from observer to member (signed)",
                  eve <### [EndsWith "from observer to member (signed)"],
                  frank <### [EndsWith "from observer to member (signed)"]
                ]

              alice #> "#team hi"
              [bob, cath] *<# "#team> hi"
              dan ?<# "#team> hi [>>]"
              [eve, frank] *<# "#team> hi [>>]"

              dan ##> "+1 #team hi"
              dan <## "added 👍"
              bob <# ("#team " <> danIncognito <> "> > hi")
              bob <## "    + 👍"
              cath <# ("#team " <> danIncognito <> "> > hi")
              cath <## "    + 👍"
              alice <# ("#team " <> danIncognito <> "> > hi")
              alice <## "    + 👍"
              eve .<##. ("#team: unknown member ", (" updated to " <> danIncognito))
              eve .<## (" introduced " <> danIncognito <> " in the channel")
              eve <# ("#team " <> danIncognito <> "> > hi")
              eve <## "    + 👍"
              frank .<##. ("#team: unknown member ", (" updated to " <> danIncognito))
              frank .<## (" introduced " <> danIncognito <> " in the channel")
              frank <# ("#team " <> danIncognito <> "> > hi")
              frank <## "    + 👍"

              alice `hasContactProfiles` ["alice", "bob", "cath", T.pack danIncognito, "eve", "frank"]
              bob `hasContactProfiles` ["alice", "bob", T.pack danIncognito, "eve", "frank"]
              cath `hasContactProfiles` ["alice", "cath", T.pack danIncognito, "eve", "frank"]
              dan `hasContactProfiles` ["alice", "bob", "cath", "dan", T.pack danIncognito]
              eve `hasContactProfiles` ["alice", "bob", "cath", T.pack danIncognito, "eve"]
              frank `hasContactProfiles` ["alice", "bob", "cath", T.pack danIncognito, "frank"]

testChannelUpdateProfileSigned :: HasCallStack => TestParams -> IO ()
testChannelUpdateProfileSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            alice ##> "/set welcome #team welcome to team"
            alice <## "welcome message changed to:"
            alice <## "welcome to team"
            concurrentlyN_
              [ do
                  bob <## "alice updated group #team: (signed)"
                  bob <## "welcome message changed to:"
                  bob <## "welcome to team",
                do
                  cath <## "alice updated group #team: (signed)"
                  cath <## "welcome message changed to:"
                  cath <## "welcome to team",
                do
                  dan <## "alice updated group #team: (signed)"
                  dan <## "welcome message changed to:"
                  dan <## "welcome to team",
                do
                  eve <## "alice updated group #team: (signed)"
                  eve <## "welcome message changed to:"
                  eve <## "welcome to team"
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "group profile updated (signed)")])

testChannelLinkAfterProfileUpdate :: HasCallStack => TestParams -> IO ()
testChannelLinkAfterProfileUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath

          -- owner updates channel profile
          alice ##> "/gp team my_team My team description"
          alice <## "changed to #my_team (My team description)"
          concurrentlyN_
            [ do
                bob <## "alice updated group #team: (signed)"
                bob <## "changed to #my_team (My team description)",
              do
                cath <## "alice updated group #team: (signed)"
                cath <## "changed to #my_team (My team description)"
            ]
          alice #$> ("/_get chat #1 count=1", chat, [(1, "group profile updated (signed)")])

          -- late subscriber joins via the same channel link after profile update
          threadDelay 100000
          alice ##> "/show link #my_team"
          (shortLink', fullLink') <- getGroupLinks alice "my_team" GRObserver False
          shortLink' `shouldBe` shortLink
          fullLink' `shouldBe` fullLink
          memberJoinChannel "my_team" [bob] [alice] shortLink' fullLink' dan

          alice #> "#my_team hi"
          bob <# "#my_team> hi"
          [cath, dan] *<# "#my_team> hi [>>]"

testChannelLinkAfterWelcomeUpdate :: HasCallStack => TestParams -> IO ()
testChannelLinkAfterWelcomeUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath

          -- owner updates channel welcome message
          alice ##> "/set welcome #team welcome to team"
          alice <## "welcome message changed to:"
          alice <## "welcome to team"
          concurrentlyN_
            [ do
                bob <## "alice updated group #team: (signed)"
                bob <## "welcome message changed to:"
                bob <## "welcome to team",
              do
                cath <## "alice updated group #team: (signed)"
                cath <## "welcome message changed to:"
                cath <## "welcome to team"
            ]
          alice #$> ("/_get chat #1 count=1", chat, [(1, "group profile updated (signed)")])

          -- re-fetch updated link, late subscriber joins
          threadDelay 100000
          alice ##> "/show link #team"
          (shortLink', fullLink') <- getGroupLinks alice "team" GRObserver False
          shortLink' `shouldBe` shortLink
          fullLink' `shouldBe` fullLink
          memberJoinChannel "team" [bob] [alice] shortLink' fullLink' dan
          dan #$> ("/_get chat #1 count=100", chat, channelFeaturesNoE2E <> [(0, "welcome to team"), (0, T.unpack publicGroupNoE2EText), (0, "connected")])

          alice #> "#team hi"
          bob <# "#team> hi"
          [cath, dan] *<# "#team> hi [>>]"

testChannelOwnerKeyAfterLinkUpdate :: HasCallStack => TestParams -> IO ()
testChannelOwnerKeyAfterLinkUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath

          threadDelay 100000

          -- Owner updates channel profile - triggers rebuilding link data.
          alice ##> "/gp team my_team My team description"
          alice <## "changed to #my_team (My team description)"
          concurrentlyN_
            [ do
                bob <## "alice updated group #team: (signed)"
                bob <## "changed to #my_team (My team description)",
              do
                cath <## "alice updated group #team: (signed)"
                cath <## "changed to #my_team (My team description)"
            ]

          threadDelay 100000

          -- Late subscriber joins via the same channel link after profile update.
          alice ##> "/show link #my_team"
          (shortLink', fullLink') <- getGroupLinks alice "my_team" GRObserver False
          shortLink' `shouldBe` shortLink
          fullLink' `shouldBe` fullLink
          memberJoinChannel "my_team" [bob] [alice] shortLink' fullLink' dan

          threadDelay 100000

          -- Verify owner member record in late subscriber's DB has a public key.
          ownerKeyPresent <- withCCTransaction dan $ \db ->
            DB.query_ db "SELECT COUNT(1) FROM group_members WHERE member_role = 'owner' AND member_pub_key IS NOT NULL" :: IO [[Int]]
          ownerKeyPresent `shouldBe` [[1]]

          -- Verify signed event is received by late subscriber.
          alice ##> "/gp my_team team"
          alice <## "changed to #team"
          concurrentlyN_
            [ do
                bob <## "alice updated group #my_team: (signed)"
                bob <## "changed to #team",
              do
                cath <## "alice updated group #my_team: (signed)"
                cath <## "changed to #team",
              do
                dan <## "alice updated group #my_team: (signed)"
                dan <## "changed to #team"
            ]

testChannelUpdatePrefsSigned :: HasCallStack => TestParams -> IO ()
testChannelUpdatePrefsSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            alice ##> "/set delete #team on"
            alice <## "updated group preferences:"
            alice <## "Full deletion: on"
            concurrentlyN_
              [ do
                  bob <## "alice updated group #team: (signed)"
                  bob <## "updated group preferences:"
                  bob <## "Full deletion: on",
                do
                  cath <## "alice updated group #team: (signed)"
                  cath <## "updated group preferences:"
                  cath <## "Full deletion: on",
                do
                  dan <## "alice updated group #team: (signed)"
                  dan <## "updated group preferences:"
                  dan <## "Full deletion: on",
                do
                  eve <## "alice updated group #team: (signed)"
                  eve <## "updated group preferences:"
                  eve <## "Full deletion: on"
              ]

testChannelChangeRoleSigned :: HasCallStack => TestParams -> IO ()
testChannelChangeRoleSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            threadDelay 1000000

            -- other members discover cath
            cath #> "#team hello from cath"
            bob <# "#team cath> hello from cath"
            concurrentlyN_
              [ alice <# "#team cath> hello from cath [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> hello from cath [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> hello from cath [>>]"
              ]

            -- change member role (XGrpMemRole) - signed (other members can verify)
            threadDelay 1000000
            alice ##> "/mr #team cath admin"
            alice <## "#team: you changed the role of cath to admin (signed)"
            bob <## "#team: alice changed the role of cath from member to admin (signed)"
            concurrentlyN_
              [ cath <## "#team: alice changed your role from member to admin (signed)",
                dan <## "#team: alice changed the role of cath from member to admin (signed)",
                eve <## "#team: alice changed the role of cath from member to admin (signed)"
              ]
            -- chat item is not created for other members
            alice #$> ("/_get chat #1 count=1", chat, [(1, "changed role of cath to admin (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")])
            cath #$> ("/_get chat #1 count=1", chat, [(0, "changed your role to admin (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")])

            -- change role of silent member
            threadDelay 1000000
            alice ##> "/mr #team dan admin"
            alice <## "#team: you changed the role of dan to admin (signed)"
            concurrentlyN_
              [ bob <## "#team: alice changed the role of dan from observer to admin (signed)",
                dan <## "#team: alice changed your role from observer to admin (signed)",
                cath .<##. ("#team: alice changed the role of ", " from observer to admin (signed)"),
                eve .<##. ("#team: alice changed the role of ", " from observer to admin (signed)")
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "changed role of dan to admin (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(0, "changed your role to admin (signed)")])

testChannelBlockMemberSigned :: HasCallStack => TestParams -> IO ()
testChannelBlockMemberSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- other members discover cath
            threadDelay 1000000
            cath #> "#team hello from cath"
            bob <# "#team cath> hello from cath"
            concurrentlyN_
              [ alice <# "#team cath> hello from cath [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> hello from cath [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> hello from cath [>>]"
              ]

            -- block member (XGrpMemRestrict) - signed (other members can verify)
            threadDelay 1000000
            alice ##> "/block for all #team cath"
            alice <## "#team: you blocked cath (signed)"
            bob <## "#team: alice blocked cath (signed)"
            concurrentlyN_
              [ dan <## "#team: alice blocked cath (signed)",
                eve <## "#team: alice blocked cath (signed)"
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "blocked cath (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "blocked cath (signed)")])
            cath #$> ("/_get chat #1 count=1", chat, [(1, "hello from cath")]) -- was blocked - no "blocked" chat item
            dan #$> ("/_get chat #1 count=1", chat, [(0, "blocked cath (signed)")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "blocked cath (signed)")])

            -- TODO [relays] member: in channels - don't create unknown member record and chat item? (just ignore)
            -- block silent member (other members create unknown member record and can verify)
            threadDelay 1000000
            alice ##> "/block for all #team dan"
            alice <## "#team: you blocked dan (signed)"
            bob <## "#team: alice blocked dan (signed)"
            concurrentlyN_
              [ do
                  cath <##. "#team: alice blocked an unknown member, creating unknown member record"
                  cath .<##. ("#team: alice blocked", "(signed)"),
                do
                  eve <##. "#team: alice blocked an unknown member, creating unknown member record"
                  eve .<##. ("#team: alice blocked", "(signed)")
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "blocked dan (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "blocked dan (signed)")])
            cath ##> "/_get chat #1 count=1"
            [(0, r1)] <- chat <$> getTermLine cath
            r1 `shouldStartWith` "blocked"
            r1 `shouldEndWith` "(signed)"
            dan #$> ("/_get chat #1 count=1", chat, [(0, "blocked cath (signed)")]) -- was blocked - no new chat item
            eve ##> "/_get chat #1 count=1"
            [(0, r2)] <- chat <$> getTermLine eve
            r2 `shouldStartWith` "blocked"
            r2 `shouldEndWith` "(signed)"

checkMemberRow :: HasCallStack => TestCC -> T.Text -> Maybe T.Text -> IO ()
checkMemberRow cc name expectedRole = do
  roles <- withCCTransaction cc $ \db ->
    DB.query db "SELECT member_role FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only T.Text]
  map (\(Only r) -> r) roles `shouldBe` maybeToList expectedRole

-- The wire member id for a named member (look it up on a client that knows the name, e.g. the owner), used to
-- find a member by id on a subscriber that only knows it by member-id hash (e.g. after roster recovery).
getMemberIdByName :: TestCC -> T.Text -> IO ByteString
getMemberIdByName cc name = do
  rows <- withCCTransaction cc $ \db ->
    DB.query db "SELECT member_id FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only ByteString]
  case rows of
    [Only mid] -> pure mid
    _ -> fail $ "expected one group_members row for " <> T.unpack name

getMemberRoleKey :: TestCC -> ByteString -> IO (T.Text, Maybe ByteString)
getMemberRoleKey cc mid = do
  rows <- withCCTransaction cc $ \db ->
    DB.query db "SELECT member_role, member_pub_key FROM group_members WHERE member_id = ?" (Only mid) :: IO [(T.Text, Maybe ByteString)]
  case rows of
    [r] -> pure r
    _ -> fail "expected one group_members row by member id"

testChannelModeratorActionViaRoster :: HasCallStack => TestParams -> IO ()
testChannelModeratorActionViaRoster ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
              forM_ [cath, dan, eve] $ \member ->
                memberJoinChannel "team" [bob] [alice] shortLink fullLink member

              -- promote dan (observer default) so it can post; cath and eve then discover dan
              threadDelay 1000000
              promoteChannelMember "team" alice bob dan [cath, eve]
              dan #> "#team hello from dan"
              bob <# "#team dan> hello from dan"
              concurrentlyN_
                [ alice <# "#team dan> hello from dan [>>]",
                  do
                    cath <### [EndsWith "updated to dan"]
                    cath <## "#team: bob introduced dan (Daniel) in the channel"
                    cath <# "#team dan> hello from dan [>>]",
                  do
                    eve <### [EndsWith "updated to dan"]
                    eve <## "#team: bob introduced dan (Daniel) in the channel"
                    eve <# "#team dan> hello from dan [>>]"
                ]

              -- cath promoted observer -> moderator; dan/eve learn cath via the roster re-serve
              -- (no name yet -> rendered by member id hash)
              threadDelay 1000000
              alice ##> "/mr #team cath moderator"
              alice <## "#team: you changed the role of cath to moderator (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
                  cath <## "#team: alice changed your role from observer to moderator (signed)",
                  dan <### [EndsWith "to moderator (signed)"],
                  eve <### [EndsWith "to moderator (signed)"]
                ]

              -- cath (moderator) blocks dan; profile prepend carries cath's full profile to dan/eve
              threadDelay 1000000
              cath ##> "/block for all #team dan"
              cath <## "#team: you blocked dan (signed)"
              bob <## "#team: cath blocked dan (signed)"
              alice <## "#team: cath blocked dan (signed)"
              eve <### [EndsWith "updated to cath"]
              eve <## "#team: bob introduced cath (Catherine) in the channel"
              eve <## "#team: cath blocked dan (signed)"
              dan <### [EndsWith "updated to cath"]
              dan <## "#team: bob introduced cath (Catherine) in the channel"

              -- frank joins after the roster update; cached roster gives him cath as moderator.
              -- both alice (owner) and cath (mod) receive XGrpMemNew(frank) via introduceInChannel.
              -- the roster apply also emits the role-change chat item on frank's side (owner
              -- profile may not be loaded yet, so the actor renders by memberId hash)
              threadDelay 1000000
              memberJoinChannel "team" [bob] [alice, cath] shortLink fullLink frank
              -- the late joiner learns the roster from the served snapshot (verified below); under the
              -- no-broadcast model the apply finds no role change to surface, so no item here
              threadDelay 1000000 -- the served roster arrives async
              checkMemberRole frank "cath" "moderator"
  where
    checkMemberRole :: HasCallStack => TestCC -> T.Text -> T.Text -> IO ()
    checkMemberRole cc name expectedRole = do
      roles <- withCCTransaction cc $ \db ->
        DB.query db "SELECT member_role FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only T.Text]
      map (\(Only r) -> r) roles `shouldBe` [expectedRole]

testChannelSubscriberRosterCatchUp :: HasCallStack => TestParams -> IO ()
testChannelSubscriberRosterCatchUp ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
              forM_ [cath, dan, eve, frank] $ \member ->
                memberJoinChannel "team" [bob] [alice] shortLink fullLink member
              -- promote dan (roster v0) then eve (v1) into the owner-signed roster; cath learns both with their keys
              threadDelay 1000000
              promoteChannelMember "team" alice bob dan [cath, eve, frank]
              threadDelay 1000000
              promoteChannelMember "team" alice bob eve [cath, dan, frank]
              threadDelay 1000000
              -- simulate cath having fallen behind and lost dan: capture dan's member id (from the owner, which
              -- knows the name) and cath's owner-pinned key for dan, then delete dan's record and rewind cath's
              -- applied frontier so the next delta arrives as a gap (v2 > applied 0 + 1)
              danId <- getMemberIdByName alice "dan"
              (_, danKey) <- getMemberRoleKey cath danId
              withCCTransaction cath $ \db -> do
                DB.execute db "DELETE FROM group_members WHERE member_id = ?" (Only danId)
                DB.execute db "UPDATE groups SET applied_complete_roster_version = ? WHERE group_id = ?" (0 :: Int64, 1 :: Int64)
              -- the next privileged change (frank -> v2) reaches cath at a jumped version, triggering catch-up:
              -- cath requests the roster from the forwarding relay, which re-serves the current snapshot
              promoteChannelMember "team" alice bob frank [cath, dan, eve]
              threadDelay 2000000 -- wait for the gap request + relay re-serve to recover dan
              -- cath recovered dan from the re-served roster: same member id, role, and owner-pinned key
              (recRole, recKey) <- getMemberRoleKey cath danId
              recRole `shouldBe` "member"
              recKey `shouldBe` danKey

-- Same recovery, but the subscriber (frank) is connected to two relays: the request goes to whichever relay
-- forwarded the gapping delta, and only an observation that catch-up works in a 2-relay channel (not the race).
testChannel2RelaysSubscriberRosterCatchUp :: HasCallStack => TestParams -> IO ()
testChannel2RelaysSubscriberRosterCatchUp ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
              forM_ [dan, eve, frank] $ \member ->
                memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink member
              -- promote dan (v0) then eve (v1) into the owner-signed roster, forwarded by both relays; frank
              -- (the subscriber) learns both with their keys
              threadDelay 1000000
              alice ##> "/mr #team dan member"
              alice <## "#team: you changed the role of dan to member (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of dan from observer to member (signed)",
                  cath <## "#team: alice changed the role of dan from observer to member (signed)",
                  dan <## "#team: alice changed your role from observer to member (signed)",
                  eve <### [EndsWith "from observer to member (signed)"],
                  frank <### [EndsWith "from observer to member (signed)"]
                ]
              threadDelay 1000000
              alice ##> "/mr #team eve member"
              alice <## "#team: you changed the role of eve to member (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of eve from observer to member (signed)",
                  cath <## "#team: alice changed the role of eve from observer to member (signed)",
                  eve <## "#team: alice changed your role from observer to member (signed)",
                  dan <### [EndsWith "from observer to member (signed)"],
                  frank <### [EndsWith "from observer to member (signed)"]
                ]
              threadDelay 1000000
              -- simulate frank having fallen behind and lost dan: delete dan's record and rewind frank's complete
              -- frontier so the next delta (eve -> v2) arrives as a gap (2 > applied 0 + 1)
              danId <- getMemberIdByName alice "dan"
              (_, danKey) <- getMemberRoleKey frank danId
              withCCTransaction frank $ \db -> do
                DB.execute db "DELETE FROM group_members WHERE member_id = ?" (Only danId)
                DB.execute db "UPDATE groups SET applied_complete_roster_version = ? WHERE group_id = ?" (0 :: Int64, 1 :: Int64)
              -- eve -> moderator (v2) reaches frank at a jumped version; it requests the roster from the relay that
              -- forwarded the delta, which re-serves the current snapshot (including dan), recovering dan
              alice ##> "/mr #team eve moderator"
              alice <## "#team: you changed the role of eve to moderator (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of eve from member to moderator (signed)",
                  cath <## "#team: alice changed the role of eve from member to moderator (signed)",
                  eve <## "#team: alice changed your role from member to moderator (signed)",
                  dan <### [EndsWith "from member to moderator (signed)"],
                  frank <### [EndsWith "from member to moderator (signed)"]
                ]
              threadDelay 2000000 -- wait for the gap request + relay re-serve to recover dan
              (recRole, recKey) <- getMemberRoleKey frank danId
              recRole `shouldBe` "member"
              recKey `shouldBe` danKey

testChannelRemovedModeratorRefreshesRoster :: HasCallStack => TestParams -> IO ()
testChannelRemovedModeratorRefreshesRoster ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
              forM_ [cath, dan, eve] $ \member ->
                memberJoinChannel "team" [bob] [alice] shortLink fullLink member
              -- cath promoted observer -> moderator; dan/eve learn cath via the roster (id hash)
              threadDelay 1000000
              alice ##> "/mr #team cath moderator"
              alice <## "#team: you changed the role of cath to moderator (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
                  cath <## "#team: alice changed your role from observer to moderator (signed)",
                  dan <### [EndsWith "to moderator (signed)"],
                  eve <### [EndsWith "to moderator (signed)"]
                ]
              threadDelay 1000000
              alice ##> "/rm #team cath"
              alice <## "#team: you removed cath from the group (signed)"
              -- the relay applies the removal via the roster (revert to observer) before the delete delta
              bob <## "#team: alice changed the role of cath from moderator to observer (signed)"
              bob <## "#team: alice removed cath from the group (signed)"
              cath <## "#team: alice removed you from the group (signed)"
              cath <## "use /d #team to delete the group"
              dan <### [EndsWith "from the group (signed)"]
              eve <### [EndsWith "from the group (signed)"]

              -- frank joins after the removal; cached roster has dropped cath
              threadDelay 1000000
              memberJoinChannel "team" [bob] [alice] shortLink fullLink frank
              threadDelay 100000
              checkMemberRow frank "cath" Nothing

testChannelRoleTransitionsUpdateRoster :: HasCallStack => TestParams -> IO ()
testChannelRoleTransitionsUpdateRoster ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
              memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
              -- observer -> moderator
              threadDelay 100000
              alice ##> "/mr #team cath moderator"
              alice <## "#team: you changed the role of cath to moderator (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
                  cath <## "#team: alice changed your role from observer to moderator (signed)"
                ]
              -- dan joins; cached roster has cath as moderator (learned from the served snapshot,
              -- no separate role-change item under the no-broadcast model)
              threadDelay 100000
              memberJoinChannel "team" [bob] [alice, cath] shortLink fullLink dan
              threadDelay 1000000 -- the served roster arrives async; wait before reading the applied state
              checkMemberRow dan "cath" (Just "moderator")
              -- moderator -> admin: dan now knows cath, role event lands cleanly
              threadDelay 100000
              alice ##> "/mr #team cath admin"
              alice <## "#team: you changed the role of cath to admin (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of cath from moderator to admin (signed)",
                  cath <## "#team: alice changed your role from moderator to admin (signed)",
                  dan <## "#team: alice changed the role of cath from moderator to admin (signed)"
                ]
              -- eve joins; cached roster has cath as admin (learned from the served snapshot)
              threadDelay 100000
              memberJoinChannel "team" [bob] [alice, cath] shortLink fullLink eve
              threadDelay 1000000 -- the served roster arrives async; wait before reading the applied state
              checkMemberRow eve "cath" (Just "admin")
              -- admin -> observer (crossing out of roster, since member is now in-roster): roster drops cath
              threadDelay 100000
              alice ##> "/mr #team cath observer"
              alice <## "#team: you changed the role of cath to observer (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of cath from admin to observer (signed)",
                  cath <## "#team: alice changed your role from admin to observer (signed)",
                  dan <## "#team: alice changed the role of cath from admin to observer (signed)",
                  eve <## "#team: alice changed the role of cath from admin to observer (signed)"
                ]
              -- frank joins; cath isn't in the roster, so frank has no record of her
              threadDelay 100000
              memberJoinChannel "team" [bob] [alice] shortLink fullLink frank
              threadDelay 100000
              checkMemberRow frank "cath" Nothing

testChannelRelayCannotDowngradeRosterMember :: HasCallStack => TestParams -> IO ()
testChannelRelayCannotDowngradeRosterMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChatOpts ps (testOpts {coreOptions = testCoreOpts {logLevel = CLLWarning}}) "frank" frankProfile $ \frank -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink frank
          -- promote cath; roster TOFU-creates cath on frank as moderator with the real key
          threadDelay 1000000
          alice ##> "/mr #team cath moderator"
          alice <## "#team: you changed the role of cath to moderator (signed)"
          concurrentlyN_
            [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
              cath <## "#team: alice changed your role from observer to moderator (signed)",
              frank <### [EndsWith "to moderator (signed)"]
            ]
          threadDelay 100000
          realKey <- getMemberPubKey bob "cath"
          -- malicious relay: corrupt bob's local record of cath so its XGrpMemNew dissemination
          -- carries a downgraded role + no key
          withCCTransaction bob $ \db ->
            DB.execute
              db
              "UPDATE group_members SET member_role = ?, member_pub_key = NULL WHERE local_display_name = ?"
              ("member" :: T.Text, "cath" :: T.Text)
          -- cath posts; bob prepends XGrpMemNew(cath, member, NULL) to the delivery (frank not yet introduced)
          threadDelay 100000
          cath #> "#team hello from cath"
          bob <# "#team cath> hello from cath"
          concurrentlyN_
            [ alice <# "#team cath> hello from cath [>>]",
              do
                frank <##. "warning: x.grp.mem.new: relay asserted key differs from roster-established key, keeping roster key, memberId="
                frank <### [EndsWith "updated to cath"]
                frank <## "#team: bob introduced cath (Catherine) in the channel"
                frank <# "#team cath> hello from cath [>>]"
            ]
          threadDelay 100000
          checkMemberRow frank "cath" (Just "moderator")
          frankKey <- getMemberPubKey frank "cath"
          frankKey `shouldBe` realKey
  where
    getMemberPubKey :: TestCC -> T.Text -> IO (Maybe ByteString)
    getMemberPubKey cc name = do
      rows <- withCCTransaction cc $ \db ->
        DB.query db "SELECT member_pub_key FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only (Maybe ByteString)]
      case rows of
        [Only k] -> pure k
        _ -> fail $ "expected one row for " <> T.unpack name

testChannelRelayCannotForgePrivilegedMember :: HasCallStack => TestParams -> IO ()
testChannelRelayCannotForgePrivilegedMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
        memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
        threadDelay 1000000
        -- the forged attribution only resolves to a privileged author if the victim already holds the
        -- owner at GROwner (established via the group link on join) - this documents and guards that premise
        checkMemberRow cath "alice" (Just "owner")
        ownerMemId <- ownerMemberId bob
        connId <- relayConnIdToMember bob "cath"
        -- the malicious relay forges the announcement, choosing the new member's role and signing key
        g <- C.newRandom
        kp <- atomically $ C.generateKeyPair g
        ts <- getCurrentTime
        let ChatController {smpAgent = bobAgent} = chatController bob
            attackerPub = fst kp :: C.PublicKeyEd25519
            forgedMemId = MemberId "forgedadmin1"
            forgedProfile = (aliceProfile :: Profile) {displayName = "forgery", fullName = "Forgery"}
            memInfo =
              MemberInfo
                { memberId = forgedMemId,
                  memberRole = GRAdmin,
                  v = Nothing,
                  profile = forgedProfile,
                  memberKey = Just (MemberKey attackerPub)
                }
            chatMsg = ChatMessage chatInitialVRange Nothing (XGrpMemNew memInfo Nothing)
            fwd = GrpMsgForward (FwdMember ownerMemId "alice") ts
            body = encodeBinaryBatch [encodeFwdElement fwd (VMUnsigned chatMsg)]
        sent <- runExceptT $ sendMessages bobAgent [(connId, PQEncOff, MsgFlags False, vrValue body)]
        either (fail . show) (const $ pure ()) sent
        -- secure: the victim rejects the forged privileged announcement instead of storing it
        cath <##. "error: x.grp.mem.new: privileged member not established by roster"
        forged <- forgedMemberRows cath "forgery"
        forged `shouldBe` []
  where
    ownerMemberId :: TestCC -> IO MemberId
    ownerMemberId cc = do
      rows <- withCCTransaction cc $ \db ->
        DB.query db "SELECT member_id FROM group_members WHERE member_role = ? LIMIT 1" (Only ("owner" :: T.Text)) :: IO [Only ByteString]
      case rows of
        [Only mid] -> pure (MemberId mid)
        _ -> fail "expected exactly one owner member on the relay"
    forgedMemberRows :: TestCC -> T.Text -> IO [(T.Text, Maybe ByteString)]
    forgedMemberRows cc name =
      withCCTransaction cc $ \db ->
        DB.query db "SELECT member_role, member_pub_key FROM group_members WHERE local_display_name = ?" (Only name)

testChannelRemoveMemberSigned :: HasCallStack => TestParams -> IO ()
testChannelRemoveMemberSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote eve to member (observer default) so it can post
            promoteChannelMember "team" alice bob eve [cath, dan]

            -- other members discover eve
            eve #> "#team hello from eve"
            bob <# "#team eve> hello from eve"
            concurrentlyN_
              [ alice <# "#team eve> hello from eve [>>]",
                do
                  dan <### [EndsWith "updated to eve"]
                  dan <## "#team: bob introduced eve (Eve) in the channel"
                  dan <# "#team eve> hello from eve [>>]",
                do
                  cath <### [EndsWith "updated to eve"]
                  cath <## "#team: bob introduced eve (Eve) in the channel"
                  cath <# "#team eve> hello from eve [>>]"
              ]

            -- before removal — owner count is maintained synchronously
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 4"
            threadDelay 100000 -- wait for async short link data update
            cath ##> "/_get group link data #1"
            cath <## "group ID: 1"
            cath <## "subscribers: 4"

            -- remove member (XGrpMemDel) - signed (other members can verify)
            threadDelay 1000000
            alice ##> "/rm #team eve"
            alice <## "#team: you removed eve from the group (signed)"
            -- the relay applies the removal via the roster (revert to observer) before the delete delta
            bob <## "#team: alice changed the role of eve from member to observer (signed)"
            bob <## "#team: alice removed eve from the group (signed)"
            concurrentlyN_
              [ cath <## "#team: alice removed eve from the group (signed)",
                dan <## "#team: alice removed eve from the group (signed)",
                do
                  eve <## "#team: alice removed you from the group (signed)"
                  eve <## "use /d #team to delete the group"
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "removed eve (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "removed eve (signed)")])
            cath #$> ("/_get chat #1 count=1", chat, [(0, "removed eve (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(0, "removed eve (signed)")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "removed you (signed)")])

            -- eve had items (posted "hello from eve") -> kept as permanent GSMemRemoved records, removed_at NULL
            checkRemovedMember alice "eve" False

            -- after first removal
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 3"
            threadDelay 100000 -- wait for async short link data update
            cath ##> "/_get group link data #1"
            cath <## "group ID: 1"
            cath <## "subscribers: 3"

            -- remove silent member (other members don't know about member)
            threadDelay 1000000
            alice ##> "/rm #team dan"
            alice <## "#team: you removed dan from the group (signed)"
            bob <## "#team: alice removed dan from the group (signed)"
            concurrentlyN_
              [ cath <## "error: x.grp.mem.del with unknown member ID",
                do
                  dan <## "#team: alice removed you from the group (signed)"
                  dan <## "use /d #team to delete the group"
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "removed dan (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "removed dan (signed)")])
            cath #$> ("/_get chat #1 count=1", chat, [(0, "removed eve (signed)")]) -- no new chat item
            dan #$> ("/_get chat #1 count=1", chat, [(0, "removed you (signed)")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "removed you (signed)")]) -- no new chat item

            -- dan had no items -> kept as GSMemRemoved record with removed_at set (TTL cleanup path)
            checkRemovedMember alice "dan" True

            -- after second removal
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 2"
            threadDelay 100000 -- wait for async short link data update
            cath ##> "/_get group link data #1"
            cath <## "group ID: 1"
            cath <## "subscribers: 2"

-- asserts the member row is GSMemRemoved, with removed_at set (TTL tombstone) or NULL (permanent)
checkRemovedMember :: HasCallStack => TestCC -> String -> Bool -> Expectation
checkRemovedMember cc name removedAtSet = do
  rows <-
    withCCTransaction cc $ \db ->
      DB.query db "SELECT member_status, removed_at FROM group_members WHERE local_display_name = ?" (Only name) :: IO [(String, Maybe UTCTime)]
  map (\(status, removedAt) -> (status, isJust removedAt)) rows `shouldBe` [("removed", removedAtSet)]

testChannelDeleteGroupSigned :: HasCallStack => TestParams -> IO ()
testChannelDeleteGroupSigned ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            alice ##> "/d #team"
            alice <## "#team: you deleted the group (signed)"
            concurrentlyN_
              [ do
                  bob <## "#team: alice deleted the group (signed)"
                  bob <## "use /d #team to delete the local copy of the group",
                do
                  cath <## "#team: alice deleted the group (signed)"
                  cath <## "use /d #team to delete the local copy of the group",
                do
                  dan <## "#team: alice deleted the group (signed)"
                  dan <## "use /d #team to delete the local copy of the group",
                do
                  eve <## "#team: alice deleted the group (signed)"
                  eve <## "use /d #team to delete the local copy of the group"
              ]

testChannelDeleteGroupCleanup :: HasCallStack => TestParams -> IO ()
testChannelDeleteGroupCleanup ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
        memberJoinChannel "team" [bob] [alice] shortLink fullLink cath

        -- verify message delivery works
        alice #> "#team hi"
        bob <# "#team> hi"
        cath <# "#team> hi [>>]"

        -- owner deletes channel
        alice ##> "/d #team"
        concurrentlyN_
          [ alice <## "#team: you deleted the group (signed)",
            do
              bob <## "#team: alice deleted the group (signed)"
              bob <## "use /d #team to delete the local copy of the group",
            do
              cath <## "#team: alice deleted the group (signed)"
              cath <## "use /d #team to delete the local copy of the group"
          ]

    -- restart relay, verify no extra subscriptions and no errors
    withTestChatOpts ps relayTestOpts "bob" $ \bob -> do
      bob <## "subscribed 1 connections on server localhost"
      bob ##> "/groups"
      bob <## "#team (group deleted, delete local copy: /d #team)"

testChannelOwnerLeave :: HasCallStack => TestParams -> IO ()
testChannelOwnerLeave ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner leaves channel (XGrpLeave is signed)
            threadDelay 1000000
            alice ##> "/leave #team"
            alice <## "#team: you left the group"
            alice <## "use /d #team to delete the group"
            bob <## "#team: alice left the group (signed)"
            concurrentlyN_
              [ cath <## "#team: alice left the group (signed)",
                dan <## "#team: alice left the group (signed)",
                eve <## "#team: alice left the group (signed)"
              ]
            alice #$> ("/_get chat #1 count=1", chat, [(1, "left (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            cath #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])

testChannelSubscriberLeave :: HasCallStack => TestParams -> IO ()
testChannelSubscriberLeave ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- other members discover cath
            threadDelay 1000000
            cath #> "#team hello from cath"
            bob <# "#team cath> hello from cath"
            concurrentlyN_
              [ alice <# "#team cath> hello from cath [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> hello from cath [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> hello from cath [>>]"
              ]

            -- before any leave — owner count is maintained synchronously
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 4"
            threadDelay 100000 -- wait for async short link data update
            eve ##> "/_get group link data #1"
            eve <## "group ID: 1"
            eve <## "subscribers: 4"

            -- known member leaves (XGrpLeave signed) - owner and relay see items
            threadDelay 1000000
            cath ##> "/leave #team"
            cath <## "#team: you left the group"
            cath <## "use /d #team to delete the group"
            bob <## "#team: cath left the group (signed)"
            alice <## "#team: cath left the group (signed)"
            -- other subscribers: cath is known, but items are muted (muteEventInChannel)
            -- member status is still updated to left in DB
            alice #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            cath #$> ("/_get chat #1 count=1", chat, [(1, "left (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")]) -- no leave item
            eve #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")]) -- no leave item

            -- after first leave
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 3"
            threadDelay 100000 -- wait for async short link data update
            eve ##> "/_get group link data #1"
            eve <## "group ID: 1"
            eve <## "subscribers: 3"

            -- verify cath's member status is "left" on all clients
            checkMemberStatus alice "cath" (Just "left")
            checkMemberStatus bob "cath" (Just "left")
            checkMemberStatus cath "cath" (Just "left")
            checkMemberStatus dan "cath" (Just "left")
            checkMemberStatus eve "cath" (Just "left")

            -- silent subscriber leaves (unknown to other subscribers)
            threadDelay 1000000
            dan ##> "/leave #team"
            dan <## "#team: you left the group"
            dan <## "use /d #team to delete the group"
            bob <## "#team: dan left the group (signed)"
            alice <## "#team: dan left the group (signed)"
            -- dan never sent before leaving and is now left, so the relay does not prepend
            -- his XGrpMemNew; eve receives the bare XGrpLeave and does not create a record (allowCreate=False)
            alice #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            bob #$> ("/_get chat #1 count=1", chat, [(0, "left (signed)")])
            dan #$> ("/_get chat #1 count=1", chat, [(1, "left (signed)")])
            eve #$> ("/_get chat #1 count=1", chat, [(0, "hello from cath")]) -- no new item

            -- after second leave
            alice ##> "/_info #1"
            alice <## "group ID: 1"
            alice <## "subscribers: 2"
            threadDelay 100000 -- wait for async short link data update
            eve ##> "/_get group link data #1"
            eve <## "group ID: 1"
            eve <## "subscribers: 2"

            -- verify dan's member status is "left" on nodes that know dan
            checkMemberStatus alice "dan" (Just "left")
            checkMemberStatus bob "dan" (Just "left")
            checkMemberStatus dan "dan" (Just "left")
            -- the relay did not announce left dan, and the bare XGrpLeave does not create a
            -- record (allowCreate=False), so eve never learned dan
            checkMemberStatus eve "dan" Nothing
            -- cath left earlier and was excluded from the forward; no record on cath
            checkMemberStatus cath "dan" Nothing
  where
    checkMemberStatus :: HasCallStack => TestCC -> T.Text -> Maybe T.Text -> IO ()
    checkMemberStatus cc name expected = do
      statuses <- withCCTransaction cc $ \db ->
        DB.query db "SELECT member_status FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only T.Text]
      map (\(Only s) -> s) statuses `shouldBe` maybeToList expected

testChannelRelayLeave :: HasCallStack => TestParams -> IO ()
testChannelRelayLeave ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
              forM_ [dan, eve] $ \member ->
                memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink member

              -- verify channel works
              alice #> "#team hello"
              [bob, cath] *<# "#team> hello"
              [dan, eve] *<# "#team> hello [>>]"

              -- relay1 (bob) leaves
              threadDelay 100000
              bob ##> "/leave #team"
              bob <## "#team: you left the group (future invitations will be rejected)"
              bob <## "use /group allow #team to allow future invitations"
              bob <## "use /d #team to delete the group (also clears the rejection)"
              concurrentlyN_
                [ alice <## "#team: bob left the group (signed)",
                  -- cath: not notified (relays not connected, owner doesn't forward)
                  dan <## "#team: bob left the group (signed)",
                  eve <## "#team: bob left the group (signed)"
                ]

              -- verify relay1 member status is "left" on all clients that know bob
              checkMemberStatus alice "bob" (Just "left")
              checkMemberStatus dan "bob" (Just "left")
              checkMemberStatus eve "bob" (Just "left")

              -- verify channel still works with remaining relay
              threadDelay 100000
              alice #> "#team still working"
              cath <# "#team> still working"
              [dan, eve] *<# "#team> still working [>>]"

              -- relay2 (cath) leaves
              threadDelay 100000
              cath ##> "/leave #team"
              cath <## "#team: you left the group (future invitations will be rejected)"
              cath <## "use /group allow #team to allow future invitations"
              cath <## "use /d #team to delete the group (also clears the rejection)"
              concurrentlyN_
                [ alice <## "#team: cath left the group (signed)",
                  dan <## "#team: cath left the group (signed)",
                  eve <## "#team: cath left the group (signed)"
                ]

              -- verify relay2 member status
              checkMemberStatus alice "cath" (Just "left")
              checkMemberStatus dan "cath" (Just "left")
              checkMemberStatus eve "cath" (Just "left")

              -- verify no delivery: owner sends but no relays to forward
              alice #> "#team no delivery"
              (dan </)
              (eve </)

              -- new subscriber tries to join channel with no relays - gets proper error
              threadDelay 100000
              frank ##> ("/_connect plan 1 " <> shortLink)
              frank <## "group link: channel has no active relays, please try to join later"
  where
    checkMemberStatus :: HasCallStack => TestCC -> T.Text -> Maybe T.Text -> IO ()
    checkMemberStatus cc name expected = do
      statuses <- withCCTransaction cc $ \db ->
        DB.query db "SELECT member_status FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only T.Text]
      map (\(Only s) -> s) statuses `shouldBe` maybeToList expected

testChannelOwnerProfileUpdate :: HasCallStack => TestParams -> IO ()
testChannelOwnerProfileUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner updates profile (XInfo is signed)
            -- profile update to group is sent lazily with next message
            threadDelay 1000000
            alice ##> "/_profile 1 {\"displayName\": \"alisa\", \"fullName\": \"\"}"
            alice <## "user profile is changed to alisa (your 0 contacts are notified)"

            -- sending as channel does NOT trigger profile update
            threadDelay 1000000
            alice #> "#team hello from channel"
            bob <# "#team> hello from channel"
            concurrentlyN_
              [ cath <# "#team> hello from channel [>>]",
                dan <# "#team> hello from channel [>>]",
                eve <# "#team> hello from channel [>>]"
              ]
            -- no profile update items on any participant
            alice #$> ("/_get chat #1 count=1", chat, [(1, "hello from channel")])
            bob #$> ("/_get chat #1 count=2", chat, [(0, "connected"), (0, "hello from channel")])
            cath #$> ("/_get chat #1 count=2", chat, [(0, "connected"), (0, "hello from channel")])
            dan #$> ("/_get chat #1 count=2", chat, [(0, "connected"), (0, "hello from channel")])
            eve #$> ("/_get chat #1 count=2", chat, [(0, "connected"), (0, "hello from channel")])
            -- verify profiles are updated correctly
            alice `hasContactProfiles` ["alisa", "bob", "cath", "dan", "eve"]
            bob `hasContactProfiles` ["alice", "bob", "cath", "dan", "eve"]
            cath `hasContactProfiles` ["alice", "bob", "cath"]
            dan `hasContactProfiles` ["alice", "bob", "dan"]
            eve `hasContactProfiles` ["alice", "bob", "eve"]

            -- sending as member (as_group=off) triggers profile update
            threadDelay 1000000
            alice ##> "/_send #1(as_group=off) text hello from alisa"
            alice <# "#team hello from alisa"
            bob <# "#team alisa> hello from alisa"
            concurrentlyN_
              [ cath <# "#team alisa> hello from alisa [>>]",
                dan <# "#team alisa> hello from alisa [>>]",
                eve <# "#team alisa> hello from alisa [>>]"
              ]
            -- profile update items on all receivers (signed), not on alice who sent it
            alice #$> ("/_get chat #1 count=2", chat, [(1, "hello from channel"), (1, "hello from alisa")])
            bob #$> ("/_get chat #1 count=2", chat, [(0, "updated profile (signed)"), (0, "hello from alisa")])
            cath #$> ("/_get chat #1 count=2", chat, [(0, "updated profile (signed)"), (0, "hello from alisa")])
            dan #$> ("/_get chat #1 count=2", chat, [(0, "updated profile (signed)"), (0, "hello from alisa")])
            eve #$> ("/_get chat #1 count=2", chat, [(0, "updated profile (signed)"), (0, "hello from alisa")])
            -- verify profiles are updated correctly
            forM_ [alice, bob] $ \cc -> cc `hasContactProfiles` ["alisa", "bob", "cath", "dan", "eve"]
            cath `hasContactProfiles` ["alisa", "bob", "cath"]
            dan `hasContactProfiles` ["alisa", "bob", "dan"]
            eve `hasContactProfiles` ["alisa", "bob", "eve"]

testChannelSubscriberProfileUpdate :: HasCallStack => TestParams -> IO ()
testChannelSubscriberProfileUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote dan to member early (observer default) so its role-change item precedes the messages
            promoteChannelMember "team" alice bob dan [cath, eve]

            -- enable support and create support chat for cath (but not dan)
            threadDelay 1000000
            alice ##> "/set support #team on"
            alice <## "updated group preferences:"
            alice <## "Chat with admins: on"
            toggledSupport bob "alice" "team" "on"
            concurrentlyN_
              [ toggledSupport cath "alice" "team" "on",
                toggledSupport dan "alice" "team" "on",
                toggledSupport eve "alice" "team" "on"
              ]

            threadDelay 1000000
            alice #> "#team (support: cath) welcome"
            bob <# "#team (support: cath) alice> welcome"
            cath <# "#team (support) alice> welcome [>>]"
            (dan </)
            (eve </)

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- other members discover cath
            threadDelay 1000000
            cath #> "#team hello from cath"
            bob <# "#team cath> hello from cath"
            concurrentlyN_
              [ alice <# "#team cath> hello from cath [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> hello from cath [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> hello from cath [>>]"
              ]

            -- known subscriber updates profile (XInfo signed)
            -- cath has support chat -> profile update created in support scope
            threadDelay 1000000
            cath ##> "/_profile 1 {\"displayName\": \"kate\", \"fullName\": \"\"}"
            cath <## "user profile is changed to kate (your 0 contacts are notified)"
            cath #> "#team hello from kate"
            bob <# "#team kate> hello from kate"
            concurrentlyN_
              [ alice <# "#team kate> hello from kate [>>]",
                dan <# "#team kate> hello from kate [>>]",
                eve <# "#team kate> hello from kate [>>]"
              ]
            -- no profile update items in main scope on alice and bob
            alice #$> ("/_get chat #1 count=2", chat, [(0, "hello from cath"), (0, "hello from kate")])
            bob #$> ("/_get chat #1 count=2", chat, [(0, "hello from cath"), (0, "hello from kate")])
            -- profile update items in cath's support scope on alice and bob
            alice #$> ("/_get chat #1(_support:3) count=1", chat, [(0, "updated profile (signed)")])
            bob #$> ("/_get chat #1(_support:3) count=1", chat, [(0, "updated profile (signed)")])
            -- no profile update items on dan and eve (subscriber-to-subscriber muted)
            dan #$> ("/_get chat #1 count=2", chat, [(0, "hello from cath"), (0, "hello from kate")])
            eve #$> ("/_get chat #1 count=2", chat, [(0, "hello from cath"), (0, "hello from kate")])
            -- cath doesn't see her own profile update
            cath #$> ("/_get chat #1 count=2", chat, [(1, "hello from cath"), (1, "hello from kate")])
            -- verify profiles are updated correctly
            forM_ [alice, bob] $ \cc -> cc `hasContactProfiles` ["alice", "bob", "kate", "dan", "eve"]
            dan `hasContactProfiles` ["alice", "bob", "kate", "dan"]
            -- cath/eve also know dan by id hash now (roster-learned before dan posts); not asserted

            -- previously silent subscriber updates profile
            -- dan has no support chat -> no profile update item created
            threadDelay 1000000
            dan ##> "/_profile 1 {\"displayName\": \"dave\", \"fullName\": \"\"}"
            dan <## "user profile is changed to dave (your 0 contacts are notified)"
            dan #> "#team hello from dave"
            bob <# "#team dave> hello from dave"
            concurrentlyN_
              [ alice <# "#team dave> hello from dave [>>]",
                do
                  eve <### [EndsWith "updated to dave"]
                  eve <## "#team: bob introduced dave in the channel"
                  eve <# "#team dave> hello from dave [>>]",
                do
                  cath <### [EndsWith "updated to dave"]
                  cath <## "#team: bob introduced dave in the channel"
                  cath <# "#team dave> hello from dave [>>]"
              ]
            -- no profile update items in main scope (dan has no support chat, item not created)
            alice #$> ("/_get chat #1 count=2", chat, [(0, "hello from kate"), (0, "hello from dave")])
            bob #$> ("/_get chat #1 count=2", chat, [(0, "hello from kate"), (0, "hello from dave")])
            -- no profile update items on cath and eve (subscriber-to-subscriber muted)
            cath #$> ("/_get chat #1 count=2", chat, [(1, "hello from kate"), (0, "hello from dave")])
            eve #$> ("/_get chat #1 count=2", chat, [(0, "hello from kate"), (0, "hello from dave")])
            -- dan doesn't see his own profile update
            dan #$> ("/_get chat #1 count=2", chat, [(0, "hello from kate"), (1, "hello from dave")])
            -- verify dan has no support chat (only kate has one)
            alice ##> "/member support chats #team"
            alice <## "members require attention: 0"
            alice <## "kate (id 3): unread: 0, require attention: 0, mentions: 0"
            -- verify profiles are updated correctly
            forM_ [alice, bob] $ \cc -> cc `hasContactProfiles` ["alice", "bob", "kate", "dave", "eve"]
            cath `hasContactProfiles` ["alice", "bob", "kate", "dave"]
            dan `hasContactProfiles` ["alice", "bob", "kate", "dave"]
            eve `hasContactProfiles` ["alice", "bob", "kate", "dave", "eve"]

testChannelAddRelay :: HasCallStack => TestParams -> IO ()
testChannelAddRelay ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            -- create channel with 1 relay (bob)
            (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob

            -- subscriber joins through bob (the only relay at this point)
            memberJoinChannel "team" [bob] [alice] shortLink fullLink dan

            -- configure cath as a second relay
            cath ##> "/ad"
            (cathSLink, _cLink) <- getContactLinks cath True
            alice ##> ("/relays name=cath " <> cathSLink)
            alice <## "ok"

            -- can't add same relay twice
            alice ##> "/_add relays #1 1"
            alice <## "bad chat command: some relays are already in the group"

            -- add cath relay to existing channel
            alice ##> "/_add relays #1 2"
            alice <## "#team: group relays:"
            alice <## "  - relay id 1: active"
            alice <## "  - relay id 2: invited"

            -- wait for cath to join as relay (async)
            concurrentlyN_
              [ do
                  alice <## "#team: group link relays updated, current relays:"
                  alice
                    <### [ "  - relay id 1: active",
                           "  - relay id 2: active"
                         ]
                  alice <## "group link:"
                  void $ getTermLine alice,
                cath <## "#team: you joined the group as relay"
              ]

            threadDelay 100000

            -- existing subscriber discovers and connects to new relay
            concurrentlyN_
              [ do
                  dan <## "#team: joining the group (connecting to relay cath)..."
                  dan <## "#team: you joined the group (connected to relay cath)",
                do
                  cath <## "dan (Daniel): accepting request to join group #team..."
                  cath <## "#team: dan joined the group"
              ]

            threadDelay 100000

            -- new subscriber joins through both relays
            memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink eve

            -- verify delivery through both relays
            alice #> "#team hello"
            [bob, cath] *<# "#team> hello"
            [dan, eve] *<# "#team> hello [>>]"

testChannelAddRelayWithRoster :: HasCallStack => TestParams -> IO ()
testChannelAddRelayWithRoster ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "dan" danProfile $ \dan ->
        withNewTestChat ps "cath" cathProfile $ \cath ->
          withNewTestChat ps "eve" eveProfile $ \_eve -> do
            (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
            memberJoinChannel "team" [bob] [alice] shortLink fullLink cath

            -- promote cath observer -> moderator: the roster is created (bob caches it)
            threadDelay 100000
            alice ##> "/mr #team cath moderator"
            alice <## "#team: you changed the role of cath to moderator (signed)"
            concurrentlyN_
              [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
                cath <## "#team: alice changed your role from observer to moderator (signed)"
              ]
            threadDelay 100000

            -- add dan as a 2nd relay; with a roster present it must cache the roster and ack
            -- (XGrpRosterAck) before alice publishes it as joinable
            dan ##> "/ad"
            (danSLink, _cLink) <- getContactLinks dan True
            alice ##> ("/relays name=dan " <> danSLink)
            alice <## "ok"
            alice ##> "/_add relays #1 2"
            alice <## "#team: group relays:"
            alice <## "  - relay id 1: active"
            alice <## "  - relay id 2: invited"
            concurrentlyN_
              [ do
                  alice <## "#team: group link relays updated, current relays:"
                  alice
                    <### [ "  - relay id 1: active",
                           "  - relay id 2: active"
                         ]
                  alice <## "group link:"
                  void $ getTermLine alice,
                dan <## "#team: you joined the group as relay"
              ]

            -- cath (an existing member) connects to the new relay and is attached to her roster
            -- record, kept as moderator (the relay learned cath from the cached roster snapshot, so
            -- it surfaces no role-change item for her)
            concurrentlyN_
              [ do
                  cath <## "#team: joining the group (connecting to relay dan)..."
                  cath <## "#team: you joined the group (connected to relay dan)",
                dan
                  <### [ EndsWith "accepting request to join group #team...",
                         EndsWith "is connected"
                       ]
              ]

            threadDelay 100000
            -- the new relay holds the roster (cath is moderator) and learns her name when she connects
            checkMemberRow dan "cath" (Just "moderator")

testChannelRosterMultipartReassembly :: HasCallStack => TestParams -> IO ()
testChannelRosterMultipartReassembly ps =
  withNewTestChatCfgOpts ps cfg testOpts "alice" aliceProfile $ \alice ->
    withNewTestChatCfgOpts ps cfg relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatCfgOpts ps cfg testOpts "cath" cathProfile $ \cath ->
        withNewTestChatCfgOpts ps cfg testOpts "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          threadDelay 100000
          alice ##> "/mr #team cath moderator"
          alice <## "#team: you changed the role of cath to moderator (signed)"
          concurrentlyN_
            [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
              cath <## "#team: alice changed your role from observer to moderator (signed)"
            ]
          threadDelay 100000
          memberJoinChannel "team" [bob] [alice, cath] shortLink fullLink dan
          -- dan reassembles the multi-chunk roster from the served snapshot (arrives async)
          threadDelay 1000000
          checkMemberRow dan "cath" (Just "moderator")
  where
    cfg = testCfg {fileChunkSize = 30}

testChannelRosterDigestMismatchRejected :: HasCallStack => TestParams -> IO ()
testChannelRosterDigestMismatchRejected ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "frank" frankProfile $ \frank -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          threadDelay 100000
          alice ##> "/mr #team cath moderator"
          alice <## "#team: you changed the role of cath to moderator (signed)"
          concurrentlyN_
            [ bob <## "#team: alice changed the role of cath from observer to moderator (signed)",
              cath <## "#team: alice changed your role from observer to moderator (signed)"
            ]
          threadDelay 100000
          -- corrupt the relay's stored blob (same length, different content) so its digest no
          -- longer matches the signed header (DB-agnostic: read it, overwrite with zeroed bytes)
          withCCTransaction bob $ \db -> do
            rows <- DB.query_ db "SELECT roster_blob FROM groups WHERE roster_blob IS NOT NULL" :: IO [Only (Binary ByteString)]
            forM_ rows $ \(Only (Binary blob)) ->
              DB.execute db "UPDATE groups SET roster_blob = ? WHERE roster_blob IS NOT NULL" (Only (Binary (B.replicate (B.length blob) '\NUL')))
          -- frank joins; bob re-serves the valid header with the corrupted blob, frank rejects it
          threadDelay 100000
          memberJoinChannel "team" [bob] [alice, cath] shortLink fullLink frank
          threadDelay 1000000
          -- the rejected roster never elevates cath: the intro caps her to the channel default, so she
          -- stays observer (not moderator), and the version must not advance to the corrupted roster's version 1
          checkMemberRow frank "cath" (Just "observer")
          checkRosterNotApplied frank
  where
    -- the version is the second guarantee (the role is asserted above): frank holds exactly the team
    -- group with no roster applied, so roster_version is NULL - it never advanced to the corrupted version 1
    checkRosterNotApplied :: HasCallStack => TestCC -> IO ()
    checkRosterNotApplied cc = do
      vs <- withCCTransaction cc $ \db ->
        DB.query_ db "SELECT roster_version FROM groups" :: IO [Only (Maybe Int64)]
      map (\(Only v) -> v) vs `shouldBe` [Nothing]

testChannelPromotedMemberCanPost :: HasCallStack => TestParams -> IO ()
testChannelPromotedMemberCanPost ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink dan
          -- promote cath to member: cath enters the owner-signed roster (dan learns cath by id hash)
          promoteChannelMember "team" alice bob cath [dan]
          -- the promoted member can now post; dan resolves cath on the first forward
          cath #> "#team hi from cath"
          bob <# "#team cath> hi from cath"
          alice <# "#team cath> hi from cath [>>]"
          dan <### [EndsWith "updated to cath"]
          dan <## "#team: bob introduced cath (Catherine) in the channel"
          dan <# "#team cath> hi from cath [>>]"
          checkMemberRow dan "cath" (Just "member")

testChannelObserverCannotPost :: HasCallStack => TestParams -> IO ()
testChannelObserverCannotPost ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          memberJoinChannel "team" [bob] [alice] shortLink fullLink dan
          -- cath is an observer (default): its own post is rejected locally and never reaches the relay
          cath ##> "#team observer attempt"
          cath <## "#team: you don't have permission to send messages"
          -- promote cath to member; the post is now accepted and delivered, dan resolves cath
          promoteChannelMember "team" alice bob cath [dan]
          cath #> "#team member post"
          bob <# "#team cath> member post"
          alice <# "#team cath> member post [>>]"
          dan <### [EndsWith "updated to cath"]
          dan <## "#team: bob introduced cath (Catherine) in the channel"
          dan <# "#team cath> member post [>>]"

testChannelPromotedMemberRejoinViaRelay :: HasCallStack => TestParams -> IO ()
testChannelPromotedMemberRejoinViaRelay ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "dan" danProfile $ \dan ->
        withNewTestChat ps "cath" cathProfile $ \cath -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
          memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
          -- promote cath to member: cath enters the owner-signed roster with her pinned key
          threadDelay 100000
          promoteChannelMember "team" alice bob cath []
          threadDelay 100000
          -- add dan as a 2nd relay; it caches the roster (incl. member cath) before joinable
          dan ##> "/ad"
          (danSLink, _cLink) <- getContactLinks dan True
          alice ##> ("/relays name=dan " <> danSLink)
          alice <## "ok"
          alice ##> "/_add relays #1 2"
          alice <## "#team: group relays:"
          alice <## "  - relay id 1: active"
          alice <## "  - relay id 2: invited"
          concurrentlyN_
            [ do
                alice <## "#team: group link relays updated, current relays:"
                alice
                  <### [ "  - relay id 1: active",
                         "  - relay id 2: active"
                       ]
                alice <## "group link:"
                void $ getTermLine alice,
              dan <## "#team: you joined the group as relay"
            ]
          -- cath (a promoted member) connects to the new relay; the widened join gate
          -- (verifyKey over the roster-pinned key) accepts her and keeps her as member
          concurrentlyN_
            [ do
                cath <## "#team: joining the group (connecting to relay dan)..."
                cath <## "#team: you joined the group (connected to relay dan)",
              dan
                <### [ EndsWith "accepting request to join group #team...",
                       EndsWith "is connected"
                     ]
            ]
          threadDelay 100000
          checkMemberRow dan "cath" (Just "member")

testChannelRosterMultiRelayMultipart :: HasCallStack => TestParams -> IO ()
testChannelRosterMultiRelayMultipart ps =
  withNewTestChatCfgOpts ps cfg testOpts "alice" aliceProfile $ \alice ->
    withNewTestChatCfgOpts ps cfg relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatCfgOpts ps cfg relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChatCfgOpts ps cfg testOpts "dan" danProfile $ \dan ->
          withNewTestChatCfgOpts ps cfg testOpts "eve" eveProfile $ \eve ->
            withNewTestChatCfgOpts ps cfg testOpts "frank" frankProfile $ \frank -> do
              createChannel2Relays "team" alice bob cath dan eve frank

              -- promote eve to moderator: the owner-signed roster broadcasts through BOTH relays to dan and
              -- frank (each connected to both). At fileChunkSize=30 the blob spans multiple chunks, so each
              -- member receives two interleaved multi-chunk streams (one per relay) for the same roster.
              threadDelay 1000000
              alice ##> "/mr #team eve moderator"
              alice <## "#team: you changed the role of eve to moderator (signed)"
              concurrentlyN_
                [ bob <## "#team: alice changed the role of eve from observer to moderator (signed)",
                  cath <## "#team: alice changed the role of eve from observer to moderator (signed)",
                  eve <## "#team: alice changed your role from observer to moderator (signed)",
                  dan <### [EndsWith "to moderator (signed)"],
                  frank <### [EndsWith "to moderator (signed)"]
                ]
              threadDelay 1000000 -- let both relays' interleaved multipart streams settle

              -- per-source transfers keep the streams independent, so each member reassembles the blob and pins
              -- eve as the single moderator WITH her owner-attested key (role + key both come from the blob)
              checkOneModeratorWithKey dan
              checkOneModeratorWithKey frank
  where
    cfg = testCfg {fileChunkSize = 30}
    checkOneModeratorWithKey cc = do
      rows <- withCCTransaction cc $ \db ->
        DB.query_ db "SELECT member_pub_key FROM group_members WHERE member_role = 'moderator'" :: IO [Only (Maybe ByteString)]
      map (\(Only k) -> isJust k) rows `shouldBe` [True]

testChannelRemoveRelay :: HasCallStack => TestParams -> IO ()
testChannelRemoveRelay ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
          memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink dan

          -- verify delivery works
          alice #> "#team hello"
          [bob, cath] *<# "#team> hello"
          dan <# "#team> hello [>>]"

          -- remove relay bob
          threadDelay 100000
          alice ##> "/rm #team bob"
          alice <## "#team: you removed bob from the group (signed)"
          concurrentlyN_
            [ do
                bob <## "#team: alice removed you from the group (signed)"
                bob <## "use /d #team to delete the group",
              -- cath doesn't have bob in member list (relays aren't introduced to each other),
              -- so x.grp.mem.del arrives with unknown member ID — cath still forwards it (Left branch in xGrpMemDel)
              cath <## "error: x.grp.mem.del with unknown member ID",
              dan <## "#team: alice removed bob from the group (signed)"
            ]

          -- verify delivery still works via remaining relay (cath)
          threadDelay 100000
          alice #> "#team still working"
          cath <# "#team> still working"
          dan <# "#team> still working [>>]"

          -- remove last relay cath
          threadDelay 100000
          alice ##> "/rm #team cath"
          alice <## "#team: you removed cath from the group (signed)"
          concurrentlyN_
            [ do
                cath <## "#team: alice removed you from the group (signed)"
                cath <## "use /d #team to delete the group",
              dan <## "#team: alice removed cath from the group (signed)"
            ]

          -- verify delivery stops — no relays to forward
          threadDelay 100000
          alice #> "#team no relays"
          (dan </)

          -- bob's and cath's member records should be deleted on alice's and dan's sides
          threadDelay 100000
          aliceMembers <- withCCTransaction alice $ \db ->
            DB.query_ db "SELECT local_display_name FROM group_members" :: IO [Only T.Text]
          aliceMembers `shouldMatchList` [Only "alice", Only "dan"]
          danMembers <- withCCTransaction dan $ \db ->
            DB.query_ db "SELECT local_display_name FROM group_members" :: IO [Only T.Text]
          danMembers `shouldMatchList` [Only "dan", Only "alice"]

          -- re-add bob as relay
          alice ##> "/_add relays #1 1"
          alice <## "#team: group relays:"
          alice .<##. ("  - relay id", ": invited")

          -- wait for bob to rejoin as relay (bob gets LDN "team_1" since old group record exists)
          concurrentlyN_
            [ do
                alice <## "#team: group link relays updated, current relays:"
                alice .<##. ("  - relay id", ": active")
                alice <## "group link:"
                void $ getTermLine alice,
              bob <## "#team_1: you joined the group as relay"
            ]

          threadDelay 100000

          -- subscriber discovers and connects to new relay
          dan ##> "/_get group link data #1"
          dan <## "group ID: 1"
          void $ getTermLine dan -- subscribers: N
          concurrentlyN_
            [ do
                dan <## "#team: joining the group (connecting to relay bob)..."
                dan <## "#team: you joined the group (connected to relay bob)",
              do
                bob <## "dan_1 (Daniel): accepting request to join group #team_1..."
                bob <## "#team_1: dan_1 joined the group"
            ]

          threadDelay 100000

          -- verify delivery works again through re-added relay
          alice #> "#team relays restored"
          bob <# "#team_1> relays restored"
          dan <# "#team> relays restored [>>]"

testChannelRemoveLeftRelay :: HasCallStack => TestParams -> IO ()
testChannelRemoveLeftRelay ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel2Relays "team" alice bob cath
          memberJoinChannel "team" [bob, cath] [alice] shortLink fullLink dan

          -- verify delivery works
          alice #> "#team hello"
          [bob, cath] *<# "#team> hello"
          dan <# "#team> hello [>>]"

          -- bob leaves
          threadDelay 100000
          bob ##> "/l team"
          concurrentlyN_
            [ do
                bob <## "#team: you left the group (future invitations will be rejected)"
                bob <## "use /group allow #team to allow future invitations"
                bob <## "use /d #team to delete the group (also clears the rejection)",
              alice <## "#team: bob left the group (signed)",
              dan <## "#team: bob left the group (signed)"
            ]

          -- alice removes left bob
          threadDelay 100000
          alice ##> "/rm #team bob"
          alice <## "#team: you removed bob from the group (signed)"
          concurrentlyN_
            [ cath <## "error: x.grp.mem.del with unknown member ID",
              dan <## "#team: alice removed bob from the group (signed)"
            ]

          -- bob's member record should be deleted on alice's and dan's sides
          threadDelay 100000
          aliceMembers <- withCCTransaction alice $ \db ->
            DB.query_ db "SELECT local_display_name FROM group_members" :: IO [Only T.Text]
          aliceMembers `shouldMatchList` [Only "alice", Only "cath", Only "dan"]
          danMembers <- withCCTransaction dan $ \db ->
            DB.query_ db "SELECT local_display_name FROM group_members" :: IO [Only T.Text]
          danMembers `shouldMatchList` [Only "dan", Only "alice", Only "cath"]

          -- cath leaves
          threadDelay 100000
          cath ##> "/l team"
          concurrentlyN_
            [ do
                cath <## "#team: you left the group (future invitations will be rejected)"
                cath <## "use /group allow #team to allow future invitations"
                cath <## "use /d #team to delete the group (also clears the rejection)",
              alice <## "#team: cath left the group (signed)",
              dan <## "#team: cath left the group (signed)"
            ]

          -- alice removes left cath - dan doesn't receive (no relay to forward)
          threadDelay 100000
          alice ##> "/rm #team cath"
          alice <## "#team: you removed cath from the group (signed)"

          -- dan syncs with link - should clean up cath's stale record
          threadDelay 100000
          dan ##> "/_get group link data #1"
          dan <## "group ID: 1"
          void $ getTermLine dan -- subscribers: N

          -- cath's member record should be cleaned up on dan's side after sync
          threadDelay 100000
          danMembers2 <- withCCTransaction dan $ \db ->
            DB.query_ db "SELECT local_display_name FROM group_members" :: IO [Only T.Text]
          danMembers2 `shouldMatchList` [Only "dan", Only "alice"]

queryRelayOwnStatus :: TestCC -> Int64 -> IO (Maybe T.Text)
queryRelayOwnStatus cc gId = do
  rows <- withCCTransaction cc $ \db ->
    DB.query db "SELECT relay_own_status FROM groups WHERE group_id = ?" (Only gId)
      :: IO [Only (Maybe T.Text)]
  pure $ case rows of
    [Only s] -> s
    _ -> Nothing

listRelayOwnStatuses :: TestCC -> IO [(Int64, T.Text)]
listRelayOwnStatuses cc =
  withCCTransaction cc $ \db ->
    DB.query_
      db
      "SELECT group_id, relay_own_status FROM groups WHERE relay_own_status IS NOT NULL ORDER BY group_id"
      :: IO [(Int64, T.Text)]

checkRelayGroupCount :: TestCC -> Int -> IO ()
checkRelayGroupCount cc expected = do
  rows <- withCCTransaction cc $ \db ->
    DB.query_ db "SELECT COUNT(*) FROM groups WHERE relay_own_status IS NOT NULL" :: IO [Only Int]
  let n = case rows of
        [Only c] -> c
        _ -> 0
  n `shouldBe` expected

testRelayRejectAfterLeave :: HasCallStack => TestParams -> IO ()
testRelayRejectAfterLeave ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
        memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
        threadDelay 100000

        -- baseline: subscriber receives forwarded messages via the active relay
        alice #> "#team hello"
        bob <# "#team> hello"
        cath <# "#team> hello [>>]"

        -- relay leaves the channel: subscriber gets the signed leave notice via bob's
        -- DJRelayRemoved job, then has no relay to forward subsequent messages.
        bob ##> "/leave #team"
        bob <## "#team: you left the group (future invitations will be rejected)"
        bob <## "use /group allow #team to allow future invitations"
        bob <## "use /d #team to delete the group (also clears the rejection)"
        concurrentlyN_
          [ alice <## "#team: bob left the group (signed)",
            cath <## "#team: bob left the group (signed)"
          ]
        threadDelay 100000

        bobLeaveStatus <- queryRelayOwnStatus bob 1
        bobLeaveStatus `shouldBe` Just "rejected"

        -- with no active relay, owner's messages don't reach the subscriber
        alice #> "#team after leave"
        (cath </)

        -- owner removes the (now-left) relay member; cascade clears alice's group_relays row
        alice ##> "/rm #team bob"
        alice <## "#team: you removed bob from the group (signed)"
        threadDelay 100000

        -- owner re-adds bob as relay
        alice ##> "/_add relays #1 1"
        alice <## "#team: group relays:"
        alice .<##. ("  - relay id", ": invited")

        -- bob's xGrpRelayInv finds the 'rejected' row for this link and sends XGrpRelayReject.
        -- alice's CONF handler emits TERelayRejected; the relay row flips to 'rejected'.
        alice <## "#team: relay rejected, reason: RRRRejoinRejected"

        -- assert alice's fresh GroupRelay row is marked 'rejected' and the relay
        -- GroupMember is GSMemLeft so the owner UI treats it as gone
        aliceRelayStatuses <- withCCTransaction alice $ \db ->
          DB.query_ db "SELECT relay_status FROM group_relays" :: IO [Only T.Text]
        map (\(Only s) -> s) aliceRelayStatuses `shouldBe` ["rejected"]
        aliceRelayMemStatuses <- withCCTransaction alice $ \db ->
          DB.query_ db "SELECT member_status FROM group_members WHERE member_role = 'relay'"
            :: IO [Only T.Text]
        map (\(Only s) -> s) aliceRelayMemStatuses `shouldBe` ["left"]

        -- subscriber still doesn't receive after the failed re-invitation
        alice #> "#team after rejection"
        (cath </)

        -- bob's transient row was created with relay_own_status='rejected';
        -- after INFO arrives the cleanup arm deletes it. Original row 1 remains rejected.
        threadDelay 1000000
        checkRelayGroupCount bob 1
        finalStatuses <- listRelayOwnStatuses bob
        finalStatuses `shouldBe` [(1, "rejected")]

testRelayAllowAcceptsAgain :: HasCallStack => TestParams -> IO ()
testRelayAllowAcceptsAgain ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
        memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
        threadDelay 100000

        -- baseline: subscriber receives forwarded messages
        alice #> "#team hello"
        bob <# "#team> hello"
        cath <# "#team> hello [>>]"

        bob ##> "/leave #team"
        bob <## "#team: you left the group (future invitations will be rejected)"
        bob <## "use /group allow #team to allow future invitations"
        bob <## "use /d #team to delete the group (also clears the rejection)"
        concurrentlyN_
          [ alice <## "#team: bob left the group (signed)",
            cath <## "#team: bob left the group (signed)"
          ]
        threadDelay 100000

        -- with no relay, subscriber doesn't receive
        alice #> "#team during downtime"
        (cath </)

        -- /_relay allow flips bob's row from 'rejected' to 'inactive'
        bob ##> "/group allow #team"
        bob <## "#team: relay rejection cleared"
        bobClearStatus <- queryRelayOwnStatus bob 1
        bobClearStatus `shouldBe` Just "inactive"

        -- owner can now re-add and bob accepts as relay (the rejection has been cleared)
        alice ##> "/rm #team bob"
        alice <## "#team: you removed bob from the group (signed)"
        threadDelay 100000

        alice ##> "/_add relays #1 1"
        concurrentlyN_
          [ do
              alice <## "#team: group relays:"
              alice .<##. ("  - relay id", ": invited")
              alice <## "#team: group link relays updated, current relays:"
              alice .<##. ("  - relay id", ": active")
              alice <## "group link:"
              void $ getTermLine alice,
            bob <## "#team_1: you joined the group as relay"
          ]
        threadDelay 100000

        -- subscriber syncs against link data and reconnects to the new relay
        cath ##> "/_get group link data #1"
        cath <## "group ID: 1"
        void $ getTermLine cath
        concurrentlyN_
          [ do
              cath <## "#team: joining the group (connecting to relay bob)..."
              cath <## "#team: you joined the group (connected to relay bob)",
            do
              bob <## "cath_1 (Catherine): accepting request to join group #team_1..."
              bob <## "#team_1: cath_1 joined the group"
          ]
        threadDelay 100000

        -- delivery resumes through the freshly accepted relay
        alice #> "#team after allow"
        bob <# "#team_1> after allow"
        cath <# "#team> after allow [>>]"

        -- after re-acceptance, the relay GroupMember is not in the rejected/left state
        aliceRelayMemStatuses <- withCCTransaction alice $ \db ->
          DB.query_ db "SELECT member_status FROM group_members WHERE member_role = 'relay'"
            :: IO [Only T.Text]
        map (\(Only s) -> s) aliceRelayMemStatuses `shouldBe` ["connected"]

testRelayDoesNotRejectUnrelatedChannel :: HasCallStack => TestParams -> IO ()
testRelayDoesNotRejectUnrelatedChannel ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        _ <- prepareChannel1Relay "teama" alice bob
        threadDelay 100000

        bob ##> "/leave #teama"
        bob <## "#teama: you left the group (future invitations will be rejected)"
        bob <## "use /group allow #teama to allow future invitations"
        bob <## "use /d #teama to delete the group (also clears the rejection)"
        alice <## "#teama: bob left the group (signed)"
        threadDelay 100000

        bobAStatus <- queryRelayOwnStatus bob 1
        bobAStatus `shouldBe` Just "rejected"

        -- alice creates a second channel reusing the same bob relay config.
        -- bob's xGrpRelayInv for teamb's link finds no rejection and accepts normally.
        (shortLinkB, fullLinkB) <- prepareChannel' 2 "teamb" alice bob
        memberJoinChannel "teamb" [bob] [alice] shortLinkB fullLinkB cath
        threadDelay 100000

        -- subscriber on teamb receives forwarded messages, proving bob accepts teamb
        -- even though teama remains rejected on bob's side.
        alice #> "#teamb hello"
        bob <# "#teamb> hello"
        cath <# "#teamb> hello [>>]"

        bobBStatus <- queryRelayOwnStatus bob 2
        bobBStatus `shouldNotBe` Just "rejected"
        bobBStatus `shouldNotBe` Nothing

testRelayRejectRaceConcurrentInvitations :: HasCallStack => TestParams -> IO ()
testRelayRejectRaceConcurrentInvitations ps =
  -- After rejection, multiple sequential re-invitations must all reject with
  -- consistent state (each transient row created with RSRejected and cleaned
  -- up by its own INFO).
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice bob
        memberJoinChannel "team" [bob] [alice] shortLink fullLink cath
        threadDelay 100000

        -- baseline: subscriber receives forwarded messages
        alice #> "#team hello"
        bob <# "#team> hello"
        cath <# "#team> hello [>>]"

        bob ##> "/leave #team"
        bob <## "#team: you left the group (future invitations will be rejected)"
        bob <## "use /group allow #team to allow future invitations"
        bob <## "use /d #team to delete the group (also clears the rejection)"
        concurrentlyN_
          [ alice <## "#team: bob left the group (signed)",
            cath <## "#team: bob left the group (signed)"
          ]
        threadDelay 100000

        -- first rejection
        alice ##> "/rm #team bob"
        alice .<##. ("#team: you removed bob from the group", "")
        threadDelay 100000
        alice ##> "/_add relays #1 1"
        alice <## "#team: group relays:"
        alice .<##. ("  - relay id", ": invited")
        alice <## "#team: relay rejected, reason: RRRRejoinRejected"
        threadDelay 1000000
        checkRelayGroupCount bob 1

        -- subscriber doesn't receive between rejections (no active relay)
        alice #> "#team between rejections"
        (cath </)

        -- second rejection
        alice ##> "/rm #team bob"
        alice .<##. ("#team: you removed bob from the group", "")
        threadDelay 100000
        alice ##> "/_add relays #1 1"
        alice <## "#team: group relays:"
        alice .<##. ("  - relay id", ": invited")
        alice <## "#team: relay rejected, reason: RRRRejoinRejected"

        -- subscriber still doesn't receive after the second rejection
        alice #> "#team after second rejection"
        (cath </)

        threadDelay 1000000
        checkRelayGroupCount bob 1
        finalStatuses <- listRelayOwnStatuses bob
        finalStatuses `shouldBe` [(1, "rejected")]

testChannelCreateDeletedRelay :: HasCallStack => TestParams -> IO ()
testChannelCreateDeletedRelay ps =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _) <- getContactLinks bob True
        cath ##> "/ad"
        (cathSLink, _) <- getContactLinks cath True

        alice ##> ("/relays name=bob " <> bobSLink <> " name=cath " <> cathSLink)
        alice <## "ok"

        -- cath deletes her address - simulates relay becoming unavailable
        cath ##> "/da"
        cath <## "Your chat address is deleted - accepted contacts will remain connected."
        cath <## "To create a new chat address use /ad"

        -- channel creation fails because one relay's address was deleted
        alice ##> "/public group relays=1,2 #team"
        alice <## "channel not created, results:"
        alice <## "  relay 1: ok"
        alice <##. "  relay 2: ChatErrorAgent"
        -- deleteInProgressGroup deletes relay connection alice joined on bob;
        -- bob's agent reports AUTH error when the queue is gone — drain it.
        void $ getTermLine bob

testChannelSupportScope :: HasCallStack => TestParams -> IO ()
testChannelSupportScope ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "relay" chatRelayProfile $ \relay ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          (shortLink, fullLink) <- prepareChannel1Relay "team" alice relay
          memberJoinChannel "team" [relay] [alice] shortLink fullLink cath
          memberJoinChannel "team" [relay] [alice] shortLink fullLink dan

          threadDelay 1000000

          alice ##> "/set support #team on"
          alice <## "updated group preferences:"
          alice <## "Chat with admins: on"
          toggledSupport relay "alice" "team" "on"
          concurrentlyN_
            [ toggledSupport cath "alice" "team" "on",
              toggledSupport dan "alice" "team" "on"
            ]

          -- owner sends to cath's support scope, dan doesn't receive
          alice #> "#team (support: cath) hello"
          relay <# "#team (support: cath) alice> hello"
          cath <# "#team (support) alice> hello [>>]"
          (dan </)

          -- cath replies in support scope, dan doesn't receive
          cath #> "#team (support) hi"
          relay <# "#team (support: cath) cath> hi"
          alice <# "#team (support: cath) cath> hi [>>]"
          (dan </)

toggledSupport :: HasCallStack => TestCC -> String -> String -> String -> IO ()
toggledSupport c owner channel onOff  = do
  c <## (owner <> " updated group #" <> channel <> ": (signed)")
  c <## "updated group preferences:"
  c <## ("Chat with admins: " <> onOff)

testChannelMessageUpdate :: HasCallStack => TestParams -> IO ()
testChannelMessageUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message
            alice #> "#team hello"
            bob <# "#team> hello"
            [cath, dan, eve] *<# "#team> hello [>>]"

            -- owner updates channel message
            msgId <- lastItemId alice
            alice ##> ("/_update item #1 " <> msgId <> " text hello updated")
            alice <# "#team [edited] hello updated"
            bob <# "#team> [edited] hello updated"
            [cath, dan, eve] *<# "#team> [edited] hello updated" -- TODO show as forwarded

testChannelMessageDelete :: HasCallStack => TestParams -> IO ()
testChannelMessageDelete ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message
            alice #> "#team hello"
            bob <# "#team> hello"
            [cath, dan, eve] *<# "#team> hello [>>]"

            -- owner deletes channel message (broadcast)
            msgId <- lastItemId alice
            alice #$> ("/_delete item #1 " <> msgId <> " broadcast", id, "message marked deleted")
            bob <# "#team> [marked deleted] hello"
            [cath, dan, eve] *<# "#team> [marked deleted] hello" -- TODO show as forwarded

testChannelMessageDeleteFromHistory :: HasCallStack => TestParams -> IO ()
testChannelMessageDeleteFromHistory ps =
  testChat4 aliceProfile bobProfile cathProfile danProfile test ps
  where
    test alice bob cath dan = withRelay ps $ \relay -> do
      (shortLink, fullLink) <- prepareChannel1Relay "team" alice relay
      memberJoinChannel "team" [relay] [alice] shortLink fullLink bob
      memberJoinChannel "team" [relay] [alice] shortLink fullLink cath

      alice #> "#team hello"
      relay <# "#team> hello"
      [bob, cath] *<# "#team> hello [>>]"

      -- owner deletes from history (relay processes locally but doesn't forward)
      msgId <- lastItemId alice
      alice #$> ("/_delete item #1 " <> msgId <> " history", id, "message marked deleted")
      relay <# "#team> [marked deleted] hello"

      -- subscribers don't receive deletion - next message arrives cleanly
      alice #> "#team still here"
      relay <# "#team> still here"
      [bob, cath] *<# "#team> still here [>>]"

      -- internal delete rejected for channel owner
      msgId2 <- lastItemId alice
      alice ##> ("/_delete item #1 " <> msgId2 <> " internal")
      alice <## "cannot delete this item"
      
      memberJoinChannel "team" [relay] [alice] shortLink fullLink dan
      dan <# "#team> still here [>>]"

testChannelMessageFile :: HasCallStack => TestParams -> IO ()
testChannelMessageFile ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> withXFTPServer $ do
            createChannel1Relay "team" alice bob cath dan eve
            -- the roster arrives as a file before this one; Postgres assigns it a new id and does not
            -- reuse it on delete (SQLite does), so the received message file is id 2 here, 1 on SQLite.
#if defined(dbPostgres)
            let rcvFileId = 2 :: Int
#else
            let rcvFileId = 1 :: Int
#endif
            -- owner sends file as channel message
            alice #> "/f #team ./tests/fixtures/test.jpg"
            alice <## "use /fc 1 to cancel sending"
            alice <## "completed uploading file 1 (test.jpg) for #team"
            bob <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it")
            concurrentlyN_
              [ do
                  cath <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  cath <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  dan <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  dan <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  eve <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  eve <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]")
              ]

            -- all members receive the file concurrently
            src <- B.readFile "./tests/fixtures/test.jpg"
            concurrentlyN_
              [ receiveFile bob "bob" rcvFileId src,
                receiveFile cath "cath" rcvFileId src,
                receiveFile dan "dan" rcvFileId src,
                receiveFile eve "eve" rcvFileId src
              ]
  where
    receiveFile cc name fileId src = do
      let path = "./tests/tmp/test_" <> name <> ".jpg"
      cc ##> ("/fr " <> show fileId <> " " <> path)
      cc
        <### [ ConsoleString ("saving file " <> show fileId <> " from #team to " <> path),
               ConsoleString ("started receiving file " <> show fileId <> " (test.jpg) from #team")
             ]
      cc <## ("completed receiving file " <> show fileId <> " (test.jpg) from #team")
      B.readFile path >>= (`shouldBe` src)

testChannelMessageFileCancel :: HasCallStack => TestParams -> IO ()
testChannelMessageFileCancel ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> withXFTPServer $ do
            createChannel1Relay "team" alice bob cath dan eve
#if defined(dbPostgres)
            let rcvFileId = 2 :: Int
#else
            let rcvFileId = 1 :: Int
#endif
            -- owner sends file as channel message
            alice #> "/f #team ./tests/fixtures/test.jpg"
            alice <## "use /fc 1 to cancel sending"
            alice <## "completed uploading file 1 (test.jpg) for #team"
            bob <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it")
            concurrentlyN_
              [ do
                  cath <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  cath <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  dan <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  dan <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  eve <# "#team> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  eve <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]")
              ]

            -- owner cancels file
            alice ##> "/fc 1"
            alice <## "cancelled sending file 1 (test.jpg) to bob"
            bob <## ("team cancelled sending file " <> show rcvFileId <> " (test.jpg)")
            concurrentlyN_
              [ cath <## ("team cancelled sending file " <> show rcvFileId <> " (test.jpg)"),
                dan <## ("team cancelled sending file " <> show rcvFileId <> " (test.jpg)"),
                eve <## ("team cancelled sending file " <> show rcvFileId <> " (test.jpg)")
              ]

testChannelMessageQuote :: HasCallStack => TestParams -> IO ()
testChannelMessageQuote ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message
            alice #> "#team hello from channel"
            bob <# "#team> hello from channel"
            [cath, dan, eve] *<# "#team> hello from channel [>>]"

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- member quotes channel message
            cath `send` "> #team (hello from) replying to channel"
            cath <# "#team > hello from channel"
            cath <## "      replying to channel"
            bob <# "#team cath> > hello from channel"
            bob <## "      replying to channel"
            concurrentlyN_
              [ do
                  alice <# "#team cath> > hello from channel [>>]"
                  alice <## "      replying to channel [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> > hello from channel [>>]"
                  dan <## "      replying to channel [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> > hello from channel [>>]"
                  eve <## "      replying to channel [>>]"
              ]

testChannelOwnerReaction :: HasCallStack => TestParams -> IO ()
testChannelOwnerReaction ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message
            alice #> "#team hello"
            bob <# "#team> hello"
            [cath, dan, eve] *<# "#team> hello [>>]"

            -- owner reacts to own channel message - reaction is forwarded as member
            alice ##> "+1 #team hello"
            alice <## "added 👍"
            bob <# "#team alice> > hello"
            bob <## "    + 👍"
            concurrentlyN_
              [ do cath <# "#team alice> > hello"
                   cath <## "    + 👍",
                do dan <# "#team alice> > hello"
                   dan <## "    + 👍",
                do eve <# "#team alice> > hello"
                   eve <## "    + 👍"
              ]

testChannelOwnerQuote :: HasCallStack => TestParams -> IO ()
testChannelOwnerQuote ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message
            alice #> "#team hello from channel"
            bob <# "#team> hello from channel"
            [cath, dan, eve] *<# "#team> hello from channel [>>]"

            -- owner quotes own channel message (sender sees own name locally, not a protocol leak)
            alice `send` "> #team (hello from) my reply"
            alice <# "#team > alice hello from channel"
            alice <## "      my reply"
            bob <# "#team> > hello from channel"
            bob <## "      my reply"
            concurrentlyN_
              [ do cath <# "#team> > hello from channel [>>]"
                   cath <## "      my reply [>>]",
                do dan <# "#team> > hello from channel [>>]"
                   dan <## "      my reply [>>]",
                do eve <# "#team> > hello from channel [>>]"
                   eve <## "      my reply [>>]"
              ]

testChannelOwnerUpdateAsMember :: HasCallStack => TestParams -> IO ()
testChannelOwnerUpdateAsMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends message as member (not as channel)
            alice ##> "/_send #1(as_group=off) text hello"
            alice <# "#team hello"
            bob <# "#team alice> hello"
            [cath, dan, eve] *<# "#team alice> hello [>>]"

            -- owner updates message
            msgId <- lastItemId alice
            alice ##> ("/_update item #1 " <> msgId <> " text hello updated")
            alice <# "#team [edited] hello updated"
            bob <# "#team alice> [edited] hello updated"
            [cath, dan, eve] *<# "#team alice> [edited] hello updated"

testChannelOwnerDeleteAsMember :: HasCallStack => TestParams -> IO ()
testChannelOwnerDeleteAsMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends message as member (not as channel)
            alice ##> "/_send #1(as_group=off) text hello"
            alice <# "#team hello"
            bob <# "#team alice> hello"
            [cath, dan, eve] *<# "#team alice> hello [>>]"

            -- owner deletes message (broadcast)
            msgId <- lastItemId alice
            alice #$> ("/_delete item #1 " <> msgId <> " broadcast", id, "message marked deleted")
            bob <# "#team alice> [marked deleted] hello"
            [cath, dan, eve] *<# "#team alice> [marked deleted] hello"

testChannelOwnerFileTransferAsMember :: HasCallStack => TestParams -> IO ()
testChannelOwnerFileTransferAsMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> withXFTPServer $ do
            createChannel1Relay "team" alice bob cath dan eve
#if defined(dbPostgres)
            let rcvFileId = 2 :: Int
#else
            let rcvFileId = 1 :: Int
#endif
            -- owner sends file as member (not as channel)
            alice ##> "/_send #1(as_group=off) json [{\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"file\", \"text\": \"\"}}]"
            alice <# "/f #team ./tests/fixtures/test.jpg"
            alice <## "use /fc 1 to cancel sending"
            alice <## "completed uploading file 1 (test.jpg) for #team"
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it")
            concurrentlyN_
              [ do
                  cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  cath <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  dan <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  dan <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  eve <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  eve <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]")
              ]

            -- all members receive the file
            src <- B.readFile "./tests/fixtures/test.jpg"
            concurrentlyN_
              [ receiveFile bob "bob" rcvFileId src,
                receiveFile cath "cath" rcvFileId src,
                receiveFile dan "dan" rcvFileId src,
                receiveFile eve "eve" rcvFileId src
              ]
  where
    receiveFile cc name fileId src = do
      let path = "./tests/tmp/test_" <> name <> ".jpg"
      cc ##> ("/fr " <> show fileId <> " " <> path)
      cc
        <### [ ConsoleString ("saving file " <> show fileId <> " from alice to " <> path),
               ConsoleString ("started receiving file " <> show fileId <> " (test.jpg) from alice")
             ]
      cc <## ("completed receiving file " <> show fileId <> " (test.jpg) from alice")
      B.readFile path >>= (`shouldBe` src)

testChannelOwnerFileCancelAsMember :: HasCallStack => TestParams -> IO ()
testChannelOwnerFileCancelAsMember ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> withXFTPServer $ do
            createChannel1Relay "team" alice bob cath dan eve
#if defined(dbPostgres)
            let rcvFileId = 2 :: Int
#else
            let rcvFileId = 1 :: Int
#endif
            -- owner sends file as member (not as channel)
            alice ##> "/_send #1(as_group=off) json [{\"filePath\": \"./tests/fixtures/test.jpg\", \"msgContent\": {\"type\": \"file\", \"text\": \"\"}}]"
            alice <# "/f #team ./tests/fixtures/test.jpg"
            alice <## "use /fc 1 to cancel sending"
            alice <## "completed uploading file 1 (test.jpg) for #team"
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it")
            concurrentlyN_
              [ do
                  cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  cath <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  dan <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  dan <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do
                  eve <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes) [>>]"
                  eve <## ("use /fr " <> show rcvFileId <> " [<dir>/ | <path>] to receive it [>>]")
              ]

            -- owner cancels file
            alice ##> "/fc 1"
            alice <## "cancelled sending file 1 (test.jpg) to bob"
            bob <## ("alice cancelled sending file " <> show rcvFileId <> " (test.jpg)")
            concurrentlyN_
              [ cath <## ("alice cancelled sending file " <> show rcvFileId <> " (test.jpg)"),
                dan <## ("alice cancelled sending file " <> show rcvFileId <> " (test.jpg)"),
                eve <## ("alice cancelled sending file " <> show rcvFileId <> " (test.jpg)")
              ]

testChannelReactionAttribution :: HasCallStack => TestParams -> IO ()
testChannelReactionAttribution ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends message as member
            alice ##> "/_send #1(as_group=off) text hello"
            alice <# "#team hello"
            bob <# "#team alice> hello"
            [cath, dan, eve] *<# "#team alice> hello [>>]"

            -- owner reacts to own member message - reaction is forwarded as member
            alice ##> "+1 #team hello"
            alice <## "added 👍"
            bob <# "#team alice> > alice hello"
            bob <## "    + 👍"
            concurrentlyN_
              [ do cath <# "#team alice> > alice hello"
                   cath <## "    + 👍",
                do dan <# "#team alice> > alice hello"
                   dan <## "    + 👍",
                do eve <# "#team alice> > alice hello"
                   eve <## "    + 👍"
              ]

testChannelUpdateFallbackSendAsGroup :: HasCallStack => TestParams -> IO ()
testChannelUpdateFallbackSendAsGroup ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner sends channel message (sendAsGroup=True)
            alice #> "#team channel msg"
            bob <# "#team> channel msg"
            [cath, dan, eve] *<# "#team> channel msg [>>]"

            -- bob locally deletes the item
            bobMsgId <- lastItemId bob
            bob #$> ("/_delete item #1 " <> bobMsgId <> " internal", id, "message deleted")

            -- owner updates message (XMsgUpdate includes asGroup=True)
            aliceMsgId <- lastItemId alice
            alice ##> ("/_update item #1 " <> aliceMsgId <> " text channel msg updated")
            alice <# "#team [edited] channel msg updated"
            -- bob's item was locally deleted, fallback recreates it with [edited] marker
            bob <# "#team> [edited] channel msg updated"
            [cath, dan, eve] *<# "#team> [edited] channel msg updated"

            -- now test sendAsGroup=False case
            -- owner sends message as member
            alice ##> "/_send #1(as_group=off) text member msg"
            alice <# "#team member msg"
            bob <# "#team alice> member msg"
            [cath, dan, eve] *<# "#team alice> member msg [>>]"

            -- bob locally deletes the item
            bobMsgId2 <- lastItemId bob
            bob #$> ("/_delete item #1 " <> bobMsgId2 <> " internal", id, "message deleted")

            -- owner updates message (XMsgUpdate includes asGroup=False)
            aliceMsgId2 <- lastItemId alice
            alice ##> ("/_update item #1 " <> aliceMsgId2 <> " text member msg updated")
            alice <# "#team [edited] member msg updated"
            -- bob's internally deleted item is re-created as from member (sendAsGroup=False)
            bob <# "#team alice> [edited] member msg updated"
            -- forwarded members see correct member attribution
            [cath, dan, eve] *<# "#team alice> [edited] member msg updated"

testForwardAPIUsesParameter :: HasCallStack => TestParams -> IO ()
testForwardAPIUsesParameter ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              createChannel1Relay "team" alice bob cath dan eve
              connectUsers alice frank

              -- frank sends alice a message
              frank #> "@alice hi there"
              alice <# "frank> hi there"

              -- forward to channel with sendAsGroup=True (as channel)
              alice ##> "/last_item_id @frank"
              msgId <- getTermLine alice
              alice ##> ("/_forward #1 as_group=on @2 " <> msgId)
              alice <# "#team <- @frank"
              alice <## "      hi there"
              bob <# "#team> -> forwarded"
              bob <## "      hi there"
              concurrentlyN_
                [ do cath <# "#team> -> forwarded [>>]"
                     cath <## "      hi there [>>]",
                  do dan <# "#team> -> forwarded [>>]"
                     dan <## "      hi there [>>]",
                  do eve <# "#team> -> forwarded [>>]"
                     eve <## "      hi there [>>]"
                ]

              -- forward to channel with sendAsGroup=False (as member)
              alice ##> ("/_forward #1 as_group=off @2 " <> msgId)
              alice <# "#team <- @frank"
              alice <## "      hi there"
              bob <# "#team alice> -> forwarded"
              bob <## "      hi there"
              concurrentlyN_
                [ do cath <# "#team alice> -> forwarded [>>]"
                     cath <## "      hi there [>>]",
                  do dan <# "#team alice> -> forwarded [>>]"
                     dan <## "      hi there [>>]",
                  do eve <# "#team alice> -> forwarded [>>]"
                     eve <## "      hi there [>>]"
                ]

testForwardCLISendAsGroup :: HasCallStack => TestParams -> IO ()
testForwardCLISendAsGroup ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve ->
            withNewTestChat ps "frank" frankProfile $ \frank -> do
              createChannel1Relay "team" alice bob cath dan eve
              connectUsers alice frank

              -- frank sends alice a message
              frank #> "@alice hi"
              alice <# "frank> hi"

              -- CLI forward to channel computes sendAsGroup=True (owner in channel)
              alice `send` "#team <- @frank hi"
              alice <# "#team <- @frank"
              alice <## "      hi"
              bob <# "#team> -> forwarded"
              bob <## "      hi"
              concurrentlyN_
                [ do cath <# "#team> -> forwarded [>>]"
                     cath <## "      hi [>>]",
                  do dan <# "#team> -> forwarded [>>]"
                     dan <## "      hi [>>]",
                  do eve <# "#team> -> forwarded [>>]"
                     eve <## "      hi [>>]"
                ]

testChannelMemberMessageUpdate :: HasCallStack => TestParams -> IO ()
testChannelMemberMessageUpdate ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- member sends a message
            cath #> "#team hello"
            bob <# "#team cath> hello"
            concurrentlyN_
              [ alice <# "#team cath> hello [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> hello [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> hello [>>]"
              ]

            -- member updates their message
            cathMsgId <- lastItemId cath
            cath ##> ("/_update item #1 " <> cathMsgId <> " text hello updated")
            cath <# "#team [edited] hello updated"
            bob <# "#team cath> [edited] hello updated"
            concurrentlyN_
              [ alice <# "#team cath> [edited] hello updated",
                dan <# "#team cath> [edited] hello updated",
                eve <# "#team cath> [edited] hello updated"
              ]

testChannelMemberMessageDelete :: HasCallStack => TestParams -> IO ()
testChannelMemberMessageDelete ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- promote cath to member (observer default) so it can post
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- member sends a message
            cath #> "#team hello"
            bob <# "#team cath> hello"
            concurrentlyN_
              [ alice <# "#team cath> hello [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> hello [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> hello [>>]"
              ]

            -- member deletes their message
            cathMsgId <- lastItemId cath
            cath #$> ("/_delete item #1 " <> cathMsgId <> " broadcast", id, "message marked deleted")
            bob <# "#team cath> [marked deleted] hello"
            concurrentlyN_
              [ alice <# "#team cath> [marked deleted] hello",
                dan <# "#team cath> [marked deleted] hello",
                eve <# "#team cath> [marked deleted] hello"
              ]

memberIdByName :: TestCC -> T.Text -> IO MemberId
memberIdByName cc name = do
  rows <- withCCTransaction cc $ \db ->
    DB.query db "SELECT member_id FROM group_members WHERE local_display_name = ?" (Only name) :: IO [Only ByteString]
  case rows of
    (Only mid : _) -> pure (MemberId mid)
    _ -> fail $ "no member " <> T.unpack name

relayConnIdToMember :: TestCC -> T.Text -> IO ByteString
relayConnIdToMember cc name = do
  rows <- withCCTransaction cc $ \db ->
    DB.query
      db
      "SELECT c.agent_conn_id FROM connections c JOIN group_members m ON m.group_member_id = c.group_member_id WHERE m.local_display_name = ?"
      (Only name) ::
      IO [Only ByteString]
  case rows of
    (Only connId : _) -> pure connId
    _ -> fail $ "no relay connection to member " <> T.unpack name

itemSharedMsgId :: TestCC -> IO SharedMsgId
itemSharedMsgId cc = do
  rows <- withCCTransaction cc $ \db ->
    DB.query_ db "SELECT shared_msg_id FROM chat_items WHERE shared_msg_id IS NOT NULL ORDER BY chat_item_id DESC LIMIT 1" :: IO [Only ByteString]
  case rows of
    (Only smid : _) -> pure (SharedMsgId smid)
    _ -> fail "no shared_msg_id"

testChannelMemberMessageSign :: HasCallStack => TestParams -> IO ()
testChannelMemberMessageSign ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- member sends a signed message
            cath ##> "/_send #1 sign=on text signed hello"
            cath <# "#team signed hello (signed)"
            bob <# "#team cath> signed hello (signed)"
            concurrentlyN_
              [ alice <# "#team cath> signed hello (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> signed hello (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> signed hello (signed) [>>]"
              ]
            -- sender and recipient hold it signed
            cath #$> ("/_get chat #1 count=100 search=signed hello", chat, [(1, "signed hello (signed)")])
            dan #$> ("/_get chat #1 count=100 search=signed hello", chat, [(0, "signed hello (signed)")])

            -- editing a signed item reuses the signature
            cathMsgId <- lastItemId cath
            cath ##> ("/_update item #1 " <> cathMsgId <> " text signed hello edited")
            cath <# "#team [edited] signed hello edited (signed)"
            bob <# "#team cath> [edited] signed hello edited (signed)"
            concurrentlyN_
              [ alice <# "#team cath> [edited] signed hello edited (signed)",
                dan <# "#team cath> [edited] signed hello edited (signed)",
                eve <# "#team cath> [edited] signed hello edited (signed)"
              ]
            cath #$> ("/_get chat #1 count=100 search=signed hello edited", chat, [(1, "signed hello edited (signed)")])
            dan #$> ("/_get chat #1 count=100 search=signed hello edited", chat, [(0, "signed hello edited (signed)")])

            -- default send is unsigned, and holds no signature
            cath #> "#team plain hello"
            bob <# "#team cath> plain hello"
            concurrentlyN_
              [ alice <# "#team cath> plain hello [>>]",
                dan <# "#team cath> plain hello [>>]",
                eve <# "#team cath> plain hello [>>]"
              ]
            cath #$> ("/_get chat #1 count=100 search=plain hello", chat, [(1, "plain hello")])
            dan #$> ("/_get chat #1 count=100 search=plain hello", chat, [(0, "plain hello")])

testChannelSignedFile :: HasCallStack => TestParams -> IO ()
testChannelSignedFile ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> withXFTPServer $ do
            xftpCLI ["rand", "./tests/tmp/testfile", "1mb"] `shouldReturn` ["File created: ./tests/tmp/testfile"]
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]
            -- roster serves arrive as files that Postgres deletes without reusing the id (SQLite reuses
            -- it), so ids run higher here. cath's send and the subscribers' (dan, eve) receive both
            -- follow only their join roster (id 2). The relay (bob) also received the roster re-served
            -- on cath's promotion, so its file is id 3. The owner (alice) has no roster file, so id 1.
#if defined(dbPostgres)
            let fileId = 2 :: Int
                relayFileId = 3 :: Int
#else
            let fileId = 1 :: Int
                relayFileId = 1 :: Int
#endif

            -- cath's first (signed) message introduces her to dan/eve
            cath ##> "/_send #1 sign=on text hi"
            cath <# "#team hi (signed)"
            bob <# "#team cath> hi (signed)"
            concurrentlyN_
              [ alice <# "#team cath> hi (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> hi (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> hi (signed) [>>]"
              ]

            -- cath sends a signed file
            cath ##> "/_send #1 sign=on json [{\"filePath\": \"./tests/tmp/testfile\", \"msgContent\": {\"text\":\"signed file\",\"type\":\"file\"}}]"
            cath <# "#team signed file (signed)"
            cath <# "/f #team ./tests/tmp/testfile"
            cath <## ("use /fc " <> show fileId <> " to cancel sending")
            cath <## ("completed uploading file " <> show fileId <> " (testfile) for #team")

            bob <# "#team cath> signed file (signed)"
            bob <# "#team cath> sends file testfile (1.0 MiB / 1048576 bytes)"
            bob <## ("use /fr " <> show relayFileId <> " [<dir>/ | <path>] to receive it")

            concurrentlyN_
              [ do alice <# "#team cath> signed file (signed) [>>]"
                   alice <# "#team cath> sends file testfile (1.0 MiB / 1048576 bytes) [>>]"
                   alice <## "use /fr 1 [<dir>/ | <path>] to receive it [>>]",
                do dan <# "#team cath> signed file (signed) [>>]"
                   dan <# "#team cath> sends file testfile (1.0 MiB / 1048576 bytes) [>>]"
                   dan <## ("use /fr " <> show fileId <> " [<dir>/ | <path>] to receive it [>>]"),
                do eve <# "#team cath> signed file (signed) [>>]"
                   eve <# "#team cath> sends file testfile (1.0 MiB / 1048576 bytes) [>>]"
                   eve <## ("use /fr " <> show fileId <> " [<dir>/ | <path>] to receive it [>>]")
              ]

            -- dan downloads: the signed digest is verified and the file completes
            dan ##> ("/fr " <> show fileId <> " ./tests/tmp")
            dan
              <### [ ConsoleString ("saving file " <> show fileId <> " from cath to ./tests/tmp/testfile_1"),
                     ConsoleString ("started receiving file " <> show fileId <> " (testfile) from cath")
                   ]
            dan <## ("completed receiving file " <> show fileId <> " (testfile) from cath")
            src <- B.readFile "./tests/tmp/testfile"
            destDan <- B.readFile "./tests/tmp/testfile_1"
            destDan `shouldBe` src
            -- the signed digest was carried to dan and stored, so verification ran (not skipped) and passed
            digestCount <- withCCTransaction dan $ \db ->
              DB.query_ db "SELECT count(1) FROM files WHERE file_digest IS NOT NULL" :: IO [[Int]]
            digestCount `shouldBe` [[1]]

testChannelMemberUpdateEnforcement :: HasCallStack => TestParams -> IO ()
testChannelMemberUpdateEnforcement ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- cath posts a signed message; dan holds it verified
            cath ##> "/_send #1 sign=on text secret"
            cath <# "#team secret (signed)"
            bob <# "#team cath> secret (signed)"
            concurrentlyN_
              [ alice <# "#team cath> secret (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> secret (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> secret (signed) [>>]"
              ]
            dan #$> ("/_get chat #1 count=100 search=secret", chat, [(0, "secret (signed)")])

            -- the malicious relay forges an unsigned XMsgUpdate of cath's signed item to dan
            cathMemId <- memberIdByName bob "cath"
            sharedId <- itemSharedMsgId cath
            connId <- relayConnIdToMember bob "dan"
            ts <- getCurrentTime
            let ChatController {smpAgent = bobAgent} = chatController bob
                chatMsg = ChatMessage chatInitialVRange Nothing (XMsgUpdate sharedId (MCText "forged") M.empty Nothing Nothing Nothing Nothing)
                fwd = GrpMsgForward (FwdMember cathMemId "cath") ts
                body = encodeBinaryBatch [encodeFwdElement fwd (VMUnsigned chatMsg)]
            sent <- runExceptT $ sendMessages bobAgent [(connId, PQEncOff, MsgFlags False, vrValue body)]
            either (fail . show) (const $ pure ()) sent
            -- dan rejects the unsigned mutation of the held-signed item (RGEMsgBadSignature, stored not shown live),
            -- and the original signed content is not overwritten
            threadDelay 2000000
            -- (critical) the forged content did NOT overwrite the original signed item
            dan #$> ("/_get chat #1 count=100 search=secret", chat, [(0, "secret (signed)")])
            -- the rejection is recorded as a bad-signature item
            dan #$> ("/_get chat #1 count=100 search=bad signature", chat, [(0, "message rejected: bad signature")])

            -- a legitimate signed edit by cath is accepted
            cathMsgId <- lastItemId cath
            cath ##> ("/_update item #1 " <> cathMsgId <> " text secret edited")
            cath <# "#team [edited] secret edited (signed)"
            bob <# "#team cath> [edited] secret edited (signed)"
            concurrentlyN_
              [ alice <# "#team cath> [edited] secret edited (signed)",
                dan <# "#team cath> [edited] secret edited (signed)",
                eve <# "#team cath> [edited] secret edited (signed)"
              ]
            dan #$> ("/_get chat #1 count=100 search=secret edited", chat, [(0, "secret edited (signed)")])
            dan #$> ("/_get chat #1 count=100 search=bad signature", chat, [(0, "message rejected: bad signature")])

testChannelAsGroupSign :: HasCallStack => TestParams -> IO ()
testChannelAsGroupSign ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve

            -- owner posts as the channel, signed: verifiable AND displayed as the channel
            alice ##> "/_send #1(as_group=on) sign=on text signed channel post"
            alice <# "#team signed channel post (signed)"
            bob <# "#team> signed channel post (signed)"
            [cath, dan, eve] *<# "#team> signed channel post (signed) [>>]"
            alice #$> ("/_get chat #1 count=100 search=signed channel post", chat, [(1, "signed channel post (signed)")])
            cath #$> ("/_get chat #1 count=100 search=signed channel post", chat, [(0, "signed channel post (signed)")])

            -- owner posts as the channel, unsigned: anonymous (FwdChannel), no signature, still as the channel
            alice ##> "/_send #1(as_group=on) text plain channel post"
            alice <# "#team plain channel post"
            bob <# "#team> plain channel post"
            [cath, dan, eve] *<# "#team> plain channel post [>>]"
            alice #$> ("/_get chat #1 count=100 search=plain channel post", chat, [(1, "plain channel post")])
            cath #$> ("/_get chat #1 count=100 search=plain channel post", chat, [(0, "plain channel post")])

testChannelSignMessagesRequired :: HasCallStack => TestParams -> IO ()
testChannelSignMessagesRequired ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- owner requires signatures
            alice ##> "/set signatures #team on"
            alice <## "updated group preferences:"
            alice <## "Sign messages: on"
            concurrentlyN_
              [ do
                  bob <## "alice updated group #team: (signed)"
                  bob <## "updated group preferences:"
                  bob <## "Sign messages: on",
                do
                  cath <## "alice updated group #team: (signed)"
                  cath <## "updated group preferences:"
                  cath <## "Sign messages: on",
                do
                  dan <## "alice updated group #team: (signed)"
                  dan <## "updated group preferences:"
                  dan <## "Sign messages: on",
                do
                  eve <## "alice updated group #team: (signed)"
                  eve <## "updated group preferences:"
                  eve <## "Sign messages: on"
              ]

            -- owner posts as the channel, signed: verified for everyone, held signed in db
            alice ##> "/_send #1(as_group=on) sign=on text signed by owner"
            alice <# "#team signed by owner (signed)"
            bob <# "#team> signed by owner (signed)"
            [cath, dan, eve] *<# "#team> signed by owner (signed) [>>]"
            alice #$> ("/_get chat #1 count=100 search=signed by owner", chat, [(1, "signed by owner (signed)")])
            dan #$> ("/_get chat #1 count=100 search=signed by owner", chat, [(0, "signed by owner (signed)")])

            -- owner posts as the channel, unsigned: recipients warn (held in db), sender does not
            alice ##> "/_send #1(as_group=on) text plain from owner"
            alice <# "#team plain from owner"
            bob <# "#team> plain from owner (signature missing)"
            [cath, dan, eve] *<# "#team> plain from owner (signature missing) [>>]"
            alice #$> ("/_get chat #1 count=100 search=plain from owner", chat, [(1, "plain from owner")])
            dan #$> ("/_get chat #1 count=100 search=plain from owner", chat, [(0, "plain from owner (signature missing)")])

            -- promoted subscriber posts signed: verified for recipients (members are required to sign too)
            cath ##> "/_send #1 sign=on text signed by member"
            cath <# "#team signed by member (signed)"
            bob <# "#team cath> signed by member (signed)"
            concurrentlyN_
              [ alice <# "#team cath> signed by member (signed) [>>]",
                do
                  dan <### [EndsWith "updated to cath"]
                  dan <## "#team: bob introduced cath (Catherine) in the channel"
                  dan <# "#team cath> signed by member (signed) [>>]",
                do
                  eve <### [EndsWith "updated to cath"]
                  eve <## "#team: bob introduced cath (Catherine) in the channel"
                  eve <# "#team cath> signed by member (signed) [>>]"
              ]
            cath #$> ("/_get chat #1 count=100 search=signed by member", chat, [(1, "signed by member (signed)")])
            dan #$> ("/_get chat #1 count=100 search=signed by member", chat, [(0, "signed by member (signed)")])

            -- promoted subscriber posts unsigned: recipients warn (held in db), sender does not
            cath #> "#team plain from member"
            bob <# "#team cath> plain from member (signature missing)"
            concurrentlyN_
              [ alice <# "#team cath> plain from member (signature missing) [>>]",
                dan <# "#team cath> plain from member (signature missing) [>>]",
                eve <# "#team cath> plain from member (signature missing) [>>]"
              ]
            cath #$> ("/_get chat #1 count=100 search=plain from member", chat, [(1, "plain from member")])
            dan #$> ("/_get chat #1 count=100 search=plain from member", chat, [(0, "plain from member (signature missing)")])

testChannelAsGroupSpoof :: HasCallStack => TestParams -> IO ()
testChannelAsGroupSpoof ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- cath posts legitimately (introduces cath to dan as a member)
            cath #> "#team hi from cath"
            bob <# "#team cath> hi from cath"
            concurrentlyN_
              [ alice <# "#team cath> hi from cath [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> hi from cath [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> hi from cath [>>]"
              ]

            -- the relay forges an asGroup=True post attributed to non-owner cath; dan rejects (owner guard, §2)
            cathMemId <- memberIdByName bob "cath"
            connId <- relayConnIdToMember bob "dan"
            ts <- getCurrentTime
            let ChatController {smpAgent = bobAgent} = chatController bob
                container = (mcSimple (MCText "fake channel announcement")) {asGroup = Just True}
                chatMsg = ChatMessage chatInitialVRange Nothing (XMsgNew container)
                fwd = GrpMsgForward (FwdMember cathMemId "cath") ts
                body = encodeBinaryBatch [encodeFwdElement fwd (VMUnsigned chatMsg)]
            sent <- runExceptT $ sendMessages bobAgent [(connId, PQEncOff, MsgFlags False, vrValue body)]
            either (fail . show) (const $ pure ()) sent
            dan <##. "error: x.msg.new: member is not allowed to send as group"
            -- not rendered as the channel: dan still holds only the legitimate member message
            threadDelay 1000000
            dan #$> ("/_get chat #1 count=100 search=hi from cath", chat, [(0, "hi from cath")])

testChannelMemberSelfDeleteSign :: HasCallStack => TestParams -> IO ()
testChannelMemberSelfDeleteSign ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- member sends a signed message; dan holds it verified
            cath ##> "/_send #1 sign=on text signed hello"
            cath <# "#team signed hello (signed)"
            bob <# "#team cath> signed hello (signed)"
            concurrentlyN_
              [ alice <# "#team cath> signed hello (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> signed hello (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> signed hello (signed) [>>]"
              ]
            dan #$> ("/_get chat #1 count=100 search=signed hello", chat, [(0, "signed hello (signed)")])

            -- self-delete of the signed item: signed delete, dan (holding it signed) accepts
            cathMsgId <- lastItemId cath
            cath #$> ("/_delete item #1 " <> cathMsgId <> " broadcast", id, "message marked deleted")
            bob <# "#team cath> [marked deleted] signed hello (signed)"
            concurrentlyN_
              [ alice <# "#team cath> [marked deleted] signed hello (signed)",
                dan <# "#team cath> [marked deleted] signed hello (signed)",
                eve <# "#team cath> [marked deleted] signed hello (signed)"
              ]

            -- self-delete of an unsigned item: unsigned delete, accepted (no enforcement)
            cath #> "#team plain hello"
            bob <# "#team cath> plain hello"
            concurrentlyN_
              [ alice <# "#team cath> plain hello [>>]",
                dan <# "#team cath> plain hello [>>]",
                eve <# "#team cath> plain hello [>>]"
              ]
            cathMsgId2 <- lastItemId cath
            cath #$> ("/_delete item #1 " <> cathMsgId2 <> " broadcast", id, "message marked deleted")
            bob <# "#team cath> [marked deleted] plain hello"
            concurrentlyN_
              [ alice <# "#team cath> [marked deleted] plain hello",
                dan <# "#team cath> [marked deleted] plain hello",
                eve <# "#team cath> [marked deleted] plain hello"
              ]

testChannelMemberDeleteEnforcement :: HasCallStack => TestParams -> IO ()
testChannelMemberDeleteEnforcement ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- cath posts a signed message; dan holds it verified
            cath ##> "/_send #1 sign=on text secret"
            cath <# "#team secret (signed)"
            bob <# "#team cath> secret (signed)"
            concurrentlyN_
              [ alice <# "#team cath> secret (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> secret (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> secret (signed) [>>]"
              ]
            dan #$> ("/_get chat #1 count=100 search=secret", chat, [(0, "secret (signed)")])

            -- the relay forges an unsigned XMsgDel of cath's signed item to dan
            cathMemId <- memberIdByName bob "cath"
            sharedId <- itemSharedMsgId cath
            connId <- relayConnIdToMember bob "dan"
            ts <- getCurrentTime
            let ChatController {smpAgent = bobAgent} = chatController bob
                chatMsg = ChatMessage chatInitialVRange Nothing (XMsgDel sharedId Nothing Nothing False)
                fwd = GrpMsgForward (FwdMember cathMemId "cath") ts
                body = encodeBinaryBatch [encodeFwdElement fwd (VMUnsigned chatMsg)]
            sent <- runExceptT $ sendMessages bobAgent [(connId, PQEncOff, MsgFlags False, vrValue body)]
            either (fail . show) (const $ pure ()) sent
            -- dan rejects the unsigned delete of the held-signed item; item not deleted, rejection recorded
            threadDelay 2000000
            dan #$> ("/_get chat #1 count=100 search=secret", chat, [(0, "secret (signed)")])
            dan #$> ("/_get chat #1 count=100 search=bad signature", chat, [(0, "message rejected: bad signature")])

            -- a legitimate signed self-delete by cath is accepted
            cathMsgId <- lastItemId cath
            cath #$> ("/_delete item #1 " <> cathMsgId <> " broadcast", id, "message marked deleted")
            bob <# "#team cath> [marked deleted] secret (signed)"
            concurrentlyN_
              [ alice <# "#team cath> [marked deleted] secret (signed)",
                dan <# "#team cath> [marked deleted] secret (signed)",
                eve <# "#team cath> [marked deleted] secret (signed)"
              ]

testChannelModerationDeleteSign :: HasCallStack => TestParams -> IO ()
testChannelModerationDeleteSign ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan ->
          withNewTestChat ps "eve" eveProfile $ \eve -> do
            createChannel1Relay "team" alice bob cath dan eve
            promoteChannelMember "team" alice bob cath [dan, eve]

            -- cath posts a signed message; dan holds it verified
            cath ##> "/_send #1 sign=on text moderated post"
            cath <# "#team moderated post (signed)"
            bob <# "#team cath> moderated post (signed)"
            concurrentlyN_
              [ alice <# "#team cath> moderated post (signed) [>>]",
                do dan <### [EndsWith "updated to cath"]
                   dan <## "#team: bob introduced cath (Catherine) in the channel"
                   dan <# "#team cath> moderated post (signed) [>>]",
                do eve <### [EndsWith "updated to cath"]
                   eve <## "#team: bob introduced cath (Catherine) in the channel"
                   eve <# "#team cath> moderated post (signed) [>>]"
              ]
            dan #$> ("/_get chat #1 count=100 search=moderated post", chat, [(0, "moderated post (signed)")])

            -- owner moderation-deletes cath's signed post; the always-signed delete is accepted by dan (holding it signed)
            -- resolve alice's item id by text (not lastItemId) so a racing trailing event can't select the wrong item
            catItemIdOnAlice <- itemIdByText alice "moderated post"
            alice ##> ("/_delete member item #1 " <> catItemIdOnAlice)
            alice <## "message marked deleted by you"
            concurrentlyN_
              [ bob <# "#team cath> [marked deleted by alice] moderated post (signed)",
                cath <# "#team cath> [marked deleted by alice] moderated post (signed)",
                dan <# "#team cath> [marked deleted by alice] moderated post (signed)",
                eve <# "#team cath> [marked deleted by alice] moderated post (signed)"
              ]
  where
    itemIdByText :: TestCC -> T.Text -> IO String
    itemIdByText cc t = do
      rows <- withCCTransaction cc $ \db ->
        DB.query db "SELECT chat_item_id FROM chat_items WHERE item_text LIKE '%' || ? || '%' ORDER BY chat_item_id DESC LIMIT 1" (Only t) :: IO [Only Int64]
      case rows of
        (Only i : _) -> pure (show i)
        _ -> fail $ "no item with text " <> T.unpack t

testGroupLinkContentFilter :: HasCallStack => TestParams -> IO ()
testGroupLinkContentFilter =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath

      let linkPreview = "{\"msgContent\": {\"type\": \"link\", \"text\": \"https://simplex.chat\", \"preview\": {\"uri\": \"https://simplex.chat\", \"title\": \"SimpleX Chat\", \"description\": \"SimpleX Chat\", \"image\": \"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      alice ##> ("/_send #1 json [" <> linkPreview <> "]")
      alice <# "#team https://simplex.chat"
      concurrently_
        (bob <# "#team alice> https://simplex.chat")
        (cath <# "#team alice> https://simplex.chat")

      threadDelay 1000000

      bob #> "#team check out https://example.com"
      concurrently_
        (alice <# "#team bob> check out https://example.com")
        (cath <# "#team bob> check out https://example.com")

      cath #> "#team hello, no links here"
      concurrently_
        (alice <# "#team cath> hello, no links here")
        (bob <# "#team cath> hello, no links here")

      alice ##> "/_get content types #1"
      alice <## "Chat content types: link, text"
      alice #$> ("/_get chat #1 content=link count=100", chat, [(1, "https://simplex.chat"), (0, "check out https://example.com")])

      bob ##> "/_get content types #1"
      bob <## "Chat content types: link, text"
      bob #$> ("/_get chat #1 content=link count=100", chat, [(0, "https://simplex.chat"), (1, "check out https://example.com")])

      cath ##> "/_get content types #1"
      cath <## "Chat content types: link, text"
      cath #$> ("/_get chat #1 content=link count=100", chat, [(0, "https://simplex.chat"), (0, "check out https://example.com")])
