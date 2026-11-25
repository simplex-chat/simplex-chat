package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.hasClickAction
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.isRoot
import androidx.compose.ui.test.isToggleable
import androidx.compose.ui.test.longClick
import androidx.compose.ui.test.onLast
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performMouseInput
import androidx.compose.ui.test.performScrollTo
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.rightClick
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.ComposeContextItem
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.ComposeView
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.chat.acceptMemberDialog
import chat.simplex.common.views.chat.archiveContact
import chat.simplex.common.views.chat.deleteAndBanContactDialog
import chat.simplex.common.views.chat.deleteContactDialog
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.MarkedDeletedItemView
import chat.simplex.common.views.chat.item.markedDeletedText
import chat.simplex.common.views.chatlist.ArchiveContactAction
import chat.simplex.common.views.chatlist.DeleteAndBanAction
import chat.simplex.common.views.chatlist.TagListAction
import chat.simplex.common.views.chatlist.TagListView
import chat.simplex.common.views.chatlist.acceptGroupInvitationAlertDialog
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.contacts.ContactListNavLinkView
import chat.simplex.common.views.createProfileOnboarding
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AppBarHandler
import chat.simplex.common.views.helpers.AppBarHeight
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.ModalData
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.rememberAppBarHandler
import chat.simplex.common.views.newchat.LinkNoticeTarget
import chat.simplex.common.views.newchat.showLinkNoticeAlert
import chat.simplex.common.views.usersettings.PrivacySettingsView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.datetime.Instant
import java.time.format.FormatStyle
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class ModerationTest : RecordedApiTest() {
  private val contactDeletion = ChatController.appPrefs.privacyContactDeletion.get()
  private val showDeleteContactNotice = ChatController.appPrefs.showDeleteContactNotice.get()
  private val showDeleteConversationNotice = ChatController.appPrefs.showDeleteConversationNotice.get()

  @AfterTest
  fun restorePrefs() {
    ChatController.appPrefs.privacyContactDeletion.set(contactDeletion)
    ChatController.appPrefs.showDeleteContactNotice.set(showDeleteContactNotice)
    ChatController.appPrefs.showDeleteConversationNotice.set(showDeleteConversationNotice)
    ChatModel.groupMembers.value = emptyList()
    ChatModel.groupMembersIndexes.value = emptyMap()
    ChatModel.userAddress.value = null
  }

  @Test
  fun contactNoticePlan() = recordedAlerts("bans/contact-notices") {
    var cleanups = 0
    connect("ALICE_LINK", cleanup = { cleanups++ })
    assertTexts("Not allowed", "Blocked by this contact until $UNTIL.", "OK", "How it works")
    press("OK")
    assertTexts()
    assertEquals(1, cleanups)
  }

  @Test
  fun contactNoticeForeverPlan() = recordedAlerts("bans/contact-notices") {
    connect("CATH_LINK")
    assertTexts("Not allowed", "Blocked by this contact.", "OK", "How it works")
  }

  @Test
  fun contactNoticeConnect() = recordedAlerts("bans/contact-notices") {
    runBlocking { ChatController.apiConnect(null, incognito = false, CreatedConnLink("ALICE_FULL_LINK", "ALICE_LINK")) }
    assertTexts("Not allowed", "Blocked by this contact until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun preparedContactNotice() = recordedAlerts("bans/connect-contact-notice") {
    runBlocking { ChatController.apiConnectPreparedContact(null, 5, incognito = false, msg = null) }
    assertTexts("Not allowed", "Blocked by this contact until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun contactCardNotice() = recordedAlerts("bans/connect-contact-notice") {
    runBlocking { ChatController.apiConnectContactViaAddress(null, incognito = false, contactId = 6) }
    assertTexts("Not allowed", "Blocked by this contact until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun groupNoticePlan() = recordedAlerts("bans/plan-group-notice") {
    connect("GROUP_LINK")
    assertTexts("Not allowed", "Blocked by the admins of this group until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun channelNoticePlan() = recordedAlerts("bans/plan-channel-notice") {
    connect("CHANNEL_LINK")
    assertTexts("Not allowed", "Blocked by the owners of this channel until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun channelNameNoticePlan() = recordedAlerts("names/plan-channel-notice") {
    connect("team.simplex")
    assertTexts("Not allowed", "Blocked by the owners of this channel until $UNTIL.", "OK", "Connect to @team.simplex", "How it works")
    press("Connect to @team.simplex")
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("alice") }
  }

  @Test
  fun businessNoticePlan() = recordedAlerts("bans/plan-business-notice") {
    connect("BUSINESS_LINK")
    assertTexts("Not allowed", "Blocked by this business until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun preparedGroupNotice() = recordedAlerts("bans/connect-group-notice") {
    connectPreparedGroup("/_get chat #2 count=0")
    assertTexts("Not allowed", "Blocked by the admins of this group until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun preparedChannelNotice() = recordedAlerts("bans/connect-channel-notice") {
    connectPreparedGroup("/_get chat #1 count=0")
    assertTexts("Not allowed", "Blocked by the owners of this channel until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun preparedBusinessNotice() = recordedAlerts("bans/connect-business-notice") {
    connectPreparedGroup("/_get chat #2 count=0")
    assertTexts("Not allowed", "Blocked by this business until $UNTIL.", "OK", "How it works")
  }

  @Test
  fun blockMember() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "bob")))
    assertTexts(
      "Block member for all?", "bob won't be notified, and if removed, can't rejoin while blocked.",
      "Block", "Block and remove", "Cancel"
    )
    press("Block")
    awaitMember("bob") { it.blockedByAdmin }
  }

  @Test
  fun unblockMember() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    val bob = member(blockedMembers("/_block #1 2 blocked=on"), "bob")
    replaceMembers(listOf(bob))
    unblockForAllAlert(null, groupInfo, bob)
    assertTexts("Unblock member for all?", "Messages from bob will be shown for all members.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("bob") { !it.blockedByAdmin }
  }

  @Test
  fun unblockMembers() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    replaceMembers(blockedMembers("/_block #1 3,4 blocked=on"))
    unblockForAllAlert(null, groupInfo, listOf(member(members, "cath").groupMemberId, member(members, "dan").groupMemberId))
    assertTexts("Unblock members for all?", "Messages from these members will be shown for all members.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("cath") { !it.blockedByAdmin }
    awaitMember("dan") { !it.blockedByAdmin }
  }

  @Test
  fun memberInfoBlocked() = recorded("bans/block-members") {
    val (groupInfo, _) = openGroup()
    var unblocked = false
    showMemberInfo(groupInfo, member(blockedMembers("/_block #1 2 blocked=on"), "bob")) { unblocked = true }
    onNodeWithText("Unblock for all").performScrollTo().performClick()
    assertTrue(unblocked)
  }

  @Test
  fun blockMembers() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    var selectionCleared = false
    banMembersDialog(null, groupInfo, listOf(member(members, "bob"), member(members, "cath"))) { selectionCleared = true }
    assertTexts(
      "Block members for all?", "Members won't be notified, and if removed, can't rejoin while blocked.",
      "Block", "Block and remove", "Cancel"
    )
    press("Block")
    awaitMember("cath") { it.blockedByAdmin }
    waitUntil(timeoutMillis = 5_000) { selectionCleared }
  }

  @Test
  fun blockAndRemoveMember() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    var closed = false
    banMembersDialog(null, groupInfo, listOf(member(members, "bob")), close = { closed = true })
    press("Block and remove")
    assertTexts(
      "Remove member?", "bob won't be notified, and can't rejoin while blocked.",
      "Block for", "1 day", "Remove", "Remove and delete messages", "Cancel"
    )
    press("Remove")
    awaitMember("bob") { it.memberStatus == GroupMemberStatus.MemRemoved }
    waitUntil(timeoutMillis = 5_000) { closed }
  }

  @Test
  fun blockAndRemoveMembersCancelled() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "bob"), member(members, "cath")))
    press("Block and remove")
    assertTexts(
      "Remove members?", "Members won't be notified, and can't rejoin while blocked.",
      "Block for", "1 day", "Remove", "Remove and delete messages", "Cancel"
    )
    press("Cancel")
    assertTexts()
    assertFalse(member(ChatModel.groupMembers.value, "bob").blockedByAdmin)
  }

  @Test
  fun removeBlockedMemberForever() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    val cath = member(blockedMembers("/_block #1 2,3 blocked=on"), "cath")
    removeMemberAlert(null, groupInfo, cath)
    assertTexts(
      "Remove member?", "cath won't be notified, and can't rejoin while blocked.",
      "Block for", "1 day", "Remove", "Remove and delete messages", "Cancel"
    )
    selectDuration("Forever")
    press("Remove and delete messages")
    assertTexts("Remove forever?", "cath can't rejoin via the group link again, except from another app.", "Type yes", "Remove", "Cancel")
    press("Remove")
    assertTexts("Remove forever?", "cath can't rejoin via the group link again, except from another app.", "Type yes", "Remove", "Cancel")
    onNode(hasSetTextAction()).performTextInput("yes")
    press("Remove")
    awaitMember("cath") { it.memberStatus == GroupMemberStatus.MemRemoved }
  }

  @Test
  fun removeBlockedMembers() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    removeMembersAlert(null, groupInfo, blockedMembers("/_block #1 2,3 blocked=on"))
    assertTexts(
      "Remove members?", "Members won't be notified, and can't rejoin while blocked.",
      "Block for", "1 day", "Remove", "Remove and delete messages", "Cancel"
    )
    press("Remove")
    assertRequested("/_remove #1 2,3 messages=off notice={\"ttl\":86400}")
  }

  @Test
  fun removeMemberNotBlocked() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    removeMemberAlert(null, groupInfo, member(members, "dan"))
    assertTexts("Remove member?", "Member will be removed from group - this cannot be undone!", "Remove", "Remove and delete messages", "Cancel")
    press("Remove")
    awaitMember("dan") { it.memberStatus == GroupMemberStatus.MemRemoved }
  }

  @Test
  fun hideMemberMessages() = recordedAlerts("bans/hide-messages") {
    val (groupInfo, members) = openGroup()
    blockMemberAlert(null, groupInfo, member(members, "cath"))
    assertTexts("Hide messages?", "All new messages from cath will be hidden!", "Cancel", "Hide")
    press("Hide")
    awaitMember("cath") { !it.memberSettings.showMessages }
  }

  @Test
  fun showMemberMessages() = recordedAlerts("bans/hide-messages") {
    val (groupInfo, members) = openGroup()
    val cath = member(members, "cath").let { it.copy(memberSettings = it.memberSettings.copy(showMessages = false)) }
    replaceMembers(listOf(cath))
    unblockMemberAlert(null, groupInfo, cath)
    assertTexts("Show messages?", "Messages from cath will be shown!", "Cancel", "Show")
    press("Show")
    awaitMember("cath") { it.memberSettings.showMessages }
  }

  @Test
  fun blockMembersOfPreviousVersion() = recordedAlerts("bans/previous-version") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "cath"), member(members, "dan")))
    assertTexts(
      "Block members for all?", "Members won't be notified, and their messages will be hidden for all.",
      "� 1 member with the previous version: block only", "Block", "Cancel"
    )
    press("Block")
    awaitMember("dan") { it.blockedByAdmin }
  }

  @Test
  fun moderatorBlocksMembersOfPreviousVersion() = recordedAlerts("bans/previous-version-moderator") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "cath"), member(members, "dan")))
    assertTexts(
      "Block members for all?", "Members won't be notified, and their messages will be hidden for all.",
      "Block", "Cancel"
    )
  }

  @Test
  fun removeBlockedMembersOfPreviousVersion() = recordedAlerts("bans/previous-version") {
    val (groupInfo, _) = openGroup()
    val blocked = blockedMembers("/_block #1 3,4 blocked=on")
    removeMembersAlert(null, groupInfo, blocked)
    assertTexts(
      "Remove members?", "Members will be removed from group - this cannot be undone!",
      "� 1 member with the previous version: cannot block", "Remove", "Remove and delete messages", "Cancel"
    )
    press("Remove")
    awaitMember("dan") { it.memberStatus == GroupMemberStatus.MemRemoved }
  }

  @Test
  fun removeBlockedMemberOfPreviousVersion() = recordedAlerts("bans/previous-version") {
    val (groupInfo, _) = openGroup()
    removeMemberAlert(null, groupInfo, member(blockedMembers("/_block #1 3,4 blocked=on"), "cath"))
    assertTexts(
      "Remove member?", "Member will be removed from group - this cannot be undone!",
      "� 1 member with the previous version: cannot block", "Remove", "Remove and delete messages", "Cancel"
    )
  }

  @Test
  fun blockPendingMember() = recordedAlerts("bans/pending-member") {
    val (groupInfo, members) = openGroup()
    val bob = member(members, "bob")
    assertTrue(bob.canBlockForAll(groupInfo))
    banMembersDialog(null, groupInfo, listOf(bob))
    press("Block")
    awaitMember("bob") { it.blockedByAdmin }
  }

  @Test
  fun acceptBlockedPendingMember() = recordedAlerts("bans/pending-member") {
    val (groupInfo, _) = openGroup()
    val bob = member(blockedMembers("/_block #1 2 blocked=on"), "bob")
    replaceMembers(listOf(bob))
    acceptMemberDialog(null, groupInfo, bob)
    assertTexts("Accept member", "Member will be unblocked for all and will join the group, accept member?", "Accept as member", "Accept as observer", "Cancel")
    press("Accept as member")
    awaitMember("bob") { !it.memberPending && !it.blockedByAdmin }
  }

  @Test
  fun promotionOfBlockedMemberAsksToUnblock() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    val bob = member(blockedMembers("/_block #1 2 blocked=on"), "bob")
    replaceMembers(listOf(bob))
    unblockBeforeRoleChangeAlert(null, groupInfo, bob)
    assertTexts("Unblock first", "Unblock bob for all before changing the role to moderator or above.", "Cancel", "Unblock")
    press("Unblock")
    awaitMember("bob") { !it.blockedByAdmin }
  }

  @Test
  fun acceptPendingMember() = recordedAlerts("bans/pending-member") {
    val (groupInfo, members) = openGroup()
    acceptMemberDialog(null, groupInfo, member(members, "bob"))
    assertTexts("Accept member", "Member will join the group, accept member?", "Accept as member", "Accept as observer", "Cancel")
    press("Accept as member")
    awaitMember("bob") { !it.memberPending }
  }

  @Test
  fun bulkBlockDisabledForPendingAmongSeveral() = recorded("bans/pending-member") {
    val (groupInfo, members) = openGroup()
    val bob = member(members, "bob")
    val cath = bob.copy(groupMemberId = 3, memberRole = GroupMemberRole.Member, memberStatus = GroupMemberStatus.MemConnected)
    val selected = mutableStateOf<Set<Long>?>(setOf(bob.groupMemberId, cath.groupMemberId))
    val delete = mutableStateOf(false)
    val block = mutableStateOf(false)
    val unblock = mutableStateOf(false)
    val toMember = mutableStateOf(false)
    val toObserver = mutableStateOf(false)
    val banAndRemove = mutableStateOf(false)
    recheckItems(groupInfo, selected, listOf(bob, cath), delete, block, unblock, banAndRemove, toMember, toObserver)
    assertFalse(block.value)
    assertFalse(unblock.value)
    selected.value = setOf(bob.groupMemberId)
    recheckItems(groupInfo, selected, listOf(bob, cath), delete, block, unblock, banAndRemove, toMember, toObserver)
    assertTrue(block.value)
  }

  @Test
  fun banAndRemoveClockFollowsSelection() = recorded("bans/previous-version") {
    val (groupInfo, members) = openGroup()
    val blocked = blockedMembers("/_block #1 3,4 blocked=on")
    val (current, previous) = blocked.partition { it.supportsLinkNotice }
    assertEquals(1, current.size)
    assertEquals(1, previous.size)
    val selected = mutableStateOf<Set<Long>?>(setOf(current[0].groupMemberId))
    val delete = mutableStateOf(false)
    val block = mutableStateOf(false)
    val unblock = mutableStateOf(false)
    val banAndRemove = mutableStateOf(false)
    val toMember = mutableStateOf(false)
    val toObserver = mutableStateOf(false)
    recheckItems(groupInfo, selected, members, delete, block, unblock, banAndRemove, toMember, toObserver)
    assertFalse(banAndRemove.value)
    selected.value = setOf(current[0].groupMemberId)
    recheckItems(groupInfo, selected, blocked, delete, block, unblock, banAndRemove, toMember, toObserver)
    assertTrue(banAndRemove.value)
    selected.value = setOf(current[0].groupMemberId, previous[0].groupMemberId)
    recheckItems(groupInfo, selected, blocked, delete, block, unblock, banAndRemove, toMember, toObserver)
    assertFalse(banAndRemove.value)
    assertTrue(delete.value)
    assertTrue(unblock.value)
  }

  @Test
  fun blockSubscriber() = recordedAlerts("bans/channel") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "cath")))
    assertTexts(
      "Block subscriber for all?", "cath won't be notified, and if removed, can't rejoin while blocked.",
      "Block", "Block and remove", "Cancel"
    )
    press("Block")
    awaitMember("cath") { it.blockedByAdmin }
  }

  @Test
  fun unblockSubscriber() = recordedAlerts("bans/channel") {
    val (groupInfo, _) = openGroup()
    val cath = member(blockedMembers("/_block #1 3 blocked=on"), "cath")
    replaceMembers(listOf(cath))
    unblockForAllAlert(null, groupInfo, cath)
    assertTexts("Unblock subscriber for all?", "Messages from cath will be shown for all subscribers.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("cath") { !it.blockedByAdmin }
  }

  @Test
  fun removeSubscriberForever() = recordedAlerts("bans/channel") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "dan")))
    press("Block and remove")
    selectDuration("Forever")
    press("Remove")
    assertTexts("Remove forever?", "dan can't rejoin via the channel link again, except from another app.", "Type yes", "Remove", "Cancel")
    onNode(hasSetTextAction()).performTextInput("yes")
    press("Remove")
    awaitMember("dan") { it.memberStatus == GroupMemberStatus.MemRemoved }
  }

  @Test
  fun deleteChannelKeepingConversation() = recordedAlerts("bans/channel") {
    val (groupInfo, _) = openGroup()
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    assertTexts("Delete channel?", "team\n\nChannel will be deleted for all subscribers - this cannot be undone!", "Delete", "Cancel")
    press("Delete")
    assertTexts("Delete channel?", "Keep messages", "Delete", "Cancel")
    onNode(isToggleable()).performClick()
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { (chatInfo(groupInfo.id) as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemGroupDeleted }
  }

  @Test
  fun deleteOwnGroup() = recordedAlerts("bans/group-deletion") {
    val (groupInfo, _) = openGroup()
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    assertTexts("Delete group?", "team\n\nGroup will be deleted for all members - this cannot be undone!", "Delete", "Cancel")
    press("Delete")
    assertTexts("Delete group?", "Keep conversation", "Delete", "Cancel")
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(groupInfo.id) }
  }

  @Test
  fun deleteGroupAsMember() = recordedAlerts("bans/group-deletion-member") {
    val (groupInfo, _) = openGroup()
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    assertTexts("Delete group?", "team\n\nGroup will be deleted for you - this cannot be undone!", "Cancel", "Delete")
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(groupInfo.id) }
  }

  @Test
  fun deleteBusinessChatKeepingConversation() = recordedAlerts("bans/business") {
    loadAddress()
    val (groupInfo, _) = openGroup("/_members #2")
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    assertTexts("Delete chat?", "cath\n\nChat will be deleted for all members - this cannot be undone!", "Delete", "Cancel")
    press("Delete")
    assertTexts("Delete chat?", "cath will be notified, and can reconnect via your address.", "Keep conversation", "Delete", "Cancel")
    onNode(isToggleable()).performClick()
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { (chatInfo(groupInfo.id) as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemGroupDeleted }
  }

  @Test
  fun deleteAndBlockCustomer() = recordedAlerts("bans/business") {
    val (groupInfo, _) = openGroup()
    deleteAndBanCustomer(groupChat(groupInfo), groupInfo)
    assertTexts(
      "Delete and block?", "Chat with bob will be deleted for all members - this cannot be undone!",
      "Block for", "1 day", "Delete and block", "Cancel"
    )
    press("Delete and block")
    assertTexts("Delete and block?", "bob won't be notified, and can't connect to you for 1 day.", "Keep conversation", "Delete and block", "Cancel")
    press("Delete and block")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(groupInfo.id) }
  }

  @Test
  fun cantBlockCustomerWhoLeft() = recordedAlerts("bans/business") {
    val (groupInfo, _) = openGroup("/_members #3")
    deleteAndBanCustomer(groupChat(groupInfo), groupInfo)
    assertTexts("Can't block", "dan left this chat or uses an older app version.", "OK")
    press("OK")
    assertTexts()
  }

  @Test
  fun deleteContactNotifying() = recordedAlerts("bans/contact-deletion") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Notify)
    loadAddress()
    val chat = openContact("/_get chat @5 count=0")
    deleteContactDialog(chat, ChatModel)
    assertTexts("Delete contact?", "Contact with eve will be deleted - this cannot be undone!", "Delete", "Cancel")
    press("Delete")
    assertTexts("Delete contact?", "eve will be notified, and can reconnect via your address.", "Keep conversation", "Delete", "Cancel")
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(chat.id) }
  }

  @Test
  fun deleteContactSilentlyKeepingConversation() = recordedAlerts("bans/contact-deletion") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Ask)
    loadAddress()
    val chat = openContact("/_get chat @5 count=0")
    deleteContactDialog(chat, ChatModel)
    assertTexts("Delete contact?", "Contact with eve will be deleted - this cannot be undone!", "Delete and notify", "Delete silently", "Cancel")
    press("Delete silently")
    assertTexts("Delete contact?", "eve won't be notified, and can reconnect via your address.", "Keep conversation", "Delete", "Cancel")
    onNode(isToggleable()).performClick()
    press("Delete")
    assertRequested("/_delete @5 entity notify=off")
  }

  @Test
  fun deleteContactWithoutAddress() = recordedAlerts("bans/contact-notices") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Ask)
    val chat = openContact("/_get chat @2 count=0")
    deleteContactDialog(chat, ChatModel)
    press("Delete silently")
    assertTexts("Delete contact?", "alice won't be notified.", "Keep conversation", "Delete", "Cancel")
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(chat.id) }
  }

  @Test
  fun archiveContactFails() = recordedAlerts("bans/contact-deletion") {
    val chat = openContact("/_get chat @3 count=0")
    archiveContact(chat, ChatModel)
    assertRequested("/_delete @3 archive")
    assertEquals("Error archiving contact", shownTexts().first())
  }

  @Test
  fun customerLoadFailureShowsError() = recordedAlerts("bans/business") {
    val (groupInfo, _) = openGroup()
    val unknown = groupInfo.copy(groupId = 9)
    deleteAndBanCustomer(groupChat(unknown), unknown)
    assertRequested("/_members #9")
    assertTexts("Error loading members", "OK")
  }

  @Test
  fun archiveContact() = recordedAlerts("bans/contact-deletion") {
    ChatController.appPrefs.showDeleteConversationNotice.set(true)
    val chat = openContact("/_get chat @2 count=0")
    archiveContact(chat, ChatModel)
    assertTexts("Contact archived!", "You can send messages to bob from Archived contacts.", "Don't show again", "OK")
    assertTrue((chatInfo(chat.id) as ChatInfo.Direct).contact.chatDeleted)
  }

  @Test
  fun deleteArchivedContactKeepingConversation() = recordedAlerts("bans/contact-deletion") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Notify)
    ChatController.appPrefs.showDeleteContactNotice.set(true)
    val archived = (recordedResult("/_delete @2 archive") as CR.ContactDeleted).contact
    val chat = openChat(Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(archived), chatItems = emptyList()))
    deleteContactDialog(chat, ChatModel)
    press("Delete")
    onNode(isToggleable()).performClick()
    press("Delete")
    assertTexts("Contact deleted!", "You can still view conversation with bob in Archived contacts.", "Don't show again", "OK")
  }

  @Test
  fun deleteAndBlockContactForDay() = recordedAlerts("bans/contact-deletion") {
    ChatController.appPrefs.showDeleteContactNotice.set(true)
    val chat = openContact("/_get chat @3 count=0")
    val contact = (chat.chatInfo as ChatInfo.Direct).contact
    assertTrue(contact.canDeleteAndBan)
    deleteAndBanContactDialog(chat, contact, ChatModel)
    assertTexts(
      "Delete and block?", "Contact with cath will be deleted - this cannot be undone!",
      "Block for", "1 day", "Delete and block", "Cancel"
    )
    press("Delete and block")
    assertTexts("Delete and block?", "cath won't be notified, and can't connect to you for 1 day.", "Keep conversation", "Delete and block", "Cancel")
    onNode(isToggleable()).performClick()
    press("Delete and block")
    assertTexts("Contact deleted!", "You can still view conversation with cath in the list of chats.", "Don't show again", "OK")
  }

  @Test
  fun deleteAndBlockContactForWeek() = recordedAlerts("bans/contact-deletion") {
    val chat = openContact("/_get chat @3 count=0")
    deleteAndBanContactDialog(chat, (chat.chatInfo as ChatInfo.Direct).contact, ChatModel)
    selectDuration("1 week")
    press("Delete and block")
    assertTexts("Delete and block?", "cath won't be notified, and can't connect to you for 1 week.", "Keep conversation", "Delete and block", "Cancel")
    press("Delete and block")
    assertRequested("/_delete @3 full notify=off notice={\"ttl\":604800}")
  }

  @Test
  fun deleteAndBlockContactForever() = recordedAlerts("bans/contact-deletion") {
    val chat = openContact("/_get chat @4 count=0")
    deleteAndBanContactDialog(chat, (chat.chatInfo as ChatInfo.Direct).contact, ChatModel)
    selectDuration("Forever")
    press("Delete and block")
    assertTexts(
      "Delete and block forever?", "dan won't be notified, and can't connect via your address again, except from another app.",
      "Keep conversation", "Type yes", "Delete and block", "Cancel"
    )
    onNode(hasSetTextAction()).performTextInput("yes")
    press("Delete and block")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(chat.id) }
  }

  @Test
  fun contactNotViaAddressCantBeBlocked() = recorded("bans/contact-deletion") {
    val contact = (openContact("/_get chat @5 count=0").chatInfo as ChatInfo.Direct).contact
    assertFalse(contact.canDeleteAndBan)
  }

  @Test
  fun deleteContactThatDeletedYou() = recordedAlerts("bans/contact-notices") {
    val chat = openContact("/_get chat @4 count=0")
    deleteContactDialog(chat, ChatModel)
    assertTexts("Confirm contact deletion?", "dan\n\nContact will be deleted - this cannot be undone!", "Confirm", "Cancel")
    press("Confirm")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(chat.id) }
  }

  @Test
  fun favoritesRow() = recorded("bans/contact-deletion") {
    val chat = openContact("/_get chat @2 count=0")
    var closed = false
    showView { TagListView(rhId = null, chat = chat, close = { closed = true }, reorderMode = false) }
    press("Favorites")
    waitUntil(timeoutMillis = 5_000) { chatInfo(chat.id).chatSettings?.favorite == true }
    assertTrue(closed)
  }

  @Test
  fun contactDeletionSetting() = recorded("bans/contact-deletion") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Ask)
    showView { PrivacySettingsView(ChatModel, showSettingsModal = { {} }, setPerformLA = {}) }
    onNodeWithText("Ask").performScrollTo().performClick()
    press("Notify")
    assertEquals(ContactDeletion.Notify, ChatController.appPrefs.privacyContactDeletion.get())
  }

  @Test
  fun reportReasonIsContent() {
    assertEquals("/_report #1 2 reason=content spam", CC.ApiReportMessage(1, 2, ReportReason.Content, "spam").cmdString)
    assertEquals("Inappropriate content", ReportReason.Content.text)
    assertIs<ReportReason.Content>(json.decodeFromString(ReportReasonSerializer, "\"illegal\""))
    assertIs<ReportReason.Content>(json.decodeFromString(ReportReasonSerializer, "\"content\""))
  }

  @Test
  fun composeShowsContentReportHeader() = recorded("bans/contact-deletion") {
    chatParseMarkdown = { _ -> "{}" }
    val chat = openContact("/_get chat @2 count=0")
    val reported = ComposeContextItem.ReportedItem(ChatItem.getSampleData(dir = CIDirection.DirectRcv()), ReportReason.Content)
    showView { ComposeViewFor(chat, ComposeState(contextItem = reported, useLinkPreviews = false)) }
    onNodeWithText("Report content: only group moderators will see it.").assertExists()
  }

  @Test
  fun composeJoinsPreparedGroup() = recorded("bans/connect-group-notice") {
    chatParseMarkdown = { _ -> "{}" }
    val chat = (recordedResult("/_get chat #2 count=0") as CR.ApiChat).chat
    openChat(chat)
    setContent {
      SimpleXTheme {
        ComposeViewFor(chat, ComposeState(useLinkPreviews = false))
        AlertManager.shared.showInView()
        AlertManager.privacySensitive.showInView()
      }
    }
    press("Join group")
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Not allowed") }
  }

  @Test
  fun onboardingProfileNotifiesOnContactDeletion() = recorded("bans/contact-deletion") {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Ask)
    createProfileOnboarding(ChatModel, "frank") {}
    waitUntil(timeoutMillis = 5_000) { ChatController.appPrefs.privacyContactDeletion.get() == ContactDeletion.Notify }
  }

  @Test
  fun localTimestampDefaultsToMediumTime() {
    val t = Instant.parse("2099-01-01T12:00:00Z")
    assertEquals(localTimestamp(t, FormatStyle.MEDIUM), localTimestamp(t))
  }

  @Test
  fun contactDeletionSettingIsExportedAndImported() {
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Notify)
    assertEquals(ContactDeletion.Notify, AppSettings.current.prepareForExport().privacyContactDeletion)
    ChatController.appPrefs.privacyContactDeletion.set(ContactDeletion.Ask)
    assertNull(AppSettings.current.prepareForExport().privacyContactDeletion)
    AppSettings(privacyContactDeletion = ContactDeletion.Notify).importIntoApp()
    assertEquals(ContactDeletion.Notify, ChatController.appPrefs.privacyContactDeletion.get())
  }

  @Test
  fun connectViaAddressFailureShowsError() = recordedAlerts("bans/connect-contact-notice") {
    runBlocking { ChatController.apiConnectContactViaAddress(null, incognito = false, contactId = 9) }
    assertRequested { it.startsWith("/_connect contact 1 incognito=off 9") }
    assertEquals("Connection error", shownTexts().first())
  }

  @Test
  fun deleteConnectionFailureShowsError() = recordedAlerts("bans/contact-deletion") {
    runBlocking { ChatController.apiDeleteChat(null, ChatType.ContactConnection, 9) }
    assertRequested { it.startsWith("/_delete :9") }
    assertEquals("Error deleting pending contact connection", shownTexts().first())
  }

  @Test
  fun deleteGroupFailureShowsError() = recordedAlerts("bans/group-deletion") {
    val (groupInfo, _) = openGroup()
    deleteGroupChat(null, groupInfo.copy(groupId = 9), ChatDeleteMode.Full(notify = true), notice = null, close = null)
    assertRequested("/_delete #9 full notify=on")
    assertEquals("Error deleting group", shownTexts().first())
  }

  @Test
  fun deleteOwnGroupCancelled() = recordedAlerts("bans/group-deletion") {
    val (groupInfo, _) = openGroup()
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    press("Cancel")
    assertTexts()
  }

  @Test
  fun deleteInvitedGroup() = recordedAlerts("bans/group-deletion-member") {
    val (groupInfo, _) = openGroup()
    acceptGroupInvitationAlertDialog(null, groupInfo, ChatModel)
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { !chatIds().contains(groupInfo.id) }
  }

  @Test
  fun acceptPendingMemberAsObserver() = recordedAlerts("bans/pending-member") {
    val (groupInfo, members) = openGroup()
    acceptMemberDialog(null, groupInfo, member(members, "bob"))
    press("Accept as observer")
    assertRequested("/_accept member #1 2 observer")
  }

  @Test
  fun removeBlockedSubscriberOfPreviousVersion() = recordedAlerts("bans/channel") {
    val (groupInfo, _) = openGroup()
    val cath = member(blockedMembers("/_block #1 3 blocked=on"), "cath")
    val previousRange = VersionRange(1, LINK_NOTICE_VERSION - 1)
    val previous = cath.copy(memberChatVRange = previousRange, activeConn = cath.activeConn?.copy(peerChatVRange = previousRange))
    val texts = arrayOf(
      "Remove subscriber?", "Subscriber will be removed from channel - this cannot be undone!",
      "� 1 member with the previous version: cannot block", "Remove", "Remove and delete messages", "Cancel"
    )
    removeMemberAlert(null, groupInfo, previous)
    assertTexts(*texts)
    press("Cancel")
    removeMemberDialog(null, groupInfo, previous, ChatModel)
    assertTexts(*texts)
    press("Cancel")
    removeMemberDialog(null, groupInfo, cath, ChatModel)
    assertTexts(
      "Remove subscriber?", "cath won't be notified, and can't rejoin while blocked.",
      "Block for", "1 day", "Remove", "Remove and delete messages", "Cancel"
    )
  }

  @Test
  fun removeBlockedMemberOfPreviousVersionFromMemberInfo() = recordedAlerts("bans/previous-version") {
    val (groupInfo, _) = openGroup()
    removeMemberDialog(null, groupInfo, member(blockedMembers("/_block #1 3,4 blocked=on"), "cath"), ChatModel)
    assertTexts(
      "Remove member?", "Member will be removed from group - this cannot be undone!",
      "� 1 member with the previous version: cannot block", "Remove", "Remove and delete messages", "Cancel"
    )
  }

  @Test
  fun blockSubscribers() = recordedAlerts("bans/channel") {
    val (groupInfo, members) = openGroup()
    banMembersDialog(null, groupInfo, listOf(member(members, "cath"), member(members, "dan")))
    assertEquals("Block subscribers for all?", shownTexts().first())
  }

  @Test
  fun removeBlockedMembersForeverCancelled() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    removeMembersAlert(null, groupInfo, blockedMembers("/_block #1 2,3 blocked=on"))
    selectDuration("Forever")
    press("Remove")
    assertTexts("Remove forever?", "Members can't rejoin via the group link again, except from another app.", "Type yes", "Remove", "Cancel")
    press("Cancel")
    assertTexts()
  }

  @Test
  fun selectionToolbarRemovesAndBlocks() = recorded("bans/block-members") {
    chatParseMarkdown = { _ -> "{}" }
    val (groupInfo, _) = openGroup()
    replaceMembers(blockedMembers("/_block #1 2,3 blocked=on"))
    val selected = mutableStateOf<Set<Long>?>(setOf(2L, 3L))
    showGroupInfo(groupInfo, selected)
    pressToolbarButton(0)
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Remove members?") }
    press("Cancel")
    selected.value = setOf(4L)
    waitForIdle()
    pressToolbarButton(1)
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Block member for all?") }
  }

  @Test
  fun memberMenuBlocksForAll() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    showGroupInfo(groupInfo, mutableStateOf(null))
    onNodeWithText("dan").performTouchInput { longClick() }
    press("Block for all")
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Block member for all?") }
  }

  @Test
  fun memberMenuShowsHiddenMessages() = recordedAlerts("bans/hide-messages") {
    val (groupInfo, members) = openGroup()
    val cath = member(members, "cath").let { it.copy(memberSettings = it.memberSettings.copy(showMessages = false)) }
    replaceMembers(listOf(cath))
    showGroupInfo(groupInfo, mutableStateOf(null))
    onNodeWithText("hidden").assertExists()
    onNodeWithText("cath").performTouchInput { longClick() }
    press("Show messages")
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Show messages?") }
  }

  @Test
  fun memberInfoShowsHiddenMessages() = recorded("bans/hide-messages") {
    chatParseMarkdown = { _ -> "{}" }
    val (groupInfo, members) = openGroup()
    var shown = false
    val cath = member(members, "cath").let { it.copy(memberSettings = it.memberSettings.copy(showMessages = false)) }
    showMemberInfo(groupInfo, cath, unblockMember = { shown = true }) {}
    onNodeWithText("Show messages").performScrollTo().performClick()
    assertTrue(shown)
  }

  @Test
  fun businessChatInfoDeletesAndBlocks() = recorded("bans/business") {
    chatParseMarkdown = { _ -> "{}" }
    val (groupInfo, _) = openGroup()
    setContent {
      SimpleXTheme {
        AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
          val modalData = remember { ModalData().apply { appBarHandler.listState = LazyListState(Int.MAX_VALUE) } }
          modalData.GroupChatInfoView(ChatModel.chatsContext, null, groupInfo.id, null, true, remember { mutableStateOf(null) }, remember { mutableStateOf(null) }, remember { mutableStateOf(null) }, {}, {}, {})
        }
        AlertManager.shared.showInView()
      }
    }
    onNodeWithText("Delete and block").performScrollTo().performClick()
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Delete and block?") }
  }

  @Test
  fun memberInfoBlocksAndAsksToUnblockBeforePromotion() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    showMemberInfoView(groupInfo, member(members, "dan"))
    onNodeWithText("Block for all").performScrollTo().performClick()
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Block member for all?") }
    press("Cancel")
    val bob = member(blockedMembers("/_block #1 2 blocked=on"), "bob")
    replaceMembers(listOf(bob))
    showMemberInfoView(groupInfo, bob)
    onNodeWithText("member").performScrollTo().performClick()
    press("moderator")
    waitUntil(timeoutMillis = 5_000) { shownTexts().contains("Unblock first") }
  }

  @Test
  fun hiddenItemsAreShownAsHidden() = recorded("bans/hide-messages") {
    val (groupInfo, _) = openGroup()
    val chat = groupChat(groupInfo)
    val items = listOf(1L, 2L).map { ChatItem.getSampleData(id = it, dir = CIDirection.DirectRcv(), itemDeleted = CIDeleted.Blocked(deletedTs = null)) }
    runBlocking { withContext(Dispatchers.Main) { ChatModel.chatsContext.chatItems.replaceAll(items) } }
    assertEquals("hidden", markedDeletedText(items[0], chat.chatInfo))
    showView {
      Column {
        MarkedDeletedItemView(ChatModel.chatsContext, items[1], chat.chatInfo, null, remember { mutableStateOf(false) }, showViaProxy = false, showTimestamp = false)
        FramedItemView(ChatModel.chatsContext, chat, items[0], linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false, showMenu = remember { mutableStateOf(false) }, showTimestamp = false, receiveFile = {}, scrollToItemId = remember { mutableStateOf(null) })
      }
    }
    onNodeWithText("2 messages hidden").assertExists()
  }

  @Test
  fun chatListActions() = recordedAlerts("bans/contact-deletion") {
    val chat = openContact("/_get chat @2 count=0")
    val contact = (chat.chatInfo as ChatInfo.Direct).contact
    var deleteAndBan = false
    setContent {
      SimpleXTheme {
        val showMenu = remember { mutableStateOf(true) }
        Column {
          TagListAction(chat, showMenu)
          ArchiveContactAction(chat, contact, ChatModel, showMenu)
          DeleteAndBanAction(showMenu) { deleteAndBan = true }
        }
        ModalManager.start.showInView()
        AlertManager.shared.showInView()
      }
    }
    press("Delete and block")
    assertTrue(deleteAndBan)
    press("Add to list")
    onNodeWithText("Favorites").assertExists()
    runBlocking { withContext(Dispatchers.Main) { ModalManager.start.closeModals() } }
    press("Archive")
    waitUntil(timeoutMillis = 5_000) { (chatInfo(chat.id) as ChatInfo.Direct).contact.chatDeleted }
  }

  @Test
  fun contactListMenuOffersDeleteAndBlock() = recorded("bans/contact-deletion") {
    val archived = (recordedResult("/_delete @2 archive") as CR.ContactDeleted).contact
    val chat = openChat(Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(archived), chatItems = emptyList()))
    showView { ContactListNavLinkView(chat, remember { mutableStateOf(false) }, showDeletedChatIcon = false) }
    onNodeWithText("bob").performMouseInput { rightClick() }
    onNodeWithText("Delete and block").assertExists()
  }

  @Test
  fun setGroupMembersLoadsMembers() = recorded("bans/block-members") {
    val (groupInfo, members) = openGroup()
    ChatModel.groupMembers.value = emptyList()
    runBlocking { setGroupMembers(null, groupInfo, ChatModel) }
    assertEquals(members.map { it.groupMemberId }.toSet(), ChatModel.groupMembers.value.map { it.groupMemberId }.toSet())
  }

  @Test
  fun noticeAlertHowItWorksAndDismiss() = recordedAlerts("bans/contact-notices") {
    var cleanups = 0
    showLinkNoticeAlert(LinkNoticeTarget.valueOf("Group"), expiresAt = null, cleanup = { cleanups++ })
    press("How it works")
    assertEquals(1, cleanups)
    showLinkNoticeAlert(LinkNoticeTarget.Group, expiresAt = null, cleanup = { cleanups++ })
    waitForIdle()
    onAllNodes(isRoot()).onLast().performKeyInput { pressKey(Key.Escape) }
    waitUntil(timeoutMillis = 5_000) { cleanups == 2 }
    assertEquals(4, LinkNoticeTarget.entries.size)
  }

  @Test
  fun groupNoticePlanWithoutParsedLink() = recordedAlerts("bans/plan-group-notice") {
    chatParseMarkdown = { _ -> "null" }
    connect("GROUP_LINK")
    assertTexts("Not allowed", "Blocked by the admins of this group until $UNTIL.", "OK", "How it works")
  }

  private fun ComposeUiTest.connectPreparedGroup(chatCmd: String) {
    val groupInfo = ((recordedResult(chatCmd) as CR.ApiChat).chat.chatInfo as ChatInfo.Group).groupInfo
    runBlocking { ChatController.apiConnectPreparedGroup(null, groupInfo.groupId, LinkNoticeTarget.fromGroupInfo(groupInfo), incognito = false, msg = null) }
    waitForIdle()
  }

  private fun ComposeUiTest.selectDuration(duration: String) {
    press("1 day")
    press(duration)
  }

  private fun ComposeUiTest.showView(content: @Composable () -> Unit) {
    setContent {
      SimpleXTheme {
        AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(), content = content)
      }
    }
  }

  @Composable
  private fun ComposeViewFor(chat: Chat, state: ComposeState) {
    ComposeView(null, ChatModel, ChatModel.chatsContext, chat, remember { mutableStateOf(state) }, remember { mutableStateOf(false) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
  }

  private fun ComposeUiTest.showGroupInfo(groupInfo: GroupInfo, selectedItems: androidx.compose.runtime.MutableState<Set<Long>?>) {
    setContent {
      SimpleXTheme {
        AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
          val modalData = remember { ModalData().apply { appBarHandler.listState = LazyListState(Int.MAX_VALUE) } }
          modalData.GroupChatInfoLayout(
            chat = groupChat(groupInfo),
            groupInfo = groupInfo,
            User.sampleData,
            sendReceipts = remember { mutableStateOf(SendReceipts.Yes) },
            setSendReceipts = {},
            chatItemTTL = remember { mutableStateOf(ChatItemTTL.fromSeconds(0)) },
            setChatItemTTL = {},
            activeSortedMembers = ChatModel.groupMembers.value,
            developerTools = false,
            onLocalAliasChanged = {},
            groupLink = null,
            groupLinkLoaded = true,
            selectedItems = selectedItems,
            appBar = remember { mutableStateOf(null) },
            scrollToItemId = remember { mutableStateOf(null) },
            addMembers = {},
            showMemberInfo = { _, _ -> },
            editGroupProfile = {},
            addOrEditWelcomeMessage = {},
            openMemberSupport = {},
            openPreferences = {},
            deleteGroup = {},
            deleteAndBan = {},
            clearChat = {},
            leaveGroup = {},
            manageGroupLink = {},
            manageWebPage = {},
            onSearchClicked = {},
            setSimplexName = {},
            deletingItems = remember { mutableStateOf(false) }
          )
        }
        AlertManager.shared.showInView()
      }
    }
  }

  private fun ComposeUiTest.pressToolbarButton(index: Int) {
    val buttons = onAllNodes(hasClickAction())
    val count = buttons.fetchSemanticsNodes().size
    buttons[count - 3 + index].performClick()
    waitForIdle()
  }

  private fun ComposeUiTest.showMemberInfoView(groupInfo: GroupInfo, member: GroupMember) {
    setContent {
      SimpleXTheme {
        AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
          GroupMemberInfoView(
            null, groupInfo, member,
            scrollToItemId = remember { mutableStateOf(null) },
            connStats = remember { mutableStateOf(null) },
            connectionCode = remember { mutableStateOf(null) },
            connectionLoaded = remember { mutableStateOf(true) },
            chatModel = ChatModel,
            openedFromSupportChat = false,
            close = {},
            closeAll = {}
          )
        }
        AlertManager.shared.showInView()
      }
    }
  }

  private fun ComposeUiTest.showMemberInfo(groupInfo: GroupInfo, member: GroupMember, unblockMember: () -> Unit = {}, unblockForAll: () -> Unit) {
    showView {
      GroupMemberInfoLayout(
        rhId = null,
        groupInfo = groupInfo,
        member = member,
        scrollToItemId = remember { mutableStateOf(null) },
        connStats = remember { mutableStateOf(null) },
        newRole = remember { mutableStateOf(member.memberRole) },
        developerTools = false,
        connectionCode = null,
        connectionLoaded = true,
        getContactChat = { null },
        openDirectChat = {},
        createMemberContact = {},
        connectViaAddress = {},
        blockMember = {},
        unblockMember = unblockMember,
        blockForAll = {},
        unblockForAll = unblockForAll,
        removeMember = {},
        deleteMemberMessages = {},
        onRoleSelected = {},
        switchMemberAddress = {},
        abortSwitchMemberAddress = {},
        syncMemberConnection = {},
        syncMemberConnectionForce = {},
        verifyClicked = {},
        openedFromSupportChat = false,
      )
    }
  }

  private fun ComposeUiTest.awaitMember(name: String, condition: (GroupMember) -> Boolean) {
    waitUntil(timeoutMillis = 5_000) { ChatModel.groupMembers.value.any { it.displayName == name && condition(it) } }
  }

  private fun openGroup(cmd: String = "/_members #1"): Pair<GroupInfo, List<GroupMember>> {
    val group = (recordedResult(cmd) as CR.GroupMembers).group
    openChat(groupChat(group.groupInfo))
    ChatModel.groupMembers.value = group.members
    ChatModel.groupMembersIndexes.value = group.members.withIndex().associate { it.value.groupMemberId to it.index }
    return group.groupInfo to group.members
  }

  private fun loadAddress() {
    ChatModel.userAddress.value = (recordedResult("/_show_address 1") as CR.UserContactLink).contactLink
  }

  private fun replaceMembers(members: List<GroupMember>) {
    ChatModel.groupMembers.value = ChatModel.groupMembers.value.map { m -> members.firstOrNull { it.groupMemberId == m.groupMemberId } ?: m }
  }

  private fun openContact(cmd: String): Chat = openChat((recordedResult(cmd) as CR.ApiChat).chat)

  private fun openChat(chat: Chat): Chat {
    runBlocking { withContext(Dispatchers.Main) { ChatModel.chatsContext.addChat(chat) } }
    ChatModel.chatId.value = chat.id
    return chat
  }

  private fun groupChat(groupInfo: GroupInfo) = Chat(remoteHostId = null, chatInfo = ChatInfo.Group(groupInfo, groupChatScope = null), chatItems = emptyList())

  private fun blockedMembers(cmd: String): List<GroupMember> = (recordedResult(cmd) as CR.MembersBlockedForAllUser).members

  private fun member(members: List<GroupMember>, name: String): GroupMember = members.first { it.displayName == name }

  private val UNTIL: String get() = localTimestamp(Instant.parse("2099-01-01T12:00:00Z"), FormatStyle.SHORT)
}
