package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.isToggleable
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollTo
import androidx.compose.ui.test.performTextInput
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.acceptMemberDialog
import chat.simplex.common.views.chat.archiveContact
import chat.simplex.common.views.chat.deleteAndBanContactDialog
import chat.simplex.common.views.chat.deleteContactDialog
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chatlist.TagListView
import chat.simplex.common.views.helpers.AppBarHandler
import chat.simplex.common.views.helpers.AppBarHeight
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import chat.simplex.common.views.newchat.LinkNoticeTarget
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
  fun blockMemberForDay() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    banMemberDialog(null, groupInfo, member(members, "bob"))
    assertTexts(
      "Block member for all?", "bob won't be notified, and if removed, can't rejoin while blocked.",
      "Block for", "1 day", "Block", "Block and remove", "Block, remove and delete messages", "Cancel"
    )
    press("Block")
    awaitMember("bob") { it.blockedByAdmin && it.blockedUntil != null }
  }

  @Test
  fun blockMemberForWeek() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    banMemberDialog(null, groupInfo, member(members, "cath"))
    selectDuration("1 week")
    press("Block")
    awaitMember("cath") { it.blockedByAdmin && it.blockedUntil != null }
  }

  @Test
  fun blockMemberForever() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    banMemberDialog(null, groupInfo, member(members, "dan"))
    selectDuration("Forever")
    press("Block")
    awaitMember("dan") { it.blockedByAdmin && it.blockedUntil == null }
  }

  @Test
  fun unblockMember() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    val bob = member(blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")
    replaceMembers(listOf(bob))
    unblockForAllAlert(null, groupInfo, bob)
    assertTexts("Unblock member for all?", "Messages from bob will be shown for all members.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("bob") { !it.blockedByAdmin }
  }

  @Test
  fun unblockMembers() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    replaceMembers(blockedMembers("/_block #1 3 blocked=on ttl=604800") + blockedMembers("/_block #1 4 blocked=on"))
    unblockForAllAlert(null, groupInfo, listOf(member(members, "cath").groupMemberId, member(members, "dan").groupMemberId))
    assertTexts("Unblock members for all?", "Messages from these members will be shown for all members.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("cath") { !it.blockedByAdmin }
    awaitMember("dan") { !it.blockedByAdmin }
  }

  @Test
  fun memberInfoBlockedUntil() = recorded("bans/block-members") {
    val (groupInfo, _) = openGroup()
    var unblocked = false
    showMemberInfo(groupInfo, member(blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")) { unblocked = true }
    onNodeWithText("Blocked until $UNTIL").assertExists()
    onNodeWithText("Unblock for all").performScrollTo().performClick()
    assertTrue(unblocked)
  }

  @Test
  fun memberInfoBlockedForever() = recorded("bans/block-members") {
    val (groupInfo, _) = openGroup()
    showMemberInfo(groupInfo, member(blockedMembers("/_block #1 4 blocked=on"), "dan")) {}
    onNodeWithText("Blocked by admin").assertExists()
    onNodeWithText("Unblock for all").assertExists()
  }

  @Test
  fun blockMembers() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    var selectionCleared = false
    banMembersDialog(null, groupInfo, listOf(member(members, "bob"), member(members, "cath"))) { selectionCleared = true }
    assertTexts(
      "Block members for all?", "Members won't be notified, and if removed, can't rejoin while blocked.",
      "Block for", "1 day", "Block", "Block and remove", "Block, remove and delete messages", "Cancel"
    )
    press("Block")
    awaitMember("cath") { it.blockedByAdmin }
    waitUntil(timeoutMillis = 5_000) { selectionCleared }
  }

  @Test
  fun blockAndRemoveMember() = recordedAlerts("bans/block-members") {
    val (groupInfo, members) = openGroup()
    var closed = false
    banMemberDialog(null, groupInfo, member(members, "bob"), close = { closed = true })
    press("Block and remove")
    awaitMember("bob") { it.memberStatus == GroupMemberStatus.MemRemoved }
    waitUntil(timeoutMillis = 5_000) { closed }
  }

  @Test
  fun removeBlockedMemberForever() = recordedAlerts("bans/block-members") {
    val (groupInfo, _) = openGroup()
    val cath = member(blockedMembers("/_block #1 2,3 blocked=on ttl=86400"), "cath")
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
    removeMembersAlert(null, groupInfo, blockedMembers("/_block #1 2,3 blocked=on ttl=86400"))
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
      "1 member with previous version: block only", "Block for", "1 day", "Block", "Cancel"
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
      "Block for", "1 day", "Block", "Cancel"
    )
  }

  @Test
  fun removeBlockedMembersOfPreviousVersion() = recordedAlerts("bans/previous-version") {
    val (groupInfo, _) = openGroup()
    val blocked = blockedMembers("/_block #1 3,4 blocked=on ttl=86400")
    removeMembersAlert(null, groupInfo, blocked)
    assertTexts(
      "Remove members?", "Members will be removed from group - this cannot be undone!",
      "1 member with previous version: cannot block", "Remove", "Remove and delete messages", "Cancel"
    )
    press("Remove")
    awaitMember("dan") { it.memberStatus == GroupMemberStatus.MemRemoved }
  }

  @Test
  fun blockPendingMember() = recordedAlerts("bans/pending-member") {
    val (groupInfo, members) = openGroup()
    val bob = member(members, "bob")
    assertTrue(bob.canBlockForAll(groupInfo))
    banMemberDialog(null, groupInfo, bob)
    press("Block")
    awaitMember("bob") { it.blockedByAdmin }
  }

  @Test
  fun acceptBlockedPendingMember() = recordedAlerts("bans/pending-member") {
    val (groupInfo, _) = openGroup()
    val bob = member(blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")
    replaceMembers(listOf(bob))
    acceptMemberDialog(null, groupInfo, bob)
    assertTexts("Accept member", "Member will be unblocked and will join the group, accept member?", "Accept as member", "Accept as observer", "Cancel")
    press("Accept as member")
    awaitMember("bob") { !it.memberPending && !it.blockedByAdmin }
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
    recheckItems(groupInfo, selected, listOf(bob, cath), delete, block, unblock, toMember, toObserver)
    assertFalse(block.value)
    assertFalse(unblock.value)
    selected.value = setOf(bob.groupMemberId)
    recheckItems(groupInfo, selected, listOf(bob, cath), delete, block, unblock, toMember, toObserver)
    assertTrue(block.value)
  }

  @Test
  fun blockSubscriber() = recordedAlerts("bans/channel") {
    val (groupInfo, members) = openGroup()
    banMemberDialog(null, groupInfo, member(members, "cath"))
    assertTexts(
      "Block subscriber for all?", "cath won't be notified, and if removed, can't rejoin while blocked.",
      "Block for", "1 day", "Block", "Block and remove", "Block, remove and delete messages", "Cancel"
    )
    press("Block")
    awaitMember("cath") { it.blockedByAdmin }
  }

  @Test
  fun unblockSubscriber() = recordedAlerts("bans/channel") {
    val (groupInfo, _) = openGroup()
    val cath = member(blockedMembers("/_block #1 3 blocked=on ttl=86400"), "cath")
    replaceMembers(listOf(cath))
    unblockForAllAlert(null, groupInfo, cath)
    assertTexts("Unblock subscriber for all?", "Messages from cath will be shown for all subscribers.", "Cancel", "Unblock for all")
    press("Unblock for all")
    awaitMember("cath") { !it.blockedByAdmin }
  }

  @Test
  fun removeSubscriberForever() = recordedAlerts("bans/channel") {
    val (groupInfo, members) = openGroup()
    banMemberDialog(null, groupInfo, member(members, "dan"))
    selectDuration("Forever")
    press("Block and remove")
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
    assertTexts("Delete channel?", "Keep conversation", "Delete", "Cancel")
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
  fun deleteOwnGroupKeepingConversation() = recordedAlerts("bans/group-deletion") {
    val (groupInfo, _) = openGroup("/_members #2")
    deleteGroupDialog(groupChat(groupInfo), groupInfo, ChatModel)
    press("Delete")
    onNode(isToggleable()).performClick()
    press("Delete")
    waitUntil(timeoutMillis = 5_000) { (chatInfo(groupInfo.id) as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemGroupDeleted }
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
    archiveContact(chat, (chat.chatInfo as ChatInfo.Direct).contact, ChatModel)
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
    archiveContact(chat, (chat.chatInfo as ChatInfo.Direct).contact, ChatModel)
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

  private fun ComposeUiTest.showMemberInfo(groupInfo: GroupInfo, member: GroupMember, unblockForAll: () -> Unit) {
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
        unblockMember = {},
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
