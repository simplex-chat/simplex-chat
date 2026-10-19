package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.closePendingConnectionInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.createOneTimeLink
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.openRowMenu
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

private const val PENDING = "invited to connect"
private val PLAN_RESULT = Regex("invitation link: ok to connect|error: connection authorization failed.*")
private val AUTH_FAILED = Regex("error: connection authorization failed - .*")
private val notOffered = listOf("markRead", "markUnread", "favorite", "unfavorite", "ntfs.None", "ntfs.All", "clear")

class ChatListMenusPendingTest : UiTest() {
  @Test fun contactMenuMarkRead() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    poll(what = "unread badge 1") { "1" in alice.chatList.rowTexts("bob") }
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "the unread badge gone") { "1" !in alice.chatList.rowTexts("bob") && alice.chatList.chat("bob").chatStats.unreadCount == 0 }
  }

  @Test fun contactMenuMarkUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    quiesce()
    val emptyTexts = alice.chatList.rowTexts("bob").count { it.isEmpty() }
    alice.chatList.rowMenu("bob", "markUnread")
    poll(what = "the unread mark and its empty badge on the row") {
      alice.chatList.chat("bob").chatStats.unreadChat && alice.chatList.rowTexts("bob").count { it.isEmpty() } > emptyTexts
    }
    val id = openRowMenu("bob")
    poll(what = "`Mark read` offered instead of `Mark unread`") {
      exists(Ui.ChatList.RowMenu(id, "markRead")) && !exists(Ui.ChatList.RowMenu(id, "markUnread"))
    }
    click(Ui.ChatList.RowMenu(id, "markRead"))
    poll(what = "the unread mark cleared") { !alice.chatList.chat("bob").chatStats.unreadChat }
  }

  @Test fun contactMenuRightClickOpens() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val id = openRowMenu("bob")
    poll(what = "the contact menu") {
      listOf("markUnread", "favorite", "ntfs.None", "clear", "delete").all { exists(Ui.ChatList.RowMenu(id, it)) } &&
        listOf("Mark unread", "Favorite", "Mute", "Add to list", "Clear", "Delete").all { seesText(it) }
    }
  }

  @Test fun groupMenuMuteModesAllMentionsNone() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    fun team() = (alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo
    fun muteIcon() = "Notifications" in alice.chatList.rowDescriptions("team")
    fun mode(expected: MsgFilter): Boolean {
      val actual = team().chatSettings.enableNtfs
      check(actual == expected) { "notification mode is $actual, expected $expected" }
      return true
    }
    poll(what = "all notifications and no mute icon on the row") { mode(MsgFilter.All) && !muteIcon() }
    alice.chatList.rowMenu("team", "ntfs.Mentions", "Mute")
    poll(what = "mentions-only notifications and the mute icon on the row") { mode(MsgFilter.Mentions) && muteIcon() }
    alice.chatList.rowMenu("team", "ntfs.None", "Mute all")
    poll(what = "notifications off and the mute icon on the row") { mode(MsgFilter.None) && muteIcon() }
    alice.chatList.rowMenu("team", "ntfs.All", "Unmute")
    poll(what = "all notifications and the mute icon gone") { mode(MsgFilter.All) && !muteIcon() }
  }

  @Test fun notesMenuMarkUnreadAndClear() = uiTest {
    val alice = app("alice")
    alice.chatList.open("Private notes").send("keep in mind")
    backToChatList()
    poll(what = "the note in the row preview") { "keep in mind" in alice.chatList.rowTexts("Private notes") }
    alice.chatList.rowMenu("Private notes", "markUnread")
    poll(what = "the notes marked unread") { alice.chatList.chat("Private notes").chatStats.unreadChat }
    alice.chatList.rowMenu("Private notes", "clear")
    poll(what = "the `Clear private notes?` alert") {
      seesText("Clear private notes?") && seesText("All messages will be deleted - this cannot be undone!")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the notes cleared and the unread mark reset") {
      alice.chatList.chat("Private notes").let { it.chatItems.isEmpty() && !it.chatStats.unreadChat } &&
        "keep in mind" !in alice.chatList.rowTexts("Private notes")
    }
  }

  @Test fun pendingConnectionMenuSetName() = uiTest {
    val alice = app("alice")
    keepOneTimeLink(alice)
    poll(what = "no pending connection info before the menu") { !seesText("You invited a contact") }
    alice.chatList.rowMenu(PENDING, "setName")
    poll(what = "`You invited a contact` with the name field") { seesText("You invited a contact") && exists(Ui.Info.Alias) }
    typeInto(Ui.Info.Alias, "friend")
    poll(what = "the pending connection named `friend` in the model") {
      chatModel.chats.value.any { it.chatInfo is ChatInfo.ContactConnection && it.chatInfo.chatViewName == "friend" }
    }
    closePendingConnectionInfo()
    poll(what = "the row named `friend`") { "friend" in alice.chatList.rowTexts("friend") }
  }

  @Test fun pendingConnectionClickOpensInfo() = uiTest {
    val alice = app("alice")
    keepOneTimeLink(alice)
    poll(what = "the pending row `invited to connect` / `you shared one-time link` and no pending connection info") {
      alice.chatList.rowTexts(PENDING).containsAll(listOf(PENDING, "you shared one-time link")) && !seesText("You invited a contact")
    }
    val id = alice.chatList.chatId(PENDING)
    alice.chatList.open(PENDING)
    poll(what = "`You invited a contact` with the one-time link and the name field") {
      chatModel.chatId.value == id && seesText("You invited a contact") && seesText("One-time invitation link") && exists(Ui.Info.Alias)
    }
  }

  @Test fun pendingConnectionMenuDeleteInitiated() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = keepOneTimeLink(alice)
    alice.chatList.rowMenu(PENDING, "delete")
    poll(what = "the `Delete pending connection?` alert") {
      seesText("Delete pending connection?") && seesText("The contact you shared this link with will NOT be able to connect!", substring = true)
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    alice.chatList.gone(PENDING)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the deleted link to fail its plan on the server") {
      bob sends "/_connect plan 1 $link"
      bob.eventuallyMatching(PLAN_RESULT).value.startsWith("error")
    }
    bob sends "/c $link"
    bob.eventuallyMatching(AUTH_FAILED)
  }

  @Test fun contactMenuForNotReadyContactOnlyDelete() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createOneTimeLink()
    alice.chatList.row("Private notes")
    enterLinkInSearch(link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Open new chat` alert for bob") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("bob")
    }
    click(planConfirm)
    poll(what = "the prepared contact bob without a connection") {
      (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.let { it.nextConnectPrepared && it.activeConn == null } == true
    }
    backToChatList()
    val id = openRowMenu("bob")
    poll(what = "a row menu with only `Delete`") {
      exists(Ui.ChatList.RowMenu(id, "delete")) && notOffered.none { exists(Ui.ChatList.RowMenu(id, it)) } && !seesText("Add to list")
    }
    click(Ui.ChatList.RowMenu(id, "delete"))
    poll(what = "the `Confirm contact deletion?` alert") { seesText("Confirm contact deletion?") }
  }
}
