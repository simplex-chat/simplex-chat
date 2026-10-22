package chat.simplex.app.ui.android

import android.app.Notification
import chat.simplex.app.model.NtfManager
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import org.junit.Test

class SystemNotificationsAndroidTest : UiTest() {
  @Test fun sysNtf_foreground_onlyForChatsNotOpen() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val bobId = alice.chatList.chatId("bob")
    alice.chatList.row("cath")
    quiesce()
    awaitForeground()
    clearNotificationsViaApi()
    val chat = alice.chatList.open("bob")
    bob sends "@alice $TO_OPEN_CHAT"
    chat.itemId(TO_OPEN_CHAT)
    cath sends "@alice $FROM_CATH"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath's message notification while bob's chat is open") { messageNotifications().any { it.title == "cath" } }
    val fromCath = messageNotifications().single { it.title == "cath" }
    check(fromCath.text == FROM_CATH && fromCath.notification.group == NtfManager.MessageGroup && (fromCath.notification.flags and Notification.FLAG_AUTO_CANCEL) != 0 && messageSummaryShown()) {
      "cath's notification: $fromCath, group ${fromCath.notification.group}, flags ${fromCath.notification.flags}, summary ${messageSummaryShown()}"
    }
    check(chatModel.chatId.value == bobId && messageNotifications().none { it.title == "bob" }) { "bob's message to his open chat was notified: ${messageNotifications()}" }

    backToChatList()
    alice.chatList.rowMenu("cath", "ntfs.None", "Mute")
    poll(what = "cath muted and her notification cancelled") {
      alice.chatList.chat("cath").chatInfo.chatSettings?.enableNtfs == MsgFilter.None && messageNotifications().none { it.title == "cath" }
    }
    cath sends "@alice $MUTED"
    awaitRowText(alice, "cath", MUTED)
    bob sends "@alice $AFTER_CLOSE"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's notification now that his chat is closed") { messageNotifications().any { it.title == "bob" } }
    check(messageNotifications().map { it.title to it.text } == listOf("bob" to AFTER_CLOSE)) { "notifications after muting cath: ${messageNotifications()}" }

    alice.chatList.open("bob")
    poll(what = "bob's notification cancelled once his chat is read") { messageNotifications().none { it.title == "bob" } }
  }

  @Test fun sysNtf_tapOpensChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    quiesce()
    awaitForeground()
    clearNotificationsViaApi()
    try {
      bob sends "@alice $HELLO"
      poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's message notification") { messageNotifications().any { it.title == "bob" && it.text == HELLO } }
      check(chatModel.chatId.value == null && runningMainActivities().size == 1) { "before the tap: open chat ${chatModel.chatId.value}, activities ${runningMainActivities()}" }
      messageNotifications().single { it.title == "bob" }.notification.contentIntent.send()
      poll(what = "MainActivity recreated with bob's chat open") { runningMainActivities().size == 1 && chatModel.chatId.value == bobId }
      ChatScreen(this).itemId(HELLO)
      poll(what = "bob's notification cancelled once `$HELLO` is read") { messageNotifications().none { it.title == "bob" } }
    } finally {
      finishMainActivities()
    }
  }

  @Test fun sysNtf_contactRequest_acceptAction() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    alice.chatList.row("Private notes")
    awaitForeground()
    clearNotificationsViaApi()
    bob sends "/c $address"
    bob expects "connection request sent!"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's contact request notification") { messageNotifications().any { it.title == "bob" && it.text == REQUEST } }
    val request = messageNotifications().single { it.text == REQUEST }
    val actions = request.notification.actions.orEmpty()
    check(actions.map { it.title.toString() } == listOf("Accept") && request.notification.getLargeIcon() != null) {
      "request notification actions ${actions.map { it.title }}, large icon ${request.notification.getLargeIcon() != null}"
    }
    check(chatModel.chats.value.none { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.ready } == true }) { "bob is a connected contact before Accept" }
    actions.single().actionIntent.send()
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the request notification replaced by bob's Connected notification") {
      messageNotifications().let { ns -> ns.none { it.text == REQUEST } && ns.any { it.title == "bob" && it.text == "Connected" } }
    }
    check(chatModel.chatId.value == null) { "Accept in the notification opened a chat: ${chatModel.chatId.value}" }
  }

  private companion object {
    const val TO_OPEN_CHAT = "sent to the open chat"
    const val FROM_CATH = "hello from cath"
    const val MUTED = "sent to a muted chat"
    const val AFTER_CLOSE = "sent after the chat closed"
    const val HELLO = "hello"
    const val REQUEST = "New contact request"
  }
}
