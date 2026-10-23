package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.confirmAlert
import chat.simplex.app.ui.harness.linkGroupWithDirectMessages
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class MemberContactTest : UiTest() {
  @Test fun memberContact_createAndSendInvitation() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = linkGroupWithDirectMessages(alice, "team", bob)
    bob sends "/set accept member contacts on"
    bob expects "ok"
    check(directChatWith("bob") == null) { "alice already has a direct chat with bob" }
    info.member("bob").message()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a new direct chat with bob open with `Send direct message to connect`") {
      chatModel.chatId.value != null && chatModel.chatId.value == directChatWith("bob")?.id && seesText("Send direct message to connect")
    }
    val chat = ChatScreen(this)
    chat.send("hi")
    bob.eventuallyAll(lines = listOf("#team alice is creating direct contact alice with you"), messages = listOf("alice> hi"), timeoutMs = CONNECT_TIMEOUT_MS)
    bob.eventually("alice: you can send messages to contact", CONNECT_TIMEOUT_MS)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(what = "`Send direct message to connect` gone once bob is connected") { !seesText("Send direct message to connect") }
    bob sends "@alice hey"
    chat.itemId("bob's `hey` in the direct chat") { it.content.text == "hey" }
  }

  @Test fun memberContact_acceptAndRejectRequests() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    linkGroupWithDirectMessages(alice, "team", bob, cath).close()
    bob.requestsContactWith("alice")
    backToChatList()
    var chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's request with `Accept` and `Reject` in place of the message field") {
      exists(Ui.Chat.RequestAccept) && exists(Ui.Chat.RequestReject) && !exists(Ui.Chat.Message) && chat.itemTexts().contains("hi")
    }
    click(Ui.Chat.RequestAccept)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message field in place of `Accept` and `Reject`") { exists(Ui.Chat.Message) && !exists(Ui.Chat.RequestAccept) }
    chat.send("welcome")
    bob.expectsMessage("alice> welcome", CONNECT_TIMEOUT_MS)
    cath.requestsContactWith("alice")
    backToChatList()
    alice.chatList.open("cath")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath's request with `Reject`") { exists(Ui.Chat.RequestReject) }
    click(Ui.Chat.RequestReject)
    confirmAlert("Reject contact request", "The sender will NOT be notified.", "Reject")
    alice.chatList.gone("cath")
    backToChatList()
    chat = alice.chatList.open("team")
    chat.send("end")
    cath.expectsMessage("#team alice> end", CONNECT_TIMEOUT_MS)
  }

  private fun directChatWith(name: String) =
    chatModel.chats.value.firstOrNull { (it.chatInfo as? ChatInfo.Direct)?.contact?.displayName == name }

  private fun CliPeer.requestsContactWith(member: String) {
    this sends "@#team $member hi"
    eventuallyAll(
      lines = listOf(
        "member #team $member does not have direct connection, creating",
        "contact for member #team $member is created",
        "sent invitation to connect directly to member #team $member",
      ),
      messages = listOf("@$member hi"),
      timeoutMs = CONNECT_TIMEOUT_MS,
    )
  }
}
