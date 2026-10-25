package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.ui.Ui
import org.junit.Test

class ContactsTest : UiTest() {
  @Test fun contactRequestMenuAccept() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    alice.chatList.rowMenu("bob", "accept")
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's request row replaced by the ready contact bob") {
      chatModel.chats.value.count { it.chatInfo.chatViewName == "bob" } == 1 &&
        (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.ready == true &&
        alice.chatList.shown("bob") && alice.chatList.rowTexts("bob").none { "Open to accept" in it }
    }
    alice.chatList.open("bob").send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
  }

  @Test fun contactRequestMenuReject() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    alice.chatList.rowMenu("bob", "reject")
    alice.chatList.gone("bob")
    check(!AlertManager.shared.hasAlertsShown()) { "an alert is shown after `Reject`" }
    restart()
    alice.chatList.row("Private notes")
    alice.chatList.gone("bob")
    bob sends "/u"
    bob expects "user profile: bob"
  }

  @Test fun editProfileDisplayName() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.profile().rename("alice2")
    bob.eventually("contact alice changed to alice2", CONNECT_TIMEOUT_MS)
    poll(what = "alice's own profile renamed") { chatModel.currentUser.value?.displayName == "alice2" }
    backToChatList()
    alice.userPicker()
    poll(what = "the user picker showing alice2") { seesText("alice2") }
  }
}
