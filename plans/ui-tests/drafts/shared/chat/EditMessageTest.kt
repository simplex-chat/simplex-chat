package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.ui.Ui
import org.junit.Test

class EditMessageTest : UiTest() {
  @Test fun edit_received_updatesBubble() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("hello")
    bob sends "! @alice hey"
    poll(what = "the edited bubble") {
      chat.item(id)?.let { it.content.text == "hey" && it.meta.itemEdited } == true && textsOf(Ui.Chat.Item(id)).any { "hey" in it }
    }
  }
}
