package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.UiTest
import org.junit.Test

class SendCompatTest : UiTest() {
  @Test fun sendText_enter_peerReceives() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob").send("hello there 🙂")
    bob expectsMessage "alice> hello there 🙂"
  }
}
