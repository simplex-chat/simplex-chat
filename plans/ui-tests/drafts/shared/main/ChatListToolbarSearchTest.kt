package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import org.junit.Test

class ChatListToolbarSearchTest : UiTest() {
  @Test fun searchFiltersByDisplayName() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    alice.chatList.search("bo")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("Private notes") }
  }
}
