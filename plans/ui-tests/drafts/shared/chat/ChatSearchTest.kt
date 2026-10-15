package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatSearchTest : UiTest() {
  @Test fun search_direct_filtersResults() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello there 🙂")
    bob expectsMessage "alice> hello there 🙂"
    bob sends "@alice hello there"
    bob sends "@alice how are you?"
    val other = chat.itemId("how are you?")
    chat.search("ello ther")
    poll(what = "only the two matching items") {
      chat.itemTexts().toSet() == setOf("hello there 🙂", "hello there") && !exists(Ui.Chat.Item(other))
    }
  }
}
