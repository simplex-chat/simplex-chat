package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatListMenuTest : UiTest() {
  @Test fun contactMenuClearChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice to be cleared"
    poll(what = "the message preview on the row") { "to be cleared" in alice.chatList.rowTexts("bob") }
    alice.chatList.rowMenu("bob", "clear")
    poll(what = "the `Clear chat?` alert for bob") {
      seesText("Clear chat?") && seesText("bob\n\nAll messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you.") &&
        "Clear" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the preview gone") { "to be cleared" !in alice.chatList.rowTexts("bob") }
    bob sends "@alice after clear"
    poll(what = "bob's new message on the row") { "after clear" in alice.chatList.rowTexts("bob") }
  }
}
