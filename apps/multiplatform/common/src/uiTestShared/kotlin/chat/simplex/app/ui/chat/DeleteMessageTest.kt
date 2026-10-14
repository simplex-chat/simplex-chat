package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class DeleteMessageTest : UiTest() {
  @Test fun delete_received_onlyForMe() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("hello")
    chat.menu("hello", "delete")
    waitFor(Ui.Alert.Option(Ui.AlertSource.Shared, "forMe"))
    assertFalse(exists(Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")))
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "forMe"))
    poll(what = "the item removed") { chat.item(id) == null && !exists(Ui.Chat.Item(id)) }
    chat.send("ping")
    bob expectsMessage "alice> ping"
  }
}
