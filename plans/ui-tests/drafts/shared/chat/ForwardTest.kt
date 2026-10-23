package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.CIForwardedFrom
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ForwardTest : UiTest() {
  @Test fun forward_contactToContact_peerSeesForwarded() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice hey"
    alice.chatList.open("bob").forwardTo("hey", "cath")
    cath.eventuallyMessage("alice> -> forwarded")
    cath expects "      hey"
  }

  @Test fun forward_toNotes_showsSavedFrom() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hey"
    val chat = alice.chatList.open("bob")
    chat.forwardTo("hey", "Private notes")
    val notes = alice.chatList.chatId("Private notes")
    poll(what = "the note saved from bob") {
      chatModel.chatId.value == notes && chatModel.chatsContext.chatItems.value.any { (it.meta.itemForwarded as? CIForwardedFrom.Contact)?.chatName == "bob" }
    }
    val id = chat.itemId("hey")
    poll(what = "the `saved from` / `bob` header") { textsOf(Ui.Chat.Item(id)).let { t -> t.any { "saved from" in it } && "bob" in t } }
  }
}
