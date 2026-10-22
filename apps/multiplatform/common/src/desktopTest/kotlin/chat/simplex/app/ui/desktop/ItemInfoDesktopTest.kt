package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ItemInfoDesktopTest : UiTest() {
  @Test fun info_share_copiesText() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice question"
    val chat = alice.chatList.open("bob")
    chat.itemId("question")
    chat.menu("question", "reply")
    chat.send("answer")
    bob expectsMessage "alice> > question"
    bob expects "      answer"
    val id = chat.itemId("answer")
    chat.itemInfo("answer")
    setClipboardText("unrelated")
    click(Ui.ItemInfo.Share)
    var shared = ""
    poll(what = "the item info text on the clipboard after Share") { shared = clipboardText().orEmpty(); shared.startsWith("# Sent message") }
    val sentAt = localTimestamp(chat.item(id)!!.meta.itemTs)
    check(shared.startsWith("# Sent message\n\nSent at: $sentAt\n")) { "the shared info does not start with the title and `Sent at: $sentAt`: `$shared`" }
    val lines = shared.lines()
    check("## In reply to" in lines && "question" in lines) { "the shared info has no `In reply to` section with bob's `question`: `$shared`" }
    check("## History" in lines && shared.endsWith(" (current)\nanswer")) { "the shared info has no history ending with the current `answer`: `$shared`" }
    val notesId = alice.chatList.chatId("Private notes")
    click(Ui.ChatList.Row(notesId))
    poll(what = "the notes open and the item info closed") { chatModel.chatId.value == notesId && !exists(Ui.ItemInfo.Share) && !seesText("Sent message") }
  }
}
