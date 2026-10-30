package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowItem
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.sendsFileTo
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

private val ACTIONS = listOf("reply", "share", "copy", "save", "download", "cancelFile", "edit", "forward", "info", "delete", "reveal", "hide", "expand", "collapse", "moderate", "report", "archive", "select")

class ContextMenuTest : UiTest() {
  @Test fun contextMenu_itemsMatrix_byItemType() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice copy me"
    bob.sendsFileTo("alice", bob.createFile("offer.txt", "offered text"), 1, "offer.txt")
    bob sends "@alice to be deleted"
    bob sends "\\@alice to be deleted"
    bob expects "message marked deleted"
    awaitRowItem(alice, "bob", "bob's deleted message") { it.meta.itemDeleted != null }
    val chat = alice.chatList.open("bob")
    val text = chat.itemId("copy me")
    val file = chat.itemId("bob's offer of offer.txt") { it.file?.fileName == "offer.txt" }
    val deleted = chat.itemId("bob's `to be deleted` marked deleted") { it.content.text == "to be deleted" && it.meta.itemDeleted != null }
    setClipboardText("nothing copied yet")
    poll(what = "the clipboard holding the placeholder text") { clipboardText() == "nothing copied yet" }
    assertEquals("menu of bob's text", setOf("reply", "share", "copy", "forward", "info", "delete", "select"), menuOf(text))
    click(Ui.Chat.ItemMenu(text, "copy"))
    poll(what = "`copy me` on the clipboard after Copy") { clipboardText() == "copy me" }
    assertEquals("menu of bob's file offer", setOf("reply", "download", "info", "delete", "select"), menuOf(file))
    closeBySelecting(file)
    assertEquals("menu of bob's message marked deleted", setOf("reveal", "info", "delete", "select"), menuOf(deleted))
    closeBySelecting(deleted)
  }

  private fun UiScope.menuOf(id: Long): Set<String> {
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    return ACTIONS.filter { exists(Ui.Chat.ItemMenu(id, it)) }.toSet()
  }

  private fun UiScope.closeBySelecting(id: Long) {
    click(Ui.Chat.ItemMenu(id, "select"))
    waitFor(Ui.Chat.Selection.part("title"))
    escape()
    poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
  }
}
