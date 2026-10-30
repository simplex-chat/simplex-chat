package chat.simplex.app.ui.desktop

import androidx.compose.ui.input.key.Key
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.withKeyDown
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.ui.Ui
import org.junit.Test

class TextSelectionDesktopTest : UiTest() {
  @Test fun textSelection_dragAcrossItems_copyButton() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    for (m in MESSAGES) bob sends "@alice $m"
    awaitRowText(alice, "bob", MESSAGES.last())
    val chat = alice.chatList.open("bob")
    val ids = MESSAGES.map { chat.itemId(it) }
    check(!exists(Ui.Chat.TextSelectionCopy)) { "the selection Copy button is shown before a selection" }
    setClipboardText(UNRELATED)
    drag(Ui.Chat.Item(ids[0]), Ui.Chat.Item(ids[2]))
    poll(what = "the selection Copy button after the drag") { exists(Ui.Chat.TextSelectionCopy) }
    check(clipboardText() == UNRELATED) { "the drag alone changed the clipboard to `${clipboardText()}`" }
    click(Ui.Chat.TextSelectionCopy)
    poll(what = "the first three messages on the clipboard, oldest first, and the Copy button gone") {
      clipboardText() == MESSAGES.take(3).joinToString("\n") && !exists(Ui.Chat.TextSelectionCopy)
    }
  }

  @Test fun textSelection_ctrlC_copies() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    for (m in MESSAGES) bob sends "@alice $m"
    awaitRowText(alice, "bob", MESSAGES.last())
    val chat = alice.chatList.open("bob")
    val ids = MESSAGES.map { chat.itemId(it) }
    setClipboardText(UNRELATED)
    drag(Ui.Chat.Message, Ui.Chat.Item(ids[1]))
    pressCtrlC()
    check(!exists(Ui.Chat.TextSelectionCopy) && clipboardText() == UNRELATED) { "a drag that started outside the messages selected text: clipboard `${clipboardText()}`" }
    drag(Ui.Chat.Item(ids[1]), Ui.Chat.Item(ids[1]))
    poll(what = "the selection Copy button after selecting `${MESSAGES[1]}`") { exists(Ui.Chat.TextSelectionCopy) }
    pressCtrlC()
    poll(what = "`${MESSAGES[1]}` on the clipboard after Ctrl+C") { clipboardText() == MESSAGES[1] }
    setClipboardText(UNRELATED)
    click(Ui.Chat.Item(ids[0]))
    poll(what = "the selection cleared by a click") { !exists(Ui.Chat.TextSelectionCopy) }
    pressCtrlC()
    check(clipboardText() == UNRELATED) { "Ctrl+C after the selection was cleared copied `${clipboardText()}`" }
  }

  private fun UiScope.pressCtrlC() {
    val root = ui.onRoot()
    ui.runOnUiThread { root.performKeyInput { withKeyDown(Key.CtrlLeft) { pressKey(Key.C) } } }
    settle()
  }

  private companion object {
    const val UNRELATED = "unrelated"
    val MESSAGES = listOf("first message here", "the middle one", "third and selected", "fourth stays out", "fifth stays out")
  }
}
