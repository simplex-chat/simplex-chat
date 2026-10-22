package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.itemFileOf
import chat.simplex.app.ui.harness.sendsFileTo
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class CopyShareDesktopTest : UiTest() {
  @Test fun copy_text_toClipboardAndToast() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello there"
    bob sends "@alice second line"
    val chat = alice.chatList.open("bob")
    chat.itemId("hello there")
    chat.itemId("second line")
    val before = copiedToasts()
    setClipboardText("unrelated")
    chat.menu("hello there", "copy")
    poll(what = "`hello there` on the clipboard and the `Copied to clipboard` toast") { clipboardText() == "hello there" && copiedToasts() == before + 1 }
    setClipboardText("unrelated")
    chat.menu("second line", "share")
    poll(what = "`second line` on the clipboard from Share and a second toast") { clipboardText() == "second line" && copiedToasts() == before + 2 }
  }

  @Test fun copy_fileItem_copiesPath() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFileTo("alice", bob.createFile(NOTES_NAME, NOTES), 1, NOTES_NAME)
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("the item of $NOTES_NAME") { it.file?.fileName == NOTES_NAME }
    rightClick(Ui.Chat.Item(id))
    click(Ui.Chat.ItemMenu(id, "download"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$NOTES_NAME received") { itemFileOf(id)?.fileStatus is CIFileStatus.RcvComplete }
    check(itemFileOf(id)?.fileSource?.cryptoArgs != null) { "$NOTES_NAME is stored unencrypted" }
    val copied = copiedToasts()
    setClipboardText(null)
    rightClick(Ui.Chat.Item(id))
    click(Ui.Chat.ItemMenu(id, "copy"))
    poll(what = "the path of a readable copy of $NOTES_NAME on the clipboard and the `Copied to clipboard` toast") {
      clipboardText()?.let { File(it).isFile && File(it).readText() == NOTES } == true && copiedToasts() == copied + 1
    }
    val target = File(exchangeDir(), "shared-$NOTES_NAME")
    val saved = toasts().count { it == "File saved" }
    check(!target.exists()) { "${target.name} exists before Share" }
    rightClick(Ui.Chat.Item(id))
    saveFileAs(target) { click(Ui.Chat.ItemMenu(id, "share")) }
    poll(what = "the decrypted $NOTES_NAME written to ${target.name} and the `File saved` toast") {
      target.length() == NOTES.length.toLong() && toasts().count { it == "File saved" } == saved + 1
    }
    assertEquals(NOTES, target.readText())
  }

  private fun toasts(): List<String> = simplexWindowState.toasts.toList().map { it.first }

  private fun copiedToasts(): Int = toasts().count { it == "Copied to clipboard" }

  private companion object {
    const val NOTES_NAME = "copy-notes.md"
    const val NOTES = "Shopping list from bob: apples and pears"
  }
}
