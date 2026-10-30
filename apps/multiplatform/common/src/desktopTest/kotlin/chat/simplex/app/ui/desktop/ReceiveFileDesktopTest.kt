package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.itemFileOf
import chat.simplex.app.ui.harness.onEdt
import chat.simplex.app.ui.harness.sendsFileTo
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class ReceiveFileDesktopTest : UiTest() {
  @Test fun rcvFile_clickCompleted_opensSaveDialog() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFileTo("alice", bob.createFile(LIST_NAME, LIST), 1, LIST_NAME)
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("the item of $LIST_NAME") { it.file?.fileName == LIST_NAME }
    val dialog = simplexWindowState.saveDialog
    check(itemFileOf(id)?.fileStatus is CIFileStatus.RcvInvitation && !dialog.isAwaiting) { "$LIST_NAME was received or a save dialog was open before the click" }
    click(Ui.Chat.ItemFile(id))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$LIST_NAME received and the save dialog opened for it") {
      itemFileOf(id)?.fileStatus is CIFileStatus.RcvComplete && dialog.isAwaiting
    }
    val first = File(exchangeDir(), "first-$LIST_NAME").apply { delete() }
    onEdt { dialog.onResult(first) }
    poll(what = "the received $LIST_NAME saved to ${first.name}") { !dialog.isAwaiting && first.length() == LIST.length.toLong() }
    assertEquals(LIST, first.readText())
    val second = File(exchangeDir(), "second-$LIST_NAME").apply { delete() }
    saveFileAs(second) { click(Ui.Chat.ItemFile(id)) }
    poll(what = "the completed $LIST_NAME saved again to ${second.name} after a click") { second.length() == LIST.length.toLong() }
    assertEquals(LIST, second.readText())
  }

  private companion object {
    const val LIST_NAME = "list.txt"
    const val LIST = "Shopping list from bob: apples and pears"
  }
}
