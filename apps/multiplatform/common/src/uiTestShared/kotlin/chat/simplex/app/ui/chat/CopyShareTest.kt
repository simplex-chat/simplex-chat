package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.itemFileOf
import chat.simplex.app.ui.harness.sendsFileTo
import chat.simplex.app.ui.harness.storedBytesOf
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class CopyShareTest : UiTest() {
  @Test fun save_encryptedFile_decrypts() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFileTo("alice", bob.createFile(NOTES_NAME, NOTES), 1, NOTES_NAME)
    val target = File(exchangeDir(), "saved-$NOTES_NAME").apply { delete() }
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("the item of $NOTES_NAME") { it.file?.fileName == NOTES_NAME }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "download"))
    check(!exists(Ui.Chat.ItemMenu(id, "copy")) && !exists(Ui.Chat.ItemMenu(id, "share")) && !exists(Ui.Chat.ItemMenu(id, "save"))) {
      "the menu of a file that is not downloaded offers Copy, Share or Save"
    }
    click(Ui.Chat.ItemMenu(id, "download"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$NOTES_NAME received") { itemFileOf(id)?.fileStatus is CIFileStatus.RcvComplete }
    val stored = itemFileOf(id)!!
    check(stored.fileSource?.cryptoArgs != null && !storedBytesOf(stored).contentEquals(NOTES.toByteArray())) { "$NOTES_NAME is stored unencrypted" }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "save"))
    check(exists(Ui.Chat.ItemMenu(id, "copy")) && exists(Ui.Chat.ItemMenu(id, "share")) && !exists(Ui.Chat.ItemMenu(id, "download"))) {
      "the menu of the downloaded file has no Copy or Share, or still offers Download"
    }
    check(!target.exists()) { "${target.name} exists before Save" }
    saveFileAs(target) { click(Ui.Chat.ItemMenu(id, "save")) }
    poll(what = "the decrypted $NOTES_NAME written to ${target.name}") { target.length() == NOTES.length.toLong() }
    assertEquals(NOTES, target.readText())
  }

  private companion object {
    const val NOTES_NAME = "save-notes.md"
    const val NOTES = "Shopping list from bob: apples and pears"
  }
}
