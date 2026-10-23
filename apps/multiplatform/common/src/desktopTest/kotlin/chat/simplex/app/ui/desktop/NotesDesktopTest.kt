package chat.simplex.app.ui.desktop

import androidx.compose.ui.input.key.Key
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performSemanticsAction
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.withKeyDown
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.readCryptoFile
import chat.simplex.common.platform.getLoadedFilePath
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class NotesDesktopTest : UiTest() {
  @Test fun notes_pasteFile_storedLocally_deleteRemovesFile() = uiTest {
    val alice = app("alice")
    val source = File(exchangeDir(), "plan.txt").apply { writeText(PLAN) }
    val notes = alice.chatList.open("Private notes")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Cancel file preview") && !seesText("plan.txt")) { "a file preview is shown before the paste" }
    setClipboardText(source.absolutePath)
    pressCtrlV(Ui.Chat.Message)
    poll(what = "the preview of plan.txt, with the path not typed into the message field") {
      seesDescription("Cancel file preview") && seesText("plan.txt") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    click(Ui.Chat.Send)
    val id = notes.itemId("the note with plan.txt") { it.file?.fileName == "plan.txt" }
    var stored: File? = null
    poll(what = "plan.txt stored in the app's files") {
      stored = getLoadedFilePath(notes.item(id)?.file)?.let { File(it) }
      stored?.exists() == true
    }
    check(stored!!.canonicalPath != source.canonicalPath) { "the note points at the pasted file instead of a stored copy" }
    assertEquals(PLAN, storedText(notes.item(id)!!.file!!, stored!!))
    rightClick(Ui.Chat.Item(id))
    click(Ui.Chat.ItemMenu(id, "delete"))
    poll(what = "the `Delete message?` alert with `Delete for me` and without `Delete for everyone`") { exists(FOR_ME) && !exists(FOR_EVERYONE) }
    click(FOR_ME)
    poll(what = "the note and its stored copy of plan.txt deleted") { notes.item(id) == null && !exists(Ui.Chat.Item(id)) && !stored!!.exists() }
    assertEquals("the pasted source file changed", PLAN, source.readText())
  }

  private fun UiScope.pressCtrlV(key: UiKey) {
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performSemanticsAction(SemanticsActions.RequestFocus) }
    ui.runOnUiThread { node.performKeyInput { withKeyDown(Key.CtrlLeft) { pressKey(Key.V) } } }
    ui.waitForIdle()
  }

  private fun storedText(file: CIFile, stored: File): String {
    val args = file.fileSource?.cryptoArgs
    return (if (args != null) readCryptoFile(stored.absolutePath, args) else stored.readBytes()).toString(Charsets.UTF_8)
  }

  private companion object {
    const val PLAN = "Plan kept in private notes"
    val FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    val FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
  }
}
