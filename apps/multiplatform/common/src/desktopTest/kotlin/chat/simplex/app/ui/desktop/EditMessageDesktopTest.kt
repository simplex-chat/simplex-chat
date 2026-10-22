package chat.simplex.app.ui.desktop

import androidx.compose.ui.input.key.Key
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performSemanticsAction
import androidx.compose.ui.test.pressKey
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File
import kotlin.io.path.createTempDirectory

class EditMessageDesktopTest : UiTest() {
  @Test fun edit_upArrow_editsLastMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("first")
    bob expectsMessage "alice> first"
    chat.send("second")
    bob expectsMessage "alice> second"
    bob sends "@alice from bob"
    chat.itemId("from bob")
    check(editableText(Ui.Chat.Message).isNullOrEmpty() && !exists(described("Cancel"))) { "the message field is not empty or already editing before Up" }
    pressUp(Ui.Chat.Message)
    poll(what = "the edit bar with alice's last message `second`") { editableText(Ui.Chat.Message) == "second" && exists(described("Cancel")) }
    chat.replaceAndSend("second!")
    bob expectsMessage "alice> [edited] second!"
    typeInto(Ui.Chat.Message, "x")
    pressUp(Ui.Chat.Message)
    assertTrue("Up with text in the field started editing", editableText(Ui.Chat.Message) == "x" && !exists(described("Cancel")))
  }

  @Test fun edit_ownFile_keepsFileWithoutPreview() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val dir = createTempDirectory("uitest-edit-file").toFile()
    val source = File(dir, "report.txt").apply { writeText(REPORT) }
    val chat = alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    enterText(Ui.Chat.Message, source.absolutePath)
    poll(what = "the report.txt preview with `Cancel file preview`") {
      seesDescription("Cancel file preview") && seesText("report.txt") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    typeInto(Ui.Chat.Message, "doc")
    click(Ui.Chat.Send)
    bob.eventuallyMessage("alice> doc", CONNECT_TIMEOUT_MS)
    bob expectsMessage "alice> sends file report.txt (${REPORT.length} bytes / ${REPORT.length} bytes)"
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
    val id = fileItemId("report.txt")
    poll(what = "the sent preview cleared and report.txt shown in the item") {
      !seesDescription("Cancel file preview") && "report.txt" in textsOf(Ui.Chat.Item(id))
    }
    val shown = texts().count { it == "report.txt" }
    rightClick(Ui.Chat.Item(id))
    click(Ui.Chat.ItemMenu(id, "edit"))
    poll(what = "the edit bar with `doc`, no compose file preview and no `Cancel file preview`") {
      editableText(Ui.Chat.Message) == "doc" && exists(described("Cancel")) && texts().count { it == "report.txt" } == shown && !seesDescription("Cancel file preview")
    }
    chat.replaceAndSend("new doc")
    bob expectsMessage "alice> [edited] new doc"
    poll(what = "the edited item `new doc` still a file item showing report.txt") {
      val item = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }
      item != null && item.content.msgContent is MsgContent.MCFile && item.content.text == "new doc" && "report.txt" in textsOf(Ui.Chat.Item(id))
    }
    chat.send("ping")
    bob expectsMessage "alice> ping"
  }

  private fun UiScope.pressUp(key: UiKey) {
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performSemanticsAction(SemanticsActions.RequestFocus) }
    ui.runOnUiThread { node.performKeyInput { pressKey(Key.DirectionUp) } }
    ui.waitForIdle()
  }

  private fun UiScope.fileItemId(name: String): Long {
    var id: Long? = null
    poll(what = "the item of file `$name`") {
      id = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.msgContent is MsgContent.MCFile && it.file?.fileName == name }?.id
      id?.let { exists(Ui.Chat.Item(it)) } == true
    }
    return id!!
  }

  private companion object {
    const val REPORT = "Quarterly report edited by Bob."
  }
}
