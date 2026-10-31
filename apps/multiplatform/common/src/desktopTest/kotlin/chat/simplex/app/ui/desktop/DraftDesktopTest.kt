package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test
import java.io.File
import kotlin.io.path.createTempDirectory

class DraftDesktopTest : UiTest() {
  @Test fun draft_withAttachment_restored() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val source = File(createTempDirectory("uitest-draft").toFile(), "report.txt").apply { writeText(REPORT) }
    val bobId = alice.chatList.chatId("bob"); val notesId = alice.chatList.chatId("Private notes")
    alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Cancel file preview")) { "a file preview is shown before the paste" }
    enterText(Ui.Chat.Message, source.absolutePath)
    poll(what = "the preview of report.txt") { seesDescription("Cancel file preview") && seesText("report.txt") }
    alice.chatList.open("Private notes")
    poll(what = "notes without the preview") { chatModel.chatId.value == notesId && exists(Ui.Chat.Message) && !seesDescription("Cancel file preview") }
    alice.chatList.open("bob")
    poll(what = "the preview restored") { chatModel.chatId.value == bobId && seesDescription("Cancel file preview") && seesText("report.txt") }
    click(Ui.Chat.Send)
    bob.eventuallyMessage("alice> sends file report.txt (${REPORT.length} bytes / ${REPORT.length} bytes)", CONNECT_TIMEOUT_MS)
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
  }

  private companion object {
    const val REPORT = "Quarterly report kept in a draft"
  }
}
