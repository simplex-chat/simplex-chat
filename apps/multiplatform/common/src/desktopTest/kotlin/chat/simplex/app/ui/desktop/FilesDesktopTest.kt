package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File
import kotlin.io.path.createTempDirectory

class FilesDesktopTest : UiTest() {
  @Test fun paste_filePathText_attachesFile() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val dir = createTempDirectory("uitest-paste").toFile()
    val source = File(dir, "report.txt").apply { writeText(REPORT) }
    alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Cancel file preview") && !seesText("report.txt")) { "a file preview is shown before the paste" }
    enterText(Ui.Chat.Message, source.absolutePath)
    poll(what = "the preview of report.txt with the path not left in the message field") {
      seesDescription("Cancel file preview") && seesText("report.txt") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    click(Ui.Chat.Send)
    bob.eventuallyMessage("alice> sends file report.txt (${REPORT.length} bytes / ${REPORT.length} bytes)", CONNECT_TIMEOUT_MS)
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
    val received = File(dir, "received.txt")
    bob sends "/fr 1 ${received.absolutePath}"
    bob.eventually("completed receiving file 1 (report.txt) from alice", CONNECT_TIMEOUT_MS)
    assertEquals(REPORT, received.readText())
  }

  private companion object {
    const val REPORT = "Quarterly report pasted by path"
  }
}
