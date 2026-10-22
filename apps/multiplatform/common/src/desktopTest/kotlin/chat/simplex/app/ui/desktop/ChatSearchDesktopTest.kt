package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.contentFilter
import chat.simplex.app.ui.harness.pickContentFilter
import chat.simplex.common.ui.Ui
import org.junit.Test
import java.io.File

class ChatSearchDesktopTest : UiTest() {
  @KnownAppIssue("15.5 item 28", "waiting for `Files` in the content filter menu after a file note was added")
  @Test fun contentFilter_fileAddedInOpenChat_listsFiles() = uiTest {
    val alice = app("alice")
    val source = File(exchangeDir(), "list.txt").apply { writeText("apples") }
    val notes = alice.chatList.open("Private notes")
    click(Ui.Chat.ContentFilter)
    poll(what = "`Images` and `Links` without `Files` in the content filter menu of the empty notes") {
      exists(contentFilter("Images")) && exists(contentFilter("Links")) && !exists(contentFilter("Files"))
    }
    click(contentFilter("Links"))
    waitFor(Ui.Chat.Bar.part("search"))
    pickContentFilter("all")
    poll(what = "the content filter off") { !exists(Ui.Chat.Bar.part("search")) }
    enterText(Ui.Chat.Message, source.absolutePath)
    poll(what = "the preview of list.txt") { seesDescription("Cancel file preview") && seesText("list.txt") }
    click(Ui.Chat.Send)
    notes.itemId("the note with list.txt") { it.file?.fileName == "list.txt" }
    click(Ui.Chat.ContentFilter)
    poll(what = "`Files` in the content filter menu after a file note was added") { exists(contentFilter("Files")) }
  }
}
