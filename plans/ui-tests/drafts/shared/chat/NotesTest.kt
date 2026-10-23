package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.CIDirection
import org.junit.Test

class NotesTest : UiTest() {
  @Test fun notes_sendText_saved() = uiTest {
    val alice = app("alice")
    val notes = alice.chatList.open("Private notes")
    notes.send("keep in mind")
    val id = notes.itemId("keep in mind")
    poll(what = "a local note") { notes.item(id)?.chatDir is CIDirection.LocalSnd }
  }
}
