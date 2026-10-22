package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.CIDirection
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class NotesTest : UiTest() {
  @Test fun notes_sendText_saved() = uiTest {
    val alice = app("alice")
    val notes = alice.chatList.open("Private notes")
    notes.send("keep in mind")
    val id = notes.itemId("keep in mind")
    poll(what = "a local note") { notes.item(id)?.chatDir is CIDirection.LocalSnd }
    restart().chatList.open("Private notes").itemId("keep in mind")
  }

  @Test fun notes_editDelete_staysLocal() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val notes = alice.chatList.open("Private notes")
    notes.send("keep in mind")
    notes.send("ahoy!")
    notes.send("scratch")
    val ahoy = notes.itemId("ahoy!"); val scratch = notes.itemId("scratch")
    hideKeyboard()
    rightClick(Ui.Chat.Item(ahoy))
    waitFor(Ui.Chat.ItemMenu(ahoy, "edit"))
    assertFalse("a note offers Reply", exists(Ui.Chat.ItemMenu(ahoy, "reply")))
    click(Ui.Chat.ItemMenu(ahoy, "edit"))
    poll(what = "`ahoy!` in the message field") { editableText(Ui.Chat.Message) == "ahoy!" }
    notes.replaceAndSend("Greetings.")
    poll(what = "the same note showing `Greetings.` instead of `ahoy!`") {
      notes.item(ahoy)?.content?.text == "Greetings." && textsOf(Ui.Chat.Item(ahoy)).let { t -> t.any { "Greetings." in it } && t.none { "ahoy!" in it } }
    }
    hideKeyboard()
    notes.menu("scratch", "delete")
    poll(what = "the `Delete message?` alert with `Delete for me` and without `Delete for everyone`") { exists(FOR_ME) && !exists(FOR_EVERYONE) }
    click(FOR_ME)
    poll(what = "`scratch` deleted") { notes.item(scratch) == null && !exists(Ui.Chat.Item(scratch)) }
    val restarted = restart()
    val again = restarted.chatList.open("Private notes")
    again.itemId("Greetings.")
    poll(what = "after a restart `keep in mind` and `Greetings.`, without `ahoy!` and `scratch`") {
      again.itemTexts().let { it.containsAll(listOf("keep in mind", "Greetings.")) && "ahoy!" !in it && "scratch" !in it }
    }
    backToChatList()
    restarted.chatList.open("bob").send("sentinel")
    bob.expectsMessage("alice> sentinel", CONNECT_TIMEOUT_MS)
  }

  private companion object {
    val FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    val FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
  }
}
