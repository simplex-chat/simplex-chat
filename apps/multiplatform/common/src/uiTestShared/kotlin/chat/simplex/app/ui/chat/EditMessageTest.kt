package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class EditMessageTest : UiTest() {
  @Test fun edit_menu_updatesPeer() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob expectsMessage "alice> hello"
    val id = chat.itemId("hello")
    chat.itemInfo("hello").run {
      val h = history()
      check(h.count { it == "(current)" } == 1 && h.indexOfFirst { it.startsWith("hello") } in 0 until h.indexOf("(current)") && h.none { "hey" in it }) { "unedited history: $h" }
      close()
    }
    waitFor(Ui.Chat.Message)
    assertFalse("edit bar before Edit", exists(described("Cancel")))
    chat.menu("hello", "edit")
    poll(what = "the edit bar with `hello` in the message field") { editableText(Ui.Chat.Message) == "hello" && exists(described("Cancel")) }
    hideKeyboard()
    click(described("Cancel"))
    poll(what = "the edit bar gone and the field empty") { !exists(described("Cancel")) && editableText(Ui.Chat.Message).isNullOrEmpty() }
    chat.menu("hello", "edit")
    poll(what = "`hello` in the message field") { editableText(Ui.Chat.Message) == "hello" }
    chat.replaceAndSend("hello")
    chat.menu("hello", "edit")
    chat.replaceAndSend("hey 👋")
    bob expectsMessage "alice> [edited] hey 👋"
    poll(what = "the same bubble showing `hey 👋`, not `hello`, edited in the model") {
      chat.item(id)?.let { it.content.text == "hey 👋" && it.meta.itemEdited } == true &&
        textsOf(Ui.Chat.Item(id)).let { t -> t.any { "hey 👋" in it } && t.none { "hello" in it } }
    }
    chat.itemInfo("hey 👋").run {
      val h = history()
      val newest = h.indexOfFirst { it.startsWith("hey 👋") }
      val current = h.indexOf("(current)")
      val old = h.indexOfFirst { it.startsWith("hello") }
      check(newest in 0 until current && current < old && h.count { it == "(current)" } == 1) { "edited history: $h" }
      close()
    }
  }

  @Test fun edit_notOffered_forReceivedOrForwarded() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice theirs"
    val chat = alice.chatList.open("bob")
    val received = chat.itemId("theirs")
    chat.send("mine")
    bob expectsMessage "alice> mine"
    val mine = chat.itemId("mine")
    chat.forwardTo("theirs", "bob")
    var forwarded = 0L
    poll(what = "alice's forwarded copy of `theirs`") {
      forwarded = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == "theirs" && it.meta.itemForwarded != null }?.id ?: 0L
      forwarded != 0L && exists(Ui.Chat.Item(forwarded))
    }
    hideKeyboard()
    assertTrue("Edit not offered on alice's own message", editOffered(mine))
    assertFalse("Edit offered on bob's message", editOffered(received))
    assertFalse("Edit offered on the forwarded message", editOffered(forwarded))
  }

  @Test fun edit_received_updatesBubble() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("hello")
    check(textsOf(Ui.Chat.Item(id)).any { "hello" in it }) { "bob's bubble does not show `hello` before the edit" }
    bob sends "!@alice hey"
    poll(what = "the edited bubble showing `hey`, not `hello`") {
      chat.item(id)?.let { it.content.text == "hey" && it.meta.itemEdited } == true &&
        textsOf(Ui.Chat.Item(id)).let { t -> t.any { "hey" in it } && t.none { "hello" in it } }
    }
  }

  private fun UiScope.editOffered(id: Long): Boolean {
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    val offered = exists(Ui.Chat.ItemMenu(id, "edit"))
    click(Ui.Chat.ItemMenu(id, "select"))
    waitFor(Ui.Chat.Selection.part("title"))
    escape()
    poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
    return offered
  }
}
