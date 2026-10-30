package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class MessagingTest : UiTest() {
  @Test fun unreadCounterIncrementsAndClears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    assertFalse("1" in alice.chatList.rowTexts("bob"))
    bob sends "@alice one"
    poll(what = "unread badge 1") { "1" in alice.chatList.rowTexts("bob") }
    bob sends "@alice two"
    poll(what = "unread badge 2") { "2" in alice.chatList.rowTexts("bob") }
    alice.chatList.open("bob")
    poll(what = "unread badge gone") { alice.chatList.rowTexts("bob").none { it == "1" || it == "2" } }
  }

  @Test fun reply_menu_sendQuoted_peerSeesQuote() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello! how are you?"
    awaitRowText(alice, "bob", "hello! how are you?")
    val chat = alice.chatList.open("bob")
    chat.menu("hello! how are you?", "reply")
    chat.send("all good - you?")
    bob expectsMessage "alice> > hello! how are you?"
    bob expects "      all good - you?"
    val reply = chat.itemId("all good - you?")
    poll(what = "alice's reply quoting `hello! how are you?`") {
      textsOf(Ui.Chat.Item(reply)).let { t -> t.any { "hello! how are you?" in it } && t.any { "all good - you?" in it } }
    }
  }

  @Test fun delete_ownForEveryone_peerSeesMarkedDeleted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob expectsMessage "alice> hello"
    chat.menu("hello", "delete")
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone"))
    bob expectsMessage "alice> [marked deleted] hello"
  }

  @Test fun reaction_received_chipCount() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(reactions = true))
    connect(bob)
    bob sends "@alice hi"
    awaitRowText(alice, "bob", "hi")
    val chat = alice.chatList.open("bob")
    chat.react("hi", "👍")
    bob expectsMessage "alice> >> hi"
    bob expects "    + 👍"
    poll(what = "alice's 👍 chip on `hi` without a count") { exists(Ui.Chat.ReactionChip(chat.itemId("hi"), "👍")) && chat.reactionCount("hi", "👍") == null }
    bob types "+1 alice hi"
    poll(what = "reaction chip count 2") { chat.reactionCount("hi", "👍") == "2" }
  }
}
