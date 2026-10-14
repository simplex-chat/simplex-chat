package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class MessagingTest : UiTest() {
  @Test fun unreadCounterIncrementsAndClears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
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
    val chat = alice.chatList.open("bob")
    chat.menu("hello! how are you?", "reply")
    chat.send("all good - you?")
    bob expectsMessage "alice> > hello! how are you?"
    bob expects "      all good - you?"
  }

  @Test fun edit_menu_updatesPeer() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob expectsMessage "alice> hello"
    chat.menu("hello", "edit")
    chat.replaceAndSend("hey 👋")
    bob expectsMessage "alice> [edited] hey 👋"
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

  @Test fun reaction_add_peerSeesAdded() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(reactions = true))
    connect(bob)
    bob sends "@alice hi"
    alice.chatList.open("bob").react("hi", "👍")
    bob expectsMessage "alice> >> hi"
    bob expects "    + 👍"
  }

  @Test fun reaction_received_chipCount() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(reactions = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hi")
    bob expectsMessage "alice> hi"
    chat.react("hi", "👍")
    bob expectsMessage "alice> > hi"
    bob expects "    + 👍"
    bob types "+1 alice hi"
    poll(what = "reaction chip count 2") { chat.reactionCount("hi", "👍") == "2" }
  }
}
