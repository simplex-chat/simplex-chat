package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import org.junit.Assert.assertFalse
import org.junit.Test

class ReplyTest : UiTest() {
  @Test fun reply_group_peerSeesMemberQuote() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    cath.eventually("#team: member bob is connected", CONNECT_TIMEOUT_MS)
    bob sends "#team $QUESTION"
    chat.menu(QUESTION, "reply")
    chat.send(ANSWER)
    bob.eventuallyMessage("#team alice!> > bob $QUESTION", CONNECT_TIMEOUT_MS)
    bob expects "      $ANSWER"
    cath.eventuallyMessage("#team alice> > bob $QUESTION", CONNECT_TIMEOUT_MS)
    cath expects "      $ANSWER"
    val answer = chat.itemId(ANSWER)
    poll(what = "alice's reply quoting `bob` / `$QUESTION`") {
      textsOf(Ui.Chat.Item(answer)).let { t -> "bob" in t && t.any { QUESTION in it } && t.any { ANSWER in it } }
    }
    chat.menu(ANSWER, "reply")
    chat.send(FOLLOW_UP)
    bob.eventuallyMessage("#team alice> > alice $ANSWER", CONNECT_TIMEOUT_MS)
    bob expects "      $FOLLOW_UP"
    cath.eventuallyMessage("#team alice> > alice $ANSWER", CONNECT_TIMEOUT_MS)
    cath expects "      $FOLLOW_UP"
    val followUp = chat.itemId(FOLLOW_UP)
    poll(what = "alice's follow-up quoting `alice` / `$ANSWER`") {
      textsOf(Ui.Chat.Item(followUp)).let { t -> "alice" in t && "bob" !in t && t.any { ANSWER in it } && t.any { FOLLOW_UP in it } }
    }
  }

  @Test fun reply_cancelContext() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    awaitRowText(alice, "bob", "hello")
    val chat = alice.chatList.open("bob")
    chat.itemId("hello")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Context icon")) { "a context bar is shown before Reply" }
    chat.menu("hello", "reply")
    poll(what = "the reply bar with `Cancel`") { seesDescription("Context icon") && exists(described("Cancel")) }
    typeInto(Ui.Chat.Message, "just text")
    hideKeyboard()
    click(described("Cancel"))
    poll(what = "the reply bar gone with `just text` kept in the field") { !seesDescription("Context icon") && editableText(Ui.Chat.Message) == "just text" }
    chat.sendComposed()
    bob.expectsMessage("alice> just text", CONNECT_TIMEOUT_MS)
  }

  @Test fun reply_whileEditing_replacesEdit() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice question"
    awaitRowText(alice, "bob", "question")
    val chat = alice.chatList.open("bob")
    chat.itemId("question")
    chat.send("draft answer")
    bob expectsMessage "alice> draft answer"
    val draft = chat.itemId("draft answer")
    chat.menu("draft answer", "edit")
    poll(what = "the edit bar with `draft answer` in the field") { editableText(Ui.Chat.Message) == "draft answer" && exists(described("Cancel")) }
    hideKeyboard()
    chat.menu("question", "reply")
    poll(what = "an empty field under the reply bar") { editableText(Ui.Chat.Message).isNullOrEmpty() && seesDescription("Context icon") && exists(described("Cancel")) }
    chat.send("final answer")
    bob.expectsMessage("alice> > question", CONNECT_TIMEOUT_MS)
    bob expects "      final answer"
    poll(what = "`draft answer` unchanged and not edited") { chat.item(draft)?.let { it.content.text == "draft answer" && !it.meta.itemEdited } == true }
  }

  @Test fun quote_click_deleted_showsNoMessageAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("the original")
    bob expectsMessage "alice> the original"
    bob types ">@alice (the original) the reply"
    bob expectsMessage "@alice > the original"
    bob expects "      the reply"
    val original = chat.itemId("the original")
    val reply = chat.itemId("the reply")
    hideKeyboard()
    click(Ui.Chat.Item(reply).part("quote"))
    poll(what = "the quote of an existing message handled, `the original` in view") { UiIdle.inFlight().isEmpty() && exists(Ui.Chat.Item(original)) }
    settle()
    assertFalse("the `No message` alert for the quote of an existing message", seesText("No message"))
    chat.menu("the original", "delete")
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "forMe"))
    poll(what = "`the original` deleted for alice") { chat.item(original) == null && !exists(Ui.Chat.Item(original)) }
    click(Ui.Chat.Item(reply).part("quote"))
    poll(what = "the `No message` alert for the quote of the deleted message") { seesText("No message") && seesText("This message was deleted or not received yet.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the `No message` alert closed") { !seesText("No message") }
  }

  private companion object {
    const val QUESTION = "hello! how are you?"
    const val ANSWER = "hello, all good, you?"
    const val FOLLOW_UP = "will tell more"
  }
}
