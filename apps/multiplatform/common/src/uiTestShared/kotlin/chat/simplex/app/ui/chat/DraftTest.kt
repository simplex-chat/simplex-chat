package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.app.ui.harness.visitNotesAndBack
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class DraftTest : UiTest() {
  @Test fun draft_onlyLastChatKept() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "for bob")
    backToChatList()
    alice.chatList.open("cath")
    poll(what = "cath's field empty, bob's draft not carried over") { chatModel.chatId.value == cathId && editableText(Ui.Chat.Message).isNullOrEmpty() }
    typeInto(Ui.Chat.Message, "for cath")
    switchAwayFromChat(alice)
    poll(what = "a draft on cath's row only") {
      alice.chatList.rowTexts("cath").any { it.endsWith(" for cath") } && alice.chatList.rowTexts("bob").none { it.endsWith(" for bob") }
    }
    alice.chatList.open("bob")
    poll(what = "bob's field empty, only the last draft kept") { chatModel.chatId.value == bobId && editableText(Ui.Chat.Message).isNullOrEmpty() }
    backToChatList()
    val chat = alice.chatList.open("cath")
    poll(what = "cath's draft restored") { chatModel.chatId.value == cathId && editableText(Ui.Chat.Message) == "for cath" }
    chat.sendComposed()
    cath.eventuallyMessage("alice> for cath", CONNECT_TIMEOUT_MS)
    poll(what = "cath's row with the sent text and no draft") {
      alice.chatList.rowTexts("cath").let { "for cath" in it && it.none { t -> t != "for cath" && t.endsWith(" for cath") } }
    }
  }

  @Test fun draft_withQuote_restored() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice question"
    awaitRowText(alice, "bob", "question")
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob expectsMessage "alice> hello"
    check(!seesDescription("Context icon")) { "a context bar is shown before Reply" }
    chat.menu("question", "reply")
    typeInto(Ui.Chat.Message, "answer")
    visitNotesAndBack(alice, "bob")
    poll(what = "the reply draft restored") { editableText(Ui.Chat.Message) == "answer" && seesDescription("Context icon") }
    chat.sendComposed()
    bob expectsMessage "alice> > question"
    bob expects "      answer"
    chat.menu("hello", "edit")
    poll(what = "the edited text in the field") { editableText(Ui.Chat.Message) == "hello" }
    replaceText(Ui.Chat.Message, "hello world")
    visitNotesAndBack(alice, "bob")
    poll(what = "the edit draft restored") { editableText(Ui.Chat.Message) == "hello world" && seesDescription("Context icon") }
    chat.sendComposed()
    bob expectsMessage "alice> [edited] hello world"
  }

  @Test fun draft_clearedWhenSendDisabled() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "for bob")
    bob sends "/d alice"
    bob expects "alice: contact is deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the field cleared once sending is impossible") {
      !alice.chatList.chat("bob").chatInfo.sendMsgEnabled && exists(Ui.Chat.Message) && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    backToChatList()
    alice.chatList.open("cath")
    poll(what = "no draft on bob's row") { chatModel.chatId.value == cathId && alice.chatList.rowTexts("bob").none { it.endsWith(" for bob") } }
    typeInto(Ui.Chat.Message, "for cath")
    backToChatList()
    alice.chatList.open("bob")
    poll(what = "bob's chat open") { chatModel.chatId.value == bobId }
    backToChatList()
    alice.chatList.open("cath")
    poll(what = "cath's draft kept through the visit to bob's chat") { chatModel.chatId.value == cathId && editableText(Ui.Chat.Message) == "for cath" }
  }
}
