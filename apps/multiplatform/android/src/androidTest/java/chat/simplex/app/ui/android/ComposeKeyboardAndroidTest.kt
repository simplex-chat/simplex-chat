package chat.simplex.app.ui.android

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.ui.Ui
import org.junit.Test

class ComposeKeyboardAndroidTest : UiTest() {
  @Test fun compose_enterKeyInsertsNewline_sendButtonSends() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "hello")
    pressImeEnter()
    poll(what = "a line break after `hello` in the message field") { editableText(Ui.Chat.Message) == "hello\n" }
    check(chat.itemTexts().none { "hello" in it }) { "Enter sent the message: ${chat.itemTexts()}" }
    chat.send("there")
    bob.expectsMessage("alice> hello", CONNECT_TIMEOUT_MS)
    bob expects "there"
  }

  @Test fun compose_replyAndEdit_showKeyboard() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send(MINE)
    bob.expectsMessage("alice> $MINE", CONNECT_TIMEOUT_MS)
    bob sends "@alice $QUESTION"
    val question = chat.itemId(QUESTION)
    hideKeyboard()
    rightClick(Ui.Chat.Item(question))
    waitFor(Ui.Chat.ItemMenu(question, "reply"))
    check(!keyboardShown() && !exists(CANCEL_CONTEXT)) { "the soft keyboard or a compose context is open before Reply" }
    click(Ui.Chat.ItemMenu(question, "reply"))
    poll(what = "the soft keyboard to open for the reply") { keyboardShown() && exists(CANCEL_CONTEXT) }
    val keyboardTop = settledKeyboardTop()
    val field = boundsInWindow(Ui.Chat.Message)
    val quoted = boundsInWindow(Ui.Chat.Item(question))
    check(field.bottom <= keyboardTop && quoted.bottom <= keyboardTop) {
      "with the keyboard open (top at $keyboardTop px) the message field is at $field and bob's message at $quoted"
    }

    hideKeyboard()
    chat.menu(MINE, "edit")
    poll(what = "the soft keyboard to open for editing `$MINE`") { keyboardShown() && editableText(Ui.Chat.Message) == MINE }
  }

  @Test fun compose_editText_clearButtonForWrappedParagraph() = uiTest {
    val alice = app("alice")
    alice.chatList.open(NOTES)
    typeInto(Ui.Chat.Message, "short")
    poll(what = "`short` on one line of the message field") { messageLineCount() == 1 }
    settle()
    check(!exists(Ui.Chat.ClearText)) { "the clear-text button is shown for one line" }
    replaceText(Ui.Chat.Message, PARAGRAPH)
    poll(what = "the paragraph wrapped to four lines or more, with the clear-text button") { messageLineCount() >= 4 && exists(Ui.Chat.ClearText) }
    check(editableText(Ui.Chat.Message)?.contains('\n') == false) { "the paragraph has a line break" }
    hideKeyboard()
    click(Ui.Chat.ClearText)
    poll(what = "the message field cleared and the button gone") { editableText(Ui.Chat.Message).isNullOrEmpty() && !exists(Ui.Chat.ClearText) }
  }

  @Test fun attachSheet_keyboardOpen_fullyExpanded() = uiTest {
    val alice = app("alice")
    alice.chatList.open(NOTES)
    click(Ui.Chat.Message)
    poll(what = "the soft keyboard to open for the message field") { keyboardShown() }
    check(!seesDescription("Close sheet")) { "the attachment sheet is open before Attach" }
    click(described("Attach"))
    poll(what = "the attachment sheet with Camera, Image, Video and File") { seesDescription("Close sheet") && SHEET_BUTTONS.all { exists(it) } }
    SHEET_BUTTONS.forEach { waitStable(it) }
    val keyboardTop = settledKeyboardTop()
    val bottoms = SHEET_BUTTONS.associateWith { boundsInWindow(it).bottom }
    check(bottoms.values.all { it <= keyboardTop }) { "attachment buttons end at $bottoms, below the keyboard top at $keyboardTop px" }
  }

  private companion object {
    const val NOTES = "Private notes"
    const val MINE = "my message to edit"
    const val QUESTION = "question to reply to"
    val PARAGRAPH = "word ".repeat(60).trim()
    val SHEET_BUTTONS = listOf("camera", "image", "video", "file").map { Ui.Chat.Attach(it) }
    val CANCEL_CONTEXT = described("Cancel")
  }
}
