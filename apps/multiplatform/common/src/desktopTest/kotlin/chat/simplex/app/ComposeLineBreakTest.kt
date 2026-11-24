package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import chat.simplex.common.platform.PlatformTextField
import chat.simplex.common.platform.lineBreakOnEnter
import chat.simplex.common.views.chat.ComposeMessage
import chat.simplex.common.views.chat.ComposeState
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull

@OptIn(ExperimentalTestApi::class)
class ComposeLineBreakTest {
  @Test
  fun testBackslashBeforeCursor() {
    assertEquals(ComposeMessage("hi\n", TextRange(3)), ComposeMessage("hi\\").lineBreakOnEnter(shiftPressed = false))
    assertEquals(ComposeMessage("a\nb", TextRange(2)), ComposeMessage("a\\b", TextRange(2)).lineBreakOnEnter(shiftPressed = false))
    assertEquals(ComposeMessage("a\\\n", TextRange(3)), ComposeMessage("a\\\\").lineBreakOnEnter(shiftPressed = false))
  }

  @Test
  fun testShiftEnter() {
    assertEquals(ComposeMessage("hi\\\n", TextRange(4)), ComposeMessage("hi\\").lineBreakOnEnter(shiftPressed = true))
    assertEquals(ComposeMessage("say \n", TextRange(5)), ComposeMessage("say hi", TextRange(4, 6)).lineBreakOnEnter(shiftPressed = true))
  }

  @Test
  fun testEnterSends() {
    assertNull(ComposeMessage().lineBreakOnEnter(shiftPressed = false))
    assertNull(ComposeMessage("hi").lineBreakOnEnter(shiftPressed = false))
    assertNull(ComposeMessage("a\\b").lineBreakOnEnter(shiftPressed = false))
  }

  @Test
  fun testEnterAfterTypedBackslash() = runComposeUiTest {
    val composeState = mutableStateOf(ComposeState(message = ComposeMessage("hi"), useLinkPreviews = false))
    val textStyle = mutableStateOf(TextStyle())
    val showDeleteTextButton = mutableStateOf(false)
    var sent = false
    setContent {
      PlatformTextField(
        composeState,
        sendMsgEnabled = true,
        disabledText = null,
        sendMsgButtonDisabled = false,
        textStyle = textStyle,
        showDeleteTextButton = showDeleteTextButton,
        placeholder = "",
        showVoiceButton = false,
        onMessageChange = { composeState.value = composeState.value.copy(message = it) },
        onUpArrow = {},
        onFilesPasted = {},
        onDone = { sent = true }
      )
    }
    onNode(hasSetTextAction()).performTextInput("\\")
    onNode(hasSetTextAction()).performKeyInput { pressKey(Key.Enter) }
    assertEquals(ComposeMessage("hi\n", TextRange(3)), composeState.value.message)
    assertFalse(sent)
  }
}
