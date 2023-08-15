package chat.simplex.common.platform

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.input.key.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.input.*
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.delay
import kotlin.math.min
import kotlin.text.substring

@Composable
actual fun PlatformTextField(
  composeState: MutableState<ComposeState>,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  onMessageChange: (String) -> Unit,
  onUpArrow: () -> Unit,
  onDone: () -> Unit,
) {
  val cs = composeState.value
  val focusRequester = remember { FocusRequester() }
  val keyboard = LocalSoftwareKeyboardController.current
  val padding = PaddingValues(12.dp, 12.dp, 45.dp, 0.dp)
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem !is ComposeContextItem.QuotedItem) return@LaunchedEffect
    // In replying state
    focusRequester.requestFocus()
    delay(50)
    keyboard?.show()
  }
  val isRtl = remember(cs.message) { isRtl(cs.message.subSequence(0, min(50, cs.message.length))) }
  var textFieldValueState by remember { mutableStateOf(TextFieldValue(text = cs.message)) }
  val textFieldValue = textFieldValueState.copy(text = cs.message)
  val clipboard = LocalClipboardManager.current
  BasicTextField(
    value = textFieldValue,
    onValueChange = {
      if (!composeState.value.inProgress && !(composeState.value.preview is ComposePreview.VoicePreview && it.text != "")) {
        textFieldValueState = it
        onMessageChange(it.text)
      }
    },
    textStyle = textStyle.value,
    maxLines = 16,
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.Sentences,
      autoCorrect = true
    ),
    modifier = Modifier
      .padding(vertical = 4.dp)
      .focusRequester(focusRequester)
      .onPreviewKeyEvent {
        if (it.key == Key.Enter && it.type == KeyEventType.KeyDown) {
          if (it.isShiftPressed) {
            val start = if (minOf(textFieldValue.selection.min) == 0) "" else textFieldValue.text.substring(0 until textFieldValue.selection.min)
            val newText = start + "\n" +
                  textFieldValue.text.substring(textFieldValue.selection.max, textFieldValue.text.length)
            textFieldValueState = textFieldValue.copy(
              text = newText,
              selection = TextRange(textFieldValue.selection.min + 1)
            )
            onMessageChange(newText)
          } else if (cs.message.isNotEmpty()) {
            onDone()
          }
          true
        } else if (it.key == Key.DirectionUp && it.type == KeyEventType.KeyDown && cs.message.isEmpty()) {
          onUpArrow()
          true
        } else if (it.key.nativeKeyCode == 16778305 /*it.key == Key.C*/ && it.type == KeyEventType.KeyDown && it.isMetaPressed && desktopPlatform.isMac()) {
          if (textFieldValue.selection.min != textFieldValue.selection.max) {
            clipboard.setText(AnnotatedString(textFieldValue.getSelectedText().text))
          }
          true
        } else if (it.key.nativeKeyCode == 16778311 /*it.key == Key.X*/ && it.type == KeyEventType.KeyDown && it.isMetaPressed && desktopPlatform.isMac()) {
          if (textFieldValue.selection.min != textFieldValue.selection.max) {
            clipboard.setText(AnnotatedString(textFieldValue.getSelectedText().text))
            val newText = textFieldValue.getTextBeforeSelection(1_000_000) + textFieldValue.getTextAfterSelection(1_000_000)
            textFieldValueState = textFieldValue.copy(
              annotatedString = newText,
              selection = TextRange(textFieldValue.selection.min)
            )
            onMessageChange(newText.text)
          }
          true
        } else if (it.key.nativeKeyCode == 16778300/*it.key == Key.V*/ && it.type == KeyEventType.KeyDown && it.isMetaPressed && desktopPlatform.isMac()) {
          val clipboardText = clipboard.getText()
          if (clipboardText != null) {
            val newText = textFieldValue.getTextBeforeSelection(1_000_000) + clipboardText +
                textFieldValue.getTextAfterSelection(1_000_000)
            textFieldValueState = textFieldValue.copy(
              annotatedString = newText,
              selection = TextRange(textFieldValue.selection.min + clipboardText.length)
            )
            onMessageChange(newText.text)
          }
          true
        } else false
      },
    cursorBrush = SolidColor(MaterialTheme.colors.secondary),
    decorationBox = { innerTextField ->
      Surface(
        shape = RoundedCornerShape(18.dp),
        border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
      ) {
        Row(
          Modifier.background(MaterialTheme.colors.background),
          verticalAlignment = Alignment.Bottom
        ) {
          CompositionLocalProvider(
            LocalLayoutDirection provides if (isRtl) LayoutDirection.Rtl else LocalLayoutDirection.current
          ) {
            Column(Modifier.weight(1f).padding(start = 12.dp, end = 32.dp)) {
              Spacer(Modifier.height(8.dp))
              innerTextField()
              Spacer(Modifier.height(10.dp))
            }
          }
        }
      }
    }
  )
  showDeleteTextButton.value = cs.message.split("\n").size >= 4 && !cs.inProgress
  if (composeState.value.preview is ComposePreview.VoicePreview) {
    ComposeOverlay(MR.strings.voice_message_send_text, textStyle, padding)
  } else if (userIsObserver) {
    ComposeOverlay(MR.strings.you_are_observer, textStyle, padding)
  }
}

@Composable
private fun ComposeOverlay(textId: StringResource, textStyle: MutableState<TextStyle>, padding: PaddingValues) {
  Text(
    generalGetString(textId),
    Modifier.padding(padding),
    color = MaterialTheme.colors.secondary,
    style = textStyle.value.copy(fontStyle = FontStyle.Italic)
  )
}
