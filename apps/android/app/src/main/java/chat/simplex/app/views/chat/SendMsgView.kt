package chat.simplex.app.views.chat

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Configuration
import android.text.InputType
import android.view.ViewGroup
import android.view.inputmethod.*
import android.widget.EditText
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.*
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.graphics.drawable.DrawableCompat
import androidx.core.view.inputmethod.EditorInfoCompat
import androidx.core.view.inputmethod.InputConnectionCompat
import androidx.core.widget.*
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.*

@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  showVoiceRecordIcon: Boolean,
  recState: RecordingState,
  recordVoiceButton: @Composable () -> Unit,
  sendMessage: () -> Unit,
  onMessageChange: (String) -> Unit,
  textStyle: MutableState<TextStyle>
) {
  Box(Modifier.padding(vertical = 8.dp)) {
    val cs = composeState.value
    val attachEnabled = !composeState.value.editing
    val showVoiceButton = cs.message.isEmpty() && showVoiceRecordIcon && attachEnabled &&
        (cs.preview is ComposePreview.NoPreview || (cs.preview is ComposePreview.VoicePreview && recState.isStarted))
    NativeKeyboard(composeState, textStyle, onMessageChange)
    // Disable clicks on text field
    if (cs.preview is ComposePreview.VoicePreview) {
      Box(Modifier.matchParentSize().clickable(enabled = false, onClick = { }))
    }
    Box(Modifier.align(Alignment.BottomEnd)) {
      when {
        cs.inProgress && (cs.preview is ComposePreview.ImagePreview || cs.preview is ComposePreview.VoicePreview || cs.preview is ComposePreview.FilePreview) -> {
          ProgressIndicator()
        }
        showVoiceButton -> recordVoiceButton()
        else -> {
          val icon = if (cs.editing) Icons.Filled.Check else Icons.Outlined.ArrowUpward
          val color = if (cs.sendEnabled()) MaterialTheme.colors.primary else HighOrLowlight
          SendTextButton(icon, color, cs.sendEnabled(), sendMessage)
        }
      }
    }
  }
}

@Composable
private fun NativeKeyboard(
  composeState: MutableState<ComposeState>,
  textStyle: MutableState<TextStyle>,
  onMessageChange: (String) -> Unit
) {
  val cs = composeState.value
  val textColor = MaterialTheme.colors.onBackground
  val tintColor = MaterialTheme.colors.secondary
  val padding = PaddingValues(12.dp, 7.dp, 45.dp, 0.dp)
  val paddingStart = with(LocalDensity.current) { 12.dp.roundToPx() }
  val paddingTop = with(LocalDensity.current) { 7.dp.roundToPx() }
  val paddingEnd = with(LocalDensity.current) { 45.dp.roundToPx() }
  val paddingBottom = with(LocalDensity.current) { 7.dp.roundToPx() }

  var showKeyboard by remember { mutableStateOf(false) }
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem is ComposeContextItem.QuotedItem) {
      delay(100)
      showKeyboard = true
    } else if (cs.contextItem is ComposeContextItem.EditingItem) {
      // Keyboard will not show up if we try to show it too fast
      delay(300)
      showKeyboard = true
    }
  }

  AndroidView(modifier = Modifier, factory = {
    val editText = @SuppressLint("AppCompatCustomView") object: EditText(it) {
      override fun setOnReceiveContentListener(
        mimeTypes: Array<out String>?,
        listener: android.view.OnReceiveContentListener?
      ) {
        super.setOnReceiveContentListener(mimeTypes, listener)
      }
      override fun onCreateInputConnection(editorInfo: EditorInfo): InputConnection {
        val connection = super.onCreateInputConnection(editorInfo)
        EditorInfoCompat.setContentMimeTypes(editorInfo, arrayOf("image/*"))
        val onCommit = InputConnectionCompat.OnCommitContentListener { inputContentInfo, _, _ ->
          try {
            inputContentInfo.requestPermission()
          } catch (e: Exception) {
            return@OnCommitContentListener false
          }
          SimplexApp.context.chatModel.sharedContent.value = SharedContent.Images("", listOf(inputContentInfo.contentUri))
          true
        }
        return InputConnectionCompat.createWrapper(connection, editorInfo, onCommit)
      }
    }
    editText.layoutParams = ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT)
    editText.maxLines = 16
    editText.inputType = InputType.TYPE_TEXT_FLAG_CAP_SENTENCES or editText.inputType
    editText.setTextColor(textColor.toArgb())
    editText.textSize = textStyle.value.fontSize.value
    val drawable = it.getDrawable(R.drawable.send_msg_view_background)!!
    DrawableCompat.setTint(drawable, tintColor.toArgb())
    editText.background = drawable
    editText.setPadding(paddingStart, paddingTop, paddingEnd, paddingBottom)
    editText.setText(cs.message)
    editText.textCursorDrawable?.let { DrawableCompat.setTint(it, HighOrLowlight.toArgb()) }
    editText.doOnTextChanged { text, _, _, _ -> onMessageChange(text.toString()) }
    editText.doAfterTextChanged { text -> if (composeState.value.preview is ComposePreview.VoicePreview && text.toString() != "") editText.setText("") }
    editText
  }) {
    it.setTextColor(textColor.toArgb())
    it.textSize = textStyle.value.fontSize.value
    DrawableCompat.setTint(it.background, tintColor.toArgb())
    it.isFocusable = composeState.value.preview !is ComposePreview.VoicePreview
    it.isFocusableInTouchMode = it.isFocusable
    if (cs.message != it.text.toString()) {
      it.setText(cs.message)
      // Set cursor to the end of the text
      it.setSelection(it.text.length)
    }
    if (showKeyboard) {
      it.requestFocus()
      val imm: InputMethodManager = SimplexApp.context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
      imm.showSoftInput(it, InputMethodManager.SHOW_IMPLICIT)
      showKeyboard = false
    }
  }
  if (composeState.value.preview is ComposePreview.VoicePreview) {
    Text(
      generalGetString(R.string.voice_message_send_text),
      Modifier.padding(padding),
      color = HighOrLowlight,
      style = textStyle.value.copy(fontStyle = FontStyle.Italic)
    )
  }
}

@Composable
private fun ProgressIndicator() {
  CircularProgressIndicator(Modifier.size(36.dp).padding(4.dp), color = HighOrLowlight, strokeWidth = 3.dp)
}

@Composable
private fun SendTextButton(icon: ImageVector, backgroundColor: Color, enabled: Boolean, sendMessage: () -> Unit) {
  IconButton(sendMessage, Modifier.size(36.dp), enabled = enabled) {
    Icon(
      icon,
      stringResource(R.string.icon_descr_send_message),
      tint = Color.White,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
        .clip(CircleShape)
        .background(backgroundColor)
    )
  }
}

@Composable
fun SendAudioButton(
  showStop: Boolean,
  allowedVoiceByPrefs: Boolean,
  clickEnabled: Boolean,
  interactionSource: MutableInteractionSource
) {
  val icon = when {
    showStop -> Icons.Filled.Stop
    allowedVoiceByPrefs -> Icons.Filled.KeyboardVoice
    else -> Icons.Outlined.KeyboardVoice
  }
  val iconTint = when {
    showStop -> MaterialTheme.colors.primary
    allowedVoiceByPrefs -> MaterialTheme.colors.primary
    else -> HighOrLowlight
  }
  IconButton({}, Modifier.size(36.dp), enabled = clickEnabled, interactionSource = interactionSource) {
    Icon(
      icon,
      stringResource(R.string.icon_descr_record_voice_message),
      tint = iconTint,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgView() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      showVoiceRecordIcon = false,
      recState = RecordingState.NotStarted,
      sendMessage = {},
      onMessageChange = { _ -> },
      recordVoiceButton = {},
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewEditing() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateEditing = ComposeState(editingItem = ChatItem.getSampleData(), useLinkPreviews = true)
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateEditing) },
      showVoiceRecordIcon = false,
      recState = RecordingState.NotStarted,
      sendMessage = {},
      onMessageChange = { _ -> },
      recordVoiceButton = {},
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewInProgress() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateInProgress = ComposeState(preview = ComposePreview.FilePreview("test.txt"), inProgress = true, useLinkPreviews = true)
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateInProgress) },
      showVoiceRecordIcon = false,
      recState = RecordingState.NotStarted,
      sendMessage = {},
      onMessageChange = { _ -> },
      recordVoiceButton = {},
      textStyle = textStyle
    )
  }
}
