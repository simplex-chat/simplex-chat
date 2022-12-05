package chat.simplex.app.views.chat

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.pm.ActivityInfo
import android.content.res.Configuration
import android.text.InputType
import android.view.ViewGroup
import android.view.inputmethod.*
import android.widget.EditText
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.*
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
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.*
import java.io.*

@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  showVoiceRecordIcon: Boolean,
  allowedVoiceByPrefs: Boolean,
  needToAllowVoiceToContact: Boolean,
  sendMessage: () -> Unit,
  onMessageChange: (String) -> Unit,
  onAudioAdded: (String, Int, Boolean) -> Unit,
  allowVoiceToContact: () -> Unit,
  showDisabledVoiceAlert: () -> Unit,
  textStyle: MutableState<TextStyle>
) {
  Column(Modifier.padding(vertical = 8.dp)) {
    Box {
      val cs = composeState.value
      val attachEnabled = !composeState.value.editing
      val filePath = rememberSaveable { mutableStateOf(null as String?) }
      var recordingTimeRange by rememberSaveable(saver = LongRange.saver) { mutableStateOf(0L..0L) } // since..to
      val showVoiceButton = ((cs.message.isEmpty() || recordingTimeRange.first > 0L) && showVoiceRecordIcon && attachEnabled && cs.preview is ComposePreview.NoPreview) || filePath.value != null
      Box(if (recordingTimeRange.first == 0L)
        Modifier
      else
        Modifier.clickable(false, onClick = {})
      ) {
        NativeKeyboard(composeState, textStyle, onMessageChange)
      }
      Box(Modifier.align(Alignment.BottomEnd)) {
        val icon = if (cs.editing) Icons.Filled.Check else Icons.Outlined.ArrowUpward
        val color = if (cs.sendEnabled()) MaterialTheme.colors.primary else HighOrLowlight
        if (cs.inProgress && (cs.preview is ComposePreview.ImagePreview || cs.preview is ComposePreview.VoicePreview || cs.preview is ComposePreview.FilePreview)) {
          CircularProgressIndicator(Modifier.size(36.dp).padding(4.dp), color = HighOrLowlight, strokeWidth = 3.dp)
        } else if (!showVoiceButton) {
          IconButton(sendMessage, Modifier.size(36.dp), enabled = cs.sendEnabled()) {
            Icon(
              icon,
              stringResource(R.string.icon_descr_send_message),
              tint = Color.White,
              modifier = Modifier
                .size(36.dp)
                .padding(4.dp)
                .clip(CircleShape)
                .background(color)
            )
          }
        } else {
          val permissionsState = rememberMultiplePermissionsState(
            permissions = listOf(
              Manifest.permission.RECORD_AUDIO,
            )
          )
          val rec: Recorder = remember { RecorderNative(MAX_VOICE_SIZE_FOR_SENDING) }
          val recordingInProgress: State<Boolean> = remember { rec.recordingInProgress }
          var now by remember { mutableStateOf(System.currentTimeMillis()) }
          LaunchedEffect(Unit) {
            while (isActive) {
              now = System.currentTimeMillis()
              if (recordingTimeRange.first != 0L && recordingInProgress.value && composeState.value.preview is ComposePreview.VoicePreview) {
                filePath.value?.let { onAudioAdded(it, (now - recordingTimeRange.first).toInt(), false) }
              }
              delay(100)
            }
          }
          val stopRecordingAndAddAudio: () -> Unit = {
            rec.stop()
            recordingTimeRange = recordingTimeRange.first..System.currentTimeMillis()
            filePath.value?.let { onAudioAdded(it, (recordingTimeRange.last - recordingTimeRange.first).toInt(), true) }
          }
          val startStopRecording: () -> Unit = {
            when {
              needToAllowVoiceToContact -> {
                AlertManager.shared.showAlertDialog(
                  title = generalGetString(R.string.allow_voice_messages_question),
                  text = generalGetString(R.string.you_need_to_allow_to_send_voice),
                  confirmText = generalGetString(R.string.allow_verb),
                  dismissText = generalGetString(R.string.cancel_verb),
                  onConfirm = allowVoiceToContact,
                )
              }
              !allowedVoiceByPrefs -> showDisabledVoiceAlert()
              !permissionsState.allPermissionsGranted -> permissionsState.launchMultiplePermissionRequest()
              recordingInProgress.value -> stopRecordingAndAddAudio()
              filePath.value == null -> {
                recordingTimeRange = System.currentTimeMillis()..0L
                filePath.value = rec.start(stopRecordingAndAddAudio)
                filePath.value?.let { onAudioAdded(it, (now - recordingTimeRange.first).toInt(), false) }
              }
            }
          }
          var stopRecOnNextClick by remember { mutableStateOf(false) }
          val context = LocalContext.current
          DisposableEffect(stopRecOnNextClick) {
            val activity = context as? Activity ?: return@DisposableEffect onDispose {}
            if (stopRecOnNextClick) {
              // Lock orientation to current orientation because screen rotation will break the recording
              activity.requestedOrientation = if (activity.resources.configuration.orientation == Configuration.ORIENTATION_PORTRAIT)
                ActivityInfo.SCREEN_ORIENTATION_PORTRAIT
              else
                ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE
            }
            // Unlock orientation
            onDispose { activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED }
          }
          val cleanUp = { remove: Boolean ->
            rec.stop()
            AudioPlayer.stop(filePath.value)
            if (remove) filePath.value?.let { File(it).delete() }
            filePath.value = null
            stopRecOnNextClick = false
            recordingTimeRange = 0L..0L
          }
          LaunchedEffect(cs.preview) {
            if (cs.preview !is ComposePreview.VoicePreview && filePath.value != null) {
              // Pressed on X icon in preview
              cleanUp(true)
            }
          }
          val interactionSource = interactionSourceWithTapDetection(
            // It's just a key for triggering dropping a state in the compose function. Without it
            // nothing will react on changed params like needToAllowVoiceToContact or allowedVoiceByPrefs
            needToAllowVoiceToContact.toString() + allowedVoiceByPrefs.toString(),
            onPress = {
              if (filePath.value == null) startStopRecording()
            },
            onClick = {
              // Voice not allowed or not granted voice record permission for the app
              if (!allowedVoiceByPrefs || !permissionsState.allPermissionsGranted) return@interactionSourceWithTapDetection
              if (!recordingInProgress.value && filePath.value != null) {
                sendMessage()
                cleanUp(false)
              } else if (stopRecOnNextClick) {
                stopRecordingAndAddAudio()
                stopRecOnNextClick = false
              } else {
                // tapped and didn't hold a finger
                stopRecOnNextClick = true
              }
            },
            onCancel = startStopRecording,
            onRelease = startStopRecording
          )
          val sendButtonModifier = if (recordingTimeRange.last != 0L)
            Modifier.clip(CircleShape).background(color)
          else
            Modifier
          IconButton({}, Modifier.size(36.dp), enabled = !cs.inProgress, interactionSource = interactionSource) {
            Icon(
              when {
                recordingTimeRange.last != 0L -> Icons.Outlined.ArrowUpward
                stopRecOnNextClick -> Icons.Filled.Stop
                allowedVoiceByPrefs -> Icons.Filled.KeyboardVoice
                else -> Icons.Outlined.KeyboardVoice
              },
              stringResource(R.string.icon_descr_record_voice_message),
              tint = when {
                recordingTimeRange.last != 0L -> Color.White
                stopRecOnNextClick -> MaterialTheme.colors.primary
                allowedVoiceByPrefs -> MaterialTheme.colors.primary
                else -> HighOrLowlight
              },
              modifier = Modifier
                .size(36.dp)
                .padding(4.dp)
                .then(sendButtonModifier)
            )
          }
          DisposableEffect(Unit) {
            onDispose {
              rec.stop()
            }
          }
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
      allowedVoiceByPrefs = false,
      needToAllowVoiceToContact = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = { _, _, _ -> },
      allowVoiceToContact = {},
      showDisabledVoiceAlert = {},
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
      allowedVoiceByPrefs = false,
      needToAllowVoiceToContact = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = { _, _, _ -> },
      allowVoiceToContact = {},
      showDisabledVoiceAlert = {},
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
      allowedVoiceByPrefs = false,
      needToAllowVoiceToContact = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = { _, _, _ -> },
      allowVoiceToContact = {},
      showDisabledVoiceAlert = {},
      textStyle = textStyle
    )
  }
}
