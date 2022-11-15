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
import androidx.compose.material.ripple.rememberRipple
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.graphics.drawable.DrawableCompat
import androidx.core.view.inputmethod.EditorInfoCompat
import androidx.core.view.inputmethod.InputConnectionCompat
import androidx.core.widget.doOnTextChanged
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
  allowVoiceRecord: Boolean,
  sendMessage: () -> Unit,
  onMessageChange: (String) -> Unit,
  onAudioAdded: (String) -> Unit,
  showRecordingUi: (Boolean) -> Unit,
  textStyle: MutableState<TextStyle>
) {
  val cs = composeState.value
  val attachEnabled = !composeState.value.editing
  var showKeyboard by remember { mutableStateOf(false) }
  LaunchedEffect(cs.contextItem) {
    when (cs.contextItem) {
      is ComposeContextItem.QuotedItem -> {
        delay(100)
        showKeyboard = true
      }
      is ComposeContextItem.EditingItem -> {
        // Keyboard will not show up if we try to show it too fast
        delay(300)
        showKeyboard = true
      }
    }
  }
  val textColor = MaterialTheme.colors.onBackground
  val tintColor = MaterialTheme.colors.secondary
  val paddingStart = with(LocalDensity.current) { 12.dp.roundToPx() }
  val paddingTop = with(LocalDensity.current) { 7.dp.roundToPx() }
  val paddingEnd = with(LocalDensity.current) { 45.dp.roundToPx() }
  val paddingBottom = with(LocalDensity.current) { 7.dp.roundToPx() }

  Column(Modifier.padding(vertical = 8.dp)) {
    Box {
      val filePath = rememberSaveable { mutableStateOf(null as String?) }
      var recordingTimeRange by rememberSaveable(saver = LongRange.saver) { mutableStateOf(0L..0L) } // since..to
      val showVoiceButton = ((cs.message.isEmpty() || recordingTimeRange.first > 0L) && allowVoiceRecord && attachEnabled && cs.preview is ComposePreview.NoPreview) || filePath.value != null
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
        editText
      }) {
        it.setTextColor(textColor.toArgb())
        it.textSize = textStyle.value.fontSize.value
        DrawableCompat.setTint(it.background, tintColor.toArgb())
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
      Box(Modifier.align(Alignment.BottomEnd)) {
        val icon = if (cs.editing) Icons.Filled.Check else Icons.Outlined.ArrowUpward
        val color = if (cs.sendEnabled()) MaterialTheme.colors.primary else HighOrLowlight
        if (cs.inProgress
          && (cs.preview is ComposePreview.ImagePreview || cs.preview is ComposePreview.FilePreview)
        ) {
          CircularProgressIndicator(
            Modifier
              .size(36.dp)
              .padding(4.dp),
            color = HighOrLowlight,
            strokeWidth = 3.dp
          )
        } else if (!showVoiceButton) {
          Icon(
            icon,
            stringResource(R.string.icon_descr_send_message),
            tint = Color.White,
            modifier = Modifier
              .size(36.dp)
              .padding(4.dp)
              .clip(CircleShape)
              .background(color)
              .clickable {
                if (cs.sendEnabled()) {
                  sendMessage()
                }
              }
          )
        }
      }
      if (showVoiceButton) {
        Row(
          if (recordingTimeRange.first == 0L)
            Modifier.matchParentSize()
          else
            Modifier.clickable(false, onClick = {}).background(MaterialTheme.colors.background).matchParentSize(),
          verticalAlignment = Alignment.CenterVertically
        ) {
          val permissionsState = rememberMultiplePermissionsState(
            permissions = listOf(
              Manifest.permission.RECORD_AUDIO,
            )
          )
          val rec: Recorder = remember { RecorderNative(MAX_VOICE_SIZE_FOR_SENDING) }
          var now by remember { mutableStateOf(System.currentTimeMillis()) }
          LaunchedEffect(Unit) {
            while (isActive) {
              now = System.currentTimeMillis()
              delay(100)
            }
          }
          val recordingInProgress = rememberSaveable { mutableStateOf(false) }
          val stopRecordingAndAddAudio = {
            rec.stop(recordingInProgress)
            filePath.value?.let(onAudioAdded)
            recordingTimeRange = recordingTimeRange.first..System.currentTimeMillis()
          }
          val startStopRecording = {
            when {
              !permissionsState.allPermissionsGranted -> permissionsState.launchMultiplePermissionRequest()
              recordingInProgress.value -> stopRecordingAndAddAudio()
              filePath.value == null -> {
                showRecordingUi(true)
                recordingTimeRange = System.currentTimeMillis()..0L
                filePath.value = rec.start(recordingInProgress, stopRecordingAndAddAudio)
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
            onDispose {
              // Unlock orientation
              activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED
            }
          }
          val cleanUp = { remove: Boolean ->
            if (remove) filePath.value?.let { File(it).delete() }
            filePath.value = null
            recordingInProgress.value = false
            stopRecOnNextClick = false
            recordingTimeRange = 0L..0L
            showRecordingUi(false)
          }
          val interactionSource = interactionSourceWithTapDetection(
            onPress = {
              if (filePath.value == null) startStopRecording()
            },
            onClick = {
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
          if (recordingTimeRange.first != 0L) {
            Icon(
              Icons.Filled.DeleteForever,
              stringResource(R.string.delete_verb),
              tint = MaterialTheme.colors.primary,
              modifier = Modifier.size(28.dp).clickable { cleanUp(true) }
            )
            Spacer(Modifier.width(5.dp))
            Box {
              val diff = if (recordingTimeRange.last == 0L) now - recordingTimeRange.first else recordingTimeRange.last - recordingTimeRange.first
              val text = "%02d:%02d.%01d".format(diff / 1000 / 60, diff / 1000 % 60, diff % 1000 / 100)
              var maxWidth by remember { mutableStateOf(0.dp) }
              val density = LocalDensity.current.density
              Text(
                text,
                Modifier.onGloballyPositioned { maxWidth = (it.size.width / density).dp },
                style = TextStyle.Default.copy(fontSize = 20.sp, fontWeight = FontWeight.Bold),
                color = HighOrLowlight,
              )
              Text(
                text,
                Modifier.requiredWidth(((diff.toDouble() / MAX_VOICE_MILLIS_FOR_SENDING) * maxWidth.value).dp).clipToBounds(),
                style = TextStyle.Default.copy(fontSize = 20.sp, fontWeight = FontWeight.Bold),
                color = MaterialTheme.colors.primary,
                maxLines = 1,
                overflow = TextOverflow.Visible,
                softWrap = false,
              )
            }
          }
          Spacer(Modifier.weight(1f))
          Icon(
            if (recordingTimeRange.last != 0L) Icons.Outlined.ArrowUpward else if (stopRecOnNextClick) Icons.Default.Stop else Icons.Default.Mic,
            stringResource(R.string.icon_descr_record_audio),
            tint = MaterialTheme.colors.primary,
            modifier = Modifier
              .size(36.dp)
              .padding(4.dp)
              .clickable(
                onClick = {},
                role = Role.Button,
                interactionSource = interactionSource,
                indication = rememberRipple(bounded = false, radius = 24.dp)
              )
          )
          DisposableEffect(Unit) {
            onDispose {
              rec.stop(recordingInProgress)
              //cleanUp(true)
            }
          }
        }
      }
    }
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
      allowVoiceRecord = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = {},
      showRecordingUi = {},
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
      allowVoiceRecord = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = {},
      showRecordingUi = {},
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
      allowVoiceRecord = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      onAudioAdded = {},
      showRecordingUi = {},
      textStyle = textStyle
    )
  }
}
