package chat.simplex.app.views.chat

import android.Manifest
import android.content.Context
import android.content.res.Configuration
import android.media.MediaRecorder
import android.net.Uri
import android.os.Build
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import java.io.BufferedOutputStream
import java.io.File

@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  sendMessage: () -> Unit,
  onMessageChange: (String) -> Unit,
  textStyle: MutableState<TextStyle>
) {
  val cs = composeState.value
  val focusRequester = remember { FocusRequester() }
  val keyboard = LocalSoftwareKeyboardController.current
  val recordingInProgress = rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem !is ComposeContextItem.QuotedItem) return@LaunchedEffect
    // In replying state
    focusRequester.requestFocus()
    delay(50)
    keyboard?.show()
  }

  Row {
    BasicTextField(
      value = cs.message,
      onValueChange = onMessageChange,
      textStyle = textStyle.value,
      maxLines = 16,
      keyboardOptions = KeyboardOptions.Default.copy(
        capitalization = KeyboardCapitalization.Sentences,
        autoCorrect = true
      ),
      modifier = Modifier.weight(1f).padding(vertical = 8.dp).focusRequester(focusRequester),
      cursorBrush = SolidColor(HighOrLowlight),
      decorationBox = { innerTextField ->
        Surface(
          shape = RoundedCornerShape(18.dp),
          border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
        ) {
          Row(
            Modifier.background(MaterialTheme.colors.background),
            verticalAlignment = Alignment.Bottom
          ) {
            Box(
              Modifier
                .weight(1f)
                .padding(horizontal = 12.dp)
                .padding(top = 5.dp)
                .padding(bottom = 7.dp)
            ) {
              innerTextField()
            }
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
            } else if (!recordingInProgress.value && cs.message.isNotEmpty()) {
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
        }
      }
    )
    if (cs.message.isEmpty()) {
      Row(Modifier.wrapContentWidth()) {
        val permissionsState = rememberMultiplePermissionsState(
          permissions = listOf(
            Manifest.permission.RECORD_AUDIO,
          )
        )
        val recorder = remember {
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            MediaRecorder(SimplexApp.context)
          } else {
            MediaRecorder()
          }
        }
        var startedRecording by remember { mutableStateOf(System.currentTimeMillis()) }
        var now by remember { mutableStateOf(System.currentTimeMillis()) }
        LaunchedEffect(Unit) {
          while (isActive) {
            now = System.currentTimeMillis()
            delay(100)
          }
        }
        val filePath = remember { mutableStateOf("" as String?) }
        val saveFileLauncher = rememberSaveFileLauncher(cxt = SimplexApp.context, filePath)
        val channel = remember { mutableStateOf(1) }
        val encoder = remember { mutableStateOf("OPUS") }
        val sampleRate = remember { mutableStateOf(8000) }
        val bitRate = remember { mutableStateOf(8000) }
        val startStopRecording = {
          if (!permissionsState.allPermissionsGranted) {
            permissionsState.launchMultiplePermissionRequest()
          } else {
            if (recordingInProgress.value) {
              stopRecording(recorder, recordingInProgress)
              saveFileLauncher.launch(filePath.value!!.substringAfterLast("/"))
            } else {
              startedRecording = System.currentTimeMillis()
              filePath.value = startRecording(channel.value, encoder.value, sampleRate.value, bitRate.value, recorder, recordingInProgress)
            }
          }
        }
        val interactionSource = interactionSourceWithTapDetection(
          onPress = {
            if (filePath.value != null && filePath.value!!.isNotEmpty()) {
              File(filePath.value).delete()
              filePath.value = ""
            }
            startStopRecording()
          },
          onClick = {
            if (!recordingInProgress.value) return@interactionSourceWithTapDetection
            Toast.makeText(SimplexApp.context, generalGetString(R.string.tap_and_hold_to_record), Toast.LENGTH_LONG).show()
            cancelRecording(filePath.value!!, recorder, recordingInProgress)
            filePath.value = ""
          },
          onCancel = {
          },
          onRelease = {
            if (!recordingInProgress.value) return@interactionSourceWithTapDetection
            startStopRecording()
          }
        )
        if (!recordingInProgress.value) {
          val channels by remember { mutableStateOf(listOf(1, 2)) }
          AnySettingRow(channel, channels, !recordingInProgress.value)
          Spacer(Modifier.width(10.dp))
          val encoders by remember { mutableStateOf(listOf("AMR_NB", "AMR_WB", "AAC", "HE_AAC", "AAC_ELD", "OPUS")) }
          AnySettingRow(encoder, encoders, !recordingInProgress.value)
          Spacer(Modifier.width(10.dp))
          val sampleRates by remember { mutableStateOf(listOf(800, 1600, 4000, 8000, 16000, 24000, 48000)) }
          AnySettingRow(sampleRate, sampleRates, !recordingInProgress.value)
          Spacer(Modifier.width(10.dp))
          val bitRates by remember { mutableStateOf(listOf(4000, 8000, 12000, 16000, 24000, 48000)) }
          AnySettingRow(bitRate, bitRates, !recordingInProgress.value)
          Spacer(Modifier.width(10.dp))
        } else {
          Text("${now - startedRecording} ms                 ")
        }
        IconButton({}, interactionSource = interactionSource) {
          Icon(
            if (recordingInProgress.value) Icons.Default.Send else Icons.Default.Mic,
            stringResource(R.string.icon_descr_record_audio),
            tint = MaterialTheme.colors.primary,
            modifier = Modifier
              .size(40.dp)
          )
        }
      }
    }
  }
}

private fun startRecording(channels: Int, encoder: String, sampleRate: Int, bitRate: Int, recorder: MediaRecorder, recordingInProgress: MutableState<Boolean>): String {
  recordingInProgress.value = true
  val format = when (encoder) {
    "AMR_NB" -> MediaRecorder.OutputFormat.AMR_NB
    "AMR_WB" -> MediaRecorder.OutputFormat.AMR_WB
    "AAC" -> MediaRecorder.OutputFormat.MPEG_4
    "HE_AAC" -> MediaRecorder.OutputFormat.MPEG_4
    "AAC_ELD" -> MediaRecorder.OutputFormat.MPEG_4
    else -> MediaRecorder.OutputFormat.OGG
  }
  val enc = when (encoder) {
    "AMR_NB" -> MediaRecorder.AudioEncoder.AMR_NB
    "AMR_WB" -> MediaRecorder.AudioEncoder.AMR_WB
    "AAC" -> MediaRecorder.AudioEncoder.AAC
    "HE_AAC" -> MediaRecorder.AudioEncoder.HE_AAC
    "AAC_ELD" -> MediaRecorder.AudioEncoder.AAC_ELD
    else -> MediaRecorder.AudioEncoder.OPUS
  }
  val ext = when (encoder) {
    "AMR_NB" -> "amr"
    "AMR_WB" -> "amr"
    "AAC" -> "m4a"
    "HE_AAC" -> "m4a"
    "AAC_ELD" -> "m4a"
    else -> "ogg"
  }
  recorder.setAudioSource(MediaRecorder.AudioSource.MIC)
  recorder.setOutputFormat(format)
  recorder.setAudioEncoder(enc)
  // Mono
  recorder.setAudioChannels(channels)
  recorder.setAudioSamplingRate(sampleRate)
  recorder.setAudioEncodingBitRate(bitRate)
  recorder.setMaxDuration(-1)

  val filePath = getAppFilePath(SimplexApp.context, uniqueCombine(SimplexApp.context, getAppFilePath(SimplexApp.context, "$encoder-$sampleRate-$bitRate-$channels-voice.$ext")))
  recorder.setOutputFile(filePath)
  recorder.prepare()
  recorder.start()
  return filePath
}

private fun stopRecording(recorder: MediaRecorder, recordingInProgress: MutableState<Boolean>) {
  recordingInProgress.value = false
  runCatching {
    recorder.stop()
  }
  runCatching {
    recorder.reset()
  }
}

private fun cancelRecording(filePath: String, recorder: MediaRecorder, recordingInProgress: MutableState<Boolean>) {
  stopRecording(recorder, recordingInProgress)
  runCatching { File(filePath).delete() }.getOrElse { Log.d(TAG, "Unable to delete a file: ${it.stackTraceToString()}") }
}

@Composable
fun <T> AnySettingRow(selection: MutableState<T>, values: List<T>, enabled: Boolean) {
  var expanded by remember { mutableStateOf(false) }
  ExposedDropdownMenuBox(
    expanded = expanded,
    onExpandedChange = {
      expanded = !expanded
    }
  ) {
    Row {
      Text(
        selection.value.toString(),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = HighOrLowlight
      )
      Spacer(Modifier.size(4.dp))
      Icon(
        if (!expanded) Icons.Outlined.ExpandMore else Icons.Outlined.ExpandLess,
        null,
        modifier = Modifier.padding(start = 8.dp),
        tint = HighOrLowlight
      )
    }
    ExposedDropdownMenu(
      expanded = expanded && enabled,
      onDismissRequest = {
        expanded = false
      }
    ) {
      values.forEach { selectionOption ->
        DropdownMenuItem(
          onClick = {
            selection.value = selectionOption
            expanded = false
          }
        ) {
          Text(
            selectionOption.toString(),
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
          )
        }
      }
    }
  }
}

@Composable
private fun rememberSaveFileLauncher(cxt: Context, chatArchiveFile: MutableState<String?>): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      try {
        destination?.let {
          val filePath = chatArchiveFile.value
          if (filePath != null) {
            val contentResolver = cxt.contentResolver
            contentResolver.openOutputStream(destination)?.let { stream ->
              val outputStream = BufferedOutputStream(stream)
              val file = File(filePath)
              outputStream.write(file.readBytes())
              outputStream.close()
              Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
            }
          } else {
            Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
          }
        }
      } catch (e: Error) {
        Toast.makeText(cxt, generalGetString(R.string.error_saving_file), Toast.LENGTH_SHORT).show()
        Log.e(TAG, "rememberSaveFileLauncher error saving file $e")
      } finally {
        chatArchiveFile.value = null
      }
    }
  )

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
      sendMessage = {},
      onMessageChange = { _ -> },
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
      sendMessage = {},
      onMessageChange = { _ -> },
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
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}
