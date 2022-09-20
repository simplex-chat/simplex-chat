package chat.simplex.app.views.chat

import android.Manifest
import android.content.Context
import android.content.res.Configuration
import android.net.Uri
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
import androidx.compose.material.ripple.rememberRipple
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
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.*
import java.io.*

@OptIn(ExperimentalComposeUiApi::class)
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
  val focusRequester = remember { FocusRequester() }
  val keyboard = LocalSoftwareKeyboardController.current
  var recordingTimeRange by remember { mutableStateOf(0L..0L) } // since..to
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem !is ComposeContextItem.QuotedItem) return@LaunchedEffect
    // In replying state
    focusRequester.requestFocus()
    delay(50)
    keyboard?.show()
  }

  Box {
    Row {
      BasicTextField(
        value = cs.message,
        enabled = recordingTimeRange.first == 0L || !allowVoiceRecord,
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
                  .height(36.dp)
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
              } else if ((recordingTimeRange.first == 0L && cs.message.isNotEmpty()) || !allowVoiceRecord) {
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
    }
    if (cs.message.isEmpty() && allowVoiceRecord) {
      Row(
        if (recordingTimeRange.first == 0L)
          Modifier.height(52.dp)
        else
          Modifier.clickable(false, onClick = {}).background(MaterialTheme.colors.background).height(52.dp),
        verticalAlignment = Alignment.CenterVertically
      ) {
        val permissionsState = rememberMultiplePermissionsState(
          permissions = listOf(
            Manifest.permission.RECORD_AUDIO,
          )
        )
        val rec: Recorder = remember { RecorderExternal() }
        var now by remember { mutableStateOf(System.currentTimeMillis()) }
        LaunchedEffect(Unit) {
          while (isActive) {
            now = System.currentTimeMillis()
            delay(100)
          }
        }
        val recordingInProgress = rememberSaveable { mutableStateOf(false) }
        val filePath = remember { mutableStateOf(null as String?) }
        val startStopRecording = {
          when {
            !permissionsState.allPermissionsGranted -> permissionsState.launchMultiplePermissionRequest()
            recordingInProgress.value -> {
              rec.stop(recordingInProgress)
              filePath.value?.let(onAudioAdded)
              recordingTimeRange = recordingTimeRange.first..System.currentTimeMillis()
            }
            filePath.value == null -> {
              showRecordingUi(true)
              filePath.value = rec.start(recordingInProgress)
              recordingTimeRange = System.currentTimeMillis()..0L
            }
          }
        }
        val cleanUp = { remove: Boolean ->
          if (remove) filePath.value?.let { File(it).delete() }
          filePath.value = null
          recordingInProgress.value = false
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
              return@interactionSourceWithTapDetection
            }
            Toast.makeText(SimplexApp.context, generalGetString(R.string.tap_and_hold_to_record), Toast.LENGTH_LONG).show()
            rec.cancel(filePath.value!!, recordingInProgress)
            cleanUp(true)
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
          val diff = if (recordingTimeRange.last == 0L) now - recordingTimeRange.first else recordingTimeRange.last - recordingTimeRange.first
          Text(
            "%02d:%02d.%01d".format(diff / 1000 / 60, diff / 1000 % 60, diff % 1000 / 100),
            style = TextStyle.Default.copy(fontSize = 20.sp, fontWeight = FontWeight.Bold),
            color = HighOrLowlight,
          )
        }
        Spacer(Modifier.weight(1f))
        Icon(
          if (recordingTimeRange.last != 0L) Icons.Outlined.ArrowUpward else Icons.Default.Mic,
          stringResource(R.string.icon_descr_record_audio),
          tint = MaterialTheme.colors.primary,
          modifier = Modifier
              .size(36.dp)
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
            cleanUp(true)
          }
        }
      }
    }
  }
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
