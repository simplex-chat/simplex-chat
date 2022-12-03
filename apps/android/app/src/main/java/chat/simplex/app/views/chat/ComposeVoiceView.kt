import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.durationText
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ComposeVoiceView(
  filePath: String,
  recordedDurationMs: Int,
  finishedRecording: Boolean,
  cancelEnabled: Boolean,
  cancelVoice: () -> Unit
) {
  BoxWithConstraints(Modifier
    .fillMaxWidth()
  ) {
    val audioPlaying = rememberSaveable { mutableStateOf(false) }
    val progress = rememberSaveable { mutableStateOf(0) }
    val duration = rememberSaveable(recordedDurationMs) { mutableStateOf(recordedDurationMs) }
    val progressBarWidth = remember { Animatable(0f) }
    LaunchedEffect(recordedDurationMs, finishedRecording) {
      snapshotFlow { progress.value }
        .distinctUntilChanged()
        .collect {
          val startTime = when {
            finishedRecording -> progress.value
            else -> recordedDurationMs
          }
          val endTime = when {
            finishedRecording -> duration.value
            audioPlaying.value -> recordedDurationMs
            else -> MAX_VOICE_MILLIS_FOR_SENDING.toInt()
          }
          val to = ((startTime.toDouble() / endTime) * maxWidth.value).dp
          progressBarWidth.animateTo(to.value, audioProgressBarAnimationSpec())
        }
    }
    Spacer(
      Modifier
        .requiredWidth(progressBarWidth.value.dp)
        .padding(top = 58.dp)
        .height(3.dp)
        .background(MaterialTheme.colors.primary)
    )
    Row(
      Modifier
        .height(60.dp)
        .fillMaxWidth()
        .padding(top = 8.dp)
        .background(SentColorLight),
      verticalAlignment = Alignment.CenterVertically
    ) {
      IconButton(
        onClick = {
          if (!audioPlaying.value) {
            AudioPlayer.play(filePath, audioPlaying, progress, duration, false)
          } else {
            AudioPlayer.pause(audioPlaying, progress)
          }
        },
        enabled = finishedRecording) {
        Icon(
          if (audioPlaying.value) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          stringResource(R.string.icon_descr_file),
          Modifier
            .padding(start = 4.dp, end = 2.dp)
            .size(36.dp),
          tint = if (finishedRecording) MaterialTheme.colors.primary else HighOrLowlight
        )
      }
      val numberInText = remember(recordedDurationMs, progress.value) {
        derivedStateOf {
          when {
            finishedRecording && progress.value == 0 && !audioPlaying.value -> duration.value / 1000
            finishedRecording -> progress.value / 1000
            else -> recordedDurationMs / 1000
          }
        }
      }
      Text(
        durationText(numberInText.value),
        fontSize = 18.sp,
        color = HighOrLowlight,
      )
      Spacer(Modifier.weight(1f))
      if (cancelEnabled) {
        IconButton(
          onClick = {
            AudioPlayer.stop(filePath)
            cancelVoice()
          },
          modifier = Modifier.padding(0.dp)
        ) {
          Icon(
            Icons.Outlined.Close,
            contentDescription = stringResource(R.string.icon_descr_cancel_file_preview),
            tint = MaterialTheme.colors.primary,
            modifier = Modifier.padding(10.dp)
          )
        }
      }
    }
  }
}

@Preview
@Composable
fun PreviewComposeAudioView() {
  SimpleXTheme {
    ComposeFileView(
      "test.txt",
      cancelFile = {},
      cancelEnabled = true
    )
  }
}
