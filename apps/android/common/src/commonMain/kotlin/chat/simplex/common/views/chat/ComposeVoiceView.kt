package chat.simplex.common.views.chat

import androidx.compose.animation.core.Animatable
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.durationText
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.AudioPlayer
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ComposeVoiceView(
  filePath: String,
  recordedDurationMs: Int,
  finishedRecording: Boolean,
  cancelEnabled: Boolean,
  cancelVoice: () -> Unit
) {
  val progress = rememberSaveable { mutableStateOf(0) }
  val duration = rememberSaveable(recordedDurationMs) { mutableStateOf(recordedDurationMs) }
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  Box {
    Box(
      Modifier
        .fillMaxWidth().padding(top = 22.dp)
    ) {
      val audioPlaying = rememberSaveable { mutableStateOf(false) }
      Row(
        Modifier
          .height(57.dp)
          .fillMaxWidth()
          .background(sentColor)
          .padding(top = 3.dp),
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
          enabled = finishedRecording
        ) {
          Icon(
            if (audioPlaying.value) painterResource(MR.images.ic_pause_filled) else painterResource(MR.images.ic_play_arrow_filled),
            stringResource(MR.strings.icon_descr_file),
            Modifier
              .padding(start = 4.dp, end = 2.dp)
              .size(36.dp),
            tint = if (finishedRecording) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
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
          color = MaterialTheme.colors.secondary,
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
              painterResource(MR.images.ic_close),
              contentDescription = stringResource(MR.strings.icon_descr_cancel_file_preview),
              tint = MaterialTheme.colors.primary,
              modifier = Modifier.padding(10.dp)
            )
          }
        }
      }
    }

    if (finishedRecording) {
      FinishedRecordingSlider(sentColor, progress, duration, filePath)
    } else {
      RecordingInProgressSlider(recordedDurationMs)
    }
  }
}

@Composable
fun FinishedRecordingSlider(backgroundColor: Color, progress: MutableState<Int>, duration: MutableState<Int>, filePath: String) {
  val dp4 = with(LocalDensity.current) { 4.dp.toPx() }
  val dp10 = with(LocalDensity.current) { 10.dp.toPx() }
  val primary = MaterialTheme.colors.primary
  val inactiveTrackColor = MaterialTheme.colors.primary.mixWith(
    backgroundColor.copy(1f).mixWith(MaterialTheme.colors.background, backgroundColor.alpha),
    0.24f)
  Slider(
    progress.value.toFloat(),
    onValueChange = { AudioPlayer.seekTo(it.toInt(), progress, filePath) },
    Modifier
      .fillMaxWidth()
      .drawBehind {
        drawRect(primary, Offset(0f, (size.height - dp4) / 2), size = androidx.compose.ui.geometry.Size(dp10, dp4))
        drawRect(inactiveTrackColor, Offset(size.width - dp10, (size.height - dp4) / 2), size = androidx.compose.ui.geometry.Size(dp10, dp4))
      },
    colors = SliderDefaults.colors(inactiveTrackColor = inactiveTrackColor),
    valueRange = 0f..duration.value.toFloat()
  )
}

@Composable
fun RecordingInProgressSlider(recordedDurationMs: Int) {
  val thumbPosition = remember { Animatable(0f) }
  val recDuration = rememberUpdatedState(recordedDurationMs)
  LaunchedEffect(Unit) {
    snapshotFlow { recDuration.value }
      .distinctUntilChanged()
      .collect {
        thumbPosition.animateTo(it.toFloat(), audioProgressBarAnimationSpec())
      }
  }
  val dp4 = with(LocalDensity.current) { 4.dp.toPx() }
  val dp10 = with(LocalDensity.current) { 10.dp.toPx() }
  val primary = MaterialTheme.colors.primary
  val inactiveTrackColor = Color.Transparent
  Slider(
    thumbPosition.value,
    onValueChange = {},
    Modifier
      .fillMaxWidth()
      .drawBehind {
        drawRect(primary, Offset(0f, (size.height - dp4) / 2), size = androidx.compose.ui.geometry.Size(dp10, dp4))
      },
    colors = SliderDefaults.colors(disabledInactiveTrackColor = inactiveTrackColor, disabledActiveTrackColor = primary, thumbColor = Color.Transparent, disabledThumbColor = Color.Transparent),
    enabled = false,
    valueRange = 0f..MAX_VOICE_MILLIS_FOR_SENDING.toFloat()
  )
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
