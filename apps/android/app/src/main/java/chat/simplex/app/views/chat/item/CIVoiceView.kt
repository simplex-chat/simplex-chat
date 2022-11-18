package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawWithCache
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive

@Composable
fun CIVoiceView(
  durationSec: Int,
  file: CIFile?,
  edited: Boolean,
  sent: Boolean,
  hasText: Boolean,
  ci: ChatItem,
  metaColor: Color
) {
  Row(
    Modifier.padding(top = 4.dp, bottom = 6.dp, start = 6.dp, end = 6.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (file != null) {
      val context = LocalContext.current
      val filePath = remember(file.filePath, file.fileStatus) { getLoadedFilePath(context, file) }
      var brokenAudio by rememberSaveable(file.filePath) { mutableStateOf(false) }
      val audioPlaying = rememberSaveable(file.filePath) { mutableStateOf(false) }
      val audioInfo = remember(file.filePath) {
        file.audioInfo.value = file.audioInfo.value.copy(durationMs = durationSec * 1000)
        file.audioInfo
      }
      val play = play@{
        audioPlaying.value = AudioPlayer.start(filePath ?: return@play, audioInfo.value.progressMs) {
          // If you want to preserve the position after switching a track, remove this line
          audioInfo.value = ProgressAndDuration(0, audioInfo.value.durationMs)
          audioPlaying.value = false
        }
        brokenAudio = !audioPlaying.value
      }
      val pause = {
        audioInfo.value = ProgressAndDuration(AudioPlayer.pause(), audioInfo.value.durationMs)
        audioPlaying.value = false
      }
      AudioInfoUpdater(filePath, audioPlaying, audioInfo)

      val time = if (audioPlaying.value) audioInfo.value.progressMs else audioInfo.value.durationMs
      val minWidth = with(LocalDensity.current) { 45.sp.toDp() }
      val text = String.format("%02d:%02d", time / 1000 / 60, time / 1000 % 60)
      if (hasText) {
        FileIndicator(file, audioPlaying.value, sent, hasText, audioInfo, brokenAudio, play, pause)
        Text(
          text,
          Modifier
            .padding(start = 12.dp, end = 5.dp)
            .widthIn(min = minWidth),
          color = HighOrLowlight,
          fontSize = 16.sp,
          textAlign = TextAlign.Start,
          maxLines = 1
        )
      } else {
        if (sent) {
          Row {
            Row(verticalAlignment = Alignment.CenterVertically) {
              Spacer(Modifier.height(56.dp))
              Text(
                text,
                Modifier
                  .padding(end = 12.dp)
                  .widthIn(min = minWidth),
                color = HighOrLowlight,
                fontSize = 16.sp,
                maxLines = 1
              )
            }
            Column {
              FileIndicator(file, audioPlaying.value, sent, hasText, audioInfo, brokenAudio, play, pause)
              Box(Modifier.align(Alignment.CenterHorizontally).padding(top = 6.dp)) {
                CIMetaView(ci, metaColor)
              }
            }
          }
        } else {
          Row {
            Column {
              FileIndicator(file, audioPlaying.value, sent, hasText, audioInfo, brokenAudio, play, pause)
              Box(Modifier.align(Alignment.CenterHorizontally).padding(top = 6.dp)) {
                CIMetaView(ci, metaColor)
              }
            }
            Row(verticalAlignment = Alignment.CenterVertically) {
              Text(
                text,
                Modifier
                  .padding(start = 12.dp)
                  .widthIn(min = minWidth),
                color = HighOrLowlight,
                fontSize = 16.sp,
                maxLines = 1
              )
              Spacer(Modifier.height(56.dp))
            }
          }
        }
      }
    } else {
      FileIndicator(null, false, sent, hasText, null, false, {}, {})
      val metaReserve = if (edited)
        "                     "
      else
        "                 "
      Text(metaReserve)
    }
  }
}

@Composable
private fun PlayPauseButton(
  audioPlaying: Boolean,
  sent: Boolean,
  angle: Float,
  strokeWidth: Float,
  strokeColor: Color,
  enabled: Boolean,
  error: Boolean,
  play: () -> Unit,
  pause: () -> Unit
) {
  Surface(
    onClick = { if (!audioPlaying) play() else pause() },
    Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
    color = if (sent) SentColorLight else ReceivedColorLight,
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50))
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 56.dp, minHeight = 56.dp),
      contentAlignment = Alignment.Center
    ) {
      Icon(
        imageVector = if (audioPlaying) Icons.Filled.Pause else Icons.Filled.PlayArrow,
        contentDescription = null,
        Modifier.size(36.dp),
        tint = if (error) WarningOrange else if (!enabled) HighOrLowlight else MaterialTheme.colors.primary
      )
    }
  }
}

@Composable
private fun FileIndicator(
  file: CIFile?,
  audioPlaying: Boolean,
  sent: Boolean,
  hasText: Boolean,
  audioInfo: State<ProgressAndDuration>?,
  error: Boolean,
  play: () -> Unit,
  pause: () -> Unit
) {
  val strokeWidth = with(LocalDensity.current){ 3.dp.toPx() }
  val strokeColor = MaterialTheme.colors.primary
  if (file != null && file.loaded && audioInfo != null) {
    val angle = 360f * (audioInfo.value.progressMs.toDouble() / audioInfo.value.durationMs).toFloat()
    if (hasText) {
      IconButton({ if (!audioPlaying) play() else pause() }, Modifier.drawRingModifier(angle, strokeColor, strokeWidth)) {
        Icon(
          imageVector = if (audioPlaying) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          contentDescription = null,
          Modifier.size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
    } else {
      PlayPauseButton(audioPlaying, sent, angle, strokeWidth, strokeColor, true, error, play, pause)
    }
  } else {
    if (file?.fileStatus == CIFileStatus.RcvInvitation
      || file?.fileStatus == CIFileStatus.RcvTransfer
      || file?.fileStatus == CIFileStatus.RcvAccepted) {
      Box(
        Modifier
          .size(56.dp)
          .clip(RoundedCornerShape(4.dp)),
        contentAlignment = Alignment.Center
      ) {
        ProgressIndicator()
      }
    } else {
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, false, false, {}, {})
    }
  }
}

private fun Modifier.drawRingModifier(angle: Float, color: Color, strokeWidth: Float) = drawWithCache {
  val brush = Brush.linearGradient(
    0f to Color.Transparent,
    0f to color,
    start = Offset(0f, 0f),
    end = Offset(strokeWidth, strokeWidth),
    tileMode = TileMode.Clamp
  )
  onDrawWithContent {
    drawContent()
    drawArc(
      brush = brush,
      startAngle = -90f,
      sweepAngle = angle,
      useCenter = false,
      topLeft = Offset(strokeWidth / 2, strokeWidth / 2),
      size = Size(size.width - strokeWidth, size.height - strokeWidth),
      style = Stroke(width = strokeWidth, cap = StrokeCap.Square)
    )
  }
}

@Composable
private fun ProgressIndicator() {
  CircularProgressIndicator(
    Modifier.size(32.dp),
    color = if (isInDarkTheme()) FileDark else FileLight,
    strokeWidth = 4.dp
  )
}

@Composable
fun AudioInfoUpdater(
  filePath: String?,
  audioPlaying: MutableState<Boolean>,
  audioInfo: MutableState<ProgressAndDuration>
) {
  LaunchedEffect(filePath) {
    if (filePath != null && audioInfo.value.durationMs == 0) {
      audioInfo.value = ProgressAndDuration(audioInfo.value.progressMs, AudioPlayer.duration(filePath))
    }
  }
  LaunchedEffect(audioPlaying.value) {
    while (isActive && audioPlaying.value) {
      audioInfo.value = AudioPlayer.progressAndDurationOrEnded()
      if (audioInfo.value.progressMs == audioInfo.value.durationMs) {
        audioInfo.value = ProgressAndDuration(0, audioInfo.value.durationMs)
        audioPlaying.value = false
      }
      delay(50)
    }
  }
}
