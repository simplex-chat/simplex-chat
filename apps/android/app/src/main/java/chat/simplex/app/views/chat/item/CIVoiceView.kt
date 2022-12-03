package chat.simplex.app.views.chat.item

import androidx.compose.foundation.combinedClickable
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
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.*
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

// TODO refactor https://github.com/simplex-chat/simplex-chat/pull/1451#discussion_r1033429901

@Composable
fun CIVoiceView(
  providedDurationSec: Int,
  file: CIFile?,
  edited: Boolean,
  sent: Boolean,
  hasText: Boolean,
  ci: ChatItem,
  longClick: () -> Unit,
) {
  Row(
    Modifier.padding(top = if (hasText) 14.dp else 4.dp, bottom = if (hasText) 14.dp else 6.dp, start = 6.dp, end = 6.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (file != null) {
      val context = LocalContext.current
      val filePath = remember(file.filePath, file.fileStatus) { getLoadedFilePath(context, file) }
      var brokenAudio by rememberSaveable(file.filePath) { mutableStateOf(false) }
      val audioPlaying = rememberSaveable(file.filePath) { mutableStateOf(false) }
      val progress = rememberSaveable(file.filePath) { mutableStateOf(0) }
      val duration = rememberSaveable(file.filePath) { mutableStateOf(providedDurationSec * 1000) }
      val play = {
        AudioPlayer.play(filePath, audioPlaying, progress, duration, true)
        brokenAudio = !audioPlaying.value
      }
      val pause = {
        AudioPlayer.pause(audioPlaying, progress)
      }
      val text = remember {
        derivedStateOf {
          val time = when {
            audioPlaying.value || progress.value != 0 -> progress.value
            else -> duration.value
          }
          durationText(time / 1000)
        }
      }
      VoiceLayout(file, ci, text, audioPlaying, progress, duration, brokenAudio, sent, hasText, play, pause, longClick)
    } else {
      VoiceMsgIndicator(null, false, sent, hasText, null, null, false, {}, {}, longClick)
      val metaReserve = if (edited)
        "                     "
      else
        "                 "
      Text(metaReserve)
    }
  }
}

@Composable
private fun VoiceLayout(
  file: CIFile,
  ci: ChatItem,
  text: State<String>,
  audioPlaying: State<Boolean>,
  progress: State<Int>,
  duration: State<Int>,
  brokenAudio: Boolean,
  sent: Boolean,
  hasText: Boolean,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit
) {
  when {
    hasText -> {
      Spacer(Modifier.width(6.dp))
      VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick)
      DurationText(text, PaddingValues(start = 12.dp))
    }
    sent -> {
      Row {
        Row(verticalAlignment = Alignment.CenterVertically) {
          Spacer(Modifier.height(56.dp))
          DurationText(text, PaddingValues(end = 12.dp))
        }
        Column {
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick)
          Box(Modifier.align(Alignment.CenterHorizontally).padding(top = 6.dp)) {
            CIMetaView(ci)
          }
        }
      }
    }
    else -> {
      Row {
        Column {
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick)
          Box(Modifier.align(Alignment.CenterHorizontally).padding(top = 6.dp)) {
            CIMetaView(ci)
          }
        }
        Row(verticalAlignment = Alignment.CenterVertically) {
          DurationText(text, PaddingValues(start = 12.dp))
          Spacer(Modifier.height(56.dp))
        }
      }
    }
  }
}

@Composable
private fun DurationText(text: State<String>, padding: PaddingValues) {
  val minWidth = with(LocalDensity.current) { 45.sp.toDp() }
  Text(
    text.value,
    Modifier
      .padding(padding)
      .widthIn(min = minWidth),
    color = HighOrLowlight,
    fontSize = 16.sp,
    maxLines = 1
  )
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
  pause: () -> Unit,
  longClick: () -> Unit
) {
  Surface(
    Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
    color = if (sent) SentColorLight else ReceivedColorLight,
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50))
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 56.dp, minHeight = 56.dp)
        .combinedClickable(
          onClick = { if (!audioPlaying) play() else pause() },
          onLongClick = longClick
        ),
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
private fun VoiceMsgIndicator(
  file: CIFile?,
  audioPlaying: Boolean,
  sent: Boolean,
  hasText: Boolean,
  progress: State<Int>?,
  duration: State<Int>?,
  error: Boolean,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit
) {
  val strokeWidth = with(LocalDensity.current) { 3.dp.toPx() }
  val strokeColor = MaterialTheme.colors.primary
  if (file != null && file.loaded && progress != null && duration != null) {
    val angle = 360f * (progress.value.toDouble() / duration.value).toFloat()
    if (hasText) {
      IconButton({ if (!audioPlaying) play() else pause() }, Modifier.size(56.dp).drawRingModifier(angle, strokeColor, strokeWidth)) {
        Icon(
          imageVector = if (audioPlaying) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          contentDescription = null,
          Modifier.size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
    } else {
      PlayPauseButton(audioPlaying, sent, angle, strokeWidth, strokeColor, true, error, play, pause, longClick = longClick)
    }
  } else {
    if (file?.fileStatus == CIFileStatus.RcvInvitation
      || file?.fileStatus == CIFileStatus.RcvTransfer
      || file?.fileStatus == CIFileStatus.RcvAccepted
    ) {
      Box(
        Modifier
          .size(56.dp)
          .clip(RoundedCornerShape(4.dp)),
        contentAlignment = Alignment.Center
      ) {
        ProgressIndicator()
      }
    } else {
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, false, false, {}, {}, longClick)
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
