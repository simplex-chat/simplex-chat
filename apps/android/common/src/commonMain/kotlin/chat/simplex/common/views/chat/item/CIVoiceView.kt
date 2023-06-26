package chat.simplex.common.views.chat.item

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.unit.*
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.getLoadedFilePath
import chat.simplex.common.platform.AudioPlayer
import kotlinx.coroutines.flow.distinctUntilChanged

// TODO refactor https://github.com/simplex-chat/simplex-chat/pull/1451#discussion_r1033429901

@Composable
fun CIVoiceView(
  providedDurationSec: Int,
  file: CIFile?,
  edited: Boolean,
  sent: Boolean,
  hasText: Boolean,
  ci: ChatItem,
  timedMessagesTTL: Int?,
  longClick: () -> Unit,
  receiveFile: (Long) -> Unit,
) {
  Row(
    Modifier.padding(top = if (hasText) 14.dp else 4.dp, bottom = if (hasText) 14.dp else 6.dp, start = if (hasText) 6.dp else 0.dp, end = if (hasText) 6.dp else 0.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (file != null) {
      val filePath = remember(file.filePath, file.fileStatus) { getLoadedFilePath(file) }
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
      VoiceLayout(file, ci, text, audioPlaying, progress, duration, brokenAudio, sent, hasText, timedMessagesTTL, play, pause, longClick, receiveFile) {
        AudioPlayer.seekTo(it, progress, filePath)
      }
    } else {
      VoiceMsgIndicator(null, false, sent, hasText, null, null, false, {}, {}, longClick, receiveFile)
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
  timedMessagesTTL: Int?,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit,
  receiveFile: (Long) -> Unit,
  onProgressChanged: (Int) -> Unit,
) {
  @Composable
  fun RowScope.Slider(backgroundColor: Color, padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING_HALF)) {
    var movedManuallyTo by rememberSaveable(file.fileId) { mutableStateOf(-1) }
    if (audioPlaying.value || progress.value > 0 || movedManuallyTo == progress.value) {
      val dp4 = with(LocalDensity.current) { 4.dp.toPx() }
      val dp10 = with(LocalDensity.current) { 10.dp.toPx() }
      val primary = MaterialTheme.colors.primary
      val inactiveTrackColor =
        MaterialTheme.colors.primary.mixWith(
          backgroundColor.copy(1f).mixWith(MaterialTheme.colors.background, backgroundColor.alpha),
          0.24f)
      val width = LocalWindowWidth()
      val colors = SliderDefaults.colors(
        inactiveTrackColor = inactiveTrackColor
      )
      Slider(
        progress.value.toFloat(),
        onValueChange = {
          onProgressChanged(it.toInt())
          movedManuallyTo = it.toInt()
        },
        Modifier
          .size(width, 48.dp)
          .weight(1f)
          .padding(padding)
          .drawBehind {
            drawRect(primary, Offset(0f, (size.height - dp4) / 2), size = androidx.compose.ui.geometry.Size(dp10, dp4))
            drawRect(inactiveTrackColor, Offset(size.width - dp10, (size.height - dp4) / 2), size = androidx.compose.ui.geometry.Size(dp10, dp4))
          },
        valueRange = 0f..duration.value.toFloat(),
        colors = colors
      )
      LaunchedEffect(Unit) {
        snapshotFlow { audioPlaying.value }
          .distinctUntilChanged()
          .collect {
            movedManuallyTo = -1
          }
      }
    }
  }
  when {
    hasText -> {
      val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
      val receivedColor = CurrentColors.collectAsState().value.appColors.receivedMessage
      Spacer(Modifier.width(6.dp))
      VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick, receiveFile)
      Row(verticalAlignment = Alignment.CenterVertically) {
        DurationText(text, PaddingValues(start = 12.dp))
        Slider(if (ci.chatDir.sent) sentColor else receivedColor)
      }
    }
    sent -> {
      Column(horizontalAlignment = Alignment.End) {
        Row {
          Row(Modifier.weight(1f, false), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.End) {
            Spacer(Modifier.height(56.dp))
            Slider(MaterialTheme.colors.background, PaddingValues(end = DEFAULT_PADDING_HALF + 3.dp))
            DurationText(text, PaddingValues(end = 12.dp))
          }
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick, receiveFile)
        }
        Box(Modifier.padding(top = 6.dp, end = 6.dp)) {
          CIMetaView(ci, timedMessagesTTL)
        }
      }
    }
    else -> {
      Column(horizontalAlignment = Alignment.Start) {
        Row {
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, play, pause, longClick, receiveFile)
          Row(Modifier.weight(1f, false), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Start) {
            DurationText(text, PaddingValues(start = 12.dp))
            Slider(MaterialTheme.colors.background, PaddingValues(start = DEFAULT_PADDING_HALF + 3.dp))
            Spacer(Modifier.height(56.dp))
          }
        }
        Box(Modifier.padding(top = 6.dp)) {
          CIMetaView(ci, timedMessagesTTL)
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
    color = MaterialTheme.colors.secondary,
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
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  val receivedColor = CurrentColors.collectAsState().value.appColors.receivedMessage
  Surface(
    Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
    color = if (sent) sentColor else receivedColor,
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
        if (audioPlaying) painterResource(MR.images.ic_pause_filled) else painterResource(MR.images.ic_play_arrow_filled),
        contentDescription = null,
        Modifier.size(36.dp),
        tint = if (error) WarningOrange else if (!enabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
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
  longClick: () -> Unit,
  receiveFile: (Long) -> Unit,
) {
  val strokeWidth = with(LocalDensity.current) { 3.dp.toPx() }
  val strokeColor = MaterialTheme.colors.primary
  if (file != null && file.loaded && progress != null && duration != null) {
    val angle = 360f * (progress.value.toDouble() / duration.value).toFloat()
    if (hasText) {
      IconButton({ if (!audioPlaying) play() else pause() }, Modifier.size(56.dp).drawRingModifier(angle, strokeColor, strokeWidth)) {
        Icon(
          if (audioPlaying) painterResource(MR.images.ic_pause_filled) else painterResource(MR.images.ic_play_arrow_filled),
          contentDescription = null,
          Modifier.size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
    } else {
      PlayPauseButton(audioPlaying, sent, angle, strokeWidth, strokeColor, true, error, play, pause, longClick = longClick)
    }
  } else {
    if (file?.fileStatus is CIFileStatus.RcvInvitation) {
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, true, error, { receiveFile(file.fileId) }, {}, longClick = longClick)
    } else if (file?.fileStatus is CIFileStatus.RcvTransfer
      || file?.fileStatus is CIFileStatus.RcvAccepted
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

fun Modifier.drawRingModifier(angle: Float, color: Color, strokeWidth: Float) = drawWithCache {
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
