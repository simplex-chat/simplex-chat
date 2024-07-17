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
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlinx.coroutines.flow.*
import kotlin.math.*

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
  showViaProxy: Boolean,
  smallView: Boolean = false,
  longClick: () -> Unit,
  receiveFile: (Long) -> Unit,
) {
  val sizeMultiplier = if (smallView) voiceMessageSizeBasedOnSquareSize(36f) / 56f else 1f
  val padding = when {
    smallView -> PaddingValues()
    hasText -> PaddingValues(top = 14.sp.toDp() * sizeMultiplier, bottom = 14.sp.toDp() * sizeMultiplier, start = 6.sp.toDp() * sizeMultiplier, end = 6.sp.toDp() * sizeMultiplier)
    else -> PaddingValues(top = 4.sp.toDp() * sizeMultiplier, bottom = 6.sp.toDp() * sizeMultiplier, start = 0.dp, end = 0.dp)
  }
  Row(
    Modifier.padding(padding),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (file != null) {
      val f = file.fileSource?.filePath
      val fileSource = remember(f, file.fileStatus, CIFile.cachedRemoteFileRequests.toList()) { mutableStateOf(getLoadedFileSource(file)) }
      var brokenAudio by rememberSaveable(f) { mutableStateOf(false) }
      val audioPlaying = rememberSaveable(f) { mutableStateOf(false) }
      val progress = rememberSaveable(f) { mutableStateOf(0) }
      val duration = rememberSaveable(f) { mutableStateOf(providedDurationSec * 1000) }
      val play: () -> Unit = {
        val playIfExists = {
          if (fileSource.value != null) {
            AudioPlayer.play(fileSource.value!!, audioPlaying, progress, duration, true)
            brokenAudio = !audioPlaying.value
          }
        }
        if (chatModel.connectedToRemote() && fileSource.value == null) {
          withBGApi {
            file.loadRemoteFile(true)
            fileSource.value = getLoadedFileSource(file)
            playIfExists()
          }
        } else playIfExists()
      }
      val pause = {
        AudioPlayer.pause(audioPlaying, progress)
      }
      val text = remember(ci.file?.fileId, ci.file?.fileStatus) {
        derivedStateOf {
          val time = when {
            audioPlaying.value || progress.value != 0 -> progress.value
            else -> duration.value
          }
          durationText(time / 1000)
        }
      }
      VoiceLayout(file, ci, text, audioPlaying, progress, duration, brokenAudio, sent, hasText, timedMessagesTTL, showViaProxy, sizeMultiplier, play, pause, longClick, receiveFile) {
        AudioPlayer.seekTo(it, progress, fileSource.value?.filePath)
      }
      if (smallView) {
        KeyChangeEffect(chatModel.chatId.value) {
          AudioPlayer.stop()
        }
      }
    } else if (smallView) {
      VoiceMsgIndicator(null, false, sent, hasText, null, null, false, sizeMultiplier, {}, {}, longClick, receiveFile)
    } else {
      VoiceMsgIndicator(null, false, sent, hasText, null, null, false, 1f, {}, {}, longClick, receiveFile)
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
  showViaProxy: Boolean,
  sizeMultiplier: Float,
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
      val primary = MaterialTheme.colors.primary
      val inactiveTrackColor =
        MaterialTheme.colors.primary.mixWith(
          backgroundColor.copy(1f).mixWith(MaterialTheme.colors.background, backgroundColor.alpha),
          0.24f)
      val width = LocalWindowWidth()
      // Built-in slider has rounded corners but we need square corners, so drawing a track manually
      val colors = SliderDefaults.colors(
        inactiveTrackColor = Color.Transparent,
        activeTrackColor = Color.Transparent
      )
      Slider(
        progress.value.toFloat(),
        onValueChange = {
          onProgressChanged(it.toInt())
          movedManuallyTo = it.toInt()
        },
        Modifier
          .size(width, 48.sp.toDp())
          .weight(1f)
          .padding(padding)
          .drawBehind {
            drawRect(inactiveTrackColor, Offset(0f, (size.height - dp4) / 2), size = Size(size.width, dp4))
            drawRect(primary, Offset(0f, (size.height - dp4) / 2), size = Size(progress.value.toFloat() / max(0.00001f, duration.value.toFloat()) * size.width, dp4))
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
    sizeMultiplier != 1f -> {
      Row(verticalAlignment = Alignment.CenterVertically) {
        VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, sizeMultiplier, play, pause, longClick, receiveFile)
        Row(Modifier.weight(1f, false), verticalAlignment = Alignment.CenterVertically) {
          DurationText(text, PaddingValues(start = 8.sp.toDp()), true)
          Slider(MaterialTheme.colors.background, PaddingValues(start = 7.sp.toDp()))
        }
      }
    }
    hasText -> {
      val sentColor = MaterialTheme.appColors.sentMessage
      val receivedColor = MaterialTheme.appColors.receivedMessage
      Spacer(Modifier.width(6.sp.toDp() * sizeMultiplier))
      VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, 1f, play, pause, longClick, receiveFile)
      Row(verticalAlignment = Alignment.CenterVertically) {
        DurationText(text, PaddingValues(start = 12.sp.toDp() * sizeMultiplier))
        Slider(if (ci.chatDir.sent) sentColor else receivedColor)
      }
    }
    sent -> {
      Column(horizontalAlignment = Alignment.End) {
        Row {
          Row(Modifier.weight(1f, false), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.End) {
            Spacer(Modifier.height(56.sp.toDp() * sizeMultiplier))
            Slider(MaterialTheme.colors.background, PaddingValues(end = DEFAULT_PADDING_HALF + 3.dp))
            DurationText(text, PaddingValues(end = 12.sp.toDp() * sizeMultiplier))
          }
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, 1f, play, pause, longClick, receiveFile)
        }
        Box(Modifier.padding(top = 6.sp.toDp() * sizeMultiplier, end = 6.sp.toDp() * sizeMultiplier)) {
          CIMetaView(ci, timedMessagesTTL, showViaProxy = showViaProxy)
        }
      }
    }
    else -> {
      Column(horizontalAlignment = Alignment.Start) {
        Row {
          VoiceMsgIndicator(file, audioPlaying.value, sent, hasText, progress, duration, brokenAudio, 1f, play, pause, longClick, receiveFile)
          Row(Modifier.weight(1f, false), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Start) {
            DurationText(text, PaddingValues(start = 12.sp.toDp() * sizeMultiplier))
            Slider(MaterialTheme.colors.background, PaddingValues(start = DEFAULT_PADDING_HALF + 3.dp))
            Spacer(Modifier.height(56.sp.toDp() * sizeMultiplier))
          }
        }
        Box(Modifier.padding(top = 6.sp.toDp() * sizeMultiplier)) {
          CIMetaView(ci, timedMessagesTTL, showViaProxy = showViaProxy)
        }
      }
    }
  }
}

@Composable
private fun DurationText(text: State<String>, padding: PaddingValues, smallView: Boolean = false) {
  val minWidth = with(LocalDensity.current) { 45.sp.toDp() }
  Text(
    text.value,
    Modifier
      .padding(padding)
      .widthIn(min = minWidth),
    color = MaterialTheme.colors.secondary,
    fontSize = if (smallView) 15.sp else 16.sp,
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
  sizeMultiplier: Float = 1f,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit,
  icon: ImageResource = MR.images.ic_play_arrow_filled,
) {
  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
    color = if (sent) sentColor else receivedColor,
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50)),
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 56.sp.toDp() * sizeMultiplier, minHeight = 56.sp.toDp() * sizeMultiplier)
        .combinedClickable(
          onClick = { if (!audioPlaying) play() else pause() },
          onLongClick = longClick
        )
        .onRightClick { longClick() },
      contentAlignment = Alignment.Center
    ) {
      Icon(
        if (audioPlaying) painterResource(MR.images.ic_pause_filled) else painterResource(icon),
        contentDescription = null,
        Modifier.size(36.sp.toDp() * sizeMultiplier),
        tint = if (error) WarningOrange else if (!enabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
    }
  }
}

@Composable
private fun PlayablePlayPauseButton(
  audioPlaying: Boolean,
  sent: Boolean,
  hasText: Boolean,
  progress: State<Int>,
  duration: State<Int>,
  strokeWidth: Float,
  strokeColor: Color,
  error: Boolean,
  sizeMultiplier: Float = 1f,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit,
) {
  val angle = 360f * (progress.value.toDouble() / duration.value).toFloat()
  if (hasText) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 56.sp.toDp() * sizeMultiplier, minHeight = 56.sp.toDp() * sizeMultiplier)
        .clip(MaterialTheme.shapes.small.copy(CornerSize(percent = 50)))
        .combinedClickable(onClick = { if (!audioPlaying) play() else pause() } )
        .drawRingModifier(angle, strokeColor, strokeWidth),
      contentAlignment = Alignment.Center
    ) {
      Icon(
        if (audioPlaying) painterResource(MR.images.ic_pause_filled) else painterResource(MR.images.ic_play_arrow_filled),
        contentDescription = null,
        Modifier.size(36.sp.toDp() * sizeMultiplier),
        tint = MaterialTheme.colors.primary
      )
    }
  } else {
    PlayPauseButton(audioPlaying, sent, angle, strokeWidth, strokeColor, true, error, sizeMultiplier, play, pause, longClick = longClick)
  }
}

@Composable
private fun VoiceMsgLoadingProgressIndicator(sizeMultiplier: Float) {
  Box(
    Modifier
      .size(56.sp.toDp() * sizeMultiplier)
      .clip(RoundedCornerShape(4.sp.toDp() * sizeMultiplier)),
    contentAlignment = Alignment.Center
  ) {
    ProgressIndicator(sizeMultiplier)
  }
}

@Composable
private fun FileStatusIcon(
  sent: Boolean,
  icon: ImageResource,
  sizeMultiplier: Float,
  longClick: () -> Unit,
  onClick: () -> Unit,
) {
  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    color = if (sent) sentColor else receivedColor,
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50)),
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 56.sp.toDp() * sizeMultiplier, minHeight = 56.sp.toDp() * sizeMultiplier)
        .combinedClickable(
          onClick = onClick,
          onLongClick = longClick
        )
        .onRightClick { longClick() },
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painterResource(icon),
        contentDescription = null,
        Modifier.size(36.sp.toDp() * sizeMultiplier),
        tint = MaterialTheme.colors.secondary
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
  sizeMultiplier: Float,
  play: () -> Unit,
  pause: () -> Unit,
  longClick: () -> Unit,
  receiveFile: (Long) -> Unit,
) {
  val strokeWidth = with(LocalDensity.current) { 3.sp.toPx() } * sizeMultiplier
  val strokeColor = MaterialTheme.colors.primary
  when {
    file?.fileStatus is CIFileStatus.SndStored ->
      if (file.fileProtocol == FileProtocol.LOCAL && progress != null && duration != null) {
        PlayablePlayPauseButton(audioPlaying, sent, hasText, progress, duration, strokeWidth, strokeColor, error, sizeMultiplier, play, pause, longClick = longClick)
      } else {
        VoiceMsgLoadingProgressIndicator(sizeMultiplier)
      }
    file?.fileStatus is CIFileStatus.SndTransfer ->
      VoiceMsgLoadingProgressIndicator(sizeMultiplier)
    file != null && file.fileStatus is CIFileStatus.SndError ->
      FileStatusIcon(
        sent,
        MR.images.ic_close,
        sizeMultiplier,
        longClick,
        onClick = {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.file_error),
            file.fileStatus.sndFileError.errorInfo
          )
        }
      )
    file != null && file.fileStatus is CIFileStatus.SndWarning ->
      FileStatusIcon(
        sent,
        MR.images.ic_warning_filled,
        sizeMultiplier,
        longClick,
        onClick = {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.temporary_file_error),
            file.fileStatus.sndFileError.errorInfo
          )
        }
      )
    file?.fileStatus is CIFileStatus.RcvInvitation ->
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, true, error, sizeMultiplier, { receiveFile(file.fileId) }, {}, longClick = longClick)
    file?.fileStatus is CIFileStatus.RcvTransfer || file?.fileStatus is CIFileStatus.RcvAccepted ->
      VoiceMsgLoadingProgressIndicator(sizeMultiplier)
    file?.fileStatus is CIFileStatus.RcvAborted ->
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, true, error, sizeMultiplier, { receiveFile(file.fileId) }, {}, longClick = longClick, icon = MR.images.ic_sync_problem)
    file != null && file.fileStatus is CIFileStatus.RcvError ->
      FileStatusIcon(
        sent,
        MR.images.ic_close,
        sizeMultiplier,
        longClick,
        onClick = {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.file_error),
            file.fileStatus.rcvFileError.errorInfo
          )
        }
      )
    file != null && file.fileStatus is CIFileStatus.RcvWarning ->
      FileStatusIcon(
        sent,
        MR.images.ic_warning_filled,
        sizeMultiplier,
        longClick,
        onClick = {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.temporary_file_error),
            file.fileStatus.rcvFileError.errorInfo
          )
        }
      )
    file != null && file.loaded && progress != null && duration != null ->
      PlayablePlayPauseButton(audioPlaying, sent, hasText, progress, duration, strokeWidth, strokeColor, error, sizeMultiplier, play, pause, longClick = longClick)
    else ->
      PlayPauseButton(audioPlaying, sent, 0f, strokeWidth, strokeColor, false, false, sizeMultiplier, {}, {}, longClick)
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

fun voiceMessageSizeBasedOnSquareSize(squareSize: Float): Float {
  val squareToCircleRatio = 0.935f
  return squareSize + squareSize * (1 - squareToCircleRatio)
}

@Composable
private fun ProgressIndicator(sizeMultiplier: Float) {
  CircularProgressIndicator(
    Modifier.size(32.sp.toDp() * sizeMultiplier),
    color = if (isInDarkTheme()) FileDark else FileLight,
    strokeWidth = 4.sp.toDp() * sizeMultiplier
  )
}
