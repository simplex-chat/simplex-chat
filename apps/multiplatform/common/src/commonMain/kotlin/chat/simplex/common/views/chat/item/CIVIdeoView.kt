package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.*
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.*
import chat.simplex.res.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import dev.icerock.moko.resources.StringResource
import java.io.File
import java.net.URI

@Composable
fun CIVideoView(
  image: String,
  duration: Int,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  showMenu: MutableState<Boolean>,
  smallView: Boolean = false,
  receiveFile: (Long) -> Unit
) {
  Box(
    Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID),
    contentAlignment = Alignment.TopEnd
  ) {
    val preview = remember(image) { base64ToBitmap(image) }
    val filePath = remember(file, CIFile.cachedRemoteFileRequests.toList()) { mutableStateOf(getLoadedFilePath(file)) }
    val sizeMultiplier = if (smallView) 0.3f else 1f
    if (chatModel.connectedToRemote()) {
      LaunchedEffect(file) {
        withLongRunningApi(slow = 600_000) {
          if (file != null && file.loaded && getLoadedFilePath(file) == null) {
            file.loadRemoteFile(false)
            filePath.value = getLoadedFilePath(file)
          }
        }
      }
    }
    val f = filePath.value
    if (file != null && f != null) {
      val view = LocalMultiplatformView()
      val openFullscreen = {
        hideKeyboard(view)
        ModalManager.fullscreen.showCustomModal(animated = false) { close ->
          ImageFullScreenView(imageProvider, close)
        }
      }

      val uri = remember(filePath) { getAppFileUri(f.substringAfterLast(File.separator))  }
      val autoPlay = remember { mutableStateOf(false) }
      val uriDecrypted = remember(filePath) { mutableStateOf(if (file.fileSource?.cryptoArgs == null) uri else file.fileSource.decryptedGet()) }
      val decrypted = uriDecrypted.value
      if (decrypted != null && smallView) {
        SmallVideoView(decrypted, file, preview, duration * 1000L, autoPlay, sizeMultiplier, openFullscreen = openFullscreen)
      } else if (decrypted != null) {
        VideoView(decrypted, file, preview, duration * 1000L, autoPlay, showMenu, openFullscreen = openFullscreen)
      } else if (smallView) {
        SmallVideoViewEncrypted(uriDecrypted, file, preview, autoPlay, showMenu, sizeMultiplier, openFullscreen = openFullscreen)
      } else {
        VideoViewEncrypted(uriDecrypted, file, preview, duration * 1000L, autoPlay, showMenu, openFullscreen = openFullscreen)
      }
    } else {
      Box {
        VideoPreviewImageView(preview, onClick = {
          if (file != null) {
            when (file.fileStatus) {
              CIFileStatus.RcvInvitation, CIFileStatus.RcvAborted ->
                receiveFileIfValidSize(file, receiveFile)
              CIFileStatus.RcvAccepted ->
                when (file.fileProtocol) {
                  FileProtocol.XFTP ->
                    AlertManager.shared.showAlertMsg(
                      generalGetString(MR.strings.waiting_for_video),
                      generalGetString(MR.strings.video_will_be_received_when_contact_completes_uploading)
                    )
                  FileProtocol.SMP ->
                    AlertManager.shared.showAlertMsg(
                      generalGetString(MR.strings.waiting_for_video),
                      generalGetString(MR.strings.video_will_be_received_when_contact_is_online)
                    )
                  FileProtocol.LOCAL -> {}
                }
              CIFileStatus.RcvTransfer(rcvProgress = 7, rcvTotal = 10) -> {} // ?
              CIFileStatus.RcvComplete -> {} // ?
              CIFileStatus.RcvCancelled -> {} // TODO
              else -> {}
            }
          }
        },
          smallView = smallView,
          onLongClick = {
            showMenu.value = true
          })
        if (file != null && !smallView) {
          DurationProgress(file, remember { mutableStateOf(false) }, remember { mutableStateOf(duration * 1000L) }, remember { mutableStateOf(0L) }/*, soundEnabled*/)
        }
        if (file?.fileStatus is CIFileStatus.RcvInvitation || file?.fileStatus is CIFileStatus.RcvAborted) {
          PlayButton(error = false, sizeMultiplier, { showMenu.value = true }) { receiveFileIfValidSize(file, receiveFile) }
        }
      }
    }
    if (!smallView) {
      fileStatusIcon(file, false)
    } else if (file?.showStatusIconInSmallView == true) {
      Box(Modifier.align(Alignment.Center)) {
        fileStatusIcon(file, true)
      }
    }
  }
}

@Composable
private fun VideoViewEncrypted(
  uriUnencrypted: MutableState<URI?>,
  file: CIFile,
  defaultPreview: ImageBitmap,
  defaultDuration: Long,
  autoPlay: MutableState<Boolean>,
  showMenu: MutableState<Boolean>,
  openFullscreen: () -> Unit,
) {
  var decryptionInProgress by rememberSaveable(file.fileName) { mutableStateOf(false) }
  val onLongClick = { showMenu.value = true }
  Box {
    VideoPreviewImageView(defaultPreview, smallView = false, if (decryptionInProgress) {{}} else openFullscreen, onLongClick)
    if (decryptionInProgress) {
      VideoDecryptionProgress(1f, onLongClick = onLongClick)
    } else {
      PlayButton(false, 1f, onLongClick = onLongClick) {
        decryptionInProgress = true
        withBGApi {
          try {
            uriUnencrypted.value = file.fileSource?.decryptedGetOrCreate()
            autoPlay.value = uriUnencrypted.value != null
          } finally {
            decryptionInProgress = false
          }
        }
      }
    }
    DurationProgress(file, remember { mutableStateOf(false) }, remember { mutableStateOf(defaultDuration) }, remember { mutableStateOf(0L) })
  }
}

@Composable
private fun SmallVideoViewEncrypted(
  uriUnencrypted: MutableState<URI?>,
  file: CIFile,
  defaultPreview: ImageBitmap,
  autoPlay: MutableState<Boolean>,
  showMenu: MutableState<Boolean>,
  sizeMultiplier: Float,
  openFullscreen: () -> Unit,
) {
  var decryptionInProgress by rememberSaveable(file.fileName) { mutableStateOf(false) }
  val onLongClick = { showMenu.value = true }
  Box {
    VideoPreviewImageView(defaultPreview, smallView = true, if (decryptionInProgress) {{}} else openFullscreen, onLongClick)
    if (decryptionInProgress) {
      VideoDecryptionProgress(sizeMultiplier, onLongClick = onLongClick)
    } else if (!file.showStatusIconInSmallView) {
      PlayButton(false, sizeMultiplier, onLongClick = onLongClick) {
        decryptionInProgress = true
        withBGApi {
          try {
            uriUnencrypted.value = file.fileSource?.decryptedGetOrCreate()
            autoPlay.value = uriUnencrypted.value != null
          } finally {
            decryptionInProgress = false
          }
        }
      }
    }
  }
}

@Composable
private fun SmallVideoView(uri: URI, file: CIFile, defaultPreview: ImageBitmap, defaultDuration: Long, autoPlay: MutableState<Boolean>, sizeMultiplier: Float, openFullscreen: () -> Unit) {
  val player = remember(uri) { VideoPlayerHolder.getOrCreate(uri, true, defaultPreview, defaultDuration, true) }
  val preview by remember { player.preview }
  //  val soundEnabled by rememberSaveable(uri.path) { player.soundEnabled }
  val brokenVideo by rememberSaveable(uri.path) { player.brokenVideo }
  Box {
    val windowWidth = LocalWindowWidth()
    val width = remember(preview) { if (preview.width * 0.97 <= preview.height) videoViewFullWidth(windowWidth) * 0.75f else DEFAULT_MAX_IMAGE_WIDTH }
    PlayerView(
      player,
      width,
      onClick = openFullscreen,
      onLongClick = {},
      {}
    )
    VideoPreviewImageView(preview, smallView = true, openFullscreen, onLongClick = {})
    if (!file.showStatusIconInSmallView) {
      PlayButton(brokenVideo, sizeMultiplier, onLongClick = {}, onClick = openFullscreen)
    }
  }
  LaunchedEffect(uri) {
    if (autoPlay.value) openFullscreen()
  }
}

@Composable
private fun VideoView(uri: URI, file: CIFile, defaultPreview: ImageBitmap, defaultDuration: Long, autoPlay: MutableState<Boolean>, showMenu: MutableState<Boolean>, openFullscreen: () -> Unit) {
  val player = remember(uri) { VideoPlayerHolder.getOrCreate(uri, false, defaultPreview, defaultDuration, true) }
  val videoPlaying = remember(uri.path) { player.videoPlaying }
  val progress = remember(uri.path) { player.progress }
  val duration = remember(uri.path) { player.duration }
  val preview by remember { player.preview }
  //  val soundEnabled by rememberSaveable(uri.path) { player.soundEnabled }
  val brokenVideo by rememberSaveable(uri.path) { player.brokenVideo }
  val play = {
    player.enableSound(true)
    player.play(true)
  }
  val stop = {
    player.enableSound(false)
    player.stop()
  }
  val showPreview = remember { derivedStateOf { !videoPlaying.value || progress.value == 0L } }
  LaunchedEffect(uri) {
    if (autoPlay.value) play()
  }
  // Drop autoPlay only when show preview changes to prevent blinking of the view
  KeyChangeEffect(showPreview.value) {
    autoPlay.value = false
  }
  DisposableEffect(Unit) {
    onDispose {
      stop()
    }
  }
  val onLongClick = { showMenu.value = true }
  Box {
    val windowWidth = LocalWindowWidth()
    val width = remember(preview) { if (preview.width * 0.97 <= preview.height) videoViewFullWidth(windowWidth) * 0.75f else DEFAULT_MAX_IMAGE_WIDTH }
    PlayerView(
      player,
      width,
      onClick = openFullscreen,
      onLongClick = onLongClick,
      stop
    )
    if (showPreview.value) {
      VideoPreviewImageView(preview, smallView = false, openFullscreen, onLongClick)
      if (!autoPlay.value) {
        PlayButton(brokenVideo, onLongClick = onLongClick, onClick = play)
      }
    }
    DurationProgress(file, videoPlaying, duration, progress/*, soundEnabled*/)
  }
}

@Composable
expect fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit)

@Composable
private fun BoxScope.PlayButton(error: Boolean = false, sizeMultiplier: Float = 1f, onLongClick: () -> Unit, onClick: () -> Unit) {
  Surface(
    Modifier.align(if (sizeMultiplier != 1f) Alignment.TopStart else Alignment.Center),
    color = Color.Black.copy(alpha = 0.25f),
    shape = RoundedCornerShape(percent = 50),
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 40.dp * sizeMultiplier, minHeight = 40.dp * sizeMultiplier)
        .combinedClickable(onClick = onClick, onLongClick = onLongClick)
        .onRightClick { onLongClick.invoke() },
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painterResource(MR.images.ic_play_arrow_filled),
        contentDescription = null,
        Modifier.size(24.dp * sizeMultiplier),
        tint = if (error) WarningOrange else Color.White
      )
    }
  }
}

@Composable
fun BoxScope.VideoDecryptionProgress(sizeMultiplier: Float = 1f, onLongClick: () -> Unit) {
  Surface(
    Modifier.align(if (sizeMultiplier != 1f) Alignment.TopStart else Alignment.Center),
    color = Color.Black.copy(alpha = 0.25f),
    shape = RoundedCornerShape(percent = 50),
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 40.dp * sizeMultiplier, minHeight = 40.dp * sizeMultiplier)
        .combinedClickable(onClick = {}, onLongClick = onLongClick)
        .onRightClick { onLongClick.invoke() },
      contentAlignment = Alignment.Center
    ) {
      CircularProgressIndicator(
        Modifier
          .size(30.dp * sizeMultiplier),
        color = Color.White,
        strokeWidth = 2.5.dp * sizeMultiplier
      )
    }
  }
}

@Composable
private fun DurationProgress(file: CIFile, playing: MutableState<Boolean>, duration: MutableState<Long>, progress: MutableState<Long>/*, soundEnabled: MutableState<Boolean>*/) {
  if (duration.value > 0L || progress.value > 0) {
    Row {
      Box(
        Modifier
          .padding(DEFAULT_PADDING_HALF)
          .background(Color.Black.copy(alpha = 0.35f), RoundedCornerShape(percent = 50))
          .padding(vertical = 2.dp, horizontal = 4.dp)
      ) {
        val time = if (progress.value > 0) progress.value else duration.value
        val timeStr = durationText((time / 1000).toInt())
        val width = if (timeStr.length <= 5) 44 else 50
        Text(
          timeStr,
          Modifier.widthIn(min = with(LocalDensity.current) { width.sp.toDp() }).padding(horizontal = 4.dp),
          fontSize = 13.sp,
          color = Color.White
        )
        /*if (!soundEnabled.value) {
        Icon(painterResource(MR.images.ic_volume_off_filled), null,
          Modifier.padding(start = 5.dp).size(10.dp),
          tint = Color.White
        )
      }*/
      }
      if (!playing.value) {
        Box(
          Modifier
            .padding(top = DEFAULT_PADDING_HALF)
            .background(Color.Black.copy(alpha = 0.35f), RoundedCornerShape(percent = 50))
            .padding(vertical = 2.dp, horizontal = 4.dp)
        ) {
          Text(
            formatBytes(file.fileSize),
            Modifier.padding(horizontal = 4.dp),
            fontSize = 13.sp,
            color = Color.White
          )
        }
      }
    }
  }
}

@Composable
fun VideoPreviewImageView(preview: ImageBitmap, smallView: Boolean, onClick: () -> Unit, onLongClick: () -> Unit) {
  val windowWidth = LocalWindowWidth()
  val width = remember(preview) { if (preview.width * 0.97 <= preview.height) videoViewFullWidth(windowWidth) * 0.75f else DEFAULT_MAX_IMAGE_WIDTH }
  Image(
    preview,
    contentDescription = stringResource(MR.strings.video_descr),
    modifier = Modifier
      .width(width)
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = onClick
      )
      .onRightClick(onLongClick),
    contentScale = if (smallView) ContentScale.Crop else ContentScale.FillWidth,
  )
}

@Composable
fun VideoPreviewImageViewFullScreen(preview: ImageBitmap, onClick: () -> Unit, onLongClick: () -> Unit) {
  Image(
    preview,
    contentDescription = stringResource(MR.strings.video_descr),
    modifier = Modifier
      .fillMaxSize()
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = onClick
      )
      .onRightClick(onLongClick),
    contentScale = ContentScale.FillWidth,
  )
}

@Composable
expect fun LocalWindowWidth(): Dp

@Composable
private fun progressIndicator() {
  CircularProgressIndicator(
    Modifier.size(16.dp),
    color = Color.White,
    strokeWidth = 2.dp
  )
}

@Composable
private fun fileIcon(icon: Painter, stringId: StringResource, onClick: (() -> Unit)? = null) {
  var modifier = Modifier.fillMaxSize()
  modifier = if (onClick != null) { modifier.clickable { onClick() } } else { modifier }
  Icon(
    icon,
    stringResource(stringId),
    modifier,
    tint = Color.White
  )
}

@Composable
private fun progressCircle(progress: Long, total: Long) {
  val angle = 360f * (progress.toDouble() / total.toDouble()).toFloat()
  val strokeWidth = with(LocalDensity.current) { 2.dp.toPx() }
  val strokeColor = Color.White
  Surface(
    Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
    color = Color.Transparent,
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50)),
    contentColor = LocalContentColor.current
  ) {
    Box(Modifier.size(16.dp))
  }
}

@Composable
private fun fileStatusIcon(file: CIFile?, smallView: Boolean) {
  if (file != null) {
    Box(
      Modifier
        .padding(if (smallView) 0.dp else 8.dp)
        .size(20.dp),
      contentAlignment = Alignment.Center
    ) {
      when (file.fileStatus) {
        is CIFileStatus.SndStored ->
          when (file.fileProtocol) {
            FileProtocol.XFTP -> progressIndicator()
            FileProtocol.SMP -> {}
            FileProtocol.LOCAL -> {}
          }
        is CIFileStatus.SndTransfer ->
          when (file.fileProtocol) {
            FileProtocol.XFTP -> progressCircle(file.fileStatus.sndProgress, file.fileStatus.sndTotal)
            FileProtocol.SMP -> progressIndicator()
            FileProtocol.LOCAL -> {}
          }
        is CIFileStatus.SndComplete -> fileIcon(painterResource(MR.images.ic_check_filled), MR.strings.icon_descr_video_snd_complete)
        is CIFileStatus.SndCancelled -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
        is CIFileStatus.SndError ->
          fileIcon(
            painterResource(MR.images.ic_close),
            MR.strings.icon_descr_file,
            onClick = {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.file_error),
                file.fileStatus.sndFileError.errorInfo
              )
            }
          )
        is CIFileStatus.SndWarning ->
          fileIcon(
            painterResource(MR.images.ic_warning_filled),
            MR.strings.icon_descr_file,
            onClick = {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.temporary_file_error),
                file.fileStatus.sndFileError.errorInfo
              )
            }
          )
        is CIFileStatus.RcvInvitation -> fileIcon(painterResource(MR.images.ic_arrow_downward), MR.strings.icon_descr_video_asked_to_receive)
        is CIFileStatus.RcvAccepted -> fileIcon(painterResource(MR.images.ic_more_horiz), MR.strings.icon_descr_waiting_for_video)
        is CIFileStatus.RcvTransfer ->
          if (file.fileProtocol == FileProtocol.XFTP && file.fileStatus.rcvProgress < file.fileStatus.rcvTotal) {
            progressCircle(file.fileStatus.rcvProgress, file.fileStatus.rcvTotal)
          } else {
            progressIndicator()
          }
        is CIFileStatus.RcvAborted -> fileIcon(painterResource(MR.images.ic_sync_problem), MR.strings.icon_descr_file)
        is CIFileStatus.RcvComplete -> {}
        is CIFileStatus.RcvCancelled -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
        is CIFileStatus.RcvError ->
          fileIcon(
            painterResource(MR.images.ic_close),
            MR.strings.icon_descr_file,
            onClick = {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.file_error),
                file.fileStatus.rcvFileError.errorInfo
              )
            }
          )
        is CIFileStatus.RcvWarning ->
          fileIcon(
            painterResource(MR.images.ic_warning_filled),
            MR.strings.icon_descr_file,
            onClick = {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.temporary_file_error),
                file.fileStatus.rcvFileError.errorInfo
              )
            }
          )
        is CIFileStatus.Invalid -> fileIcon(painterResource(MR.images.ic_question_mark), MR.strings.icon_descr_file)
      }
    }
  }
}

private fun fileSizeValid(file: CIFile?): Boolean {
  if (file != null) {
    return file.fileSize <= getMaxFileSize(file.fileProtocol)
  }
  return false
}

private fun receiveFileIfValidSize(file: CIFile, receiveFile: (Long) -> Unit) {
  if (fileSizeValid(file)) {
    receiveFile(file.fileId)
  } else {
    AlertManager.shared.showAlertMsg(
      generalGetString(MR.strings.large_file),
      String.format(generalGetString(MR.strings.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
    )
  }
}

private fun videoViewFullWidth(windowWidth: Dp): Dp {
  val approximatePadding = 100.dp
  return minOf(DEFAULT_MAX_IMAGE_WIDTH, windowWidth - approximatePadding)
}
