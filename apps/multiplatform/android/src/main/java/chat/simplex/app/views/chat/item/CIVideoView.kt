package chat.simplex.app.views.chat.item

import android.graphics.Bitmap
import android.graphics.Rect
import android.net.Uri
import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.*
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.FileProvider
import androidx.core.graphics.drawable.toDrawable
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout.RESIZE_MODE_FIXED_WIDTH
import com.google.android.exoplayer2.ui.StyledPlayerView
import java.io.File

@Composable
fun CIVideoView(
  image: String,
  duration: Int,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  showMenu: MutableState<Boolean>,
  receiveFile: (Long) -> Unit
) {
  Box(
    Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID),
    contentAlignment = Alignment.TopEnd
  ) {
    val context = LocalContext.current
    val filePath = remember(file) { getLoadedFilePath(SimplexApp.context, file) }
    val preview = remember(image) { base64ToBitmap(image) }
    if (file != null && filePath != null) {
      val uri = remember(filePath) { FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))  }
      val view = LocalView.current
      VideoView(uri, file, preview, duration * 1000L, showMenu, onClick = {
        hideKeyboard(view)
        ModalManager.shared.showCustomModal(animated = false) { close ->
          ImageFullScreenView(imageProvider, close)
        }
      })
    } else {
      Box {
        ImageView(preview, showMenu, onClick = {
          if (file != null) {
            when (file.fileStatus) {
              CIFileStatus.RcvInvitation ->
                receiveFileIfValidSize(file, receiveFile)
              CIFileStatus.RcvAccepted ->
                when (file.fileProtocol) {
                  FileProtocol.XFTP ->
                    AlertManager.shared.showAlertMsg(
                      generalGetString(R.string.waiting_for_video),
                      generalGetString(R.string.video_will_be_received_when_contact_completes_uploading)
                    )

                  FileProtocol.SMP ->
                    AlertManager.shared.showAlertMsg(
                      generalGetString(R.string.waiting_for_video),
                      generalGetString(R.string.video_will_be_received_when_contact_is_online)
                    )
                }
              CIFileStatus.RcvTransfer(rcvProgress = 7, rcvTotal = 10) -> {} // ?
              CIFileStatus.RcvComplete -> {} // ?
              CIFileStatus.RcvCancelled -> {} // TODO
              else -> {}
            }
          }
        })
        if (file != null) {
          DurationProgress(file, remember { mutableStateOf(false) }, remember { mutableStateOf(duration * 1000L) }, remember { mutableStateOf(0L) }/*, soundEnabled*/)
        }
        if (file?.fileStatus is CIFileStatus.RcvInvitation) {
          PlayButton(error = false, { showMenu.value = true }) { receiveFileIfValidSize(file, receiveFile) }
        }
      }
    }
    loadingIndicator(file)
  }
}

@Composable
private fun VideoView(uri: Uri, file: CIFile, defaultPreview: Bitmap, defaultDuration: Long, showMenu: MutableState<Boolean>, onClick: () -> Unit) {
  val context = LocalContext.current
  val player = remember(uri) { VideoPlayer.getOrCreate(uri, false, defaultPreview, defaultDuration, true, context) }
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
  DisposableEffect(Unit) {
    onDispose {
      stop()
    }
  }
  Box {
    val windowWidth = LocalWindowWidth()
    val width = remember(preview) { if (preview.width * 0.97 <= preview.height) videoViewFullWidth(windowWidth) * 0.75f else 1000.dp }
    AndroidView(
      factory = { ctx ->
        StyledPlayerView(ctx).apply {
          useController = false
          resizeMode = RESIZE_MODE_FIXED_WIDTH
          this.player = player.player
        }
      },
      Modifier
        .width(width)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = { if (player.player.playWhenReady) stop() else onClick() }
        )
    )
    if (showPreview.value) {
      ImageView(preview, showMenu, onClick)
      PlayButton(brokenVideo, onLongClick =  { showMenu.value = true }, play)
    }
    DurationProgress(file, videoPlaying, duration, progress/*, soundEnabled*/)
  }
}

@Composable
private fun BoxScope.PlayButton(error: Boolean = false, onLongClick: () -> Unit, onClick: () -> Unit) {
  Surface(
    Modifier.align(Alignment.Center),
    color = Color.Black.copy(alpha = 0.25f),
    shape = RoundedCornerShape(percent = 50)
  ) {
    Box(
      Modifier
        .defaultMinSize(minWidth = 40.dp, minHeight = 40.dp)
        .combinedClickable(onClick = onClick, onLongClick = onLongClick),
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painterResource(R.drawable.ic_play_arrow_filled),
        contentDescription = null,
        tint = if (error) WarningOrange else Color.White
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
        Icon(painterResource(R.drawable.ic_volume_off_filled), null,
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
private fun ImageView(preview: Bitmap, showMenu: MutableState<Boolean>, onClick: () -> Unit) {
  val windowWidth = LocalWindowWidth()
  val width = remember(preview) { if (preview.width * 0.97 <= preview.height) videoViewFullWidth(windowWidth) * 0.75f else 1000.dp }
  Image(
    preview.asImageBitmap(),
    contentDescription = stringResource(R.string.video_descr),
    modifier = Modifier
      .width(width)
      .combinedClickable(
        onLongClick = { showMenu.value = true },
        onClick = onClick
      ),
    contentScale = ContentScale.FillWidth,
  )
}

@Composable
private fun LocalWindowWidth(): Dp {
  val view = LocalView.current
  val density = LocalDensity.current.density
  return remember {
    val rect = Rect()
    view.getWindowVisibleDisplayFrame(rect)
    (rect.width() / density).dp
  }
}

@Composable
private fun progressIndicator() {
  CircularProgressIndicator(
    Modifier.size(16.dp),
    color = Color.White,
    strokeWidth = 2.dp
  )
}

@Composable
private fun fileIcon(icon: Painter, @StringRes stringId: Int) {
  Icon(
    icon,
    stringResource(stringId),
    Modifier.fillMaxSize(),
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
    shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50))
  ) {
    Box(Modifier.size(16.dp))
  }
}

@Composable
private fun loadingIndicator(file: CIFile?) {
  if (file != null) {
    Box(
      Modifier
        .padding(8.dp)
        .size(20.dp),
      contentAlignment = Alignment.Center
    ) {
      when (file.fileStatus) {
        is CIFileStatus.SndStored ->
          when (file.fileProtocol) {
            FileProtocol.XFTP -> progressIndicator()
            FileProtocol.SMP -> {}
          }
        is CIFileStatus.SndTransfer ->
          when (file.fileProtocol) {
            FileProtocol.XFTP -> progressCircle(file.fileStatus.sndProgress, file.fileStatus.sndTotal)
            FileProtocol.SMP -> progressIndicator()
          }
        is CIFileStatus.SndComplete -> fileIcon(painterResource(R.drawable.ic_check_filled), R.string.icon_descr_video_snd_complete)
        is CIFileStatus.SndCancelled -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
        is CIFileStatus.SndError -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
        is CIFileStatus.RcvInvitation -> fileIcon(painterResource(R.drawable.ic_arrow_downward), R.string.icon_descr_video_asked_to_receive)
        is CIFileStatus.RcvAccepted -> fileIcon(painterResource(R.drawable.ic_more_horiz), R.string.icon_descr_waiting_for_video)
        is CIFileStatus.RcvTransfer ->
          if (file.fileProtocol == FileProtocol.XFTP && file.fileStatus.rcvProgress < file.fileStatus.rcvTotal) {
            progressCircle(file.fileStatus.rcvProgress, file.fileStatus.rcvTotal)
          } else {
            progressIndicator()
          }
        is CIFileStatus.RcvCancelled -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
        is CIFileStatus.RcvError -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
        else -> {}
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
      generalGetString(R.string.large_file),
      String.format(generalGetString(R.string.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
    )
  }
}

private fun videoViewFullWidth(windowWidth: Dp): Dp {
  val approximatePadding = 100.dp
  return minOf(1000.dp, windowWidth - approximatePadding)
}
