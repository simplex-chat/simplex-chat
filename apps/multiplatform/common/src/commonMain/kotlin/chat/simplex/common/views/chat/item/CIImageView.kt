package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_MAX_IMAGE_WIDTH
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.runBlocking
import java.io.File
import java.net.URI

@Composable
fun CIImageView(
  image: String,
  file: CIFile?,
  metaColor: Color,
  imageProvider: () -> ImageGalleryProvider,
  showMenu: MutableState<Boolean>,
  receiveFile: (Long) -> Unit
) {
  @Composable
  fun progressIndicator() {
    CircularProgressIndicator(
      Modifier.size(16.dp),
      color = Color.White,
      strokeWidth = 2.dp
    )
  }

  @Composable
  fun fileIcon(icon: Painter, stringId: StringResource) {
    Icon(
      icon,
      stringResource(stringId),
      Modifier.fillMaxSize(),
      tint = metaColor
    )
  }

  @Composable
  fun loadingIndicator() {
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
              FileProtocol.LOCAL -> {}
            }
          is CIFileStatus.SndTransfer -> progressIndicator()
          is CIFileStatus.SndComplete -> fileIcon(painterResource(MR.images.ic_check_filled), MR.strings.icon_descr_image_snd_complete)
          is CIFileStatus.SndCancelled -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
          is CIFileStatus.SndError -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
          is CIFileStatus.RcvInvitation -> fileIcon(painterResource(MR.images.ic_arrow_downward), MR.strings.icon_descr_asked_to_receive)
          is CIFileStatus.RcvAccepted -> fileIcon(painterResource(MR.images.ic_more_horiz), MR.strings.icon_descr_waiting_for_image)
          is CIFileStatus.RcvTransfer -> progressIndicator()
          is CIFileStatus.RcvCancelled -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
          is CIFileStatus.RcvError -> fileIcon(painterResource(MR.images.ic_close), MR.strings.icon_descr_file)
          is CIFileStatus.Invalid -> fileIcon(painterResource(MR.images.ic_question_mark), MR.strings.icon_descr_file)
          else -> {}
        }
      }
    }
  }

  @Composable
  fun imageViewFullWidth(): Dp {
    val approximatePadding = 100.dp
    return with(LocalDensity.current) { minOf(DEFAULT_MAX_IMAGE_WIDTH, LocalWindowWidth() - approximatePadding) }
  }

  @Composable
  fun imageView(imageBitmap: ImageBitmap, onClick: () -> Unit) {
    Image(
      imageBitmap,
      contentDescription = stringResource(MR.strings.image_descr),
      // .width(DEFAULT_MAX_IMAGE_WIDTH) is a hack for image to increase IntrinsicSize of FramedItemView
      // if text is short and take all available width if text is long
      modifier = Modifier
        .width(if (imageBitmap.width * 0.97 <= imageBitmap.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = onClick
        )
        .onRightClick { showMenu.value = true },
      contentScale = ContentScale.FillWidth,
    )
  }

  @Composable
  fun ImageView(painter: Painter, image: String, fileSource: CryptoFile?, onClick: () -> Unit) {
    // On my Android device Compose fails to display 6000x6000 px WebP image with exception:
    // IllegalStateException: Recording currently in progress - missing #endRecording() call?
    // but can display 5000px image. Using even lower value here just to feel safer.
    // It happens to WebP because it's not compressed while sending since it can be animated.
    if (painter.intrinsicSize.width <= 4320 && painter.intrinsicSize.height <= 4320) {
      Image(
        painter,
        contentDescription = stringResource(MR.strings.image_descr),
        // .width(DEFAULT_MAX_IMAGE_WIDTH) is a hack for image to increase IntrinsicSize of FramedItemView
        // if text is short and take all available width if text is long
        modifier = Modifier
          .width(if (painter.intrinsicSize.width * 0.97 <= painter.intrinsicSize.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH)
          .combinedClickable(
            onLongClick = { showMenu.value = true },
            onClick = onClick
          )
          .onRightClick { showMenu.value = true },
        contentScale = ContentScale.FillWidth,
      )
    } else {
      Box(Modifier
        .width(if (painter.intrinsicSize.width * 0.97 <= painter.intrinsicSize.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = {}
        )
        .onRightClick { showMenu.value = true },
        contentAlignment = Alignment.Center
      ) {
        imageView(base64ToBitmap(image), onClick = {
          if (fileSource != null) {
            openFile(fileSource)
          }
        })
        Icon(
          painterResource(MR.images.ic_open_in_new),
          contentDescription = stringResource(MR.strings.image_descr),
          modifier = Modifier.size(30.dp),
          tint = MaterialTheme.colors.primary,
        )
      }
    }
  }

  fun fileSizeValid(): Boolean {
    if (file != null) {
      return file.fileSize <= getMaxFileSize(file.fileProtocol)
    }
    return false
  }

  suspend fun imageAndFilePath(file: CIFile?): Triple<ImageBitmap, ByteArray, String>? {
    val res = getLoadedImage(file)
    if (res != null) {
      val (imageBitmap: ImageBitmap, data: ByteArray) = res
      val filePath = getLoadedFilePath(file)!!
      return Triple(imageBitmap, data, filePath)
    }
    return null
  }

  Box(
    Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID),
    contentAlignment = Alignment.TopEnd
  ) {
    val res: MutableState<Triple<ImageBitmap, ByteArray, String>?> = remember {
      mutableStateOf(
        if (chatModel.connectedToRemote()) null else runBlocking { imageAndFilePath(file) }
      )
    }
    if (chatModel.connectedToRemote()) {
      LaunchedEffect(file, CIFile.cachedRemoteFileRequests.toList()) {
        withBGApi {
          if (res.value == null || res.value!!.third != getLoadedFilePath(file)) {
            res.value = imageAndFilePath(file)
          }
        }
      }
    } else {
      KeyChangeEffect(file) {
        if (res.value == null) {
          res.value = imageAndFilePath(file)
        }
      }
    }
    val loaded = res.value
    if (loaded != null && file != null) {
      val (imageBitmap, data, _) = loaded
      SimpleAndAnimatedImageView(data, imageBitmap, file, imageProvider, @Composable { painter, onClick -> ImageView(painter, image, file.fileSource, onClick) })
    } else {
      imageView(base64ToBitmap(image), onClick = {
        if (file != null) {
          when (file.fileStatus) {
            CIFileStatus.RcvInvitation ->
              if (fileSizeValid()) {
                receiveFile(file.fileId)
              } else {
                AlertManager.shared.showAlertMsg(
                  generalGetString(MR.strings.large_file),
                  String.format(generalGetString(MR.strings.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
                )
              }
            CIFileStatus.RcvAccepted ->
              when (file.fileProtocol) {
                FileProtocol.XFTP ->
                  AlertManager.shared.showAlertMsg(
                    generalGetString(MR.strings.waiting_for_image),
                    generalGetString(MR.strings.image_will_be_received_when_contact_completes_uploading)
                  )
                FileProtocol.SMP ->
                  AlertManager.shared.showAlertMsg(
                    generalGetString(MR.strings.waiting_for_image),
                    generalGetString(MR.strings.image_will_be_received_when_contact_is_online)
                  )
                FileProtocol.LOCAL -> {}
              }
            CIFileStatus.RcvTransfer(rcvProgress = 7, rcvTotal = 10) -> {} // ?
            CIFileStatus.RcvComplete -> {} // ?
            CIFileStatus.RcvCancelled -> {} // TODO
            else -> {}
          }
        }
      })
    }
    loadingIndicator()
  }
}

@Composable
expect fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
)
