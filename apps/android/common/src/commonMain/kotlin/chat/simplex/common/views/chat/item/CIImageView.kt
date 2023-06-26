package chat.simplex.common.views.chat.item

import androidx.compose.foundation.Image
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Icon
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
import com.icerockdev.library.MR
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import dev.icerock.moko.resources.StringResource
import java.io.File
import java.net.URI

@Composable
fun CIImageView(
  image: String,
  file: CIFile?,
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
      tint = Color.White
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
          else -> {}
        }
      }
    }
  }

  @Composable
  fun imageViewFullWidth(): Dp {
    val approximatePadding = 100.dp
    return with(LocalDensity.current) { minOf(1000.dp, LocalWindowWidth() - approximatePadding) }
  }

  @Composable
  fun imageView(imageBitmap: ImageBitmap, onClick: () -> Unit) {
    Image(
      imageBitmap,
      contentDescription = stringResource(MR.strings.image_descr),
      // .width(1000.dp) is a hack for image to increase IntrinsicSize of FramedItemView
      // if text is short and take all available width if text is long
      modifier = Modifier
        .width(if (imageBitmap.width * 0.97 <= imageBitmap.height) imageViewFullWidth() * 0.75f else 1000.dp)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = onClick
        ),
      contentScale = ContentScale.FillWidth,
    )
  }

  @Composable
  fun ImageView(painter: Painter, onClick: () -> Unit) {
    Image(
      painter,
      contentDescription = stringResource(MR.strings.image_descr),
      // .width(1000.dp) is a hack for image to increase IntrinsicSize of FramedItemView
      // if text is short and take all available width if text is long
      modifier = Modifier
        .width(if (painter.intrinsicSize.width * 0.97 <= painter.intrinsicSize.height) imageViewFullWidth() * 0.75f else 1000.dp)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = onClick
        ),
      contentScale = ContentScale.FillWidth,
    )
  }

  fun fileSizeValid(): Boolean {
    if (file != null) {
      return file.fileSize <= getMaxFileSize(file.fileProtocol)
    }
    return false
  }

  fun imageAndFilePath(file: CIFile?): Pair<ImageBitmap?, String?> {
    val imageBitmap: ImageBitmap? = getLoadedImage(file)
    val filePath = getLoadedFilePath(file)
    return imageBitmap to filePath
  }

  Box(
    Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID),
    contentAlignment = Alignment.TopEnd
  ) {
    val (imageBitmap, filePath) = remember(file) { imageAndFilePath(file) }
    if (imageBitmap != null && filePath != null) {
      val uri = remember(filePath) { getAppFileUri(filePath.substringAfterLast(File.separator))  }
      SimpleAndAnimatedImageView(uri, imageBitmap, file, imageProvider, @Composable { painter, onClick -> ImageView(painter, onClick) })
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
  uri: URI,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
)

