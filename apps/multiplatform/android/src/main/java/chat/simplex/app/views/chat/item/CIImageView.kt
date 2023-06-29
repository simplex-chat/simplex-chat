package chat.simplex.app.views.chat.item

import android.graphics.Bitmap
import android.os.Build.VERSION.SDK_INT
import androidx.annotation.StringRes
import androidx.compose.foundation.Image
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Icon
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.*
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import java.io.File

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
  fun fileIcon(icon: Painter, @StringRes stringId: Int) {
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
          is CIFileStatus.SndComplete -> fileIcon(painterResource(R.drawable.ic_check_filled), R.string.icon_descr_image_snd_complete)
          is CIFileStatus.SndCancelled -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
          is CIFileStatus.SndError -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
          is CIFileStatus.RcvInvitation -> fileIcon(painterResource(R.drawable.ic_arrow_downward), R.string.icon_descr_asked_to_receive)
          is CIFileStatus.RcvAccepted -> fileIcon(painterResource(R.drawable.ic_more_horiz), R.string.icon_descr_waiting_for_image)
          is CIFileStatus.RcvTransfer -> progressIndicator()
          is CIFileStatus.RcvCancelled -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
          is CIFileStatus.RcvError -> fileIcon(painterResource(R.drawable.ic_close), R.string.icon_descr_file)
          else -> {}
        }
      }
    }
  }

  @Composable
  fun imageViewFullWidth(): Dp {
    val approximatePadding = 100.dp
    return with(LocalDensity.current) { minOf(1000.dp, LocalView.current.width.toDp() - approximatePadding) }
  }

  @Composable
  fun imageView(imageBitmap: Bitmap, onClick: () -> Unit) {
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = stringResource(R.string.image_descr),
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
  fun imageView(painter: Painter, onClick: () -> Unit) {
    Image(
      painter,
      contentDescription = stringResource(R.string.image_descr),
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

  fun imageAndFilePath(file: CIFile?): Pair<Bitmap?, String?> {
    val imageBitmap: Bitmap? = getLoadedImage(SimplexApp.context, file)
    val filePath = getLoadedFilePath(SimplexApp.context, file)
    return imageBitmap to filePath
  }

  Box(
    Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID),
    contentAlignment = Alignment.TopEnd
  ) {
    val context = LocalContext.current
    val (imageBitmap, filePath) = remember(file) { imageAndFilePath(file) }
    if (imageBitmap != null && filePath != null) {
      val uri = remember(filePath) { FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))  }
      val imagePainter = rememberAsyncImagePainter(
        ImageRequest.Builder(context).data(data = uri).size(coil.size.Size.ORIGINAL).build(),
        placeholder = BitmapPainter(imageBitmap.asImageBitmap()), // show original image while it's still loading by coil
        imageLoader = imageLoader
      )
      val view = LocalView.current
      imageView(imagePainter, onClick = {
        hideKeyboard(view)
        if (getLoadedFilePath(context, file) != null) {
          ModalManager.shared.showCustomModal(animated = false) { close ->
            ImageFullScreenView(imageProvider, close)
          }
        }
      })
    } else {
      imageView(base64ToBitmap(image), onClick = {
        if (file != null) {
          when (file.fileStatus) {
            CIFileStatus.RcvInvitation ->
              if (fileSizeValid()) {
                receiveFile(file.fileId)
              } else {
                AlertManager.shared.showAlertMsg(
                  generalGetString(R.string.large_file),
                  String.format(generalGetString(R.string.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
                )
              }
            CIFileStatus.RcvAccepted ->
              when (file.fileProtocol) {
                FileProtocol.XFTP ->
                  AlertManager.shared.showAlertMsg(
                    generalGetString(R.string.waiting_for_image),
                    generalGetString(R.string.image_will_be_received_when_contact_completes_uploading)
                  )
                FileProtocol.SMP ->
                  AlertManager.shared.showAlertMsg(
                    generalGetString(R.string.waiting_for_image),
                    generalGetString(R.string.image_will_be_received_when_contact_is_online)
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

private val imageLoader = ImageLoader.Builder(SimplexApp.context)
  .components {
    if (SDK_INT >= 28) {
      add(ImageDecoderDecoder.Factory())
    } else {
      add(GifDecoder.Factory())
    }
  }
  .build()
