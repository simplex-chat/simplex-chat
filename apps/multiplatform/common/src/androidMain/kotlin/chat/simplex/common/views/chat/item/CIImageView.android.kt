package chat.simplex.common.views.chat.item

import android.os.Build.VERSION.SDK_INT
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.helpers.ModalManager
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest

@Composable
actual fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  smallView: Boolean,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  val context = LocalContext.current
  val imagePainter = rememberAsyncImagePainter(
    ImageRequest.Builder(context).data(data = data).size(coil.size.Size.ORIGINAL).build(),
    placeholder = BitmapPainter(imageBitmap), // show original image while it's still loading by coil
    imageLoader = imageLoader
  )
  val view = LocalMultiplatformView()
  ImageView(imagePainter) {
    hideKeyboard(view)
    if (getLoadedFilePath(file) != null) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
        if (smallView) {
          DisposableEffect(Unit) {
            onDispose {
              val c = CurrentColors.value.colors
              platform.androidSetStatusAndNavBarColors(c.isLight, c.background, !appPrefs.chatToolbarOnTop.get(), appPrefs.chatToolbarOnTop.get())
            }
          }
        }
      }
    }
  }
}

private val imageLoader = ImageLoader.Builder(androidAppContext)
  .networkObserverEnabled(false)
  .components {
    if (SDK_INT >= 28) {
      add(ImageDecoderDecoder.Factory())
    } else {
      add(GifDecoder.Factory())
    }
  }
  .build()
