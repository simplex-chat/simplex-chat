package chat.simplex.common.views.chat.item

import android.os.Build.VERSION.SDK_INT
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import chat.simplex.common.model.CIFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.ModalManager
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import java.net.URI

@Composable
actual fun SimpleAndAnimatedImageView(
  uri: URI,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  val context = LocalContext.current
  val imagePainter = rememberAsyncImagePainter(
    ImageRequest.Builder(context).data(data = uri).size(coil.size.Size.ORIGINAL).build(),
    placeholder = BitmapPainter(imageBitmap), // show original image while it's still loading by coil
    imageLoader = imageLoader
  )
  val view = LocalMultiplatformView()
  ImageView(imagePainter) {
    hideKeyboard(view)
    if (getLoadedFilePath(file) != null) {
      ModalManager.shared.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}

private val imageLoader = ImageLoader.Builder(androidAppContext)
  .components {
    if (SDK_INT >= 28) {
      add(ImageDecoderDecoder.Factory())
    } else {
      add(GifDecoder.Factory())
    }
  }
  .build()