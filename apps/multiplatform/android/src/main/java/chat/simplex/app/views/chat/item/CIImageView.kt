package chat.simplex.app.views.chat.item

import android.net.Uri
import android.os.Build
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.*
import chat.simplex.app.*
import chat.simplex.app.model.*
import chat.simplex.app.platform.getLoadedFilePath
import chat.simplex.app.platform.hideKeyboard
import chat.simplex.app.views.helpers.*
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest

@Composable
fun SimpleAndAnimatedImageView(
  uri: Uri,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  val imagePainter = rememberAsyncImagePainter(
    ImageRequest.Builder(SimplexApp.context).data(data = uri).size(coil.size.Size.ORIGINAL).build(),
    placeholder = BitmapPainter(imageBitmap), // show original image while it's still loading by coil
    imageLoader = imageLoader
  )
  val view = LocalView.current
  ImageView(imagePainter) {
    hideKeyboard(view)
    if (getLoadedFilePath(file) != null) {
      ModalManager.shared.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}

private val imageLoader = ImageLoader.Builder(SimplexApp.context)
  .components {
    if (Build.VERSION.SDK_INT >= 28) {
      add(ImageDecoderDecoder.Factory())
    } else {
      add(GifDecoder.Factory())
    }
  }
  .build()
