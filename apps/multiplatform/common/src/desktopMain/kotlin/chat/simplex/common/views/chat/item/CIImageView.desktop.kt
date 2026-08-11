package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.State
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import chat.simplex.common.model.CIFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.ModalManager

@Composable
actual fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  smallView: Boolean,
  blurred: State<Boolean>,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  // The small view is the chat list preview: a 36dp box that the desktop layout keeps on screen the whole
  // time. Decoding an animation at its own resolution to fill it would hold a raster and spend a frame of
  // work per listed chat, without pause, so it keeps the still image as it did before.
  val frame = if (smallView) null else rememberAnimatedImage(data, imageBitmap, blurred)
  ImageView(BitmapPainter(frame?.value ?: imageBitmap)) {
    if (getLoadedFilePath(file) != null) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}
