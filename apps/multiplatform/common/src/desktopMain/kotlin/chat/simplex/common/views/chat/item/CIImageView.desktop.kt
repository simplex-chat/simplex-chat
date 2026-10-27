package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.unit.DpSize
import chat.simplex.common.model.CIFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.ModalManager

@Composable
internal actual fun chatImageWindowSize(): DpSize = DpSize(LocalWindowWidth(), LocalWindowHeight())

@Composable
actual fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  smallView: Boolean,
  createTransition: () -> ImageGalleryTransition,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  // LALAL make it animated too
  ImageView(BitmapPainter(imageBitmap)) {
    if (getLoadedFilePath(file) != null) {
      val transition = createTransition()
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close, transition)
      }
    }
  }
}
