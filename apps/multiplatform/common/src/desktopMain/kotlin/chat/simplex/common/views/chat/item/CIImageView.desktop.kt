package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import chat.simplex.common.model.CIFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.ModalManager
import java.net.URI

@Composable
actual fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  // LALAL make it animated too
  ImageView(imageBitmap.toAwtImage().toPainter()) {
    if (getLoadedFilePath(file) != null) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}
