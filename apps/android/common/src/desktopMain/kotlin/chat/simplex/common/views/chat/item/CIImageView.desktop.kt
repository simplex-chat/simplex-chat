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
  uri: URI,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  val view = LocalMultiplatformView()
  // LALAL make it animated too
  ImageView(imageBitmap.toAwtImage().toPainter()) {
    hideKeyboard(view)
    if (getLoadedFilePath(file) != null) {
      ModalManager.shared.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}