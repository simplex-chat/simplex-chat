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
  // The small view is the chat list preview, which the layout keeps on screen without pause, so it stays a
  // still image. A full screen modal is shown beside the chat rather than in place of it, so this item keeps
  // composing under one and would otherwise decode where nobody can see it.
  val frame = if (smallView) imageBitmap
  else rememberAnimatedImage(data, imageBitmap) { blurred.value || ModalManager.fullscreen.hasModalsOpen() }
  ImageView(BitmapPainter(frame)) {
    if (getLoadedFilePath(file) != null) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}
