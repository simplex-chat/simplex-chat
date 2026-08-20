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
  // The small view is the chat list preview: a 36sp box that the desktop layout keeps on screen the whole
  // time. Decoding an animation at its own resolution to fill it would hold a raster and spend a frame of
  // work per listed chat, without pause, so it keeps the still image as it did before.
  // A full screen modal is shown beside the chat rather than in place of it, so this item keeps composing
  // under one: the viewer would otherwise decode the same animation a second time, and every other animation
  // in the chat would keep decoding where nobody can see it
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
