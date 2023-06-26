package chat.simplex.common.views.newchat

import androidx.compose.ui.graphics.*

actual fun ImageBitmap.replaceColor(from: Int, to: Int): ImageBitmap {
  val image = toAwtImage()
  val pixels = IntArray(width * height)
  image.getRGB(0, 0, width, height, pixels, 0, image.width)
  var i = 0
  while (i < pixels.size) {
    if (pixels[i] == from) {
      pixels[i] = to
    }
    i++
  }
  image.setRGB(0, 0, width, height, pixels, 0, image.width)
  return image.toComposeImageBitmap()
}