package chat.simplex.common.views.newchat

import androidx.compose.ui.graphics.*

actual fun ImageBitmap.replaceColor(from: Int, to: Int): ImageBitmap {
  val pixels = IntArray(width * height)
  val bitmap = this.asAndroidBitmap()
  bitmap.getPixels(pixels, 0, width, 0, 0, width, height)
  var i = 0
  while (i < pixels.size) {
    if (pixels[i] == from) {
      pixels[i] = to
    }
    i++
  }
  bitmap.setPixels(pixels, 0, width, 0, 0, width, height)
  return bitmap.asImageBitmap()
}