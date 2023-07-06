package chat.simplex.app.views.newchat

import android.graphics.Bitmap

fun Bitmap.replaceColor(from: Int, to: Int): Bitmap {
  val pixels = IntArray(width * height)
  getPixels(pixels, 0, width, 0, 0, width, height)
  var i = 0
  while (i < pixels.size) {
    if (pixels[i] == from) {
      pixels[i] = to
    }
    i++
  }
  setPixels(pixels, 0, width, 0, 0, width, height)
  return this
}