package chat.simplex.common.platform

import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.BlurEffect
import androidx.compose.ui.graphics.asComposeCanvas
import androidx.compose.ui.graphics.drawscope.CanvasDrawScope
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.drawscope.drawIntoCanvas
import androidx.compose.ui.graphics.drawscope.scale
import androidx.compose.ui.graphics.nativeCanvas
import androidx.compose.ui.unit.Density
import org.jetbrains.skia.*
import kotlin.math.ceil

// Skia's raster blur costs width x (bar + 6 sigma) pixels every frame, which is what makes the bars lag. The blur cannot be
// made cheaper by scaling the layer it runs on, because a layer's filter is evaluated in device space: any scale applied to
// the layer scales the sigma straight back up, which weakens the blur without saving anything. So the bar is drawn into a
// narrower offscreen surface and blurred there, at its own resolution, and only the finished blur is scaled back out.
// Only the width is reduced: a bar is just AppBarHeight tall, and taking rows away from it visibly weakens the blur.
private const val MAX_BLUR_NARROWING = 8
private const val MIN_BLUR_WIDTH = 128

// How small the blur's own sigma is allowed to get. Below about this the narrowed copy is barely blurred, and stretching it
// back out shows the steps of the narrowing instead of a blur.
private const val MIN_NARROWED_SIGMA = 1.5f

// The rows are halved as well, with the vertical sigma halved to match, which is only sound because the copy is drawn at
// that size rather than scaled afterwards. A bar has few rows to begin with, so it is not reduced past this.
private const val MIN_BLUR_HEIGHT = 24

// The panes of the desktop window have different widths, so bars of several sizes are drawn in the same frame and a single
// pair of surfaces would be reallocated for each of them. Keeping one pair per size, with the least recently drawn size
// released once there are more than the window can show at once, allocates each pair once instead.
private const val MAX_BAR_SURFACES = 4

private class BarBlurSurface(width: Int, height: Int) {
  val surface: Surface = Surface.makeRaster(ImageInfo.makeN32Premul(width, height))

  fun close() = surface.close()
}

private val barBlurSurfaces = ThreadLocal.withInitial { LinkedHashMap<Long, BarBlurSurface>() }

private fun surfaceFor(width: Int, height: Int): BarBlurSurface {
  val cached = barBlurSurfaces.get()
  val key = (width.toLong() shl 32) or height.toLong()
  val surface = cached.remove(key) ?: BarBlurSurface(width, height)
  cached[key] = surface
  while (cached.size > MAX_BAR_SURFACES) {
    cached.remove(cached.keys.first())?.close()
  }
  return surface
}

private fun narrowing(sigma: Float, barWidth: Float): Int {
  var scale = 1
  while (sigma / (scale * 2) >= MIN_NARROWED_SIGMA && scale < MAX_BLUR_NARROWING && barWidth / (scale * 2) >= MIN_BLUR_WIDTH) scale *= 2
  return scale
}

private fun shortening(sigma: Float, barHeight: Float): Int =
  if (sigma / 2 >= MIN_NARROWED_SIGMA && barHeight / 2 >= MIN_BLUR_HEIGHT) 2 else 1

actual fun DrawScope.drawBarsBlurred(radiusPx: Float, barWidth: Float, barHeight: Float, drawBar: DrawScope.() -> Unit) {
  val sigma = BlurEffect.convertRadiusToSigma(radiusPx)
  val scale = narrowing(sigma, barWidth)
  val rows = shortening(sigma, barHeight)
  val width = ceil(barWidth / scale).toInt()
  val height = ceil(barHeight / rows).toInt()
  if (width <= 0 || height <= 0) return
  val surface = surfaceFor(width, height).surface

  surface.canvas.clear(Color.TRANSPARENT)
  // the blur is the layer's own paint, so it runs at this surface's resolution rather than at the canvas the bar draws to
  surface.canvas.saveLayer(Rect.makeWH(width.toFloat(), height.toFloat()), Paint().apply {
    imageFilter = ImageFilter.makeBlur(sigma / scale, sigma / rows, FilterTileMode.CLAMP)
  })
  CanvasDrawScope().draw(Density(density, fontScale), layoutDirection, surface.canvas.asComposeCanvas(), Size(width.toFloat(), height.toFloat())) {
    scale(1f / scale, 1f / rows, Offset.Zero) { drawBar() }
  }
  surface.canvas.restore()

  // The surface is whole pixels while the bar is not, so only the bar's own area is taken from it.
  drawIntoCanvas {
    it.nativeCanvas.drawImageRect(
      surface.makeImageSnapshot(),
      Rect.makeWH(barWidth / scale, barHeight / rows),
      Rect.makeWH(barWidth, barHeight),
      SamplingMode.LINEAR,
      null,
      true
    )
  }
}
