package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.unit.DpSize
import kotlin.math.*

// Spec: spec/client/chat-view.md#chat-photo-sizing
// Telegram Android ChatMessageCell single-photo bounds, expressed in dp. The
// 1280 limit is in physical pixels; it applies to portrait/tablet photos only.
internal fun chatImageSize(source: Size, window: Size, density: Float, minWidth: Float = 0f): Size {
  if (listOf(source.width, source.height, window.width, window.height, density).any { !it.isFinite() || it <= 0f }) return Size.Zero
  val shortSide = min(window.width, window.height)
  val longSide = max(window.width, window.height)
  val tablet = shortSide >= 600f
  val fullWidth = !tablet && source.width >= source.height
  val tabletSide = if (shortSide > 690f) {
    shortSide - max(320f, shortSide * 0.35f)
  } else {
    min(shortSide, longSide - max(320f, longSide * 0.35f))
  }
  val initialWidth = (when {
    tablet -> tabletSide * 0.7f
    fullWidth -> shortSide - 64f
    else -> shortSide * 0.7f
  }).coerceAtLeast(1f)
  val widthCap = if (fullWidth) initialWidth else min(initialWidth, 1280f / density)
  val heightCap = if (fullWidth) initialWidth + 100f else min(initialWidth + 100f, 1280f / density)
  var width = widthCap
  var height = width * source.height / source.width
  if (height > heightCap) {
    width *= heightCap / height
    height = heightCap
  } else if (height < 120f) {
    height = 120f
    width = min(widthCap, height * source.width / source.height)
  }
  // Like Telegram's timestamp minimum, make room for SimpleX's measured status
  // row without increasing the photo's height. Crop rather than stretch.
  width = max(width, minWidth.coerceIn(0f, widthCap))
  return Size(width, height)
}

@Composable
internal expect fun chatImageWindowSize(): DpSize