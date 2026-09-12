package chat.simplex.common.platform

import androidx.compose.foundation.combinedClickable
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawWithContent
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.unit.IntSize
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.KeyChangeEffect
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.filter
import java.io.File
import kotlin.math.roundToInt

@Composable
expect fun Modifier.desktopOnExternalDrag(
  enabled: Boolean = true,
  onFiles: (List<File>) -> Unit = {},
  onImage: (File) -> Unit = {},
  onText: (String) -> Unit = {}
): Modifier

expect fun Modifier.onRightClick(action: () -> Unit): Modifier

expect fun Modifier.desktopPointerHoverIconHand(): Modifier

expect fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier

@Composable
fun Modifier.desktopModifyBlurredState(enabled: Boolean, blurred: MutableState<Boolean>, showMenu: State<Boolean>,): Modifier {
  val blurRadius = remember { appPrefs.privacyMediaBlurRadius.state }
  if (appPlatform.isDesktop) {
    KeyChangeEffect(blurRadius.value) {
      blurred.value = enabled && blurRadius.value > 0
    }
  }
  return if (appPlatform.isDesktop && enabled && blurRadius.value > 0 && !showMenu.value) {
    var job: Job = remember { Job() }
    LaunchedEffect(Unit) {
      // The approach here is to allow menu to show up and to not blur the view. When menu is shown and mouse is hovering,
      // unhovered action is still received, but we don't need to handle it until menu closes. When it closes, it takes one frame to catch a
      // hover action again and if:
      // 1. mouse is still on the view, the hover action will cancel this coroutine and the view will stay unblurred
      // 2. mouse is not on the view, the view will become blurred after 100 ms
      job = launch {
        delay(100)
        blurred.value = true
      }
    }
    this then Modifier.desktopOnHovered { hovered ->
      job.cancel()
      blurred.value = !hovered && !showMenu.value
    }
  } else {
    this
  }
}

@Composable
fun blurHidesMedia(enabled: Boolean, blurred: State<Boolean>): Boolean =
  enabled && blurred.value && remember { appPrefs.privacyMediaBlurRadius.state }.value > 0

@Composable
fun Modifier.privacyBlur(
  enabled: Boolean,
  preview: ImageBitmap,
  blurred: MutableState<Boolean> = remember { mutableStateOf(appPrefs.privacyMediaBlurRadius.get() > 0) },
  scrollState: State<Boolean>,
  onLongClick: () -> Unit = {}
): Modifier {
  val blurRadius = remember { appPrefs.privacyMediaBlurRadius.state }
  return if (blurHidesMedia(enabled, blurred)) {
    val blurredPreview = remember(preview, blurRadius.value) { preview.blurredBy(blurRadius.value) }
    this then Modifier
      .drawWithContent { drawImage(blurredPreview, dstSize = IntSize(size.width.roundToInt(), size.height.roundToInt())) }
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = {
          blurred.value = false
        }
      )
  } else if (enabled && blurRadius.value > 0 && appPlatform.isAndroid) {
      LaunchedEffect(Unit) {
        snapshotFlow { scrollState.value }
          .filter { it }
          .filter { !blurred.value }
          .collect { blurred.value = true }
      }
      this
    } else {
      this
    }
}

// Media is drawn about this wide, so the resample keeps roughly one pixel per blur radius.
private const val BLURRED_MEDIA_WIDTH_DP = 360
// Bounds the first step: nothing bounds a decoded video frame, and reading every pixel of a 4K one would stall.
private const val RESAMPLE_MEDIA_FROM_SIDE = 512

private fun ImageBitmap.blurredBy(radius: Int): ImageBitmap {
  if (width <= 0 || height <= 0) return this
  val w = (BLURRED_MEDIA_WIDTH_DP / radius).coerceIn(1, width)
  val h = (w * height / width).coerceIn(1, BLURRED_MEDIA_WIDTH_DP)
  val longest = maxOf(width, height)
  var image = if (longest > RESAMPLE_MEDIA_FROM_SIDE) {
    val step = RESAMPLE_MEDIA_FROM_SIDE.toFloat() / longest
    scale((width * step).roundToInt().coerceAtLeast(1), (height * step).roundToInt().coerceAtLeast(1))
  } else this
  // One bilinear step from a large image undersamples it.
  while (image.width / 2 > w) image = image.scale(image.width / 2, (image.height / 2).coerceAtLeast(1))
  return image.scale(w, h)
}
