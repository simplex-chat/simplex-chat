package chat.simplex.common.views.chat.item

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.Easing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.MotionDurationScale
import androidx.compose.ui.geometry.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.clipPath
import androidx.compose.ui.graphics.drawscope.clipRect
import androidx.compose.ui.layout.*
import androidx.compose.ui.unit.*
import kotlin.math.*

// Spec: spec/client/chat-view.md#image-viewer-transition
data class ImageGallerySourceBounds(val bounds: Rect, val visible: Rect, val cornerRadius: Float, val crop: Boolean)

class ImageGalleryThumbnail(private val cornerRadius: Float, private val crop: Boolean) {
  var coordinates: LayoutCoordinates? = null
  var hidden by mutableStateOf(false)
  private var active = true

  fun capture(): ImageGallerySourceBounds? {
    val coordinates = coordinates?.takeIf { active && it.isAttached } ?: return null
    val bounds = Rect(coordinates.positionInWindow(), coordinates.size.toSize())
    val visible = coordinates.boundsInWindow().intersect(bounds)
    return if (bounds.validImageRect() && visible.validImageRect()) ImageGallerySourceBounds(bounds, visible, cornerRadius, crop) else null
  }

  fun dispose() {
    active = false
    coordinates = null
    hidden = false
  }
}

class ImageGalleryTransition(
  val image: ImageBitmap,
  val source: ImageGallerySourceBounds?,
  val currentSource: () -> ImageGallerySourceBounds?,
  val setSourceHidden: (Boolean) -> Unit,
)

data class ImageGalleryGeometry(val image: Rect, val clip: Rect, val visible: Rect, val cornerRadius: Float)

private fun Rect.validImageRect() = !isEmpty && listOf(left, top, right, bottom).all { it.isFinite() }

// Image bounds and viewport clips move independently, preserving the bitmap's
// aspect ratio while a cropped thumbnail expands to a fitted full-screen image.
fun imageGalleryGeometry(source: ImageGallerySourceBounds, viewport: Rect, image: Size, progress: Float): ImageGalleryGeometry? {
  if (!source.bounds.validImageRect() || !source.visible.validImageRect() || !viewport.validImageRect() ||
    !image.width.isFinite() || !image.height.isFinite() || image.width <= 0f || image.height <= 0f ||
    !source.cornerRadius.isFinite() || !progress.isFinite()) return null
  val p = progress.coerceIn(0f, 1f)
  fun centered(rect: Rect, scale: Float): Rect {
    val size = Size(image.width * scale, image.height * scale)
    return Rect(rect.center - Offset(size.width / 2, size.height / 2), size)
  }
  val sourceScale = if (source.crop) max(source.bounds.width / image.width, source.bounds.height / image.height) else source.bounds.width / image.width
  val start = centered(source.bounds, sourceScale)
  val end = centered(viewport, min(viewport.width / image.width, viewport.height / image.height))
  if (!start.validImageRect() || !end.validImageRect()) return null
  val origin = viewport.topLeft
  return ImageGalleryGeometry(
    lerp(start, end, p).translate(-origin),
    lerp(source.bounds, end, p).translate(-origin),
    lerp(source.visible, end, p).translate(-origin),
    source.cornerRadius.coerceAtLeast(0f) * (1f - p),
  )
}

private enum class ImageGalleryPhase { Opening, Open, ClosingToSource, ClosingFade }
private val imageGalleryEasing = Easing { ((1f - cos(it * PI.toFloat())) / 2f) }

@Composable
fun ImageGalleryAnimatedContainer(
  transition: ImageGalleryTransition?,
  canReturnToSource: () -> Boolean,
  close: () -> Unit,
  content: @Composable (close: () -> Unit, gesturesEnabled: Boolean) -> Unit,
) {
  var viewport by remember { mutableStateOf<Rect?>(null) }
  var source by remember { mutableStateOf(transition?.source) }
  var phase by remember { mutableStateOf(if (transition == null) ImageGalleryPhase.Open else ImageGalleryPhase.Opening) }
  val progress = remember { Animatable(if (transition == null) 1f else 0f) }
  val motionScale = rememberCoroutineScope().coroutineContext[MotionDurationScale]
  val finishClose by rememberUpdatedState(close)
  val imageSize = transition?.image?.let { Size(it.width.toFloat(), it.height.toFloat()) }
  DisposableEffect(transition) {
    if (transition?.source != null) transition.setSourceHidden(true)
    onDispose { transition?.setSourceHidden(false) }
  }

  val requestClose = {
    if (transition == null) {
      finishClose()
    } else if (phase != ImageGalleryPhase.ClosingToSource && phase != ImageGalleryPhase.ClosingFade) {
      val target = if (phase == ImageGalleryPhase.Opening) source else transition.currentSource()
      val viewportNow = viewport
      if ((phase == ImageGalleryPhase.Opening || canReturnToSource()) && target != null && viewportNow != null && imageSize != null &&
        target.visible.overlaps(viewportNow) && imageGalleryGeometry(target, viewportNow, imageSize, 0f) != null) {
        source = target
        phase = ImageGalleryPhase.ClosingToSource
      } else {
        transition.setSourceHidden(false)
        phase = ImageGalleryPhase.ClosingFade
      }
    }
  }

  LaunchedEffect(phase) {
    if (phase == ImageGalleryPhase.Open) return@LaunchedEffect
    val opening = phase == ImageGalleryPhase.Opening
    val target = if (opening) 1f else 0f
    if (motionScale?.scaleFactor == 0f) progress.snapTo(target)
    else progress.animateTo(target, tween(200, easing = imageGalleryEasing))
    if (opening) {
      if (phase == ImageGalleryPhase.Opening) phase = ImageGalleryPhase.Open
    } else if (phase == ImageGalleryPhase.ClosingToSource || phase == ImageGalleryPhase.ClosingFade) {
      finishClose()
    }
  }

  Box(Modifier.fillMaxSize().onGloballyPositioned { viewport = Rect(it.positionInWindow(), it.size.toSize()) }) {
    Canvas(Modifier.matchParentSize()) {
      drawRect(Color.Black, alpha = if (motionScale?.scaleFactor == 0f) 1f else progress.value)
    }
    // Animation values are read only during drawing: the pager's synchronous
    // media loader must not be called again for every animation frame.
    Box(Modifier.fillMaxSize().graphicsLayer {
      alpha = when {
        motionScale?.scaleFactor == 0f || phase == ImageGalleryPhase.Open -> 1f
        phase == ImageGalleryPhase.ClosingFade || source == null -> progress.value
        else -> 0f
      }
    }) {
      content(requestClose, phase == ImageGalleryPhase.Open)
    }
    Canvas(Modifier.matchParentSize()) {
      if (transition == null || motionScale?.scaleFactor == 0f || phase == ImageGalleryPhase.Open || phase == ImageGalleryPhase.ClosingFade) return@Canvas
      val src = source ?: return@Canvas
      val frame = imageGalleryGeometry(src, viewport ?: return@Canvas, imageSize ?: return@Canvas, progress.value) ?: return@Canvas
      val radius = frame.cornerRadius.coerceAtMost(min(frame.clip.width, frame.clip.height) / 2f)
      val clip = Path().apply { addRoundRect(RoundRect(frame.clip, CornerRadius(radius))) }
      clipPath(clip) {
        clipRect(frame.visible.left, frame.visible.top, frame.visible.right, frame.visible.bottom) {
          drawImage(
            transition.image,
            dstOffset = IntOffset(frame.image.left.roundToInt(), frame.image.top.roundToInt()),
            dstSize = IntSize(frame.image.width.roundToInt().coerceAtLeast(1), frame.image.height.roundToInt().coerceAtLeast(1)),
            filterQuality = FilterQuality.Medium,
          )
        }
      }
    }
    if (phase != ImageGalleryPhase.Open) {
      Box(Modifier.matchParentSize().clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = requestClose))
    }
  }
}