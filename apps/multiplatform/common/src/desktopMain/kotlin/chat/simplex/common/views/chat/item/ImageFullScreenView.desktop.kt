package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.getBitmapFromByteArray
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableSharedFlow
import org.jetbrains.compose.videoplayer.SkiaBitmapVideoSurface
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow

private const val WHEEL_ZOOM_STEP = 1.2f
private const val MAC_SCROLL_UNITS_PER_NOTCH = 3f
private val DRAG_THRESHOLD = 4.dp

internal val pinchZooms = MutableSharedFlow<Float>(extraBufferCapacity = Int.MAX_VALUE)

@Composable
actual fun FullScreenImageView(modifier: Modifier, data: ByteArray, imageBitmap: ImageBitmap) {
  // Decoded once, as an animation recomposes this on every frame
  val still = remember(data) { getBitmapFromByteArray(data, false) ?: MR.images.decentralized.image.toComposeImageBitmap() }
  val image = rememberAnimatedImage(data, still)
  Image(
    image,
    contentDescription = stringResource(MR.strings.image_descr),
    contentScale = ContentScale.Fit,
    modifier = modifier.zoomable(image),
  )
}

@Composable
actual fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier, close: () -> Unit) {
  Box {
    Box(Modifier.fillMaxSize().padding(bottom = 50.dp)) {
      SurfaceFromPlayer(player, modifier, frameModifier = { Modifier.zoomable(it) })
      IconButton(onClick = close, Modifier.padding(top = 5.dp)) {
        Icon(painterResource(MR.images.ic_arrow_back_ios_new), null, Modifier.size(30.dp), tint = MaterialTheme.colors.primary)
      }
    }
    Controls(player)
  }
}

@Composable
fun BoxScope.SurfaceFromPlayer(player: VideoPlayer, modifier: Modifier, contentScale: ContentScale = ContentScale.Fit, frameModifier: @Composable (ImageBitmap) -> Modifier = { Modifier }) {
  val surface = remember {
    SkiaBitmapVideoSurface().also {
      player.player.videoSurface().set(it)
    }
  }
  surface.bitmap.value?.let { bitmap ->
    Image(
      bitmap,
      modifier = modifier.then(frameModifier(bitmap)).align(Alignment.Center),
      contentDescription = null,
      contentScale = contentScale,
      alignment = Alignment.Center,
    )
  }
}

@Composable
private fun BoxScope.Controls(player: VideoPlayer) {
  val playing = remember(player) { player.videoPlaying }
  val progress = remember(player) { player.progress }
  val duration = remember(player) { player.duration }
  Row(Modifier.fillMaxWidth().align(Alignment.BottomCenter).height(50.dp)) {
    IconButton(onClick = { if (playing.value) player.player.pause() else player.play(true) },) {
      Icon(painterResource(if (playing.value) MR.images.ic_pause_filled else MR.images.ic_play_arrow_filled), null, Modifier.size(30.dp), tint = MaterialTheme.colors.primary)
    }
    Slider(
      value = progress.value.toFloat() / max(0.0001f, duration.value.toFloat()),
      onValueChange = { player.player.seekTo((it * duration.value).toInt()) },
      modifier = Modifier.fillMaxWidth()
    )
  }
}

@Composable
private fun Modifier.zoomable(content: ImageBitmap): Modifier {
  var scale by remember { mutableStateOf(1f) }
  var translation by remember { mutableStateOf(Offset.Zero) }
  var viewSize by remember { mutableStateOf(IntSize.Zero) }
  val contentSize by rememberUpdatedState(IntSize(content.width, content.height))
  fun panBy(delta: Offset) {
    translation = boundTranslation(translation + delta, viewSize, contentSize, scale)
  }
  LaunchedEffect(contentSize) { panBy(Offset.Zero) }
  return this
    .onSizeChanged {
      viewSize = it
      panBy(Offset.Zero)
    }
    .pointerInput(Unit) {
      detectDragPastThreshold { pan -> panBy(pan) }
    }
    .pointerInput(Unit) {
      awaitPointerEventScope {
        while (true) {
          val event = awaitPointerEvent()
          if (event.type != PointerEventType.Scroll) continue
          val change = event.changes.first()
          if (change.scrollDelta.y == 0f) continue
          val notches = wheelNotches(change.scrollDelta.y)
          val zoomed = (scale * WHEEL_ZOOM_STEP.pow(-notches)).coerceIn(1f, 20f)
          val pointer = change.position - Offset(size.width / 2f, size.height / 2f) - translation
          val shift = pointer * (1 - zoomed / scale)
          scale = zoomed
          panBy(shift)
          change.consume()
        }
      }
    }
    .clipToBounds()
    .graphicsLayer {
      scaleX = scale
      scaleY = scale
      translationX = translation.x
      translationY = translation.y
    }
}

private fun boundTranslation(translation: Offset, view: IntSize, content: IntSize, scale: Float): Offset {
  val fit = min(view.width.toFloat() / content.width, view.height.toFloat() / content.height)
  val maxX = max(0f, (content.width * fit * scale - view.width) / 2)
  val maxY = max(0f, (content.height * fit * scale - view.height) / 2)
  return Offset(translation.x.coerceIn(-maxX, maxX), translation.y.coerceIn(-maxY, maxY))
}

private suspend fun PointerInputScope.detectDragPastThreshold(onDrag: (Offset) -> Unit) = awaitEachGesture {
  val down = awaitFirstDown(requireUnconsumed = false)
  var pan = Offset.Zero
  var dragging = false
  do {
    val change = awaitPointerEvent().changes.firstOrNull { it.id == down.id } ?: break
    pan += change.positionChange()
    if (!dragging) dragging = pan.getDistance() > DRAG_THRESHOLD.toPx()
    if (dragging) {
      onDrag(pan)
      pan = Offset.Zero
      change.consume()
    }
  } while (change.pressed)
}

private fun wheelNotches(scrollDelta: Float): Float =
  if (desktopPlatform.isMac()) scrollDelta / MAC_SCROLL_UNITS_PER_NOTCH else scrollDelta
