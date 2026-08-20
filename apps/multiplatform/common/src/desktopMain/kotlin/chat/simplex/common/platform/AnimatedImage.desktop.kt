package chat.simplex.common.platform

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asComposeImageBitmap
import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.first
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Codec
import org.jetbrains.skia.ColorAlphaType
import org.jetbrains.skia.Data

// Animated images are decoded from data received from other users, which is what the bounds below are for

// In bytes as the file chooses the color type, 1920x1920 at 4 bytes a pixel is ~15MB
private const val MAX_ANIMATED_RASTER_BYTES: Long = 1920L * 1920 * 4
// 65535x32 is only 2.1MP, so each side is bounded as well
private const val MAX_ANIMATED_SIDE = 4096
// Skia copies the encoded bytes into native memory and scans them to count frames
private const val MAX_ANIMATED_FILE_SIZE = 32 * 1024 * 1024
// Counting the frames also builds a table of them, which a file of minimal frames makes several times its own
// size, and the codec holds it for as long as the animation plays. The longest animation here has 1041 frames.
private const val MAX_ANIMATED_FRAMES = 10_000
// A frame may declare no delay, and 10ms or less is how "as fast as possible" is written. Browsers substitute
// 100ms for both.
private const val MAX_UNSPECIFIED_FRAME_DURATION_MS = 10
private const val DEFAULT_FRAME_DURATION_MS = 100L
// Bounds the frame rate of the rest
private const val MIN_FRAME_DURATION_MS = 20L
// A frame costing more than this holds most of a core to show under 10 frames a second
private const val MAX_FRAME_DECODE_MS = 100L
// What Skia calls a frame that is decoded without one
private const val NO_PRIOR_FRAME = -1
// A frame given no prior frame is rebuilt by decoding its chain back to the last independent frame, which
// Skia does by recursing, so a long enough chain overflows the native stack and no catch can stop it. Every
// animation in this repository rebuilds nothing, and a GIF disposing to what came before it rebuilds two.
private const val MAX_REBUILT_FRAMES = 64
// For a caller that is always seen where it is, like the full screen viewer
private val alwaysVisible: State<Boolean> = mutableStateOf(false)

/**
 * The current frame of [data], or [still] when [data] is not an animation, falls outside the bounds above, or
 * fails to decode. Decoding runs off the UI thread and stops when the caller leaves the composition.
 */
@Composable
fun rememberAnimatedImage(data: ByteArray, still: ImageBitmap, hidden: State<Boolean> = alwaysVisible): ImageBitmap {
  // The state is keyed as the decoding is, so frames are not written into a replaced state.
  // hidden is not a key, it pauses the animation instead of restarting it
  // Every frame is a new wrapper around one raster, and only the wrapper's identity says it changed
  val frame = remember(data, still) { mutableStateOf(still, neverEqualPolicy()) }
  LaunchedEffect(data, still) {
    withContext(animationDecoder) {
      val codec = animatableCodec(data) ?: return@withContext
      try {
        playFrames(codec, hidden) { frame.value = it }
      } finally {
        codec.close()
      }
    }
  }
  return frame.value
}

// Decoding several large animations must not starve the long running calls that share this pool
@OptIn(ExperimentalCoroutinesApi::class)
private val animationDecoder = Dispatchers.Default.limitedParallelism(2)

private fun animatableCodec(data: ByteArray): Codec? {
  if (!looksAnimatable(data) || data.size > MAX_ANIMATED_FILE_SIZE) return null
  var codec: Codec? = null
  try {
    // Skia retains the encoded bytes, so this native buffer is freed as soon as the codec has taken it
    val encoded = Data.makeFromBytes(data)
    codec = try {
      Codec.makeFromData(encoded)
    } finally {
      encoded.close()
    }
    val info = codec.imageInfo
    // Counting frames scans the file, while dimensions are only read from the header. Fewer than two frames
    // must not reach playFrames, whose loop only suspends inside the range and would spin uncancellably.
    if (!rasterWithinBounds(info.width, info.height, info.bytesPerPixel)) return null
    val frameCount = codec.frameCount
    if (frameCount in 2..MAX_ANIMATED_FRAMES &&
      rebuiltFramesWithinBounds(IntArray(frameCount) { codec.getFrameInfo(it).requiredFrame })) return codec
  } catch (e: Throwable) {
    Log.e(TAG, "Unable to read animated image: $e")
  }
  codec?.close()
  return null
}

internal fun looksAnimatable(data: ByteArray): Boolean =
  data.startsWith("GIF8") || (data.startsWith("RIFF") && data.startsWith("WEBP", offset = 8))

private fun ByteArray.startsWith(ascii: String, offset: Int = 0): Boolean {
  if (size < offset + ascii.length) return false
  return ascii.indices.all { this[offset + it] == ascii[it].code.toByte() }
}

internal fun rasterWithinBounds(width: Int, height: Int, bytesPerPixel: Int): Boolean {
  if (width !in 1..MAX_ANIMATED_SIDE || height !in 1..MAX_ANIMATED_SIDE) return false
  // 0 bytes per pixel would let any raster pass the bound below
  if (bytesPerPixel < 1) return false
  // The sides are bounded before they are multiplied, so the product cannot overflow
  return width.toLong() * height * bytesPerPixel <= MAX_ANIMATED_RASTER_BYTES
}

private suspend fun playFrames(codec: Codec, hidden: State<Boolean>, showFrame: (ImageBitmap) -> Unit) {
  val bitmap = Bitmap()
  try {
    // A frame that has alpha cannot be read into an opaque bitmap, and the codec reports the alpha type of
    // the first frame only, so a GIF that starts with an opaque frame disposed to the background would stop
    // animating on the second one. allocPixels returns false rather than throwing.
    if (!bitmap.allocPixels(codec.imageInfo.withColorAlphaType(ColorAlphaType.PREMUL))) return
    // Frames are only parsed by reading the frame count, and until they are, getFrameInfo below reports
    // uninitialised memory rather than failing
    val frameCount = codec.frameCount
    var loopsLeft = codec.repetitionCount // negative repeats forever
    var slowFrames = 0
    while (true) {
      for (i in 0 until frameCount) {
        awaitFramesAreSeen(hidden)
        // One frame at a time, reading the whole array costs an object per frame
        val info = codec.getFrameInfo(i)
        val startedDecoding = System.nanoTime()
        codec.readPixels(bitmap, i, priorFrame(i, info.requiredFrame))
        // Two in a row as this is wall time, one frame can overrun by being descheduled
        if (System.nanoTime() - startedDecoding > MAX_FRAME_DECODE_MS * 1_000_000) slowFrames++ else slowFrames = 0
        // A new wrapper around the same raster, so the state changes. The bitmap is never closed, as the
        // wrapper points at its pixels and a frame may still be drawn
        showFrame(bitmap.asComposeImageBitmap())
        if (slowFrames >= 2) {
          Log.d(TAG, "Animation too expensive to decode, stopping on this frame")
          return
        }
        delay(frameDuration(info.duration))
      }
      if (loopsLeft == 0) return
      if (loopsLeft > 0) loopsLeft--
    }
  } catch (e: CancellationException) {
    throw e // the view is gone, not a decoding failure
  } catch (e: Throwable) {
    Log.e(TAG, "Unable to play animated image: $e")
  }
}

// Composition survives the window being hidden in the tray, and the caller knows when its own image cannot
// be seen where it is
private suspend fun awaitFramesAreSeen(hidden: State<Boolean>) {
  if (framesAreSeen(hidden)) return
  snapshotFlow { framesAreSeen(hidden) }.first { it }
}

private fun framesAreSeen(hidden: State<Boolean>): Boolean =
  simplexWindowState.windowVisible.value && !hidden.value

/**
 * The frame the codec may decode [index] from, which is the previous one when the bitmap still holds what
 * [index] continues. Decoding the whole chain from the last independent frame instead costs 9.10ms a frame
 * against 0.05ms for images/groups.gif. Skia refuses a frame it did not ask for, so anything else is
 * [NO_PRIOR_FRAME] - including a predecessor disposed to what came before it, which it never asks for.
 */
internal fun priorFrame(index: Int, requiredFrame: Int): Int =
  if (requiredFrame == index - 1) index - 1 else NO_PRIOR_FRAME

/**
 * Whether every frame this codec cannot be given a prior frame for is rebuilt from a short enough chain.
 * [requiredFrames] is what each frame continues, as Skia reports it, and a frame that continues nothing or
 * something it cannot have starts a chain of its own.
 */
internal fun rebuiltFramesWithinBounds(requiredFrames: IntArray): Boolean {
  val chain = IntArray(requiredFrames.size)
  requiredFrames.forEachIndexed { index, required ->
    val continues = required in 0 until index
    chain[index] = if (continues) chain[required] + 1 else 1
    if (continues && priorFrame(index, required) == NO_PRIOR_FRAME && chain[required] > MAX_REBUILT_FRAMES) return false
  }
  return true
}

internal fun frameDuration(declaredMs: Int): Long =
  if (declaredMs <= MAX_UNSPECIFIED_FRAME_DURATION_MS) DEFAULT_FRAME_DURATION_MS
  else declaredMs.toLong().coerceAtLeast(MIN_FRAME_DURATION_MS)
