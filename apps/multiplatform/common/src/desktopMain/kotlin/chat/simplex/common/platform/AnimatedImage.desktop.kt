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
// Counting frames builds a table the codec holds while it plays, several times the file's size for minimal ones
private const val MAX_ANIMATED_FRAMES = 10_000
// 10ms or less is how "as fast as possible" is written, and browsers substitute 100ms for it
private const val MAX_UNSPECIFIED_FRAME_DURATION_MS = 10
private const val DEFAULT_FRAME_DURATION_MS = 100L
private const val MIN_FRAME_DURATION_MS = 20L
// A frame costing more than this holds most of a core to show under 10 frames a second
private const val MAX_FRAME_DECODE_MS = 100L
// Far above what a frame within the bounds above can cost, so only a stall reaches it
private const val MAX_WAITED_FRAME_COST_MS = 10 * MAX_FRAME_DECODE_MS
private const val SLOW_FRAME_COST = 2
internal const val MAX_SLOW_FRAME_DEBT = 4
private const val NO_PRIOR_FRAME = -1
// A frame given no prior frame is rebuilt by recursing down its chain, so a long enough one overflows the
// native stack, which no catch can stop. Real animations rebuild nothing.
private const val MAX_REBUILT_FRAMES = 64

// Read once, as asking the codec about a frame allocates and the loop may repeat forever
private class Animation(val codec: Codec, val priorFrames: IntArray, val frameDelays: LongArray)

/**
 * The current frame of [data], or [still] when it is not an animation, falls outside the bounds above, or
 * fails before showing a frame; after that it stops on the frame it reached. Decoding runs off the UI thread.
 */
@Composable
fun rememberAnimatedImage(data: ByteArray, still: ImageBitmap, hidden: () -> Boolean = { false }): ImageBitmap {
  // Keyed as the decoding is, so frames are not written into a replaced state, and hidden is not a key so it
  // pauses instead of restarting. Every frame is a new wrapper, and only its identity says the image changed.
  val frame = remember(data, still) { mutableStateOf(still, neverEqualPolicy()) }
  LaunchedEffect(data, still) {
    withContext(animationDecoder) {
      val animation = openAnimation(data) ?: return@withContext
      try {
        playFrames(animation, hidden) { frame.value = it }
      } finally {
        animation.codec.close()
      }
    }
  }
  return frame.value
}

// Decoding several large animations must not starve the long running calls that share this pool
@OptIn(ExperimentalCoroutinesApi::class)
private val animationDecoder = Dispatchers.Default.limitedParallelism(2)

private fun openAnimation(data: ByteArray): Animation? {
  if (!looksAnimatable(data) || !fileSizeWithinBounds(data.size)) return null
  var codec: Codec? = null
  var animation: Animation? = null
  try {
    // Skia retains the encoded bytes, so this native buffer is freed as soon as the codec has taken it
    val encoded = Data.makeFromBytes(data)
    codec = try {
      Codec.makeFromData(encoded)
    } finally {
      encoded.close()
    }
    animation = boundedAnimation(codec)
  } catch (e: Throwable) {
    Log.e(TAG, "Unable to read animated image: $e")
  }
  // The codec is only left open for an animation that took it, so no bound can return past closing it
  if (animation == null) codec?.close()
  return animation
}

private fun boundedAnimation(codec: Codec): Animation? {
  val info = codec.imageInfo
  if (!rasterWithinBounds(info.width, info.height, info.bytesPerPixel)) return null
  // Counting frames scans the file, while dimensions are only read from the header
  val frameCount = codec.frameCount
  if (!frameCountWithinBounds(frameCount)) return null
  val requiredFrames = IntArray(frameCount)
  val frameDelays = LongArray(frameCount)
  for (i in 0 until frameCount) {
    val frameInfo = codec.getFrameInfo(i)
    requiredFrames[i] = frameInfo.requiredFrame
    frameDelays[i] = frameDuration(frameInfo.duration)
  }
  if (!rebuiltFramesWithinBounds(requiredFrames)) return null
  return Animation(codec, IntArray(frameCount) { priorFrame(it, requiredFrames[it]) }, frameDelays)
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

private suspend fun playFrames(animation: Animation, hidden: () -> Boolean, showFrame: (ImageBitmap) -> Unit) {
  try {
    val codec = animation.codec
    val bitmap = Bitmap()
    // The codec reports only the first frame's alpha type, and a frame with alpha cannot be read into an
    // opaque bitmap. allocPixels returns false rather than throwing.
    if (!bitmap.allocPixels(codec.imageInfo.withColorAlphaType(ColorAlphaType.PREMUL))) return
    var loopsLeft = codec.repetitionCount // negative repeats forever
    var debt = 0
    while (true) {
      for (i in animation.priorFrames.indices) {
        awaitFramesAreSeen(hidden)
        val startedDecoding = System.nanoTime()
        codec.readPixels(bitmap, i, animation.priorFrames[i])
        // Wall time, so a frame can overrun by being descheduled rather than by being expensive
        val decodedIn = System.nanoTime() - startedDecoding
        debt = slowFrameDebt(debt, decodedIn > MAX_FRAME_DECODE_MS * 1_000_000)
        // The bitmap is never closed, as the wrapper points at its pixels and a frame may still be drawn
        showFrame(bitmap.asComposeImageBitmap())
        if (debt >= MAX_SLOW_FRAME_DEBT) {
          Log.d(TAG, "Animation too expensive to decode, stopping on this frame")
          return
        }
        delay(frameWait(animation.frameDelays[i], decodedIn / 1_000_000))
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

// Composition survives the window being minimized or hidden, and the caller knows when its image cannot be seen
private suspend fun awaitFramesAreSeen(hidden: () -> Boolean) {
  if (framesAreSeen(hidden)) return
  snapshotFlow { framesAreSeen(hidden) }.first { it }
}

private fun framesAreSeen(hidden: () -> Boolean): Boolean =
  simplexWindowState.windowVisible.value && !simplexWindowState.windowState.isMinimized && !hidden()

// Waiting out the cost as well as the delay leaves an animation about half a decoder thread. The cost is
// wall time, so a stall is only waited out so far.
internal fun frameWait(delayMs: Long, costMs: Long): Long =
  maxOf(delayMs, costMs.coerceAtMost(MAX_WAITED_FRAME_COST_MS))

internal fun fileSizeWithinBounds(size: Int): Boolean = size <= MAX_ANIMATED_FILE_SIZE

// A file of no frames would spin the playback loop uncancellably, as it only suspends inside the range
internal fun frameCountWithinBounds(frameCount: Int): Boolean = frameCount in 2..MAX_ANIMATED_FRAMES

// The frame the codec may decode this one from, which is the one before it when the bitmap still holds it.
// Rebuilding the chain instead costs 9.10ms a frame against 0.05ms, and Skia refuses a frame it did not ask for.
internal fun priorFrame(index: Int, requiredFrame: Int): Int =
  if (requiredFrame == index - 1) index - 1 else NO_PRIOR_FRAME

// requiredFrames is what each frame continues; one that continues nothing starts a chain of its own
internal fun rebuiltFramesWithinBounds(requiredFrames: IntArray): Boolean {
  val chain = IntArray(requiredFrames.size)
  requiredFrames.forEachIndexed { index, required ->
    val continues = required in 0 until index
    chain[index] = if (continues) chain[required] + 1 else 1
    if (continues && priorFrame(index, required) == NO_PRIOR_FRAME && chain[required] > MAX_REBUILT_FRAMES) return false
  }
  return true
}

// Two expensive frames in a row reach the debt, and so do frames that alternate with cheap ones, which a
// count that reset would miss
internal fun slowFrameDebt(debt: Int, tooSlow: Boolean): Int =
  (debt + if (tooSlow) SLOW_FRAME_COST else -1).coerceAtLeast(0)

internal fun frameDuration(declaredMs: Int): Long =
  if (declaredMs <= MAX_UNSPECIFIED_FRAME_DURATION_MS) DEFAULT_FRAME_DURATION_MS
  else declaredMs.toLong().coerceAtLeast(MIN_FRAME_DURATION_MS)
