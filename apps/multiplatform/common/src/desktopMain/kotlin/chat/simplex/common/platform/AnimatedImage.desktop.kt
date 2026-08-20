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
// Far above what a frame within the bounds above can cost, so only a stall reaches it
private const val MAX_WAITED_FRAME_COST_MS = 10 * MAX_FRAME_DECODE_MS
private const val SLOW_FRAME_COST = 2
internal const val MAX_SLOW_FRAME_DEBT = 4
// What Skia calls a frame that is decoded without one
private const val NO_PRIOR_FRAME = -1
// A frame given no prior frame is rebuilt by decoding its chain back to the last independent frame, which
// Skia does by recursing, so a long enough chain overflows the native stack and no catch can stop it. Every
// animation in this repository rebuilds nothing, and a GIF disposing to what came before it rebuilds two.
private const val MAX_REBUILT_FRAMES = 64

// What playing an animation needs, read once. Asking the codec about a frame allocates, and a loop that
// repeats forever would ask again on every pass.
private class Animation(val codec: Codec, val priorFrames: IntArray, val frameDelays: LongArray)

/**
 * The current frame of [data], or [still] when [data] is not an animation, falls outside the bounds above, or
 * fails to decode. Decoding runs off the UI thread and stops when the caller leaves the composition.
 */
@Composable
fun rememberAnimatedImage(data: ByteArray, still: ImageBitmap, hidden: () -> Boolean = { false }): ImageBitmap {
  // The state is keyed as the decoding is, so frames are not written into a replaced state.
  // hidden is not a key, it pauses the animation instead of restarting it
  // Every frame is a new wrapper around one raster, and only the wrapper's identity says it changed
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
  if (!looksAnimatable(data) || data.size > MAX_ANIMATED_FILE_SIZE) return null
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
    animation = animationWithinBounds(codec)
  } catch (e: Throwable) {
    Log.e(TAG, "Unable to read animated image: $e")
  }
  // The codec is only left open for an animation that took it, so no bound can return past closing it
  if (animation == null) codec?.close()
  return animation
}

/** The animation [codec] holds if every bound admits it. The caller keeps the codec on every other path. */
private fun animationWithinBounds(codec: Codec): Animation? {
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
  // The frames that are played are the frames that were bounded, rather than read again and hoped alike
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
    // A frame that has alpha cannot be read into an opaque bitmap, and the codec reports the alpha type of
    // the first frame only, so a GIF that starts with an opaque frame disposed to the background would stop
    // animating on the second one. allocPixels returns false rather than throwing.
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
        // A new wrapper each frame, as its identity is what says the drawn image changed. The bitmap itself
        // is never closed, since the wrapper points at its pixels and a frame may still be drawn.
        showFrame(bitmap.asComposeImageBitmap())
        if (debt >= MAX_SLOW_FRAME_DEBT) {
          Log.d(TAG, "Animation too expensive to decode, stopping on this frame")
          return
        }
        // Waiting out what the frame cost as well as what it asks for leaves the animation about half of a
        // decoder thread, however expensive its frames are. Frames cheaper than their delay, which is all of
        // a real animation's, wait exactly as long as they always did. The cost is wall time, so it is only
        // waited out as far as a frame can really take: a machine that stalls mid-decode, or suspends while
        // this thread is inside it, should not leave an animation waiting for as long as it was away.
        delay(maxOf(animation.frameDelays[i], (decodedIn / 1_000_000).coerceAtMost(MAX_WAITED_FRAME_COST_MS)))
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
private suspend fun awaitFramesAreSeen(hidden: () -> Boolean) {
  if (framesAreSeen(hidden)) return
  snapshotFlow { framesAreSeen(hidden) }.first { it }
}

private fun framesAreSeen(hidden: () -> Boolean): Boolean =
  simplexWindowState.windowVisible.value && !hidden()

/**
 * Whether this is an animation of a length worth holding frames for. A file of no frames would spin the
 * playback loop uncancellably, as it only suspends inside the range of frames, and one of a single frame
 * would decode that frame again for as long as it was shown.
 */
internal fun frameCountWithinBounds(frameCount: Int): Boolean = frameCount in 2..MAX_ANIMATED_FRAMES

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

/**
 * What an animation owes after a frame that [tooSlow] says cost too much, or one that did not. Two expensive
 * frames in a row reach [MAX_SLOW_FRAME_DEBT], one that only overran by being descheduled is paid off, and
 * frames that alternate expensive with cheap - which are never two in a row - still reach it.
 */
internal fun slowFrameDebt(debt: Int, tooSlow: Boolean): Int =
  (debt + if (tooSlow) SLOW_FRAME_COST else -1).coerceAtLeast(0)

internal fun frameDuration(declaredMs: Int): Long =
  if (declaredMs <= MAX_UNSPECIFIED_FRAME_DURATION_MS) DEFAULT_FRAME_DURATION_MS
  else declaredMs.toLong().coerceAtLeast(MIN_FRAME_DURATION_MS)
