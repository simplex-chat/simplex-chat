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
// Counting the frames also builds a table of them, which a file of minimal frames can make several times its
// own size. The longest animation in this repository has 1041 frames.
private const val MAX_ANIMATED_FRAMES = 10_000
// A frame may declare no delay, and 10ms or less is how "as fast as possible" is written. Browsers substitute
// 100ms for both.
private const val UNSPECIFIED_FRAME_DURATION_MS = 10
private const val DEFAULT_FRAME_DURATION_MS = 100L
// Bounds the frame rate of the rest
private const val MIN_FRAME_DURATION_MS = 20L
// A frame costing more than this holds most of a core to show under 10 frames a second
private const val MAX_FRAME_DECODE_MS = 100L

/**
 * The current frame of [data], or [still] when [data] is not an animation, falls outside the bounds above, or
 * fails to decode. Decoding runs off the UI thread and stops when the caller leaves the composition.
 */
@Composable
fun rememberAnimatedImage(data: ByteArray, still: ImageBitmap, blurred: State<Boolean>? = null): State<ImageBitmap> {
  // The state is keyed as the decoding is, so frames are not written into a replaced state.
  // blurred is not a key, it pauses the animation instead of restarting it
  val frame = remember(data, still) { mutableStateOf(still) }
  LaunchedEffect(data, still) {
    withContext(animationDecoder) {
      val codec = animatableCodec(data) ?: return@withContext
      try {
        playFrames(codec, blurred) { frame.value = it }
      } finally {
        codec.close()
      }
    }
  }
  return frame
}

// Decoding several large animations must not starve the coroutines delivering messages
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
    // Counting frames scans the file, while dimensions are only read from the header
    if (rasterWithinBounds(info.width, info.height, info.bytesPerPixel) && codec.frameCount in 2..MAX_ANIMATED_FRAMES) return codec
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

private suspend fun playFrames(codec: Codec, blurred: State<Boolean>?, showFrame: (ImageBitmap) -> Unit) {
  val bitmap = Bitmap()
  try {
    // A frame that has alpha cannot be read into an opaque bitmap, and the codec reports the alpha type of
    // the first frame only, so a GIF that starts with an opaque frame disposed to the background would stop
    // animating on the second one. allocPixels returns false rather than throwing.
    if (!bitmap.allocPixels(codec.imageInfo.withColorAlphaType(ColorAlphaType.PREMUL))) return
    // Frames are only parsed by reading the frame count, and until they are, getFrameInfo below reports
    // uninitialised memory rather than failing
    val frameCount = codec.frameCount
    // The loop below only suspends inside the range, so without frames it would spin uncancellably
    if (frameCount < 2) return
    var loopsLeft = codec.repetitionCount // negative repeats forever
    var slowFrames = 0
    while (true) {
      for (i in 0 until frameCount) {
        awaitFramesAreSeen(blurred)
        // One frame at a time, reading the whole array costs an object per frame
        val info = codec.getFrameInfo(i)
        val startedDecoding = System.nanoTime()
        // The bitmap still holds the previous frame, and without saying so the codec decodes the whole chain
        // from the last independent frame: 9.10ms a frame against 0.07ms for images/groups.gif
        if (i > 0 && info.requiredFrame == i - 1) codec.readPixels(bitmap, i, i - 1) else codec.readPixels(bitmap, i)
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

// Composition survives the window being hidden in the tray, and a blurred image is only revealed on hover
private suspend fun awaitFramesAreSeen(blurred: State<Boolean>?) {
  if (framesAreSeen(blurred)) return
  snapshotFlow { framesAreSeen(blurred) }.first { it }
}

private fun framesAreSeen(blurred: State<Boolean>?): Boolean =
  simplexWindowState.windowVisible.value && blurred?.value != true

internal fun frameDuration(declaredMs: Int): Long =
  if (declaredMs <= UNSPECIFIED_FRAME_DURATION_MS) DEFAULT_FRAME_DURATION_MS
  else declaredMs.toLong().coerceAtLeast(MIN_FRAME_DURATION_MS)
