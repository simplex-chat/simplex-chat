package chat.simplex.common.platform

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asComposeImageBitmap
import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.first
import org.jetbrains.skia.AnimationFrameInfo
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Codec
import org.jetbrains.skia.Data

// The bytes decoded here come from whoever sent the file, so each bound below answers a specific crafted
// input rather than estimating what is reasonable. Whatever falls outside the bounds keeps showing the still
// image the chat already renders: animation degrades to a picture, never to an error. Failures are logged and
// never alerted - an alert per malformed file would itself let a sender disrupt the app.

// Largest animation we hold frames for: one reused raster of 1920x1920 at four bytes a pixel is ~15MB, and
// several can be on screen at once. A crafted file can ask for far more - a 35-byte GIF declaring
// 65535x65535 asks for 17GB - and it also chooses the colour type, so this is measured in bytes rather than
// assuming four of them per pixel.
private const val MAX_ANIMATED_RASTER_BYTES: Long = 1920L * 1920 * 4
// Neither side may exceed this, independently of the raster bound: 65535x32 is only 2.1MP and would otherwise
// be animated with a 65535-pixel scanline. Wide enough to leave banner-shaped animations playing.
private const val MAX_ANIMATED_SIDE = 4096
// Skia copies the encoded bytes into native memory and scans them to count frames, so the file size bounds
// both. Comfortably above real animations - the largest in this repository is 1.5MB.
private const val MAX_ANIMATED_FILE_SIZE = 32 * 1024 * 1024
// A frame may declare no delay at all: a 4.6MB GIF can hold 200 000 zero-delay frames, which would decode
// flat out for as long as it stayed on screen. Browsers substitute 100ms for an unspecified delay, and the
// floor bounds the rate for the rest. Long delays are left alone - they are the author's, and they cost
// nothing to honour.
private const val DEFAULT_FRAME_DURATION_MS = 100L
private const val MIN_FRAME_DURATION_MS = 20L
// An animation whose frames cost more than this to decode is left as a still. A 1244x554 animation decodes a
// frame in under 3ms and spends under 2% of a core playing; a 2000x891 one takes 180ms a frame, which is a
// whole core held to show about five frames a second. The still is the better picture and costs nothing.
private const val MAX_FRAME_DECODE_MS = 100L

/**
 * The frame of [data] to draw right now, or [still] when [data] is not an animation, falls outside the bounds
 * above, or fails to decode. Decoding runs off the UI thread; the animation stops when the caller leaves.
 */
@Composable
fun rememberAnimatedImage(data: ByteArray, still: ImageBitmap, blurred: State<Boolean>? = null): State<ImageBitmap> {
  // The state and the decoding are keyed alike on purpose: were the state replaced without the decoding
  // restarting, frames would keep being written into a state nobody reads. The blur is deliberately not a
  // key - it changes as the mouse moves, and it pauses the animation rather than starting it over.
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

// Decoding is CPU work on somebody else's data, so it is confined to a small share of the shared pool:
// several large animations on screen must not starve the coroutines that deliver messages.
@OptIn(ExperimentalCoroutinesApi::class)
private val animationDecoder = Dispatchers.Default.limitedParallelism(2)

// A codec for data worth animating, or null. Null covers "not an animation" and "outside the bounds" alike,
// because the caller responds to both the same way - by keeping the still image.
private fun animatableCodec(data: ByteArray): Codec? {
  // Photos are almost all of what a chat holds and none of them are animations, so they leave here without
  // their bytes ever being copied into native memory or handed to a second decoder.
  if (!looksAnimatable(data) || data.size > MAX_ANIMATED_FILE_SIZE) return null
  var codec: Codec? = null
  try {
    // Skia keeps its own reference to the encoded bytes, so this copy is released the moment the codec has
    // taken it, rather than left to the collector: it is a native buffer of up to the file size behind a
    // small Java object, which is not something the collector has much reason to reclaim. Frames still
    // decode afterwards - checked against a codec whose Data had already been freed.
    val encoded = Data.makeFromBytes(data)
    codec = try {
      Codec.makeFromData(encoded)
    } finally {
      encoded.close()
    }
    val info = codec.imageInfo
    // Dimensions are read from the header, while counting frames scans the file, so the frames are only
    // counted once the dimensions are known to be sane
    if (rasterWithinBounds(info.width, info.height, info.bytesPerPixel) && codec.frameCount > 1) return codec
  } catch (e: Throwable) {
    // Not the stack trace: this runs on data from other people, at the rate they can send it
    Log.e(TAG, "Unable to read animated image: $e")
  }
  codec?.close()
  return null
}

// The two container formats this app treats as animated, recognised by their own bytes rather than by the
// name the sender chose for the file.
internal fun looksAnimatable(data: ByteArray): Boolean =
  data.startsWith("GIF8") || (data.startsWith("RIFF") && data.startsWith("WEBP", offset = 8))

private fun ByteArray.startsWith(ascii: String, offset: Int = 0): Boolean {
  if (size < offset + ascii.length) return false
  return ascii.indices.all { this[offset + it] == ascii[it].code.toByte() }
}

// Whether a frame of this size is one we will hold in memory, as plain numbers so that the bounds can be
// checked on their own - they are the part that has to be right about a file someone else composed.
internal fun rasterWithinBounds(width: Int, height: Int, bytesPerPixel: Int): Boolean {
  if (width !in 1..MAX_ANIMATED_SIDE || height !in 1..MAX_ANIMATED_SIDE) return false
  // A colour type claiming no bytes per pixel would make any raster look free
  if (bytesPerPixel < 1) return false
  // The sides are bounded before they are multiplied, so that the product cannot wrap: 65535 * 65535 already
  // overflows Int to a negative number, and an unbounded three-way product can overflow Long as well.
  return width.toLong() * height * bytesPerPixel <= MAX_ANIMATED_RASTER_BYTES
}

private suspend fun playFrames(codec: Codec, blurred: State<Boolean>?, showFrame: (ImageBitmap) -> Unit) {
  val bitmap = Bitmap()
  try {
    // allocPixels reports failure by returning false rather than throwing, and reading a frame into an
    // unallocated bitmap throws, so the result is checked instead of assumed.
    if (!bitmap.allocPixels(codec.imageInfo)) return
    // Reading the frame count is also what makes getFrameInfo below work at all: in skiko 0.9.4 the
    // single-frame accessor reads past its own buffer until the count has been taken (or the whole frames
    // array, which costs an object per frame). The loop is bounded by the count, so the order holds.
    // Never loop over fewer frames than an animation has, either: the loop only suspends inside the range,
    // so a frameless codec would spin a core forever and could not even be cancelled, and a single-frame one
    // would re-decode the same picture for as long as it was on screen.
    val frameCount = codec.frameCount
    if (frameCount < 2) return
    var loopsLeft = codec.repetitionCount // negative repeats forever
    var slowFrames = 0
    while (true) {
      for (i in 0 until frameCount) {
        awaitFramesAreSeen(blurred)
        val startedDecoding = System.nanoTime()
        codec.readPixels(bitmap, i)
        // Two in a row, because this is wall time: one frame can overrun simply by being descheduled, and a
        // busy machine should not turn a cheap animation into a still. Expensive ones overrun every frame.
        if (System.nanoTime() - startedDecoding > MAX_FRAME_DECODE_MS * 1_000_000) slowFrames++ else slowFrames = 0
        if (slowFrames >= 2) {
          Log.d(TAG, "Animation too expensive to decode, stopping on this frame")
          return
        }
        // A new wrapper around the same raster, so the state sees a change - as the video surface does.
        // The bitmap itself is deliberately never closed: the wrapper handed to Compose points at its
        // pixels, and freeing them while a frame may still be drawn would be a use-after-free.
        showFrame(bitmap.asComposeImageBitmap())
        // Frame info is read one frame at a time: reading the whole array costs 200 000 objects (~24MB) for
        // a 4.6MB file. The wait is not shortened by the time decoding took - playing a little slower than
        // authored is better than a sleep that can shrink to nothing.
        delay(frameDuration(codec.getFrameInfo(i)))
      }
      if (loopsLeft == 0) return
      if (loopsLeft > 0) loopsLeft--
    }
  } catch (e: CancellationException) {
    throw e // the view went away, which is not a decoding failure
  } catch (e: Throwable) {
    // Every call above reads a file composed by somebody else, and the frame count and repeat count are read
    // from it too. A failure in any of them ends the animation on the last frame that decoded, instead of
    // reaching the composition as a crash.
    Log.e(TAG, "Unable to play animated image: $e")
  }
}

// Frames are only decoded while there is somebody to see them. The app is built to live in the tray and
// composition survives being hidden there; and a blurred image is only revealed while the mouse is over it,
// so the rest of the time each frame would be decoded, uploaded and then blurred away again for nobody.
private suspend fun awaitFramesAreSeen(blurred: State<Boolean>?) {
  if (framesAreSeen(blurred)) return
  snapshotFlow { framesAreSeen(blurred) }.first { it }
}

private fun framesAreSeen(blurred: State<Boolean>?): Boolean =
  simplexWindowState.windowVisible.value && blurred?.value != true

private fun frameDuration(info: AnimationFrameInfo): Long {
  val declared = info.duration.toLong()
  return if (declared <= 0) DEFAULT_FRAME_DURATION_MS else declared.coerceAtLeast(MIN_FRAME_DURATION_MS)
}
