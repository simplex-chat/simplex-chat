package org.jetbrains.compose.videoplayer

import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asComposeImageBitmap
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.ColorAlphaType
import org.jetbrains.skia.ColorType
import org.jetbrains.skia.ImageInfo
import uk.co.caprica.vlcj.media.VideoOrientation
import uk.co.caprica.vlcj.player.base.MediaPlayer
import uk.co.caprica.vlcj.player.embedded.videosurface.CallbackVideoSurface
import uk.co.caprica.vlcj.player.embedded.videosurface.VideoSurface
import uk.co.caprica.vlcj.player.embedded.videosurface.VideoSurfaceAdapters
import uk.co.caprica.vlcj.player.embedded.videosurface.callback.BufferFormat
import uk.co.caprica.vlcj.player.embedded.videosurface.callback.BufferFormatCallback
import uk.co.caprica.vlcj.player.embedded.videosurface.callback.RenderCallback
import uk.co.caprica.vlcj.player.embedded.videosurface.callback.format.RV32BufferFormat
import java.nio.ByteBuffer
import javax.swing.SwingUtilities

// https://github.com/JetBrains/compose-multiplatform/pull/3336/files
internal class SkiaBitmapVideoSurface : VideoSurface(VideoSurfaceAdapters.getVideoSurfaceAdapter()) {

  private companion object {
    // A received file declares its own size, and vlc allocates the buffer we ask for here (and we copy
    // it into a java array of the same size), so an unbounded request is an out-of-memory from a message.
    // Above the budget the picture is scaled down keeping its aspect - 4096x4096 of RV32 is 64 MB
    const val MAX_BUFFER_PIXELS = 4096L * 4096L
    val transposedOrientations = setOf(
      VideoOrientation.LEFT_TOP,
      VideoOrientation.LEFT_BOTTOM,
      VideoOrientation.RIGHT_TOP,
      VideoOrientation.RIGHT_BOTTOM,
    )

    // Keeps the aspect, never returns a side below 1, and keeps width * height * 4 inside an Int
    fun boundedSize(width: Int, height: Int): Pair<Int, Int> {
      val w = width.coerceAtLeast(1)
      val h = height.coerceAtLeast(1)
      val pixels = w.toLong() * h.toLong()
      if (pixels <= MAX_BUFFER_PIXELS) return w to h
      val scale = kotlin.math.sqrt(MAX_BUFFER_PIXELS.toDouble() / pixels.toDouble())
      return (w * scale).toInt().coerceAtLeast(1) to (h * scale).toInt().coerceAtLeast(1)
    }
  }

  private val videoSurface = SkiaBitmapVideoSurface()
  @Volatile private var mediaPlayer: MediaPlayer? = null
  private lateinit var imageInfo: ImageInfo
  private lateinit var frameBytes: ByteArray
  private val skiaBitmap: Bitmap = Bitmap()
  private val composeBitmap = mutableStateOf<ImageBitmap?>(null)

  val bitmap: State<ImageBitmap?> = composeBitmap

  override fun attach(mediaPlayer: MediaPlayer) {
    this.mediaPlayer = mediaPlayer
    videoSurface.attach(mediaPlayer)
  }

  private inner class SkiaBitmapBufferFormatCallback : BufferFormatCallback {
    private var sourceWidth: Int = 0
    private var sourceHeight: Int = 0

    override fun getBufferFormat(sourceWidth: Int, sourceHeight: Int): BufferFormat {
      // libvlc passes the size the decoder padded the picture to, not the size of the picture (dav1d
      // pads to a multiple of 128, so 1920x1080 arrives as 1920x1152), and vlc stretches the picture to
      // fill whatever size is returned. Ask for the size of the track being played instead. The format
      // is negotiated more than once, and vlc has not selected the track yet on the first calls
      val player = mediaPlayer
      val tracks = player?.media()?.info()?.videoTracks()
      val playingTrack = player?.video()?.track()
      val track = tracks?.firstOrNull { it.id() == playingTrack } ?: tracks?.singleOrNull()
      // Both track sides or neither: one side from the track and the other from the padded size libvlc
      // passed never described the same picture, and transposing such a pair compounds the mismatch
      val trackW = track?.width() ?: 0
      val trackH = track?.height() ?: 0
      val useTrack = trackW > 0 && trackH > 0
      val width = if (useTrack) trackW else sourceWidth
      val height = if (useTrack) trackH else sourceHeight
      // The track carries the size before rotation, but vlc rotates the picture before it reaches
      // this buffer, so for the transposed orientations the picture arrives with the sides swapped
      val transposed = track?.orientation() in transposedOrientations
      val orientedWidth = if (transposed) height else width
      val orientedHeight = if (transposed) width else height
      val (w, h) = boundedSize(orientedWidth, orientedHeight)
      this.sourceWidth = w
      this.sourceHeight = h
      return RV32BufferFormat(w, h)
    }

    override fun allocatedBuffers(buffers: Array<ByteBuffer>) {
      frameBytes = buffers[0].run { ByteArray(remaining()).also(::get) }
      imageInfo = ImageInfo(
        sourceWidth,
        sourceHeight,
        ColorType.BGRA_8888,
        ColorAlphaType.PREMUL,
      )
    }
  }

  private inner class SkiaBitmapRenderCallback : RenderCallback {
    override fun display(
      mediaPlayer: MediaPlayer,
      nativeBuffers: Array<ByteBuffer>,
      bufferFormat: BufferFormat,
    ) {
      SwingUtilities.invokeLater {
        // imageInfo comes from the format that was last allocated and the buffer from the frame being
        // displayed; the format is renegotiated while playing, so a frame can arrive that does not fill
        // the bitmap skia is told to read. Reading it would run off the end of the array inside skia
        val rowBytes = imageInfo.width.toLong() * 4
        val needed = rowBytes * imageInfo.height
        val buffer = nativeBuffers[0]
        // rewind first: the same buffer is reused for every frame, so its position is at the end of
        // the previous read and remaining() would be 0
        buffer.rewind()
        if (needed <= frameBytes.size && buffer.remaining().toLong() >= needed) {
          buffer.get(frameBytes, 0, needed.toInt())
          skiaBitmap.installPixels(imageInfo, frameBytes, rowBytes.toInt())
          composeBitmap.value = skiaBitmap.asComposeImageBitmap()
        }
      }
    }
  }

  private inner class SkiaBitmapVideoSurface : CallbackVideoSurface(
    SkiaBitmapBufferFormatCallback(),
    SkiaBitmapRenderCallback(),
    true,
    videoSurfaceAdapter,
  )
}
