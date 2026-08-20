package org.jetbrains.compose.videoplayer

import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asComposeImageBitmap
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.ColorAlphaType
import org.jetbrains.skia.ColorType
import org.jetbrains.skia.ImageInfo
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
      this.sourceWidth = track?.width()?.takeIf { it > 0 } ?: sourceWidth
      this.sourceHeight = track?.height()?.takeIf { it > 0 } ?: sourceHeight
      return RV32BufferFormat(this.sourceWidth, this.sourceHeight)
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
        nativeBuffers[0].rewind()
        nativeBuffers[0].get(frameBytes)
        skiaBitmap.installPixels(imageInfo, frameBytes, bufferFormat.width * 4)
        composeBitmap.value = skiaBitmap.asComposeImageBitmap()
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
