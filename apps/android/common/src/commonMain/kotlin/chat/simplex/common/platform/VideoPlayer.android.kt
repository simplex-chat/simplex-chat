package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.ui.graphics.ImageBitmap
import java.net.URI

interface VideoPlayerInterface {
  data class PreviewAndDuration(val preview: ImageBitmap?, val duration: Long?, val timestamp: Long)

  val soundEnabled: MutableState<Boolean>
  val brokenVideo: MutableState<Boolean>
  val videoPlaying: MutableState<Boolean>
  val progress: MutableState<Long>
  val duration: MutableState<Long>
  val preview: MutableState<ImageBitmap>

  fun stop()
  fun play(resetOnEnd: Boolean)
  fun enableSound(enable: Boolean): Boolean
  fun release(remove: Boolean)
}

expect class VideoPlayer: VideoPlayerInterface {
  companion object {
    fun getOrCreate(
      uri: URI,
      gallery: Boolean,
      defaultPreview: ImageBitmap,
      defaultDuration: Long,
      soundEnabled: Boolean
    ): VideoPlayer
    fun enableSound(enable: Boolean, fileName: String?, gallery: Boolean): Boolean
    fun release(uri: URI, gallery: Boolean, remove: Boolean)
    fun stopAll()
    fun releaseAll()
  }
}