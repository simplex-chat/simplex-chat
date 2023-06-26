package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.ui.graphics.ImageBitmap
import java.net.URI

actual class VideoPlayer: VideoPlayerInterface {
  actual companion object {
    actual fun getOrCreate(
      uri: URI,
      gallery: Boolean,
      defaultPreview: ImageBitmap,
      defaultDuration: Long,
      soundEnabled: Boolean
    ): VideoPlayer = VideoPlayer()
    actual fun enableSound(enable: Boolean, fileName: String?, gallery: Boolean): Boolean { TODO() }
    actual fun release(uri: URI, gallery: Boolean, remove: Boolean) { TODO() }
    actual fun stopAll() { /*LALAL*/ }
    actual fun releaseAll() { /*LALAL*/ }
  }

  override val soundEnabled: MutableState<Boolean>
    get() = TODO("Not yet implemented")
  override val brokenVideo: MutableState<Boolean>
    get() = TODO("Not yet implemented")
  override val videoPlaying: MutableState<Boolean>
    get() = TODO("Not yet implemented")
  override val progress: MutableState<Long>
    get() = TODO("Not yet implemented")
  override val duration: MutableState<Long>
    get() = TODO("Not yet implemented")
  override val preview: MutableState<ImageBitmap>
    get() = TODO("Not yet implemented")

  override fun stop() {
    TODO("Not yet implemented")
  }

  override fun play(resetOnEnd: Boolean) {
    TODO("Not yet implemented")
  }

  override fun enableSound(enable: Boolean): Boolean {
    TODO("Not yet implemented")
  }

  override fun release(remove: Boolean) {
    TODO("Not yet implemented")
  }
}