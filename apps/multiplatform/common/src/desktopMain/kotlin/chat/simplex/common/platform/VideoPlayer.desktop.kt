package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.ImageBitmap
import chat.simplex.common.views.usersettings.showInDevelopingAlert
import java.net.URI

actual class VideoPlayer: VideoPlayerInterface {
  actual companion object {
    actual fun getOrCreate(
      uri: URI,
      gallery: Boolean,
      defaultPreview: ImageBitmap,
      defaultDuration: Long,
      soundEnabled: Boolean
    ): VideoPlayer = VideoPlayer().also {
      it.preview.value = defaultPreview
      it.duration.value = defaultDuration
      it.soundEnabled.value = soundEnabled
    }
    actual fun enableSound(enable: Boolean, fileName: String?, gallery: Boolean): Boolean { /*TODO*/ return false }
    actual fun release(uri: URI, gallery: Boolean, remove: Boolean) { /*TODO*/ }
    actual fun stopAll() { /*LALAL*/ }
    actual fun releaseAll() { /*LALAL*/ }
  }

  override val soundEnabled: MutableState<Boolean> = mutableStateOf(false)
  override val brokenVideo: MutableState<Boolean> = mutableStateOf(false)
  override val videoPlaying: MutableState<Boolean> = mutableStateOf(false)
  override val progress: MutableState<Long> = mutableStateOf(0L)
  override val duration: MutableState<Long> = mutableStateOf(0L)
  override val preview: MutableState<ImageBitmap> = mutableStateOf(ImageBitmap(0, 0))

  override fun stop() {
    /*TODO*/
  }

  override fun play(resetOnEnd: Boolean) {
    if (appPlatform.isDesktop) {
      showInDevelopingAlert()
    }
  }

  override fun enableSound(enable: Boolean): Boolean {
    /*TODO*/
    return false
  }

  override fun release(remove: Boolean) {
    /*TODO*/
  }
}
