package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.ui.graphics.ImageBitmap
import java.net.URI

interface VideoPlayerInterface {
  data class PreviewAndDuration(val preview: ImageBitmap?, val duration: Long?, val timestamp: Long)

  val uri: URI
  val gallery: Boolean
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

expect class VideoPlayer(
  uri: URI,
  gallery: Boolean,
  defaultPreview: ImageBitmap,
  defaultDuration: Long,
  soundEnabled: Boolean
): VideoPlayerInterface

object VideoPlayerHolder {
  val players: MutableMap<Pair<URI, Boolean>, VideoPlayer> = mutableMapOf()
  val previewsAndDurations: MutableMap<URI, VideoPlayerInterface.PreviewAndDuration> = mutableMapOf()

  fun getOrCreate(
    uri: URI,
    gallery: Boolean,
    defaultPreview: ImageBitmap,
    defaultDuration: Long,
    soundEnabled: Boolean
  ): VideoPlayer =
    players.getOrPut(uri to gallery) { VideoPlayer(uri, gallery, defaultPreview, defaultDuration, soundEnabled) }

  fun enableSound(enable: Boolean, fileName: String?, gallery: Boolean): Boolean =
    player(fileName, gallery)?.enableSound(enable) == true

  private fun player(fileName: String?, gallery: Boolean): VideoPlayer? {
    fileName ?: return null
    return players.values.firstOrNull { player -> player.uri.path?.endsWith(fileName) == true && player.gallery == gallery }
  }

  fun release(uri: URI, gallery: Boolean, remove: Boolean) =
    player(uri.path, gallery)?.release(remove).run { }

  fun stopAll() {
    players.values.forEach { it.stop() }
  }

  fun releaseAll() {
    players.values.forEach { it.release(false) }
    players.clear()
    previewsAndDurations.clear()
  }
}
