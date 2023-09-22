package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import uk.co.caprica.vlcj.player.base.MediaPlayer
import uk.co.caprica.vlcj.player.component.CallbackMediaPlayerComponent
import uk.co.caprica.vlcj.player.component.EmbeddedMediaPlayerComponent
import java.awt.Component
import java.io.File
import java.net.URI
import kotlin.math.max

actual class VideoPlayer actual constructor(
  override val uri: URI,
  override val gallery: Boolean,
  private val defaultPreview: ImageBitmap,
  defaultDuration: Long,
  soundEnabled: Boolean
): VideoPlayerInterface {
  override val soundEnabled: MutableState<Boolean> = mutableStateOf(false)
  override val brokenVideo: MutableState<Boolean> = mutableStateOf(false)
  override val videoPlaying: MutableState<Boolean> = mutableStateOf(false)
  override val progress: MutableState<Long> = mutableStateOf(0L)
  override val duration: MutableState<Long> = mutableStateOf(0L)
  override val preview: MutableState<ImageBitmap> = mutableStateOf(defaultPreview)

  val mediaPlayerComponent = initializeMediaPlayerComponent()
  val player by lazy { mediaPlayerComponent.mediaPlayer() }

  init {
    withBGApi {
      setPreviewAndDuration()
    }
  }

  private val currentVolume: Int by lazy { player.audio().volume() }
  private var isReleased: Boolean = false

  private val listener: MutableState<((position: Long?, state: TrackState) -> Unit)?> = mutableStateOf(null)
  private var progressJob: Job? = null

  enum class TrackState {
    PLAYING, PAUSED, STOPPED
  }

  private fun start(seek: Long? = null, onProgressUpdate: (position: Long?, state: TrackState) -> Unit): Boolean {
    val filepath = getAppFilePath(uri)
    if (filepath == null || !File(filepath).exists()) {
      Log.e(TAG, "No such file: $uri")
      brokenVideo.value = true
      return false
    }

    if (soundEnabled.value)  {
      RecorderInterface.stopRecording?.invoke()
    }
    AudioPlayer.stop()
    VideoPlayerHolder.stopAll()
    val playerFilePath = uri.toString().replaceFirst("file:", "file://")
    if (listener.value == null) {
      runCatching {
        player.media().prepare(playerFilePath)
        if (seek != null) {
          player.seekTo(seek.toInt())
        }
      }.onFailure {
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error), it.message)
        brokenVideo.value = true
        return false
      }
    }
    player.start()
    if (seek != null) player.seekTo(seek.toInt())
    if (!player.isPlaying) {
      // Can happen when video file is broken
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error))
      brokenVideo.value = true
      return false
    }
    listener.value = onProgressUpdate
    // Player can only be accessed in one specific thread
    progressJob = CoroutineScope(Dispatchers.Main).launch {
      onProgressUpdate(player.currentPosition.toLong(), TrackState.PLAYING)
      while (isActive && !isReleased && player.isPlaying) {
        // Even when current position is equal to duration, the player has isPlaying == true for some time,
        // so help to make the playback stopped in UI immediately
        if (player.currentPosition == player.duration) {
          onProgressUpdate(player.currentPosition.toLong(), TrackState.PLAYING)
          break
        }
        delay(50)
        onProgressUpdate(player.currentPosition.toLong(), TrackState.PLAYING)
      }
      if (isActive && !isReleased) {
        onProgressUpdate(player.currentPosition.toLong(), TrackState.PAUSED)
      }
      onProgressUpdate(null, TrackState.PAUSED)
    }

    return true
  }

  override fun stop() {
    if (isReleased || !videoPlaying.value) return
    player.controls().stop()
    stopListener()
  }

  private fun stopListener() {
    val afterCoroutineCancel: CompletionHandler = {
      // Notify prev video listener about stop
      listener.value?.invoke(null, TrackState.STOPPED)
    }
    /** Preventing race by calling a code AFTER coroutine ends, so [TrackState] will be:
     * [TrackState.PLAYING] -> [TrackState.PAUSED] -> [TrackState.STOPPED] (in this order)
     * */
    if (progressJob != null) {
      progressJob?.invokeOnCompletion(afterCoroutineCancel)
    } else {
      afterCoroutineCancel(null)
    }
    progressJob?.cancel()
    progressJob = null
  }

  override fun play(resetOnEnd: Boolean) {
    if (progress.value == duration.value) {
      progress.value = 0
    }
    videoPlaying.value = start(progress.value) { pro, _ ->
      if (pro != null) {
        progress.value = pro
      }
      if ((pro == null || pro == duration.value) && duration.value != 0L) {
        videoPlaying.value = false
        if (pro == duration.value) {
          progress.value = if (resetOnEnd) 0 else duration.value
        }/* else if (state == TrackState.STOPPED) {
          progress.value = 0 //
        }*/
      }
    }
  }

  override fun enableSound(enable: Boolean): Boolean {
    if (isReleased) return false
    if (soundEnabled.value == enable) return false
    soundEnabled.value = enable
    player.audio().setVolume(if (enable) currentVolume else 0)
    return true
  }

  override fun release(remove: Boolean) { withApi {
    if (isReleased) return@withApi
    isReleased = true
    // TODO
    /** [player.release] freezes thread for some reason. It happens periodically. So doing this we don't see the freeze, but it's still there */
    if (player.isPlaying) player.stop()
    CoroutineScope(Dispatchers.IO).launch { player.release() }
    if (remove) {
      VideoPlayerHolder.players.remove(uri to gallery)
    }
  }}

  private val MediaPlayer.currentPosition: Int
    get() = if (isReleased) 0 else max(0, player.status().time().toInt())

  private suspend fun setPreviewAndDuration() {
    // It freezes main thread, doing it in IO thread
    CoroutineScope(Dispatchers.IO).launch {
      val previewAndDuration = VideoPlayerHolder.previewsAndDurations.getOrPut(uri) { getBitmapFromVideo(defaultPreview, uri) }
      withContext(Dispatchers.Main) {
        preview.value = previewAndDuration.preview ?: defaultPreview
        duration.value = (previewAndDuration.duration ?: 0)
      }
    }
  }

  private fun initializeMediaPlayerComponent(): Component {
    return if (desktopPlatform.isMac()) {
      CallbackMediaPlayerComponent()
    } else {
      EmbeddedMediaPlayerComponent()
    }
  }

  private fun Component.mediaPlayer() = when (this) {
    is CallbackMediaPlayerComponent -> mediaPlayer()
    is EmbeddedMediaPlayerComponent -> mediaPlayer()
    else -> error("mediaPlayer() can only be called on vlcj player components")
  }

  companion object {
    suspend fun getBitmapFromVideo(defaultPreview: ImageBitmap?, uri: URI?): VideoPlayerInterface.PreviewAndDuration {
      val player = CallbackMediaPlayerComponent().mediaPlayer()
      if (uri == null || !File(uri.rawPath).exists()) {
        return VideoPlayerInterface.PreviewAndDuration(preview = defaultPreview, timestamp = 0L, duration = 0L)
      }
      player.media().startPaused(uri.toString().replaceFirst("file:", "file://"))
      val start = System.currentTimeMillis()
      while (player.snapshots()?.get() == null && start + 5000 > System.currentTimeMillis()) {
        delay(10)
      }
      val preview = player.snapshots()?.get()?.toComposeImageBitmap()
      val duration = player.duration.toLong()
      CoroutineScope(Dispatchers.IO).launch { player.release() }
      return VideoPlayerInterface.PreviewAndDuration(preview = preview, timestamp = 0L, duration = duration)
    }
  }
}
