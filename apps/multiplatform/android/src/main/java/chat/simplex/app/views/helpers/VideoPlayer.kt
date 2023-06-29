package chat.simplex.app.views.helpers

import android.content.Context
import android.graphics.Bitmap
import android.media.session.PlaybackState
import android.net.Uri
import android.util.Log
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import chat.simplex.app.*
import chat.simplex.app.R
import com.google.android.exoplayer2.*
import com.google.android.exoplayer2.C.*
import com.google.android.exoplayer2.audio.AudioAttributes
import com.google.android.exoplayer2.source.ProgressiveMediaSource
import com.google.android.exoplayer2.upstream.DefaultDataSource
import com.google.android.exoplayer2.upstream.DefaultHttpDataSource
import kotlinx.coroutines.*
import java.io.File

class VideoPlayer private constructor(
  private val uri: Uri,
  private val gallery: Boolean,
  private val defaultPreview: Bitmap,
  defaultDuration: Long,
  soundEnabled: Boolean,
  context: Context
) {
  companion object {
    private val players: MutableMap<Pair<Uri, Boolean>, VideoPlayer> = mutableMapOf()
    private val previewsAndDurations: MutableMap<Uri, PreviewAndDuration> = mutableMapOf()

    fun getOrCreate(
      uri: Uri,
      gallery: Boolean,
      defaultPreview: Bitmap,
      defaultDuration: Long,
      soundEnabled: Boolean,
      context: Context
    ): VideoPlayer =
      players.getOrPut(uri to gallery) { VideoPlayer(uri, gallery, defaultPreview, defaultDuration, soundEnabled, context) }

    fun enableSound(enable: Boolean, fileName: String?, gallery: Boolean): Boolean =
      player(fileName, gallery)?.enableSound(enable) == true

    private fun player(fileName: String?, gallery: Boolean): VideoPlayer? {
      fileName ?: return null
      return players.values.firstOrNull { player -> player.uri.path?.endsWith(fileName) == true && player.gallery == gallery }
    }

    fun release(uri: Uri, gallery: Boolean, remove: Boolean) =
      player(uri.path, gallery)?.release(remove)

    fun stopAll() {
      players.values.forEach { it.stop() }
    }

    fun releaseAll() {
      players.values.forEach { it.release(false) }
      players.clear()
      previewsAndDurations.clear()
    }
  }

  data class PreviewAndDuration(val preview: Bitmap?, val duration: Long?, val timestamp: Long)

  private val currentVolume: Float
  val soundEnabled: MutableState<Boolean> = mutableStateOf(soundEnabled)
  val brokenVideo: MutableState<Boolean> = mutableStateOf(false)
  val videoPlaying: MutableState<Boolean> = mutableStateOf(false)
  val progress: MutableState<Long> = mutableStateOf(0L)
  val duration: MutableState<Long> = mutableStateOf(defaultDuration)
  val preview: MutableState<Bitmap> = mutableStateOf(defaultPreview)

  init {
    setPreviewAndDuration()
  }

  val player = ExoPlayer.Builder(context,
    DefaultRenderersFactory(context))
    /*.setLoadControl(DefaultLoadControl.Builder()
      .setPrioritizeTimeOverSizeThresholds(false) // Could probably save some megabytes in memory in case it will be needed
      .createDefaultLoadControl())*/
    .setSeekBackIncrementMs(10_000)
    .setSeekForwardIncrementMs(10_000)
    .build()
    .apply {
    // Repeat the same track endlessly
    repeatMode = Player.REPEAT_MODE_ONE
    currentVolume = volume
    if (!soundEnabled) {
      volume = 0f
    }
    setAudioAttributes(
      AudioAttributes.Builder()
        .setContentType(CONTENT_TYPE_MUSIC)
        .setUsage(USAGE_MEDIA)
        .build(),
      true // disallow to play multiple instances simultaneously
    )
  }

  private val listener: MutableState<((position: Long?, state: TrackState) -> Unit)?> = mutableStateOf(null)
  private var progressJob: Job? = null

  enum class TrackState {
    PLAYING, PAUSED, STOPPED
  }

  private fun start(seek: Long? = null, onProgressUpdate: (position: Long?, state: TrackState) -> Unit): Boolean {
    val filepath = getAppFilePath(SimplexApp.context, uri)
    if (filepath == null || !File(filepath).exists()) {
      Log.e(TAG, "No such file: $uri")
      brokenVideo.value = true
      return false
    }

    if (soundEnabled.value)  {
      RecorderNative.stopRecording?.invoke()
    }
    AudioPlayer.stop()
    stopAll()
    if (listener.value == null) {
      runCatching {
        val dataSourceFactory = DefaultDataSource.Factory(SimplexApp.context, DefaultHttpDataSource.Factory())
        val source = ProgressiveMediaSource.Factory(dataSourceFactory).createMediaSource(MediaItem.fromUri(uri))
        player.setMediaSource(source, seek ?: 0L)
      }.onFailure {
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), it.message)
        brokenVideo.value = true
        return false
      }
    }
    if (player.playbackState == PlaybackState.STATE_NONE || player.playbackState == PlaybackState.STATE_STOPPED) {
      runCatching { player.prepare() }.onFailure {
        // Can happen when video file is broken
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), it.message)
        brokenVideo.value = true
        return false
      }
    }
    if (seek != null) player.seekTo(seek)
    player.play()
    listener.value = onProgressUpdate
    // Player can only be accessed in one specific thread
    progressJob = CoroutineScope(Dispatchers.Main).launch {
      onProgressUpdate(player.currentPosition, TrackState.PLAYING)
      while (isActive && player.playbackState != Player.STATE_IDLE && player.playWhenReady) {
        // Even when current position is equal to duration, the player has isPlaying == true for some time,
        // so help to make the playback stopped in UI immediately
        if (player.currentPosition == player.duration) {
          onProgressUpdate(player.currentPosition, TrackState.PLAYING)
          break
        }
        delay(50)
        onProgressUpdate(player.currentPosition, TrackState.PLAYING)
      }
      /*
      * Since coroutine is still NOT canceled, means player ended (no stop/no pause). But in some cases
      * the player can show position != duration even if they actually equal.
      * Let's say to a listener that the position == duration in case of coroutine finished without cancel
      * */
      if (isActive) {
        onProgressUpdate(player.duration, TrackState.PAUSED)
      }
      onProgressUpdate(null, TrackState.PAUSED)
    }
    player.addListener(object: Player.Listener{
      override fun onIsPlayingChanged(isPlaying: Boolean) {
        super.onIsPlayingChanged(isPlaying)
        // Produce non-ideal transition from stopped to playing state while showing preview image in ChatView
//        videoPlaying.value = isPlaying
      }
    })

    return true
  }

  fun stop() {
    player.stop()
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

  fun play(resetOnEnd: Boolean) {
    if (progress.value == duration.value) {
      progress.value = 0
    }
    videoPlaying.value = start(progress.value) { pro, _ ->
      if (pro != null) {
        progress.value = pro
      }
      if (pro == null || pro == duration.value) {
        videoPlaying.value = false
        if (pro == duration.value) {
          progress.value = if (resetOnEnd) 0 else duration.value
        }/* else if (state == TrackState.STOPPED) {
          progress.value = 0 //
        }*/
      }
    }
  }

  fun enableSound(enable: Boolean): Boolean {
    if (soundEnabled.value == enable) return false
    soundEnabled.value = enable
    player.volume = if (enable) currentVolume else 0f
    return true
  }

  fun release(remove: Boolean) {
    player.release()
    if (remove) {
      players.remove(uri to gallery)
    }
  }

  private fun setPreviewAndDuration() {
    // It freezes main thread, doing it in IO thread
    CoroutineScope(Dispatchers.IO).launch {
      val previewAndDuration = previewsAndDurations.getOrPut(uri) { getBitmapFromVideo(uri) }
      withContext(Dispatchers.Main) {
        preview.value = previewAndDuration.preview ?: defaultPreview
        duration.value = (previewAndDuration.duration ?: 0)
      }
    }
  }
}
