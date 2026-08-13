package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import org.jetbrains.compose.videoplayer.SkiaBitmapVideoSurface
import uk.co.caprica.vlcj.media.Media
import uk.co.caprica.vlcj.media.MediaEventAdapter
import uk.co.caprica.vlcj.media.MediaParsedStatus
import uk.co.caprica.vlcj.media.ParseFlag
import uk.co.caprica.vlcj.media.VideoOrientation
import uk.co.caprica.vlcj.player.base.*
import uk.co.caprica.vlcj.player.component.CallbackMediaPlayerComponent
import uk.co.caprica.vlcj.player.component.EmbeddedMediaPlayerComponent
import uk.co.caprica.vlcj.player.component.MediaPlayerSpecs
import java.awt.Component
import java.awt.image.BufferedImage
import java.io.File
import java.net.URI
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
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
  override val duration: MutableState<Long> = mutableStateOf(defaultDuration)
  override val preview: MutableState<ImageBitmap> = mutableStateOf(defaultPreview)

  val mediaPlayerComponent by lazy { getOrCreatePlayer() }
  val player by lazy { mediaPlayerComponent.mediaPlayer() }

  init {
    setPreviewAndDuration()
  }

  private var isReleased: AtomicBoolean = AtomicBoolean(false)

  private val listener: MutableState<((position: Long?, state: TrackState) -> Unit)?> = mutableStateOf(null)
  private var progressJob: Job? = null

  enum class TrackState {
    PLAYING, PAUSED, STOPPED
  }

  /** Should be called in [playerThread]. Otherwise, it creates deadlocks in [player.stop] and [player.release] calls */
  private fun start(seek: Long? = null, onProgressUpdate: (position: Long?, state: TrackState) -> Unit): Boolean {
    val filepath = getAppFilePath(uri)
    if (filepath == null || !File(filepath).exists()) {
      Log.e(TAG, "No such file: $filepath")
      brokenVideo.value = true
      return false
    }

    if (soundEnabled.value)  {
      RecorderInterface.stopRecording?.invoke()
    }
    AudioPlayer.stop()
    VideoPlayerHolder.stopAll()
    if (listener.value == null) {
      runCatching {
        player.media().prepare(uri.toFile().absolutePath)
        if (seek != null) {
          player.seekTo(seek.toInt())
        }
      }.onFailure {
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error), it.stackTraceToString())
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
      while (isActive && !isReleased.get() && player.isPlaying) {
        // Even when current position is equal to duration, the player has isPlaying == true for some time,
        // so help to make the playback stopped in UI immediately
        if (player.currentPosition == player.duration) {
          onProgressUpdate(player.currentPosition.toLong(), TrackState.PLAYING)
          break
        }
        delay(50)
        onProgressUpdate(player.currentPosition.toLong(), TrackState.PLAYING)
      }
      if (isActive && !isReleased.get()) {
        onProgressUpdate(player.currentPosition.toLong(), TrackState.PAUSED)
      }
      onProgressUpdate(null, TrackState.PAUSED)
    }

    return true
  }

  override fun stop() {
    if (isReleased.get() || !videoPlaying.value) return
    playerThread.execute {
      player.stop()
      stopListener()
    }
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
    playerThread.execute {
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
  }

  override fun enableSound(enable: Boolean): Boolean {
    // Impossible to change volume for only one player. It changes for every player
    // https://github.com/caprica/vlcj/issues/985
    return false
    /*if (isReleased.get() || soundEnabled.value == enable) return false
    soundEnabled.value = enable
    playerThread.execute {
      player.audio().isMute = !enable
    }
    return true*/
  }

  override fun release(remove: Boolean) {
    CoroutineScope(playerThread.asCoroutineDispatcher()).launch {
      if (isReleased.get()) return@launch
      isReleased.set(true)
      if (player.isPlaying) {
        player.stop()
      }
      if (usePool) {
        putPlayer(mediaPlayerComponent)
      } else {
        player.release()
      }
      if (remove) {
        VideoPlayerHolder.players.remove(uri to gallery)
      }
    }
  }

  private val MediaPlayer.currentPosition: Int
    get() = if (isReleased.get()) 0 else max(0, status().time().toInt())

  private fun setPreviewAndDuration() {
    // It freezes main thread, doing it in IO thread
    CoroutineScope(Dispatchers.IO).launch {
      val previewAndDuration = VideoPlayerHolder.previewsAndDurations.getOrPut(uri) { getBitmapFromVideo(defaultPreview, uri, withAlertOnException = false) }
      withContext(Dispatchers.Main) {
        preview.value = previewAndDuration.preview ?: defaultPreview
        duration.value = (previewAndDuration.duration ?: 0)
      }
    }
  }

  companion object {
    private val usePool = false

    private fun Component.mediaPlayer() = when (this) {
      is CallbackMediaPlayerComponent -> mediaPlayer()
      is EmbeddedMediaPlayerComponent -> mediaPlayer()
      else -> error("mediaPlayer() can only be called on vlcj player components")
    }

    private fun initializeMediaPlayerComponent(): Component {
      return if (desktopPlatform.isMac()) {
        CallbackMediaPlayerComponent(MediaPlayerSpecs.callbackMediaPlayerSpec().apply { withFactory(vlcFactory) })
      } else {
        EmbeddedMediaPlayerComponent(MediaPlayerSpecs.embeddedMediaPlayerSpec().apply { withFactory(vlcFactory) })
      }
    }

    suspend fun getBitmapFromVideo(defaultPreview: ImageBitmap?, uri: URI?, withAlertOnException: Boolean = true): VideoPlayerInterface.PreviewAndDuration = withContext(previewThread.asCoroutineDispatcher()) {
      val mediaComponent = getOrCreateHelperPlayer()
      val player = mediaComponent.mediaPlayer()
      if (uri == null || !uri.toFile().exists()) {
        if (withAlertOnException) showVideoDecodingException()

        return@withContext VideoPlayerInterface.PreviewAndDuration(preview = defaultPreview, timestamp = 0L, duration = 0L)
      }
      val surface = SkiaBitmapVideoSurface()
      player.videoSurface().set(surface)
      player.media().startPaused(uri.toFile().absolutePath)
      val snap = withTimeoutOrNull(1500L) {
        while (surface.bitmap.value == null) delay(50)
        surface.bitmap.value!!.toAwtImage()
      }
      val orientation = player.media().info().videoTracks().firstOrNull()?.orientation()
      if (orientation == null) {
        player.stop()
        putHelperPlayer(mediaComponent)
        if (withAlertOnException) showVideoDecodingException()

        return@withContext VideoPlayerInterface.PreviewAndDuration(preview = defaultPreview, timestamp = 0L, duration = 0L)
      }
      val preview: ImageBitmap? = when (orientation) {
        VideoOrientation.TOP_LEFT -> snap
        VideoOrientation.TOP_RIGHT -> snap?.flip(false, true)
        VideoOrientation.BOTTOM_LEFT -> snap?.flip(true, false)
        VideoOrientation.BOTTOM_RIGHT -> snap?.rotate(180.0)
        VideoOrientation.LEFT_TOP -> snap  /* Transposed */
        VideoOrientation.LEFT_BOTTOM -> snap?.rotate(-90.0)
        VideoOrientation.RIGHT_TOP -> snap?.rotate(90.0)
        VideoOrientation.RIGHT_BOTTOM -> snap /* Anti-transposed */
        else -> snap
      }?.toComposeImageBitmap()
      val duration = player.duration.toLong()
      player.stop()
      putHelperPlayer(mediaComponent)
      return@withContext VideoPlayerInterface.PreviewAndDuration(preview = preview, timestamp = 0L, duration = duration)
    }

    // Parsing a local container header takes a few dozen ms, this is only a guard against a stuck parse
    private const val PARSE_TIMEOUT_MS = 3000L

    // Reads container metadata to tell whether there is a video track at all, without decoding a frame.
    // libvlc signals the end of parsing with an event, so no polling or frame-decoding budget is needed.
    suspend fun hasVideoTrack(uri: URI): Boolean = withContext(previewThread.asCoroutineDispatcher()) {
      if (!uri.toFile().exists()) return@withContext false
      val media = try {
        vlcPreviewFactory.media().newMedia(uri.toFile().absolutePath)
      } catch (e: Exception) {
        Log.e(TAG, "hasVideoTrack unable to create media: ${e.stackTraceToString()}")
        null
      } ?: return@withContext false
      try {
        val parsed = CompletableDeferred<MediaParsedStatus?>()
        media.events().addMediaEventListener(object: MediaEventAdapter() {
          // vlcj maps an unknown status int to null, and a null here would throw on its event thread
          override fun mediaParsedChanged(parsedMedia: Media?, newStatus: MediaParsedStatus?) {
            parsed.complete(newStatus)
          }
        })
        if (!media.parsing().parse(PARSE_TIMEOUT_MS.toInt(), ParseFlag.PARSE_LOCAL)) {
          return@withContext false
        }
        if (withTimeoutOrNull(PARSE_TIMEOUT_MS) { parsed.await() } != MediaParsedStatus.DONE) {
          media.parsing().stop()
          return@withContext false
        }
        media.info().videoTracks().isNotEmpty()
      } catch (e: Exception) {
        Log.e(TAG, "hasVideoTrack error: ${e.stackTraceToString()}")
        false
      } finally {
        media.release()
      }
    }

    val playerThread = Executors.newSingleThreadExecutor()
    private val previewThread = Executors.newSingleThreadExecutor()
    private val playersPool: ArrayList<Component> = ArrayList()
    private val helperPlayersPool: ArrayList<CallbackMediaPlayerComponent> = ArrayList()

    private fun getOrCreatePlayer(): Component = playersPool.removeFirstOrNull() ?: createNew()

    private fun createNew(): Component =
      initializeMediaPlayerComponent().apply {
        mediaPlayer().events().addMediaPlayerEventListener(object: MediaPlayerEventAdapter() {
          override fun mediaPlayerReady(mediaPlayer: MediaPlayer?) {
            playerThread.execute {
              // Do not call setVolume here: on Windows VLCJ routes it through WASAPI ISimpleAudioVolume,
              // which resets SimpleX Chat's per-app volume in the Windows Volume Mixer on every playback
              // (VLCJ issue #985). A fresh VLCJ MediaPlayer already defaults to volume 100, so this was redundant.
              mediaPlayer?.audio()?.isMute = false
            }
          }

          override fun stopped(mediaPlayer: MediaPlayer?) {
            //playerThread.execute { mediaPlayer().videoSurface().set(null) }
          }
        })
      }

    private fun putPlayer(player: Component) = playersPool.add(player)

    private fun getOrCreateHelperPlayer(): CallbackMediaPlayerComponent = helperPlayersPool.removeFirstOrNull() ?: CallbackMediaPlayerComponent(MediaPlayerSpecs.callbackMediaPlayerSpec().apply { withFactory(vlcPreviewFactory) })
    private fun putHelperPlayer(player: CallbackMediaPlayerComponent) = helperPlayersPool.add(player)
  }
}
