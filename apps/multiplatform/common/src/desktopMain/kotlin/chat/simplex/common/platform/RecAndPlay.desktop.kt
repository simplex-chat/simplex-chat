package chat.simplex.common.platform

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import uk.co.caprica.vlcj.player.base.MediaPlayer
import uk.co.caprica.vlcj.player.base.State
import uk.co.caprica.vlcj.player.component.AudioPlayerComponent
import java.io.File
import java.util.*
import kotlin.math.max

actual class RecorderNative: RecorderInterface {
  override fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String {
    /*LALAL*/
    return ""
  }

  override fun stop(): Int {
    /*LALAL*/
    return 0
  }
}

actual object AudioPlayer: AudioPlayerInterface {
  val player by lazy { AudioPlayerComponent().mediaPlayer() }

  // Filepath: String, onProgressUpdate
  private val currentlyPlaying: MutableState<Pair<CryptoFile, (position: Int?, state: TrackState) -> Unit>?> = mutableStateOf(null)
  private var progressJob: Job? = null

  enum class TrackState {
    PLAYING, PAUSED, REPLACED
  }

  // Returns real duration of the track
  private fun start(fileSource: CryptoFile, seek: Int? = null, onProgressUpdate: (position: Int?, state: TrackState) -> Unit): Int? {
    val absoluteFilePath = if (fileSource.isAbsolutePath) fileSource.filePath else getAppFilePath(fileSource.filePath)
    if (!File(absoluteFilePath).exists()) {
      Log.e(TAG, "No such file: ${fileSource.filePath}")
      return null
    }

    VideoPlayerHolder.stopAll()
    RecorderInterface.stopRecording?.invoke()
    val current = currentlyPlaying.value
    if (current == null || current.first != fileSource || !player.status().isPlayable) {
      stopListener()
      player.stop()
      runCatching {
        if (fileSource.cryptoArgs != null) {
          val tmpFile = fileSource.createTmpFileIfNeeded()
          decryptCryptoFile(absoluteFilePath, fileSource.cryptoArgs, tmpFile.absolutePath)
          player.media().prepare(tmpFile.absolutePath)
        } else {
          player.media().prepare(absoluteFilePath)
        }
      }.onFailure {
        Log.e(TAG, it.stackTraceToString())
        fileSource.deleteTmpFile()
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error), it.message)
        return null
      }
    }
    if (seek != null) player.seekTo(seek)
    player.start()
    currentlyPlaying.value = fileSource to onProgressUpdate
    progressJob = CoroutineScope(Dispatchers.Default).launch {
      onProgressUpdate(player.currentPosition, TrackState.PLAYING)
      while(isActive && (player.isPlaying || player.status().state() == State.OPENING)) {
        // Even when current position is equal to duration, the player has isPlaying == true for some time,
        // so help to make the playback stopped in UI immediately
        if (player.currentPosition == player.duration) {
          onProgressUpdate(player.currentPosition, TrackState.PLAYING)
          break
        }
        delay(50)
        onProgressUpdate(player.currentPosition, TrackState.PLAYING)
      }
      onProgressUpdate(null, TrackState.PAUSED)
      currentlyPlaying.value?.first?.deleteTmpFile()
    }
    return player.duration
  }

  private fun pause(): Int {
    progressJob?.cancel()
    progressJob = null
    val position = player.currentPosition
    player.pause()
    return position
  }

  override fun stop() {
    if (currentlyPlaying.value == null) return
    player.stop()
    stopListener()
  }

  override fun stop(item: ChatItem) = stop(item.file?.fileName)

  // FileName or filePath are ok
  override fun stop(fileName: String?) {
    if (fileName != null && currentlyPlaying.value?.first?.filePath?.endsWith(fileName) == true) {
      stop()
    }
  }

  private fun stopListener() {
    val afterCoroutineCancel: CompletionHandler = {
      // Notify prev audio listener about stop
      currentlyPlaying.value?.second?.invoke(null, TrackState.REPLACED)
      currentlyPlaying.value?.first?.deleteTmpFile()
      currentlyPlaying.value = null
    }
    /** Preventing race by calling a code AFTER coroutine ends, so [TrackState] will be:
     * [TrackState.PLAYING] -> [TrackState.PAUSED] -> [TrackState.REPLACED] (in this order)
     * */
    if (progressJob != null) {
      progressJob?.invokeOnCompletion(afterCoroutineCancel)
    } else {
      afterCoroutineCancel(null)
    }
    progressJob?.cancel()
    progressJob = null
  }

  override fun play(
    fileSource: CryptoFile,
    audioPlaying: MutableState<Boolean>,
    progress: MutableState<Int>,
    duration: MutableState<Int>,
    resetOnEnd: Boolean,
  ) {
    if (progress.value == duration.value) {
      progress.value = 0
    }
    val realDuration = start(fileSource, progress.value) { pro, state ->
      if (pro != null) {
        progress.value = pro
      }
      if (pro == null || pro == duration.value) {
        audioPlaying.value = false
        if (pro == duration.value) {
          progress.value = if (resetOnEnd) 0 else duration.value
        } else if (state == TrackState.REPLACED) {
          progress.value = 0
        }
      }
    }
    audioPlaying.value = realDuration != null
    // Update to real duration instead of what was received in ChatInfo
    realDuration?.let { duration.value = it }
  }

  override fun pause(audioPlaying: MutableState<Boolean>, pro: MutableState<Int>) {
    pro.value = pause()
    audioPlaying.value = false
  }

  override fun seekTo(ms: Int, pro: MutableState<Int>, filePath: String?) {
    pro.value = ms
    if (currentlyPlaying.value?.first?.filePath == filePath) {
      player.seekTo(ms)
    }
  }

  override fun duration(unencryptedFilePath: String): Int? {
    var res: Int? = null
    try {
      val helperPlayer = AudioPlayerComponent().mediaPlayer()
      helperPlayer.media().startPaused(unencryptedFilePath)
      res = helperPlayer.duration
      helperPlayer.stop()
      helperPlayer.release()
    } catch (e: Exception) {
      Log.e(TAG, e.stackTraceToString())
    }
    return res
  }
}

val MediaPlayer.isPlaying: Boolean
  get() = status().isPlaying

fun MediaPlayer.seekTo(time: Int) {
  controls().setTime(time.toLong())
}

fun MediaPlayer.start() {
  controls().start()
}

fun MediaPlayer.pause() {
  controls().pause()
}

fun MediaPlayer.stop() {
  controls().stop()
}

private val MediaPlayer.currentPosition: Int
  get() = max(0, status().time().toInt())

val MediaPlayer.duration: Int
  get() = media().info().duration().toInt()

actual object SoundPlayer: SoundPlayerInterface {
  var playing = false

  override fun start(scope: CoroutineScope, sound: Boolean) {
    val tmpFile = File(tmpDir, UUID.randomUUID().toString())
    tmpFile.deleteOnExit()
    SoundPlayer::class.java.getResource("/media/ring_once.mp3").openStream()!!.use { it.copyTo(tmpFile.outputStream()) }
    playing = true
    scope.launch {
      while (playing && sound) {
        AudioPlayer.play(CryptoFile.plain(tmpFile.absolutePath), mutableStateOf(true), mutableStateOf(0), mutableStateOf(0), true)
        delay(3500)
      }
    }
  }

  override fun vibrate() {}

  override fun stop() {
    playing = false
    AudioPlayer.stop()
  }
}
