package chat.simplex.common.platform

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import uk.co.caprica.vlcj.factory.MediaPlayerFactory
import uk.co.caprica.vlcj.player.base.MediaPlayer
import uk.co.caprica.vlcj.player.base.State
import uk.co.caprica.vlcj.player.component.AudioPlayerComponent
import java.io.File
import java.util.*
import kotlin.math.max

actual class RecorderNative: RecorderInterface {
  private var player: MediaPlayer? = null
  private var progressJob: Job? = null
  private var filePath: String? = null
  private var recStartedAt: Long? = null

  companion object {
    private val factory by lazy { MediaPlayerFactory("--no-video", "--sout-avcodec-strict=-2") }
  }

  override fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String {
    VideoPlayerHolder.stopAll()
    AudioPlayer.stop()
    val fileToSave = File.createTempFile(generateNewFileName("voice", "${RecorderInterface.extension}_", tmpDir), ".tmp", tmpDir)
    fileToSave.deleteOnExit()
    val path = fileToSave.absolutePath
    filePath = path
    val mrl = when {
      desktopPlatform.isMac() -> "qtsound://"
      desktopPlatform.isLinux() -> "pulse://"
      desktopPlatform.isWindows() -> "dshow://"
      else -> {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.voice_recording_not_supported))
        return ""
      }
    }
    val sout = ":sout=#transcode{vcodec=none,acodec=mp4a,ab=32,channels=1,samplerate=16000}:std{access=file,mux=mp4,dst=$path}"
    val options = mutableListOf(sout)
    if (desktopPlatform.isWindows()) {
      options.add(":dshow-vdev=none")
      options.add(":dshow-adev=")
    }
    RecorderInterface.stopRecording = { stop() }
    progressJob = CoroutineScope(Dispatchers.Default).launch {
      // Factory init may take a few seconds on first use — progress shows 0 until recording starts
      val p = factory.mediaPlayers().newMediaPlayer()
      player = p
      p.media().play(mrl, *options.toTypedArray())
      recStartedAt = System.currentTimeMillis()
      while (isActive) {
        val ms = progress()
        onProgressUpdate(ms, false)
        if (ms != null && ms >= MAX_VOICE_MILLIS_FOR_SENDING) {
          stop()
          break
        }
        delay(50)
      }
    }.apply {
      invokeOnCompletion { onProgressUpdate(realDuration(path), true) }
    }
    return path
  }

  override fun stop(): Int {
    val path = filePath ?: return 0
    RecorderInterface.stopRecording = null
    runCatching { player?.controls()?.stop() }
    runCatching { player?.release() }
    runBlocking { progressJob?.cancelAndJoin() }
    progressJob = null
    filePath = null
    player = null
    return (realDuration(path) ?: 0).also { recStartedAt = null }
  }

  private fun progress(): Int? = recStartedAt?.let { (System.currentTimeMillis() - it).toInt() }

  private fun realDuration(path: String): Int? = AudioPlayer.duration(path) ?: progress()
}

actual object AudioPlayer: AudioPlayerInterface {
  private val player by lazy { AudioPlayerComponent().mediaPlayer() }

  override val currentlyPlaying: MutableState<CurrentlyPlayingState?> = mutableStateOf(null)
  private var progressJob: Job? = null

  // Returns real duration of the track
  private fun start(fileSource: CryptoFile, smallView: Boolean, seek: Int? = null, onProgressUpdate: (position: Int?, state: TrackState) -> Unit): Int? {
    val absoluteFilePath = if (fileSource.isAbsolutePath) fileSource.filePath else getAppFilePath(fileSource.filePath)
    if (!File(absoluteFilePath).exists()) {
      Log.e(TAG, "No such file: ${fileSource.filePath}")
      return null
    }

    VideoPlayerHolder.stopAll()
    RecorderInterface.stopRecording?.invoke()
    val current = currentlyPlaying.value
    if (current == null || current.fileSource != fileSource || !player.status().isPlayable || smallView != current.smallView) {
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
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error), it.stackTraceToString())
        return null
      }
    }
    if (seek != null) player.seekTo(seek)
    player.start()
    currentlyPlaying.value = CurrentlyPlayingState(fileSource, onProgressUpdate, smallView)
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
      currentlyPlaying.value?.fileSource?.deleteTmpFile()
      // Since coroutine is still NOT canceled, means player ended (no stop/no pause).
      if (smallView && isActive) {
        stopListener()
      }
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
    if (fileName != null && currentlyPlaying.value?.fileSource?.filePath?.endsWith(fileName) == true) {
      stop()
    }
  }

  private fun stopListener() {
    val afterCoroutineCancel: CompletionHandler = {
      // Notify prev audio listener about stop
      currentlyPlaying.value?.onProgressUpdate?.invoke(null, TrackState.REPLACED)
      currentlyPlaying.value?.fileSource?.deleteTmpFile()
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
    smallView: Boolean,
  ) {
    if (progress.value == duration.value) {
      progress.value = 0
    }
    val realDuration = start(fileSource, smallView = smallView, progress.value) { pro, state ->
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
    if (currentlyPlaying.value?.fileSource?.filePath == filePath) {
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
    SoundPlayer::class.java.getResource("/media/ring_once.mp3")!!.openStream()!!.use { it.copyTo(tmpFile.outputStream()) }
    playing = true
    scope.launch {
      while (playing && sound) {
        AudioPlayer.play(CryptoFile.plain(tmpFile.absolutePath), mutableStateOf(true), mutableStateOf(0), mutableStateOf(0), resetOnEnd = true, smallView = false)
        delay(3500)
      }
    }
  }

  override fun stop() {
    playing = false
    AudioPlayer.stop()
  }
}

actual object CallSoundsPlayer: CallSoundsPlayerInterface {
  private var playingJob: Job? = null

  private fun start(soundPath: String, delay: Long, scope: CoroutineScope) {
    playingJob?.cancel()
    val tmpFile = File(tmpDir, UUID.randomUUID().toString())
    tmpFile.deleteOnExit()
    SoundPlayer::class.java.getResource(soundPath)!!.openStream()!!.use { it.copyTo(tmpFile.outputStream()) }
    playingJob = scope.launch {
      while (isActive) {
        AudioPlayer.play(CryptoFile.plain(tmpFile.absolutePath), mutableStateOf(true), mutableStateOf(0), mutableStateOf(0), resetOnEnd = true, smallView = false)
        delay(delay)
      }
    }
  }

  override fun startConnectingCallSound(scope: CoroutineScope) {
    // Taken from https://github.com/TelegramOrg/Telegram-Android
    // https://github.com/TelegramOrg/Telegram-Android/blob/master/LICENSE
    start("/media/connecting_call.mp3", 3000, scope)
  }

  override fun startInCallSound(scope: CoroutineScope) {
    start("/media/in_call.mp3", 5000, scope)
  }

  override fun vibrate(times: Int) {}

  override fun stop() {
    playingJob?.cancel()
    AudioPlayer.stop()
  }
}
