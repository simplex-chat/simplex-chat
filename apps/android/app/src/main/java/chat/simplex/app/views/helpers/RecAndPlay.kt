package chat.simplex.app.views.helpers

import android.app.Application
import android.content.Context
import android.media.*
import android.media.AudioManager.AudioPlaybackCallback
import android.media.MediaRecorder.MEDIA_RECORDER_INFO_MAX_DURATION_REACHED
import android.media.MediaRecorder.MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED
import android.os.Build
import android.util.Log
import androidx.compose.runtime.*
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.views.helpers.AudioPlayer.duration
import kotlinx.coroutines.*
import java.io.*

interface Recorder {
  fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String
  fun stop(): Int
}

class RecorderNative(): Recorder {
  companion object {
    // Allows to stop the recorder from outside without having the recorder in a variable
    var stopRecording: (() -> Unit)? = null
    const val extension = "m4a"
  }
  private var recorder: MediaRecorder? = null
  private var progressJob: Job? = null
  private var filePath: String? = null
  private var recStartedAt: Long? = null
  private fun initRecorder() =
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      MediaRecorder(SimplexApp.context)
    } else {
      MediaRecorder()
    }

  override fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String {
    VideoPlayer.stopAll()
    AudioPlayer.stop()
    val rec: MediaRecorder
    recorder = initRecorder().also { rec = it }
    rec.setAudioSource(MediaRecorder.AudioSource.MIC)
    rec.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4)
    rec.setAudioEncoder(MediaRecorder.AudioEncoder.AAC)
    rec.setAudioChannels(1)
    rec.setAudioSamplingRate(16000)
    rec.setAudioEncodingBitRate(32000)
    rec.setMaxDuration(MAX_VOICE_MILLIS_FOR_SENDING)
    val tmpDir = SimplexApp.context.getDir("temp", Application.MODE_PRIVATE)
    val fileToSave = File.createTempFile(generateNewFileName(SimplexApp.context, "voice", "${extension}_"), ".tmp", tmpDir)
    fileToSave.deleteOnExit()
    val path = fileToSave.absolutePath
    filePath = path
    rec.setOutputFile(path)
    rec.prepare()
    rec.start()
    recStartedAt = System.currentTimeMillis()
    progressJob = CoroutineScope(Dispatchers.Default).launch {
      while(isActive) {
        onProgressUpdate(progress(), false)
        delay(50)
      }
    }.apply {
      invokeOnCompletion {
        onProgressUpdate(realDuration(path), true)
      }
    }
    rec.setOnInfoListener { _, what, _ ->
      if (what == MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED || what == MEDIA_RECORDER_INFO_MAX_DURATION_REACHED) {
        stop()
      }
    }
    stopRecording = { stop() }
    return path
  }

  override fun stop(): Int {
    val path = filePath ?: return 0
    stopRecording = null
    runCatching {
      recorder?.stop()
    }
    runCatching {
      recorder?.reset()
    }
    runCatching {
      recorder?.release()
    }
    // Await coroutine finishes in order to send real duration to it's listener
    runBlocking {
      progressJob?.cancelAndJoin()
    }
    progressJob = null
    filePath = null
    recorder = null
    return (realDuration(path) ?: 0).also { recStartedAt = null }
  }

  private fun progress(): Int? = recStartedAt?.let { (System.currentTimeMillis() - it).toInt() }

  /**
  * Return real duration from [AudioPlayer] if it's possible (should always be possible).
  * As a fallback, return internally counted duration
  * */
  private fun realDuration(path: String): Int? = duration(path) ?: progress()
}

object AudioPlayer {
  private val player = MediaPlayer().apply {
    setAudioAttributes(
      AudioAttributes.Builder()
        .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
        .setUsage(AudioAttributes.USAGE_MEDIA)
        .build()
    )
    (SimplexApp.context.getSystemService(Context.AUDIO_SERVICE) as AudioManager)
      .registerAudioPlaybackCallback(object: AudioPlaybackCallback() {
        override fun onPlaybackConfigChanged(configs: MutableList<AudioPlaybackConfiguration>?) {
          if (configs?.any { it.audioAttributes.usage == AudioAttributes.USAGE_VOICE_COMMUNICATION } == true) {
            // In a process of making a call
            RecorderNative.stopRecording?.invoke()
            stop()
          }
          super.onPlaybackConfigChanged(configs)
        }
      }, null)
  }
  private val helperPlayer: MediaPlayer =  MediaPlayer().apply {
        setAudioAttributes(
          AudioAttributes.Builder()
            .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
            .setUsage(AudioAttributes.USAGE_MEDIA)
            .build()
        )
  }
  // Filepath: String, onProgressUpdate
  private val currentlyPlaying: MutableState<Pair<String, (position: Int?, state: TrackState) -> Unit>?> = mutableStateOf(null)
  private var progressJob: Job? = null

  enum class TrackState {
    PLAYING, PAUSED, REPLACED
  }

  // Returns real duration of the track
  private fun start(filePath: String, seek: Int? = null, onProgressUpdate: (position: Int?, state: TrackState) -> Unit): Int? {
    if (!File(filePath).exists()) {
      Log.e(TAG, "No such file: $filePath")
      return null
    }

    VideoPlayer.stopAll()
    RecorderNative.stopRecording?.invoke()
    val current = currentlyPlaying.value
    if (current == null || current.first != filePath) {
      stopListener()
      player.reset()
      runCatching {
        player.setDataSource(filePath)
      }.onFailure {
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), it.message)
        return null
      }
      runCatching { player.prepare() }.onFailure {
        // Can happen when audio file is broken
        Log.e(TAG, it.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), it.message)
        return null
      }
    }
    if (seek != null) player.seekTo(seek)
    player.start()
    currentlyPlaying.value = filePath to onProgressUpdate
    progressJob = CoroutineScope(Dispatchers.Default).launch {
      onProgressUpdate(player.currentPosition, TrackState.PLAYING)
      while(isActive && player.isPlaying) {
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
    return player.duration
  }

  private fun pause(): Int {
    progressJob?.cancel()
    progressJob = null
    player.pause()
    return player.currentPosition
  }

  fun stop() {
    if (currentlyPlaying.value == null) return
    player.stop()
    stopListener()
  }

  fun stop(item: ChatItem) = stop(item.file?.fileName)

  // FileName or filePath are ok
  fun stop(fileName: String?) {
    if (fileName != null && currentlyPlaying.value?.first?.endsWith(fileName) == true) {
      stop()
    }
  }

  private fun stopListener() {
    val afterCoroutineCancel: CompletionHandler = {
      // Notify prev audio listener about stop
      currentlyPlaying.value?.second?.invoke(null, TrackState.REPLACED)
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

  fun play(
    filePath: String?,
    audioPlaying: MutableState<Boolean>,
    progress: MutableState<Int>,
    duration: MutableState<Int>,
    resetOnEnd: Boolean,
  ) {
    if (progress.value == duration.value) {
      progress.value = 0
    }
    val realDuration = start(filePath ?: return, progress.value) { pro, state ->
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

  fun pause(audioPlaying: MutableState<Boolean>, pro: MutableState<Int>) {
    pro.value = pause()
    audioPlaying.value = false
  }

  fun seekTo(ms: Int, pro: MutableState<Int>, filePath: String?) {
    pro.value = ms
    if (this.currentlyPlaying.value?.first == filePath) {
      player.seekTo(ms)
    }
  }

  fun duration(filePath: String): Int? {
    var res: Int? = null
    kotlin.runCatching {
      helperPlayer.setDataSource(filePath)
      helperPlayer.prepare()
      helperPlayer.start()
      helperPlayer.stop()
      res = helperPlayer.duration
      helperPlayer.reset()
    }
    return res
  }
}
