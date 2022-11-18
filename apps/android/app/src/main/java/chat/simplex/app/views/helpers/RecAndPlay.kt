package chat.simplex.app.views.helpers

import android.media.*
import android.media.MediaRecorder.MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED
import android.os.Build
import android.util.Log
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import java.io.*
import java.text.SimpleDateFormat
import java.util.*

interface Recorder {
  val recordingInProgress: MutableState<Boolean>
  fun start(onStop: () -> Unit): String
  fun stop()
  fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>)
}

data class ProgressAndDuration(
  val progressMs: Int = 0,
  val durationMs: Int = 0
) {
  companion object {
    val Saver
      get() = Saver<MutableState<ProgressAndDuration>, Pair<Int, Int>>(
        save = { it.value.progressMs to it.value.durationMs },
        restore = { mutableStateOf(ProgressAndDuration(it.first, it.second)) }
      )
  }
}

class RecorderNative(private val recordedBytesLimit: Long): Recorder {
  companion object {
    // Allows to stop the recorder from outside without having the recorder in a variable
    var stopRecording: (() -> Unit)? = null
  }
  override val recordingInProgress = mutableStateOf(false)
  private var recorder: MediaRecorder? = null
  private fun initRecorder() =
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      MediaRecorder(SimplexApp.context)
    } else {
      MediaRecorder()
    }

  override fun start(onStop: () -> Unit): String {
    AudioPlayer.stop()
    recordingInProgress.value = true
    val rec: MediaRecorder
    recorder = initRecorder().also { rec = it }
    rec.setAudioSource(MediaRecorder.AudioSource.MIC)
    rec.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4)
    rec.setAudioEncoder(MediaRecorder.AudioEncoder.AAC)
    rec.setAudioChannels(1)
    rec.setAudioSamplingRate(16000)
    rec.setAudioEncodingBitRate(16000)
    rec.setMaxDuration(-1)
    rec.setMaxFileSize(recordedBytesLimit)
    val timestamp = SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(Date())
    val filePath = getAppFilePath(SimplexApp.context, uniqueCombine(SimplexApp.context, getAppFilePath(SimplexApp.context, "voice_${timestamp}.m4a")))
    rec.setOutputFile(filePath)
    rec.prepare()
    rec.start()
    rec.setOnInfoListener { mr, what, extra ->
      if (what == MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED) {
        stop()
        onStop()
      }
    }
    stopRecording = { stop(); onStop() }
    return filePath
  }

  override fun stop() {
    if (!recordingInProgress.value) return
    stopRecording = null
    recordingInProgress.value = false
    recorder?.metrics?.
    runCatching {
      recorder?.stop()
    }
    runCatching {
      recorder?.reset()
    }
    runCatching {
      // release all resources
      recorder?.release()
    }
    recorder = null
  }

  override fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>) {
    stop()
    runCatching { File(filePath).delete() }.getOrElse { Log.d(TAG, "Unable to delete a file: ${it.stackTraceToString()}") }
  }
}

object AudioPlayer {
  private val player = MediaPlayer().apply {
    setAudioAttributes(
      AudioAttributes.Builder()
        .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
        .setUsage(AudioAttributes.USAGE_MEDIA)
        .build()
    )
  }
  private val helperPlayer: MediaPlayer =  MediaPlayer().apply {
        setAudioAttributes(
          AudioAttributes.Builder()
            .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
            .setUsage(AudioAttributes.USAGE_MEDIA)
            .build()
        )
  }
  // Filepath: String, onStop: () -> Unit
  private val currentlyPlaying: MutableState<Pair<String, () -> Unit>?> = mutableStateOf(null)

  fun start(filePath: String, seek: Int? = null, onStop: () -> Unit): Boolean {
    if (!File(filePath).exists()) {
      Log.e(TAG, "No such file: $filePath")
      return false
    }

    RecorderNative.stopRecording?.invoke()
    val current = currentlyPlaying.value
    if (current == null || current.first != filePath) {
      player.reset()
      // Notify prev audio listener about stop
      current?.second?.invoke()
      kotlin.runCatching {
        player.setDataSource(filePath)
      }.getOrElse { Log.e(TAG, it.stackTraceToString()); return false }
      runCatching { player.prepare() }.onFailure {
        // Can happen when audio file is broken
        AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), it.message)
      }
    }
    if (seek != null) player.seekTo(seek)
    player.start()
    // Repeated calls to play/pause on the same track will not recompose all dependent views
    if (currentlyPlaying.value?.first != filePath) {
      currentlyPlaying.value = filePath to onStop
    }
    return true
  }

  fun pause(): Int {
    player.pause()
    return player.currentPosition
  }

  fun stop() {
    if (!player.isPlaying) return
    // Notify prev audio listener about stop
    currentlyPlaying.value?.second?.invoke()
    currentlyPlaying.value = null
    player.stop()
  }

  fun stop(item: ChatItem) = stop(item.file?.fileName)

  // FileName or filePath are ok
  fun stop(fileName: String?) {
    if (fileName != null && currentlyPlaying.value?.first?.endsWith(fileName) == true) {
      stop()
    }
  }

  /**
   * If player starts playing at 2637 ms in a track 2816 ms long (these numbers are just an example),
   * it will stop immediately after start but will not change currentPosition, so it will not be equal to duration.
   * However, it sets isPlaying to false. Let's do it ourselves in order to prevent endless waiting loop
   * */
  fun progressAndDurationOrEnded(): ProgressAndDuration =
    ProgressAndDuration(if (player.isPlaying) player.currentPosition else player.duration, player.duration)

  fun duration(filePath: String): Int {
    var res = 0
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
