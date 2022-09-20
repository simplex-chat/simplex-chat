package chat.simplex.app.views.helpers

import android.media.*
import android.os.Build
import android.util.Log
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import chat.simplex.app.SimplexApp
import chat.simplex.app.TAG
import java.io.*

interface Recorder {
  fun start(recordingInProgress: MutableState<Boolean>): String
  fun stop(recordingInProgress: MutableState<Boolean>)
  fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>)
}

class RecorderNative: Recorder {
  private val recorder =
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      MediaRecorder(SimplexApp.context)
    } else {
      MediaRecorder()
    }

  override fun start(recordingInProgress: MutableState<Boolean>): String {
    recordingInProgress.value = true
    recorder.setAudioSource(MediaRecorder.AudioSource.MIC)
    recorder.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4)
    recorder.setAudioEncoder(MediaRecorder.AudioEncoder.AAC)
    recorder.setAudioChannels(1)
    recorder.setAudioSamplingRate(16000)
    recorder.setAudioEncodingBitRate(16000)
    recorder.setMaxDuration(-1)
    val filePath = getAppFilePath(SimplexApp.context, uniqueCombine(SimplexApp.context, getAppFilePath(SimplexApp.context, "voice.m4a")))
    recorder.setOutputFile(filePath)
    recorder.prepare()
    recorder.start()
    return filePath
  }

  override fun stop(recordingInProgress: MutableState<Boolean>) {
    recordingInProgress.value = false
    runCatching {
      recorder.stop()
    }
    runCatching {
      recorder.reset()
    }
  }

  override fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>) {
    stop(recordingInProgress)
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
  val currentlyPlaying: MutableState<String?> = mutableStateOf(null)

  fun start(filePath: String, seek: Int? = null) {
    if (currentlyPlaying.value != filePath) {
      player.reset()
      kotlin.runCatching {
        player.setDataSource(filePath)
      }.getOrElse { Log.e(TAG, it.stackTraceToString()); return } // probably missing file in data directory
      player.prepare()
    }
    if (seek != null) player.seekTo(seek)
    player.start()
    currentlyPlaying.value = filePath
  }

  fun pause(): Int {
    player.pause()
    return player.currentPosition
  }

  fun stop() {
    if (!player.isPlaying) return
    player.stop()
  }

  fun progressAndDuration(): Pair<Int, Int> = player.currentPosition to player.duration

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