package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import chat.simplex.common.model.ChatItem
import kotlinx.coroutines.CoroutineScope

interface RecorderInterface {
  companion object {
    // Allows to stop the recorder from outside without having the recorder in a variable
    var stopRecording: (() -> Unit)? = null
    val extension: String = "m4a"
  }
  fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String
  fun stop(): Int
}

expect class RecorderNative: RecorderInterface

interface AudioPlayerInterface {
  fun play(
    filePath: String?,
    audioPlaying: MutableState<Boolean>,
    progress: MutableState<Int>,
    duration: MutableState<Int>,
    resetOnEnd: Boolean,
  )
  fun stop()
  fun stop(item: ChatItem)
  fun stop(fileName: String?)
  fun pause(audioPlaying: MutableState<Boolean>, pro: MutableState<Int>)
  fun seekTo(ms: Int, pro: MutableState<Int>, filePath: String?)
  fun duration(filePath: String): Int?
}

expect object AudioPlayer: AudioPlayerInterface

interface SoundPlayerInterface {
  fun start(scope: CoroutineScope, sound: Boolean)
  fun stop()
}

expect object SoundPlayer: SoundPlayerInterface