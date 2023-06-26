package chat.simplex.common.platform

import androidx.compose.runtime.MutableState
import chat.simplex.common.model.ChatItem
import kotlinx.coroutines.CoroutineScope

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
  override fun play(filePath: String?, audioPlaying: MutableState<Boolean>, progress: MutableState<Int>, duration: MutableState<Int>, resetOnEnd: Boolean) {
    /*LALAL*/
  }

  override fun stop() {
    /*LALAL*/
  }

  override fun stop(item: ChatItem) {
    /*LALAL*/
  }

  override fun stop(fileName: String?) {
    TODO("Not yet implemented")
  }

  override fun pause(audioPlaying: MutableState<Boolean>, pro: MutableState<Int>) {
    TODO("Not yet implemented")
  }

  override fun seekTo(ms: Int, pro: MutableState<Int>, filePath: String?) {
    /*LALAL*/
  }

  override fun duration(filePath: String): Int? {
    /*LALAL*/
    return null
  }
}

actual object SoundPlayer: SoundPlayerInterface {
  override fun start(scope: CoroutineScope, sound: Boolean) { /*LALAL*/ }
  override fun stop() { /*LALAL*/ }
}