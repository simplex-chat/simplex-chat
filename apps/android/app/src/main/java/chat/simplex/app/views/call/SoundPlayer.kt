package chat.simplex.app.views.call

import android.content.Context
import android.media.MediaPlayer
import android.os.VibrationEffect
import android.os.Vibrator
import androidx.core.content.ContextCompat
import chat.simplex.app.R
import chat.simplex.app.views.helpers.withScope
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay

class SoundPlayer {
  var player: MediaPlayer? = null
  var playing = false

  fun start(cxt: Context, scope: CoroutineScope, sound: Boolean) {
    if (sound) player = MediaPlayer.create(cxt, R.raw.ring_once)
    val vibrator = ContextCompat.getSystemService(cxt, Vibrator::class.java)
    val effect = VibrationEffect.createOneShot(250, VibrationEffect.DEFAULT_AMPLITUDE)
    playing = true
    withScope(scope) {
      while (playing) {
        if (sound) player?.start()
        vibrator?.vibrate(effect)
        delay(3500)
      }
    }
  }

  fun stop() {
    playing = false
    player?.stop()
  }

  companion object {
    val shared = SoundPlayer()
  }
}