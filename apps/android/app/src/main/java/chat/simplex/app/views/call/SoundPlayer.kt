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
  var sounds: Sounds? = null
  var playing = false

  class Sounds(val player: MediaPlayer, val vibrator: Vibrator?, val effect: VibrationEffect)

  fun initialize(cxt: Context): Sounds {
    val s = Sounds(
      player = MediaPlayer.create(cxt, R.raw.ring_once),
      vibrator = ContextCompat.getSystemService(cxt, Vibrator::class.java),
      effect = VibrationEffect.createOneShot(250, VibrationEffect.DEFAULT_AMPLITUDE)
    )
    sounds = s
    return s
  }

  fun start(cxt: Context, scope: CoroutineScope, sound: Boolean) {
    val s = sounds ?: initialize(cxt)
    playing = true
    withScope(scope) {
      while (playing) {
        withScope(scope) {
          if (sound) s.player.start()
          s.vibrator?.vibrate(s.effect)
        }
        delay(3500)
      }
    }
  }

  fun stop() {
    playing = false
    sounds?.player?.stop()
  }

  companion object {
    val shared = SoundPlayer()
  }
}