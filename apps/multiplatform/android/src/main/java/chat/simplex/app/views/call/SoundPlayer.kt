package chat.simplex.app.views.call

import android.content.Context
import android.media.*
import android.net.Uri
import android.os.VibrationEffect
import android.os.Vibrator
import androidx.core.content.ContextCompat
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.views.helpers.withScope
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay

class SoundPlayer {
  private var player: MediaPlayer? = null
  var playing = false

  fun start(cxt: Context, scope: CoroutineScope, sound: Boolean) {
    player?.reset()
    player = MediaPlayer().apply {
      setAudioAttributes(
        AudioAttributes.Builder()
          .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
          .setUsage(AudioAttributes.USAGE_NOTIFICATION_RINGTONE)
          .build()
      )
      setDataSource(SimplexApp.context, Uri.parse("android.resource://" + SimplexApp.context.packageName + "/" + R.raw.ring_once))
      prepare()
    }
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
