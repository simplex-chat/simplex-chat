package chat.simplex.common.helpers

import android.media.*
import android.net.Uri
import android.os.VibrationEffect
import android.os.Vibrator
import androidx.core.content.ContextCompat
import chat.simplex.common.R
import chat.simplex.common.platform.SoundPlayerInterface
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.views.helpers.withScope
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay

object SoundPlayer: SoundPlayerInterface {
  private var player: MediaPlayer? = null
  var playing = false

  override fun start(scope: CoroutineScope, sound: Boolean) {
    player?.reset()
    player = MediaPlayer().apply {
      setAudioAttributes(
        AudioAttributes.Builder()
          .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
          .setUsage(AudioAttributes.USAGE_NOTIFICATION_RINGTONE)
          .build()
      )
      setDataSource(androidAppContext, Uri.parse("android.resource://" + androidAppContext.packageName + "/" + R.raw.ring_once))
      prepare()
    }
    val vibrator = ContextCompat.getSystemService(androidAppContext, Vibrator::class.java)
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

  override fun stop() {
    playing = false
    player?.stop()
  }
}
