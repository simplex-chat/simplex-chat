package chat.simplex.common.helpers

import android.media.*
import android.net.Uri
import android.os.*
import androidx.core.content.ContextCompat
import chat.simplex.common.R
import chat.simplex.common.platform.*
import kotlinx.coroutines.*

object SoundPlayer: SoundPlayerInterface {
  private var player: MediaPlayer? = null
  private var playing = false

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
    scope.launch {
      while (playing) {
        if (sound) player?.start()
        vibrator?.vibrateApiVersionAware(effect)
        delay(3500)
      }
    }
  }

  override fun stop() {
    playing = false
    player?.stop()
  }
}

object CallSoundsPlayer: CallSoundsPlayerInterface {
  private var player: MediaPlayer? = null
  private var playing = false

  override fun startWaitingAnswerSound(scope: CoroutineScope) {
    player?.reset()
    player = MediaPlayer().apply {
      setAudioAttributes(
        AudioAttributes.Builder()
          .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
          .setUsage(AudioAttributes.USAGE_VOICE_COMMUNICATION_SIGNALLING)
          .build()
      )
      setDataSource(androidAppContext, Uri.parse("android.resource://" + androidAppContext.packageName + "/" + R.raw.call_sound_before_answer))
      prepare()
    }
    playing = true
    scope.launch {
      while (playing) {
        player?.start()
        delay(2000)
      }
    }
  }

  override fun vibrate() {
    val vibrator = ContextCompat.getSystemService(androidAppContext, Vibrator::class.java)
    val effect = VibrationEffect.createOneShot(50, VibrationEffect.DEFAULT_AMPLITUDE)
    vibrator?.vibrateApiVersionAware(effect)
  }

  override fun stop() {
    playing = false
    player?.stop()
  }
}

private fun Vibrator.vibrateApiVersionAware(effect: VibrationEffect) {
  if (Build.VERSION.SDK_INT >= 33) {
    vibrate(
      effect,
      VibrationAttributes.Builder()
        .setUsage(VibrationAttributes.USAGE_ALARM)
        .build()
    )
  } else if (Build.VERSION.SDK_INT >= 29) {
    vibrate(
      effect,
      AudioAttributes.Builder()
        .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
        .setUsage(AudioAttributes.USAGE_ALARM)
        .build()
    )
  } else {
    vibrate(effect)
  }
}
