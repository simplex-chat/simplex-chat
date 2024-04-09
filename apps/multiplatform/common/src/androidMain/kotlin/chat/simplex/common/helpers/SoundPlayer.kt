package chat.simplex.common.helpers

import android.media.*
import android.net.Uri
import android.os.*
import androidx.core.content.ContextCompat
import chat.simplex.common.R
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.withApi
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
  private var playingJob: Job? = null

  private fun start(soundPath: String, delay: Long, scope: CoroutineScope) {
    playingJob?.cancel()
    player?.reset()
    player = MediaPlayer().apply {
      setAudioAttributes(
        AudioAttributes.Builder()
          .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
          .setUsage(AudioAttributes.USAGE_VOICE_COMMUNICATION_SIGNALLING)
          .build()
      )
      setDataSource(androidAppContext, Uri.parse(soundPath))
      prepare()
    }
    playingJob = scope.launch {
      while (isActive) {
        player?.start()
        delay(delay)
      }
    }
  }

  override fun startInCallSound(scope: CoroutineScope) {
    start("android.resource://" + androidAppContext.packageName + "/" + R.raw.in_call, 2000, scope)
  }

  override fun startConnectingCallSound(scope: CoroutineScope) {
    // Taken from https://github.com/TelegramOrg/Telegram-Android
    // https://github.com/TelegramOrg/Telegram-Android/blob/master/LICENSE
    start("android.resource://" + androidAppContext.packageName + "/" + R.raw.connecting_call, 0, scope)
  }

  override fun vibrate(times: Int) {
    val vibrator = ContextCompat.getSystemService(androidAppContext, Vibrator::class.java)
    val effect = VibrationEffect.createOneShot(20, VibrationEffect.DEFAULT_AMPLITUDE)
    vibrator?.vibrateApiVersionAware(effect)
    repeat(times - 1) {
      withApi {
        delay(50)
        vibrator?.vibrateApiVersionAware(effect)
      }
    }
  }

  override fun stop() {
    playingJob?.cancel()
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
