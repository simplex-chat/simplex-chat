package chat.simplex.common.platform

import android.content.Context
import android.os.PowerManager

actual fun getWakeLock(timeout: Long): (() -> Unit) {
  val context = AppContextProvider.getApplicationContext()
    ?: throw IllegalStateException("Application context not initialized")
  var wakeLock: PowerManager.WakeLock? = (context.applicationContext.getSystemService(Context.POWER_SERVICE) as PowerManager).run {
    newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "SimplexService::lock").apply {
      acquire(timeout)
    }
  }
  return {
    wakeLock?.release()
    wakeLock = null
  }
}

object AppContextProvider {
  private var applicationContext: Context? = null

  fun initialize(context: Context) {
    this.applicationContext = context.applicationContext
  }

  fun getApplicationContext(): Context? = applicationContext
}
