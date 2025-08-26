package chat.simplex.common.platform

import android.content.Context
import android.os.PowerManager

actual fun getWakeLock(timeout: Long): WakeLockProxy {
  val context = AppContextProvider.getApplicationContext()
    ?: throw IllegalStateException("Application context not initialized")
  val wakeLock: PowerManager.WakeLock? = (context.applicationContext.getSystemService(Context.POWER_SERVICE) as PowerManager).run {
    newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "SimplexService::lock").apply {
      acquire(timeout)
    }
  }
  return WakeLockProxy(
    acquire = { t ->
      val lock = wakeLock
      if (lock == null) Log.e(TAG,"WakeLockProxy.extend called on released lock")
      else lock.acquire(t)
    }
  )
}

object AppContextProvider {
  private var applicationContext: Context? = null

  fun initialize(context: Context) {
    this.applicationContext = context.applicationContext
  }

  fun getApplicationContext(): Context? = applicationContext
}
