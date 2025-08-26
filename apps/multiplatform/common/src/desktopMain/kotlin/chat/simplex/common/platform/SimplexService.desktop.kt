package chat.simplex.common.platform

import kotlin.math.exp

actual fun getWakeLock(timeout: Long): WakeLockProxy {
  return WakeLockProxy(acquire = { _ -> })
}
