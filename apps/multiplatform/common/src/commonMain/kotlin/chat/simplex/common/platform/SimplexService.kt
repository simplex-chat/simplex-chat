package chat.simplex.common.platform

expect fun getWakeLock(timeout: Long): WakeLockProxy

class WakeLockProxy(val acquire: (timeout: Long) -> Unit)
