package chat.simplex.common.platform

actual fun getWakeLock(timeout: Long): (() -> Unit) = {}
