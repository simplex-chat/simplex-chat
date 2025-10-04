package chat.simplex.common.platform

expect fun getWakeLock(timeout: Long): (() -> Unit)
