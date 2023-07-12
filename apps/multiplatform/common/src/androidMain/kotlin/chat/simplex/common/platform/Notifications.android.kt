package chat.simplex.common.platform

actual fun allowedToShowNotification(): Boolean = !isAppOnForeground
