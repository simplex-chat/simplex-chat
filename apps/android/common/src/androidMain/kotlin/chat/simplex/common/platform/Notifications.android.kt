package chat.simplex.common.platform

import chat.simplex.common.model.NotificationsMode

actual fun allowedToShowNotification(): Boolean = !isAppOnForeground

actual var notificationsModeChanged: (mode: NotificationsMode) -> Unit = {}

actual var chatStartedAfterBeingOff: () -> Unit = {}

actual var chatStopped: () -> Unit = {}