package chat.simplex.common.platform

import chat.simplex.common.model.NotificationsMode

expect fun allowedToShowNotification(): Boolean

expect var notificationsModeChanged: (mode: NotificationsMode) -> Unit

expect var chatStartedAfterBeingOff: () -> Unit

expect var chatStopped: () -> Unit