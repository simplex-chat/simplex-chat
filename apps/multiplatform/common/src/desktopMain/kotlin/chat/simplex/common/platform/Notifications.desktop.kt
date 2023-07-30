package chat.simplex.common.platform

import chat.simplex.common.simplexWindowState

actual fun allowedToShowNotification(): Boolean = !simplexWindowState.windowFocused.value
