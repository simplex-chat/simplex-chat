package chat.simplex.app.views.helpers

import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.tween

fun <T> newChatSheetAnimSpec(delayMillis: Int = 0) = tween<T>(200, delayMillis, LinearEasing)
