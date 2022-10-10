package chat.simplex.app.views.helpers

import androidx.compose.animation.core.*

fun <T> newChatSheetAnimSpec(delayInMillis: Int = 0) = tween<T>(256, delayInMillis, LinearEasing)
