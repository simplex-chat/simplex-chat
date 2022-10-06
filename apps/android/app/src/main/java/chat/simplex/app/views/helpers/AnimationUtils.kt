package chat.simplex.app.views.helpers

import androidx.compose.animation.core.FastOutSlowInEasing
import androidx.compose.animation.core.tween

fun <T> newChatSheetAnimSpec(delayMillis: Int = 0) = tween<T>(256, delayMillis, FastOutSlowInEasing)
