package chat.simplex.app.views.helpers

import androidx.compose.animation.core.*

fun <T> newChatSheetAnimSpec() = tween<T>(256, 0, LinearEasing)
