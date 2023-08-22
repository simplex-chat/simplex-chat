package chat.simplex.common.views.helpers

import androidx.compose.animation.core.*

fun <T> chatListAnimationSpec() = tween<T>(durationMillis = 250, easing = FastOutSlowInEasing)

fun <T> newChatSheetAnimSpec() = tween<T>(256, 0, LinearEasing)

fun <T> audioProgressBarAnimationSpec() = tween<T>(durationMillis = 30, easing = LinearEasing)
