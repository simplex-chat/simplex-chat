package chat.simplex.common.views.helpers

import androidx.compose.animation.core.*

fun <T> chatListAnimationSpec() = tween<T>(durationMillis = 350, easing = FastOutSlowInEasing)

fun <T> audioProgressBarAnimationSpec() = tween<T>(durationMillis = 30, easing = LinearEasing)

fun <T> userPickerAnimSpec() = tween<T>(350, 0, FastOutSlowInEasing)

fun <T> mentionPickerAnimSpec() = tween<T>(350, 0, FastOutSlowInEasing)

fun <T> commandMenuAnimSpec() = tween<T>(350, 0, FastOutSlowInEasing)

fun <T> contextUserPickerAnimSpec() = tween<T>(350, 0, FastOutSlowInEasing)
