package chat.simplex.app.views.helpers

import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun <S: Comparable<S>> AnimateScreens(
  targetState: MutableState<S>,
  content: @Composable AnimatedVisibilityScope.(targetState: S) -> Unit
) {
  AnimatedContent(
    targetState = targetState.value,
    transitionSpec = {
      when {
        targetState.value > initialState -> fromEndToStartTransition()
        else -> fromStartToEndTransition()
      }.using(SizeTransform(clip = false))
    },
    content = content
  )
}

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun <S> AnimateScreensNullable(
  targetState: MutableState<S>,
  content: @Composable AnimatedVisibilityScope.(targetState: S) -> Unit
) {
  AnimatedContent(
    targetState = targetState.value,
    transitionSpec = {
      when {
        targetState.value != null -> fromEndToStartTransition()
        else -> fromStartToEndTransition()
      }.using(SizeTransform(clip = false))
    },
    content = content
  )
}

@OptIn(ExperimentalAnimationApi::class)
private fun fromStartToEndTransition() =
  slideInHorizontally(
    initialOffsetX = { fullWidth -> -fullWidth },
    animationSpec = animationSpec()
  ) with slideOutHorizontally(
    targetOffsetX = { fullWidth -> fullWidth },
    animationSpec = animationSpec()
  )

@OptIn(ExperimentalAnimationApi::class)
private fun fromEndToStartTransition() =
  slideInHorizontally(
    initialOffsetX = { fullWidth -> fullWidth },
    animationSpec = animationSpec()
  ) with slideOutHorizontally(
    targetOffsetX = { fullWidth -> -fullWidth },
    animationSpec = animationSpec()
  )

private fun <T> animationSpec() = tween<T>(durationMillis = 250, easing = FastOutSlowInEasing)

fun <T> newChatSheetAnimSpec() = tween<T>(256, 0, LinearEasing)
