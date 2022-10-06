package chat.simplex.app.views.helpers

import androidx.compose.animation.*
import androidx.compose.animation.core.FastOutSlowInEasing
import androidx.compose.animation.core.tween
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.navigation.NavHostController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.ChatListView
import chat.simplex.app.views.chatlist.ShareListView
import com.google.accompanist.navigation.animation.*

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
@Composable
fun ExperimentalAnimationNav(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit, stopped: Boolean, ): NavHostController {
  val navController = rememberAnimatedNavController()
  AnimatedNavHost(navController, startDestination = "LEFT") {
    composable(
      "LEFT",
      enterTransition = {
        when (initialState.destination.route) {
          "RIGHT" ->
            slideIntoContainer(AnimatedContentScope.SlideDirection.Left, animationSpec = tween(700))
          else -> null
        }
      },
      exitTransition = {
        when (targetState.destination.route) {
          "RIGHT" ->
            slideOutOfContainer(AnimatedContentScope.SlideDirection.Left, animationSpec = tween(700))
          else -> null
        }
      },
      popEnterTransition = {
        when (initialState.destination.route) {
          "RIGHT" ->
            slideIntoContainer(AnimatedContentScope.SlideDirection.Right, animationSpec = tween(700))
          else -> null
        }
      },
      popExitTransition = {
        when (targetState.destination.route) {
          "RIGHT" ->
            slideOutOfContainer(AnimatedContentScope.SlideDirection.Right, animationSpec = tween(700))
          else -> null
        }
      }
    ) {
      if (chatModel.sharedContent.value == null)
        ChatListView(chatModel, setPerformLA, stopped)
      else
        ShareListView(chatModel, stopped)
    }
    composable(
      "RIGHT",
      enterTransition = {
        when (initialState.destination.route) {
          "LEFT" ->
            slideIntoContainer(AnimatedContentScope.SlideDirection.Left, animationSpec = tween(700))
          else -> null
        }
      },
      exitTransition = {
        when (targetState.destination.route) {
          "LEFT" ->
            slideOutOfContainer(AnimatedContentScope.SlideDirection.Left, animationSpec = tween(700))
          else -> null
        }
      },
      popEnterTransition = {
        when (initialState.destination.route) {
          "LEFT" ->
            slideIntoContainer(AnimatedContentScope.SlideDirection.Right, animationSpec = tween(700))
          else -> null
        }
      },
      popExitTransition = {
        when (targetState.destination.route) {
          "LEFT" ->
            slideOutOfContainer(AnimatedContentScope.SlideDirection.Right, animationSpec = tween(700))
          else -> null
        }
      }
    ) { ChatView("@2", chatModel) }
  }

  return navController
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
