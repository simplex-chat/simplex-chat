package chat.simplex.app.views.helpers

import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.app.TAG
import chat.simplex.app.ui.theme.SettingsBackgroundLight
import chat.simplex.app.ui.theme.isInDarkTheme
import kotlinx.coroutines.delay

@Composable
fun ModalView(
  close: () -> Unit,
  background: Color = MaterialTheme.colors.background,
  modifier: Modifier = Modifier,
  content: @Composable () -> Unit,
) {
  BackHandler(onBack = close)
  Surface(Modifier.fillMaxSize()) {
    Column(Modifier.background(background)) {
      CloseSheetBar(close)
      Box(modifier) { content() }
    }
  }
}

class ModalManager {
  private val modalViews = arrayListOf<(@Composable (close: () -> Unit) -> Unit)?>()
  private val modalCount = mutableStateOf(0)
  private val toRemove = mutableSetOf<Int>()

  fun showModal(settings: Boolean = false, content: @Composable () -> Unit) {
    showCustomModal { close ->
      ModalView(close, if (!settings || isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight, content = content)
    }
  }

  fun showModalCloseable(settings: Boolean = false, content: @Composable (close: () -> Unit) -> Unit) {
    showCustomModal { close ->
      ModalView(close, if (!settings || isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight, content = { content(close) })
    }
  }

  fun showCustomModal(modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d(TAG, "ModalManager.showModal")
    modalViews.add(modal)
    modalCount.value = modalViews.size - toRemove.size
  }

  fun closeModal() {
    if (modalViews.isNotEmpty()) {
      //modalViews.removeAt(modalViews.lastIndex)
      toRemove.add(modalViews.lastIndex - toRemove.size)
    }
    modalCount.value = modalViews.size - toRemove.size
  }

  fun closeModals() {
    while (modalViews.isNotEmpty()) closeModal()
  }

  @OptIn(ExperimentalAnimationApi::class)
  @Composable
  fun showInView() {
    AnimatedContent(targetState = modalCount.value,
      transitionSpec = {
        if (targetState > initialState) {
          fromEndToStartTransition()
        } else {
          fromStartToEndTransition()
        }.using(SizeTransform(clip = false))
      }
    ) {
      modalViews.getOrNull(it - 1)?.invoke(::closeModal)
      // This is needed because if we delete from modalViews immediately on request, animation will be bad
      if (toRemove.isNotEmpty() && it == modalCount.value && transition.currentState == EnterExitState.Visible && !transition.isRunning) {
        toRemove.removeIf { elem -> modalViews.removeAt(elem); true }
      }
    }
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
//  private fun <T> animationSpecFromStart() = tween<T>(durationMillis = 150, easing = FastOutLinearInEasing)
//  private fun <T> animationSpecFromEnd() = tween<T>(durationMillis = 100, easing = FastOutSlowInEasing)

  companion object {
    val shared = ModalManager()
  }
}
