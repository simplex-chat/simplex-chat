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
import java.util.concurrent.atomic.AtomicBoolean

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
  private val modalViews = arrayListOf<Pair<Boolean, (@Composable (close: () -> Unit) -> Unit)>>()
  private val modalCount = mutableStateOf(0)
  private val toRemove = mutableSetOf<Int>()
  private var oldViewChanging = AtomicBoolean(false)

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

  fun showCustomModal(animated: Boolean = true, modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d(TAG, "ModalManager.showModal")
    // Means, animation is in progress or not started yet. Do not wait until animation finishes, just remove all from screen.
    // This is useful when invoking close() and ShowCustomModal one after another without delay. Otherwise, screen will hold prev view
    if (toRemove.isNotEmpty()) {
      runAtomically { toRemove.removeIf { elem -> modalViews.removeAt(elem); true } }
    }
    modalViews.add(animated to modal)
    modalCount.value = modalViews.size - toRemove.size
  }

  fun hasModalsOpen() = modalCount.value > 0

  fun closeModal() {
    if (modalViews.isNotEmpty()) {
      if (modalViews.lastOrNull()?.first == false) modalViews.removeAt(modalViews.lastIndex)
      else runAtomically { toRemove.add(modalViews.lastIndex - toRemove.size) }
    }
    modalCount.value = modalViews.size - toRemove.size
  }

  fun closeModals() {
    while (modalCount.value > 0) closeModal()
  }

  @OptIn(ExperimentalAnimationApi::class)
  @Composable
  fun showInView() {
    // Without animation
    if (modalCount.value > 0 && modalViews.lastOrNull()?.first == false) {
      modalViews.lastOrNull()?.second?.invoke(::closeModal)
      return
    }
    AnimatedContent(targetState = modalCount.value,
      transitionSpec = {
        if (targetState > initialState) {
          fromEndToStartTransition()
        } else {
          fromStartToEndTransition()
        }.using(SizeTransform(clip = false))
      }
    ) {
      modalViews.getOrNull(it - 1)?.second?.invoke(::closeModal)
      // This is needed because if we delete from modalViews immediately on request, animation will be bad
      if (toRemove.isNotEmpty() && it == modalCount.value && transition.currentState == EnterExitState.Visible && !transition.isRunning) {
        runAtomically { toRemove.removeIf { elem -> modalViews.removeAt(elem); true } }
      }
    }
  }

  /**
  * Allows to modify a list without getting [ConcurrentModificationException]
  * */
  private fun runAtomically(atomicBoolean: AtomicBoolean = oldViewChanging, block: () -> Unit) {
    while (!atomicBoolean.compareAndSet(false, true)) {
      Thread.sleep(10)
    }
    block()
    atomicBoolean.set(false)
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
