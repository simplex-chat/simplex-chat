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
    modalCount.value = modalViews.count()
  }

  fun closeModal() {
    if (modalViews.isNotEmpty()) {
      modalViews.removeAt(modalViews.lastIndex)
    }
    modalCount.value = modalViews.size
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
    }
  }

  @OptIn(ExperimentalAnimationApi::class)
  private fun fromStartToEndTransition() =
    slideInHorizontally(
      initialOffsetX = { fullWidth -> -fullWidth / 20 },
      animationSpec = animationSpec()
    ) with slideOutHorizontally(
      targetOffsetX = { fullWidth -> fullWidth },
      animationSpec = animationSpec()
    )
  @OptIn(ExperimentalAnimationApi::class)
  private fun fromEndToStartTransition() =
    slideInHorizontally(
      initialOffsetX = { fullWidth -> fullWidth / 20 },
      animationSpec = animationSpec()
    ) with slideOutHorizontally (
      targetOffsetX = { fullWidth -> 0 },
      animationSpec = animationSpec()
    )

  fun <T> animationSpec() = spring<T>()

  companion object {
    val shared = ModalManager()
  }
}
