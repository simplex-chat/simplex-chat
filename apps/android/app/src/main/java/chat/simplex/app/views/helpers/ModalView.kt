package chat.simplex.app.views.helpers

import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.app.TAG
import chat.simplex.app.ui.theme.SettingsBackgroundLight
import chat.simplex.app.ui.theme.isInDarkTheme

@Composable
fun ModalView(
  title: String?,
  close: () -> Unit,
  background: Color = MaterialTheme.colors.background,
  modifier: Modifier = Modifier,
  content: @Composable () -> Unit,
) {
  BackHandler(onBack = close)
  Surface(Modifier.fillMaxSize()) {
    Column(Modifier.background(background)) {
      CloseSheetBar(title, close)
      Box(modifier) { content() }
    }
  }
}

class ModalManager {
  private val modalViews = arrayListOf<(@Composable (close: () -> Unit) -> Unit)?>()
  private val modalCount = mutableStateOf(0)

  fun showModal(title: String?, settings: Boolean = false, content: @Composable () -> Unit) {
    showCustomModal { close ->
      ModalView(title, close, if (!settings || isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight, content = content)
    }
  }

  fun showModalCloseable(title: String?, settings: Boolean = false, content: @Composable (close: () -> Unit) -> Unit) {
    showCustomModal { close ->
      ModalView(title, close, if (!settings || isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight, content = { content(close) })
    }
  }

  fun showCustomModal(modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d(TAG, "ModalManager.showModal")
    modalViews.add(modal)
    modalCount.value = modalViews.count()
  }

  fun closeModal() {
    if (modalViews.isNotEmpty()) {
      modalViews.removeAt(modalViews.count() - 1)
    }
    modalCount.value = modalViews.count()
  }

  fun closeModals() {
    while (modalViews.isNotEmpty()) closeModal()
  }

  @Composable
  fun showInView() {
    if (modalCount.value > 0) modalViews.lastOrNull()?.invoke(::closeModal)
  }

  companion object {
    val shared = ModalManager()
  }
}
