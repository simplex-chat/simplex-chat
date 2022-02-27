package chat.simplex.app.views.newchat

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.app.views.helpers.CloseSheetBar

@Composable
fun ModalView(close: () -> Unit, content: @Composable () -> Unit) {
  Surface(
    Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
    Column(Modifier.padding(horizontal = 16.dp)) {
      CloseSheetBar(close)
      content()
    }
  }
}

class ModalManager {
  private val modalViews = arrayListOf<(@Composable (close: () -> Unit) -> Unit)?>()
  private val modalCount = mutableStateOf(0)

  fun showModal(content: @Composable () -> Unit) {
    showCustomModal { close -> ModalView(close, content) }
  }

  fun showCustomModal(modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d("SIMPLEX", "ModalManager.showModal")
    modalViews.add(modal)
    modalCount.value = modalViews.count()
  }

  fun closeModal() {
    if (!modalViews.isEmpty()) {
      modalViews.removeAt(modalViews.count() - 1)
    }
    modalCount.value = modalViews.count()
  }

  @Composable
  fun showInView() {
    if (modalCount.value > 0) modalViews.lastOrNull()?.invoke(::closeModal)
  }

  companion object {
    val shared = ModalManager()
  }
}
