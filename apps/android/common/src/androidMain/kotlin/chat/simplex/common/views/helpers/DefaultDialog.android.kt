package chat.simplex.common.views.helpers

import androidx.compose.runtime.Composable
import androidx.compose.ui.window.Dialog

@Composable
actual fun DefaultDialog(
  onDismissRequest: () -> Unit,
  content: @Composable () -> Unit
) {
  Dialog(
    onDismissRequest = onDismissRequest
  ) {
    content()
  }
}