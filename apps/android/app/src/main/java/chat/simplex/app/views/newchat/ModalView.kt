package chat.simplex.app.views.newchat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.Composable
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