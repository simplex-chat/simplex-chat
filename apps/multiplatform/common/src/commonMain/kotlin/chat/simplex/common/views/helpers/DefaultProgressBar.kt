package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.DEFAULT_PADDING

@Composable
fun DefaultProgressView(description: String?) {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      CircularProgressIndicator(
        Modifier
          .padding(bottom = if (description != null) DEFAULT_PADDING else 0.dp)
          .size(30.dp),
        color = MaterialTheme.colors.secondary,
        strokeWidth = 2.5.dp
      )
      if (description != null) {
        Text(description, textAlign = TextAlign.Center)
      }
    }
  }
}
