package chat.simplex.app.views

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier

@Composable
fun ConnectWithView(link: String) {
  Box(modifier = Modifier.fillMaxSize()){
    Text(link)
  }
}
