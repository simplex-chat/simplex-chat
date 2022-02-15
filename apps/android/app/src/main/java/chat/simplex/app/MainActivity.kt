package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.material.Button
import androidx.compose.material.TextField
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.material.Text
import androidx.compose.ui.unit.dp
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.lifecycle.AndroidViewModel
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.chat.SendMsgView

class MainActivity: ComponentActivity() {
  private val viewModel by viewModels<SimplexViewModel>()
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    setContent {
      SimpleXTheme {
        MainPage(viewModel)
      }
    }
  }
}

class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

@Composable
fun MainPage(vm: SimplexViewModel) {
  TerminalView(vm.chatModel)
}
