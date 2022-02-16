package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.runtime.Composable
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.lifecycle.AndroidViewModel
import chat.simplex.app.model.*
import chat.simplex.app.views.TerminalView
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.modules.*


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
