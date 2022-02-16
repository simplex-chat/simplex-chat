package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.lifecycle.AndroidViewModel


class MainActivity: ComponentActivity() {
  private val viewModel by viewModels<SimplexViewModel>()
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    setContent {
      SimpleXTheme {
        Navigation(viewModel = viewModel)
      }
    }
  }
}

class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

