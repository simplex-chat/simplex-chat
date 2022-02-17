package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.runtime.Composable
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.lifecycle.AndroidViewModel
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.WelcomeView


class MainActivity: ComponentActivity() {
  private val viewModel by viewModels<SimplexViewModel>()
  override fun onCreate(savedInstanceState: Bundle?) {
    val route: String = if(viewModel.chatModel.currentUser == null) Pages.Welcome.route else Pages.Home.route
    super.onCreate(savedInstanceState)
    setContent {
      SimpleXTheme {
        Navigation(viewModel=viewModel, initialRoute=route)
      }
    }
  }
}

class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

@Composable
fun Navigation(viewModel: SimplexViewModel, initialRoute: String) {
  val navController = rememberNavController()

  NavHost(navController=navController, startDestination=initialRoute){
    composable(route=Pages.Home.route){
      MainPage(vm = viewModel)
    }
    composable(route=Pages.Welcome.route){
      WelcomeView(vm = viewModel) {navController.navigate(Pages.Home.route) { popUpTo(Pages.Home.route) { inclusive = true }}}
    }
    composable(route=Pages.Terminal.route){
      TerminalView(chatModel = viewModel.chatModel, navController = navController)
    }
  }
}

sealed class Pages(val route: String) {
  object Home : Pages("home")
  object Terminal : Pages("terminal")
  object Welcome : Pages("welcome")
}
