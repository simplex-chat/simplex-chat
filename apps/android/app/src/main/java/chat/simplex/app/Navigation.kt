package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController

@Composable
fun Navigation(viewModel: SimplexViewModel) {
  val navController = rememberNavController()

  NavHost(navController=navController, startDestination=Pages.Home.route){
    composable(route=Pages.Home.route){
      MainPage(vm = viewModel)
    }
//    composable(route=Pages.Welcome.route){
//      WelcomeView(vm.)
//    }
  }
}