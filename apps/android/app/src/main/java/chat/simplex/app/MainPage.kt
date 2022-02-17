package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.navigation.NavController
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.WelcomeView

@Composable
fun MainPage(vm: SimplexViewModel, navController: NavController) {
  if (vm.chatModel.currentUser.value == null) WelcomeView(vm=vm) {
    navController.navigate(Pages.Terminal.route)
  }
  else TerminalView(vm.chatModel, navController)
}
