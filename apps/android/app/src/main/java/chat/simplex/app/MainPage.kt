package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.navigation.NavController
import chat.simplex.app.views.TerminalView

@Composable
fun MainPage(vm: SimplexViewModel, navController: NavController) {
  TerminalView(vm.chatModel, navController)
}
