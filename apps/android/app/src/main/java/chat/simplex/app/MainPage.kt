package chat.simplex.app

import androidx.compose.runtime.Composable
import chat.simplex.app.views.TerminalPage

@Composable
fun MainPage(vm: SimplexViewModel) {

  TerminalPage(vm.chatModel)
}