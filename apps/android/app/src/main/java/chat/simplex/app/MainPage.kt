package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.WelcomeView
import chat.simplex.app.views.chatlist.ChatListView

@Composable
fun MainPage(chatModel: ChatModel, nav: NavController) {
  if (chatModel.currentUser.value == null) WelcomeView(chatModel) {
    nav.navigate(Pages.Chats.route)
  }
  else ChatListView(chatModel, nav)
}
