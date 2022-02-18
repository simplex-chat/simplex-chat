package chat.simplex.app.views.chatlist

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.toMutableStateList
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.chat.SendMsgView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@Composable
fun ChatListView(chatModel: ChatModel, navController: NavController) {
    Column(modifier = Modifier.padding(all = 8.dp)) {
      ChatListToolbar()
      ChatList(chatModel, navController)
      Button(onClick = { navController.navigate(Pages.Terminal.route) }) {
        Text("Terminal")
      }
    }
}

@Composable
fun ChatListToolbar() {
  Text("ChatListToolbar")
}

fun goToChat(chat: Chat, chatModel: ChatModel, navController: NavController) {
  GlobalScope.launch {
    withContext(Dispatchers.Main) {
      val cInfo = chat.chatInfo
      val chat = chatModel.controller.apiGetChat(cInfo.chatType, cInfo.apiId)
      if (chat != null ) {
        chatModel.chatId = mutableStateOf(cInfo.id)
        chatModel.chatItems = chat.chatItems.toMutableStateList()
        navController.navigate(Pages.Chat.route)
      } else {
        // TODO show error? or will apiGetChat show it
      }
    }
  }
}

@Composable
fun ChatList(chatModel: ChatModel, navController: NavController) {
  LazyColumn(
    modifier=Modifier.fillMaxWidth()
  ) {
    items(chatModel.chats) { chat ->
      ChatPreviewView(chat) {goToChat(chat, chatModel, navController)}
    }
  }
}


//@Preview
//@Composable
//fun PreviewSendMsgView() {
//  SimpleXTheme {
//    ChatListView(
//      chats = listOf(
//        Chat()
//      ),
//
//    )
//  }
//}
