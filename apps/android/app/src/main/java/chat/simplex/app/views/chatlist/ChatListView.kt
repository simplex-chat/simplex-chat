package chat.simplex.app.views.chatlist

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
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
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.text.font.FontWeight

@Composable
fun ChatListView(chatModel: ChatModel, navController: NavController) {
    Column(modifier = Modifier
      .padding(vertical = 8.dp)
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
    ) {
      ChatListToolbar()
      ChatList(chatModel, navController)
      Button(
        onClick = { navController.navigate(Pages.Terminal.route) },
        modifier = Modifier.padding(14.dp)
      ) {
        Text("Terminal")
      }
    }
}

@Composable
fun ChatListToolbar() {
  Row(
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier
      .fillMaxWidth()
      .height(40.dp)) {
    Icon(
      Icons.Outlined.Settings,
      "Settings Cog",
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(horizontal = 10.dp)
    )
    Text(
      "Your chats",
      color=MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Bold,
      modifier = Modifier.padding(5.dp)
    )
    Icon(
      Icons.Outlined.PersonAdd,
      "Add Contact",
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(horizontal = 10.dp)
    )
  }
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
    modifier = Modifier.fillMaxWidth()
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
