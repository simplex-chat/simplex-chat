package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.navigation.NavController
import chat.simplex.app.model.*
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.chatlist.ChatPreviewView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@Composable
fun ChatView(chatModel: ChatModel, nav: NavController) {
  if (chatModel.chatId.value != null && chatModel.chats.count() > 0) {
    val chat: Chat = chatModel.chats.first { chat -> chat.chatInfo.id == chatModel.chatId.value }
    Column {
      ChatInfoToolbar(chat, nav)
      ChatItemsList(chatModel.chatItems)
      SendMsgView(sendMessage = { msg ->
        GlobalScope.launch {
          withContext(Dispatchers.Main) {
            // show "in progress"
            val cInfo = chat.chatInfo
            val newItem = chatModel.controller.apiSendMessage(
              type = cInfo.chatType,
              id = cInfo.apiId,
              mc = MsgContent.MCText(msg)
            )
            // hide "in progress"
            // TODO add new item
          }
        }
      })
    }
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, nav: NavController) {
  Row {
    Button(onClick = { nav.popBackStack() }) {
      Text("Back")
    }
    Column {
      Text(chat.chatInfo.displayName)
      Text(chat.chatInfo.fullName)
    }
  }
}

@Composable
fun ChatItemsList(chatItems: List<ChatItem>) {
  LazyColumn {
    items(chatItems) { cItem ->
      ChatItemView(cItem)
    }
  }
}
