package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.tooling.preview.Preview
import androidx.navigation.NavController
import chat.simplex.app.model.*
import chat.simplex.app.views.chat.item.ChatItemView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel, nav: NavController) {
  if (chatModel.chatId.value != null && chatModel.chats.count() > 0) {
    val chat: Chat = chatModel.chats.first { chat -> chat.chatInfo.id == chatModel.chatId.value }
    ChatViewLayout(chat, chatModel.chatItems, back = { nav.popBackStack() }, sendMessage = { msg ->
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
          if (newItem != null) chatModel.addChatItem(cInfo, newItem.chatItem)
        }
      }
    })
  }
}

@Composable
fun ChatViewLayout(
  chat: Chat, chatItems: List<ChatItem>,
  back: () -> Unit,
  sendMessage: (String) -> Unit
) {
  Column {
    ChatInfoToolbar(chat, back)
    ChatItemsList(chatItems)
    SendMsgView(sendMessage)
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, back: () -> Unit) {
  Row {
    Button(onClick = back) {
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

@Preview
@Composable
fun PreviewChatViewLayout() {
  val chatItems = listOf(
    ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    ),
    ChatItem.getSampleData(
      1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
    ),
    ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    ),
    ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    ),
    ChatItem.getSampleData(
      1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
    )
  )
  ChatViewLayout(
    chat = Chat(
      chatInfo = ChatInfo.Direct.sampleData,
      chatItems = chatItems,
      chatStats = Chat.ChatStats()
    ),
    chatItems = chatItems,
    back = {},
    sendMessage = {}
  )
}
