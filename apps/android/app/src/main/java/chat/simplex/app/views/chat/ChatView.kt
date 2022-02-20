package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBack
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.DelicateCoroutinesApi
import kotlinx.datetime.Clock

@DelicateCoroutinesApi
@Composable
fun ChatView(chatModel: ChatModel, nav: NavController) {
  if (chatModel.chatId.value != null && chatModel.chats.count() > 0) {
    val chat: Chat = chatModel.chats.first { chat -> chat.chatInfo.id == chatModel.chatId.value }
    ChatLayout(chat, chatModel.chatItems, back = { nav.popBackStack() }, sendMessage = { msg ->
      withApi {
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
    })
  }
}

@Composable
fun ChatLayout(
  chat: Chat, chatItems: List<ChatItem>,
  back: () -> Unit,
  sendMessage: (String) -> Unit
) {
  Scaffold(
    topBar = { ChatInfoToolbar(chat, back) },
    bottomBar = { SendMsgView(sendMessage) }
  ) { contentPadding ->
    Box(
      modifier = Modifier
        .padding(contentPadding)
        .fillMaxWidth()
        .background(MaterialTheme.colors.background)
    ) {
      ChatItemsList(chatItems)
    }
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, back: () -> Unit) {
  Box(
    modifier = Modifier.fillMaxWidth(),
    contentAlignment = Alignment.CenterStart
  ) {
    Icon(
      Icons.Outlined.ArrowBack,
      "Back",
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .clickable(onClick = back)
        .padding(start = 16.dp)
    )
    Column(
      Modifier
        .padding(horizontal = 40.dp)
        .fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
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
  SimpleXTheme {
    val chatItems = listOf(
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        2, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        3, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        4, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        5, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      )
    )
    ChatLayout(
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
}
