package chat.simplex.app.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun ChatPreviewView(chat: Chat) {
  Column(modifier = Modifier.padding(all = 8.dp)) {
    Text(chat.chatInfo.chatViewName)
    if (chat.chatItems.count() > 0) {
      Text(chat.chatItems.last().content.text)
    }
  }
}

@Preview
@Composable
fun ChatPreviewView() {
  SimpleXTheme {
    ChatPreviewView(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = listOf(),
        chatStats = Chat.ChatStats()
      )
    )
  }
}
