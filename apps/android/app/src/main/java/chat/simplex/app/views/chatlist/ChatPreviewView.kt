package chat.simplex.app.views.chatlist

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.ChatInfoImage

@Composable
fun ChatPreviewView(chat: Chat, goToChat: () -> Unit) {
  Surface(
    border = BorderStroke(0.5.dp, MaterialTheme.colors.secondary),
    modifier = Modifier
      .fillMaxWidth()
      .clickable(onClick = goToChat)
      .height(80.dp)
  ) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(horizontal = 14.dp),
    ) {
      Column(verticalArrangement = Arrangement.Center, modifier = Modifier.fillMaxHeight()) {
        ChatInfoImage(chat, size = 60.dp)
      }
      Spacer(modifier = Modifier.width(6.dp))
      Column(modifier = Modifier.padding(all = 8.dp)) {
        Row(horizontalArrangement = Arrangement.SpaceBetween, modifier = Modifier.fillMaxWidth()) {
          Text(chat.chatInfo.chatViewName, fontWeight = FontWeight.Bold)
          val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.createdAt)
          Text(ts, color = HighOrLowlight, style = MaterialTheme.typography.body2)
        }
        if (chat.chatItems.count() > 0) {
          Text(
            chat.chatItems.last().content.text,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis,
          )
        }
      }
    }
  }
}

@Preview
@Composable
fun ChatPreviewViewExample() {
  SimpleXTheme {
    ChatPreviewView(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = listOf(),
        chatStats = Chat.ChatStats()
      ),
      goToChat = {}
    )
  }
}
