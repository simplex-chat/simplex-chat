package chat.simplex.app.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Person
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme

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
      modifier = Modifier.fillMaxWidth().padding(horizontal = 14.dp),
    ) {
      Column(verticalArrangement = Arrangement.Center, modifier = Modifier.fillMaxHeight()) {
        Icon(
          Icons.Filled.Person,
          contentDescription = "Avatar Placeholder",
          tint = MaterialTheme.colors.background,
          modifier = Modifier
            .size(55.dp)
            .clip(CircleShape)
            .border(1.5.dp, MaterialTheme.colors.secondary, CircleShape)
            .background(MaterialTheme.colors.secondary)
        )
      }
      Spacer(modifier = Modifier.width(6.dp))
      Column(modifier = Modifier.padding(all = 8.dp)) {
        Row(horizontalArrangement = Arrangement.SpaceBetween, modifier = Modifier.fillMaxWidth()) {
          Text(chat.chatInfo.chatViewName, fontWeight = FontWeight.Bold)
          (
            if (chat.chatItems.count() > 0) {
              Text(getTimestampText(chat.chatItems.last().meta.itemTs), color = HighOrLowlight)
            }
            else Text(getTimestampText(chat.chatInfo.createdAt), color = HighOrLowlight)
          )
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
