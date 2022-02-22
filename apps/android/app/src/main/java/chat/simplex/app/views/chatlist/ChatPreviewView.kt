package chat.simplex.app.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.ChatInfoImage
import chat.simplex.app.views.helpers.badgeLayout
import kotlinx.datetime.Clock

@Composable
fun ChatPreviewView(chat: Chat, goToChat: () -> Unit) {
  Surface(
    border = BorderStroke(0.5.dp, MaterialTheme.colors.secondary),
    modifier = Modifier
      .fillMaxWidth()
      .clickable(onClick = goToChat)
      .height(88.dp)
  ) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(vertical = 8.dp)
        .padding(start = 8.dp)
        .padding(end = 12.dp),
      verticalAlignment = Alignment.Top
    ) {
      ChatInfoImage(chat, size = 72.dp)
      Column(modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)) {
        Text(
          chat.chatInfo.chatViewName,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.h3,
          fontWeight = FontWeight.Bold
        )
        if (chat.chatItems.count() > 0) {
          Text(
            chat.chatItems.last().content.text,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis
          )
        }
      }
      val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.createdAt)
      Column(Modifier.fillMaxHeight(),
        verticalArrangement = Arrangement.Top) {
        Text(ts,
          color = HighOrLowlight,
          style = MaterialTheme.typography.body2,
          modifier = Modifier.padding(bottom=5.dp)
        )

        if (chat.chatStats.unreadCount > 0) {
          Text(
            chat.chatStats.unreadCount.toString(),
            color = MaterialTheme.colors.onPrimary,
            style = MaterialTheme.typography.body2,
            modifier = Modifier
              .background(MaterialTheme.colors.primary, shape = CircleShape)
              .align(Alignment.End)
              .badgeLayout()
              .padding(2.dp)
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
        chatItems = listOf(ChatItem.getSampleData(
          1,
          CIDirection.DirectSnd(),
          Clock.System.now(),
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        )),
        chatStats = Chat.ChatStats()
      ),
      goToChat = {}
    )
  }
}
