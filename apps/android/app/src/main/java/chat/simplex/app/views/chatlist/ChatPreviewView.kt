package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.ExperimentalTextApi
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.Chat
import chat.simplex.app.model.getTimestampText
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.ChatInfoImage
import chat.simplex.app.views.helpers.badgeLayout

@ExperimentalTextApi
@Composable
fun ChatPreviewView(chat: Chat) {
  Row {
    ChatInfoImage(chat, size = 72.dp)
    Column(
      modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)
    ) {
      Text(
        chat.chatInfo.chatViewName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold
      )

      if (chat.chatItems.count() > 0) {
        MarkdownText(
          chat.chatItems.last(),
          maxLines = 2,
          overflow = TextOverflow.Ellipsis
        )
      }
    }
    val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.createdAt)
    Column(
      Modifier.fillMaxHeight(),
      verticalArrangement = Arrangement.Top
    ) {
      Text(
        ts,
        color = HighOrLowlight,
        style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(bottom = 5.dp)
      )
      val n = chat.chatStats.unreadCount
      if (n > 0) {
        Text(
          if (n < 1000) "$n" else "${n / 1000}k",
          color = MaterialTheme.colors.onPrimary,
          fontSize = 14.sp,
          modifier = Modifier
            .background(MaterialTheme.colors.primary, shape = CircleShape)
            .align(Alignment.End)
            .badgeLayout()
            .padding(horizontal = 3.dp)
            .padding(vertical = 1.dp)
        )
      }
    }
  }
}

@ExperimentalTextApi
@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatPreviewView() {
  SimpleXTheme {
    ChatPreviewView(Chat.sampleData)
  }
}
