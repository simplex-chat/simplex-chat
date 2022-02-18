package chat.simplex.app.views.chatlist

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import androidx.compose.material.Icon
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Person
import androidx.compose.ui.Alignment
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.*
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun ChatPreviewView(chat: Chat, onClick: () -> Unit) {
  Surface(
    border=BorderStroke(0.5.dp, MaterialTheme.colors.secondaryVariant),
    modifier= Modifier
      .fillMaxWidth()
      .clickable(onClick = onClick)

  ) {
    Row(
      modifier = Modifier.fillMaxWidth().padding(horizontal = 10.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {

      Icon(
        Icons.Filled.Person,
        contentDescription = "Avatar Placeholder",
        modifier = Modifier
          .clip(CircleShape)
          .border(1.5.dp, MaterialTheme.colors.secondary, CircleShape)
      )

      Spacer(modifier=Modifier.width(6.dp))
      Column(modifier = Modifier.padding(all = 8.dp)) {
        Text(chat.chatInfo.chatViewName, fontWeight = FontWeight.Bold)
        if (chat.chatItems.count() > 0) {
          Text(chat.chatItems.last().content.text, fontStyle = FontStyle.Italic)
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
      onClick = {}
    )
  }
}
