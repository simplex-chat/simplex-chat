package chat.simplex.app.views.chatlist

import android.icu.text.SimpleDateFormat
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
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.TextOverflow
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.Instant
import kotlinx.datetime.toJavaInstant
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.Instant as JavaInstant
import java.util.*



fun getDisplayTime(t: Instant) : String {
  val timeFormatter = SimpleDateFormat("HH:mm", Locale.getDefault())
  val dateFormatter = SimpleDateFormat("dd/MM/y", Locale.getDefault())
  val timeProvided: JavaInstant = t.toJavaInstant()
  val tz = TimeZone.getDefault()
  val dateProvided = LocalDateTime.ofInstant(timeProvided, tz.toZoneId()).toLocalDate()
  val today = LocalDateTime.now().atZone(tz.toZoneId()).toLocalDate()
  val yesterday = today.minusDays(1)

  return when (dateProvided) {
    today -> timeFormatter.format(Date.from(timeProvided))
    yesterday -> "yesterday"
    else -> dateFormatter.format(Date.from(timeProvided))
  }
}

@Composable
fun ChatPreviewView(chat: Chat, goToChat: () -> Unit) {
  Surface(
    border=BorderStroke(0.5.dp, MaterialTheme.colors.secondaryVariant),
    modifier= Modifier
      .fillMaxWidth()
      .clickable(onClick = goToChat)
      .height(80.dp)
  ) {
    Row(
      modifier = Modifier.fillMaxWidth().padding(horizontal = 14.dp),
    ) {
      Column(verticalArrangement=Arrangement.Center, modifier=Modifier.fillMaxHeight()) {
        Icon(
          Icons.Filled.Person,
          contentDescription = "Avatar Placeholder",
          modifier = Modifier
            .size(30.dp)
            .clip(CircleShape)
            .border(1.5.dp, MaterialTheme.colors.secondary, CircleShape)
        )
      }
      Spacer(modifier = Modifier.width(6.dp))
      Column(modifier = Modifier.padding(all = 8.dp)) {
        Row(horizontalArrangement=Arrangement.SpaceBetween, modifier=Modifier.fillMaxWidth()) {
          Text(chat.chatInfo.chatViewName, fontWeight = FontWeight.Bold)
          (
            if (chat.chatItems.count() > 0) {
              Text(getDisplayTime(chat.chatItems.last().meta.itemTs))
            }
            else Text("")  // TODO
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
