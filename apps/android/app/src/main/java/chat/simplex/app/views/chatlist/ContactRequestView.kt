package chat.simplex.app.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.Chat
import chat.simplex.app.model.getTimestampText
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.views.helpers.ChatInfoImage

@Composable
fun ContactRequestView(chat: Chat) {
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
        fontWeight = FontWeight.Bold,
        color = MaterialTheme.colors.primary
      )
      Text(
        stringResource(R.string.contact_wants_to_connect_with_you),
        maxLines = 2,
        overflow = TextOverflow.Ellipsis
      )
    }
    val ts = getTimestampText(chat.chatInfo.createdAt)
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
    }
  }
}
