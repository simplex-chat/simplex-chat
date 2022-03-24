package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.CIDirection
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.*
import kotlinx.datetime.Clock

@Composable
fun EditingItemView(editingItem: MutableState<ChatItem?>) {
  val ei = editingItem.value
  if (ei != null) {
    val sent = ei.chatDir.sent
    Row(
      Modifier
        .padding(top = 8.dp)
        .background(if (sent) SentColorLight else ReceivedColorLight),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(
        Modifier
          .padding(start = 16.dp)
          .padding(vertical = 12.dp)
          .fillMaxWidth()
          .weight(1F)
      ) {
        Text(ei.content.text, maxLines = 3)
      }
      IconButton(onClick = { editingItem.value = null }) {
        Icon(
          Icons.Outlined.Close,
          "Cancel editing",
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.padding(10.dp)
        )
      }
    }
  }
}

@Preview
@Composable
fun PreviewEditingItemView() {
  SimpleXTheme {
    EditingItemView(
      editingItem = remember {
        mutableStateOf(
          ChatItem.getSampleData(
            1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
          )
        )
      }
    )
  }
}
