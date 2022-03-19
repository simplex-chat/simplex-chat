package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.app.model.ChatItem

@Composable
fun ComposeView(quotedItem: MutableState<ChatItem?>, sendMessage: (String) -> Unit) {
  Column {
    QuotedItemView(quotedItem)
    SendMsgView(sendMessage)
  }
}
