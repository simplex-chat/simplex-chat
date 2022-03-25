package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.*
import chat.simplex.app.model.ChatItem

// TODO ComposeState

@Composable
fun ComposeView(quotedItem: MutableState<ChatItem?>, editingItem: MutableState<ChatItem?>, sendMessage: (String) -> Unit) {
  Column {
    when {
      quotedItem.value != null -> {
        RelatedItemView(quotedItem)
      }
      editingItem.value != null -> {
        RelatedItemView(editingItem)
      }
      else -> { }
    }
    SendMsgView(sendMessage, editingItem.value != null)
  }
}
