package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.app.model.ChatItem

// TODO ComposeState

@Composable
fun ComposeView(
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  sendMessage: (String) -> Unit,
  resetMessage: () -> Unit
) {
  Column {
    when {
      quotedItem.value != null -> {
        ContextItemView(quotedItem)
      }
      editingItem.value != null -> {
        ContextItemView(editingItem, editing = editingItem.value != null, resetMessage)
      }
      else -> {}
    }
    SendMsgView(msg, sendMessage, editing = editingItem.value != null)
  }
}
