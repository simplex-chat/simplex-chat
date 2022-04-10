package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.ComposeLinkView

// TODO ComposeState

@Composable
fun ComposeView(
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  linkPreview: MutableState<LinkPreview?>,
  sendMessage: (String) -> Unit,
  resetMessage: () -> Unit,
  parseMarkdown: (String) -> List<FormattedText>?
) {
  val cancelledLinks = remember { mutableSetOf<String>() }

  fun cancelPreview() {
    val uri = linkPreview.value?.uri
    if (uri != null) {
      cancelledLinks.add(uri)
    }
    linkPreview.value = null
  }

  Column {
    val lp = linkPreview.value
    if (lp != null) ComposeLinkView(lp, ::cancelPreview)
    when {
      quotedItem.value != null -> {
        ContextItemView(quotedItem)
      }
      editingItem.value != null -> {
        ContextItemView(editingItem, editing = editingItem.value != null, resetMessage)
      }
      else -> {}
    }
    SendMsgView(msg, linkPreview, cancelledLinks, parseMarkdown, sendMessage, editing = editingItem.value != null)
  }
}
