package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.ComposeLinkPreview

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
  var cancelledLinks = mutableSetOf<String>()

  fun cancelPreview() {
    val uri = linkPreview.value?.uri
    if (uri != null) {
      cancelledLinks.add(uri)
    }
    linkPreview.value = null
  }

  fun isValidLink(link: String): Boolean {
    return !(link.startsWith("https://simplex.chat",true) || link.startsWith("http://simplex.chat", true))
  }

  fun parseMessage(msg: String): String? {
    val parsedMsg = parseMarkdown(msg)
    if (parsedMsg != null){
      val link = parsedMsg.firstOrNull {
          item -> (item.format is Format.Uri && (item.link != null) && !(cancelledLinks.contains(item.link)) && isValidLink(item.link))
      }
      return link?.link
    }
    return null
  }

  Column {
    when {
      linkPreview.value != null -> {
        ComposeLinkPreview(linkPreview.value!!) { cancelPreview() }
      }
    }
    when {
      quotedItem.value != null -> {
        ContextItemView(quotedItem)
      }
      editingItem.value != null -> {
        ContextItemView(editingItem, editing = editingItem.value != null, resetMessage)
      }
      else -> {}
    }
    SendMsgView(msg, linkPreview, cancelledLinks, { text -> parseMessage(text) }, sendMessage, editing = editingItem.value != null)
  }
}
