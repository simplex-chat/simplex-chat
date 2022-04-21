package chat.simplex.app.views.chat

import ComposeImageView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.AddCircleOutline
import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.ComposeLinkView
import chat.simplex.app.views.helpers.generalGetString

// TODO ComposeState

@Composable
fun ComposeView(
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  linkPreview: MutableState<LinkPreview?>,
  imagePreview: MutableState<String?>,
  sendMessage: (String) -> Unit,
  resetMessage: () -> Unit,
  parseMarkdown: (String) -> List<FormattedText>?,
  showBottomSheet: () -> Unit
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
    val ip = imagePreview.value
    if (ip != null) {
      ComposeImageView(ip)
    } else {
      val lp = linkPreview.value
      if (lp != null) ComposeLinkView(lp, ::cancelPreview)
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
    Row(
      modifier = Modifier.padding(start = 2.dp, end = 8.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(2.dp)
    ) {
      Icon(
        Icons.Outlined.AddCircleOutline,
        contentDescription = generalGetString(R.string.attach),
        tint = if (editingItem.value == null) MaterialTheme.colors.primary else Color.Gray,
        modifier = Modifier
          .size(40.dp)
          .padding(vertical = 4.dp)
          .clip(CircleShape)
          .clickable {
            if (editingItem.value == null) {
              showBottomSheet()
            }
          }
      )
      SendMsgView(msg, linkPreview, cancelledLinks, parseMarkdown, sendMessage,
        editing = editingItem.value != null, sendEnabled = msg.value.isNotEmpty() || imagePreview.value != null)
    }
  }
}
