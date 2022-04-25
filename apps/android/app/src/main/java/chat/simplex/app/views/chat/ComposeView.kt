package chat.simplex.app.views.chat

import ComposeImageView
import android.graphics.Bitmap
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AttachFile
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.ComposeLinkView

// TODO ComposeState
@Composable
fun ComposeView(
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  linkPreview: MutableState<LinkPreview?>,
  chosenImage: MutableState<Bitmap?>,
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

  fun cancelImage() {
    chosenImage.value = null
    imagePreview.value = null
  }

  Column {
    val ip = imagePreview.value
    if (ip != null) {
      ComposeImageView(ip, ::cancelImage)
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
      modifier = Modifier.padding(start = 4.dp, end = 8.dp),
      verticalAlignment = Alignment.Bottom,
      horizontalArrangement = Arrangement.spacedBy(2.dp)
    ) {
      Box(Modifier.padding(bottom = 12.dp)) {
        Icon(
          Icons.Filled.AttachFile,
          contentDescription = stringResource(R.string.attach),
          tint = if (editingItem.value == null) MaterialTheme.colors.primary else Color.Gray,
          modifier = Modifier
            .size(28.dp)
            .clip(CircleShape)
            .clickable {
              if (editingItem.value == null) {
                showBottomSheet()
              }
            }
        )
      }
      SendMsgView(
        msg, linkPreview, cancelledLinks, parseMarkdown, sendMessage,
        editing = editingItem.value != null, sendEnabled = msg.value.isNotEmpty() || imagePreview.value != null
      )
    }
  }
}
