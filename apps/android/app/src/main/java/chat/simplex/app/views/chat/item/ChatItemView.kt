package chat.simplex.app.views.chat.item

import android.content.Context
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.copyText
import chat.simplex.app.views.helpers.shareText
import kotlinx.datetime.Clock

@Composable
fun ChatItemView(user: User, cItem: ChatItem, quotedItem: MutableState<ChatItem?>, cxt: Context, uriHandler: UriHandler? = null) {
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  var showMenu by remember { mutableStateOf(false) }
  Box(
    modifier = Modifier
      .padding(bottom = 4.dp)
      .fillMaxWidth()
      .padding(
        start = if (sent) 86.dp else 16.dp,
        end = if (sent) 16.dp else 86.dp,
      ),
    contentAlignment = alignment,
  ) {
    Column(Modifier.combinedClickable(onLongClick = { showMenu = true }, onClick = {})) {
      if (cItem.quotedItem == null && isShortEmoji(cItem.content.text)) {
        EmojiItemView(cItem)
      } else {
        FramedItemView(user, cItem, uriHandler)
      }
      DropdownMenu(expanded = showMenu, onDismissRequest = { showMenu = false }) {
        ItemAction("Reply", Icons.Outlined.Reply, onClick = {
          quotedItem.value = cItem
          showMenu = false
        })
        ItemAction("Share", Icons.Outlined.Share, onClick = {
          shareText(cxt, cItem.content.text)
          showMenu = false
        })
        ItemAction("Copy", Icons.Outlined.ContentCopy,  onClick = {
          copyText(cxt, cItem.content.text)
          showMenu = false
        })
      }
    }
  }
}

@Composable
private fun ItemAction(text: String, icon: ImageVector, onClick: () -> Unit) {
  DropdownMenuItem(onClick) {
    Row {
      Text(text, modifier = Modifier
        .fillMaxWidth()
        .weight(1F))
      Icon(icon, text, tint = HighOrLowlight)
    }
  }
}

@Preview
@Composable
fun PreviewChatItemView() {
  SimpleXTheme {
    ChatItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      quotedItem = remember { mutableStateOf(null) },
      cxt = LocalContext.current
    )
  }
}
