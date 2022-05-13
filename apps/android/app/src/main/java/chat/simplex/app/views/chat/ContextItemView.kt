package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.CIDirection
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.*
import kotlinx.datetime.Clock

@Composable
fun ContextItemView(
  contextItem: ChatItem,
  contextIcon: ImageVector,
  cancelContextItem: () -> Unit
) {
  val sent = contextItem.chatDir.sent
  Row(
    Modifier
      .padding(top = 8.dp)
      .background(if (sent) SentColorLight else ReceivedColorLight),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier
        .padding(vertical = 12.dp)
        .fillMaxWidth()
        .weight(1F),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        contextIcon,
        modifier = Modifier
          .padding(horizontal = 8.dp)
          .height(20.dp)
          .width(20.dp),
        contentDescription = stringResource(R.string.icon_descr_context),
        tint = HighOrLowlight,
      )
      MarkdownText(
        contextItem.text, contextItem.formattedText,
        sender = contextItem.memberDisplayName, senderBold = true, maxLines = 3
      )
    }
    IconButton(onClick = cancelContextItem) {
      Icon(
        Icons.Outlined.Close,
        contentDescription = stringResource(R.string.cancel_verb),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@Preview
@Composable
fun PreviewContextItemView() {
  SimpleXTheme {
    ContextItemView(
      contextItem = ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), "hello"),
      contextIcon = Icons.Filled.Edit,
      cancelContextItem = {}
    )
  }
}
