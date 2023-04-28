package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.*
import kotlinx.datetime.Clock

@Composable
fun ContextItemView(
  contextItem: ChatItem,
  contextIcon: Painter,
  cancelContextItem: () -> Unit
) {
  val sent = contextItem.chatDir.sent
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  val receivedColor = CurrentColors.collectAsState().value.appColors.receivedMessage
  Row(
    Modifier
      .padding(top = 8.dp)
      .background(if (sent) sentColor else receivedColor),
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
        tint = MaterialTheme.colors.secondary,
      )
      MarkdownText(
        contextItem.text, contextItem.formattedText,
        sender = contextItem.memberDisplayName, senderBold = true, maxLines = 3,
        linkMode = SimplexLinkMode.DESCRIPTION,
        modifier = Modifier.fillMaxWidth(),
      )
    }
    IconButton(onClick = cancelContextItem) {
      Icon(
        painterResource(R.drawable.ic_close),
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
      contextIcon = painterResource(R.drawable.ic_edit_filled)
    ) {}
  }
}
