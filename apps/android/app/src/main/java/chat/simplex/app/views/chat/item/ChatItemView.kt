package chat.simplex.app.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.Clock

@Composable
fun ChatItemView(user: User, chatItem: ChatItem, uriHandler: UriHandler? = null) {
  val sent = chatItem.chatDir.sent
//  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart

//  Box(
//    modifier = Modifier
//      .padding(bottom = 4.dp)
//      .fillMaxWidth()
//      .padding(
//        start = if (sent) 60.dp else 16.dp,
//        end = if (sent) 16.dp else 60.dp,
//      ),
//    contentAlignment = alignment,
//  ) {
    if (chatItem.quotedItem == null && isShortEmoji(chatItem.content.text)) {
      EmojiItemView(chatItem)
    } else {
      FramedItemView(user, chatItem, uriHandler)
    }
//  }
}

@Preview
@Composable
fun PreviewChatItemView() {
  SimpleXTheme {
    ChatItemView(
      user = User.sampleData,
      chatItem = ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      )
    )
  }
}
