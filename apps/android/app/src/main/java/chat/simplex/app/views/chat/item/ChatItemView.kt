package chat.simplex.app.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.model.*
import kotlinx.datetime.Clock

@Composable
fun ChatItemView(chatItem: ChatItem) {
  TextItemView(chatItem)
}

@Preview
@Composable
fun PreviewChatItemView() {
  ChatItemView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    )
  )
}
