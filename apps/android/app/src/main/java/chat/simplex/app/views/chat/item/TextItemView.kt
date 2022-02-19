package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.*

val SentColorLight = Color(0x1E45B8FF)
val ReceivedColorLight = Color(0x1EF1F0F5)

@Composable
fun TextItemView(chatItem: ChatItem) {
  val sent = chatItem.chatDir.sent
  val color = if (sent) SentColorLight else ReceivedColorLight

  Surface(
    shape = RoundedCornerShape(18.dp),
    color = color
  ) {
    Box(
      modifier = Modifier.padding(vertical = 6.dp, horizontal = 12.dp)
    ) {
      Column {
        Text(text = chatItem.content.text)
        CIMetaView(chatItem)
      }
    }
  }
}

@Preview
@Composable
fun PreviewTextItemViewSnd() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      )
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewRcv() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      )
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewLong() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1,
        CIDirection.DirectSnd(),
        Clock.System.now(),
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    )
  }
}
