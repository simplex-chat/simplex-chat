package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.relocationRequester
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import kotlinx.datetime.*

val SentColorLight = Color(0x1E45B8FF)
val ReceivedColorLight = Color(0x1EF1F0F5)

@Composable
fun TextItemView(chatItem: ChatItem) {
  val sent = chatItem.chatDir.sent
  val color = if (sent) SentColorLight else ReceivedColorLight

  // TODO alignment
  Surface(
    shape = RoundedCornerShape(18.dp),
    modifier = Modifier
      .width(200.dp), // TODO move width somewhere else
    color = color
  ) {
    Column {
      Text(modifier = Modifier.padding(top = 6.dp, start = 12.dp), text = chatItem.content.text)
      Box(
        modifier = Modifier
          .align(Alignment.End)
          .padding(end = 12.dp, bottom = 6.dp)
      ) {
        CIMetaView(chatItem)
      }
    }
  }
}

@Preview
@Composable
fun PreviewTextItemViewSnd() {
  TextItemView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    )
  )
}

@Preview
@Composable
fun PreviewTextItemViewRcv() {
  TextItemView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
    )
  )
}
