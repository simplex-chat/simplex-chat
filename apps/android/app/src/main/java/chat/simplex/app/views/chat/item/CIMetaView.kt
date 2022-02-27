package chat.simplex.app.views.chat.item

import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.CIDirection
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import kotlinx.datetime.Clock

@Composable
fun CIMetaView(chatItem: ChatItem) {
  Text(
    chatItem.timestampText,
    color = HighOrLowlight,
    fontSize = 14.sp
  )
}

@Preview
@Composable
fun PreviewCIMetaView() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    )
  )
}
