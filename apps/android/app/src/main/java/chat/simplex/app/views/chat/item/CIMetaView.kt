package chat.simplex.app.views.chat.item

import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.model.CIDirection
import chat.simplex.app.model.ChatItem
import kotlinx.datetime.Clock

@Composable
fun CIMetaView(chatItem: ChatItem) {
  Text(
    chatItem.timestampText,
    color = Color.Gray,
    style = MaterialTheme.typography.caption
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
