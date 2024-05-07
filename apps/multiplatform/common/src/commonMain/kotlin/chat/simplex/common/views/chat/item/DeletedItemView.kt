package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem
import chat.simplex.common.ui.theme.*

@Composable
fun DeletedItemView(ci: ChatItem, timedMessagesTTL: Int?) {
  val sent = ci.chatDir.sent
  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (sent) sentColor else receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.Bottom
    ) {
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = MaterialTheme.colors.secondary)) { append(ci.content.text) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
        modifier = Modifier.padding(end = 8.dp)
      )
      CIMetaView(ci, timedMessagesTTL)
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun PreviewDeletedItemView() {
  SimpleXTheme {
    DeletedItemView(
      ChatItem.getDeletedContentSampleData(),
      null
    )
  }
}
