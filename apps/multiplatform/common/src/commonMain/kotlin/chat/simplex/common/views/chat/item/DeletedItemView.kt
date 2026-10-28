package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem
import chat.simplex.common.ui.theme.*

@Composable
fun DeletedItemView(ci: ChatItem, timedMessagesTTL: Int?, showViaProxy: Boolean, showTimestamp: Boolean, tailRendered: Boolean) {
  ItemWithMeta(meta = { CIMetaView(ci, timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp) }) { inlineMeta ->
    ItemTextCard(ci, tailRendered, 16.sp, inlineMeta) {
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = MaterialTheme.colors.secondary)) { append(ci.content.text) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp).merge(bubbleTextStyle(16.sp, MaterialTheme.colors.secondary)),
        modifier = Modifier.padding(end = if (inlineMeta != null) 8.dp else 0.dp).alignBy(LastBaseline)
      )
    }
  }
}

@Composable
fun ItemTextCard(ci: ChatItem, tailRendered: Boolean, firstLineFontSize: TextUnit, inlineMeta: (@Composable () -> Unit)?, modifier: Modifier = Modifier, text: @Composable RowScope.() -> Unit) {
  val color = if (ci.chatDir.sent) MaterialTheme.appColors.sentMessage else MaterialTheme.appColors.receivedMessage
  if (inlineMeta != null) {
    Surface(modifier, color = color, contentColor = LocalContentColor.current) {
      Row(Modifier.bubbleTailSpace(ci.chatDir.sent, tailRendered).bubbleTextPadding(firstLineFontSize)) {
        text()
        Box(bubbleMetaOnBaseline()) {
          inlineMeta()
        }
      }
    }
  } else {
    Surface(modifier, shape = RoundedCornerShape(18.dp), color = color, contentColor = LocalContentColor.current) {
      Row(Modifier.padding(horizontal = 12.dp, vertical = 6.dp)) {
        text()
      }
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun PreviewDeletedItemView() = BubblePreview {
  SimpleXTheme {
    val ci = ChatItem.getDeletedContentSampleData(ts = previewDate)
    ChatItemClipped(ci) { tailRendered ->
      DeletedItemView(
        ci,
        null,
        showViaProxy = false,
        showTimestamp = true,
        tailRendered = tailRendered
      )
    }
  }
}
