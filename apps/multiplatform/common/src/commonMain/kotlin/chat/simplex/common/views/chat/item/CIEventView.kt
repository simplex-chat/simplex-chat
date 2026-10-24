package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.padding
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem
import chat.simplex.common.ui.theme.*

@Composable
fun CIEventView(text: AnnotatedString) {
  Text(text, Modifier.padding(horizontal = chatEventHorizontalPadding(), vertical = 6.dp), style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp), maxLines = 4)
}

@Composable
fun chatEventHorizontalPadding(): Dp = if (LocalTerminalLayout.current) CHAT_ITEM_TEXT_PADDING else 6.dp

@Composable
fun ChatEventWithMeta(ci: ChatItem, content: @Composable (ts: AnnotatedString) -> Unit) {
  ItemWithMeta(meta = { CIMetaView(ci, timedMessagesTTL = null, showStatus = false, showEdited = false, showViaProxy = false, showTimestamp = true) }) { inlineMeta ->
    content(
      if (inlineMeta != null) {
        AnnotatedString(ci.timestampText)
      } else {
        AnnotatedString("\u00A0" + reserveSpaceForMeta(ci.meta, null, null, MaterialTheme.colors.secondary, showStatus = false, showEdited = false, showTimestamp = true), reserveTimestampStyle)
      }
    )
  }
}
@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun CIEventViewPreview() {
  SimpleXTheme {
    CIEventView(buildAnnotatedString { append("event happened") })
  }
}
