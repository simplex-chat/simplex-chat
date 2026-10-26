package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.PaddingValues
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
import chat.simplex.common.views.helpers.toDp

@Composable
fun CIEventView(text: AnnotatedString) {
  Text(text, Modifier.padding(chatEventPadding()), style = chatEventTextStyle(MaterialTheme.typography.body1.copy(lineHeight = 22.sp)), maxLines = if (LocalTerminalLayout.current) Int.MAX_VALUE else 4)
}

@Composable
fun chatEventTextStyle(base: TextStyle = LocalTextStyle.current): TextStyle =
  if (LocalTerminalLayout.current) {
    base.merge(terminalTextStyle(12.sp, MaterialTheme.colors.secondary))
  } else {
    base
  }

@Composable
fun chatEventPadding(vertical: Dp = 6.dp): PaddingValues =
  if (LocalTerminalLayout.current) PaddingValues(start = CHAT_ITEM_TEXT_PADDING, top = TERMINAL_EVENT_TOP_GAP.toDp(), end = CHAT_ITEM_TEXT_PADDING, bottom = TERMINAL_EVENT_BOTTOM_GAP.toDp())
  else PaddingValues(horizontal = 6.dp, vertical = vertical)

@Composable
fun ChatEventWithMeta(ci: ChatItem, content: @Composable (separator: AnnotatedString, ts: AnnotatedString) -> Unit) {
  ItemWithMeta(meta = { CIMetaView(ci, timedMessagesTTL = null, showStatus = false, showEdited = false, showViaProxy = false, showTimestamp = true) }, metaReserved = true) { inlineMeta ->
    if (inlineMeta != null) {
      content(AnnotatedString("  "), AnnotatedString(ci.timestampText))
    } else {
      val copy = terminalReserveSpaceForMeta(ci.meta, chatTTL = null, encrypted = null, signedFileVerified = null, secondaryColor = MaterialTheme.colors.secondary, showStatus = false, showEdited = false, showViaProxy = false, showTimestamp = true)
      content(
        buildAnnotatedString { withStyle(chatEventStyle) { withStyle(reserveTimestampStyle) { append("\u00A0\u00A0\u00A0") } } },
        buildAnnotatedString { withStyle(reserveTimestampStyle) { append(copy) } }
      )
    }
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
