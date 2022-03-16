package chat.simplex.app.views.chat.item

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.Clock

val SentColorLight = Color(0x1E45B8FF)
val ReceivedColorLight = Color(0x1EB1B0B5)
val SentQuoteColorLight = Color(0x1E45B8FF)
//val SentQuoteColorDark = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.09)

@Composable
fun FramedItemView(user: User, ci: ChatItem, uriHandler: UriHandler? = null) {
  val sent = ci.chatDir.sent
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (sent) SentColorLight else ReceivedColorLight
  ) {
    Box(
      modifier = Modifier.padding(vertical = 6.dp, horizontal = 12.dp)
    ) {
      Box(contentAlignment = Alignment.BottomEnd) {
        Column {
          val qi = ci.quotedItem
          if (qi != null) {
            Box(Modifier.background(if (sent) SentColorLight else ReceivedColorLight)) {
              MarkdownText(
                qi, sender = qi.sender(user), senderBold = true, maxLines = 3,
                style = TextStyle(fontSize = 15.sp, color = MaterialTheme.colors.onSurface)
              )
            }
          }
          MarkdownText(
            ci.content, ci.formattedText, ci.memberDisplayName,
            metaText = ci.timestampText, uriHandler = uriHandler, senderBold = true
          )
        }
        CIMetaView(ci)
      }
    }
  }
}

@Preview
@Composable
fun PreviewTextItemViewSnd() {
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      )
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewRcv() {
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      )
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewLong() {
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1,
        CIDirection.DirectSnd(),
        Clock.System.now(),
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    )
  }
}
