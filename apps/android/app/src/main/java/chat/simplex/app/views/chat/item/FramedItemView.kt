package chat.simplex.app.views.chat.item

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
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
val ReceivedColorLight = Color(0x20B1B0B5)
val SentQuoteColorLight = Color(0x2545B8FF)
val ReceivedQuoteColorLight = Color(0x25B1B0B5)

@Composable
fun FramedItemView(user: User, ci: ChatItem, uriHandler: UriHandler? = null) {
  val sent = ci.chatDir.sent
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (sent) SentColorLight else ReceivedColorLight
  ) {
    Box(contentAlignment = Alignment.BottomEnd) {
      Column(Modifier.width(IntrinsicSize.Max)) {
        val qi = ci.quotedItem
        if (qi != null) {
          Box(
            Modifier
              .background(if (sent) SentQuoteColorLight else ReceivedQuoteColorLight)
              .padding(vertical = 6.dp, horizontal = 12.dp)
              .fillMaxWidth()
          ) {
            MarkdownText(
              qi, sender = qi.sender(user), senderBold = true, maxLines = 3,
              style = TextStyle(fontSize = 15.sp, color = MaterialTheme.colors.onSurface)
            )
          }
        }
        Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
          if (ci.formattedText == null && isShortEmoji(ci.content.text)) {
            Column(
              Modifier.padding(bottom = 2.dp).fillMaxWidth(),
              horizontalAlignment = Alignment.CenterHorizontally
            ) {
              EmojiText(ci.content.text)
              Text("")
            }
          } else {
            MarkdownText(
              ci.content, ci.formattedText, ci.memberDisplayName,
              metaText = ci.timestampText, uriHandler = uriHandler, senderBold = true
            )
          }
        }
      }
      Box(Modifier.padding(bottom = 6.dp, end = 12.dp)) {
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

@Preview
@Composable
fun PreviewTextItemViewQuote() {
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "https://simplex.chat",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "hi", chatDir = CIDirection.DirectRcv())
      )
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewEmoji() {
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "👍",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "Lorem ipsum dolor sit amet", chatDir = CIDirection.DirectRcv())
      )
    )
  }
}
