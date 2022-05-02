package chat.simplex.app.views.chat.item

import CIFileView
import CIImageView
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.ChatItemLinkView
import chat.simplex.app.views.helpers.base64ToBitmap
import kotlinx.datetime.Clock

val SentColorLight = Color(0x1E45B8FF)
val ReceivedColorLight = Color(0x20B1B0B5)
val SentQuoteColorLight = Color(0x2545B8FF)
val ReceivedQuoteColorLight = Color(0x25B1B0B5)

@Composable
fun FramedItemView(
  user: User,
  ci: ChatItem,
  uriHandler: UriHandler? = null,
  showMember: Boolean = false,
  showMenu: MutableState<Boolean>,
  receiveFile: (Long) -> Unit
) {
  val sent = ci.chatDir.sent
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (sent) SentColorLight else ReceivedColorLight
  ) {
    var metaColor = HighOrLowlight
    Box(contentAlignment = Alignment.BottomEnd) {
      Column(Modifier.width(IntrinsicSize.Max)) {
        val qi = ci.quotedItem
        if (qi != null) {
          Row(
            Modifier
              .background(if (sent) SentQuoteColorLight else ReceivedQuoteColorLight)
              .fillMaxWidth()
          ) {
            Box(
              Modifier.padding(vertical = 6.dp, horizontal = 12.dp),
              contentAlignment = Alignment.TopStart
            ) {
              MarkdownText(
                qi.text, sender = qi.sender(user), senderBold = true, maxLines = 3,
                style = TextStyle(fontSize = 15.sp, color = MaterialTheme.colors.onSurface)
              )
            }
            Spacer(Modifier.weight(1f))
            if (qi.content is MsgContent.MCImage) {
              val imageBitmap = base64ToBitmap(qi.content.image).asImageBitmap()
              Image(
                imageBitmap,
                contentDescription = stringResource(R.string.image_descr),
                contentScale = ContentScale.Crop,
                modifier = Modifier
                  .size(60.dp)
                  .clipToBounds()
              )
            }
          }
        }
        if (ci.file == null && ci.formattedText == null && isShortEmoji(ci.content.text)) {
          Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
            Column(
              Modifier
                .padding(bottom = 2.dp)
                .fillMaxWidth(),
              horizontalAlignment = Alignment.CenterHorizontally
            ) {
              EmojiText(ci.content.text)
              Text("")
            }
          }
        } else {
          Column(Modifier.fillMaxWidth()) {
            when (val mc = ci.content.msgContent) {
              is MsgContent.MCImage -> {
                CIImageView(image = mc.image, file = ci.file, showMenu)
                if (mc.text == "") {
                  metaColor = Color.White
                } else {
                  CIMarkdownText(ci, showMember, uriHandler)
                }
              }
              is MsgContent.MCFile -> {
                CIFileView(ci.file, ci.meta.itemEdited, receiveFile)
                if (mc.text != "") {
                  CIMarkdownText(ci, showMember, uriHandler)
                }
              }
              is MsgContent.MCLink -> {
                ChatItemLinkView(mc.preview)
                CIMarkdownText(ci, showMember, uriHandler)
              }
              else -> CIMarkdownText(ci, showMember, uriHandler)
            }
          }
        }
      }
      Box(Modifier.padding(bottom = 6.dp, end = 12.dp)) {
        CIMetaView(ci, metaColor)
      }
    }
  }
}

@Composable
fun CIMarkdownText(ci: ChatItem, showMember: Boolean, uriHandler: UriHandler?) {
  Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
    MarkdownText(
      ci.content.text, ci.formattedText, if (showMember) ci.memberDisplayName else null,
      metaText = ci.timestampText, edited = ci.meta.itemEdited, uriHandler = uriHandler, senderBold = true
    )
  }
}

class EditedProvider: PreviewParameterProvider<Boolean> {
  override val values = listOf(false, true).asSequence()
}

@Preview
@Composable
fun PreviewTextItemViewSnd(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello", itemEdited = edited,
      ),
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewRcv(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectRcv(), Clock.System.now(), "hello", itemEdited = edited
      ),
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewLong(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1,
        CIDirection.DirectSnd(),
        Clock.System.now(),
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        itemEdited = edited
      ),
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewQuote(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "https://simplex.chat",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "hi", chatDir = CIDirection.DirectRcv()),
        itemEdited = edited
      ),
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewEmoji(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "👍",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "Lorem ipsum dolor sit amet", chatDir = CIDirection.DirectRcv()),
        itemEdited = edited
      ),
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}
