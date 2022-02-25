package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.ClickableText
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.LightGray
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.Clock

// TODO move to theme
val SentColorLight = Color(0x1E45B8FF)
val ReceivedColorLight = Color(0x1EB1B0B5)

@ExperimentalTextApi
@Composable
fun TextItemView(chatItem: ChatItem, uriHandler: UriHandler? = null) {
  val sent = chatItem.chatDir.sent
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (sent) SentColorLight else ReceivedColorLight
  ) {
    Box(
      modifier = Modifier.padding(vertical = 6.dp, horizontal = 12.dp)
    ) {
      Box(contentAlignment = Alignment.BottomEnd) {
        MarkdownText(chatItem, uriHandler = uriHandler, groupMemberBold = true)
        CIMetaView(chatItem)
      }
    }
  }
}

val reserveTimestampStyle = SpanStyle(color = Color.Transparent)
val boldFont = SpanStyle(fontWeight = FontWeight.Bold)

fun appendGroupMember(b: AnnotatedString.Builder, chatItem: ChatItem, groupMemberBold: Boolean) {
  if (chatItem.chatDir is CIDirection.GroupRcv) {
    val name = chatItem.chatDir.groupMember.memberProfile.displayName
    if (groupMemberBold) b.withStyle(boldFont) { append(name) }
    else b.append(name)
    b.append(": ")
  }
}

@ExperimentalTextApi
@Composable
fun MarkdownText (
  chatItem: ChatItem,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  groupMemberBold: Boolean = false,
  modifier: Modifier = Modifier
) {
  if (chatItem.formattedText == null) {
    val annotatedText = buildAnnotatedString {
      appendGroupMember(this, chatItem, groupMemberBold)
      append(chatItem.content.text)
      withStyle(reserveTimestampStyle) { append("  ${chatItem.timestampText}") }
    }
    SelectionContainer {
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
    }
  } else {
    val annotatedText = buildAnnotatedString {
      appendGroupMember(this, chatItem, groupMemberBold)
      for (ft in chatItem.formattedText) {
        if (ft.format == null) append(ft.text)
        else {
          val link = ft.link
          if (link != null) {
            withAnnotation(tag = "URL", annotation = link) {
              withStyle(ft.format.style) { append(ft.text) }
            }
          } else {
            withStyle(ft.format.style) { append(ft.text) }
          }
        }
      }
      withStyle(reserveTimestampStyle) { append("  ${chatItem.timestampText}") }
    }
    if (uriHandler != null) {
      SelectionContainer {
        ClickableText(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow,
          onClick = { offset ->
            annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
              .firstOrNull()?.let { annotation -> uriHandler.openUri(annotation.item) }
          }
        )
      }
    } else {
      SelectionContainer {
        Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
      }
    }
  }
}

@ExperimentalTextApi
@Preview
@Composable
fun PreviewTextItemViewSnd() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      )
    )
  }
}

@ExperimentalTextApi
@Preview
@Composable
fun PreviewTextItemViewRcv() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      )
    )
  }
}

@ExperimentalTextApi
@Preview
@Composable
fun PreviewTextItemViewLong() {
  SimpleXTheme {
    TextItemView(
      chatItem = ChatItem.getSampleData(
        1,
        CIDirection.DirectSnd(),
        Clock.System.now(),
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    )
  }
}
