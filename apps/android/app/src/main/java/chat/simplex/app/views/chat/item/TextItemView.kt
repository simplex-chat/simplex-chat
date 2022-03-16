package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
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
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.datetime.Clock

val reserveTimestampStyle = SpanStyle(color = Color.Transparent)
val boldFont = SpanStyle(fontWeight = FontWeight.Medium)

fun appendGroupMember(b: AnnotatedString.Builder, chatItem: ChatItem, groupMemberBold: Boolean) {
  if (chatItem.chatDir is CIDirection.GroupRcv) {
    val name = chatItem.chatDir.groupMember.memberProfile.displayName
    if (groupMemberBold) b.withStyle(boldFont) { append(name) }
    else b.append(name)
    b.append(": ")
  }
}

fun appendSender(b: AnnotatedString.Builder, sender: String?, senderBold: Boolean) {
  if (sender != null) {
    if (senderBold) b.withStyle(boldFont) { append(sender) }
    else b.append(sender)
    b.append(": ")
  }
}

@Composable
fun MarkdownText (
  content: ItemContent,
  formattedText: List<FormattedText>? = null,
  sender: String? = null,
  metaText: String? = null,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  senderBold: Boolean = false,
  modifier: Modifier = Modifier
) {
  if (formattedText == null) {
    val annotatedText = buildAnnotatedString {
      appendSender(this, sender, senderBold)
//      appendGroupMember(this, chatItem, senderBold)
      append(content.text)
      if (metaText != null) withStyle(reserveTimestampStyle) { append("  $metaText") }
        // {chatItem.timestampText}
    }
    Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
  } else {
    var hasLinks = false
    val annotatedText = buildAnnotatedString {
      appendSender(this, sender, senderBold)
//      appendGroupMember(this, chatItem, senderBold)
      for (ft in formattedText) {
        if (ft.format == null) append(ft.text)
        else {
          val link = ft.link
          if (link != null) {
            hasLinks = true
            withAnnotation(tag = "URL", annotation = link) {
              withStyle(ft.format.style) { append(ft.text) }
            }
          } else {
            withStyle(ft.format.style) { append(ft.text) }
          }
        }
      }
      if (metaText != null) withStyle(reserveTimestampStyle) { append("  $metaText") }
      // {chatItem.timestampText}
    }
    if (hasLinks && uriHandler != null) {
      ClickableText(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow,
        onClick = { offset ->
          annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
            .firstOrNull()?.let { annotation -> uriHandler.openUri(annotation.item) }
        }
      )
    } else {
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
    }
  }
}
