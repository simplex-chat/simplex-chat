package chat.simplex.app.views.chat.item

import androidx.compose.foundation.text.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.detectGesture

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
  text: String,
  formattedText: List<FormattedText>? = null,
  sender: String? = null,
  metaText: String? = null,
  edited: Boolean = false,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  senderBold: Boolean = false,
  modifier: Modifier = Modifier,
  onLinkLongClick: (link: String) -> Unit = {}
) {
  val reserve = if (edited) "        " else "    "
  if (formattedText == null) {
    val annotatedText = buildAnnotatedString {
      appendSender(this, sender, senderBold)
      append(text)
      if (metaText != null) withStyle(reserveTimestampStyle) { append(reserve + metaText) }
    }
    Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
  } else {
    var hasLinks = false
    val annotatedText = buildAnnotatedString {
      appendSender(this, sender, senderBold)
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
      if (metaText != null) withStyle(reserveTimestampStyle) { append(reserve + metaText) }
    }
    if (hasLinks && uriHandler != null) {
      ClickableText(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow,
        onLongClick = { offset ->
          annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
            .firstOrNull()?.let { annotation -> onLinkLongClick(annotation.item) }
        },
        onClick = { offset ->
          annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
            .firstOrNull()?.let { annotation -> uriHandler.openUri(annotation.item) }
        },
        shouldConsumeEvent = { offset ->
          annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset).any()
        }
      )
    } else {
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow)
    }
  }
}

@Composable
fun ClickableText(
  text: AnnotatedString,
  modifier: Modifier = Modifier,
  style: TextStyle = TextStyle.Default,
  softWrap: Boolean = true,
  overflow: TextOverflow = TextOverflow.Clip,
  maxLines: Int = Int.MAX_VALUE,
  onTextLayout: (TextLayoutResult) -> Unit = {},
  onClick: (Int) -> Unit,
  onLongClick: (Int) -> Unit = {},
  shouldConsumeEvent: (Int) -> Boolean
) {
  val layoutResult = remember { mutableStateOf<TextLayoutResult?>(null) }
  val pressIndicator = Modifier.pointerInput(onClick, onLongClick) {
    detectGesture(onLongPress = { pos ->
      layoutResult.value?.let { layoutResult ->
        onLongClick(layoutResult.getOffsetForPosition(pos))
      }
    }, onPress = { pos ->
      layoutResult.value?.let { layoutResult ->
        val res  = tryAwaitRelease()
        if (res) {
          onClick(layoutResult.getOffsetForPosition(pos))
        }
      }
    }, shouldConsumeEvent = { pos ->
      var consume = false
        layoutResult.value?.let { layoutResult ->
          consume = shouldConsumeEvent(layoutResult.getOffsetForPosition(pos))
        }
      consume
      }
    )
  }

  BasicText(
    text = text,
    modifier = modifier.then(pressIndicator),
    style = style,
    softWrap = softWrap,
    overflow = overflow,
    maxLines = maxLines,
    onTextLayout = {
      layoutResult.value = it
      onTextLayout(it)
    }
  )
}