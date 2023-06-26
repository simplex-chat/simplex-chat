package chat.simplex.common.views.chat.item

import androidx.compose.foundation.text.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.helpers.DisposableEffectOnGone
import chat.simplex.common.views.helpers.detectGesture
import kotlinx.coroutines.*

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

private val noTyping: AnnotatedString = AnnotatedString("   ")

private val typingIndicators: List<AnnotatedString> = listOf(
  typing(FontWeight.Black) + typing() + typing(),
  typing(FontWeight.Bold) + typing(FontWeight.Black) + typing(),
  typing() + typing(FontWeight.Bold) + typing(FontWeight.Black),
  typing() + typing() + typing(FontWeight.Bold)
)


private fun typingIndicator(recent: Boolean, typingIdx: Int): AnnotatedString = buildAnnotatedString {
  pushStyle(SpanStyle(color = CurrentColors.value.colors.secondary, fontFamily = FontFamily.Monospace, letterSpacing = (-1).sp))
  append(if (recent) typingIndicators[typingIdx] else noTyping)
}

private fun typing(w: FontWeight = FontWeight.Light): AnnotatedString =
  AnnotatedString(".", SpanStyle(fontWeight = w))

@Composable
fun MarkdownText (
  text: CharSequence,
  formattedText: List<FormattedText>? = null,
  sender: String? = null,
  meta: CIMeta? = null,
  chatTTL: Int? = null,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  senderBold: Boolean = false,
  modifier: Modifier = Modifier,
  linkMode: SimplexLinkMode,
  inlineContent: Map<String, InlineTextContent>? = null,
  onLinkLongClick: (link: String) -> Unit = {}
) {
  val textLayoutDirection = remember (text) {
    if (isRrl(text.subSequence(0, kotlin.math.min(50, text.length)))) LayoutDirection.Rtl else LayoutDirection.Ltr
  }
  val reserve = if (textLayoutDirection != LocalLayoutDirection.current && meta != null) {
    "\n"
  } else if (meta != null) {
    reserveSpaceForMeta(meta, chatTTL)
  } else {
    "    "
  }
  val scope = rememberCoroutineScope()
  CompositionLocalProvider(
    LocalLayoutDirection provides if (textLayoutDirection != LocalLayoutDirection.current)
      if (LocalLayoutDirection.current == LayoutDirection.Ltr) LayoutDirection.Rtl else LayoutDirection.Ltr
    else
      LocalLayoutDirection.current
  ) {
    var timer: Job? by remember { mutableStateOf(null) }
    var typingIdx by rememberSaveable { mutableStateOf(0) }
    fun stopTyping() {
      timer?.cancel()
      timer = null
    }
    fun switchTyping() {
      if (meta != null && meta.isLive && meta.recent) {
        timer = timer ?: scope.launch {
          while (isActive) {
            typingIdx = (typingIdx + 1) % typingIndicators.size
            delay(250)
          }
        }
      } else {
        stopTyping()
      }
    }
    if (meta?.isLive == true) {
      LaunchedEffect(meta.recent, meta.isLive) {
        switchTyping()
      }
      DisposableEffectOnGone(
        whenGone = {
          stopTyping()
        }
      )
    }
    if (formattedText == null) {
      val annotatedText = buildAnnotatedString {
        appendSender(this, sender, senderBold)
        if (text is String) append(text)
        else if (text is AnnotatedString) append(text)
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent ?: mapOf())
    } else {
      var hasLinks = false
      val annotatedText = buildAnnotatedString {
        appendSender(this, sender, senderBold)
        for (ft in formattedText) {
          if (ft.format == null) append(ft.text)
          else {
            val link = ft.link(linkMode)
            if (link != null) {
              hasLinks = true
              val ftStyle = if (ft.format is Format.SimplexLink && !ft.format.trustedUri && linkMode == SimplexLinkMode.BROWSER) {
                SpanStyle(color = Color.Red, textDecoration = TextDecoration.Underline)
              } else {
                ft.format.style
              }
              withAnnotation(tag = "URL", annotation = link) {
                withStyle(ftStyle) { append(ft.viewText(linkMode)) }
              }
            } else {
              withStyle(ft.format.style) { append(ft.text) }
            }
          }
        }
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        // With RTL language set globally links looks bad sometimes, better to add a new line to bo sure everything looks good
        /*if (metaText != null && hasLinks && LocalLayoutDirection.current == LayoutDirection.Rtl)
          withStyle(reserveTimestampStyle) { append("\n" + metaText) }
        else */if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      if (hasLinks && uriHandler != null) {
        ClickableText(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow,
          onLongClick = { offset ->
            annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
              .firstOrNull()?.let { annotation -> onLinkLongClick(annotation.item) }
          },
          onClick = { offset ->
            annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
              .firstOrNull()?.let { annotation ->
                try {
                  uriHandler.openUri(annotation.item)
                } catch (e: Exception) {
                  // It can happen, for example, when you click on a text 0.00001 but don't have any app that can catch
                  // `tel:` scheme in url installed on a device (no phone app or contacts, maybe)
                  Log.e(TAG, "Open url: ${e.stackTraceToString()}")
                }
              }
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

private fun isRrl(s: CharSequence): Boolean {
  for (element in s) {
    val d = Character.getDirectionality(element)
    if (d == Character.DIRECTIONALITY_RIGHT_TO_LEFT || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE) {
      return true
    }
  }
  return false
}