package chat.simplex.common.views.chat.item

import androidx.compose.foundation.text.BasicText
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.*
import java.awt.*

val reserveTimestampStyle = SpanStyle(color = Color.Transparent)
val boldFont = SpanStyle(fontWeight = FontWeight.Medium)

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
  toggleSecrets: Boolean,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  senderBold: Boolean = false,
  modifier: Modifier = Modifier,
  linkMode: SimplexLinkMode,
  inlineContent: Pair<AnnotatedString.Builder.() -> Unit, Map<String, InlineTextContent>>? = null,
  onLinkLongClick: (link: String) -> Unit = {},
  showViaProxy: Boolean = false
) {
  val textLayoutDirection = remember (text) {
    if (isRtl(text.subSequence(0, kotlin.math.min(50, text.length)))) LayoutDirection.Rtl else LayoutDirection.Ltr
  }
  val reserve = if (textLayoutDirection != LocalLayoutDirection.current && meta != null) {
    "\n"
  } else if (meta != null) {
    reserveSpaceForMeta(meta, chatTTL, null, secondaryColor = MaterialTheme.colors.secondary, showViaProxy = showViaProxy) // LALAL
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
    val showSecrets = remember { mutableStateMapOf<String, Boolean>() }
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
        inlineContent?.first?.invoke(this)
        appendSender(this, sender, senderBold)
        if (text is String) append(text)
        else if (text is AnnotatedString) append(text)
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent?.second ?: mapOf())
    } else {
      var hasAnnotations = false
      val annotatedText = buildAnnotatedString {
        inlineContent?.first?.invoke(this)
        appendSender(this, sender, senderBold)
        for ((i, ft) in formattedText.withIndex()) {
          if (ft.format == null) append(ft.text)
          else if (toggleSecrets && ft.format is Format.Secret) {
            val ftStyle = ft.format.style
            hasAnnotations = true
            val key = i.toString()
            withAnnotation(tag = "SECRET", annotation = key) {
              if (showSecrets[key] == true) append(ft.text) else withStyle(ftStyle) { append(ft.text) }
            }
          } else {
            val link = ft.link(linkMode)
            if (link != null) {
              hasAnnotations = true
              val ftStyle = ft.format.style
              withAnnotation(tag = if (ft.format is Format.SimplexLink) "SIMPLEX_URL" else "URL", annotation = link) {
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
      if (hasAnnotations && uriHandler != null) {
        val icon = remember { mutableStateOf(PointerIcon.Default) }
        ClickableText(annotatedText, style = style, modifier = modifier.pointerHoverIcon(icon.value), maxLines = maxLines, overflow = overflow,
          onLongClick = { offset ->
            annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
              .firstOrNull()?.let { annotation -> onLinkLongClick(annotation.item) }
            annotatedText.getStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset)
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
            annotatedText.getStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset)
              .firstOrNull()?.let { annotation ->
                uriHandler.openVerifiedSimplexUri(annotation.item)
              }
            annotatedText.getStringAnnotations(tag = "SECRET", start = offset, end = offset)
              .firstOrNull()?.let { annotation ->
                val key = annotation.item
                showSecrets[key] = !(showSecrets[key] ?: false)
              }
          },
          onHover = { offset ->
            icon.value = annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset)
              .firstOrNull()?.let {
                PointerIcon.Hand
              } ?: annotatedText.getStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset)
              .firstOrNull()?.let {
                PointerIcon.Hand
              } ?: annotatedText.getStringAnnotations(tag = "SECRET", start = offset, end = offset)
              .firstOrNull()?.let {
                PointerIcon.Hand
              } ?: PointerIcon.Default
          },
          shouldConsumeEvent = { offset ->
            annotatedText.getStringAnnotations(tag = "URL", start = offset, end = offset).any()
            annotatedText.getStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset).any()
          }
        )
      } else {
        Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent?.second ?: mapOf())
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
  onHover: (Int) -> Unit = {},
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
  }.pointerInput(onHover) {
    if (appPlatform.isDesktop) {
      detectCursorMove { pos ->
        layoutResult.value?.let { layoutResult ->
          onHover(layoutResult.getOffsetForPosition(pos))
        }
      }
    }
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

private fun isRtl(s: CharSequence): Boolean {
  for (element in s) {
    val d = Character.getDirectionality(element)
    if (d == Character.DIRECTIONALITY_RIGHT_TO_LEFT || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING || d == Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE) {
      return true
    }
  }
  return false
}
