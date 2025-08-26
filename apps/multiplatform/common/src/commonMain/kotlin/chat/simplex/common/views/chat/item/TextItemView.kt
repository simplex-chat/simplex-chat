package chat.simplex.common.views.chat.item

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
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
import androidx.compose.ui.text.AnnotatedString.Range
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import kotlinx.coroutines.*

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
  mentions: Map<String, CIMention>? = null,
  userMemberId: String? = null,
  toggleSecrets: Boolean,
  sendCommandMsg: ((String) -> Unit)? = null,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp),
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  uriHandler: UriHandler? = null,
  senderBold: Boolean = false,
  modifier: Modifier = Modifier,
  linkMode: SimplexLinkMode,
  inlineContent: Pair<AnnotatedString.Builder.() -> Unit, Map<String, InlineTextContent>>? = null,
  onLinkLongClick: (link: String) -> Unit = {},
  showViaProxy: Boolean = false,
  showTimestamp: Boolean = true,
  prefix: AnnotatedString? = null
) {
  val textLayoutDirection = remember (text) {
    if (isRtl(text.subSequence(0, kotlin.math.min(50, text.length)))) LayoutDirection.Rtl else LayoutDirection.Ltr
  }
  val reserve = if (textLayoutDirection != LocalLayoutDirection.current && meta != null) {
    "\n"
  } else if (meta != null) {
    reserveSpaceForMeta(meta, chatTTL, null, secondaryColor = MaterialTheme.colors.secondary, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
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
        if (prefix != null) append(prefix)
        if (text is String) append(text)
        else if (text is AnnotatedString) append(text)
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent?.second ?: mapOf())
    } else {
      var hasLinks = false
      var hasSecrets = false
      var hasCommands = false
      val annotatedText = buildAnnotatedString {
        inlineContent?.first?.invoke(this)
        appendSender(this, sender, senderBold)
        if (prefix != null) append(prefix)
        for ((i, ft) in formattedText.withIndex()) {
          if (ft.format == null) append(ft.text)
          else when(ft.format) {
            is Format.Bold -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Italic -> withStyle(ft.format.style) { append(ft.text) }
            is Format.StrikeThrough -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Snippet -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Colored -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Secret -> {
              val ftStyle = ft.format.style
              if (toggleSecrets) {
                hasSecrets = true
                val key = i.toString()
                withAnnotation(tag = "SECRET", annotation = key) {
                  if (showSecrets[key] == true) append(ft.text) else withStyle(ftStyle) { append(ft.text) }
                }
              } else {
                withStyle(ftStyle) { append(ft.text) }
              }
            }
            is Format.Mention -> {
              val mention = mentions?.get(ft.format.memberName)
              if (mention != null) {
                val ftStyle = ft.format.style
                if (mention.memberRef != null) {
                  val displayName = mention.memberRef.displayName
                  val name = if (mention.memberRef.localAlias.isNullOrEmpty()) {
                    displayName
                  } else {
                    "${mention.memberRef.localAlias} ($displayName)"
                  }
                  val mentionStyle = if (mention.memberId == userMemberId) ftStyle.copy(color = MaterialTheme.colors.primary) else ftStyle
                  withStyle(mentionStyle) { append(mentionText(name)) }
                } else {
                  withStyle(ftStyle) { append(mentionText(ft.format.memberName)) }
                }
              } else {
                append(ft.text)
              }
            }
            is Format.Command ->
              if (sendCommandMsg == null) {
                append(ft.text)
              } else {
                hasCommands = true
                val ftStyle = ft.format.style
                val cmd = ft.format.commandStr
                withAnnotation(tag = "COMMAND", annotation = cmd) {
                  withStyle(ftStyle) { append("/$cmd") }
                }
              }
            is Format.Uri -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              val s = ft.text
              val link = if (s.startsWith("http://") || s.startsWith("https://")) s else "https://$s"
              withAnnotation(tag = "WEB_URL", annotation = link) {
                withStyle(ftStyle) { append(ft.text) }
              }
            }
            is Format.HyperLink -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = "WEB_URL", annotation = ft.format.linkUri) {
                withStyle(ftStyle) { append(ft.format.showText ?: ft.text) }
              }
            }
            is Format.SimplexLink -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              val link =
                if (linkMode == SimplexLinkMode.BROWSER && ft.format.showText == null && !ft.text.startsWith("[")) ft.text
                else ft.format.simplexUri
              val t = ft.format.showText ?: if (linkMode == SimplexLinkMode.DESCRIPTION) ft.format.linkType.description else null
              withAnnotation(tag = "SIMPLEX_URL", annotation = link) {
                if (t == null) {
                  withStyle(ftStyle) { append(ft.text) }
                } else {
                  withStyle(ftStyle) { append("$t ") }
                  withStyle(ftStyle.copy(fontStyle = FontStyle.Italic)) { append(ft.format.viaHosts) }
                }
              }
            }
            is Format.Email -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = "OTHER_URL", annotation = "mailto:${ft.text}") {
                withStyle(ftStyle) { append(ft.text) }
              }
            }
            is Format.Phone -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = "OTHER_URL", annotation = "tel:${ft.text}") {
                withStyle(ftStyle) { append(ft.text) }
              }
            }
            is Format.Unknown -> append(ft.text)
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
      if ((hasLinks && uriHandler != null) || hasSecrets || (hasCommands && sendCommandMsg != null)) {
        val icon = remember { mutableStateOf(PointerIcon.Default) }
        ClickableText(annotatedText, style = style, modifier = modifier.pointerHoverIcon(icon.value), maxLines = maxLines, overflow = overflow,
          onLongClick = { offset ->
            if (hasLinks) {
              val withAnnotation: (String, (Range<String>) -> Unit) -> Unit = { tag, f ->
                annotatedText.getStringAnnotations(tag, start = offset, end = offset).firstOrNull()?.let(f)
              }
              withAnnotation("WEB_URL") { a -> onLinkLongClick(a.item) }
              withAnnotation("SIMPLEX_URL") { a -> onLinkLongClick(a.item) }
              withAnnotation("OTHER_URL") { a -> onLinkLongClick(a.item) }
            }
          },
          onClick = { offset ->
            val withAnnotation: (String, (Range<String>) -> Unit) -> Unit = { tag, f ->
              annotatedText.getStringAnnotations(tag, start = offset, end = offset).firstOrNull()?.let(f)
            }
            if (hasLinks && uriHandler != null) {
              withAnnotation("WEB_URL") { a -> openBrowserAlert(a.item, uriHandler) }
              withAnnotation("OTHER_URL") { a -> safeOpenUri(a.item, uriHandler) }
              withAnnotation("SIMPLEX_URL") { a -> uriHandler.openVerifiedSimplexUri(a.item) }
            }
            if (hasSecrets) {
              withAnnotation("SECRET") { a ->
                val key = a.item
                showSecrets[key] = !(showSecrets[key] ?: false)
              }
            }
            if (hasCommands && sendCommandMsg != null) {
              withAnnotation("COMMAND") { a -> sendCommandMsg("/${a.item}") }
            }
          },
          onHover = { offset ->
            val hasAnnotation: (String) -> Boolean = { tag -> annotatedText.hasStringAnnotations(tag, start = offset, end = offset) }
            icon.value =
              if (hasAnnotation("WEB_URL") || hasAnnotation("SIMPLEX_URL") || hasAnnotation("OTHER_URL") || hasAnnotation("SECRET") || hasAnnotation("COMMAND")) {
                PointerIcon.Hand
              } else {
                PointerIcon.Default
              }
          },
          shouldConsumeEvent = { offset ->
            annotatedText.hasStringAnnotations(tag = "WEB_URL", start = offset, end = offset)
                || annotatedText.hasStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset)
                || annotatedText.hasStringAnnotations(tag = "OTHER_URL", start = offset, end = offset)
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

fun openBrowserAlert(uri: String, uriHandler: UriHandler) {
  val (res, err) = sanitizeUri(uri)
  if (res == null) {
    showInvalidLinkAlert(uri, err)
  } else {
    val message = if (uri.count() > 160) uri.substring(0, 159) + "…" else uri
    val sanitizedUri = res.second
    if (sanitizedUri == null) {
      AlertManager.shared.showAlertDialog(
        generalGetString(MR.strings.privacy_chat_list_open_web_link_question),
        message,
        confirmText = generalGetString(MR.strings.open_verb),
        onConfirm = { safeOpenUri(uri, uriHandler) }
      )
    } else {
      AlertManager.shared.showAlertDialogButtonsColumn(
        generalGetString(MR.strings.privacy_chat_list_open_web_link_question),
        message,
        buttons = {
        Column {
          SectionItemView({
            AlertManager.shared.hideAlert()
            safeOpenUri(uri, uriHandler)
          }) {
            Text(generalGetString(MR.strings.privacy_chat_list_open_full_web_link), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
          SectionItemView({
            AlertManager.shared.hideAlert()
            safeOpenUri(sanitizedUri, uriHandler)
          }) {
            Text(generalGetString(MR.strings.privacy_chat_list_open_clean_web_link), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
          SectionItemView({
            AlertManager.shared.hideAlert()
          }) {
            Text(generalGetString(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
        }
      })
    }
  }
}

fun safeOpenUri(uri: String, uriHandler: UriHandler) {
  try {
    uriHandler.openUri(uri)
  } catch (e: Exception) {
    // It can happen, for example, when you click on a text 0.00001 but don't have any app that can catch
    // `tel:` scheme in url installed on a device (no phone app or contacts, maybe)
    Log.e(TAG, "Open url: ${e.stackTraceToString()}")
    showInvalidLinkAlert(uri, error = e.message)
  }
}

fun showInvalidLinkAlert(uri: String, error: String? = null) {
  val message = if (error.isNullOrEmpty()) { uri } else { error + "\n" + uri }
  AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_parsing_uri_title), message)
}

fun sanitizeUri(s: String): Pair<Pair<Boolean, String?>?, String?> {
  val parsed = parseSanitizeUri(s, safe = false)
  return if (parsed?.uriInfo != null) {
    (true to parsed.uriInfo.sanitized) to null
  } else {
    null to parsed?.parseError
  }
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

private fun mentionText(name: String): String = if (name.contains(" @"))  "@'$name'" else "@$name"
