package chat.simplex.common.views.chat.item

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.text.BasicText
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.draw.drawWithContent
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ColorFilter
import androidx.compose.ui.graphics.ColorMatrix
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.ImageShader
import androidx.compose.ui.graphics.ShaderBrush
import androidx.compose.ui.graphics.TileMode
import androidx.compose.ui.graphics.drawscope.ContentDrawScope
import androidx.compose.ui.graphics.drawscope.clipPath
import androidx.compose.ui.graphics.drawscope.rotate
import androidx.compose.ui.graphics.drawscope.scale
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.AnnotatedString.Range
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.appColors
import chat.simplex.common.views.chat.ItemSelection
import chat.simplex.common.views.chat.SelectionHighlightColor
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import kotlinx.coroutines.*

val reserveTimestampStyle = SpanStyle(color = Color.Transparent)
val boldFont = SpanStyle(fontWeight = FontWeight.Medium)

@Composable
fun messageTextColor(): Color = MaterialTheme.colors.onSurface.copy(alpha = if (MaterialTheme.colors.isLight) 1f else 0.87f)

fun appendSender(b: AnnotatedString.Builder, sender: String?, senderBold: Boolean) {
  if (sender != null) {
    if (senderBold) b.withStyle(boldFont) { append(sender) }
    else b.append(sender)
    b.append(": ")
  }
}

private fun openMarkdownModal(modal: Format.Modal) {
  when (modal.modalName) {
    Format.Modal.Description -> showFullProfileDescription(modal.text)
  }
}

private fun showFullProfileDescription(description: String) {
  if (ModalManager.end.isTopModalOpen(ModalViewId.PROFILE_DESCRIPTION)) ModalManager.end.closeModal()
  ModalManager.end.showModalCloseable(id = ModalViewId.PROFILE_DESCRIPTION) { _ ->
    key(this) {
      ColumnWithScrollBar {
        AppBarTitle(generalGetString(MR.strings.profile_description__field))
        val parsed = parseToMarkdownText(description)
        MarkdownText(
          description,
          parsed?.formattedText,
          parsed?.markdownBlocks,
          toggleSecrets = true,
          style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground, lineHeight = 22.sp),
          uriHandler = LocalUriHandler.current,
          linkMode = chatModel.simplexLinkMode.value,
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING),
          blockLayout = true,
        )
      }
    }
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

// Display text for a single formatted segment — must be coordinated with MarkdownText.
fun segmentDisplayText(ft: FormattedText, mentions: Map<String, CIMention>?, linkMode: SimplexLinkMode): String =
  when (ft.format) {
    is Format.Mention -> {
      val mention = mentions?.get(ft.format.memberName)
      if (mention?.memberRef != null) {
        val name = if (mention.memberRef.localAlias.isNullOrEmpty()) mention.memberRef.displayName
          else "${mention.memberRef.localAlias} (${mention.memberRef.displayName})"
        mentionText(name)
      } else if (mention != null) mentionText(ft.format.memberName)
      else ft.text
    }
    is Format.Uri -> displayedUri(ft.text)
    is Format.HyperLink -> ft.format.showText ?: ft.text
    is Format.SimplexLink -> {
      val t = ft.format.showText
        ?: if (linkMode == SimplexLinkMode.DESCRIPTION) ft.format.linkType.description else null
      if (t != null) "$t ${ft.format.viaHosts}" else ft.text
    }
    is Format.Command -> ft.text
    is Format.Escape -> ""
    is Format.Markup -> ft.format.token.drawnText
    else -> ft.text
  }

class ItemDisplay(val parts: List<Pair<String, String>>, val bases: List<Int>)

fun itemDisplay(ci: ChatItem, linkMode: SimplexLinkMode): ItemDisplay? {
  val fts = ci.formattedText ?: return null
  val segment = { ft: FormattedText ->
    val display = segmentDisplayText(ft, ci.mentions, linkMode)
    display to if (ft.format is Format.Markup) display else ft.text
  }
  val blocks = ci.markdownBlocks ?: return ItemDisplay(fts.map(segment), listOf(0))
  val parts = mutableListOf<Pair<String, String>>()
  var offset = itemPrefixText(ci).length
  val bases = blockTextRanges(blocks).mapIndexed { i, t ->
    if (i > 0) {
      val separator = if (t.inRow) "\t" else "\n"
      parts += separator to separator
      offset += 1
    }
    val base = if (i == 0) 0 else offset
    for (j in t.range.from.coerceIn(0, fts.size) until t.range.to.coerceIn(0, fts.size)) {
      val part = segment(fts[j])
      parts += part
      offset += part.first.length
    }
    base
  }
  return ItemDisplay(parts, bases)
}

// Display-only prefix rendered before ci.text (e.g. "Spam: " for reports).
// Renderers and selection code MUST share this string — otherwise selection offsets drift from screen.
fun itemPrefixText(ci: ChatItem): String = when (val mc = ci.content.msgContent) {
  is MsgContent.MCReport -> if (mc.text.isEmpty()) mc.reason.text else "${mc.reason.text}: "
  else -> ""
}

// Text transformations in MarkdownText must match segmentDisplayText above
@Composable
fun MarkdownText (
  text: CharSequence,
  formattedText: List<FormattedText>? = null,
  markdownBlocks: List<MarkdownBlock>? = null,
  sender: String? = null,
  meta: CIMeta? = null,
  chatTTL: Int? = null,
  encrypted: Boolean? = null,
  signedFileVerified: Boolean? = null,
  mentions: Map<String, CIMention>? = null,
  userMemberId: String? = null,
  toggleSecrets: Boolean,
  sendCommandMsg: ((String) -> Unit)? = null,
  style: TextStyle = MaterialTheme.typography.body1.copy(color = messageTextColor(), lineHeight = 22.sp),
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
  prefix: AnnotatedString? = null,
  stripLink: String? = null,
  selection: ItemSelection? = null,
  textIndex: Int = 0,
  blockLayout: Boolean = false,
  range: IntRange? = null,
  softWrap: Boolean = true,
  cardColor: Color? = null,
  parentDirection: LayoutDirection? = null
) {
  val text = if (stripLink != null) stripTextLink(text.toString(), stripLink) else text
  val formattedText = if (stripLink != null) stripFormattedTextLink(formattedText, stripLink) else formattedText
  val textLayoutDirection = remember (text) {
    if (isRtl(text.subSequence(0, kotlin.math.min(50, text.length)))) LayoutDirection.Rtl else LayoutDirection.Ltr
  }
  val metaCopy = if (meta != null && LocalTerminalLayout.current) {
    terminalReserveSpaceForMeta(meta, chatTTL, encrypted, signedFileVerified, MaterialTheme.colors.secondary, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
  } else null
  if (blockLayout && formattedText != null && markdownBlocks != null && range == null) {
    val direction = LocalLayoutDirection.current
    val terminal = LocalTerminalLayout.current
    val ruleMinWidth = if (metaCopy != null && markdownBlocks.isNotEmpty() && markdownBlocks.all { it is MarkdownBlock.Rule }) {
      with(LocalDensity.current) { rememberTextMeasurer().measure(metaCopy, style).size.width.toDp() }
    } else 0.dp
    CompositionLocalProvider(LocalLayoutDirection provides textLayoutDirection) {
      MarkdownBlocks(
        formattedText, markdownBlocks, style, modifier, cardColor ?: MaterialTheme.appColors.receivedQuote,
        blockText = { index, r, blockStyle, blockModifier, blockSoftWrap, last ->
          MarkdownText(
            text, formattedText, markdownBlocks, sender = if (index == 0) sender else null, meta = if (last) meta else null, chatTTL = chatTTL,
            encrypted = encrypted, signedFileVerified = signedFileVerified,
            mentions = mentions, userMemberId = userMemberId, toggleSecrets = toggleSecrets, sendCommandMsg = sendCommandMsg, style = blockStyle,
            uriHandler = uriHandler, senderBold = senderBold, modifier = blockModifier, linkMode = linkMode, onLinkLongClick = onLinkLongClick,
            showViaProxy = showViaProxy, showTimestamp = showTimestamp, prefix = if (index == 0) prefix else null,
            selection = selection, textIndex = index, range = r.from until r.to, softWrap = blockSoftWrap, parentDirection = direction
          )
        },
        metaLine = if (terminal && meta?.isLive != true) null else {
          {
            MarkdownText(
              text, formattedText, markdownBlocks, meta = meta, chatTTL = chatTTL, encrypted = encrypted, signedFileVerified = signedFileVerified, toggleSecrets = toggleSecrets, style = style,
              linkMode = linkMode, showViaProxy = showViaProxy, showTimestamp = showTimestamp, range = IntRange.EMPTY, parentDirection = if (terminal) direction else null
            )
          }
        },
        ruleMinWidth = ruleMinWidth
      )
    }
    return
  }
  val reserve = meta?.let {
    if (textLayoutDirection != (parentDirection ?: LocalLayoutDirection.current)) {
      AnnotatedString("\n")
    } else if (metaCopy != null) {
      metaCopy
    } else {
      AnnotatedString(reserveSpaceForMeta(it, chatTTL, null, secondaryColor = MaterialTheme.colors.secondary, showViaProxy = showViaProxy, showTimestamp = showTimestamp))
    }
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
    val textSelection = selection?.text(textIndex)
    if (formattedText == null) {
      var selectableEnd = 0
      val annotatedText = buildAnnotatedString {
        inlineContent?.first?.invoke(this)
        appendSender(this, sender, senderBold)
        if (prefix != null) append(prefix)
        if (text is String) append(text)
        else if (text is AnnotatedString) append(text)
        selectableEnd = this.length
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        if (reserve != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      val clampedRange = textSelection?.highlightRange?.let { it.first .. minOf(it.last, selectableEnd - 1) }?.takeUnless { it.isEmpty() }
      if (textSelection != null) {
        SelectableText(annotatedText, style = style, modifier = modifier.then(textSelection.positionModifier), maxLines = maxLines, overflow = overflow, selectionRange = clampedRange, onTextLayoutResult = textSelection.onTextLayoutResult)
      } else {
        Text(annotatedText, style = style, modifier = modifier, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent?.second ?: mapOf())
      }
    } else {
      var selectableEnd = 0
      var hasLinks = false
      var hasSecrets = false
      var hasCommands = false
      var hasModals = false
      val hiddenSecrets = mutableListOf<IntRange>()
      val annotatedText = buildAnnotatedString {
        inlineContent?.first?.invoke(this)
        appendSender(this, sender, senderBold)
        if (prefix != null) append(prefix)
        val headings = if (range == null && markdownBlocks != null) headingSegments(formattedText.size, markdownBlocks) else null
        fun appendSecret(s: String) {
          val start = length
          withStyle(hiddenSecretStyle) { append(s) }
          hiddenSecrets += start until length
        }
        for (i in range?.let { it.first.coerceAtLeast(0) .. minOf(it.last, formattedText.lastIndex) } ?: formattedText.indices) {
          val ft = formattedText[i]
          val heading = headings?.get(i) == true
          if (heading) pushStyle(SpanStyle(fontWeight = FontWeight.Bold))
          if (ft.format == null) append(ft.text)
          else when(ft.format) {
            is Format.Bold -> withStyle(if (markdownBlocks != null && ft.format.singleStar) Format.Italic().style else ft.format.style) { append(ft.text) }
            is Format.Italic -> withStyle(ft.format.style) { append(ft.text) }
            is Format.StrikeThrough -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Snippet -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Small -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Colored -> withStyle(ft.format.style) { append(ft.text) }
            is Format.Secret -> {
              if (toggleSecrets) {
                hasSecrets = true
                val key = i.toString()
                withAnnotation(tag = "SECRET", annotation = key) {
                  if (showSecrets[key] == true) append(ft.text) else appendSecret(ft.text)
                }
              } else {
                appendSecret(ft.text)
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
                withStyle(ftStyle) { append(displayedUri(ft.text)) }
              }
            }
            is Format.HyperLink -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = if (ft.format.isEmail) "OTHER_URL" else "WEB_URL", annotation = ft.format.linkUri) {
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
            is Format.SimplexName -> {
              hasLinks = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = "SIMPLEX_NAME", annotation = i.toString()) {
                withStyle(ftStyle) { append(ft.text) }
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
            is Format.Modal -> {
              hasModals = true
              val ftStyle = Format.linkStyle
              withAnnotation(tag = "MODAL", annotation = i.toString()) {
                withStyle(ftStyle) { append(ft.text) }
              }
            }
            is Format.Escape -> {}
            is Format.Markup -> append(
              if (range != null) ft.format.token.drawnText
              else if (markdownBlocks != null) ft.format.token.previewText
              else ft.text
            )
            is Format.Unknown -> append(ft.text)
          }
          if (heading) pop()
        }
        selectableEnd = this.length
        if (meta?.isLive == true) {
          append(typingIndicator(meta.recent, typingIdx))
        }
        // With RTL language set globally links looks bad sometimes, better to add a new line to bo sure everything looks good
        /*if (metaText != null && hasLinks && LocalLayoutDirection.current == LayoutDirection.Rtl)
          withStyle(reserveTimestampStyle) { append("\n" + metaText) }
        else */if (reserve != null) withStyle(reserveTimestampStyle) { append(reserve) }
      }
      val clampedRange = textSelection?.highlightRange?.let { it.first .. minOf(it.last, selectableEnd - 1) }?.takeUnless { it.isEmpty() }
      val layoutResult = remember { mutableStateOf<TextLayoutResult?>(null) }
      val onTextLayout: (TextLayoutResult) -> Unit = { r ->
        layoutResult.value = r
        textSelection?.onTextLayoutResult?.invoke(r)
      }
      val textModifier = modifier
        .then(textSelection?.positionModifier ?: Modifier)
        .then(if (hiddenSecrets.isEmpty()) Modifier else Modifier.drawWithContent {
          drawContent()
          drawSecretPattern(hiddenSecrets, layoutResult.value)
        })
      if ((hasLinks && uriHandler != null) || hasSecrets || (hasCommands && sendCommandMsg != null) || hasModals) {
        val icon = remember { mutableStateOf(PointerIcon.Text) }
        ClickableText(annotatedText, style = style, selectionRange = clampedRange, modifier = textModifier.pointerHoverIcon(icon.value), softWrap = softWrap, maxLines = maxLines, overflow = overflow,
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
              withAnnotation("SIMPLEX_NAME") { a ->
                val idx = a.item.toIntOrNull()
                val nameText = idx?.let { formattedText.getOrNull(it) }?.text
                // The name string is routed through the same connect path as a
                // link; planAndConnect resolves it on the core (name target).
                if (nameText != null) uriHandler.openVerifiedSimplexUri(nameText)
              }
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
            if (hasModals) {
              withAnnotation("MODAL") { a ->
                (a.item.toIntOrNull()?.let { formattedText.getOrNull(it)?.format } as? Format.Modal)?.let { openMarkdownModal(it) }
              }
            }
          },
          onHover = { offset ->
            val hasAnnotation: (String) -> Boolean = { tag -> annotatedText.hasStringAnnotations(tag, start = offset, end = offset) }
            val hand = hasAnnotation("WEB_URL") || hasAnnotation("SIMPLEX_URL") || hasAnnotation("OTHER_URL") || hasAnnotation("SIMPLEX_NAME") || hasAnnotation("SECRET") || hasAnnotation("COMMAND") || hasAnnotation("MODAL")
            icon.value = if (hand) PointerIcon.Hand else PointerIcon.Text
          },
          onHoverExit = {
            // reset icon.value too, or pointerHoverIcon re-displays a stale Hand on the next Enter
            icon.value = PointerIcon.Text
          },
          shouldConsumeEvent = { offset ->
            annotatedText.hasStringAnnotations(tag = "WEB_URL", start = offset, end = offset)
                || annotatedText.hasStringAnnotations(tag = "SIMPLEX_URL", start = offset, end = offset)
                || annotatedText.hasStringAnnotations(tag = "OTHER_URL", start = offset, end = offset)
                || annotatedText.hasStringAnnotations(tag = "MODAL", start = offset, end = offset)
          },
          onTextLayout = onTextLayout
        )
      } else {
        if (textSelection != null) {
          SelectableText(annotatedText, style = style, modifier = textModifier, softWrap = softWrap, maxLines = maxLines, overflow = overflow, selectionRange = clampedRange, onTextLayoutResult = onTextLayout)
        } else {
          Text(annotatedText, style = style, modifier = textModifier, softWrap = softWrap, maxLines = maxLines, overflow = overflow, inlineContent = inlineContent?.second ?: mapOf(), onTextLayout = onTextLayout)
        }
      }
    }
  }
}

private val hiddenSecretStyle = SpanStyle(color = Color.Transparent)

private val secretTile: ImageBitmap? by lazy { MR.images.vertical_logo.toComposeImageBitmap() }

private fun ContentDrawScope.drawSecretPattern(ranges: List<IntRange>, layout: TextLayoutResult?) {
  val tile = secretTile ?: return
  if (layout == null) return
  val brush = ShaderBrush(ImageShader(tile, TileMode.Repeated, TileMode.Repeated))
  val tileScale = 8.dp.toPx() / tile.width
  val saturation = ColorFilter.colorMatrix(ColorMatrix().apply { setToSaturation(0.65f) })
  val textLength = layout.layoutInput.text.length
  ranges.forEach { r ->
    val path = layout.getPathForRange(r.first.coerceIn(0, textLength), (r.last + 1).coerceIn(0, textLength))
    val bounds = path.getBounds()
    val side = (bounds.width + bounds.height) / tileScale
    clipPath(path) {
      rotate(45f, bounds.center) {
        scale(tileScale, bounds.center) {
          drawRect(brush, Offset(bounds.center.x - side / 2, bounds.center.y - side / 2), Size(side, side), alpha = 0.35f, colorFilter = saturation)
        }
      }
    }
  }
}

@Composable
fun ClickableText(
  text: AnnotatedString,
  modifier: Modifier = Modifier,
  style: TextStyle = TextStyle.Default,
  selectionRange: IntRange? = null,
  softWrap: Boolean = true,
  overflow: TextOverflow = TextOverflow.Clip,
  maxLines: Int = Int.MAX_VALUE,
  onTextLayout: (TextLayoutResult) -> Unit = {},
  onClick: (Int) -> Unit,
  onLongClick: (Int) -> Unit = {},
  onHover: (Int) -> Unit = {},
  onHoverExit: () -> Unit = {},
  shouldConsumeEvent: (Int) -> Boolean
) {
  val layoutResult = remember { mutableStateOf<TextLayoutResult?>(null) }
  // pointerInput keyed on these lambdas restarts on every recomposition (they are new
  // instances each time), and a restart mid-gesture swallows the click/hover in flight;
  // key on Unit and read the latest handlers via rememberUpdatedState instead
  val currentOnClick = rememberUpdatedState(onClick)
  val currentOnLongClick = rememberUpdatedState(onLongClick)
  val currentOnHover = rememberUpdatedState(onHover)
  val currentOnHoverExit = rememberUpdatedState(onHoverExit)
  val currentShouldConsumeEvent = rememberUpdatedState(shouldConsumeEvent)
  // to tell a moved pointer from text that shifted under a stationary pointer (see waitForUpOrCancellation)
  val textCoordinates = remember { mutableStateOf<LayoutCoordinates?>(null) }
  val pressIndicator = Modifier.pointerInput(Unit) {
    detectGesture(onLongPress = { pos ->
      layoutResult.value?.let { layoutResult ->
        currentOnLongClick.value(layoutResult.getOffsetForPosition(pos))
      }
    }, onPress = { pos ->
      layoutResult.value?.let { layoutResult ->
        val res  = tryAwaitRelease()
        if (res) {
          currentOnClick.value(layoutResult.getOffsetForPosition(pos))
        }
      }
    }, positionInWindow = { textCoordinates.value?.positionInWindow() ?: Offset.Zero }, shouldConsumeEvent = { pos ->
      var consume = false
      layoutResult.value?.let { layoutResult ->
        consume = currentShouldConsumeEvent.value(layoutResult.getOffsetForPosition(pos))
      }
      consume
    }
    )
  }.pointerInput(Unit) {
    if (appPlatform.isDesktop) {
      detectCursorMove(onExit = { currentOnHoverExit.value() }) { pos ->
        layoutResult.value?.let { layoutResult ->
          currentOnHover.value(layoutResult.getOffsetForPosition(pos))
        }
      }
    }
  }

  BasicText(
    text = text,
    modifier = modifier.then(selectionHighlight(selectionRange, text.length, layoutResult)).then(pressIndicator)
      .onGloballyPositioned { textCoordinates.value = it },
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

@Composable
private fun SelectableText(
  text: AnnotatedString,
  style: TextStyle,
  modifier: Modifier = Modifier,
  softWrap: Boolean = true,
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  selectionRange: IntRange? = null,
  onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null
) {
  val layoutResult = remember { mutableStateOf<TextLayoutResult?>(null) }

  BasicText(
    text = text,
    modifier = modifier.pointerHoverIcon(PointerIcon.Text).then(selectionHighlight(selectionRange, text.length, layoutResult)),
    style = style,
    softWrap = softWrap,
    maxLines = maxLines,
    overflow = overflow,
    onTextLayout = {
      layoutResult.value = it
      onTextLayoutResult?.invoke(it)
    }
  )
}

private fun selectionHighlight(selectionRange: IntRange?, textLength: Int, layoutResult: State<TextLayoutResult?>): Modifier =
  if (selectionRange != null) {
    Modifier.drawBehind {
      layoutResult.value?.let { result ->
        if (selectionRange.first <= selectionRange.last && selectionRange.last + 1 <= textLength) {
          drawPath(result.getPathForRange(selectionRange.first, selectionRange.last + 1), SelectionHighlightColor)
        }
      }
    }
  } else Modifier

fun openBrowserAlert(uri: String, uriHandler: UriHandler) {
  val (res, err) = sanitizeUri(uri)
  if (res == null) {
    showInvalidLinkAlert(uri, err)
  } else {
    val shownUri = displayedUri(uri)
    val message = if (shownUri.count() > 160) shownUri.substring(0, 159) + "…" else shownUri
    val sanitizedUri = res.second
    if (sanitizedUri == null) {
      AlertManager.shared.showAlertDialog(
        generalGetString(MR.strings.privacy_chat_list_open_web_link_question),
        message,
        confirmText = generalGetString(MR.strings.open_verb),
        onConfirm = { safeOpenUri(uri, uriHandler) },
        parseHtml = false
      )
    } else {
      AlertManager.shared.showAlertDialogButtonsColumn(
        generalGetString(MR.strings.privacy_chat_list_open_web_link_question),
        message,
        parseHtml = false,
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
  val message = if (error.isNullOrEmpty()) { displayedUri(uri) } else { error + "\n" + displayedUri(uri) }
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

fun mentionText(name: String): String = if (name.contains(" @"))  "@'$name'" else "@$name"

fun stripTextLink(text: String, link: String): String =
  if (text == link) ""
  else if (text.endsWith("\n$link")) text.dropLast(link.length + 1)
  else text

fun stripFormattedTextLink(ft: List<FormattedText>?, link: String): List<FormattedText>? {
  if (ft == null || ft.isEmpty() || ft.last().text != link) return ft
  val result = ft.toMutableList()
  result.removeAt(result.lastIndex)
  val i = result.lastIndex
  if (i >= 0 && result[i].format is Format.Markup) {
    result.removeAt(i)
  } else if (i >= 0 && result[i].format == null && result[i].text.endsWith("\n")) {
    result[i] = FormattedText(result[i].text.dropLast(1), null)
    if (result[i].text.isEmpty()) result.removeAt(result.lastIndex)
  }
  return result.ifEmpty { null }
}
