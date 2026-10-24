package chat.simplex.common.views.chat.item

import androidx.compose.foundation.LocalIndication
import androidx.compose.foundation.hoverable
import androidx.compose.foundation.indication
import androidx.compose.foundation.interaction.Interaction
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.foundation.layout.*
import androidx.compose.material.LocalTextStyle
import androidx.compose.material.MaterialTheme
import androidx.compose.material.ProvideTextStyle
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.compositeOver
import androidx.compose.ui.graphics.luminance
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.changedToDownIgnoreConsumed
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.AlignmentLine
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.HorizontalAlignmentLine
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.Placeable
import androidx.compose.ui.layout.VerticalAlignmentLine
import androidx.compose.ui.layout.layout
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.offset
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.Flow
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.roundToInt

val LocalTerminalLayout = staticCompositionLocalOf { false }

val TERMINAL_BLOCK_TOP_GAP = 5.75.sp
val TERMINAL_BLOCK_BOTTOM_GAP = 5.sp
val TERMINAL_LEVEL_STEP = 4.75.sp
val TERMINAL_HEADING_BOTTOM_GAP = TERMINAL_LEVEL_STEP / 2
val TERMINAL_SENDER_TOP_GAP = 6.5.sp
private val TERMINAL_NAME_BOTTOM_GAP = 5.5.sp
val TERMINAL_HEADER_TOP_GAP = 2.5.sp
val TERMINAL_HEADER_BOTTOM_GAP = 1.5.sp
val TERMINAL_EVENT_TOP_GAP = 8.25.sp
val TERMINAL_EVENT_BOTTOM_GAP = 1.5.sp
val TERMINAL_FILE_TOP_GAP = 2.sp
val TERMINAL_FILE_CAPTION_GAP = 1.sp
val TERMINAL_EMOJI_TOP_GAP = 4.25.sp
val TERMINAL_EMOJI_BOTTOM_GAP = 1.5.sp
private val TERMINAL_META_GAP = 6.sp
const val TERMINAL_HEADING_RATIO = 1.3125f
private const val TERMINAL_LINE_HEIGHT_RATIO = 22f / 16f
private const val TERMINAL_LINE_HEIGHT_CONTRAST_SLOPE = 0.1515f
const val INTER_CAP_HEIGHT = 2048f / 2816f
private val TERMINAL_LINE_HEIGHT_STYLE = LineHeightStyle(LineHeightStyle.Alignment.Proportional, LineHeightStyle.Trim.None)

@Composable
fun terminalTextStyle(fontSize: TextUnit, color: Color = messageTextColor()): TextStyle =
  if (LocalTerminalLayout.current) {
    TextStyle(lineHeight = fontSize * (TERMINAL_LINE_HEIGHT_RATIO * contrastLineHeightFactor(color)), lineHeightStyle = TERMINAL_LINE_HEIGHT_STYLE)
  } else {
    TextStyle.Default
  }

@Composable
private fun contrastLineHeightFactor(color: Color): Float {
  val background = MaterialTheme.colors.background
  val textContrast = lightnessContrast(messageTextColor(), background)
  val weight = if (textContrast > 0f) (lightnessContrast(color, background) / textContrast).coerceIn(0f, 1f) else 1f
  return 1 - TERMINAL_LINE_HEIGHT_CONTRAST_SLOPE * (1 - weight)
}

private fun lightnessContrast(color: Color, background: Color): Float =
  abs(lightness(color.compositeOver(background)) - lightness(background))

private fun lightness(color: Color): Float {
  val y = color.luminance()
  return if (y > 216f / 24389f) 116f * y.pow(1f / 3) - 16f else y * 24389f / 27f
}

fun terminalItemShowsSender(item: ChatItem, older: ChatItem?): Boolean =
  showsSender(item, older) { it.content.hasTerminalSender }

fun terminalRowGap(item: ChatItem, older: ChatItem?, separated: Boolean, largeGap: Boolean, hasSenderRow: Boolean): TextUnit =
  if (older != null && !separated && (largeGap || hasSenderRow || !item.content.hasTerminalSender || !older.content.hasTerminalSender || item.textAtItemTop && item.startsWithHeading)) TERMINAL_LEVEL_STEP else 0.sp

fun terminalHeadingGap(item: ChatItem): TextUnit =
  if (!item.textAtItemTop && item.startsWithHeading) TERMINAL_LEVEL_STEP else 0.sp

private val ChatItem.textAtItemTop: Boolean get() =
  (content.msgContent is MsgContent.MCText || content.msgContent is MsgContent.MCUnknown && file == null) && !hasQuoteOrHeader

private val ChatItem.startsWithHeading: Boolean get() =
  markdownBlocks?.firstOrNull() is MarkdownBlock.Heading

private val CIContent.hasTerminalSender: Boolean get() = when (this) {
  is CIContent.SndMsgContent,
  is CIContent.SndDeleted,
  is CIContent.SndCall,
  is CIContent.SndGroupInvitation,
  is CIContent.SndModerated -> true
  else -> showMemberName
}

class TerminalRowInteractionSource : MutableInteractionSource {
  private val source = MutableInteractionSource()
  private var press: PressInteraction.Press? = null
  var downPosition = Offset.Zero
  override val interactions: Flow<Interaction> = source.interactions
  override suspend fun emit(interaction: Interaction) = source.emit(rowInteraction(interaction))
  override fun tryEmit(interaction: Interaction): Boolean = source.tryEmit(rowInteraction(interaction))

  private fun rowInteraction(interaction: Interaction): Interaction = when (interaction) {
    is PressInteraction.Press -> PressInteraction.Press(downPosition).also { press = it }
    is PressInteraction.Release -> PressInteraction.Release(press ?: interaction.press)
    is PressInteraction.Cancel -> PressInteraction.Cancel(press ?: interaction.press)
    else -> interaction
  }
}

@Composable
fun Modifier.terminalRowIndication(interactionSource: TerminalRowInteractionSource): Modifier =
  this
    .pointerInput(interactionSource) {
      awaitPointerEventScope {
        while (true) {
          awaitPointerEvent(PointerEventPass.Initial).changes.firstOrNull { it.changedToDownIgnoreConsumed() }?.let { interactionSource.downPosition = it.position }
        }
      }
    }
    .hoverable(interactionSource)
    .indication(interactionSource, LocalIndication.current)

fun Modifier.terminalStartOffset(offset: () -> Dp): Modifier =
  layout { measurable, constraints ->
    val start = offset().roundToPx()
    val placeable = measurable.measure(constraints.offset(horizontal = -start))
    layout(placeable.width + start, placeable.height) { placeable.placeRelative(start, 0) }
  }

private val LastLineLeft = VerticalAlignmentLine(::min)
private val LastLineRight = VerticalAlignmentLine(::max)
private val LastBlockBottom = HorizontalAlignmentLine(::max)

fun Modifier.messageLastLine(): Modifier = lastLine { emptyMap() }

fun Modifier.messageLastBlock(): Modifier = lastLine { mapOf(LastBlockBottom to it.height) }

private fun Modifier.lastLine(extraAlignmentLines: (Placeable) -> Map<AlignmentLine, Int>): Modifier =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)
    layout(placeable.width, placeable.height, mapOf(LastLineLeft to 0, LastLineRight to placeable.width) + extraAlignmentLines(placeable)) { placeable.placeRelative(0, 0) }
  }

@Composable
fun Modifier.inlineIcon(): Modifier =
  if (LocalTerminalLayout.current) {
    layout { measurable, constraints ->
      val placeable = measurable.measure(constraints)
      layout(placeable.width, 0) { placeable.placeRelative(0, -placeable.height / 2) }
    }
  } else {
    this
  }

@Composable
fun ItemWithMeta(meta: @Composable () -> Unit, overlayMeta: (@Composable () -> Unit)? = null, metaReserved: Boolean = false, content: @Composable (inlineMeta: (@Composable () -> Unit)?) -> Unit) {
  if (LocalTerminalLayout.current) {
    TerminalMetaLayout(metaReserved) {
      content(null)
      listOfNotNull(meta, overlayMeta).forEach { m ->
        Box(Modifier.padding(end = 12.dp)) {
          m()
        }
      }
    }
  } else {
    content(meta)
  }
}

@Composable
private fun TerminalMetaLayout(metaReserved: Boolean, contentWithMeta: @Composable () -> Unit) {
  Layout(contentWithMeta) { measurables, constraints ->
    val metaConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val meta = measurables[1].measure(metaConstraints)
    val overlay = measurables.getOrNull(2)?.measure(metaConstraints)
    val besideMeta = !metaReserved && overlay == null
    val contentMaxWidth = if (besideMeta && constraints.hasBoundedWidth) (constraints.maxWidth - meta.width).coerceAtLeast(0) else constraints.maxWidth
    val content = measurables[0].measure(constraints.copy(minWidth = 0, maxWidth = contentMaxWidth))
    val width = if (constraints.hasBoundedWidth) constraints.maxWidth else if (besideMeta) content.width + meta.width else max(content.width, meta.width)
    val gap = TERMINAL_META_GAP.toPx().roundToInt()
    val lineEnd = if (layoutDirection == LayoutDirection.Ltr) {
      content[LastLineRight].takeIf { it != AlignmentLine.Unspecified }
    } else {
      content[LastLineLeft].takeIf { it != AlignmentLine.Unspecified }?.let { content.width - it }
    }
    val metaBelow = lineEnd != null && lineEnd + meta.width > width
    val textBaseline = content[LastBaseline]
    val metaBaseline = meta[FirstBaseline].takeIf { it != AlignmentLine.Unspecified } ?: ((meta.height + 12.sp.toPx() * INTER_CAP_HEIGHT) / 2).roundToInt()
    val metaTop = when {
      metaBelow -> content.height + gap
      overlay == null && textBaseline != AlignmentLine.Unspecified -> textBaseline - metaBaseline
      else -> content.height - gap - meta.height
    }.coerceAtLeast(0)
    val blockGap = if (!metaBelow && content[LastBlockBottom] == content.height) TERMINAL_BLOCK_BOTTOM_GAP.toPx().roundToInt() else 0
    layout(width, max(content.height + blockGap, metaTop + meta.height)) {
      content.placeRelative(0, 0)
      if (overlay != null && content.width + meta.width > width) {
        overlay.placeRelative(content.width - overlay.width, content.height - gap - overlay.height)
      } else {
        meta.placeRelative(width - meta.width, metaTop)
      }
    }
  }
}

@Composable
fun terminalGutter(chatInfo: ChatInfo): Dp =
  if (chatInfo is ChatInfo.Group) MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier else GO_TO_ITEM_BUTTON_SIZE

@Composable
fun TerminalChatItemRow(
  sender: ChatItemSender?,
  gutter: Dp,
  topGap: Dp,
  modifier: Modifier = Modifier,
  senderTopGap: Dp = TERMINAL_SENDER_TOP_GAP.toDp(),
  goToButton: @Composable () -> Unit = {},
  content: @Composable () -> Unit
) {
  Layout(
    content = {
      Box {
        sender?.image?.invoke()
      }
      Box {
        goToButton()
      }
      Box {
        if (sender != null) {
          ProvideTextStyle(LocalTextStyle.current.merge(terminalTextStyle(13.5.sp))) {
            SenderNameAndRole(sender, Modifier.padding(start = CHAT_ITEM_TEXT_PADDING, bottom = TERMINAL_NAME_BOTTOM_GAP.toDp()))
          }
        }
      }
      Box {
        content()
      }
    },
    modifier
      .padding(top = topGap + if (sender != null) senderTopGap else 0.dp, start = 8.dp, end = 12.dp)
      .fillMaxWidth()
  ) { measurables, constraints ->
    val gutterWidth = gutter.roundToPx()
    val messageStart = gutterWidth + 4.dp.roundToPx()
    val looseConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val messageConstraints = looseConstraints.copy(maxWidth = (constraints.maxWidth - messageStart).coerceAtLeast(0))
    val avatar = measurables[0].measure(looseConstraints)
    val button = measurables[1].measure(looseConstraints)
    val name = measurables[2].measure(messageConstraints)
    val message = measurables[3].measure(messageConstraints)
    val messageTop = name.height
    val buttonTop = max(avatar.height, messageTop + (message.height - button.height) / 2)
    val height = maxOf(messageTop + message.height, avatar.height, buttonTop + button.height)
    layout(constraints.maxWidth, height) {
      avatar.placeRelative(0, 0)
      button.placeRelative((gutterWidth - button.width) / 2, buttonTop)
      name.placeRelative(messageStart, 0)
      message.placeRelative(messageStart, messageTop)
    }
  }
}
