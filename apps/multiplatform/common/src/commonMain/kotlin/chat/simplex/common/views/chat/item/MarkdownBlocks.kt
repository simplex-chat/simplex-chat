package chat.simplex.common.views.chat.item

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.gestures.waitForUpOrCancellation
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.hoverable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.CornerRadius
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Shape
import androidx.compose.ui.graphics.takeOrElse
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.AlignmentLine
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.layout
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.unit.takeOrElse
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlin.math.pow
import kotlin.math.roundToInt

@Composable
fun MarkdownBlocks(
  formattedText: List<FormattedText>,
  blocks: List<MarkdownBlock>,
  style: TextStyle,
  modifier: Modifier,
  cardColor: Color,
  blockText: @Composable (index: Int, range: SegmentRange, style: TextStyle, modifier: Modifier, softWrap: Boolean, last: Boolean) -> Unit,
  metaLine: (@Composable () -> Unit)?,
  ruleMinWidth: Dp
) {
  val roundness = remember { appPreferences.chatItemRoundness.state }.value.coerceIn(0f, 1f)
  val cardShape = RoundedCornerShape(12.dp * roundness)
  val lineColor = MaterialTheme.colors.secondary.copy(alpha = 0.35f)
  val textColor = style.color.takeOrElse { MaterialTheme.colors.onSurface }
  val fontSize = style.fontSize.takeOrElse { 16.sp }
  val lineHeight = style.lineHeight.takeOrElse { 22.sp }
  val terminal = LocalTerminalLayout.current
  val starts = remember(blocks) { blocks.runningFold(0) { n, b -> n + b.textRanges.size } }
  val lastIsText = when (blocks.lastOrNull()) {
    is MarkdownBlock.Paragraph, is MarkdownBlock.Heading, is MarkdownBlock.Quote, is MarkdownBlock.ItemList -> true
    else -> false
  }
  @Composable
  fun BlockText(index: Int, range: SegmentRange, style: TextStyle, modifier: Modifier = Modifier, softWrap: Boolean = true) {
    blockText(index, range, style, modifier, softWrap, lastIsText && index == starts.last() - 1)
  }
  Column(modifier) {
    blocks.forEachIndexed { k, block ->
      if (k > 0 && terminal && blocks[k - 1] is MarkdownBlock.Heading && block !is MarkdownBlock.Heading) {
        Spacer(Modifier.height(TERMINAL_HEADING_BOTTOM_GAP.toDp()))
      } else if (k > 0 && (paragraphBreak(formattedText, blocks[k - 1].end, block.start) || terminal && block is MarkdownBlock.Heading)) {
        Spacer(Modifier.height(if (terminal) TERMINAL_LEVEL_STEP.toDp() else with(LocalDensity.current) { (lineHeight.toPx() / 2).toDp() }))
      }
      val start = starts[k]
      val marksLastBlock = k == blocks.lastIndex && metaLine == null
      val cardMargins = if (!terminal) Modifier else if (marksLastBlock) Modifier.padding(top = TERMINAL_BLOCK_TOP_GAP.toDp()).messageLastBlock() else Modifier.padding(top = TERMINAL_BLOCK_TOP_GAP.toDp(), bottom = TERMINAL_BLOCK_BOTTOM_GAP.toDp())
      when (block) {
        is MarkdownBlock.Paragraph -> BlockText(start, block.content, style)
        is MarkdownBlock.Heading -> {
          val scale = if (terminal) TERMINAL_HEADING_RATIO.pow((3 - block.level).coerceAtLeast(0)) else when (block.level) {
            1 -> 1.5f
            2 -> 1.3f
            3 -> 1.15f
            else -> 1f
          }
          BlockText(
            start, block.content, style.copy(fontSize = fontSize * scale, lineHeight = lineHeight * scale, fontWeight = FontWeight.Bold),
            if (terminal) Modifier.headingBox(scale, fontSize) else Modifier
          )
        }
        is MarkdownBlock.Quote -> Box(Modifier.quoteBar(textColor)) {
          val quoteColor = textColor.copy(alpha = 0.7f)
          BlockText(start, block.content, style.copy(color = quoteColor).merge(terminalTextStyle(fontSize, quoteColor)))
        }
        is MarkdownBlock.ItemList -> listEntries(block.items, 0).forEachIndexed { j, (entry, level) ->
          Row(Modifier.padding(start = 20.dp * level)) {
            Text(entry.number ?: "•", Modifier.widthIn(min = 14.dp), style = style)
            Spacer(Modifier.width(6.dp))
            BlockText(start + j, entry.content, style, Modifier.weight(1f, fill = false))
          }
        }
        is MarkdownBlock.CodeBlock -> CodeBlockCard(cardShape, cardColor, roundness, codeText(formattedText, block.content), cardMargins) { m ->
          val codeFontSize = fontSize * 0.875f
          BlockText(start, block.content, style.copy(fontFamily = FontFamily.Monospace, fontSize = codeFontSize, lineHeight = lineHeight * 0.9f).merge(terminalTextStyle(codeFontSize)), m, softWrap = false)
        }
        is MarkdownBlock.Table -> {
          val tableRows = listOf(block.header) + block.rows
          val rowStarts = tableRows.runningFold(start) { n, row -> n + row.size }
          Box(cardMargins.horizontalScroll(rememberScrollState())) {
            TableGrid(
              block.columns, tableRows.map { it.size }, cardColor, MaterialTheme.colors.secondary.copy(alpha = 0.1f), lineColor,
              Modifier.clip(cardShape).border(1.dp, lineColor, cardShape)
            ) {
              tableRows.forEachIndexed { r, row ->
                row.forEachIndexed { c, cell ->
                  BlockText(
                    rowStarts[r] + c, cell, if (r == 0) style.copy(fontWeight = FontWeight.Bold) else style,
                    Modifier.padding(horizontal = 12.dp, vertical = 4.dp), softWrap = false
                  )
                }
              }
            }
          }
        }
        is MarkdownBlock.Rule -> Box((if (terminal) cardMargins.widthIn(min = ruleMinWidth) else Modifier.padding(vertical = 6.dp)).fillMaxWidth().height(1.dp).background(lineColor))
      }
    }
    if (!lastIsText) metaLine?.invoke()
  }
}

@Composable
fun Modifier.quoteBar(textColor: Color): Modifier {
  val color = textColor.copy(alpha = if (MaterialTheme.colors.isLight) 0.16f else 0.22f)
  val barWidth = 4.dp
  return drawBehind {
    val width = barWidth.roundToPx().toFloat()
    drawRoundRect(color, Offset(if (layoutDirection == LayoutDirection.Ltr) 0f else size.width - width, 0f), Size(width, size.height), CornerRadius(2.dp.toPx()))
  }.padding(start = barWidth + 10.dp)
}

private fun Modifier.headingBox(scale: Float, bodyFontSize: TextUnit): Modifier =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)
    val firstBaseline = placeable[FirstBaseline]
    val lastBaseline = placeable[LastBaseline]
    if (firstBaseline == AlignmentLine.Unspecified || lastBaseline == AlignmentLine.Unspecified) {
      layout(placeable.width, placeable.height) { placeable.placeRelative(0, 0) }
    } else {
      val bodyRatio = bodyFontSize.toPx() / (bodyFontSize * scale).toPx()
      val top = (firstBaseline * (1 - bodyRatio) - INTER_CAP_HEIGHT * ((bodyFontSize * scale).toPx() - bodyFontSize.toPx())).roundToInt()
      val bottom = ((placeable.height - lastBaseline) * (1 - bodyRatio)).roundToInt()
      layout(placeable.width, placeable.height - top - bottom) { placeable.placeRelative(0, -top) }
    }
  }

private val MarkdownBlock.start: Int get() = when (this) {
  is MarkdownBlock.Rule -> mark.from
  else -> textRanges.firstOrNull()?.range?.from ?: 0
}

private val MarkdownBlock.end: Int get() = when (this) {
  is MarkdownBlock.Rule -> mark.to
  else -> textRanges.lastOrNull()?.range?.to ?: 0
}

private fun paragraphBreak(fts: List<FormattedText>, from: Int, to: Int): Boolean =
  (from.coerceIn(0, fts.size) until to.coerceIn(0, fts.size)).any { (fts[it].format as? Format.Markup)?.token is BlockToken.ParagraphBreak }

private fun codeText(fts: List<FormattedText>, r: SegmentRange): String =
  (r.from.coerceIn(0, fts.size) until r.to.coerceIn(0, fts.size)).joinToString("") { i ->
    when (val f = fts[i].format) {
      is Format.Markup -> f.token.drawnText
      is Format.Escape -> ""
      else -> fts[i].text
    }
  }

@Composable
private fun CodeBlockCard(shape: Shape, color: Color, roundness: Float, copyText: String, modifier: Modifier, content: @Composable (Modifier) -> Unit) {
  var tapped by remember { mutableStateOf(false) }
  val interactionSource = remember { MutableInteractionSource() }
  val hovered by interactionSource.collectIsHoveredAsState()
  val clipboard = LocalClipboardManager.current
  Box(
    modifier
      .fillMaxWidth()
      .clip(shape)
      .background(color)
      .hoverable(interactionSource)
      .pointerInput(Unit) {
        awaitEachGesture {
          awaitFirstDown(requireUnconsumed = false)
          if (waitForUpOrCancellation() != null) tapped = !tapped
        }
      }
  ) {
    content(Modifier.horizontalScroll(rememberScrollState()).padding(horizontal = 12.dp, vertical = 8.dp))
    if (hovered || tapped) {
      Box(Modifier.matchParentSize(), contentAlignment = Alignment.TopEnd) {
        Box(
          Modifier
            .padding(4.dp)
            .size(28.dp)
            .clip(RoundedCornerShape(8.dp * roundness))
            .background(color)
            .clickable {
              clipboard.setText(AnnotatedString(copyText))
              showToast(generalGetString(MR.strings.copied))
            },
          contentAlignment = Alignment.Center
        ) {
          Icon(painterResource(MR.images.ic_content_copy), stringResource(MR.strings.copy_verb), Modifier.size(18.dp), tint = MaterialTheme.colors.primary)
        }
      }
    }
  }
}

private class TableGridLines {
  var xs: List<Int> = emptyList()
  var ys: List<Int> = emptyList()
}

@Composable
private fun TableGrid(
  columns: List<TableAlign>,
  rowSizes: List<Int>,
  headerColor: Color,
  stripeColor: Color,
  lineColor: Color,
  modifier: Modifier,
  content: @Composable () -> Unit
) {
  val lines = remember { TableGridLines() }
  Layout(
    content,
    modifier.drawBehind {
      val xs = lines.xs
      val ys = lines.ys
      if (ys.size < 2) return@drawBehind
      drawRect(headerColor, Offset.Zero, Size(size.width, ys[1].toFloat()))
      for (r in 2 until ys.size - 1 step 2) {
        drawRect(stripeColor, Offset(0f, ys[r].toFloat()), Size(size.width, (ys[r + 1] - ys[r]).toFloat()))
      }
      val stroke = 1.dp.toPx()
      xs.drop(1).dropLast(1).forEach { x -> drawLine(lineColor, Offset(x.toFloat(), 0f), Offset(x.toFloat(), size.height), stroke) }
      ys.drop(1).dropLast(1).forEach { y -> drawLine(lineColor, Offset(0f, y.toFloat()), Offset(size.width, y.toFloat()), stroke) }
    }
  ) { measurables, _ ->
    val cells = rowSizes.flatMapIndexed { r, n -> List(n) { c -> r to c } }.take(measurables.size)
    val widths = IntArray(maxOf(columns.size, rowSizes.maxOrNull() ?: 0))
    cells.forEachIndexed { k, (_, c) -> widths[c] = maxOf(widths[c], measurables[k].maxIntrinsicWidth(Constraints.Infinity)) }
    val placeables = cells.mapIndexed { k, (_, c) -> measurables[k].measure(Constraints(maxWidth = widths[c])) }
    val heights = IntArray(rowSizes.size)
    placeables.forEachIndexed { k, p -> heights[cells[k].first] = maxOf(heights[cells[k].first], p.height) }
    val xs = widths.toList().runningFold(0) { a, w -> a + w }
    val ys = heights.toList().runningFold(0) { a, h -> a + h }
    lines.xs = xs
    lines.ys = ys
    layout(xs.last(), ys.last()) {
      placeables.forEachIndexed { k, p ->
        val (r, c) = cells[k]
        val free = widths[c] - p.width
        val x = xs[c] + when (columns.getOrNull(c)) {
          TableAlign.Right -> free
          TableAlign.Center -> free / 2
          else -> 0
        }
        p.place(x, ys[r])
      }
    }
  }
}
