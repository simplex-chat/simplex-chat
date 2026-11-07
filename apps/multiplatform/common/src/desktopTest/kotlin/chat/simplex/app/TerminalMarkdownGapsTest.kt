package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.*
import kotlin.test.Test
import kotlin.test.assertEquals

class TerminalMarkdownGapsTest {
  private val density = Density(1f)
  private val placed = mutableMapOf<String, Placed>()
  private val ruleText = listOf(FormattedText("---", Format.Markup(BlockToken.RuleMark())))
  private val ruleBlocks = listOf(MarkdownBlock.Rule(SegmentRange(0, 1)))

  private fun paragraphGaps(n: Int) = with(density) { (TERMINAL_PARAGRAPH_GAP.value * n).dp.roundToPx() }

  private fun lineBreaks(n: Int) =
    if (n == 1) FormattedText("\n", Format.Markup(BlockToken.LineBreak())) else FormattedText("\n".repeat(n), Format.Markup(BlockToken.ParagraphBreak(n)))

  private fun height(terminal: Boolean, fts: List<FormattedText>, blocks: List<MarkdownBlock>?, prefix: AnnotatedString? = null): Int {
    renderScene {
      CompositionLocalProvider(LocalTerminalLayout provides terminal, LocalBubbleLayout provides !terminal) {
        Box(Modifier.width(300.dp).record(placed, "text")) {
          MarkdownText(
            fts.joinToString("") { it.text }, fts, blocks, toggleSecrets = false, modifier = Modifier.record(placed, "drawn"),
            style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp), linkMode = SimplexLinkMode.DESCRIPTION, prefix = prefix, blockLayout = true
          )
        }
      }
    }
    return placed.getValue("text").height
  }

  private fun paragraphs(terminal: Boolean, emptyLines: Int) = height(
    terminal,
    listOf(FormattedText("a"), lineBreaks(emptyLines + 1), FormattedText("b")),
    listOf(MarkdownBlock.Paragraph(SegmentRange(0, 1)), MarkdownBlock.Paragraph(SegmentRange(2, 3)))
  )

  private fun paragraphAndCode(emptyLines: Int) = height(
    terminal = true,
    listOf(
      FormattedText("a"), lineBreaks(emptyLines + 1), FormattedText("```", Format.Markup(BlockToken.FenceMark())),
      FormattedText("\n", Format.Markup(BlockToken.HardBreak())), FormattedText("code"),
      FormattedText("\n", Format.Markup(BlockToken.HardBreak())), FormattedText("```", Format.Markup(BlockToken.FenceMark()))
    ),
    listOf(MarkdownBlock.Paragraph(SegmentRange(0, 1)), MarkdownBlock.CodeBlock(content = SegmentRange(4, 5)))
  )

  @Test
  fun emptyLinesAreParagraphGapsInTerminalRows() {
    val noEmptyLine = paragraphs(terminal = true, emptyLines = 0)
    assertEquals(paragraphGaps(1), paragraphs(terminal = true, emptyLines = 1) - noEmptyLine)
    assertEquals(paragraphGaps(2), paragraphs(terminal = true, emptyLines = 2) - noEmptyLine)
  }

  @Test
  fun emptyLinesAreFullLinesInBubbles() {
    assertEquals(22, paragraphs(terminal = false, emptyLines = 1) - paragraphs(terminal = false, emptyLines = 0))
  }

  @Test
  fun cardGapIsLargerOfParagraphGapAndEmptyLines() {
    val oneGap = paragraphAndCode(emptyLines = 0)
    assertEquals(oneGap, paragraphAndCode(emptyLines = 1))
    assertEquals(paragraphGaps(2) - paragraphGaps(1), paragraphAndCode(emptyLines = 2) - oneGap)
  }

  @Test
  fun typingLineIsParagraphGapBelowLastCard() {
    renderScene {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        MarkdownBlocks(
          ruleText, ruleBlocks, 0, MaterialTheme.typography.body1, Modifier, Color.Gray,
          blockText = { _, _, _, _, _, _ -> }, metaLine = { Box(Modifier.record(placed, "typing")) }, ruleMinWidth = 0.dp
        )
      }
    }
    assertEquals(1 + paragraphGaps(1), placed.getValue("typing").y)
  }

  @Test
  fun reportOfRulesIsDrawnAsOneText() {
    val prefix = AnnotatedString("Spam: ")
    assertEquals(1, height(terminal = true, ruleText, ruleBlocks))
    val oneText = height(terminal = true, ruleText, null, prefix) to placed.getValue("drawn").width
    assertEquals(oneText, height(terminal = true, ruleText, ruleBlocks, prefix) to placed.getValue("drawn").width)
  }
}
