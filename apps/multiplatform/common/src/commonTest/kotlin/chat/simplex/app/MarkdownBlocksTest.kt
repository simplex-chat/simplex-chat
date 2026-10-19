package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.itemDisplay
import chat.simplex.common.views.chat.item.segmentDisplayText
import chat.simplex.common.views.helpers.ParsedFormattedText
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertNull

class MarkdownBlocksTest {
  private val headingAndParagraph = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"headingMark","level":1}},"text":"# "},{"text":"Title "},{"format":{"type":"bold","singleStar":true},"text":"x"},{"format":{"type":"markup","token":{"type":"lineBreak"}},"text":"\n"},{"text":"Para a"},{"format":{"type":"markup","token":{"type":"softBreak"}},"text":"\n"},{"text":"b"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"  \n"},{"text":"c"}],"markdownBlocks":[{"type":"heading","level":1,"content":{"from":1,"to":3}},{"type":"paragraph","content":{"from":4,"to":9}}]}""")

  private val nestedList = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"listMark"}},"text":"- "},{"text":"x"},{"format":{"type":"markup","token":{"type":"softBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"indent"}},"text":"  "},{"text":"y"},{"format":{"type":"markup","token":{"type":"lineBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"listMark"}},"text":"  - "},{"text":"z"},{"format":{"type":"markup","token":{"type":"lineBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"listMark","number":"2."}},"text":"2. "},{"text":"w"}],"markdownBlocks":[{"type":"list","items":[{"content":{"from":1,"to":5},"items":[{"content":{"from":7,"to":8},"items":[]}]},{"number":"2.","content":{"from":10,"to":11},"items":[]}]}]}""")

  private val quote = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"quoteMark"}},"text":"> "},{"text":"q1"},{"format":{"type":"markup","token":{"type":"softBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"quoteMark"}},"text":"> "},{"text":"q2"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"quoteMark"}},"text":">"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"quoteMark"}},"text":"> "},{"format":{"type":"bold","singleStar":false},"text":"q3"}],"markdownBlocks":[{"type":"quote","content":{"from":1,"to":10}}]}""")

  private val codeBlockAndRule = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"fenceMark","language":"bash"}},"text":"```bash"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"text":"ls -l"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"text":"pwd"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"fenceMark"}},"text":"```"},{"format":{"type":"markup","token":{"type":"lineBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"ruleMark"}},"text":"---"}],"markdownBlocks":[{"type":"codeBlock","language":"bash","content":{"from":2,"to":6}},{"type":"rule","mark":{"from":9,"to":10}}]}""")

  private val tableAligned = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"text":"a"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"b"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"c"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"d"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" |"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"delimiterRow","columns":["none","left","right","center"]}},"text":"|---|:--|--:|:-:|"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"text":"xxx"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"format":{"type":"bold","singleStar":false},"text":"yyy"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"zzz"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" |"}],"markdownBlocks":[{"type":"table","columns":["none","left","right","center"],"header":[{"from":1,"to":2},{"from":3,"to":4},{"from":5,"to":6},{"from":7,"to":8}],"rows":[[{"from":13,"to":14},{"from":15,"to":16},{"from":17,"to":18}]]}]}""")

  private val gaps = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"paragraphBreak","lineBreaks":2}},"text":"\n\n"},{"text":"a"},{"format":{"type":"markup","token":{"type":"paragraphBreak","lineBreaks":3}},"text":"\n\n\n"},{"format":{"type":"markup","token":{"type":"headingMark","level":1}},"text":"# "},{"text":"h"},{"format":{"type":"markup","token":{"type":"lineBreak"}},"text":"\n"}],"markdownBlocks":[{"type":"paragraph","content":{"from":1,"to":2}},{"type":"heading","level":1,"content":{"from":4,"to":5}}]}""")

  private val escapes = parsed("""{"formattedText":[{"text":"text "},{"format":{"type":"escape"},"text":"\\"},{"text":"*not"},{"format":{"type":"escape"},"text":"\\"},{"text":"*\nmore"}]}""")

  @Test
  fun testDecodeTokens() {
    val tokens = (headingAndParagraph.formattedText!! + nestedList.formattedText!! + codeBlockAndRule.formattedText!! + tableAligned.formattedText!! + gaps.formattedText!!)
      .mapNotNull { (it.format as? Format.Markup)?.token }
    assertEquals(1, tokens.filterIsInstance<BlockToken.HeadingMark>().first().level)
    assertEquals(listOf(null, null, "2."), tokens.filterIsInstance<BlockToken.ListMark>().map { it.number })
    assertEquals(listOf("bash", null), tokens.filterIsInstance<BlockToken.FenceMark>().map { it.language })
    assertEquals(listOf(TableAlign.None, TableAlign.Left, TableAlign.Right, TableAlign.Center), tokens.filterIsInstance<BlockToken.DelimiterRow>().first().columns)
    assertEquals(listOf(2, 3), tokens.filterIsInstance<BlockToken.ParagraphBreak>().map { it.lineBreaks })
    listOf(
      BlockToken.LineBreak::class, BlockToken.SoftBreak::class, BlockToken.HardBreak::class, BlockToken.Indent::class,
      BlockToken.RowMark::class, BlockToken.CellMark::class, BlockToken.RuleMark::class
    ).forEach { kind -> assert(tokens.any { kind.isInstance(it) }) { "no $kind" } }
    assertIs<BlockToken.QuoteMark>((quote.formattedText!![0].format as Format.Markup).token)
    val bold = headingAndParagraph.formattedText!!.mapNotNull { it.format as? Format.Bold } + quote.formattedText!!.mapNotNull { it.format as? Format.Bold }
    assertEquals(listOf(true, false), bold.map { it.singleStar })
    assertEquals(listOf("text ", "", "*not", "", "*\nmore"), escapes.formattedText!!.map(::display))
    assertNull(escapes.markdownBlocks)
  }

  @Test
  fun testDecodeBlocks() {
    val heading = assertIs<MarkdownBlock.Heading>(headingAndParagraph.markdownBlocks!![0])
    assertEquals(1, heading.level)
    assertEquals(1 to 3, heading.content.pair)
    assertEquals(4 to 9, assertIs<MarkdownBlock.Paragraph>(headingAndParagraph.markdownBlocks!![1]).content.pair)
    val list = assertIs<MarkdownBlock.ItemList>(nestedList.markdownBlocks!!.single())
    assertEquals(listOf(null, "2."), list.items.map { it.number })
    assertEquals(7 to 8, list.items[0].items.single().content.pair)
    assertEquals(1 to 10, assertIs<MarkdownBlock.Quote>(quote.markdownBlocks!!.single()).content.pair)
    val code = assertIs<MarkdownBlock.CodeBlock>(codeBlockAndRule.markdownBlocks!![0])
    assertEquals("bash", code.language)
    assertEquals(2 to 6, code.content.pair)
    assertEquals(9 to 10, assertIs<MarkdownBlock.Rule>(codeBlockAndRule.markdownBlocks!![1]).mark.pair)
    val table = assertIs<MarkdownBlock.Table>(tableAligned.markdownBlocks!!.single())
    assertEquals(listOf(TableAlign.None, TableAlign.Left, TableAlign.Right, TableAlign.Center), table.columns)
    assertEquals(listOf(1 to 2, 3 to 4, 5 to 6, 7 to 8), table.header.map { it.pair })
    assertEquals(listOf(listOf(13 to 14, 15 to 16, 17 to 18)), table.rows.map { row -> row.map { it.pair } })
  }

  @Test
  fun testBlockTextRanges() {
    assertEquals(listOf(1 to 3, 4 to 9), texts(headingAndParagraph).map { it.first })
    assertEquals(listOf(1 to 5, 7 to 8, 10 to 11), texts(nestedList).map { it.first })
    assertEquals(listOf(1 to 10), texts(quote).map { it.first })
    assertEquals(listOf(2 to 6), texts(codeBlockAndRule).map { it.first })
    assertEquals(
      listOf(1 to 2 to false, 3 to 4 to true, 5 to 6 to true, 7 to 8 to true, 13 to 14 to false, 15 to 16 to true, 17 to 18 to true),
      texts(tableAligned)
    )
  }

  @Test
  fun testListEntryLevels() {
    val list = assertIs<MarkdownBlock.ItemList>(nestedList.markdownBlocks!!.single())
    assertEquals(listOf(1 to 0, 7 to 1, 10 to 0), listEntries(list.items, 0).map { (entry, level) -> entry.content.from to level })
  }

  @Test
  fun testHeadingSegments() {
    assertEquals(listOf(1, 2), headingSegments(9, headingAndParagraph.markdownBlocks!!).withIndex().filter { it.value }.map { it.index })
    assertEquals(listOf(4), headingSegments(6, gaps.markdownBlocks!!).withIndex().filter { it.value }.map { it.index })
    assertEquals(listOf(1), headingSegments(2, headingAndParagraph.markdownBlocks!!).withIndex().filter { it.value }.map { it.index })
  }

  @Test
  fun testTokenTexts() {
    val tokens = listOf(
      BlockToken.SoftBreak(), BlockToken.LineBreak(), BlockToken.HardBreak(), BlockToken.ParagraphBreak(3), BlockToken.Indent(),
      BlockToken.HeadingMark(2), BlockToken.ListMark("1."), BlockToken.ListMark(null), BlockToken.QuoteMark(), BlockToken.FenceMark(null),
      BlockToken.RowMark(), BlockToken.CellMark(), BlockToken.DelimiterRow(listOf(TableAlign.None)), BlockToken.RuleMark()
    )
    assertEquals(listOf(" ", "\n", "\n", "\n\n\n", "", "", "", "", "", "", "", "", "", ""), tokens.map { it.drawnText })
    assertEquals(listOf(" ", "\n", " ", "\n", "", "", "1. ", "• ", "", "", "", " ", "", ""), tokens.map { it.previewText })
    assertEquals("", display(FormattedText("\\", Format.Escape())))
    assertEquals("Title x\nPara a b c", preview(headingAndParagraph))
    assertEquals("• x y\n• z\n2. w", preview(nestedList))
  }

  @Test
  fun testItemDisplay() {
    val item = ChatItem.getSampleData(text = "> q1\n> q2\n>\n> **q3**").copy(formattedText = quote.formattedText, markdownBlocks = quote.markdownBlocks)
    val display = itemDisplay(item, SimplexLinkMode.DESCRIPTION)!!
    assertEquals("q1 q2\n\nq3", display.parts.joinToString("") { it.first })
    assertEquals(listOf(0), display.bases)
    val table = item.copy(formattedText = tableAligned.formattedText, markdownBlocks = tableAligned.markdownBlocks)
    val tableDisplay = itemDisplay(table, SimplexLinkMode.DESCRIPTION)!!
    assertEquals("a\tb\tc\td\nxxx\tyyy\tzzz", tableDisplay.parts.joinToString("") { it.first })
    assertEquals(listOf(0, 2, 4, 6, 8, 12, 16), tableDisplay.bases)
    val plain = item.copy(formattedText = escapes.formattedText, markdownBlocks = null)
    val plainDisplay = itemDisplay(plain, SimplexLinkMode.DESCRIPTION)!!
    assertEquals(listOf("text " to "text ", "" to "\\", "*not" to "*not", "" to "\\", "*\nmore" to "*\nmore"), plainDisplay.parts)
    assertEquals(listOf(0), plainDisplay.bases)
  }

  private val SegmentRange.pair: Pair<Int, Int> get() = from to to

  private fun parsed(s: String): ParsedFormattedText = json.decodeFromString(ParsedFormattedText.serializer(), s)

  private fun display(ft: FormattedText): String = segmentDisplayText(ft, null, SimplexLinkMode.DESCRIPTION)

  private fun texts(p: ParsedFormattedText): List<Pair<Pair<Int, Int>, Boolean>> =
    blockTextRanges(p.markdownBlocks!!).map { it.range.pair to it.inRow }

  private fun preview(p: ParsedFormattedText): String =
    p.formattedText!!.joinToString("") { ft -> (ft.format as? Format.Markup)?.token?.previewText ?: ft.text }
}
