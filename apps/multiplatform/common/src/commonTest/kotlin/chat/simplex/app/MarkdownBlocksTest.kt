package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.itemDisplayParts
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

  private val tableCenterEmoji = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"text":"abcd"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"ab"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" |"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"delimiterRow","columns":["center","none"]}},"text":"|:-:|--|"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"text":"x"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"👍"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" |"}],"markdownBlocks":[{"type":"table","columns":["center","none"],"header":[{"from":1,"to":2},{"from":3,"to":4}],"rows":[[{"from":9,"to":10},{"from":11,"to":12}]]}]}""")

  private val tableMentionCjk = parsed("""{"formattedText":[{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"format":{"type":"mention","memberName":"bob"},"text":"@bob"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"text":"中文"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" |"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"delimiterRow","columns":["none","none"]}},"text":"|---|---|"},{"format":{"type":"markup","token":{"type":"hardBreak"}},"text":"\n"},{"format":{"type":"markup","token":{"type":"rowMark"}},"text":"| "},{"text":"x"},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":" | "},{"format":{"type":"markup","token":{"type":"cellMark"}},"text":"|"}],"markdownBlocks":[{"type":"table","columns":["none","none"],"header":[{"from":1,"to":2},{"from":3,"to":4}],"rows":[[{"from":9,"to":10},{"from":11,"to":11}]]}]}""")

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
  fun testHeadingAndParagraph() {
    assertEquals(
      listOf("Title x" to BlockFont.Bold, "\n" to BlockFont.Regular, "Para a b\nc" to BlockFont.Regular),
      drawn(headingAndParagraph)
    )
  }

  @Test
  fun testGaps() {
    assertEquals(
      listOf("\n\n" to BlockFont.Regular, "a" to BlockFont.Regular, "\n\n\n" to BlockFont.Regular, "h" to BlockFont.Bold, "\n" to BlockFont.Regular),
      drawn(gaps)
    )
  }

  @Test
  fun testNestedList() {
    assertEquals(
      listOf("• ", "x y", "\n", "    • ", "z", "\n", "2. ", "w").map { it to BlockFont.Regular },
      drawn(nestedList)
    )
  }

  @Test
  fun testListEntryLines() {
    val fts = listOf(
      FormattedText("- ", Format.Markup(BlockToken.ListMark())), FormattedText("a"),
      FormattedText("\n", Format.Markup(BlockToken.HardBreak())), FormattedText("  ", Format.Markup(BlockToken.Indent())), FormattedText("b")
    )
    val blocks = listOf(MarkdownBlock.ItemList(listOf(ListEntry(null, SegmentRange(1, 5), emptyList()))))
    assertEquals("• a\n  b", drawnText(fts, blocks))
  }

  @Test
  fun testQuote() {
    assertEquals(
      listOf("│ ", "q1 q2", "\n│ ", "", "\n│ ", "q3").map { it to BlockFont.Regular },
      drawn(quote)
    )
  }

  @Test
  fun testCodeBlockAndRule() {
    assertEquals(
      listOf("ls -l\n\npwd" to BlockFont.Monospace, "\n" to BlockFont.Regular, "———" to BlockFont.Regular),
      drawn(codeBlockAndRule)
    )
  }

  @Test
  fun testTableAlignments() {
    assertEquals(
      "a  " + " │ " + "b  " + " │ " + "  c" + " │ " + "d" + "\n───" + "─┼─" + "───" + "─┼─" + "───" + "─┼─" + "─" +
        "\nxxx" + " │ " + "yyy" + " │ " + "zzz",
      drawnText(tableAligned.formattedText!!, tableAligned.markdownBlocks!!)
    )
    assert(drawn(tableAligned).all { it.second == BlockFont.Monospace })
  }

  @Test
  fun testTableCenterAndEmoji() {
    assertEquals(
      "abcd" + " │ " + "ab" + "\n────" + "─┼─" + "──" +
        "\n x  " + " │ " + "👍",
      drawnText(tableCenterEmoji.formattedText!!, tableCenterEmoji.markdownBlocks!!)
    )
  }

  @Test
  fun testTableWidthsOfDisplayedTexts() {
    val fts = tableMentionCjk.formattedText!!
    val displayed = { ft: FormattedText -> if (ft.format is Format.Mention) "@Robert" else display(ft) }
    val text = blockParts(fts, tableMentionCjk.markdownBlocks!!, displayed).joinToString("") { part ->
      when (part) {
        is BlockPart.Segments -> part.range.joinToString("") { displayed(fts[it]) }
        is BlockPart.Text -> part.text
      }
    }
    assertEquals(
      "@Robert" + " │ " + "中文" + "\n───────" + "─┼─" + "────" +
        "\nx      " + " │ " + "    ",
      text
    )
  }

  @Test
  fun testTableColumnWidthLimit() {
    val long = "x".repeat(50)
    val fts = listOf(
      FormattedText("| ", Format.Markup(BlockToken.RowMark())), FormattedText(long), FormattedText(" |", Format.Markup(BlockToken.CellMark())),
      FormattedText("\n", Format.Markup(BlockToken.HardBreak())), FormattedText("|-|", Format.Markup(BlockToken.DelimiterRow(listOf(TableAlign.Right)))),
      FormattedText("\n", Format.Markup(BlockToken.HardBreak())),
      FormattedText("| ", Format.Markup(BlockToken.RowMark())), FormattedText("y"), FormattedText(" |", Format.Markup(BlockToken.CellMark()))
    )
    val blocks = listOf(MarkdownBlock.Table(listOf(TableAlign.Right), listOf(SegmentRange(1, 2)), listOf(listOf(SegmentRange(7, 8)))))
    assertEquals(long + "\n" + "─".repeat(MAX_TABLE_COLUMN_WIDTH) + "\n" + " ".repeat(MAX_TABLE_COLUMN_WIDTH - 1) + "y", drawnText(fts, blocks))
  }

  @Test
  fun testRangesBeyondList() {
    val fts = headingAndParagraph.formattedText!!.take(5)
    assertEquals("Title x\nPara a", drawnText(fts, headingAndParagraph.markdownBlocks!!))
    assertEquals("", drawnText(emptyList(), codeBlockAndRule.markdownBlocks!!.take(1)))
  }

  @Test
  fun testDisplayWidth() {
    assertEquals(3, displayWidth("abc"))
    assertEquals(4, displayWidth("中文"))
    assertEquals(2, displayWidth("👍"))
    assertEquals(2, displayWidth("👍🏽"))
    assertEquals(2, displayWidth("❤️"))
    assertEquals(1, displayWidth("é"))
    assertEquals(2, displayWidth(String(intArrayOf(0x1F468, 0x200D, 0x1F469, 0x200D, 0x1F467), 0, 5)))
    assertEquals(2, displayWidth(String(intArrayOf(0x1F1FA, 0x1F1F8), 0, 2)))
  }

  @Test
  fun testTokenDisplayText() {
    assertEquals(
      listOf(" ", "\n", "\n", "\n\n\n", "", "", "", "", "", "", ""),
      listOf(
        BlockToken.SoftBreak(), BlockToken.LineBreak(), BlockToken.HardBreak(), BlockToken.ParagraphBreak(3), BlockToken.Indent(),
        BlockToken.HeadingMark(2), BlockToken.ListMark("1."), BlockToken.QuoteMark(), BlockToken.FenceMark(null), BlockToken.CellMark(), BlockToken.RuleMark()
      ).map { display(FormattedText("syntax", Format.Markup(it))) }
    )
    assertEquals("", display(FormattedText("\\", Format.Escape())))
  }

  @Test
  fun testItemDisplayParts() {
    val item = ChatItem.getSampleData(text = "> q1\n> q2\n>\n> **q3**").copy(formattedText = quote.formattedText, markdownBlocks = quote.markdownBlocks)
    val parts = itemDisplayParts(item, SimplexLinkMode.DESCRIPTION)!!
    assertEquals("│ q1 q2\n│ \n│ q3", parts.joinToString("") { it.first })
    assertEquals(listOf("│ " to "│ ", "q1" to "q1", " " to " ", "" to "", "q2" to "q2"), parts.take(5))
    val plain = item.copy(formattedText = escapes.formattedText, markdownBlocks = null)
    assertEquals(listOf("text " to "text ", "" to "\\", "*not" to "*not", "" to "\\", "*\nmore" to "*\nmore"), itemDisplayParts(plain, SimplexLinkMode.DESCRIPTION))
  }

  private val SegmentRange.pair: Pair<Int, Int> get() = from to to

  private fun parsed(s: String): ParsedFormattedText = json.decodeFromString(ParsedFormattedText.serializer(), s)

  private fun display(ft: FormattedText): String = segmentDisplayText(ft, null, SimplexLinkMode.DESCRIPTION)

  private fun drawn(p: ParsedFormattedText): List<Pair<String, BlockFont>> {
    val fts = p.formattedText!!
    return blockParts(fts, p.markdownBlocks!!, ::display).map { part ->
      when (part) {
        is BlockPart.Segments -> part.range.joinToString("") { display(fts[it]) } to part.font
        is BlockPart.Text -> part.text to part.font
      }
    }
  }

  private fun drawnText(fts: List<FormattedText>, blocks: List<MarkdownBlock>): String =
    blockParts(fts, blocks, ::display).joinToString("") { part ->
      when (part) {
        is BlockPart.Segments -> part.range.joinToString("") { display(fts[it]) }
        is BlockPart.Text -> part.text
      }
    }
}
