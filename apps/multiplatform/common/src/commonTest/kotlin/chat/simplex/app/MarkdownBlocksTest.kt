package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.serialization.builtins.ListSerializer
import kotlin.reflect.KClass
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class MarkdownBlocksTest {
  @Test
  fun testDecodeMarkers() {
    val fts = json.decodeFromString(
      ListSerializer(FormattedText.serializer()),
      """[{"format":{"type":"blockStart","block":{"type":"paragraph"}},"text":""},""" +
        """{"format":{"type":"blockStart","block":{"type":"heading","level":2}},"text":"## "},""" +
        """{"format":{"type":"blockStart","block":{"type":"codeBlock","language":"bash"}},"text":"```bash\n"},""" +
        """{"format":{"type":"blockStart","block":{"type":"codeBlock"}},"text":"```\n"},""" +
        """{"format":{"type":"blockStart","block":{"type":"quote"}},"text":"> "},""" +
        """{"format":{"type":"blockStart","block":{"type":"listItem","number":"2.","indent":1}},"text":"  2. "},""" +
        """{"format":{"type":"blockStart","block":{"type":"listItem","indent":0}},"text":"- "},""" +
        """{"format":{"type":"blockStart","block":{"type":"table","columns":["none","left","center","right"]}},"text":"| "},""" +
        """{"format":{"type":"blockStart","block":{"type":"rule"}},"text":"---"},""" +
        """{"format":{"type":"blockEnd"},"text":"\n```"},""" +
        """{"format":{"type":"lineBreak"},"text":"\n> "},""" +
        """{"format":{"type":"cellBreak"},"text":" | "}]"""
    )
    val blocks = fts.take(9).map { (it.format as Format.BlockStart).block }
    assertEquals(MarkdownBlock.Paragraph, blocks[0])
    assertEquals(2, (blocks[1] as MarkdownBlock.Heading).level)
    assertEquals("bash", (blocks[2] as MarkdownBlock.CodeBlock).language)
    assertNull((blocks[3] as MarkdownBlock.CodeBlock).language)
    assertEquals(MarkdownBlock.Quote, blocks[4])
    val item = blocks[5] as MarkdownBlock.ListItem
    assertEquals("2.", item.number)
    assertEquals(1, item.indent)
    val itemNoNumber = blocks[6] as MarkdownBlock.ListItem
    assertNull(itemNoNumber.number)
    assertEquals(0, itemNoNumber.indent)
    assertEquals(listOf(TableAlign.None, TableAlign.Left, TableAlign.Center, TableAlign.Right), (blocks[7] as MarkdownBlock.Table).columns)
    assertEquals(MarkdownBlock.Rule, blocks[8])
    assertEquals(
      listOf(s("\n```", Format.BlockEnd::class), s("\n> ", Format.LineBreak::class), s(" | ", Format.CellBreak::class)),
      shape(fts.drop(9))
    )
  }

  @Test
  fun testNoMarkers() {
    val fts = listOf(FormattedText("a\nb", Format.Bold()), FormattedText("\nc"))
    assertEquals(fts, markdownSegments(fts))
    assertNull(markdownSegments(null))
  }

  @Test
  fun testBlockSegments() {
    val fts = listOf(
      start(MarkdownBlock.Heading(1), "# "), FormattedText("Title"), FormattedText("x", Format.Italic()), end(),
      FormattedText("\n"),
      start(MarkdownBlock.Paragraph, ""), FormattedText("a "), FormattedText("b", Format.Bold()), lineBreak("\n"), FormattedText("c"), end(),
      FormattedText("\n\n"),
      start(MarkdownBlock.ListItem(null, 0), "- "), FormattedText("x"), lineBreak("\n  "), FormattedText("y"), end(),
      FormattedText("\n"),
      start(MarkdownBlock.ListItem("2.", 1), "  2. "), FormattedText("z"), end(),
      FormattedText("\n"),
      start(MarkdownBlock.Quote, "> "), FormattedText("q1"), lineBreak("\n> "), FormattedText("q2"), lineBreak("\n>"), lineBreak("\n> "), FormattedText("q3", Format.Bold()), end(),
      FormattedText("\n"),
      start(MarkdownBlock.CodeBlock("bash"), "```bash\n"), FormattedText("ls -l"), lineBreak("\n"), lineBreak("\n"), FormattedText("pwd"), end("\n```"),
      FormattedText("\n"),
      start(MarkdownBlock.Rule, "---"), end()
    )
    assertEquals(
      listOf(
        s("Title", Format.Bold::class), s("x", Format.Italic::class),
        s("\n"),
        s("a "), s("b", Format.Bold::class), s(" "), s("c"),
        s("\n\n"),
        s("• "), s("x"), s(" "), s("y"),
        s("\n"),
        s("    2. "), s("z"),
        s("\n"),
        s("│ "), s("q1"), s(" "), s("q2"), s("\n│ "), s("\n│ "), s("q3", Format.Bold::class),
        s("\n"),
        snippet("ls -l"), snippet("\n"), snippet("\n"), snippet("pwd"),
        s("\n"),
        s("———")
      ),
      shape(markdownSegments(fts))
    )
  }

  @Test
  fun testHardBreaks() {
    val fts = listOf(
      start(MarkdownBlock.Paragraph, ""), FormattedText("```"), lineBreak("\n"), lineBreak("\n"), FormattedText("```"), end(),
      FormattedText("\n"),
      start(MarkdownBlock.ListItem(null, 0), "- "), FormattedText("h"), lineBreak("\n"), FormattedText("```"), lineBreak("\n   "), lineBreak("\n"), FormattedText("```"), end()
    )
    assertEquals(
      listOf(
        s("```"), s("\n"), s("\n"), s("```"),
        s("\n"),
        s("• "), s("h"), s(" "), s("```"), s("\n"), s("\n"), s("```")
      ),
      shape(markdownSegments(fts))
    )
  }

  @Test
  fun testSegmentsAfterBlocks() {
    val fts = listOf(
      start(MarkdownBlock.ListItem(null, 0), "- "), FormattedText("Berlin"), end(),
      FormattedText(" "), FormattedText("Read more", Format.Modal(Format.Modal.Description, "- Berlin\n- Developer"))
    )
    assertEquals(
      listOf(s("• "), s("Berlin"), s(" "), s("Read more", Format.Modal::class)),
      shape(markdownSegments(fts))
    )
  }

  @Test
  fun testTableSegments() {
    val fts = listOf(
      start(MarkdownBlock.Table(listOf(TableAlign.None, TableAlign.Left, TableAlign.Right, TableAlign.Center)), "| "),
      FormattedText("a"), cellBreak(" | "), FormattedText("b"), cellBreak(" | "), FormattedText("c"), cellBreak(" | "), FormattedText("d"),
      lineBreak(" |\n|---|:--|--:|:-:|\n| "),
      FormattedText("xxx"), cellBreak(" | "), FormattedText("yyy", Format.Bold()), cellBreak(" | "), FormattedText("zzz"), cellBreak(""),
      end(" |")
    )
    assertEquals(
      listOf(
        snippet("a"), snippet("  "),
        snippet(" │ "), snippet("b"), snippet("  "),
        snippet(" │   "), snippet("c"),
        snippet(" │ "), snippet("d"),
        snippet("\n───" + "─┼─" + "───" + "─┼─" + "───" + "─┼─" + "─"),
        snippet("\n"),
        snippet("xxx"),
        snippet(" │ "), s("yyy", Format.Bold::class),
        snippet(" │ "), snippet("zzz"),
        snippet(" │ "), snippet(" ")
      ),
      shape(markdownSegments(fts))
    )
  }

  @Test
  fun testTableCenterAndEmoji() {
    val fts = listOf(
      start(MarkdownBlock.Table(listOf(TableAlign.Center, TableAlign.None)), ""),
      FormattedText("abcd"), cellBreak(" | "), FormattedText("ab"),
      lineBreak("\n:-:|--\n"),
      FormattedText("x"), cellBreak(" | "), FormattedText("👍"),
      end()
    )
    assertEquals(
      listOf(
        snippet("abcd"),
        snippet(" │ "), snippet("ab"),
        snippet("\n────" + "─┼─" + "──"),
        snippet("\n"),
        snippet(" "), snippet("x"), snippet("  "),
        snippet(" │ "), snippet("👍"), snippet(" ")
      ),
      shape(markdownSegments(fts))
    )
  }

  private fun start(block: MarkdownBlock, text: String) = FormattedText(text, Format.BlockStart(block))

  private fun end(text: String = "") = FormattedText(text, Format.BlockEnd())

  private fun lineBreak(text: String) = FormattedText(text, Format.LineBreak())

  private fun cellBreak(text: String) = FormattedText(text, Format.CellBreak())

  private fun s(text: String, format: KClass<out Format>? = null): Pair<String, KClass<out Format>?> = text to format

  private fun snippet(text: String): Pair<String, KClass<out Format>?> = text to Format.Snippet::class

  private fun shape(segments: List<FormattedText>?): List<Pair<String, KClass<out Format>?>>? =
    segments?.map { it.text to it.format?.let { f -> f::class } }
}
