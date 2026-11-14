package chat.simplex.app

import chat.simplex.common.model.Format
import chat.simplex.common.model.FormattedText
import chat.simplex.common.views.chat.sanitizeMessage
import kotlin.test.Test
import kotlin.test.assertEquals

class SanitizeMessageTest {
  private val sanitize: (String) -> String? = { if (it.contains("?utm_source=a")) it.replace("?utm_source=a", "") else null }

  @Test
  fun testKeepsMarkdownAroundSanitizedLink() {
    val text = "**Note:** see https://x.com/?utm_source=a and `code`"
    val parsed = listOf(
      FormattedText("Note:", Format.Bold()),
      FormattedText(" see "),
      FormattedText("https://x.com/?utm_source=a", Format.Uri()),
      FormattedText(" and "),
      FormattedText("code", Format.Snippet())
    )
    val (msg, parsedMsg, pos) = sanitizeMessage(text, parsed, sanitize)
    assertEquals("**Note:** see https://x.com/ and `code`", msg)
    assertEquals("**Note:** see https://x.com/".length, pos)
    assertEquals("https://x.com/", parsedMsg[2].text)
  }

  @Test
  fun testKeepsDelimitersOfHyperlinks() {
    val text = "**[t](https://x.com/?utm_source=a)** and [**u**](https://y.com/?utm_source=a)"
    val parsed = listOf(
      FormattedText("**[t](https://x.com/?utm_source=a)**", Format.HyperLink("t", "https://x.com/?utm_source=a")),
      FormattedText(" and "),
      FormattedText("[**u**](https://y.com/?utm_source=a)", Format.HyperLink("u", "https://y.com/?utm_source=a"))
    )
    val (msg, parsedMsg, pos) = sanitizeMessage(text, parsed, sanitize)
    assertEquals("**[t](https://x.com/)** and [**u**](https://y.com/)", msg)
    assertEquals(msg.length, pos)
    assertEquals("https://y.com/", (parsedMsg[2].format as Format.HyperLink).linkUri)
  }

  @Test
  fun testSkipsEarlierCopyOfLink() {
    val text = "`https://x.com/?utm_source=a` https://x.com/?utm_source=a"
    val parsed = listOf(
      FormattedText("https://x.com/?utm_source=a", Format.Snippet()),
      FormattedText(" "),
      FormattedText("https://x.com/?utm_source=a", Format.Uri())
    )
    assertEquals("`https://x.com/?utm_source=a` https://x.com/", sanitizeMessage(text, parsed, sanitize).first)
  }

  @Test
  fun testWithoutSanitizedLink() {
    val text = "**a** https://x.com/"
    val parsed = listOf(FormattedText("a", Format.Bold()), FormattedText(" "), FormattedText("https://x.com/", Format.Uri()))
    assertEquals(Triple(text, parsed, null), sanitizeMessage(text, parsed, sanitize))
  }
}
