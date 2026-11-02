package chat.simplex.app

import chat.simplex.common.views.chat.safeMarkdown
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatExportMarkdownTest {
  @Test
  fun testImagesAndHtmlEscaped() {
    assertEquals("!\\[x](https://example.com/a.png) [link](https://example.com)", safeMarkdown("![x](https://example.com/a.png) [link](https://example.com)"))
    assertEquals("\\<img src=x> \\</b> \\<!-- c --> \\<?x", safeMarkdown("<img src=x> </b> <!-- c --> <?x"))
    assertEquals("a < b, a <= b", safeMarkdown("a < b, a <= b"))
    assertEquals("\\<b>", safeMarkdown("\\<b>"))
    assertEquals("\\\\\\<b>", safeMarkdown("\\\\<b>"))
  }

  @Test
  fun testUnclosedFenceClosed() {
    assertEquals("```\ncode\n```", safeMarkdown("```\ncode"))
    assertEquals("````\n```\n````", safeMarkdown("````\n```"))
    assertEquals("```\n```\u00A0\n```", safeMarkdown("```\n```\u00A0"))
  }

  @Test
  fun testClosedFenceUnchanged() {
    assertEquals("```kotlin\ncode\n```  ", safeMarkdown("```kotlin\ncode\n```  "))
    assertEquals("~~~\n```\n~~~", safeMarkdown("~~~\n```\n~~~"))
    assertEquals("```a`b\ntext", safeMarkdown("```a`b\ntext"))
    assertEquals("    ```\ntext", safeMarkdown("    ```\ntext"))
  }
}
