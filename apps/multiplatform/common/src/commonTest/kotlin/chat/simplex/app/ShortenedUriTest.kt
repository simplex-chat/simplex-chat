package chat.simplex.app

import chat.simplex.common.views.helpers.shortenedUri
import kotlin.test.Test
import kotlin.test.assertEquals

class ShortenedUriTest {
  @Test
  fun testShortUriUnchanged() {
    val uri = "https://example.com/page?q=1"
    assertEquals(uri, shortenedUri(uri))
  }

  @Test
  fun testLongPathShortened() {
    val uri = "https://example.com/" + "a".repeat(200)
    assertEquals(uri.substring(0, 159) + "…", shortenedUri(uri))
  }

  @Test
  fun testLongHostShownInFull() {
    val authority = "https://" + "a".repeat(180) + ".example.com"
    assertEquals("$authority…", shortenedUri("$authority/" + "b".repeat(100)))
    assertEquals(authority, shortenedUri(authority))
  }

  @Test
  fun testLongUserInfoShownInFull() {
    val authority = "https://" + "a".repeat(180) + "@example.com"
    assertEquals("$authority…", shortenedUri("$authority?q=" + "b".repeat(100)))
  }
}
