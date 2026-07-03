package chat.simplex.common.views.migration

import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class MigrationLinkTest {
  @Test
  fun strHasSimplexFileLink_acceptsSimplexAndHttpsPrefixes() {
    assertTrue(strHasSimplexFileLink("simplex:/file/test"))
    assertTrue(strHasSimplexFileLink("https://simplex.chat/file/test"))
    assertFalse(strHasSimplexFileLink("https://example.com/file"))
    assertFalse(strHasSimplexFileLink("not a link"))
  }
}