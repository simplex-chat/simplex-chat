package chat.simplex.app

import chat.simplex.common.views.migration.strHasSimplexFileLink
import kotlin.test.Test
import kotlin.test.assertTrue

class MigrationLinkTest {
  @Test
  fun acceptsAFileLinkWithoutTheSlashAfterTheScheme() {
    assertTrue(strHasSimplexFileLink("simplex:file#/?desc=abc"))
  }
}
