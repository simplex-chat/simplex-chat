package chat.simplex.app

import chat.simplex.common.platform.saveDialogFileName
import kotlin.test.Test
import kotlin.test.assertEquals

class SaveDialogFileNameTest {
  @Test
  fun testLeadingDotsRemoved() {
    assertEquals("bashrc", saveDialogFileName(".bashrc"))
    assertEquals("hidden.txt", saveDialogFileName("..hidden.txt"))
  }

  @Test
  fun testOtherNamesUnchanged() {
    assertEquals("report.pdf", saveDialogFileName("report.pdf"))
    assertEquals("archive.tar.gz", saveDialogFileName("archive.tar.gz"))
    assertEquals("...", saveDialogFileName("..."))
  }
}
