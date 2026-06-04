package chat.simplex.app

import chat.simplex.common.views.helpers.FileTooLargeException
import chat.simplex.common.views.helpers.copyInputStreamToFile
import java.io.ByteArrayInputStream
import kotlin.io.path.createTempFile
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class UtilsFileCopyTest {
  @Test
  fun testCopyInputStreamAllowsLimitBoundary() {
    val dest = createTempFile().toFile()
    val data = ByteArray(4) { it.toByte() }

    copyInputStreamToFile(ByteArrayInputStream(data), dest, maxBytes = 4)

    assertTrue(dest.exists())
    assertContentEquals(data, dest.readBytes())
    dest.delete()
  }

  @Test
  fun testCopyInputStreamRejectsAndDeletesOversizedOutput() {
    val dest = createTempFile().toFile()
    val data = ByteArray(5) { it.toByte() }

    assertFailsWith<FileTooLargeException> {
      copyInputStreamToFile(ByteArrayInputStream(data), dest, maxBytes = 4)
    }

    assertFalse(dest.exists())
  }
}
