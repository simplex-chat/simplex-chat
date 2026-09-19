package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.CancellationException
import java.io.ByteArrayInputStream
import java.io.IOException
import kotlin.io.path.createTempFile
import kotlin.test.*

class AttachmentMaximumTest {
  @Test
  fun metadataIsAdvisoryExceptForKnownExcess() {
    try {
      for (size in listOf(null, -1L, 0L, 3L, 4L)) {
        assertTrue(attachmentSizeAllowed(4) { size })
        assertFalse(AlertManager.shared.hasAlertsShown())
      }
      assertTrue(attachmentSizeAllowed(4) { throw IOException("metadata unavailable") })
      assertFalse(attachmentSizeAllowed(4) { 5 })
      assertTrue(AlertManager.shared.hasAlertsShown())
    } finally { AlertManager.shared.hideAllAlerts() }
  }

  @Test
  fun metadataCancellationPropagates() {
    val cancellation = CancellationException("selection cancelled")
    assertSame(cancellation, assertFailsWith<CancellationException> { attachmentSizeAllowed(4) { throw cancellation } })
  }

  @Test
  fun selectedFileCanGrowWithinCurrentMaximum() {
    val file = createTempFile().toFile()
    val dest = createTempFile().toFile()
    try {
      file.writeText("a")
      val compose = mutableStateOf(ComposeState(useLinkPreviews = false, maxFileSize = 4))
      compose.processPickedFile(file.toURI(), null)
      assertIs<ComposePreview.FilePreview>(compose.value.preview)
      file.appendText("bcd")
      file.inputStream().use { copyInputStreamToFile(it, dest, compose.value.maxFileSize) }
      assertEquals("abcd", dest.readText())
      for (metadata in listOf(1L, null)) {
        assertTrue(attachmentSizeAllowed(4) { metadata })
        file.appendText("e")
        assertFailsWith<FileTooLargeException> { file.inputStream().use { copyInputStreamToFile(it, dest, 4) } }
        assertFalse(dest.exists())
        assertFailsWith<FileTooLargeException> { readInputStreamBounded(ByteArrayInputStream(file.readBytes()), 4) }
        file.writeText("abcd")
      }
    } finally { file.delete(); dest.delete() }
  }
}
