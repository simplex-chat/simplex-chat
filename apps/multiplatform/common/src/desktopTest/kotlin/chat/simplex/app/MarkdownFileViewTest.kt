package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.platform.ClipEntry
import androidx.compose.ui.platform.Clipboard
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboard
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.NativeClipboard
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performMouseInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.text.AnnotatedString
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.item.MarkdownFileCopyButton
import chat.simplex.common.views.chat.item.MarkdownFileView
import chat.simplex.common.views.chat.item.markdownDocument
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import kotlinx.coroutines.runBlocking
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

@OptIn(ExperimentalTestApi::class)
class MarkdownFileViewTest {
  @Test
  fun copiedSelectionKeepsLineBreaks() = runComposeUiTest {
    val textView = appPrefs.markdownFileTextView.get()
    appPrefs.markdownFileTextView.set(true)
    try {
      val clipboard = RecordingClipboard()
      val document = runBlocking { markdownDocument("alpha\n\ngamma".toByteArray()) }
      setContent {
        CompositionLocalProvider(LocalClipboard provides clipboard, LocalAppBarHandler provides rememberAppBarHandler()) {
          MarkdownFileView("test.md", document)
        }
      }
      val first = onNodeWithText("alpha").fetchSemanticsNode().boundsInRoot
      val last = onNodeWithText("gamma").fetchSemanticsNode().boundsInRoot
      onRoot().performMouseInput {
        moveTo(Offset(first.left + 1f, first.center.y))
        press()
        moveTo(Offset(last.right + 10f, last.center.y))
        release()
      }
      onRoot().performKeyInput { pressKey(Key.Copy) }
      waitForIdle()
      assertEquals("alpha\n\ngamma", clipboard.text)
    } finally {
      appPrefs.markdownFileTextView.set(textView)
    }
  }

  @Test
  fun copyButtonWarnsAbove128KB() = runComposeUiTest {
    var copied: String? = null
    val clipboard = object : ClipboardManager {
      override fun setText(annotatedString: AnnotatedString) { copied = annotatedString.text }
      override fun getText(): AnnotatedString? = null
    }
    val document = runBlocking { markdownDocument("alpha\n\ngamma".toByteArray()) }
    var fileSize by mutableStateOf(131_072)
    try {
      setContent {
        CompositionLocalProvider(LocalClipboardManager provides clipboard) {
          MarkdownFileCopyButton(document.text, fileSize, null)
        }
        AlertManager.shared.showInView()
      }
      onNodeWithContentDescription("Copy").performClick()
      assertEquals("alpha\n\ngamma", copied)
      copied = null
      fileSize = 131_073
      onNodeWithContentDescription("Copy").performClick()
      onNodeWithText("Your file is 128 KB. Do you want to copy it?").assertExists()
      assertNull(copied)
      onNodeWithText("Copy").performClick()
      assertEquals("alpha\n\ngamma", copied)
    } finally {
      AlertManager.shared.hideAllAlerts()
      simplexWindowState.toasts.clear()
    }
  }

  @Test
  fun copyButtonRefusesAboveHardLimit() = runComposeUiTest {
    var copied: String? = null
    val clipboard = object : ClipboardManager {
      override fun setText(annotatedString: AnnotatedString) { copied = annotatedString.text }
      override fun getText(): AnnotatedString? = null
    }
    try {
      setContent {
        CompositionLocalProvider(LocalClipboardManager provides clipboard) {
          MarkdownFileCopyButton("alpha", 524_289, 524_288)
        }
        AlertManager.shared.showInView()
      }
      onNodeWithContentDescription("Copy").performClick()
      onNodeWithText("Only files up to 512 KB can be copied. Save the file to open it in another app.").assertExists()
      assertNull(copied)
    } finally {
      AlertManager.shared.hideAllAlerts()
    }
  }
}

private class RecordingClipboard : Clipboard {
  var text: String? = null

  override suspend fun getClipEntry(): ClipEntry? = null

  override suspend fun setClipEntry(clipEntry: ClipEntry?) {
    text = (clipEntry?.nativeClipEntry as Transferable).getTransferData(DataFlavor.stringFlavor) as String
  }

  override val nativeClipboard: NativeClipboard get() = this
}
