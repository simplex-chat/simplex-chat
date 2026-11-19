package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.platform.ClipEntry
import androidx.compose.ui.platform.Clipboard
import androidx.compose.ui.platform.LocalClipboard
import androidx.compose.ui.platform.NativeClipboard
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performMouseInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.chat.item.MarkdownFileView
import chat.simplex.common.views.chat.item.markdownDocument
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import kotlinx.coroutines.runBlocking
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import kotlin.test.Test
import kotlin.test.assertEquals

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
}

private class RecordingClipboard : Clipboard {
  var text: String? = null

  override suspend fun getClipEntry(): ClipEntry? = null

  override suspend fun setClipEntry(clipEntry: ClipEntry?) {
    text = (clipEntry?.nativeClipEntry as Transferable).getTransferData(DataFlavor.stringFlavor) as String
  }

  override val nativeClipboard: NativeClipboard get() = this
}
