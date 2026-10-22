package chat.simplex.app.ui.harness

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.AnnotatedString
import java.awt.datatransfer.Clipboard
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.Transferable
import java.awt.datatransfer.UnsupportedFlavorException

class TestClipboard : ClipboardManager {
  private val system = Clipboard("uitest").also(::installAsSkikoSystemClipboard)
  var content: AnnotatedString?
    get() = runCatching { system.getData(DataFlavor.stringFlavor) as String }.getOrNull()?.let(::AnnotatedString)
    set(v) { system.setContents(v?.let { StringSelection(it.text) } ?: NO_TEXT, null) }
  override fun getText(): AnnotatedString? = content
  override fun setText(annotatedString: AnnotatedString) { content = annotatedString }
}

private val NO_TEXT = object : Transferable {
  override fun getTransferDataFlavors() = emptyArray<DataFlavor>()
  override fun isDataFlavorSupported(flavor: DataFlavor) = false
  override fun getTransferData(flavor: DataFlavor): Any = throw UnsupportedFlavorException(flavor)
}

private fun installAsSkikoSystemClipboard(clipboard: Clipboard) {
  val lazy = Class.forName("org.jetbrains.skiko.Actuals_awtKt").getDeclaredField("systemClipboard\$delegate").apply { isAccessible = true }.get(null)
  lazy.javaClass.getDeclaredField("_value").apply { isAccessible = true }.set(lazy, clipboard)
}

class RecordingUriHandler : UriHandler {
  val opened = mutableListOf<String>()
  override fun openUri(uri: String) { synchronized(opened) { opened += uri } }
}

@Composable
fun TestRoot(clipboard: TestClipboard, uriHandler: RecordingUriHandler, content: @Composable () -> Unit) {
  CompositionLocalProvider(LocalClipboardManager provides clipboard, LocalUriHandler provides uriHandler) {
    content()
  }
}
