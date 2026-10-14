package chat.simplex.app.ui.harness

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.AnnotatedString

class TestClipboard : ClipboardManager {
  @Volatile var content: AnnotatedString? = null
  override fun getText(): AnnotatedString? = content
  override fun setText(annotatedString: AnnotatedString) { content = annotatedString }
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
