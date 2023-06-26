package chat.simplex.common.platform

import androidx.compose.runtime.*
import chat.simplex.common.DesktopApp
import chat.simplex.common.simplexWindowState
import java.io.*
import java.net.URI

private fun applicationParentPath(): String = try {
  DesktopApp::class.java.protectionDomain!!.codeSource.location.toURI().path
    .replaceAfterLast("/", "")
    .replaceAfterLast(File.separator, "")
    .replace("/", File.separator)
} catch (e: Exception) {
  "./"
}

actual val dataDir: File = File(platform.configPath)
actual val tmpDir: File = File(System.getProperty("java.io.tmpdir"))
actual val cacheDir: File = tmpDir

@Composable
actual fun rememberFileChooserLauncher(getContent: Boolean, onResult: (URI?) -> Unit): FileChooserLauncher =
  remember { FileChooserLauncher(getContent, onResult) }

actual class FileChooserLauncher actual constructor() {
  var getContent: Boolean = false
  lateinit var onResult: (URI?) -> Unit

  constructor(getContent: Boolean, onResult: (URI?) -> Unit): this() {
    this.getContent = getContent
    this.onResult = onResult
  }

  actual suspend fun launch(input: String) {
    val res = if (getContent) simplexWindowState.openDialog.awaitResult() else simplexWindowState.saveDialog.awaitResult()
    onResult(if (!getContent && input.isNotEmpty() && res != null) File(res, input).toURI() else res?.toURI())
  }
}

actual fun URI.inputStream(): InputStream? = File(URI("file:" + toString().removePrefix("file:"))).inputStream()
actual fun URI.outputStream(): OutputStream = File(URI("file:" + toString().removePrefix("file:"))).outputStream()
