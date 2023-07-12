package chat.simplex.common.platform

import android.app.Application
import android.net.Uri
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.Composable
import chat.simplex.common.helpers.toURI
import chat.simplex.common.helpers.toUri
import java.io.*
import java.net.URI

actual val dataDir: File = androidAppContext.dataDir
actual val tmpDir: File = androidAppContext.getDir("temp", Application.MODE_PRIVATE)
actual val cacheDir: File = androidAppContext.cacheDir

@Composable
actual fun rememberFileChooserLauncher(getContent: Boolean, onResult: (URI?) -> Unit): FileChooserLauncher {
  val launcher = rememberLauncherForActivityResult(
    contract = if (getContent) ActivityResultContracts.GetContent() else ActivityResultContracts.CreateDocument(),
    onResult = { onResult(it?.toURI()) }
  )
  return FileChooserLauncher(launcher)
}

actual class FileChooserLauncher actual constructor() {
  private lateinit var launcher: ManagedActivityResultLauncher<String, Uri?>

  constructor(launcher: ManagedActivityResultLauncher<String, Uri?>): this() {
    this.launcher = launcher
  }

  actual suspend fun launch(input: String) {
    launcher.launch(input)
  }
}

actual fun URI.inputStream(): InputStream? = androidAppContext.contentResolver.openInputStream(toUri())
actual fun URI.outputStream(): OutputStream = androidAppContext.contentResolver.openOutputStream(toUri())!!
