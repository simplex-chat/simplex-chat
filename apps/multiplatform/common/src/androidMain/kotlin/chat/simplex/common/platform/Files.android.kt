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
actual val filesDir: File = File(dataDir.absolutePath + File.separator + "files")
actual val appFilesDir: File = File(filesDir.absolutePath + File.separator + "app_files")
actual val wallpapersDir: File = File(filesDir.absolutePath + File.separator + "assets" + File.separator + "wallpapers")
actual val coreTmpDir: File = File(filesDir.absolutePath + File.separator + "temp_files")
actual val dbAbsolutePrefixPath: String = dataDir.absolutePath + File.separator + "files"
actual val preferencesDir = File(dataDir.absolutePath + File.separator + "shared_prefs")

actual val chatDatabaseFileName: String = "files_chat.db"
actual val agentDatabaseFileName: String = "files_agent.db"

actual val databaseExportDir: File = androidAppContext.cacheDir

actual val remoteHostsDir: File = File(tmpDir.absolutePath + File.separator + "remote_hosts")

actual fun desktopOpenDatabaseDir() {}

@Composable
actual fun rememberFileChooserLauncher(getContent: Boolean, rememberedValue: Any?, onResult: (URI?) -> Unit): FileChooserLauncher {
  val launcher = rememberLauncherForActivityResult(
    contract = if (getContent) ActivityResultContracts.GetContent() else ActivityResultContracts.CreateDocument(),
    onResult = { onResult(it?.toURI()) }
  )
  return FileChooserLauncher(launcher)
}

@Composable
actual fun rememberFileChooserMultipleLauncher(onResult: (List<URI>) -> Unit): FileChooserMultipleLauncher {
  val launcher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.GetMultipleContents(),
    onResult = { onResult(it.map { it.toURI() }) }
  )
  return FileChooserMultipleLauncher(launcher)
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

actual class FileChooserMultipleLauncher actual constructor() {
  private lateinit var launcher: ManagedActivityResultLauncher<String, List<Uri>>

  constructor(launcher: ManagedActivityResultLauncher<String, List<Uri>>): this() {
    this.launcher = launcher
  }

  actual suspend fun launch(input: String) {
    launcher.launch(input)
  }
}

actual fun URI.inputStream(): InputStream? = androidAppContext.contentResolver.openInputStream(toUri())
actual fun URI.outputStream(): OutputStream = androidAppContext.contentResolver.openOutputStream(toUri())!!
