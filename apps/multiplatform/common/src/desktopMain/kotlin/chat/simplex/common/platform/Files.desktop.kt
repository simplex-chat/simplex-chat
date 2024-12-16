package chat.simplex.common.platform

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import chat.simplex.common.*
import chat.simplex.common.views.chatlist.acceptContactRequest
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import java.awt.Desktop
import java.io.*
import java.net.URI

actual val dataDir: File = File(desktopPlatform.dataPath)
actual val tmpDir: File = File(System.getProperty("java.io.tmpdir") + File.separator + "simplex").also { it.deleteOnExit() }
actual val filesDir: File = File(dataDir.absolutePath + File.separator + "simplex_v1_files")
actual val appFilesDir: File = filesDir
actual val wallpapersDir: File = File(dataDir.absolutePath + File.separator + "simplex_v1_assets" + File.separator + "wallpapers").also { it.mkdirs() }
actual val coreTmpDir: File = File(dataDir.absolutePath + File.separator + "tmp")
actual val dbAbsolutePrefixPath: String = dataDir.absolutePath + File.separator + "simplex_v1"
actual val preferencesDir = File(desktopPlatform.configPath).also { it.parentFile.mkdirs() }

actual val chatDatabaseFileName: String = "simplex_v1_chat.db"
actual val agentDatabaseFileName: String = "simplex_v1_agent.db"

actual val databaseExportDir: File = tmpDir

actual val remoteHostsDir: File = File(dataDir.absolutePath + File.separator + "remote_hosts")

actual fun desktopOpenDatabaseDir() {
  desktopOpenDir(dataDir)
}

actual fun desktopOpenDir(dir: File) {
  if (Desktop.isDesktopSupported()) {
    try {
      Desktop.getDesktop().open(dir);
    } catch (e: IOException) {
      Log.e(TAG, e.stackTraceToString())
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.unknown_error),
        text = e.stackTraceToString()
      )
    }
  }
}

@Composable
actual fun rememberFileChooserLauncher(getContent: Boolean, rememberedValue: Any?, onResult: (URI?) -> Unit): FileChooserLauncher =
  remember(rememberedValue) { FileChooserLauncher(getContent, onResult) }

@Composable
actual fun rememberFileChooserMultipleLauncher(onResult: (List<URI>) -> Unit): FileChooserMultipleLauncher =
  remember { FileChooserMultipleLauncher(onResult) }

actual class FileChooserLauncher actual constructor() {
  var getContent: Boolean = false
  lateinit var onResult: (URI?) -> Unit

  constructor(getContent: Boolean, onResult: (URI?) -> Unit): this() {
    this.getContent = getContent
    this.onResult = onResult
  }

  actual suspend fun launch(input: String) {
    var res: File?
    if (getContent) {
      val params = DialogParams(
        allowMultiple = false,
        fileFilter = fileFilter(input),
        fileFilterDescription = fileFilterDescription(input),
      )
      res = simplexWindowState.openDialog.awaitResult(params)
    } else {
      res = simplexWindowState.saveDialog.awaitResult(DialogParams(filename = input))
      if (res != null && res.isDirectory) {
        res = File(res, input)
      }
    }
    if (res == null) {
      onResult(null)
    } else if (desktopPlatform.isMac() || !res.exists()) { // mac has an alert confirmation if the file exists
      onResult(res.toURI())
    } else {
      showSaveRenameFileAlert(res, onResult)
    }
  }
}

private fun showSaveRenameFileAlert(file: File, onResult: (URI?) -> Unit) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.file_already_exists),
    text = generalGetString(MR.strings.what_would_you_like_to_do_with_file),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          onResult(file.toURI())
        }) {
          Text(generalGetString(MR.strings.overwrite_file), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          onResult(null)
        }) {
          Text(generalGetString(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    onDismissRequest = {
      onResult(null)
    }
  )
}

actual class FileChooserMultipleLauncher actual constructor() {
  lateinit var onResult: (List<URI>) -> Unit

  constructor(onResult: (List<URI>) -> Unit): this() {
    this.onResult = onResult
  }

  actual suspend fun launch(input: String) {
    val params = DialogParams(
        allowMultiple = true,
        fileFilter = fileFilter(input),
        fileFilterDescription = fileFilterDescription(input),
      )
    onResult(simplexWindowState.openMultipleDialog.awaitResult(params).map { it.toURI() })
  }
}

private fun fileFilter(input: String): (File?) -> Boolean = when(input) {
  "image/*" -> { file -> if (file?.isDirectory == true) true else if (file != null) isImage(file.toURI()) else false }
  "video/*" -> { file -> if (file?.isDirectory == true) true else if (file != null) isVideo(file.toURI()) else false }
  "*/*" -> { _ -> true }
  else -> { _ -> true }
}

private fun fileFilterDescription(input: String): String = when(input) {
  "image/*" -> generalGetString(MR.strings.gallery_image_button)
  "video/*" -> generalGetString(MR.strings.gallery_video_button)
  "*/*" -> generalGetString(MR.strings.choose_file)
  else -> ""
}

actual fun URI.inputStream(): InputStream? = toFile().inputStream()
actual fun URI.outputStream(): OutputStream = toFile().outputStream()
