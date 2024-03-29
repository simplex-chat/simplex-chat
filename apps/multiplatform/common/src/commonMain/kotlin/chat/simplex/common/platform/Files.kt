package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.CryptoFile
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import java.io.*
import java.net.URI
import java.net.URLDecoder
import java.net.URLEncoder

expect val dataDir: File
expect val tmpDir: File
expect val filesDir: File
expect val appFilesDir: File
expect val coreTmpDir: File
expect val dbAbsolutePrefixPath: String

expect val chatDatabaseFileName: String
expect val agentDatabaseFileName: String

/**
* This is used only for temporary storing db archive for export.
* Providing [tmpDir] instead crashes the app. Check db export before moving from this path to something else
* */
expect val databaseExportDir: File

expect val remoteHostsDir: File

expect fun desktopOpenDatabaseDir()

fun createURIFromPath(absolutePath: String): URI = URI.create(URLEncoder.encode(absolutePath, "UTF-8"))

fun URI.toFile(): File = File(URLDecoder.decode(rawPath, "UTF-8").removePrefix("file:"))

fun copyFileToFile(from: File, to: URI, finally: () -> Unit) {
  try {
    to.outputStream().use { stream ->
      BufferedOutputStream(stream).use { outputStream ->
        from.inputStream().use { it.copyTo(outputStream) }
      }
    }
    showToast(generalGetString(MR.strings.file_saved))
  } catch (e: Throwable) {
    showToast(generalGetString(MR.strings.error_saving_file))
    Log.e(TAG, "copyFileToFile error saving file $e")
  } finally {
    finally()
  }
}

fun copyBytesToFile(bytes: ByteArrayInputStream, to: URI, finally: () -> Unit) {
  try {
    to.outputStream().use { stream ->
      BufferedOutputStream(stream).use { outputStream ->
        bytes.use { it.copyTo(outputStream) }
      }
    }
    showToast(generalGetString(MR.strings.file_saved))
  } catch (e: Throwable) {
    showToast(generalGetString(MR.strings.error_saving_file))
    Log.e(TAG, "copyBytesToFile error saving file $e")
  } finally {
    finally()
  }
}

fun getMigrationTempFilesDirectory(): File = File(dataDir, "migration_temp_files")

fun getAppFilePath(fileName: String): String {
  val rh = chatModel.currentRemoteHost.value
  val s = File.separator
  return if (rh == null) {
    appFilesDir.absolutePath + s + fileName
  } else {
    remoteHostsDir.absolutePath + s + rh.storePath + s + "simplex_v1_files" + s + fileName
  }
}

fun getLoadedFilePath(file: CIFile?): String? {
  val f = file?.fileSource?.filePath
  return if (f != null && file.loaded) {
    val filePath = getAppFilePath(f)
    if (fileReady(file, filePath)) filePath else null
  } else {
    null
  }
}

fun getLoadedFileSource(file: CIFile?): CryptoFile? {
  val f = file?.fileSource?.filePath
  return if (f != null && file.loaded) {
    val filePath = getAppFilePath(f)
    if (fileReady(file, filePath)) file.fileSource else null
  } else {
    null
  }
}

private fun fileReady(file: CIFile, filePath: String) =
  File(filePath).exists() &&
  CIFile.cachedRemoteFileRequests[file.fileSource] != false
  && File(filePath).length() >= file.fileSize

/**
* [rememberedValue] is used in `remember(rememberedValue)`. So when the value changes, file saver will update a callback function
* */
@Composable
expect fun rememberFileChooserLauncher(getContent: Boolean, rememberedValue: Any? = null, onResult: (URI?) -> Unit): FileChooserLauncher

expect fun rememberFileChooserMultipleLauncher(onResult: (List<URI>) -> Unit): FileChooserMultipleLauncher

expect class FileChooserLauncher() {
  suspend fun launch(input: String)
}

expect class FileChooserMultipleLauncher() {
  suspend fun launch(input: String)
}

expect fun URI.inputStream(): InputStream?
expect fun URI.outputStream(): OutputStream
