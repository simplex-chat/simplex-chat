package chat.simplex.common.platform

import chat.simplex.common.model.CIFile
import chat.simplex.common.views.helpers.generalGetString
import com.icerockdev.library.MR
import java.io.*
import java.net.URI

expect val dataDir: File
expect val tmpDir: File
expect val cacheDir: File

fun copyFileToFile(from: File, to: URI, finally: () -> Unit) {
  try {
    to.outputStream().use { stream ->
      BufferedOutputStream(stream).use { outputStream ->
        from.inputStream().use { it.copyTo(outputStream) }
      }
    }
    showToast(generalGetString(MR.strings.file_saved))
  } catch (e: Error) {
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
  } catch (e: Error) {
    showToast(generalGetString(MR.strings.error_saving_file))
    Log.e(TAG, "copyBytesToFile error saving file $e")
  } finally {
    finally()
  }
}

fun getFilesDirectory(): String {
  return dataDir.absolutePath + File.separator + "files"
}

// LALAL
fun getTempFilesDirectory(): String {
  return getFilesDirectory() + File.separator + "temp_files"
}

fun getAppFilesDirectory(): String {
  return getFilesDirectory() + File.separator + "app_files"
}

fun getAppFilePath(fileName: String): String {
  return getAppFilesDirectory() + File.separator + fileName
}

fun getLoadedFilePath(file: CIFile?): String? {
  return if (file?.filePath != null && file.loaded) {
    val filePath = getAppFilePath(file.filePath)
    if (File(filePath).exists()) filePath else null
  } else {
    null
  }
}

expect fun rememberFileChooserLauncher(getContent: Boolean, onResult: (URI?) -> Unit): FileChooserLauncher

expect class FileChooserLauncher() {
  suspend fun launch(input: String)
}

expect fun URI.inputStream(): InputStream?
expect fun URI.outputStream(): OutputStream
