package chat.simplex.app.platform

import chat.simplex.app.SimplexApp
import chat.simplex.app.model.CIFile
import java.io.File

fun getFilesDirectory(): String {
  return SimplexApp.context.filesDir.toString()
}

fun getTempFilesDirectory(): String {
  return "${getFilesDirectory()}/temp_files"
}

fun getAppFilesDirectory(): String {
  return "${getFilesDirectory()}/app_files"
}

fun getAppFilePath(fileName: String): String {
  return "${getAppFilesDirectory()}/$fileName"
}

fun getLoadedFilePath(file: CIFile?): String? {
  return if (file?.filePath != null && file.loaded) {
    val filePath = getAppFilePath(file.filePath)
    if (File(filePath).exists()) filePath else null
  } else {
    null
  }
}
