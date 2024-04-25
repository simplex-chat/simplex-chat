package chat.simplex.common.platform

import android.Manifest
import android.content.*
import android.content.Intent.FLAG_ACTIVITY_NEW_TASK
import android.net.Uri
import android.provider.MediaStore
import android.webkit.MimeTypeMap
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.UriHandler
import chat.simplex.common.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import java.io.BufferedOutputStream
import java.io.File
import chat.simplex.res.MR
import kotlin.math.min

actual fun ClipboardManager.shareText(text: String) {
  var text = text
  for (i in 10 downTo 1) {
    try {
      val sendIntent: Intent = Intent().apply {
        action = Intent.ACTION_SEND
        putExtra(Intent.EXTRA_TEXT, text)
        type = "text/plain"
        flags = FLAG_ACTIVITY_NEW_TASK
      }
      val shareIntent = Intent.createChooser(sendIntent, null)
      shareIntent.addFlags(FLAG_ACTIVITY_NEW_TASK)
      androidAppContext.startActivity(shareIntent)
      break
    } catch (e: Exception) {
      Log.e(TAG, "Failed to share text: ${e.stackTraceToString()}")
      text = text.substring(0, min(i * 1000, text.length))
    }
  }
}

actual fun shareFile(text: String, fileSource: CryptoFile) {
  val uri = if (fileSource.cryptoArgs != null) {
    val tmpFile = File(tmpDir, fileSource.filePath)
    tmpFile.deleteOnExit()
    ChatModel.filesToDelete.add(tmpFile)
    try {
      decryptCryptoFile(getAppFilePath(fileSource.filePath), fileSource.cryptoArgs, tmpFile.absolutePath)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to decrypt crypto file: " + e.stackTraceToString())
      AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
      return
    }
    getAppFileUri(tmpFile.absolutePath)
  } else {
    getAppFileUri(fileSource.filePath)
  }
  val ext = fileSource.filePath.substringAfterLast(".")
  val mimeType = MimeTypeMap.getSingleton().getMimeTypeFromExtension(ext) ?: return
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    /*if (text.isNotEmpty()) {
      putExtra(Intent.EXTRA_TEXT, text)
    }*/
    putExtra(Intent.EXTRA_STREAM, uri.toUri())
    type = mimeType
    flags = Intent.FLAG_ACTIVITY_NEW_TASK
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  shareIntent.addFlags(FLAG_ACTIVITY_NEW_TASK)
  androidAppContext.startActivity(shareIntent)
}

actual fun UriHandler.sendEmail(subject: String, body: CharSequence) {
  val emailIntent = Intent(Intent.ACTION_SENDTO, Uri.parse("mailto:"))
  emailIntent.putExtra(Intent.EXTRA_SUBJECT, subject)
  emailIntent.putExtra(Intent.EXTRA_TEXT, body)
  emailIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  try {
    androidAppContext.startActivity(emailIntent)
  } catch (e: ActivityNotFoundException) {
    Log.e(TAG, "No activity was found for handling email intent")
  }
}

fun imageMimeType(fileName: String): String {
  val lowercaseName = fileName.lowercase()
  return when {
    lowercaseName.endsWith(".png") -> "image/png"
    lowercaseName.endsWith(".gif") -> "image/gif"
    lowercaseName.endsWith(".webp") -> "image/webp"
    lowercaseName.endsWith(".avif") -> "image/avif"
    lowercaseName.endsWith(".svg") -> "image/svg+xml"
    else -> "image/jpeg"
  }
}

/** Before calling, make sure the user allows to write to external storage [Manifest.permission.WRITE_EXTERNAL_STORAGE] */
fun saveImage(ciFile: CIFile?) {
  val filePath = getLoadedFilePath(ciFile)
  val fileName = ciFile?.fileName
  if (filePath != null && fileName != null) {
    val values = ContentValues()
    values.put(MediaStore.Images.Media.DATE_TAKEN, System.currentTimeMillis())
    values.put(MediaStore.Images.Media.MIME_TYPE, imageMimeType(fileName))
    values.put(MediaStore.MediaColumns.DISPLAY_NAME, fileName)
    values.put(MediaStore.MediaColumns.TITLE, fileName)
    val uri = androidAppContext.contentResolver.insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values)
    uri?.let {
      androidAppContext.contentResolver.openOutputStream(uri)?.let { stream ->
        val outputStream = BufferedOutputStream(stream)
        if (ciFile.fileSource?.cryptoArgs != null) {
          createTmpFileAndDelete { tmpFile ->
            try {
              decryptCryptoFile(filePath, ciFile.fileSource.cryptoArgs, tmpFile.absolutePath)
            } catch (e: Exception) {
              Log.e(TAG, "Unable to decrypt crypto file: " + e.stackTraceToString())
              AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
              return@createTmpFileAndDelete
            }
            tmpFile.inputStream().use { it.copyTo(outputStream) }
            showToast(generalGetString(MR.strings.image_saved))
          }
          outputStream.close()
        } else {
          File(filePath).inputStream().use { it.copyTo(outputStream) }
          outputStream.close()
          showToast(generalGetString(MR.strings.image_saved))
        }
      }
    }
  } else {
    showToast(generalGetString(MR.strings.file_not_found))
  }
}
