package chat.simplex.common.platform

import android.Manifest
import android.content.*
import android.content.Intent.FLAG_ACTIVITY_NEW_TASK
import android.content.pm.PackageManager
import android.net.Uri
import android.provider.MediaStore
import android.webkit.MimeTypeMap
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.UriHandler
import androidx.core.graphics.drawable.toBitmap
import chat.simplex.common.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import java.io.BufferedOutputStream
import java.io.File
import java.net.URI
import kotlin.math.min

data class OpenDefaultApp(
  val name: String,
  val icon: ImageBitmap,
  val isSystemChooser: Boolean
)

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

fun openOrShareFile(text: String, fileSource: CryptoFile, justOpen: Boolean, useChooser: Boolean = true) {
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
  val sendIntent: Intent = Intent(if (justOpen) Intent.ACTION_VIEW else Intent.ACTION_SEND).apply {
    /*if (text.isNotEmpty()) {
      putExtra(Intent.EXTRA_TEXT, text)
    }*/
    if (justOpen) {
      flags = Intent.FLAG_GRANT_READ_URI_PERMISSION
      setDataAndType(uri.toUri(), mimeType)
    } else {
      putExtra(Intent.EXTRA_STREAM, uri.toUri())
      type = mimeType
    }
  }
  if (useChooser) {
    val shareIntent = Intent.createChooser(sendIntent, null)
    shareIntent.addFlags(FLAG_ACTIVITY_NEW_TASK)
    androidAppContext.startActivity(shareIntent)
  } else {
    sendIntent.addFlags(FLAG_ACTIVITY_NEW_TASK)
    androidAppContext.startActivity(sendIntent)
  }
}

fun queryDefaultAppForExtension(ext: String, encryptedFileUri: URI): OpenDefaultApp? {
  val mimeType = MimeTypeMap.getSingleton().getMimeTypeFromExtension(ext) ?: return null
  val openIntent = Intent(Intent.ACTION_VIEW)
  openIntent.flags = Intent.FLAG_GRANT_READ_URI_PERMISSION
  openIntent.setDataAndType(encryptedFileUri.toUri(), mimeType)
  val pm = androidAppContext.packageManager
//// This method returns the list of apps but no priority, nor default flag
//  val resInfoList: List<ResolveInfo> = if (Build.VERSION.SDK_INT >= 33) {
//    pm.queryIntentActivities(openIntent, PackageManager.ResolveInfoFlags.of((PackageManager.MATCH_DEFAULT_ONLY).toLong()))
//  } else {
//    pm.queryIntentActivities(openIntent, PackageManager.MATCH_DEFAULT_ONLY)
//  }.sortedBy { it.priority }
//  val first = resInfoList.firstOrNull { it.isDefault } ?: resInfoList.firstOrNull() ?: return null
  val act = pm.resolveActivity(openIntent, PackageManager.MATCH_DEFAULT_ONLY) ?: return null
//  Log.d(TAG, "Default launch action ${act} ${act.loadLabel(pm)} ${act.activityInfo?.name}")
  val label = act.loadLabel(pm).toString()
  val icon = act.loadIcon(pm).toBitmap().asImageBitmap()
  val chooser = act.activityInfo?.name?.endsWith("ResolverActivity") == true
  return OpenDefaultApp(label, icon, chooser)
}

actual fun shareFile(text: String, fileSource: CryptoFile) {
  openOrShareFile(text, fileSource, justOpen = false)
}

actual fun openFile(fileSource: CryptoFile) {
  openOrShareFile("", fileSource, justOpen = true)
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
