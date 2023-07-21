package chat.simplex.app.views.helpers

import android.Manifest
import android.content.*
import android.net.Uri
import android.provider.MediaStore
import android.util.Log
import android.webkit.MimeTypeMap
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.Composable
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.model.CIFile
import chat.simplex.res.MR
import java.io.BufferedOutputStream
import java.io.File

fun shareText(text: String) {
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    putExtra(Intent.EXTRA_TEXT, text)
    type = "text/plain"
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  // This flag is needed when you start a new activity from non-Activity context
  shareIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  SimplexApp.context.startActivity(shareIntent)
}

fun shareFile(text: String, filePath: String) {
  val uri = FileProvider.getUriForFile(SimplexApp.context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))
  val ext = filePath.substringAfterLast(".")
  val mimeType = MimeTypeMap.getSingleton().getMimeTypeFromExtension(ext) ?: return
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    /*if (text.isNotEmpty()) {
      putExtra(Intent.EXTRA_TEXT, text)
    }*/
    putExtra(Intent.EXTRA_STREAM, uri)
    type = mimeType
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  // This flag is needed when you start a new activity from non-Activity context
  shareIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  SimplexApp.context.startActivity(shareIntent)
}

fun copyText(text: String) {
  val clipboard = ContextCompat.getSystemService(SimplexApp.context, ClipboardManager::class.java)
  clipboard?.setPrimaryClip(ClipData.newPlainText("text", text))
}

fun sendEmail(subject: String, body: CharSequence) {
  val emailIntent = Intent(Intent.ACTION_SENDTO, Uri.parse("mailto:"))
  emailIntent.putExtra(Intent.EXTRA_SUBJECT, subject)
  emailIntent.putExtra(Intent.EXTRA_TEXT, body)
  // This flag is needed when you start a new activity from non-Activity context
  emailIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  try {
    SimplexApp.context.startActivity(emailIntent)
  } catch (e: ActivityNotFoundException) {
    Log.e(TAG, "No activity was found for handling email intent")
  }
}

@Composable
fun rememberSaveFileLauncher(ciFile: CIFile?): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      destination?.let {
        val cxt = SimplexApp.context
        val filePath = getLoadedFilePath(ciFile)
        if (filePath != null) {
          val contentResolver = cxt.contentResolver
          contentResolver.openOutputStream(destination)?.let { stream ->
            val outputStream = BufferedOutputStream(stream)
            File(filePath).inputStream().use { it.copyTo(outputStream) }
            outputStream.close()
            Toast.makeText(cxt, generalGetString(MR.strings.file_saved), Toast.LENGTH_SHORT).show()
          }
        } else {
          Toast.makeText(cxt, generalGetString(MR.strings.file_not_found), Toast.LENGTH_SHORT).show()
        }
      }
    }
  )

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
  val cxt = SimplexApp.context
  val filePath = getLoadedFilePath(ciFile)
  val fileName = ciFile?.fileName
  if (filePath != null && fileName != null) {
    val values = ContentValues()
    values.put(MediaStore.Images.Media.DATE_TAKEN, System.currentTimeMillis())
    values.put(MediaStore.Images.Media.MIME_TYPE, imageMimeType(fileName))
    values.put(MediaStore.MediaColumns.DISPLAY_NAME, fileName)
    values.put(MediaStore.MediaColumns.TITLE, fileName)
    val uri = cxt.contentResolver.insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values)
    uri?.let {
      cxt.contentResolver.openOutputStream(uri)?.let { stream ->
        val outputStream = BufferedOutputStream(stream)
        File(filePath).inputStream().use { it.copyTo(outputStream) }
        outputStream.close()
        Toast.makeText(cxt, generalGetString(MR.strings.image_saved), Toast.LENGTH_SHORT).show()
      }
    }
  } else {
    Toast.makeText(cxt, generalGetString(MR.strings.file_not_found), Toast.LENGTH_SHORT).show()
  }
}
