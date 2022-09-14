package chat.simplex.app.views.helpers

import android.content.*
import android.net.Uri
import android.provider.MediaStore
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.Composable
import androidx.core.content.ContextCompat
import chat.simplex.app.R
import chat.simplex.app.model.CIFile
import java.io.BufferedOutputStream
import java.io.File

fun shareText(cxt: Context, text: String) {
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    putExtra(Intent.EXTRA_TEXT, text)
    type = "text/plain"
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  cxt.startActivity(shareIntent)
}

fun copyText(cxt: Context, text: String) {
  val clipboard = ContextCompat.getSystemService(cxt, ClipboardManager::class.java)
  clipboard?.setPrimaryClip(ClipData.newPlainText("text", text))
}

@Composable
fun rememberSaveFileLauncher(cxt: Context, ciFile: CIFile?): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      destination?.let {
        val filePath = getLoadedFilePath(cxt, ciFile)
        if (filePath != null) {
          val contentResolver = cxt.contentResolver
          contentResolver.openOutputStream(destination)?.let { stream ->
            val outputStream = BufferedOutputStream(stream)
            val file = File(filePath)
            outputStream.write(file.readBytes())
            outputStream.close()
            Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
          }
        } else {
          Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
        }
      }
    }
  )

fun saveImage(cxt: Context, ciFile: CIFile?) {
  val filePath = getLoadedFilePath(cxt, ciFile)
  val fileName = ciFile?.fileName
  if (filePath != null && fileName != null) {
    val values = ContentValues()
    values.put(MediaStore.Images.Media.DATE_TAKEN, System.currentTimeMillis())
    values.put(MediaStore.Images.Media.MIME_TYPE, "image/jpeg")
    values.put(MediaStore.MediaColumns.DISPLAY_NAME, fileName)
    values.put(MediaStore.MediaColumns.TITLE, fileName)
    val uri = cxt.contentResolver.insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values)
    uri?.let {
      cxt.contentResolver.openOutputStream(uri)?.let { stream ->
        val outputStream = BufferedOutputStream(stream)
        val file = File(filePath)
        outputStream.write(file.readBytes())
        outputStream.close()
        Toast.makeText(cxt, generalGetString(R.string.image_saved), Toast.LENGTH_SHORT).show()
      }
    }
  } else {
    Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
  }
}
