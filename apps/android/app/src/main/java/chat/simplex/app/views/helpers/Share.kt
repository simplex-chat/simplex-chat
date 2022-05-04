package chat.simplex.app.views.helpers

import android.content.*
import android.net.Uri
import android.widget.Toast
import androidx.core.content.ContextCompat
import chat.simplex.app.R
import chat.simplex.app.model.CIFile
import java.io.File
import java.io.IOException

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

fun saveFile(cxt: Context, ciFile: CIFile?, destination: Uri?) {
  if (destination != null) {
    val filePath = getStoredFilePath(cxt, ciFile)
    if (filePath != null) {
      val contentResolver = cxt.contentResolver
      val file = File(filePath)
      try {
        val outputStream = contentResolver.openOutputStream(destination)
        if (outputStream != null) {
          outputStream.write(file.readBytes())
          outputStream.close()
          Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
        }
      } catch (e: IOException) {
        Toast.makeText(cxt, generalGetString(R.string.error_saving_file), Toast.LENGTH_SHORT).show()
      }
    } else {
      Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
    }
  }
}
