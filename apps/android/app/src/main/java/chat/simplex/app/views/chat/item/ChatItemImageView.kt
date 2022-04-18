import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.core.content.FileProvider
import chat.simplex.app.BuildConfig
import chat.simplex.app.model.CIFile
import chat.simplex.app.views.helpers.base64ToBitmap
import chat.simplex.app.views.helpers.getAppFilesDirectory
import java.io.*

@Composable
fun ChatItemImageView(image: String, file: CIFile?) {
  Column(
    Modifier.width(300.dp)
  ) {
    val imageBitmap = if (file?.filePath != null) {
      val context = LocalContext.current
      val filePath = getAppFilesDirectory(context) + "/" + file.filePath
      if (
        File(filePath).exists() &&
        file.stored // TODO more advanced approach would be to send progressive jpeg and only check for filepath
      ) {
        try {
          getBitmapFromUri(context, filePath)
        } catch (e: Exception) {
          base64ToBitmap(image)
        }
      } else {
        base64ToBitmap(image)
      }
    } else {
      base64ToBitmap(image)
    }
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = "image",
      modifier = Modifier.fillMaxWidth(),
      contentScale = ContentScale.FillWidth,
    )
  }
}

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
@Throws(IOException::class)
private fun getBitmapFromUri(context: Context, file: String): Bitmap {
  val uri = FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", File(file))
  val parcelFileDescriptor = context.contentResolver.openFileDescriptor(uri, "r")
  val fileDescriptor = parcelFileDescriptor?.fileDescriptor
  val image = BitmapFactory.decodeFileDescriptor(fileDescriptor)
  parcelFileDescriptor?.close()
  return image
}
