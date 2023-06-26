package chat.simplex.common.views.helpers

import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.unit.Density
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import java.io.File
import java.net.URI
import javax.imageio.ImageIO
import kotlin.io.path.toPath

actual fun spannableStringToAnnotatedString(
  text: CharSequence,
  density: Density,
): AnnotatedString {
  // LALAL
  return AnnotatedString(text.toString())
}

actual fun getLoadedImage(file: CIFile?): ImageBitmap? {
  val filePath = getLoadedFilePath(file)
  return if (filePath != null) {
    val uri = getAppFileUri(filePath.substringAfterLast(File.separator))
    getBitmapFromUri(uri, false)
  } else {
    null
  }
}

actual fun getFileName(uri: URI): String? = uri.toPath().toFile().name

actual fun getAppFilePath(uri: URI): String? = getFilesDirectory() + File.separator + "app_files"

actual fun getFileSize(uri: URI): Long? = uri.toPath().toFile().length()

actual fun getBitmapFromUri(uri: URI, withAlertOnException: Boolean): ImageBitmap? =
  ImageIO.read(uri.inputStream()).toComposeImageBitmap()

// LALAL implement to support animated drawable
actual fun getDrawableFromUri(uri: URI, withAlertOnException: Boolean): Any? = null

actual suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File? {
  val file = simplexWindowState.saveDialog.awaitResult()
  return if (file != null) {
    try {
      val ext = if (asPng) "png" else "jpg"
      val newFile = File(file.absolutePath + File.separator + generateNewFileName("IMG", ext))
      // LALAL FILE IS EMPTY
      ImageIO.write(image.toAwtImage(), ext.uppercase(), newFile.outputStream())
      newFile
    } catch (e: Exception) {
      Log.e(TAG, "Util.kt saveTempImageUncompressed error: ${e.message}")
      null
    }
  } else null
}

actual fun getBitmapFromVideo(uri: URI, timestamp: Long?, random: Boolean): VideoPlayerInterface.PreviewAndDuration {
  // LALAL
  return VideoPlayerInterface.PreviewAndDuration(preview = null, timestamp = 0L, duration = 0L)
}