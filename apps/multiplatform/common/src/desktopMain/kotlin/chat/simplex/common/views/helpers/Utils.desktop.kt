package chat.simplex.common.views.helpers

import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Density
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.readCryptoFile
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import java.io.*
import java.net.URI
import javax.imageio.ImageIO
import kotlin.io.encoding.Base64
import kotlin.io.encoding.ExperimentalEncodingApi

private val bStyle = SpanStyle(fontWeight = FontWeight.Bold)
private val iStyle = SpanStyle(fontStyle = FontStyle.Italic)
private fun fontStyle(color: String) =
  SpanStyle(color = Color(color.replace("#", "ff").toLongOrNull(16) ?: Color.White.toArgb().toLong()))

actual fun escapedHtmlToAnnotatedString(text: String, density: Density): AnnotatedString = try {
  buildAnnotatedString {
    fun String.substringSafe(start: Int, len: Int): String =
      if (start < 0 || start >= this.length || start + len < 0 || start + len > this.length) ""
      else substring(start, start + len)

    var skipTil = 0
    for (outerI in text.indices) {
      if (skipTil > outerI) continue
      if (text[outerI] == '<') {
        for (innerI in outerI + 1 until text.length) {
          when {
            text.substringSafe(innerI, 2) == "b>" -> {
              val textStart = innerI + 2
              for (insideTagI in textStart until text.length) {
                if (text[insideTagI] == '<') {
                  withStyle(bStyle) { append(text.substring(textStart, insideTagI)) }
                  skipTil = insideTagI + 4
                  break
                }
              }
              break
            }
            text.substringSafe(innerI, 2) == "i>" -> {
              val textStart = innerI + 2
              for (insideTagI in textStart until text.length) {
                if (text[insideTagI] == '<') {
                  withStyle(iStyle) { append(text.substring(textStart, insideTagI)) }
                  skipTil = insideTagI + 4
                  break
                }
              }
              break
            }
            text.substringSafe(innerI, 4) == "font" -> {
              var textStart = innerI + 5
              var color = "#000000"
              for (i in textStart until text.length) {
                if (text[i] == '#') {
                  color = text.substring(i, i + 7)
                  textStart = i + 9
                  break
                }
              }
              for (insideTagI in textStart until text.length) {
                if (text[insideTagI] == '<') {
                  withStyle(fontStyle(color)) { append(text.substring(textStart, insideTagI)) }
                  skipTil = insideTagI + 7
                  break
                }
              }
              break
            }
          }
          if (skipTil > innerI) continue
        }
      } else {
        append(text[outerI])
      }
    }
  }
} catch (e: Exception) {
  AnnotatedString(text)
}

actual fun getAppFileUri(fileName: String): URI {
  val rh = chatModel.currentRemoteHost.value
  return if (rh == null) {
    createURIFromPath(appFilesDir.absolutePath + "/" + fileName)
  } else {
    createURIFromPath(dataDir.absolutePath + "/remote_hosts/" + rh.storePath + "/simplex_v1_files/" + fileName)
  }
}

actual suspend fun getLoadedImage(file: CIFile?): Pair<ImageBitmap, ByteArray>? {
  var filePath = getLoadedFilePath(file)
  if (chatModel.connectedToRemote() && filePath == null) {
    file?.loadRemoteFile(false)
    filePath = getLoadedFilePath(file)
  }
  return if (filePath != null) {
    try {
      val data = if (file?.fileSource?.cryptoArgs != null) readCryptoFile(filePath, file.fileSource.cryptoArgs) else File(filePath).readBytes()
      val bitmap = getBitmapFromByteArray(data, false)
      if (bitmap != null) bitmap to data else null
    } catch (e: Exception) {
      Log.e(TAG, "Unable to read crypto file: " + e.stackTraceToString())
      null
    }
  } else {
    null
  }
}

actual fun getFileName(uri: URI): String? = uri.toFile().name

actual fun getAppFilePath(uri: URI): String? = uri.toFile().absolutePath

actual fun getFileSize(uri: URI): Long? = uri.toFile().length()

actual fun getBitmapFromUri(uri: URI, withAlertOnException: Boolean): ImageBitmap? =
  try {
    ImageIO.read(uri.inputStream()).toComposeImageBitmap()
  } catch (e: Exception) {
    Log.e(TAG, "Error while decoding drawable: ${e.stackTraceToString()}")
    if (withAlertOnException) showImageDecodingException()

    null
  }

actual fun getBitmapFromByteArray(data: ByteArray, withAlertOnException: Boolean): ImageBitmap? =
  try {
    ImageIO.read(ByteArrayInputStream(data)).toComposeImageBitmap()
  } catch (e: Exception) {
    Log.e(TAG, "Error while encoding bitmap from byte array: ${e.stackTraceToString()}")
    if (withAlertOnException) showImageDecodingException()

    null
  }

// LALAL implement to support animated drawable
actual fun getDrawableFromUri(uri: URI, withAlertOnException: Boolean): Any? = null

actual suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File? {
  val file = simplexWindowState.saveDialog.awaitResult()
  return if (file != null) {
    try {
      val ext = if (asPng) "png" else "jpg"
      val newFile = File(file.absolutePath + File.separator + generateNewFileName("IMG", ext, File(file.absolutePath)))
      ImageIO.write(image.toAwtImage(), ext, newFile.outputStream())
      newFile
    } catch (e: Exception) {
      Log.e(TAG, "Util.kt saveTempImageUncompressed error: ${e.message}")
      null
    }
  } else null
}

actual suspend fun getBitmapFromVideo(uri: URI, timestamp: Long?, random: Boolean, withAlertOnException: Boolean): VideoPlayerInterface.PreviewAndDuration {
  return VideoPlayer.getBitmapFromVideo(null, uri, withAlertOnException)
}

@OptIn(ExperimentalEncodingApi::class)
actual fun ByteArray.toBase64StringForPassphrase(): String = Base64.encode(this)

@OptIn(ExperimentalEncodingApi::class)
actual fun String.toByteArrayFromBase64ForPassphrase(): ByteArray = Base64.decode(this)
