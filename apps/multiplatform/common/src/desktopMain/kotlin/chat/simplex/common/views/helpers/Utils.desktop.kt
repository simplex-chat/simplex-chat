package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.Density
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.readCryptoFile
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.delay
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.File
import java.io.IOException
import java.net.URI
import java.util.*
import javax.imageio.ImageIO
import kotlin.io.encoding.Base64
import kotlin.io.encoding.ExperimentalEncodingApi

private val bStyle = SpanStyle(fontWeight = FontWeight.Bold)
private val iStyle = SpanStyle(fontStyle = FontStyle.Italic)
private val uStyle = SpanStyle(textDecoration = TextDecoration.Underline)
// Full-screen view downsamples to fit within this on each side
private const val MAX_IMAGE_DIMENSION = 4320
// Chat render (getLoadedImage) downsamples to this, matching Android's getLoadedImage target
private const val MAX_THUMBNAIL_DIMENSION = 1000
// Source images larger than this on either side are rejected (bounds the decoder's per-scanline buffer)
private const val MAX_SOURCE_IMAGE_DIMENSION = 16384
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
            text.substringSafe(innerI, 2) == "u>" -> {
              val textStart = innerI + 2
              for (insideTagI in textStart until text.length) {
                if (text[insideTagI] == '<') {
                  withStyle(uStyle) { append(text.substring(textStart, insideTagI)) }
                  skipTil = insideTagI + 4
                  break
                }
              }
              break
            }
            text.substringSafe(innerI, 3) == "br>" -> {
              val textStart = innerI + 3
              append("\n")
              skipTil = textStart
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

@Composable
actual fun SetupClipboardListener() {
  val clipboard = LocalClipboardManager.current
  chatModel.clipboardHasText.value = clipboard.hasText()
  LaunchedEffect(Unit) {
    while (true) {
      delay(1000)
      chatModel.clipboardHasText.value = clipboard.hasText()
    }
  }
}

actual fun getAppFileUri(fileName: String): URI {
  val rh = chatModel.currentRemoteHost.value
  return if (rh == null) {
    createURIFromPath(appFilesDir.absolutePath + "/" + fileName)
  } else {
    createURIFromPath(dataDir.absolutePath + "/remote_hosts/" + rh.storePath + "/simplex_v1_files/" + fileName)
  }
}

private val loadedImageCache = Collections.synchronizedMap(object : LinkedHashMap<String, Pair<ImageBitmap, ByteArray>>(30, 0.75f, true) {
  override fun removeEldestEntry(eldest: Map.Entry<String, Pair<ImageBitmap, ByteArray>>): Boolean = size > 30
})

actual fun clearImageCaches() {
  loadedImageCache.clear()
}

actual suspend fun getLoadedImage(file: CIFile?): Pair<ImageBitmap, ByteArray>? {
  var filePath = getLoadedFilePath(file)
  if (chatModel.connectedToRemote() && filePath == null) {
    file?.loadRemoteFile(false)
    filePath = getLoadedFilePath(file)
  }
  return if (filePath != null) {
    loadedImageCache[filePath] ?: try {
      val data = if (file?.fileSource?.cryptoArgs != null) readCryptoFile(filePath, file.fileSource.cryptoArgs) else File(filePath).readBytes()
      val bitmap = decodeBoundedImage(data, MAX_THUMBNAIL_DIMENSION)
      (bitmap to data).also { loadedImageCache[filePath] = it }
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
    // No dimension cap here: this path decodes user-picked local files (image picker, compose, save),
    // not untrusted received attachments. The cap is applied in getBitmapFromByteArray (auto-rendered content).
    uri.inputStream().use {
      ImageIO.read(it).toComposeImageBitmap()
    }
  } catch (e: Exception) {
    Log.e(TAG, "Error while decoding drawable: ${e.stackTraceToString()}")
    if (withAlertOnException) showImageDecodingException()

    null
  }

actual fun getBitmapFromByteArray(data: ByteArray, withAlertOnException: Boolean): ImageBitmap? =
  try {
    decodeBoundedImage(data, MAX_IMAGE_DIMENSION)
  } catch (e: Exception) {
    Log.e(TAG, "Error while encoding bitmap from byte array: ${e.stackTraceToString()}")
    if (withAlertOnException) showImageDecodingException()

    null
  }

private fun decodeBoundedImage(data: ByteArray, maxDimension: Int): ImageBitmap =
  decodeBoundedBufferedImage(data, maxDimension).toComposeImageBitmap()

// Decodes downloaded/auto-rendered image bytes with bounded memory: rejects absurd source dimensions,
// then downsamples (like Android's inSampleSize) so even large legitimate images decode to a bounded raster.
internal fun decodeBoundedBufferedImage(data: ByteArray, maxDimension: Int): BufferedImage {
  val stream = ImageIO.createImageInputStream(ByteArrayInputStream(data))
    ?: throw IOException("Unsupported image format")
  stream.use {
    val readers = ImageIO.getImageReaders(it)
    if (!readers.hasNext()) throw IOException("Unsupported image format")

    val reader = readers.next()
    try {
      reader.input = it
      val width = reader.getWidth(0)
      val height = reader.getHeight(0)
      if (!sourceDimensionsWithinLimits(width, height)) throw IOException("Image dimensions exceed limit")
      val sampleSize = imageSampleSize(width, height, maxDimension)
      val param = reader.defaultReadParam.apply {
        if (sampleSize > 1) setSourceSubsampling(sampleSize, sampleSize, 0, 0)
      }
      return reader.read(0, param) ?: throw IOException("Unable to decode image")
    } finally {
      reader.dispose()
    }
  }
}

internal fun sourceDimensionsWithinLimits(width: Int, height: Int): Boolean =
  width in 1..MAX_SOURCE_IMAGE_DIMENSION && height in 1..MAX_SOURCE_IMAGE_DIMENSION

// Smallest power-of-two subsampling factor so both dimensions fit within maxDimension (mirrors Android inSampleSize)
internal fun imageSampleSize(width: Int, height: Int, maxDimension: Int): Int {
  var sampleSize = 1
  while (width / sampleSize > maxDimension || height / sampleSize > maxDimension) {
    sampleSize *= 2
  }
  return sampleSize
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
