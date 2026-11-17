package chat.simplex.common.platform

import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.Stable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.toSize
import boofcv.struct.image.GrayU8
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.withContext
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.net.URI
import java.util.Base64
import java.util.Collections
import kotlin.math.roundToInt

expect fun base64ToBitmap(base64ImageString: String): ImageBitmap
expect fun base64BitmapSize(base64ImageString: String): IntSize
expect fun decodeBase64Image(base64: String, width: Int): ImageBitmap?
expect fun cropToSquare(image: ImageBitmap): ImageBitmap
expect fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream

expect fun GrayU8.toImageBitmap(): ImageBitmap

expect fun ImageBitmap.hasAlpha(): Boolean
expect fun ImageBitmap.addLogo(size: Float): ImageBitmap
expect fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap

expect fun isImage(uri: URI): Boolean
expect fun isAnimImage(uri: URI, drawable: Any?): Boolean

enum class ImageFileType(val signatures: List<List<Int?>>) {
  JPEG(listOf(listOf(0xFF, 0xD8, 0xFF))),
  PNG(listOf(listOf(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))),
  GIF(listOf(ascii("GIF87a"), ascii("GIF89a"))),
  WEBP(listOf(ascii("RIFF") + List(4) { null } + ascii("WEBP")));

  fun matches(data: ByteArray): Boolean = signatures.any { signature ->
    data.size >= signature.size && signature.withIndex().all { (i, b) -> b == null || b == data[i].toInt() and 0xFF }
  }
}

fun isImageFile(data: ByteArray): Boolean = ImageFileType.entries.any { it.matches(data) }

private fun ascii(s: String): List<Int?> = s.map { it.code }

expect fun loadImageBitmap(inputStream: InputStream): ImageBitmap

expect fun resizeImageFileUri(uri: URI, maxSize: Int): String
expect fun resizeImageFileData(uri: URI, maxSize: Int): String

fun resizeImageFileToStrSize(uri: URI, maxDataSize: Long): String? =
  resizeImageFileUri(uri, maxDataSize.toInt()).ifEmpty {
    Log.d(TAG, "resizeImageFileToStrSize: core returned no image")
    null
  }

fun resizeImageFileToDataSize(uri: URI, maxDataSize: Long): ByteArray? =
  resizeImageFileData(uri, maxDataSize.toInt()).ifEmpty {
    Log.d(TAG, "resizeImageFileToDataSize: core returned no image")
    null
  }?.let { Base64.getDecoder().decode(it) }

fun resizeImageToStrSize(image: ImageBitmap, maxDataSize: Long): String {
  val str = chatResizeImageUri(compressImageData(image, image.hasAlpha()).toByteArray().toDirectBuffer(), maxDataSize.toInt())
  if (str.isEmpty()) throw Exception("Image resize failed")
  return str
}

fun resizeImageToDataSize(image: ImageBitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream {
  val str = chatResizeImage(compressImageData(image, usePng).toByteArray().toDirectBuffer(), maxDataSize.toInt(), if (usePng) 1 else 0)
  if (str.isEmpty()) throw Exception("Image resize failed")
  val stream = ByteArrayOutputStream()
  stream.write(Base64.getDecoder().decode(str))
  return stream
}

internal val base64BitmapCache = Collections.synchronizedMap(object : LinkedHashMap<String, ImageBitmap>(200, 0.75f, true) {
  override fun removeEldestEntry(eldest: Map.Entry<String, ImageBitmap>): Boolean {
    var bytes = values.sumOf { it.bytes() }
    val bitmaps = values.iterator()
    while (size > 1 && (size > 200 || bytes > MAX_BASE64_BITMAP_CACHE_BYTES)) {
      bytes -= bitmaps.next().bytes()
      bitmaps.remove()
    }
    return false
  }
})

private fun ImageBitmap.bytes(): Long = width.toLong() * height * 4

private const val MAX_BASE64_BITMAP_CACHE_BYTES = 64L * 1024 * 1024

@OptIn(ExperimentalCoroutinesApi::class)
internal val base64ImageDecoder = Dispatchers.IO.limitedParallelism(1)

@Stable
class Base64ImagePainter internal constructor(private val placeholderSize: IntSize, bitmap: ImageBitmap?) : Painter() {
  var bitmap: ImageBitmap? by mutableStateOf(bitmap)
    internal set

  override val intrinsicSize: Size
    get() = (bitmap?.let { IntSize(it.width, it.height) } ?: placeholderSize).toSize()

  override fun DrawScope.onDraw() {
    bitmap?.let { drawImage(it, dstSize = IntSize(size.width.roundToInt(), size.height.roundToInt())) }
  }
}

@Composable
fun rememberBase64ImagePainter(base64ImageString: String): Base64ImagePainter {
  val painter = remember(base64ImageString) {
    val cached = base64BitmapCache[base64ImageString]
    Base64ImagePainter(cached?.let { IntSize(it.width, it.height) } ?: base64BitmapSize(base64ImageString), cached)
  }
  LaunchedEffect(painter) {
    if (painter.bitmap == null) painter.bitmap = withContext(base64ImageDecoder) { base64ToBitmap(base64ImageString) }
  }
  return painter
}

@Composable
fun Base64AsyncImage(
  base64ImageString: String,
  contentDescription: String?,
  contentScale: ContentScale,
  modifier: Modifier = Modifier
) {
  Image(
    painter = rememberBase64ImagePainter(base64ImageString),
    contentDescription = contentDescription,
    contentScale = contentScale,
    modifier = modifier
  )
}
