package chat.simplex.common.platform

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.drawable.AnimatedImageDrawable
import android.os.Build
import android.util.Base64
import android.webkit.MimeTypeMap
import androidx.compose.ui.graphics.*
import androidx.core.graphics.applyCanvas
import androidx.core.graphics.drawable.toBitmap
import androidx.core.graphics.scale
import boofcv.android.ConvertBitmap
import boofcv.struct.image.GrayU8
import chat.simplex.common.R
import chat.simplex.common.views.helpers.errorBitmap
import chat.simplex.common.views.helpers.getFileName
import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.URI
import kotlin.math.min
import kotlin.math.sqrt

internal const val MAX_IMAGE_DIMENSION = 4320
internal const val MAX_THUMBNAIL_DIMENSION = 1000
private const val MAX_SOURCE_IMAGE_DIMENSION = 16384
private const val MAX_DECODED_PIXELS = 18_662_400L
internal const val MAX_IMAGE_HEADER_BYTES = 1024 * 1024

actual fun base64ToBitmap(base64ImageString: String): ImageBitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  return try {
    val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
    val options = BitmapFactory.Options().apply { inJustDecodeBounds = true }
    BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size, options)
    if (options.outWidth <= 0 || options.outHeight <= 0 || options.outWidth > MAX_IMAGE_DIMENSION || options.outHeight > MAX_IMAGE_DIMENSION || options.outHeight > options.outWidth * 256) {
      return errorBitmap.asImageBitmap()
    }
    BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size).asImageBitmap()
  } catch (e: Exception) {
    Log.e(TAG, "base64ToBitmap error: $e")
    errorBitmap.asImageBitmap()
  }
}

actual fun resizeImageToStrSize(image: ImageBitmap, maxDataSize: Long): String {
  var img = image
  var str = compressImageStr(img)
  while (str.length > maxDataSize) {
    val ratio = sqrt(str.length.toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img.asAndroidBitmap(), width, height, true).asImageBitmap()
    str = compressImageStr(img)
  }
  return str
}

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery
actual fun cropToSquare(image: ImageBitmap): ImageBitmap {
  var xOffset = 0
  var yOffset = 0
  val side = min(image.height, image.width)
  if (image.height < image.width) {
    xOffset = (image.width - side) / 2
  } else {
    yOffset = (image.height - side) / 2
  }
  return Bitmap.createBitmap(image.asAndroidBitmap(), xOffset, yOffset, side, side).asImageBitmap()
}

fun Bitmap.clipToCircle(): Bitmap {
  val circle = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
  val path = android.graphics.Path()
  path.addCircle(width / 2f, height / 2f, min(width, height) / 2f, android.graphics.Path.Direction.CCW)
  val canvas = android.graphics.Canvas(circle)
  canvas.clipPath(path)
  canvas.drawBitmap(this, 0f, 0f, null)
  return circle
}

actual fun compressImageStr(bitmap: ImageBitmap): String {
  val usePng = bitmap.hasAlpha()
  val ext = if (usePng) "png" else "jpg"
  return "data:image/$ext;base64," + Base64.encodeToString(compressImageData(bitmap, usePng).toByteArray(), Base64.NO_WRAP)
}

actual fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream {
  val stream = ByteArrayOutputStream()
  bitmap.asAndroidBitmap().compress(if (!usePng) Bitmap.CompressFormat.JPEG else Bitmap.CompressFormat.PNG, 85, stream)
  return stream
}

actual fun resizeImageToDataSize(image: ImageBitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream {
  var img = image
  var stream = compressImageData(img, usePng)
  while (stream.size() > maxDataSize) {
    val ratio = sqrt(stream.size().toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img.asAndroidBitmap(), width, height, true).asImageBitmap()
    stream = compressImageData(img, usePng)
  }
  return stream
}

actual fun GrayU8.toImageBitmap(): ImageBitmap = ConvertBitmap.grayToBitmap(this, Bitmap.Config.RGB_565).asImageBitmap()

actual fun ImageBitmap.hasAlpha(): Boolean = hasAlpha

actual fun ImageBitmap.addLogo(size: Float): ImageBitmap = asAndroidBitmap().applyCanvas {
  val radius = (width * size) / 2
  val paint = android.graphics.Paint()
  paint.color = android.graphics.Color.WHITE
  drawCircle(width / 2f, height / 2f, radius, paint)
  val logo = androidAppContext.resources.getDrawable(R.drawable.icon_foreground_android_common, null).toBitmap()
  val logoSize = (width * size * 1.5).toInt()
  translate((width - logoSize) / 2f, (height - logoSize) / 2f)
  drawBitmap(logo, null, android.graphics.Rect(0, 0, logoSize, logoSize), null)
}.asImageBitmap()

actual fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap = asAndroidBitmap().scale(width, height).asImageBitmap()

actual fun isImage(uri: URI): Boolean =
  MimeTypeMap.getSingleton().getMimeTypeFromExtension(getFileName(uri)?.split(".")?.last())?.contains("image/") == true

actual fun isAnimImage(uri: URI, drawable: Any?): Boolean {
  val isAnimNewApi = Build.VERSION.SDK_INT >= 28 && drawable is AnimatedImageDrawable
  val isAnimOldApi = Build.VERSION.SDK_INT < 28 &&
      (getFileName(uri)?.endsWith(".gif") == true || getFileName(uri)?.endsWith(".webp") == true)
  return isAnimNewApi || isAnimOldApi
}

internal fun boundedImageSampleSize(width: Int, height: Int, target: Int): Int? {
  if (width !in 1..MAX_SOURCE_IMAGE_DIMENSION || height !in 1..MAX_SOURCE_IMAGE_DIMENSION) return null
  var sampleSize = 1
  val halfHeight = height / 2
  val halfWidth = width / 2
  while (halfHeight / sampleSize >= target && halfWidth / sampleSize >= target) sampleSize *= 2
  while (ceilDiv(width, sampleSize) * ceilDiv(height, sampleSize) > MAX_DECODED_PIXELS) sampleSize *= 2
  return sampleSize
}

private fun ceilDiv(value: Int, divisor: Int): Long =
  (value.toLong() + divisor - 1L) / divisor

internal class LimitedInputStream(
  private val source: InputStream,
  limit: Int,
) : InputStream() {
  private var remaining = limit

  override fun read(): Int {
    if (remaining == 0) return -1
    val value = source.read()
    if (value >= 0) remaining--
    return value
  }

  override fun read(buffer: ByteArray, offset: Int, length: Int): Int {
    if (length == 0) return 0
    if (remaining == 0) return -1
    val count = source.read(buffer, offset, minOf(length, remaining))
    if (count > 0) remaining -= count
    return count
  }

  override fun skip(count: Long): Long {
    val allowed = minOf(count.coerceAtLeast(0), remaining.toLong())
    val skipped = source.skip(allowed).coerceIn(0, allowed)
    remaining -= skipped.toInt()
    return skipped
  }

  override fun available(): Int = source.available().coerceIn(0, remaining)
}

internal fun decodeImageBitmap(inputStream: InputStream, target: Int): ImageBitmap {
  val source = BufferedInputStream(inputStream)
  source.mark(MAX_IMAGE_HEADER_BYTES)
  val options = BitmapFactory.Options().apply { inJustDecodeBounds = true }
  BitmapFactory.decodeStream(LimitedInputStream(source, MAX_IMAGE_HEADER_BYTES), null, options)
  val sampleSize = boundedImageSampleSize(options.outWidth, options.outHeight, target)
    ?: throw IOException("Image dimensions exceed limit")
  source.reset()
  options.inJustDecodeBounds = false
  options.inSampleSize = sampleSize
  return (BitmapFactory.decodeStream(source, null, options) ?: throw IOException("Unable to decode image")).asImageBitmap()
}

actual fun loadImageBitmap(inputStream: InputStream): ImageBitmap =
  decodeImageBitmap(inputStream, MAX_IMAGE_DIMENSION)
