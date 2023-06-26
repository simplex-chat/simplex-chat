package chat.simplex.common.platform

import androidx.compose.ui.graphics.*
import boofcv.io.image.ConvertBufferedImage
import boofcv.struct.image.GrayU8
import com.icerockdev.library.MR
import org.jetbrains.skia.Image
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.*
import java.net.URI
import java.util.*
import javax.imageio.ImageIO
import kotlin.math.sqrt

private fun errorBitmap(): ImageBitmap =
  ImageIO.read(ByteArrayInputStream(Base64.getDecoder().decode("iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg=="))).toComposeImageBitmap()

actual fun base64ToBitmap(base64ImageString: String): ImageBitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  return try {
    ImageIO.read(ByteArrayInputStream(Base64.getDecoder().decode(imageString))).toComposeImageBitmap()
  } catch (e: IOException) {
    Log.e(TAG, "base64ToBitmap error: $e")
    errorBitmap()
  }
}

actual fun resizeImageToStrSize(image: ImageBitmap, maxDataSize: Long): String {
  var img = image
  var str = compressImageStr(img)
  while (str.length > maxDataSize) {
    val ratio = sqrt(str.length.toDouble() / maxDataSize.toDouble())
    val clippedRatio = kotlin.math.min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = img.scale(width, height)
    str = compressImageStr(img)
  }
  return str
}
actual fun resizeImageToDataSize(image: ImageBitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream = TODO()
actual fun cropToSquare(image: ImageBitmap): ImageBitmap {
  var xOffset = 0
  var yOffset = 0
  val side = kotlin.math.min(image.height, image.width)
  if (image.height < image.width) {
    xOffset = (image.width - side) / 2
  } else {
    yOffset = (image.height - side) / 2
  }
  // LALAL MAKE REAL CROP
  return image
}

actual fun compressImageStr(bitmap: ImageBitmap): String {
  val usePng = bitmap.hasAlpha
  val ext = if (usePng) "png" else "jpg"
  return try {
    val encoded  =Base64.getEncoder().encodeToString(compressImageData(bitmap, usePng).toByteArray())
    "data:image/$ext;base64,$encoded"
  } catch (e: IOException) {
    Log.e(TAG, "resizeImageToStrSize error: $e")
    throw e
  }
}

actual fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream {
  val stream = ByteArrayOutputStream()
  stream.use { s -> ImageIO.write(bitmap.toAwtImage(), if (usePng) "png" else "jpg", s) }
  // MAKE REAL COMPRESSION
  //bitmap.compress(if (!usePng) Bitmap.CompressFormat.JPEG else Bitmap.CompressFormat.PNG, 85, stream)
  return stream
}

actual fun GrayU8.toImageBitmap(): ImageBitmap = ConvertBufferedImage.extractBuffered(this).toComposeImageBitmap()

actual fun ImageBitmap.addLogo(): ImageBitmap {
  val radius = (width * 0.16f).toInt()
  val logoSize = (width * 0.24).toInt()
  val logo: BufferedImage = MR.images.icon_foreground_common.image
  val original = toAwtImage()
  val withLogo = BufferedImage(width, height, original.type)
  val g = withLogo.createGraphics()
  g.setRenderingHint(
    RenderingHints.KEY_INTERPOLATION,
    RenderingHints.VALUE_INTERPOLATION_BILINEAR
  )
  g.drawImage(original, 0, 0, width, height, 0, 0, original.width, original.height, null)
  g.fillRoundRect(width / 2 - radius / 2, height / 2 - radius / 2, radius, radius, radius, radius)
  g.drawImage(logo, (width - logoSize) / 2, (height - logoSize) / 2, logoSize, logoSize, null)
  g.dispose()

  return withLogo.toComposeImageBitmap()
}

actual fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap {
  val original = toAwtImage()
  val resized = BufferedImage(width, height, original.type)
  val g = resized.createGraphics()
  g.setRenderingHint(
    RenderingHints.KEY_INTERPOLATION,
    RenderingHints.VALUE_INTERPOLATION_BILINEAR
  )
  g.drawImage(original, 0, 0, width, height, 0, 0, original.width, original.height, null)
  g.dispose()
  return resized.toComposeImageBitmap()
}

// LALAL
actual fun isImage(uri: URI): Boolean {
  val path = uri.path.lowercase()
  return path.endsWith(".gif") ||
      path.endsWith(".webp") ||
      path.endsWith(".png") ||
      path.endsWith(".jpg") ||
      path.endsWith(".jpeg")
}

actual fun isAnimImage(uri: URI, drawable: Any?): Boolean {
  val path = uri.path.lowercase()
  return path.endsWith(".gif") || path.endsWith(".webp")
}

@Suppress("NewApi")
actual fun loadImageBitmap(inputStream: InputStream): ImageBitmap =
  Image.makeFromEncoded(inputStream.readAllBytes()).toComposeImageBitmap()