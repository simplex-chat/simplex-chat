package chat.simplex.common.platform

import androidx.compose.ui.graphics.*
import boofcv.io.image.ConvertBufferedImage
import boofcv.struct.image.GrayU8
import chat.simplex.res.MR
import org.jetbrains.skia.Image
import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.io.*
import java.net.URI
import java.util.*
import javax.imageio.*
import javax.imageio.stream.MemoryCacheImageOutputStream
import kotlin.math.sqrt

private fun errorBitmap(): ImageBitmap =
  ImageIO.read(ByteArrayInputStream(Base64.getMimeDecoder().decode("iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg=="))).toComposeImageBitmap()

actual fun base64ToBitmap(base64ImageString: String): ImageBitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  return try {
    ImageIO.read(ByteArrayInputStream(Base64.getMimeDecoder().decode(imageString))).toComposeImageBitmap()
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
actual fun resizeImageToDataSize(image: ImageBitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream {
  var img = image
  var stream = compressImageData(img, usePng)
  while (stream.size() > maxDataSize) {
    val ratio = sqrt(stream.size().toDouble() / maxDataSize.toDouble())
    val clippedRatio = kotlin.math.min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = img.scale(width, height)
    stream = compressImageData(img, usePng)
  }
  return stream
}

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
  val usePng = bitmap.hasAlpha()
  val ext = if (usePng) "png" else "jpg"
  return try {
    val encoded  = Base64.getEncoder().encodeToString(compressImageData(bitmap, usePng).toByteArray())
    "data:image/$ext;base64,$encoded"
  } catch (e: Exception) {
    Log.e(TAG, "resizeImageToStrSize error: $e")
    throw e
  }
}

actual fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream {
  val writer = ImageIO.getImageWritersByFormatName(if (usePng) "png" else "jpg").next()
  val writeParam = writer.defaultWriteParam
  writeParam.compressionMode = ImageWriteParam.MODE_EXPLICIT
  writeParam.compressionQuality = 0.85f
  val stream = ByteArrayOutputStream()
  writer.output = MemoryCacheImageOutputStream(stream)
  val outputImage = IIOImage(if (usePng) bitmap.toAwtImage() else removeAlphaChannel(bitmap.toAwtImage()), null, null)
  writer.write(null, outputImage, writeParam)
  writer.dispose()
  stream.flush()
  return stream
}

private fun removeAlphaChannel(img: BufferedImage): BufferedImage {
  if (!img.colorModel.hasAlpha()) return img
  val target = BufferedImage(img.width, img.height, BufferedImage.TYPE_INT_RGB)
  val g = target.createGraphics()
  g.fillRect(0, 0, img.width, img.height)
  g.drawImage(img, 0, 0, null)
  g.dispose()
  return target
}

actual fun GrayU8.toImageBitmap(): ImageBitmap = ConvertBufferedImage.extractBuffered(this).toComposeImageBitmap()

actual fun ImageBitmap.hasAlpha(): Boolean {
  val map = toPixelMap()
  var y = 0
  while (y < height) {
    var x = 0
    while (x < width) {
      if (map[x, y].alpha < 1f) return true
      x++
    }
    y++
  }
  return false
}

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
  val path = uri.toFile().path.lowercase()
  return path.endsWith(".gif") ||
      path.endsWith(".webp") ||
      path.endsWith(".png") ||
      path.endsWith(".jpg") ||
      path.endsWith(".jpeg")
}

actual fun isAnimImage(uri: URI, drawable: Any?): Boolean {
  val path = uri.toFile().path.lowercase()
  return path.endsWith(".gif") || path.endsWith(".webp")
}

actual fun loadImageBitmap(inputStream: InputStream): ImageBitmap =
  Image.makeFromEncoded(inputStream.readBytes()).toComposeImageBitmap()

// https://stackoverflow.com/a/68926993
fun BufferedImage.rotate(angle: Double): BufferedImage {
  val sin = Math.abs(Math.sin(Math.toRadians(angle)))
  val cos = Math.abs(Math.cos(Math.toRadians(angle)))
  val w = width
  val h = height
  val neww = Math.floor(w * cos + h * sin).toInt()
  val newh = Math.floor(h * cos + w * sin).toInt()
  val rotated = BufferedImage(neww, newh, type)
  val graphic = rotated.createGraphics()
  graphic.translate((neww - w) / 2, (newh - h) / 2)
  graphic.rotate(Math.toRadians(angle), (w / 2).toDouble(), (h / 2).toDouble())
  graphic.drawRenderedImage(this, null)
  graphic.dispose()
  return rotated
}

// https://stackoverflow.com/a/9559043
fun BufferedImage.flip(vertically: Boolean, horizontally: Boolean): BufferedImage {
  if (!vertically && !horizontally) return this
  val tx: AffineTransform
  if (vertically && horizontally) {
    tx = AffineTransform.getScaleInstance(-1.0, -1.0)
    tx.translate(-width.toDouble(), -height.toDouble())
  } else if (vertically) {
    tx = AffineTransform.getScaleInstance(1.0, -1.0)
    tx.translate(0.0, -height.toDouble())
  } else {
    tx = AffineTransform.getScaleInstance(-1.0, 1.0)
    tx.translate(-width.toDouble(), 0.0)
  }
  return AffineTransformOp(tx, AffineTransformOp.TYPE_NEAREST_NEIGHBOR).filter(this, null)
}
