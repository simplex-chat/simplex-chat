package chat.simplex.app.platform

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.drawable.AnimatedImageDrawable
import android.net.Uri
import android.os.Build
import android.util.Base64
import android.util.Log
import android.webkit.MimeTypeMap
import androidx.core.graphics.applyCanvas
import androidx.core.graphics.drawable.toBitmap
import chat.simplex.app.*
import chat.simplex.app.views.helpers.errorBitmap
import chat.simplex.app.views.helpers.getFileName
import java.io.ByteArrayOutputStream
import kotlin.math.min
import kotlin.math.sqrt

fun base64ToBitmap(base64ImageString: String): Bitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  try {
    val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
    return BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size)
  } catch (e: Exception) {
    Log.e(TAG, "base64ToBitmap error: $e")
    return errorBitmap
  }
}

fun resizeImageToStrSize(image: Bitmap, maxDataSize: Long): String {
  var img = image
  var str = compressImageStr(img)
  while (str.length > maxDataSize) {
    val ratio = sqrt(str.length.toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img, width, height, true)
    str = compressImageStr(img)
  }
  return str
}

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery
fun cropToSquare(image: Bitmap): Bitmap {
  var xOffset = 0
  var yOffset = 0
  val side = min(image.height, image.width)
  if (image.height < image.width) {
    xOffset = (image.width - side) / 2
  } else {
    yOffset = (image.height - side) / 2
  }
  return Bitmap.createBitmap(image, xOffset, yOffset, side, side)
}

private fun compressImageStr(bitmap: Bitmap): String {
  val usePng = bitmap.hasAlpha()
  val ext = if (usePng) "png" else "jpg"
  return "data:image/$ext;base64," + Base64.encodeToString(compressImageData(bitmap, usePng).toByteArray(), Base64.NO_WRAP)
}

private fun compressImageData(bitmap: Bitmap, usePng: Boolean): ByteArrayOutputStream {
  val stream = ByteArrayOutputStream()
  bitmap.compress(if (!usePng) Bitmap.CompressFormat.JPEG else Bitmap.CompressFormat.PNG, 85, stream)
  return stream
}

fun resizeImageToDataSize(image: Bitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream {
  var img = image
  var stream = compressImageData(img, usePng)
  while (stream.size() > maxDataSize) {
    val ratio = sqrt(stream.size().toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img, width, height, true)
    stream = compressImageData(img, usePng)
  }
  return stream
}

fun Bitmap.addLogo(): Bitmap = applyCanvas {
  val radius = (width * 0.16f) / 2
  val paint = android.graphics.Paint()
  paint.color = android.graphics.Color.WHITE
  drawCircle(width / 2f, height / 2f, radius, paint)
  val logo = SimplexApp.context.resources.getDrawable(R.mipmap.icon_foreground, null).toBitmap()
  val logoSize = (width * 0.24).toInt()
  translate((width - logoSize) / 2f, (height - logoSize) / 2f)
  drawBitmap(logo, null, android.graphics.Rect(0, 0, logoSize, logoSize), null)
}

fun isImage(uri: Uri): Boolean =
  MimeTypeMap.getSingleton().getMimeTypeFromExtension(getFileName(uri)?.split(".")?.last())?.contains("image/") == true


fun isAnimImage(uri: Uri, drawable: Any?): Boolean {
  val isAnimNewApi = Build.VERSION.SDK_INT >= 28 && drawable is AnimatedImageDrawable
  val isAnimOldApi = Build.VERSION.SDK_INT < 28 &&
      (getFileName(uri)?.endsWith(".gif") == true || getFileName(uri)?.endsWith(".webp") == true)
  return isAnimNewApi || isAnimOldApi
}