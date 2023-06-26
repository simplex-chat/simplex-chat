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
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.net.URI
import kotlin.math.min
import kotlin.math.sqrt

actual fun base64ToBitmap(base64ImageString: String): ImageBitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  return try {
    val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
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

actual fun compressImageStr(bitmap: ImageBitmap): String {
  val usePng = bitmap.hasAlpha
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

actual fun ImageBitmap.addLogo(): ImageBitmap = asAndroidBitmap().applyCanvas {
  val radius = (width * 0.16f) / 2
  val paint = android.graphics.Paint()
  paint.color = android.graphics.Color.WHITE
  drawCircle(width / 2f, height / 2f, radius, paint)
  val logo = androidAppContext.resources.getDrawable(R.drawable.icon_foreground_android_common, null).toBitmap()
  val logoSize = (width * 0.24).toInt()
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

actual fun loadImageBitmap(inputStream: InputStream): ImageBitmap =
  BitmapFactory.decodeStream(inputStream).asImageBitmap()
