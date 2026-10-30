package chat.simplex.common.platform

import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.produceState
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.layout.ContentScale
import boofcv.struct.image.GrayU8
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.net.URI
import java.util.Base64

expect fun base64ToBitmap(base64ImageString: String): ImageBitmap
expect fun decodeBase64Image(base64: String, width: Int): ImageBitmap?
expect fun cropToSquare(image: ImageBitmap): ImageBitmap
expect fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream

expect fun GrayU8.toImageBitmap(): ImageBitmap

expect fun ImageBitmap.hasAlpha(): Boolean
expect fun ImageBitmap.addLogo(size: Float): ImageBitmap
expect fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap

expect fun isImage(uri: URI): Boolean
expect fun isAnimImage(uri: URI, drawable: Any?): Boolean

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

@Composable
fun Base64AsyncImage(
  base64ImageString: String,
  contentDescription: String?,
  contentScale: ContentScale,
  modifier: Modifier = Modifier
) {
  val imageBitmap by produceState<ImageBitmap?>(initialValue = null, base64ImageString) {
    value = withContext(Dispatchers.IO) { base64ToBitmap(base64ImageString) }
  }
  imageBitmap?.let {
    Image(
      bitmap = it,
      contentDescription = contentDescription,
      contentScale = contentScale,
      modifier = modifier
    )
  }
}
