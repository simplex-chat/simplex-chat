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

expect fun base64ToBitmap(base64ImageString: String): ImageBitmap
expect fun resizeImageToStrSize(image: ImageBitmap, maxDataSize: Long): String
expect fun resizeImageToDataSize(image: ImageBitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream
expect fun cropToSquare(image: ImageBitmap): ImageBitmap
expect fun compressImageStr(bitmap: ImageBitmap): String
expect fun compressImageData(bitmap: ImageBitmap, usePng: Boolean): ByteArrayOutputStream

expect fun GrayU8.toImageBitmap(): ImageBitmap

expect fun ImageBitmap.hasAlpha(): Boolean
expect fun ImageBitmap.addLogo(size: Float): ImageBitmap
expect fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap

expect fun isImage(uri: URI): Boolean
expect fun isAnimImage(uri: URI, drawable: Any?): Boolean

expect fun loadImageBitmap(inputStream: InputStream): ImageBitmap

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
