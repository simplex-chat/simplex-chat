package chat.simplex.common.platform

import androidx.compose.ui.graphics.ImageBitmap
import boofcv.struct.image.GrayU8
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

expect fun ImageBitmap.addLogo(): ImageBitmap
expect fun ImageBitmap.scale(width: Int, height: Int): ImageBitmap

expect fun isImage(uri: URI): Boolean
expect fun isAnimImage(uri: URI, drawable: Any?): Boolean

expect fun loadImageBitmap(inputStream: InputStream): ImageBitmap