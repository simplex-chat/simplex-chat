import android.graphics.Bitmap
import androidx.compose.foundation.Image
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.CIFile
import chat.simplex.app.views.helpers.*

@Composable
fun CIImageView(image: String, file: CIFile?) {
  Column {
    val context = LocalContext.current
    var imageBitmap: Bitmap? = getStoredImage(context, file)
    if (imageBitmap == null) {
      imageBitmap = base64ToBitmap(image)
    }
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = "image",
      // hack for image to increase IntrinsicSize of FramedItemView if text is short
      // and take all available width if text is long
      modifier = Modifier
        .width(1000.dp)
        .pointerInput(Unit) {
          detectTapGestures(onTap = {
            if (getStoredFilePath(context, file) != null) {
              ModalManager.shared.showCustomModal { close -> ImageFullScreenView(imageBitmap, close) }
            }
          })
        },
      contentScale = ContentScale.FillWidth,
    )
  }
}
