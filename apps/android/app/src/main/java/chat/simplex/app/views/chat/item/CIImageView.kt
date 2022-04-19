import android.graphics.Bitmap
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.CIFile
import chat.simplex.app.views.helpers.*
import chat.simplex.app.R

@Composable
fun CIImageView(
  image: String,
  file: CIFile?,
  showMenu: MutableState<Boolean>
) {
  Column {
    val context = LocalContext.current
    var imageBitmap: Bitmap? = getStoredImage(context, file)
    if (imageBitmap == null) {
      imageBitmap = base64ToBitmap(image)
    }
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = generalGetString(R.string.image_descr),
      // .width(1000.dp) is a hack for image to increase IntrinsicSize of FramedItemView
      // if text is short and take all available width if text is long
      modifier = Modifier
        .width(1000.dp)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = {
            if (getStoredFilePath(context, file) != null) {
              ModalManager.shared.showCustomModal { close -> ImageFullScreenView(imageBitmap, close) }
            }
          }
        ),
      contentScale = ContentScale.FillWidth,
    )
  }
}
