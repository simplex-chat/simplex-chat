import android.graphics.Bitmap
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Icon
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.MoreHoriz
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.*

@Composable
fun CIImageView(
  image: String,
  file: CIFile?,
  showMenu: MutableState<Boolean>
) {
  @Composable
  fun loadingIndicator() {
    if (file != null) {
      Box(
        Modifier
          .padding(8.dp)
          .size(20.dp),
        contentAlignment = Alignment.Center
      ) {
        when (file.fileStatus) {
          CIFileStatus.RcvAccepted ->
            Icon(
              Icons.Outlined.MoreHoriz,
              stringResource(R.string.icon_descr_waiting_for_image),
              Modifier.fillMaxSize(),
              tint = Color.White
            )
          CIFileStatus.RcvTransfer ->
            CircularProgressIndicator(
              Modifier.size(16.dp),
              color = Color.White,
              strokeWidth = 2.dp
            )
          else -> {}
        }
      }
    }
  }

  @Composable
  fun imageView(imageBitmap: Bitmap, onClick: () -> Unit) {
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = stringResource(R.string.image_descr),
      // .width(1000.dp) is a hack for image to increase IntrinsicSize of FramedItemView
      // if text is short and take all available width if text is long
      modifier = Modifier
        .width(1000.dp)
        .combinedClickable(
          onLongClick = { showMenu.value = true },
          onClick = onClick
        ),
      contentScale = ContentScale.FillWidth,
    )
  }

  Box(contentAlignment = Alignment.TopEnd) {
    val context = LocalContext.current
    val imageBitmap: Bitmap? = getStoredImage(context, file)
    if (imageBitmap != null) {
      imageView(imageBitmap, onClick = {
        if (getStoredFilePath(context, file) != null) {
          ModalManager.shared.showCustomModal { close -> ImageFullScreenView(imageBitmap, close) }
        }
      })
    } else {
      imageView(base64ToBitmap(image), onClick = {
        if (file != null && file.fileStatus == CIFileStatus.RcvAccepted)
          AlertManager.shared.showAlertMsg(
            generalGetString(R.string.waiting_for_image),
            generalGetString(R.string.image_will_be_received_when_contact_is_online)
          )
      })
    }
    loadingIndicator()
  }
}
