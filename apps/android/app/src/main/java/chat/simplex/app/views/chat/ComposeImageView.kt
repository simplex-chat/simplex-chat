import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.base64ToBitmap

@Composable
fun ComposeImageView(image: String, cancelImage: () -> Unit, cancelEnabled: Boolean) {
  Row(
    Modifier
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(SentColorLight),
    verticalAlignment = Alignment.CenterVertically
  ) {
    val imageBitmap = base64ToBitmap(image).asImageBitmap()
    Image(
      imageBitmap,
      "preview image",
      modifier = Modifier
        .width(80.dp)
        .height(60.dp)
        .padding(end = 8.dp)
    )
    Spacer(Modifier.weight(1f))
    if (cancelEnabled) {
      IconButton(onClick = cancelImage, modifier = Modifier.padding(0.dp)) {
        Icon(
          Icons.Outlined.Close,
          contentDescription = stringResource(R.string.icon_descr_cancel_image_preview),
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.padding(10.dp)
        )
      }
    }
  }
}
