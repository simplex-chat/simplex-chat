import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.ImageType
import chat.simplex.app.views.helpers.base64ToBitmap

@Composable
fun ComposeImageView(images: List<Pair<String, ImageType>>, cancelImage: (Int) -> Unit, cancelEnabled: Boolean) {
  Row(
    Modifier
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(SentColorLight),
    verticalAlignment = Alignment.CenterVertically
  ) {
    LazyRow(
      verticalAlignment = Alignment.CenterVertically
    ) {
      items(images.size) { index ->
        val imageBitmap = base64ToBitmap(images[index].first).asImageBitmap()
        Image(
          imageBitmap,
          "preview image",
          modifier = Modifier.padding(start = DEFAULT_PADDING).widthIn(max = 80.dp).height(60.dp)
        )
        if (cancelEnabled && images.size > 1) {
          IconButton(
            onClick = { cancelImage(index) },
            Modifier.padding(end = 20.dp),
          ) {
            Icon(
              Icons.Outlined.Close,
              contentDescription = stringResource(R.string.icon_descr_cancel_image_preview),
              tint = MaterialTheme.colors.primary,
            )
          }
        }
      }
    }
    if (cancelEnabled && images.size == 1) {
      Spacer(Modifier.weight(1f))
      IconButton(onClick = { cancelImage(0) }) {
        Icon(
          Icons.Outlined.Close,
          contentDescription = stringResource(R.string.icon_descr_cancel_image_preview),
          tint = MaterialTheme.colors.primary,
        )
      }
    }
  }
}
