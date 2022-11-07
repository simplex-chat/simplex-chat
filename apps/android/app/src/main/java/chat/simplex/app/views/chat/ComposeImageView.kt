package chat.simplex.app.views.chat

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
import chat.simplex.app.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.base64ToBitmap

@Composable
fun ComposeImageView(images: List<String>, cancelImages: () -> Unit, cancelEnabled: Boolean) {
  Row(
    Modifier
      .padding(top = 8.dp)
      .background(SentColorLight),
    verticalAlignment = Alignment.CenterVertically,
  ) {
    LazyRow(
      Modifier.weight(1f).padding(start = DEFAULT_PADDING_HALF, end = if (cancelEnabled) 0.dp else DEFAULT_PADDING_HALF),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF),
    ) {
      items(images.size) { index ->
        val imageBitmap = base64ToBitmap(images[index]).asImageBitmap()
        Image(
          imageBitmap,
          "preview image",
          modifier = Modifier.widthIn(max = 80.dp).height(60.dp)
        )
      }
    }
    if (cancelEnabled) {
      IconButton(onClick = cancelImages) {
        Icon(
          Icons.Outlined.Close,
          contentDescription = stringResource(R.string.icon_descr_cancel_image_preview),
          tint = MaterialTheme.colors.primary,
        )
      }
    }
  }
}
