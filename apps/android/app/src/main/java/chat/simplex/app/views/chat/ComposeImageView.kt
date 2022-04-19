import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.unit.dp
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.base64ToBitmap

@Composable
fun ComposeImageView(image: String) {
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
  }
}
