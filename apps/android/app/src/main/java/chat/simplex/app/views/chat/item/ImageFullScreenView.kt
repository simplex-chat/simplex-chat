import android.graphics.Bitmap
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import chat.simplex.app.R
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun ImageFullScreenView(imageBitmap: Bitmap, close: () -> Unit) {
  BackHandler(onBack = close)
  Column(
    Modifier
      .fillMaxSize()
      .background(Color.Black)
      .clickable(onClick = close)
  ) {
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = generalGetString(R.string.image_descr),
      modifier = Modifier.fillMaxSize(),
      contentScale = ContentScale.Fit,
    )
  }
}
