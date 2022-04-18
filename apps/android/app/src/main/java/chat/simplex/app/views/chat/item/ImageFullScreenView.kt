import android.graphics.Bitmap
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import chat.simplex.app.views.helpers.CloseSheetBar

@Composable
fun ImageFullScreenView(imageBitmap: Bitmap, close: () -> Unit) {
  BackHandler(onBack = close)
  Column(
    Modifier
      .fillMaxSize()
      .background(Color.Black)
  ) {
    CloseSheetBar(close)
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = "image",
      modifier = Modifier.fillMaxSize(),
      contentScale = ContentScale.Fit,
    )
  }
}
