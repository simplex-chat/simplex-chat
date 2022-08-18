import android.graphics.Bitmap
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.detectTransformGestures
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R

@Composable
fun ImageFullScreenView(imageBitmap: Bitmap, close: () -> Unit) {
  BackHandler(onBack = close)
  Column(
    Modifier
      .fillMaxSize()
      .background(Color.Black)
      .clickable(onClick = close)
  ) {
    var scale by remember { mutableStateOf(1f) }
    var translationX by remember { mutableStateOf(0f) }
    var translationY by remember { mutableStateOf(0f) }
    Image(
      imageBitmap.asImageBitmap(),
      contentDescription = stringResource(R.string.image_descr),
      contentScale = ContentScale.Fit,
      modifier = Modifier
        .graphicsLayer(
          scaleX = scale,
          scaleY = scale,
          translationX = translationX,
          translationY = translationY,
        )
        .pointerInput(Unit) {
          detectTransformGestures(
            onGesture = { _, pan, gestureZoom, _ ->
              scale = (scale * gestureZoom).coerceIn(1f, 20f)
              if (scale > 1) {
                translationX += pan.x * scale
                translationY += pan.y * scale
              } else {
                translationX = 0f
                translationY = 0f
              }
            }
          )
        }
        .fillMaxSize(),
    )
  }
}
