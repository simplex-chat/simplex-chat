import android.graphics.Bitmap
import android.net.Uri
import android.os.Build
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.detectTransformGestures
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import coil.size.Size

@Composable
fun ImageFullScreenView(imageBitmap: Bitmap, uri: Uri, close: () -> Unit) {
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
    // I'm making a new instance of imageLoader here because if I use one instance in multiple places
    // after end of composition here a GIF from the first instance will be paused automatically which isn't what I want
    val imageLoader = ImageLoader.Builder(LocalContext.current)
      .components {
        if (Build.VERSION.SDK_INT >= 28) {
          add(ImageDecoderDecoder.Factory())
        } else {
          add(GifDecoder.Factory())
        }
      }
      .build()
    Image(
      rememberAsyncImagePainter(
        ImageRequest.Builder(LocalContext.current).data(data = uri).size(Size.ORIGINAL).build(),
        placeholder = BitmapPainter(imageBitmap.asImageBitmap()), // show original image while it's still loading by coil
        imageLoader = imageLoader
      ),
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
