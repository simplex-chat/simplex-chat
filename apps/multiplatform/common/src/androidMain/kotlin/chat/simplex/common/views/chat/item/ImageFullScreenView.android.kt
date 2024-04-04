package chat.simplex.common.views.chat.item

import android.os.Build
import android.view.View
import androidx.compose.foundation.Image
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.view.isVisible
import chat.simplex.common.platform.VideoPlayer
import chat.simplex.common.platform.androidAppContext
import chat.simplex.res.MR
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import coil.size.Size
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout
import com.google.android.exoplayer2.ui.StyledPlayerView
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun FullScreenImageView(modifier: Modifier, data: ByteArray, imageBitmap: ImageBitmap) {
  // I'm using a new private instance of imageLoader here because if I use one instance in multiple places
  // after end of composition here a GIF from the first instance will be paused automatically which isn't what I want
  Image(
    rememberAsyncImagePainter(
      ImageRequest.Builder(LocalContext.current).data(data = data).size(Size.ORIGINAL).build(),
      placeholder = BitmapPainter(imageBitmap), // show original image while it's still loading by coil
      imageLoader = imageLoader
    ),
    contentDescription = stringResource(MR.strings.image_descr),
    contentScale = ContentScale.Fit,
    modifier = modifier,
  )
}

@Composable
actual fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier, close: () -> Unit) {
  AndroidView(
    factory = { ctx ->
      StyledPlayerView(ctx).apply {
        resizeMode = if (ctx.resources.configuration.screenWidthDp > ctx.resources.configuration.screenHeightDp) {
          AspectRatioFrameLayout.RESIZE_MODE_FIXED_HEIGHT
        } else {
          AspectRatioFrameLayout.RESIZE_MODE_FIXED_WIDTH
        }
        setShowPreviousButton(false)
        setShowNextButton(false)
        setShowSubtitleButton(false)
        setShowVrButton(false)
        controllerAutoShow = false
        findViewById<View>(com.google.android.exoplayer2.R.id.exo_controls_background).setBackgroundColor(Color.Black.copy(alpha = 0.3f).toArgb())
        findViewById<View>(com.google.android.exoplayer2.R.id.exo_settings).isVisible = false
        this.player = player.player
      }
    },
    modifier
  )
}

private val imageLoader = ImageLoader.Builder(androidAppContext)
  .networkObserverEnabled(false)
  .components {
    if (Build.VERSION.SDK_INT >= 28) {
      add(ImageDecoderDecoder.Factory())
    } else {
      add(GifDecoder.Factory())
    }
  }
  .build()
