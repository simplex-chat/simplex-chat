package chat.simplex.app.views.chat.item

import android.graphics.Bitmap
import android.net.Uri
import android.os.Build
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.views.helpers.*
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import coil.size.Size
import com.google.accompanist.pager.*
import java.io.File

interface ImageGalleryProvider {
  val currentItem: Int
  val totalImagesSize: Int
  fun uniqueKey(index: Int): Long
  fun getImage(index: Int): Pair<Bitmap, Uri>?
  fun onDismiss(index: Int)

  companion object {
    fun from(chatItemId: Long, items: () -> List<ChatItem>, onDismiss: (Int) -> Unit): ImageGalleryProvider = object: ImageGalleryProvider {
      override val currentItem: Int
        get() = items().indexOfFirst { it.id == chatItemId }
      override val totalImagesSize: Int
        get() = items().size

      override fun uniqueKey(index: Int): Long = items()[index].id
      override fun getImage(index: Int): Pair<Bitmap, Uri>? {
        val file = items().getOrNull(index)?.file
        val imageBitmap: Bitmap? = getLoadedImage(SimplexApp.context, file)
        val filePath = getLoadedFilePath(SimplexApp.context, file)
        return if (imageBitmap != null && filePath != null) {
          val uri = FileProvider.getUriForFile(SimplexApp.context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))
          imageBitmap to uri
        } else null
      }
      override fun onDismiss(index: Int) { onDismiss(index) }
    }
  }
}

@OptIn(ExperimentalPagerApi::class)
@Composable
fun ImageFullScreenView(provider: ImageGalleryProvider, close: () -> Unit) {
  val pagerState = rememberPagerState(provider.currentItem)
  val goBack = { provider.onDismiss(pagerState.currentPage); close() }
  BackHandler(onBack = goBack)
  HorizontalPager(count = provider.totalImagesSize, state = pagerState, key = { provider.uniqueKey(it) }) { index ->
    val (imageBitmap: Bitmap, uri: Uri) = provider.getImage(index) ?: return@HorizontalPager
    Column(
      Modifier
        .fillMaxSize()
        .background(Color.Black)
        .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = goBack)
    ) {
      var scale by remember { mutableStateOf(1f) }
      var translationX by remember { mutableStateOf(0f) }
      var translationY by remember { mutableStateOf(0f) }
      LaunchedEffect(pagerState.currentPage) {
        scale = 1f
        translationX = 0f
        translationY = 0f
      }
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
            detectTransformGestures (
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
}
