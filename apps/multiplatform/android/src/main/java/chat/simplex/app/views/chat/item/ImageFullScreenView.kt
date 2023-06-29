package chat.simplex.app.views.chat.item

import android.graphics.Bitmap
import android.net.Uri
import android.os.Build
import android.view.View
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.*
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.view.isVisible
import chat.simplex.app.R
import chat.simplex.app.views.chat.ProviderMedia
import chat.simplex.app.views.helpers.*
import coil.ImageLoader
import coil.compose.rememberAsyncImagePainter
import coil.decode.GifDecoder
import coil.decode.ImageDecoderDecoder
import coil.request.ImageRequest
import coil.size.Size
import com.google.accompanist.pager.*
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout
import com.google.android.exoplayer2.ui.StyledPlayerView
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch
import kotlin.math.absoluteValue

interface ImageGalleryProvider {
  val initialIndex: Int
  val totalMediaSize: MutableState<Int>
  fun getMedia(index: Int): ProviderMedia?
  fun currentPageChanged(index: Int)
  fun scrollToStart()
  fun onDismiss(index: Int)
}

@OptIn(ExperimentalPagerApi::class)
@Composable
fun ImageFullScreenView(imageProvider: () -> ImageGalleryProvider, close: () -> Unit) {
  val provider = remember { imageProvider() }
  val pagerState = rememberPagerState(provider.initialIndex)
  val goBack = { provider.onDismiss(pagerState.currentPage); close() }
  BackHandler(onBack = goBack)
  // Pager doesn't ask previous page at initialization step who knows why. By not doing this, prev page is not checked and can be blank,
  // which makes this blank page visible for a moment. Prevent it by doing the check ourselves
  LaunchedEffect(Unit) {
    if (provider.getMedia(provider.initialIndex - 1) == null) {
      provider.scrollToStart()
      pagerState.scrollToPage(0)
    }
  }
  val scope = rememberCoroutineScope()
  val playersToRelease = rememberSaveable { mutableSetOf<Uri>() }
  DisposableEffectOnGone(
    whenGone = { playersToRelease.forEach { VideoPlayer.release(it, true, true) } }
  )
  HorizontalPager(count = remember { provider.totalMediaSize }.value, state = pagerState) { index ->
    Column(
      Modifier
        .fillMaxSize()
        .background(Color.Black)
        .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = goBack)
    ) {
      var settledCurrentPage by remember { mutableStateOf(pagerState.currentPage) }
      LaunchedEffect(pagerState) {
        snapshotFlow {
          if (!pagerState.isScrollInProgress) pagerState.currentPage else settledCurrentPage
        }.collect {
          settledCurrentPage = it
        }
      }
      LaunchedEffect(settledCurrentPage) {
        // Make this pager with infinity scrolling with only 3 pages at a time when left and right pages constructs in real time
        if (settledCurrentPage != provider.initialIndex)
          provider.currentPageChanged(index)
      }
      val media = provider.getMedia(index)
      if (media == null) {
        // No such image. Let's shrink total pages size or scroll to start of the list of pages to remove blank page automatically
        SideEffect {
          scope.launch {
            when (settledCurrentPage) {
              index - 1 -> provider.totalMediaSize.value = settledCurrentPage + 1
              index + 1 -> {
                provider.scrollToStart()
                pagerState.scrollToPage(0)
              }
            }
          }
        }
      } else {
        var scale by remember { mutableStateOf(1f) }
        var translationX by remember { mutableStateOf(0f) }
        var translationY by remember { mutableStateOf(0f) }
        var viewWidth by remember { mutableStateOf(0) }
        var allowTranslate by remember { mutableStateOf(true) }
        LaunchedEffect(settledCurrentPage) {
          scale = 1f
          translationX = 0f
          translationY = 0f
        }
        val modifier = Modifier
          .onGloballyPositioned {
            viewWidth = it.size.width
          }
          .graphicsLayer(
            scaleX = scale,
            scaleY = scale,
            translationX = translationX,
            translationY = translationY,
          )
          .pointerInput(Unit) {
            detectTransformGestures(
              { allowTranslate },
              onGesture = { _, pan, gestureZoom, _ ->
                scale = (scale * gestureZoom).coerceIn(1f, 20f)
                allowTranslate = viewWidth * (scale - 1f) - ((translationX + pan.x * scale).absoluteValue * 2) > 0
                if (scale > 1 && allowTranslate) {
                  translationX += pan.x * scale
                  translationY += pan.y * scale
                } else if (allowTranslate) {
                  translationX = 0f
                  translationY = 0f
                }
              }
            )
          }
          .fillMaxSize()
        if (media is ProviderMedia.Image) {
          val (uri: Uri, imageBitmap: Bitmap) = media
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
            modifier = modifier,
          )
        } else if (media is ProviderMedia.Video) {
          val preview = remember(media.uri.path) { base64ToBitmap(media.preview) }
          VideoView(modifier, media.uri, preview, index == settledCurrentPage)
          DisposableEffect(Unit) {
            onDispose { playersToRelease.add(media.uri) }
          }
        }
      }
    }
  }
}

@Composable
private fun VideoView(modifier: Modifier, uri: Uri, defaultPreview: Bitmap, currentPage: Boolean) {
  val context = LocalContext.current
  val player = remember(uri) { VideoPlayer.getOrCreate(uri, true, defaultPreview, 0L, true, context) }
  val isCurrentPage = rememberUpdatedState(currentPage)
  val play = {
    player.play(true)
  }
  val stop = {
    player.stop()
  }
  LaunchedEffect(Unit) {
    player.enableSound(true)
    snapshotFlow { isCurrentPage.value }
      .distinctUntilChanged()
      .collect { if (it) play() else stop() }
  }

  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
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
}
