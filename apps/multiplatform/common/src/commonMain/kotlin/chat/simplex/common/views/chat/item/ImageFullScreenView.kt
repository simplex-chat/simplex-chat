package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.CryptoFile
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.chat.ProviderMedia
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch
import java.net.URI
import kotlin.math.absoluteValue
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow

private const val WHEEL_ZOOM_STEP = 1.2f

private fun boundTranslation(translation: Offset, view: IntSize, image: IntSize, scale: Float): Offset {
  val fit = min(view.width.toFloat() / image.width, view.height.toFloat() / image.height)
  val maxX = max(0f, (image.width * fit * scale - view.width) / 2)
  val maxY = max(0f, (image.height * fit * scale - view.height) / 2)
  return Offset(translation.x.coerceIn(-maxX, maxX), translation.y.coerceIn(-maxY, maxY))
}

private val DRAG_THRESHOLD = 4.dp

private suspend fun PointerInputScope.detectDragPastThreshold(onDrag: (Offset) -> Unit) = awaitEachGesture {
  val down = awaitFirstDown(requireUnconsumed = false)
  var pan = Offset.Zero
  var dragging = false
  do {
    val change = awaitPointerEvent().changes.firstOrNull { it.id == down.id } ?: break
    pan += change.positionChange()
    if (!dragging) dragging = pan.getDistance() > DRAG_THRESHOLD.toPx()
    if (dragging) {
      onDrag(pan)
      pan = Offset.Zero
      change.consume()
    }
  } while (change.pressed)
}

interface ImageGalleryProvider {
  val initialIndex: Int
  val totalMediaSize: MutableState<Int>
  fun getMedia(index: Int): ProviderMedia?
  fun currentPageChanged(index: Int)
  fun scrollToStart()
  fun onDismiss(index: Int)
}

@Composable
fun ImageFullScreenView(imageProvider: () -> ImageGalleryProvider, close: () -> Unit) {
  val provider = remember { imageProvider() }
  val pagerState = rememberPagerState(
    initialPage = provider.initialIndex,
    initialPageOffsetFraction = 0f
  ) {
    provider.totalMediaSize.value
  }
  val firstValidPageBeforeScrollingToStart = remember { mutableStateOf(0) }
  val goBack = { provider.onDismiss(pagerState.currentPage); close() }
  BackHandler(onBack = goBack)
  // Pager doesn't ask previous page at initialization step who knows why. By not doing this, prev page is not checked and can be blank,
  // which makes this blank page visible for a moment. Prevent it by doing the check ourselves
  LaunchedEffect(Unit) {
    if (provider.getMedia(provider.initialIndex - 1) == null) {
      firstValidPageBeforeScrollingToStart.value = provider.initialIndex
      provider.scrollToStart()
      pagerState.scrollToPage(0)
      firstValidPageBeforeScrollingToStart.value = 0
    }
  }
  val scope = rememberCoroutineScope()
  val playersToRelease = rememberSaveable { mutableSetOf<URI>() }
  DisposableEffectOnGone(
    always = {
      platform.androidSetStatusAndNavigationBarAppearance(false, false, blackNavBar = true)
      chatModel.fullscreenGalleryVisible.value = true
    },
    whenDispose = {
      val c = CurrentColors.value.colors
      platform.androidSetStatusAndNavigationBarAppearance(c.isLight, c.isLight)
      chatModel.fullscreenGalleryVisible.value = false
    },
    whenGone = {
      playersToRelease.forEach { VideoPlayerHolder.release(it, true, true) }
    }
  )

  @Composable
  fun Content(index: Int) {
    // Index can be huge but in reality at that moment pager state scrolls to 0 and that page should have index 0 too if it's the first one.
    // Or index 1 if it's the second page
    val index = index - firstValidPageBeforeScrollingToStart.value
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
              // Current media was deleted or moderated, close gallery
              index -> close()
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
        fun modifier(image: ImageBitmap): Modifier {
          val imageSize = IntSize(image.width, image.height)
          fun panBy(delta: Offset, view: IntSize) {
            val panned = boundTranslation(Offset(translationX, translationY) + delta, view, imageSize, scale)
            translationX = panned.x
            translationY = panned.y
          }
          return Modifier
          .onGloballyPositioned {
            viewWidth = it.size.width
          }
          .then(if (appPlatform.isAndroid) Modifier.graphicsLayer(
            scaleX = scale,
            scaleY = scale,
            translationX = translationX,
            translationY = translationY,
          ).pointerInput(Unit) {
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
          } else Modifier
            .onSizeChanged { panBy(Offset.Zero, it) }
            .pointerInput(imageSize) {
              detectDragPastThreshold { pan -> panBy(pan, size) }
            }
            .pointerInput(imageSize) {
              awaitPointerEventScope {
                while (true) {
                  val event = awaitPointerEvent()
                  if (event.type != PointerEventType.Scroll) continue
                  val change = event.changes.first()
                  if (change.scrollDelta.y == 0f) continue
                  val zoomed = (scale * WHEEL_ZOOM_STEP.pow(-change.scrollDelta.y)).coerceIn(1f, 20f)
                  val pointer = change.position - Offset(size.width / 2f, size.height / 2f) - Offset(translationX, translationY)
                  val shift = pointer * (1 - zoomed / scale)
                  scale = zoomed
                  panBy(shift, size)
                  change.consume()
                }
              }
            }
            .graphicsLayer {
              scaleX = scale
              scaleY = scale
              this.translationX = translationX
              this.translationY = translationY
            }
          )
          .fillMaxSize()
        }
        if (media is ProviderMedia.Image) {
          val (data: ByteArray, imageBitmap: ImageBitmap) = media
          FullScreenImageView(modifier(imageBitmap), data, imageBitmap)
        } else if (media is ProviderMedia.Video) {
          val preview = remember(media.uri.path) { base64ToBitmap(media.preview) }
          val uriDecrypted = remember(media.uri.path) { mutableStateOf(if (media.fileSource?.cryptoArgs == null) media.uri else media.fileSource.decryptedGet()) }
          val decrypted = uriDecrypted.value
          if (decrypted != null) {
            // settledCurrentPage finishes **only** when fully swiped
            // So we use pagerState.currentPage that changes right away as the screen is being dragged
            val isCurrentPage = index == pagerState.currentPage && kotlin.math.abs(pagerState.currentPageOffsetFraction) < 0.3f
            VideoView(modifier(preview), decrypted, preview, isCurrentPage, close)
            DisposableEffect(Unit) {
              onDispose { playersToRelease.add(decrypted) }
            }
          } else if (media.fileSource != null) {
            VideoViewEncrypted(uriDecrypted, media.fileSource, preview, close)
          }
        }
      }
    }
  }
  if (appPlatform.isAndroid) {
    HorizontalPager(state = pagerState) { index -> Content(index) }
  } else {
    Content(pagerState.currentPage)
  }
}

@Composable
expect fun FullScreenImageView(modifier: Modifier, data: ByteArray, imageBitmap: ImageBitmap)

@Composable
private fun VideoViewEncrypted(uriUnencrypted: MutableState<URI?>, fileSource: CryptoFile, defaultPreview: ImageBitmap, close: () -> Unit) {
  LaunchedEffect(Unit) {
    withBGApi {
      uriUnencrypted.value = fileSource.decryptedGetOrCreate()
      if (uriUnencrypted.value == null) {
        close()
      }
    }
  }
  Box(contentAlignment = Alignment.Center) {
    VideoPreviewImageViewFullScreen(defaultPreview, {}, {})
    VideoDecryptionProgress() {}
  }
}

@Composable
private fun VideoView(modifier: Modifier, uri: URI, defaultPreview: ImageBitmap, currentPage: Boolean, close: () -> Unit) {
  val player = remember(uri) { VideoPlayerHolder.getOrCreate(uri, true, defaultPreview, 0L, true) }
  val isCurrentPage = rememberUpdatedState(currentPage)
  val play = {
    player.play(true)
  }
  val stop = {
    player.stop()
  }
  LaunchedEffect(Unit) {
    snapshotFlow { isCurrentPage.value }
      .distinctUntilChanged()
      .collect {
        if (it) play() else stop()
        player.enableSound(true)
      }
  }

  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    FullScreenVideoView(player, modifier, close)
  }
}

@Composable
expect fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier, close: () -> Unit)
