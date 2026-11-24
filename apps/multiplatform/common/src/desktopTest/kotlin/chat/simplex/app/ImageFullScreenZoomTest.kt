package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.input.pointer.PointerButton
import androidx.compose.ui.input.pointer.PointerButtons
import androidx.compose.ui.input.pointer.PointerEventType
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.use
import androidx.compose.ui.window.WindowState
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.ProviderMedia
import chat.simplex.common.views.chat.item.ImageFullScreenView
import chat.simplex.common.views.chat.item.ImageGalleryProvider
import chat.simplex.common.views.chat.item.pinchZooms
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.swing.Swing
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import kotlin.math.pow
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

@OptIn(ExperimentalComposeUiApi::class)
class ImageFullScreenZoomTest {
  private fun quadrants(width: Int, height: Int): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply {
      color = java.awt.Color.RED
      fillRect(0, 0, width / 2, height / 2)
      color = java.awt.Color.BLUE
      fillRect(width / 2, 0, width / 2, height / 2)
      color = java.awt.Color.GREEN
      fillRect(0, height / 2, width / 2, height / 2)
      color = java.awt.Color.YELLOW
      fillRect(width / 2, height / 2, width / 2, height / 2)
      dispose()
    }
    return ByteArrayOutputStream().also { ImageIO.write(image, "png", it) }.toByteArray()
  }

  private fun gallery(image: ByteArray, data: () -> ByteArray = { image }) = object : ImageGalleryProvider {
    private val bitmap = Image.makeFromEncoded(image).toComposeImageBitmap()
    override val initialIndex = 0
    override val totalMediaSize = mutableStateOf(1)
    override fun getMedia(index: Int) = if (index == 0) ProviderMedia.Image(data(), bitmap) else null
    override fun currentPageChanged(index: Int) {}
    override fun scrollToStart() {}
    override fun onDismiss(index: Int) {}
  }

  private enum class Shown { RED, BLUE, GREEN, YELLOW, BACKGROUND }

  private fun ImageComposeScene.shownAt(x: Int, y: Int = 200): Shown? {
    val color = Color(Bitmap.makeFromImage(render()).getColor(x, y))
    fun high(c: Float) = c > 0.8f
    fun low(c: Float) = c < 0.2f
    return when {
      high(color.red) && low(color.green) && low(color.blue) -> Shown.RED
      low(color.red) && low(color.green) && high(color.blue) -> Shown.BLUE
      low(color.red) && high(color.green) && low(color.blue) -> Shown.GREEN
      high(color.red) && high(color.green) && low(color.blue) -> Shown.YELLOW
      color.red < 0.1f && color.green < 0.1f && color.blue < 0.1f -> Shown.BACKGROUND
      else -> null
    }
  }

  private fun ImageComposeScene.wheel(x: Float, notches: Float) {
    sendPointerEvent(PointerEventType.Scroll, Offset(x, 300f), scrollDelta = Offset(0f, notches))
    render()
  }

  private fun ImageComposeScene.drag(from: Offset, to: Offset, steps: Int = 1, beforeEachMove: () -> Unit = {}) {
    sendPointerEvent(PointerEventType.Press, from, buttons = PointerButtons(isPrimaryPressed = true), button = PointerButton.Primary)
    for (step in 1..steps) {
      beforeEachMove()
      sendPointerEvent(PointerEventType.Move, from + (to - from) * (step.toFloat() / steps), buttons = PointerButtons(isPrimaryPressed = true))
      render()
    }
    sendPointerEvent(PointerEventType.Release, to, buttons = PointerButtons(), button = PointerButton.Primary)
  }

  private fun ImageComposeScene.click(x: Float) {
    sendPointerEvent(PointerEventType.Press, Offset(x, 300f), buttons = PointerButtons(isPrimaryPressed = true), button = PointerButton.Primary)
    sendPointerEvent(PointerEventType.Release, Offset(x, 300f), buttons = PointerButtons(), button = PointerButton.Primary)
  }

  private fun fullScreen(
    provider: ImageGalleryProvider = gallery(quadrants(400, 200)),
    wrapper: @Composable (@Composable () -> Unit) -> Unit = { it() },
    test: ImageComposeScene.(closed: () -> Boolean) -> Unit
  ) {
    var closed = false
    simplexWindowState.windowState = WindowState(size = DpSize(800.dp, 600.dp))
    runBlocking(Dispatchers.Swing) {
      ImageComposeScene(800, 600, Density(1f), coroutineContext = Dispatchers.Unconfined) {
        wrapper { ImageFullScreenView({ provider }) { closed = true } }
      }.use { scene -> scene.test { closed } }
    }
  }

  @Test
  fun testWheelZoomsAtThePointerDragPansAndWheelBackFits() = fullScreen { closed ->
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(390), shownAt(410)))
    wheel(200f, -3f)
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(530), shownAt(560)))
    drag(Offset(300f, 300f), Offset(200f, 300f))
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(430), shownAt(460)))
    wheel(200f, 10f)
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(390), shownAt(410)))
    drag(Offset(300f, 300f), Offset(500f, 300f))
    assertFalse(closed())
    click(300f)
    assertTrue(closed())
  }

  @Test
  fun testPinchZoomsAtThePointerAsTheWheel() = fullScreen { _ ->
    sendPointerEvent(PointerEventType.Move, Offset(200f, 300f))
    pinchZooms.tryEmit(1.2f.pow(3))
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(530), shownAt(560)))
  }

  @Test
  fun testDragKeepsALetterboxedImageInTheView() = fullScreen(gallery(quadrants(200, 400))) { closed ->
    wheel(400f, -8f)
    drag(Offset(750f, 300f), Offset(50f, 300f))
    drag(Offset(750f, 300f), Offset(50f, 300f))
    assertFalse(closed())
    assertEquals(Shown.BLUE, shownAt(790, 200))
  }

  @Test
  fun testShortDragAt4xPansWithThePointer() = fullScreen { closed ->
    wheel(400f, -8f)
    drag(Offset(400f, 300f), Offset(340f, 300f), steps = 6)
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(330), shownAt(350)))
  }

  @Test
  fun testDragFollowsThePointerWhileTheGalleryDecodesTheImageAgain() {
    val image = quadrants(400, 200)
    val larger = quadrants(800, 400)
    val data = mutableStateOf(image)
    fullScreen(gallery(image) { data.value }) { closed ->
      wheel(400f, -8f)
      drag(Offset(400f, 300f), Offset(340f, 300f), steps = 6) {
        data.value = larger.copyOf()
        render()
      }
      assertFalse(closed())
      assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(330), shownAt(350)))
    }
  }

  @Test
  fun testSecondaryButtonDragDoesNotPan() = fullScreen { _ ->
    wheel(400f, -3f)
    sendPointerEvent(PointerEventType.Press, Offset(500f, 300f), buttons = PointerButtons(isSecondaryPressed = true), button = PointerButton.Secondary)
    sendPointerEvent(PointerEventType.Move, Offset(400f, 300f), buttons = PointerButtons(isSecondaryPressed = true))
    sendPointerEvent(PointerEventType.Release, Offset(400f, 300f), buttons = PointerButtons(), button = PointerButton.Secondary)
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(390), shownAt(410)))
  }

  @Test
  fun testClickWithPointerJitterCloses() = fullScreen { closed ->
    wheel(400f, -8f)
    drag(Offset(300f, 300f), Offset(301f, 300f))
    assertTrue(closed())
  }

  @Test
  fun testNarrowerViewKeepsTheZoomedImageInIt() {
    val width = mutableStateOf(800)
    fullScreen(wrapper = { Box(Modifier.size(width.value.dp, 600.dp)) { it() } }) { _ ->
      wheel(0f, -3f)
      assertEquals(Shown.RED, shownAt(600))
      width.value = 600
      render()
      assertEquals(Shown.RED, shownAt(15))
    }
  }

  @Test
  fun testZoomedImageIsClippedToItsView() = fullScreen(wrapper = { Box(Modifier.padding(bottom = 100.dp)) { it() } }) { _ ->
    wheel(400f, -8f)
    assertEquals(listOf(Shown.GREEN, Shown.BACKGROUND), listOf(shownAt(300, 490), shownAt(300, 510)))
  }
}
