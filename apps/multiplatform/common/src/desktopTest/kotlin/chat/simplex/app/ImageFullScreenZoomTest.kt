package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.ImageComposeScene
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
import chat.simplex.common.views.chat.item.panFitsWidth
import kotlinx.coroutines.Dispatchers
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
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

  private fun gallery(image: ByteArray) = object : ImageGalleryProvider {
    override val initialIndex = 0
    override val totalMediaSize = mutableStateOf(1)
    override fun getMedia(index: Int) = if (index == 0) ProviderMedia.Image(image, Image.makeFromEncoded(image).toComposeImageBitmap()) else null
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

  private fun ImageComposeScene.drag(from: Offset, to: Offset, steps: Int = 1) {
    sendPointerEvent(PointerEventType.Press, from, buttons = PointerButtons(isPrimaryPressed = true), button = PointerButton.Primary)
    for (step in 1..steps) {
      sendPointerEvent(PointerEventType.Move, from + (to - from) * (step.toFloat() / steps), buttons = PointerButtons(isPrimaryPressed = true))
    }
    sendPointerEvent(PointerEventType.Release, to, buttons = PointerButtons(), button = PointerButton.Primary)
  }

  private fun ImageComposeScene.click(x: Float) {
    sendPointerEvent(PointerEventType.Press, Offset(x, 300f), buttons = PointerButtons(isPrimaryPressed = true), button = PointerButton.Primary)
    sendPointerEvent(PointerEventType.Release, Offset(x, 300f), buttons = PointerButtons(), button = PointerButton.Primary)
  }

  private fun fullScreen(image: ByteArray = quadrants(400, 200), test: ImageComposeScene.(closed: () -> Boolean) -> Unit) {
    var closed = false
    simplexWindowState.windowState = WindowState(size = DpSize(800.dp, 600.dp))
    ImageComposeScene(800, 600, Density(1f), coroutineContext = Dispatchers.Unconfined) {
      ImageFullScreenView({ gallery(image) }) { closed = true }
    }.use { scene -> scene.test { closed } }
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
  fun testDragBeyondTheEdgeKeepsTheImageAtTheEdge() = fullScreen { closed ->
    wheel(400f, -3f)
    drag(Offset(700f, 300f), Offset(100f, 300f))
    assertEquals(listOf(Shown.RED, Shown.BLUE, Shown.BLUE), listOf(shownAt(90), shownAt(130), shownAt(790)))
    drag(Offset(500f, 300f), Offset(400f, 300f))
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.BLUE), listOf(shownAt(90), shownAt(130)))
  }

  @Test
  fun testVerticalDragAtTheHorizontalEdgeMovesTheImage() = fullScreen { closed ->
    wheel(0f, -3f)
    drag(Offset(300f, 400f), Offset(300f, 300f))
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.GREEN), listOf(shownAt(300, 240), shownAt(300, 270)))
  }

  @Test
  fun testVerticalDragAfterZoomOutAtTheEdgeMovesTheImage() = fullScreen { closed ->
    wheel(400f, -4f)
    drag(Offset(100f, 300f), Offset(700f, 300f))
    wheel(400f, 1f)
    drag(Offset(300f, 400f), Offset(300f, 300f))
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.GREEN), listOf(shownAt(300, 240), shownAt(300, 270)))
  }

  @Test
  fun testDragKeepsTheZoomedImageOverTheView() = fullScreen { closed ->
    wheel(400f, -3f)
    drag(Offset(300f, 100f), Offset(300f, 550f), steps = 45)
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.RED, Shown.GREEN), listOf(shownAt(300, 10), shownAt(300, 330), shownAt(300, 360)))
  }

  @Test
  fun testDragKeepsALetterboxedImageInTheView() = fullScreen(quadrants(200, 400)) { closed ->
    wheel(400f, -8f)
    drag(Offset(750f, 300f), Offset(50f, 300f))
    drag(Offset(750f, 300f), Offset(50f, 300f))
    assertFalse(closed())
    assertEquals(Shown.BLUE, shownAt(790, 200))
  }

  @Test
  fun testAndroidPansOnlyWithinTheWidth() {
    assertEquals(listOf(false, false, false), listOf(-1f, 0f, 1f).map { panFitsWidth(800, 1f, 0f, it) })
    assertFalse(panFitsWidth(800, 2f, 400f, 5f))
    assertTrue(panFitsWidth(800, 2f, 400f, -5f))
    assertTrue(panFitsWidth(800, 2f, 0f, 5f))
  }
}
