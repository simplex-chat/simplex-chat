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
  private val redAndBlue: ByteArray = run {
    val image = BufferedImage(400, 200, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply {
      color = java.awt.Color.RED
      fillRect(0, 0, 200, 200)
      color = java.awt.Color.BLUE
      fillRect(200, 0, 200, 200)
      dispose()
    }
    ByteArrayOutputStream().also { ImageIO.write(image, "png", it) }.toByteArray()
  }

  private val gallery = object : ImageGalleryProvider {
    override val initialIndex = 0
    override val totalMediaSize = mutableStateOf(1)
    override fun getMedia(index: Int) = if (index == 0) ProviderMedia.Image(redAndBlue, Image.makeFromEncoded(redAndBlue).toComposeImageBitmap()) else null
    override fun currentPageChanged(index: Int) {}
    override fun scrollToStart() {}
    override fun onDismiss(index: Int) {}
  }

  private enum class Shown { RED, BLUE, BACKGROUND }

  private fun ImageComposeScene.shownAt(x: Int, y: Int = 300): Shown? {
    val color = Color(Bitmap.makeFromImage(render()).getColor(x, y))
    return when {
      color.red > 0.8f && color.blue < 0.2f -> Shown.RED
      color.blue > 0.8f && color.red < 0.2f -> Shown.BLUE
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

  private fun fullScreen(test: ImageComposeScene.(closed: () -> Boolean) -> Unit) {
    var closed = false
    simplexWindowState.windowState = WindowState(size = DpSize(800.dp, 600.dp))
    ImageComposeScene(800, 600, Density(1f), coroutineContext = Dispatchers.Unconfined) {
      ImageFullScreenView({ gallery }) { closed = true }
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
    assertEquals(listOf(Shown.RED, Shown.BACKGROUND), listOf(shownAt(300, 500), shownAt(300, 570)))
  }

  @Test
  fun testVerticalDragAfterZoomOutAtTheEdgeMovesTheImage() = fullScreen { closed ->
    wheel(400f, -4f)
    drag(Offset(100f, 300f), Offset(700f, 300f))
    wheel(400f, 1f)
    drag(Offset(300f, 400f), Offset(300f, 300f))
    assertFalse(closed())
    assertEquals(listOf(Shown.RED, Shown.BACKGROUND), listOf(shownAt(300, 500), shownAt(300, 570)))
  }

  @Test
  fun testVerticalDragStopsAtTheImageEdge() = fullScreen { closed ->
    wheel(400f, -3f)
    drag(Offset(300f, 100f), Offset(300f, 550f), steps = 45)
    assertFalse(closed())
    assertEquals(listOf(Shown.BACKGROUND, Shown.RED), listOf(shownAt(300, 150), shownAt(300, 200)))
  }
}
