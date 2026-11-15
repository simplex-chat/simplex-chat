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

  private enum class Half { RED, BLUE }

  private fun ImageComposeScene.halfAt(x: Int): Half? {
    val color = Color(Bitmap.makeFromImage(render()).getColor(x, 300))
    return when {
      color.red > 0.8f && color.blue < 0.2f -> Half.RED
      color.blue > 0.8f && color.red < 0.2f -> Half.BLUE
      else -> null
    }
  }

  private fun ImageComposeScene.wheel(x: Float, notches: Float) =
    sendPointerEvent(PointerEventType.Scroll, Offset(x, 300f), scrollDelta = Offset(0f, notches))

  private fun ImageComposeScene.drag(fromX: Float, toX: Float) {
    sendPointerEvent(PointerEventType.Press, Offset(fromX, 300f), buttons = PointerButtons(isPrimaryPressed = true), button = PointerButton.Primary)
    sendPointerEvent(PointerEventType.Move, Offset(toX, 300f), buttons = PointerButtons(isPrimaryPressed = true))
    sendPointerEvent(PointerEventType.Release, Offset(toX, 300f), buttons = PointerButtons(), button = PointerButton.Primary)
  }

  @Test
  fun testWheelZoomsAtThePointerDragPansAndWheelBackFits() {
    var closed = false
    simplexWindowState.windowState = WindowState(size = DpSize(800.dp, 600.dp))
    ImageComposeScene(800, 600, Density(1f), coroutineContext = Dispatchers.Unconfined) {
      ImageFullScreenView({ gallery }) { closed = true }
    }.use { scene ->
      assertEquals(listOf(Half.RED, Half.BLUE), listOf(scene.halfAt(390), scene.halfAt(410)))
      scene.wheel(200f, -3f)
      assertEquals(listOf(Half.RED, Half.BLUE), listOf(scene.halfAt(530), scene.halfAt(560)))
      scene.drag(300f, 200f)
      assertFalse(closed)
      assertEquals(listOf(Half.RED, Half.BLUE), listOf(scene.halfAt(430), scene.halfAt(460)))
      scene.wheel(200f, 10f)
      assertEquals(listOf(Half.RED, Half.BLUE), listOf(scene.halfAt(390), scene.halfAt(410)))
      assertFalse(closed)
      scene.drag(300f, 300f)
      assertTrue(closed)
    }
  }
}
