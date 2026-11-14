package chat.simplex.app

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.use
import androidx.compose.ui.window.WindowState
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.ProviderMedia
import chat.simplex.common.views.chat.item.FullScreenImageView
import chat.simplex.common.views.chat.item.ImageFullScreenView
import chat.simplex.common.views.chat.item.ImageGalleryProvider
import kotlinx.coroutines.Dispatchers
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ImageFullScreenViewTest {
  @Test
  fun testFullScreenImageShowsLoadedImageUntilDecoded() {
    val data = encodePng(3000, 3000, Color.RED)
    scene { FullScreenImageView(Modifier.fillMaxSize(), data, solidImage(Color.BLUE)) }.use { scene ->
      assertEquals(Color.BLUE.rgb, scene.render().centerColor())
      assertTrue(scene.renderUntil { it.centerColor() == Color.RED.rgb })
    }
  }

  @Test
  fun testFullScreenImageShowsPlaceholderWhenNotDecoded() {
    val data = "not an image".toByteArray()
    val placeholder = scene { FullScreenImageView(Modifier.fillMaxSize(), data, solidImage(Color.GREEN)) }.use { scene ->
      assertTrue(scene.renderUntil { it.centerColor() != Color.GREEN.rgb })
      scene.render().pixels()
    }
    scene { FullScreenImageView(Modifier.fillMaxSize(), data, solidImage(Color.BLUE)) }.use { scene ->
      assertTrue(scene.renderUntil { it.pixels().contentEquals(placeholder) })
    }
  }

  @Test
  fun testGalleryLoadsImagesOffTheUiThread() {
    simplexWindowState.windowState = WindowState(size = DpSize(SCENE_SIZE.dp, SCENE_SIZE.dp))
    val image = ProviderMedia.Image(encodePng(2, 2, Color.RED), solidImage(Color.RED))
    val loadedOn = ConcurrentLinkedQueue<Thread>()
    val scrolledToStart = AtomicBoolean(false)
    val provider = object : ImageGalleryProvider {
      override val initialIndex = 5000
      override val totalMediaSize = mutableStateOf(10000)
      override fun getMedia(index: Int): ProviderMedia? {
        loadedOn += Thread.currentThread()
        return if (index == initialIndex - 1) null else image
      }
      override fun currentPageChanged(index: Int) {}
      override fun scrollToStart() = scrolledToStart.set(true)
      override fun onDismiss(index: Int) {}
    }
    scene { ImageFullScreenView({ provider }, close = {}) }.use { scene ->
      assertTrue(scene.renderUntil { scrolledToStart.get() && it.centerColor() == Color.RED.rgb })
    }
    assertTrue(loadedOn.isNotEmpty())
    assertTrue(loadedOn.none { it == Thread.currentThread() })
  }

  @Test
  fun testGalleryShowsNothingUntilTheImageIsLoaded() {
    simplexWindowState.windowState = WindowState(size = DpSize(SCENE_SIZE.dp, SCENE_SIZE.dp))
    val image = ProviderMedia.Image(encodePng(2, 2, Color.RED), solidImage(Color.RED))
    val imageLoaded = CountDownLatch(1)
    val closed = AtomicBoolean(false)
    val provider = object : ImageGalleryProvider {
      override val initialIndex = 5000
      override val totalMediaSize = mutableStateOf(10000)
      override fun getMedia(index: Int): ProviderMedia? {
        if (index == initialIndex) imageLoaded.await(3, TimeUnit.SECONDS)
        return image
      }
      override fun currentPageChanged(index: Int) {}
      override fun scrollToStart() {}
      override fun onDismiss(index: Int) {}
    }
    scene { ImageFullScreenView({ provider }, close = { closed.set(true) }) }.use { scene ->
      repeat(3) { assertNotEquals(Color.RED.rgb, scene.render().centerColor()) }
      imageLoaded.countDown()
      assertTrue(scene.renderUntil { it.centerColor() == Color.RED.rgb })
    }
    assertFalse(closed.get())
  }

  private fun scene(content: @Composable () -> Unit): ImageComposeScene =
    ImageComposeScene(width = SCENE_SIZE, height = SCENE_SIZE, density = Density(1f), coroutineContext = Dispatchers.Unconfined, content = content)

  private fun ImageComposeScene.renderUntil(rendered: (Image) -> Boolean): Boolean {
    val deadline = System.nanoTime() + 30_000_000_000L
    while (System.nanoTime() < deadline) {
      Snapshot.sendApplyNotifications()
      if (rendered(render(System.nanoTime()))) return true
      Thread.sleep(20)
    }
    return false
  }

  private fun Image.centerColor(): Int = Bitmap.makeFromImage(this).getColor(SCENE_SIZE / 2, SCENE_SIZE / 2)

  private fun Image.pixels(): ByteArray = assertNotNull(Bitmap.makeFromImage(this).readPixels())

  private fun solidImage(color: Color): ImageBitmap = filledImage(4, 4, color).toComposeImageBitmap()

  private fun encodePng(width: Int, height: Int, color: Color): ByteArray =
    ByteArrayOutputStream().use { out ->
      ImageIO.write(filledImage(width, height, color), "png", out)
      out.toByteArray()
    }

  private fun filledImage(width: Int, height: Int, color: Color): BufferedImage =
    BufferedImage(width, height, BufferedImage.TYPE_INT_RGB).also { image ->
      val graphics = image.createGraphics()
      graphics.color = color
      graphics.fillRect(0, 0, width, height)
      graphics.dispose()
    }

  private companion object {
    const val SCENE_SIZE = 100
  }
}
