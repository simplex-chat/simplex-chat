package chat.simplex.app

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.layout.ContentScale
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
import chat.simplex.res.MR
import kotlinx.coroutines.Dispatchers
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image as SkiaImage
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals
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
    val placeholder = scene {
      Image(MR.images.decentralized.image.toComposeImageBitmap(), null, Modifier.fillMaxSize(), contentScale = ContentScale.Fit)
    }.use { it.render().pixels() }
    scene { FullScreenImageView(Modifier.fillMaxSize(), "not an image".toByteArray(), solidImage(Color.BLUE)) }.use { scene ->
      assertTrue(scene.renderUntil { it.pixels().contentEquals(placeholder) })
    }
  }

  @Test
  fun testGalleryLooksForPreviousImageOffTheUiThread() {
    simplexWindowState.windowState = WindowState(size = DpSize(SCENE_SIZE.dp, SCENE_SIZE.dp))
    val image = ProviderMedia.Image(encodePng(2, 2, Color.RED), solidImage(Color.RED))
    val lookedUpOn = AtomicReference<Thread>()
    val scrolledToStart = AtomicBoolean(false)
    val provider = object : ImageGalleryProvider {
      override val initialIndex = 5000
      override val totalMediaSize = mutableStateOf(10000)
      override fun getMedia(index: Int): ProviderMedia? =
        if (index == initialIndex - 1) null.also { lookedUpOn.set(Thread.currentThread()) } else image
      override fun currentPageChanged(index: Int) {}
      override fun scrollToStart() = scrolledToStart.set(true)
      override fun onDismiss(index: Int) {}
    }
    scene { ImageFullScreenView({ provider }, close = {}) }.use { scene ->
      assertTrue(scene.renderUntil { scrolledToStart.get() })
    }
    assertNotEquals(Thread.currentThread(), assertNotNull(lookedUpOn.get()))
  }

  private fun scene(content: @Composable () -> Unit): ImageComposeScene =
    ImageComposeScene(width = SCENE_SIZE, height = SCENE_SIZE, density = Density(1f), coroutineContext = Dispatchers.Unconfined, content = content)

  private fun ImageComposeScene.renderUntil(rendered: (SkiaImage) -> Boolean): Boolean {
    val deadline = System.nanoTime() + 30_000_000_000L
    while (System.nanoTime() < deadline) {
      Snapshot.sendApplyNotifications()
      if (rendered(render(System.nanoTime()))) return true
      Thread.sleep(20)
    }
    return false
  }

  private fun SkiaImage.centerColor(): Int = Bitmap.makeFromImage(this).getColor(SCENE_SIZE / 2, SCENE_SIZE / 2)

  private fun SkiaImage.pixels(): ByteArray = assertNotNull(Bitmap.makeFromImage(this).readPixels())

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
