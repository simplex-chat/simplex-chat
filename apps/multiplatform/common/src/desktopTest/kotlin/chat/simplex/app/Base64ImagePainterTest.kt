package chat.simplex.app

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toAwtImage
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.use
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.Base64ImagePainter
import chat.simplex.common.platform.base64BitmapCache
import chat.simplex.common.platform.base64BitmapSize
import chat.simplex.common.platform.base64ImageDecoder
import chat.simplex.common.platform.base64ToBitmap
import chat.simplex.common.platform.rememberBase64ImagePainter
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.item.FramedItemView
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.concurrent.CountDownLatch
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue

class Base64ImagePainterTest {
  @Test
  fun testCachedPictureIsShownWithoutWaitingForTheDecoder() {
    val picture = base64Png(41, 31)
    val cached = base64ToBitmap(picture)
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy {
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        assertTrue(scene.render().isRedAt(SCENE_SIZE / 2, SCENE_SIZE / 2))
        assertSame(cached, painter.bitmap)
      }
    }
  }

  @Test
  fun testPictureIsShownAtItsSizeUntilItIsDecoded() {
    val picture = base64Png(1501, 1001)
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy { release ->
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        assertFalse(scene.render().isRedAt(SCENE_SIZE / 2, SCENE_SIZE / 2))
        assertNull(painter.bitmap)
        assertEquals(Size(751f, 501f), painter.intrinsicSize)
        release()
        awaitDecodedPictures()
        assertTrue(scene.render().isRedAt(SCENE_SIZE / 2, SCENE_SIZE / 2))
        assertEquals(IntSize(751, 501), assertNotNull(painter.bitmap).run { IntSize(width, height) })
      }
    }
  }

  @Test
  fun testPictureOverTheSizeLimitIsShownAsTheErrorPicture() {
    val picture = base64Png(4321, 1)
    val error = base64ToBitmap(picture)
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy { release ->
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        scene.render()
        assertNull(painter.bitmap)
        assertEquals(Size(error.width.toFloat(), error.height.toFloat()), painter.intrinsicSize)
        release()
        awaitDecodedPictures()
        scene.render()
        assertContentEquals(error.pixels(), assertNotNull(painter.bitmap).pixels())
      }
    }
  }

  @Test
  fun testPictureSizeIsReadFromItsHeaderAlone() {
    val header = png(1501, 1001).copyOf(33)
    assertEquals(IntSize(751, 501), base64BitmapSize(Base64.getEncoder().encodeToString(header)))
  }

  @Test
  fun testPictureIsNotDecodedForAViewThatIsGone() {
    val picture = base64Png(43, 37)
    whileDecoderIsBusy {
      scene { Image(rememberBase64ImagePainter(picture), null, Modifier.fillMaxSize()) }.use { it.render() }
    }
    awaitDecodedPictures()
    assertNull(base64BitmapCache[picture])
  }

  @Test
  fun testChatItemKeepsItsSizeWhenItsPictureIsDecoded() {
    val contents = listOf(
      MsgContent.MCImage("", base64Png(301, 199)),
      MsgContent.MCVideo("", base64Png(303, 197), 10),
      MsgContent.MCLink("https://simplex.chat", LinkPreview("https://simplex.chat", "SimpleX Chat", "", base64Png(305, 195))),
    )
    val blurRadius = appPrefs.privacyMediaBlurRadius.get()
    try {
      appPrefs.privacyMediaBlurRadius.set(0)
      simplexWindowState.windowState = WindowState(size = DpSize(ROW_WIDTH.dp, ROW_HEIGHT.dp))
      for (content in contents) {
        val placed = mutableMapOf<String, Placed>()
        whileDecoderIsBusy { release ->
          scene(ROW_WIDTH, ROW_HEIGHT) { Box(Modifier.record(placed, "item")) { ReceivedItem(content) } }.use { scene ->
            val pending = scene.render()
            val pendingItem = placed.getValue("item")
            release()
            awaitDecodedPictures()
            val decoded = scene.render()
            val name = content::class.simpleName
            assertEquals(pendingItem, placed.getValue("item"), name)
            assertFalse(pending.hasRedOnRow(20), name)
            assertTrue(decoded.hasRedOnRow(20), name)
          }
        }
      }
    } finally {
      appPrefs.privacyMediaBlurRadius.set(blurRadius)
    }
  }

  @Composable
  private fun ReceivedItem(content: MsgContent) {
    FramedItemView(
      ChatModel.ChatsContext(secondaryContextFilter = null), Chat.sampleData,
      ChatItem.getSampleData(1, CIDirection.DirectRcv(), text = content.text).copy(content = CIContent.RcvMsgContent(content)),
      imageProvider = { error("not opened") }, linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false,
      showMenu = mutableStateOf(false), showTimestamp = true, receiveFile = {}, scrollToItemId = mutableStateOf(null)
    )
  }

  private fun <T> whileDecoderIsBusy(block: (release: () -> Unit) -> T): T {
    val busy = CountDownLatch(1)
    val released = CountDownLatch(1)
    CoroutineScope(base64ImageDecoder).launch {
      busy.countDown()
      released.await()
    }
    busy.await()
    try {
      return block { released.countDown() }
    } finally {
      released.countDown()
    }
  }

  private fun scene(width: Int = SCENE_SIZE, height: Int = SCENE_SIZE, content: @Composable () -> Unit): ImageComposeScene =
    ImageComposeScene(width = width, height = height, density = Density(1f), coroutineContext = Dispatchers.Unconfined, content = content)

  private fun Image.isRedAt(x: Int, y: Int): Boolean = Bitmap.makeFromImage(this).getColor(x, y) == Color.RED.rgb

  private fun Image.hasRedOnRow(y: Int): Boolean {
    val bitmap = Bitmap.makeFromImage(this)
    return (0 until width).any { bitmap.getColor(it, y) == Color.RED.rgb }
  }

  private fun ImageBitmap.pixels(): IntArray = toAwtImage().run { getRGB(0, 0, width, height, null, 0, width) }

  private fun base64Png(width: Int, height: Int): String =
    "data:image/png;base64," + Base64.getEncoder().encodeToString(png(width, height))

  private fun png(width: Int, height: Int): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply { color = Color.RED; fillRect(0, 0, width, height); dispose() }
    return ByteArrayOutputStream().use { out ->
      ImageIO.write(image, "png", out)
      out.toByteArray()
    }
  }

  private companion object {
    const val SCENE_SIZE = 100
    const val ROW_WIDTH = 402
    const val ROW_HEIGHT = 1200
  }
}
