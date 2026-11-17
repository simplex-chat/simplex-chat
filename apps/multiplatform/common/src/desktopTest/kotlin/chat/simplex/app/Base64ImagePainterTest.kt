package chat.simplex.app

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Size
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
import chat.simplex.common.platform.errorImageBitmap
import chat.simplex.common.platform.rememberBase64ImagePainter
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.helpers.decodeBoundedBufferedImage
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.file.StandardWatchEventKinds
import java.util.Base64
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.zip.CRC32
import javax.imageio.ImageIO
import kotlin.test.Test
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
  fun testPictureOverTheSizeLimitIsShownAsTheErrorPictureWithoutWaitingForTheDecoder() {
    val picture = base64Png(4321, 1)
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy {
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        scene.render()
        assertSame(errorImageBitmap, painter.bitmap)
      }
    }
    assertNull(base64BitmapSize(picture))
    assertSame(errorImageBitmap, base64ToBitmap(picture))
  }

  @Test
  fun testPictureThatCannotBeDecodedIsNotDecodedAgain() {
    val picture = Base64.getEncoder().encodeToString(png(31, 29).copyOf(33))
    assertEquals(IntSize(31, 29), base64BitmapSize(picture))
    assertSame(errorImageBitmap, base64ToBitmap(picture))
    assertSame(errorImageBitmap, base64BitmapCache[picture])
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy {
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        scene.render()
        assertSame(errorImageBitmap, painter.bitmap)
      }
    }
  }

  @Test
  fun testPictureSizeIsReadFromItsHeaderAlone() {
    val header = png(1501, 1001).copyOf(33)
    assertEquals(IntSize(751, 501), base64BitmapSize(Base64.getEncoder().encodeToString(header)))
  }

  @Test
  fun testPicturesAtTheSizeLimitAreDecodedAtPreviewSize() {
    assertEquals(IntSize(540, 540), base64BitmapSize(base64PngHeader(4320, 4320)))
    assertEquals(IntSize(1080, 256), base64BitmapSize(base64PngHeader(4320, 1023)))
    assertEquals(IntSize(4320, 17), base64BitmapSize(base64PngHeader(4320, 17)))
    assertEquals(IntSize(1025, 1024), base64BitmapSize(base64PngHeader(2049, 2047)))
  }

  @Test
  fun testGifPictureWithTooManyMetadataBlocksIsShownAsTheErrorPicture() {
    assertEquals(IntSize(2, 2), base64BitmapSize(base64GifWithComment(1000)))
    assertNull(base64BitmapSize(base64GifWithComment(1025)))
    assertSame(errorImageBitmap, base64ToBitmap(base64GifWithComment(1025)))
  }

  @Test
  fun testReceivedPicturesAreReadWithoutTemporaryFiles() = withTempDir { dir ->
    val cacheDirectory = ImageIO.getCacheDirectory()
    val useCache = ImageIO.getUseCache()
    try {
      ImageIO.setUseCache(true)
      ImageIO.setCacheDirectory(dir.toFile())
      dir.fileSystem.newWatchService().use { watcher ->
        dir.register(watcher, StandardWatchEventKinds.ENTRY_CREATE)
        val picture = base64Png(47, 39)
        assertEquals(IntSize(47, 39), base64BitmapSize(picture))
        assertEquals(47, base64ToBitmap(picture).width)
        assertEquals(47, decodeBoundedBufferedImage(png(47, 39), 1000).width)
        assertNull(watcher.poll(1, TimeUnit.SECONDS))
        ImageIO.createImageInputStream(ByteArrayInputStream(png(1, 1))).close()
        assertNotNull(watcher.poll(10, TimeUnit.SECONDS))
      }
    } finally {
      ImageIO.setCacheDirectory(cacheDirectory)
      ImageIO.setUseCache(useCache)
    }
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
  fun testImageItemKeepsItsSizeWhenItsPreviewIsDecoded() {
    val picture = base64Png(301, 199)
    assertItemKeepsItsSizeWhenDecoded(picture, MsgContent.MCImage("", picture))
  }

  @Test
  fun testVideoItemKeepsItsSizeWhenItsPreviewIsDecoded() {
    val picture = base64Png(303, 197)
    assertItemKeepsItsSizeWhenDecoded(picture, MsgContent.MCVideo("", picture, 10))
  }

  @Test
  fun testLinkItemKeepsItsSizeWhenItsPreviewIsDecoded() {
    val picture = base64Png(305, 195)
    assertItemKeepsItsSizeWhenDecoded(picture, MsgContent.MCLink("https://simplex.chat", LinkPreview("https://simplex.chat", "SimpleX Chat", "", picture)))
  }

  private fun assertItemKeepsItsSizeWhenDecoded(picture: String, content: MsgContent) {
    val placed = mutableMapOf<String, Placed>()
    val blurRadius = appPrefs.privacyMediaBlurRadius.get()
    try {
      appPrefs.privacyMediaBlurRadius.set(0)
      simplexWindowState.windowState = WindowState(size = DpSize(ROW_WIDTH.dp, ROW_HEIGHT.dp))
      whileDecoderIsBusy { release ->
        scene(ROW_WIDTH, ROW_HEIGHT) { Box(Modifier.record(placed, "item")) { ReceivedItem(content) } }.use { scene ->
          assertFalse(scene.render().hasRedOnRow(20))
          val pending = placed.getValue("item")
          release()
          awaitDecodedPictures()
          assertNotNull(base64BitmapCache[picture])
          assertTrue(scene.renderUntil { it.hasRedOnRow(20) })
          assertEquals(pending, placed.getValue("item"))
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
    return (0 until width).any { x -> Color(bitmap.getColor(x, y), true).let { it.red > 204 && it.green < 51 && it.blue < 51 && it.alpha > 204 } }
  }

  private fun ImageComposeScene.renderUntil(rendered: (Image) -> Boolean): Boolean {
    val deadline = System.nanoTime() + 30_000_000_000L
    while (System.nanoTime() < deadline) {
      Snapshot.sendApplyNotifications()
      if (rendered(render(System.nanoTime()))) return true
      Thread.sleep(20)
    }
    return false
  }

  private fun base64Png(width: Int, height: Int): String =
    "data:image/png;base64," + Base64.getEncoder().encodeToString(png(width, height))

  private fun base64GifWithComment(subBlocks: Int): String {
    val gif = encodeImage(2, 2, "gif")
    val flags = gif[10].toInt()
    val headerSize = 13 + if (flags and 0x80 != 0) 3 shl ((flags and 7) + 1) else 0
    val comment = byteArrayOf(0x21, 0xFE.toByte()) + ByteArray(subBlocks * 2) { if (it % 2 == 0) 1 else 'x'.code.toByte() } + byteArrayOf(0)
    return Base64.getEncoder().encodeToString(gif.copyOf(headerSize) + comment + gif.copyOfRange(headerSize, gif.size))
  }

  private fun png(width: Int, height: Int): ByteArray = encodeImage(width, height, "png")

  private fun base64PngHeader(width: Int, height: Int): String {
    val ihdr = ByteBuffer.allocate(17).put("IHDR".toByteArray()).putInt(width).putInt(height).put(byteArrayOf(8, 2, 0, 0, 0)).array()
    val crc = CRC32().apply { update(ihdr) }.value.toInt()
    val header = ByteBuffer.allocate(33).put(png(1, 1).copyOf(8)).putInt(13).put(ihdr).putInt(crc).array()
    return Base64.getEncoder().encodeToString(header)
  }

  private fun encodeImage(width: Int, height: Int, format: String): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply { color = Color.RED; fillRect(0, 0, width, height); dispose() }
    return ByteArrayOutputStream().use { out ->
      ImageIO.write(image, format, out)
      out.toByteArray()
    }
  }

  private companion object {
    const val SCENE_SIZE = 100
    const val ROW_WIDTH = 402
    const val ROW_HEIGHT = 1200
  }
}
