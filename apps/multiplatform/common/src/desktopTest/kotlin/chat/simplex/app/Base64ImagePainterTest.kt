package chat.simplex.app

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.asSkiaBitmap
import androidx.compose.ui.input.pointer.PointerEventType
import androidx.compose.ui.input.pointer.PointerType
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.use
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.Base64ImagePainter
import chat.simplex.common.platform.VideoPlayerHolder
import chat.simplex.common.platform.appFilesDir
import chat.simplex.common.platform.base64BitmapCache
import chat.simplex.common.platform.base64BitmapSize
import chat.simplex.common.platform.base64ImageDecoder
import chat.simplex.common.platform.base64ToBitmap
import chat.simplex.common.platform.byteCount
import chat.simplex.common.platform.errorImageBitmap
import chat.simplex.common.platform.rememberBase64ImagePainter
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.ProviderMedia
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.ImageFullScreenView
import chat.simplex.common.views.chat.item.ImageGalleryProvider
import chat.simplex.common.views.chatlist.ChatPreviewView
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.common.views.helpers.decodeBoundedBufferedImage
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.ByteBuffer
import java.nio.file.StandardWatchEventKinds
import java.util.Base64
import java.util.UUID
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
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
        assertTrue(scene.renderUntil { it.isRedAt(SCENE_SIZE / 2, SCENE_SIZE / 2) })
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
        assertEquals(Size(31f, 29f), painter.intrinsicSize)
      }
    }
  }

  @Test
  fun testPictureThatFailsToDecodeKeepsTheSizeOfItsHeader() {
    val picture = Base64.getEncoder().encodeToString(png(37, 23).copyOf(33))
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy { release ->
      scene { Image(rememberBase64ImagePainter(picture).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        scene.render()
        assertNull(painter.bitmap)
        assertEquals(Size(37f, 23f), painter.intrinsicSize)
        release()
        awaitDecodedPictures()
        assertTrue(scene.renderUntil { painter.bitmap != null })
        assertSame(errorImageBitmap, painter.bitmap)
        assertEquals(Size(37f, 23f), painter.intrinsicSize)
      }
    }
  }

  @Test
  fun testCachedPicturesOutnumberingTwoHundredAreShownWithoutWaitingForTheDecoder() {
    val pictures = (0 until 300).map { base64Png(1 + it % 20, 100 + it / 20) }
    val first = base64ToBitmap(pictures.first())
    pictures.drop(1).forEach { base64ToBitmap(it) }
    lateinit var painter: Base64ImagePainter
    whileDecoderIsBusy {
      scene { Image(rememberBase64ImagePainter(pictures.first()).also { painter = it }, null, Modifier.fillMaxSize()) }.use { scene ->
        scene.render()
        assertSame(first, painter.bitmap)
      }
    }
  }

  @Test
  fun testCachedPicturesAreCountedByTheirBitmapMemory() {
    for (bitmap in listOf(base64ToBitmap(base64Png(53, 29)), errorImageBitmap)) {
      val pixels = bitmap.asSkiaBitmap()
      assertEquals(pixels.rowBytes.toLong() * pixels.height, bitmap.byteCount())
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
    val data = png(47, 39)
    val picture = Base64.getEncoder().encodeToString(data)
    val cacheDirectory = ImageIO.getCacheDirectory()
    val useCache = ImageIO.getUseCache()
    try {
      ImageIO.setUseCache(true)
      ImageIO.setCacheDirectory(dir.toFile())
      dir.fileSystem.newWatchService().use { watcher ->
        dir.register(watcher, StandardWatchEventKinds.ENTRY_CREATE)
        assertEquals(IntSize(47, 39), base64BitmapSize(picture))
        assertEquals(47, base64ToBitmap(picture).width)
        assertEquals(47, decodeBoundedBufferedImage(data, 1000).width)
        assertNull(watcher.poll(1, TimeUnit.SECONDS))
        ImageIO.createImageInputStream(ByteArrayInputStream(data)).close()
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

  @Test
  fun testBlurredItemShowsItsBlurredPreviewOnceDecoded() {
    val picture = base64Png(313, 187)
    withMediaBlurRadius(50) {
      whileDecoderIsBusy { release ->
        scene(ROW_WIDTH, ROW_HEIGHT) { ReceivedItem(MsgContent.MCImage("", picture)) }.use { scene ->
          assertFalse(scene.render().hasRedOnRow(20))
          release()
          awaitDecodedPictures()
          assertTrue(scene.renderUntil { it.hasRedOnRow(20) })
        }
      }
    }
  }

  @Test
  fun testSceneShowsPicturesRequestedAfterItsFirstFrame() {
    val picture = base64Png(61, 47)
    val image = renderScene(width = SCENE_SIZE, height = SCENE_SIZE, frames = 2) {
      var shown by remember { mutableStateOf(false) }
      LaunchedEffect(Unit) { shown = true }
      if (shown) Image(rememberBase64ImagePainter(picture), null, Modifier.fillMaxSize())
    }
    assertTrue(image.isRedAt(SCENE_SIZE / 2, SCENE_SIZE / 2))
  }

  @Test
  fun testVideoItemShowsItsStatusWhileItsPreviewIsDecoded() {
    val picture = base64Png(307, 193)
    val file = CIFile.getSample(fileName = "video.mp4", fileStatus = CIFileStatus.RcvInvitation)
    withMediaBlurRadius(0) {
      whileDecoderIsBusy { release ->
        scene(ROW_WIDTH, ROW_HEIGHT) { ReceivedItem(MsgContent.MCVideo("", picture, 10), file) }.use { scene ->
          val pending = scene.render()
          assertTrue(pending.hasWhiteIn(0 until 60))
          assertFalse(pending.hasRedOnRow(20))
          release()
          awaitDecodedPictures()
          assertTrue(scene.renderUntil { it.hasRedOnRow(20) })
        }
      }
    }
  }

  @Test
  fun testVideoItemReceivesItsFileWhenTappedWhileItsPreviewIsDecoded() {
    val picture = base64Png(317, 183)
    val file = CIFile.getSample(fileId = 7, fileName = "video.mp4", fileStatus = CIFileStatus.RcvInvitation)
    val received = mutableListOf<Long>()
    val placed = mutableMapOf<String, Placed>()
    withMediaBlurRadius(0) {
      whileDecoderIsBusy {
        scene(ROW_WIDTH, ROW_HEIGHT) {
          Box(Modifier.record(placed, "item")) { ReceivedItem(MsgContent.MCVideo("", picture, 10), file) { received += it } }
        }.use { scene ->
          scene.render()
          val item = placed.getValue("item")
          val center = Offset(item.x + item.width / 2f, item.y + item.height / 2f)
          scene.sendPointerEvent(PointerEventType.Press, center, type = PointerType.Touch)
          scene.sendPointerEvent(PointerEventType.Release, center, type = PointerType.Touch)
          scene.render()
          assertEquals(listOf(7L), received)
        }
      }
    }
  }

  @Test
  fun testBlurredQuoteShowsItsBlurredPictureOnceDecoded() {
    val picture = base64Png(319, 181)
    val quote = CIQuote(chatDir = CIDirection.DirectSnd(), sentAt = Clock.System.now(), content = MsgContent.MCImage("", picture))
    withMediaBlurRadius(50) {
      whileDecoderIsBusy { release ->
        scene(ROW_WIDTH, ROW_HEIGHT) { ReceivedItem(MsgContent.MCText("reply"), quote = quote) }.use { scene ->
          assertFalse(scene.render().hasRedIn(0 until 150))
          release()
          awaitDecodedPictures()
          assertTrue(scene.renderUntil { it.hasRedIn(0 until 150) })
        }
      }
    }
  }

  @Test
  fun testChatListLinkPreviewIsShownOnceDecoded() {
    val picture = base64Png(321, 179)
    val link = MsgContent.MCLink("https://simplex.chat", LinkPreview("https://simplex.chat", "SimpleX Chat", "", picture))
    val chat = Chat.sampleData.copy(chatItems = listOf(ChatItem.getSampleData(1, CIDirection.DirectRcv(), text = link.text).copy(content = CIContent.RcvMsgContent(link))))
    withMediaBlurRadius(0) {
      whileDecoderIsBusy { release ->
        scene(ROW_WIDTH, 150) {
          SimpleXTheme {
            ChatPreviewView(chat, true, null, null, "", disabled = false, linkMode = SimplexLinkMode.DESCRIPTION, inProgress = false, progressByTimeout = false, {})
          }
        }.use { scene ->
          assertFalse(scene.render().hasRedIn(0 until 150))
          release()
          awaitDecodedPictures()
          assertTrue(scene.renderUntil { it.hasRedIn(0 until 150) })
        }
      }
    }
  }

  @Test
  fun testChatListChatLinkPictureIsShownOnceDecoded() {
    val picture = base64Png(325, 175)
    val card = MsgContent.MCChat("", MsgChatLink.Contact("https://simplex.chat/contact", Profile.sampleData.copy(image = picture), business = false))
    val chat = Chat.sampleData.copy(chatItems = listOf(ChatItem.getSampleData(1, CIDirection.DirectRcv(), text = card.text).copy(content = CIContent.RcvMsgContent(card))))
    whileDecoderIsBusy { release ->
      scene(ROW_WIDTH, 150) {
        SimpleXTheme {
          ChatPreviewView(chat, true, null, null, "", disabled = false, linkMode = SimplexLinkMode.DESCRIPTION, inProgress = false, progressByTimeout = false, {})
        }
      }.use { scene ->
        assertFalse(scene.render().hasRedIn(0 until 150))
        release()
        awaitDecodedPictures()
        assertTrue(scene.renderUntil { it.hasRedIn(0 until 150) })
      }
    }
  }

  @Test
  fun testProfilePictureIsShownOnceDecoded() {
    val picture = base64Png(323, 177)
    whileDecoderIsBusy { release ->
      scene { ProfileImage(SCENE_SIZE.dp, picture) }.use { scene ->
        assertFalse(scene.render().hasRedIn(0 until SCENE_SIZE))
        release()
        awaitDecodedPictures()
        assertTrue(scene.renderUntil { it.hasRedIn(0 until SCENE_SIZE) })
      }
    }
  }

  @Test
  fun testVideoIsNotPlayedWhileItsPreviewIsDecoded() {
    val picture = base64Png(309, 191)
    val name = "received-video-test-${UUID.randomUUID()}.mp4"
    val video = File(appFilesDir, name)
    try {
      appFilesDir.mkdirs()
      video.writeBytes(ByteArray(100))
      val file = CIFile.getSample(fileName = name, fileSize = 100, filePath = name)
      withMediaBlurRadius(0) {
        whileDecoderIsBusy {
          scene(ROW_WIDTH, ROW_HEIGHT) { ReceivedItem(MsgContent.MCVideo("", picture, 10), file) }.use { scene ->
            repeat(3) { scene.render() }
            assertTrue(VideoPlayerHolder.players.isEmpty())
          }
        }
      }
    } finally {
      video.delete()
    }
  }

  @Test
  fun testFullScreenVideoIsNotPlayedWhileItsPreviewIsDecoded() {
    simplexWindowState.windowState = WindowState(size = DpSize(SCENE_SIZE.dp, SCENE_SIZE.dp))
    val video = ProviderMedia.Video(File(appFilesDir, "received-video-test.mp4").toURI(), null, base64Png(311, 189))
    val loaded = AtomicBoolean(false)
    val provider = object : ImageGalleryProvider {
      override val initialIndex = 5000
      override val totalMediaSize = mutableStateOf(10000)
      override fun getMedia(index: Int): ProviderMedia {
        loaded.set(true)
        return video
      }
      override fun currentPageChanged(index: Int) {}
      override fun scrollToStart() {}
      override fun onDismiss(index: Int) {}
    }
    whileDecoderIsBusy {
      scene { ImageFullScreenView({ provider }, close = {}) }.use { scene ->
        assertTrue(scene.renderUntil { loaded.get() })
        repeat(3) {
          Snapshot.sendApplyNotifications()
          scene.render()
        }
        assertTrue(VideoPlayerHolder.players.isEmpty())
      }
    }
  }

  private fun assertItemKeepsItsSizeWhenDecoded(picture: String, content: MsgContent) {
    val placed = mutableMapOf<String, Placed>()
    withMediaBlurRadius(0) {
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
    }
  }

  private fun <T> withMediaBlurRadius(radius: Int, block: () -> T): T {
    val blurRadius = appPrefs.privacyMediaBlurRadius.get()
    try {
      appPrefs.privacyMediaBlurRadius.set(radius)
      simplexWindowState.windowState = WindowState(size = DpSize(ROW_WIDTH.dp, ROW_HEIGHT.dp))
      return block()
    } finally {
      appPrefs.privacyMediaBlurRadius.set(blurRadius)
    }
  }

  @Composable
  private fun ReceivedItem(content: MsgContent, file: CIFile? = null, quote: CIQuote? = null, receiveFile: (Long) -> Unit = {}) {
    FramedItemView(
      ChatModel.ChatsContext(secondaryContextFilter = null), Chat.sampleData,
      ChatItem.getSampleData(1, CIDirection.DirectRcv(), text = content.text, quotedItem = quote, file = file).copy(content = CIContent.RcvMsgContent(content)),
      imageProvider = { error("not opened") }, linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false,
      showMenu = mutableStateOf(false), showTimestamp = true, receiveFile = receiveFile, scrollToItemId = mutableStateOf(null)
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

  private fun Image.hasRedOnRow(y: Int): Boolean = hasRedIn(y..y)

  private fun Image.hasRedIn(rows: IntRange): Boolean = hasPixelIn(rows) { it.red > 204 && it.green < 51 && it.blue < 51 && it.alpha > 204 }

  private fun Image.hasWhiteIn(rows: IntRange): Boolean = hasPixelIn(rows) { it.red > 230 && it.green > 230 && it.blue > 230 && it.alpha > 230 }

  private fun Image.hasPixelIn(rows: IntRange, matches: (Color) -> Boolean): Boolean {
    val bitmap = Bitmap.makeFromImage(this)
    return rows.any { y -> (0 until width).any { x -> matches(Color(bitmap.getColor(x, y), true)) } }
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
