package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.use
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.platform.appFilesDir
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.ImageFullScreenView
import chat.simplex.common.views.chat.providerForGallery
import chat.simplex.common.views.helpers.clearImageCaches
import chat.simplex.common.views.helpers.getLoadedImage
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.File
import javax.imageio.ImageIO
import kotlin.test.*

class ReceivedPicturesTest {
  private val sendCmd = chatSendCmdRetry
  private val unrecorded = mutableListOf<String>()
  private var receivedFile: File? = null

  @BeforeTest
  fun setUp() {
    simplexWindowState.windowState = WindowState(size = DpSize(SCENE_SIZE.dp, SCENE_SIZE.dp))
    clearImageCaches()
  }

  @AfterTest
  fun tearDown() {
    receivedFile?.delete()
    clearImageCaches()
    chatSendCmdRetry = sendCmd
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun testReceivedPictureIsShownInTheChat() {
    val (chat, item) = receivedPicture()
    receive(item, filledPng(160, 90, Color.RED))
    scene { PictureItem(chat, item) }.use { scene ->
      assertTrue(scene.renderUntil { it.shows(Color.RED) })
    }
  }

  @Test
  fun testReceivedPictureOverThePixelLimitShowsItsPreview() {
    val (chat, item) = receivedPicture()
    val preview = scene { PictureItem(chat, item) }.use { it.settled() }
    receive(item, binaryPng(8193, 8192))
    runBlocking { getLoadedImage(item.file) }
    assertContentEquals(preview, scene { PictureItem(chat, item) }.use { it.settled() })
  }

  @Test
  fun testReceivedPictureIsShownFullScreen() {
    val (chat, item) = receivedPicture()
    receive(item, filledPng(160, 90, Color.RED))
    scene { ImageFullScreenView({ providerForGallery(chat.chatItems, item.id) {} }, close = {}) }.use { scene ->
      assertTrue(scene.renderUntil { it.centerColor() == Color.RED.rgb })
    }
  }

  @Test
  fun testPictureThatFailedIsDecodedAgainAfterImageCachesAreCleared() = runBlocking {
    val (_, item) = receivedPicture()
    receive(item, filledPng(160, 90, Color.RED).copyOf(8))
    assertNull(getLoadedImage(item.file))
    receive(item, filledPng(160, 90, Color.RED))
    clearImageCaches()
    assertEquals(160, getLoadedImage(item.file)?.first?.width)
  }

  @Composable
  private fun PictureItem(chat: Chat, item: ChatItem) {
    FramedItemView(
      ChatModel.ChatsContext(secondaryContextFilter = null), chat, item, imageProvider = { providerForGallery(chat.chatItems, item.id) {} },
      linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false, showMenu = mutableStateOf(false), showTimestamp = true,
      receiveFile = {}, scrollToItemId = mutableStateOf(null)
    )
  }

  private fun receivedPicture(): Pair<Chat, ChatItem> {
    val commands = Json.parseToJsonElement(File("../../../tests/fixtures/api/files/received-image.json").readText()).jsonObject["commands"]?.jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands?.get(cmd)?.toString() ?: unrecordedRequest(cmd) }
    val (chat, _) = assertNotNull(runBlocking { ChatController.apiGetChat(null, ChatType.Direct, 2, null, pagination = ChatPagination.Last(1)) })
    val item = chat.chatItems.last()
    val file = File(appFilesDir, assertNotNull(item.file?.fileSource).filePath)
    assertFalse(file.exists(), "$file exists")
    receivedFile = file
    return chat to item
  }

  private fun receive(item: ChatItem, data: ByteArray) {
    val file = assertNotNull(item.file)
    appFilesDir.mkdirs()
    File(appFilesDir, assertNotNull(file.fileSource).filePath).writeBytes(data + ByteArray(maxOf(0, file.fileSize.toInt() - data.size)))
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
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

  private fun ImageComposeScene.settled(): ByteArray {
    repeat(50) {
      render(System.nanoTime())
      awaitDecodedPictures()
      Thread.sleep(20)
    }
    return assertNotNull(Bitmap.makeFromImage(render(System.nanoTime())).readPixels())
  }

  private fun Image.shows(color: Color): Boolean {
    val bitmap = Bitmap.makeFromImage(this)
    return (0 until bitmap.width step 2).any { x -> (0 until bitmap.height step 2).any { y -> bitmap.getColor(x, y) == color.rgb } }
  }

  private fun Image.centerColor(): Int = Bitmap.makeFromImage(this).getColor(SCENE_SIZE / 2, SCENE_SIZE / 2)

  private fun filledPng(width: Int, height: Int, color: Color): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply { this.color = color; fillRect(0, 0, width, height); dispose() }
    return png(image)
  }

  private fun binaryPng(width: Int, height: Int): ByteArray = png(BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY))

  private fun png(image: BufferedImage): ByteArray =
    ByteArrayOutputStream().use { out ->
      ImageIO.write(image, "png", out)
      out.toByteArray()
    }

  private companion object {
    const val SCENE_SIZE = 400
  }
}
