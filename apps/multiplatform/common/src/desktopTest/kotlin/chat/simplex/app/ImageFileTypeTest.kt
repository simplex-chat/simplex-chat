package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.platform.ImageFileType
import chat.simplex.common.platform.chatResizeImageFileUri
import chat.simplex.common.platform.chatResizeImageUri
import chat.simplex.common.platform.isAnimImage
import chat.simplex.common.platform.isImageFile
import chat.simplex.common.platform.isWebpFile
import chat.simplex.common.views.chat.ComposePreview
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.processPickedMedia
import chat.simplex.common.views.helpers.UploadContent
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.URI
import java.nio.file.Path
import javax.imageio.ImageIO
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertIs
import kotlin.test.assertTrue

class ImageFileTypeTest {
  private val resizeImageUri = chatResizeImageUri
  private val resizeImageFileUri = chatResizeImageFileUri

  @AfterTest
  fun tearDown() {
    chatResizeImageUri = resizeImageUri
    chatResizeImageFileUri = resizeImageFileUri
  }

  @Test
  fun testJpegImagesLoad() {
    assertTrue(ImageFileType.JPEG.matches(encoded("jpg")))
    assertTrue(isImageFile(encoded("jpg")))
  }

  @Test
  fun testPngImagesLoad() {
    assertTrue(ImageFileType.PNG.matches(encoded("png")))
    assertTrue(isImageFile(encoded("png")))
  }

  @Test
  fun testGifImagesLoad() {
    assertTrue(ImageFileType.GIF.matches(encoded("gif")))
    assertTrue(isImageFile(encoded("gif")))
  }

  @Test
  fun testWebpImagesLoad() {
    assertTrue(ImageFileType.WEBP.matches(webpHeader))
    assertTrue(isImageFile(webpHeader))
  }

  @Test
  fun testAnimatedImagesByHeader() = withTempDir { dir ->
    assertTrue(isAnimImage(dir.fileUri("animated.webp", webpHeader + byteArrayOf(0x0A, 0, 0, 0, 0x12, 0, 0, 0)), null))
    assertFalse(isAnimImage(dir.fileUri("still.webp", webpHeader + byteArrayOf(0x0A, 0, 0, 0, 0x10, 0, 0, 0)), null))
    assertFalse(isAnimImage(dir.fileUri("simple.webp", webpHeader.copyOf(12) + "VP8 ".toByteArray() + byteArrayOf(0x0A, 0, 0, 0, 0x12)), null))
    assertTrue(isAnimImage(dir.fileUri("image.gif", encoded("gif")), null))
  }

  @Test
  fun testWebpFilesBySignature() = withTempDir { dir ->
    assertTrue(isWebpFile(dir.fileUri("image.gif", webpHeader)))
    assertFalse(isWebpFile(dir.fileUri("image.webp", encoded("gif"))))
    assertFalse(isWebpFile(dir.fileUri("short.webp", webpHeader.copyOf(11))))
  }

  @Test
  fun testAnimatedWebpPreviewFromCore() {
    val webp = File("../../../tests/fixtures/webp/animated.webp").canonicalFile
    val recorded = Json.parseToJsonElement(fixtureText("images/resize-file-uri")).jsonObject.getValue("webp/animated.webp").jsonPrimitive.content
    val requests = mutableListOf<Pair<String, Int>>()
    chatResizeImageFileUri = { path, maxSize -> requests += path to maxSize; recorded }
    val picked = pickedMedia(webp.toURI())
    assertEquals(listOf(webp.path to 14000), requests)
    assertEquals(listOf(recorded), picked.images)
    assertEquals(listOf<UploadContent>(UploadContent.AnimatedImage(webp.toURI())), picked.content)
  }

  @Test
  fun testGifPreviewFromBitmap() = withTempDir { dir ->
    val gif = dir.fileUri("image.gif", encoded("gif"))
    val fileRequests = mutableListOf<String>()
    val bitmapRequests = mutableListOf<Int>()
    chatResizeImageFileUri = { path, _ -> fileRequests += path; "" }
    chatResizeImageUri = { _, maxSize -> bitmapRequests += maxSize; "bitmap preview" }
    val picked = pickedMedia(gif)
    assertEquals(listOf(), fileRequests)
    assertEquals(listOf(14000), bitmapRequests)
    assertEquals(listOf("bitmap preview"), picked.images)
    assertEquals(listOf<UploadContent>(UploadContent.AnimatedImage(gif)), picked.content)
  }

  private fun pickedMedia(uri: URI): ComposePreview.MediaPreview {
    val composeState = mutableStateOf(ComposeState(useLinkPreviews = false))
    runBlocking { composeState.processPickedMedia(listOf(uri), null) }
    return assertIs<ComposePreview.MediaPreview>(composeState.value.preview)
  }

  private fun Path.fileUri(name: String, bytes: ByteArray): URI = resolve(name).toFile().apply { writeBytes(bytes) }.toURI()

  private fun encoded(format: String): ByteArray {
    val out = ByteArrayOutputStream()
    ImageIO.write(BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB), format, out)
    return out.toByteArray()
  }

  private val webpHeader = listOf(0x52, 0x49, 0x46, 0x46, 0x36, 0xC8, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x58).map { it.toByte() }.toByteArray()
}
