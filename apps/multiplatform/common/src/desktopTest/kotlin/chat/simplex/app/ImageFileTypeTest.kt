package chat.simplex.app

import chat.simplex.common.platform.ImageFileType
import chat.simplex.common.platform.isAnimImage
import chat.simplex.common.platform.isImageFile
import chat.simplex.common.platform.isWebpFile
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.file.Path
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ImageFileTypeTest {
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

  private fun Path.fileUri(name: String, bytes: ByteArray): URI = resolve(name).toFile().apply { writeBytes(bytes) }.toURI()

  private fun encoded(format: String): ByteArray {
    val out = ByteArrayOutputStream()
    ImageIO.write(BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB), format, out)
    return out.toByteArray()
  }

  private val webpHeader = listOf(0x52, 0x49, 0x46, 0x46, 0x36, 0xC8, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x58).map { it.toByte() }.toByteArray()
}
