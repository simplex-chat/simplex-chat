package chat.simplex.app

import chat.simplex.common.platform.ImageFileType
import chat.simplex.common.platform.isAnimImage
import chat.simplex.common.platform.isImageFile
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
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
    fun animated(name: String, bytes: List<Int>): Boolean {
      val file = dir.resolve(name).toFile()
      file.writeBytes(bytes.map { it.toByte() }.toByteArray())
      return isAnimImage(file.toURI(), null)
    }
    val vp8x = listOf(0x52, 0x49, 0x46, 0x46, 0x16, 0x00, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x58, 0x0A, 0x00, 0x00, 0x00)
    assertTrue(animated("animated.webp", vp8x + listOf(0x12, 0x00, 0x00, 0x00)))
    assertFalse(animated("still.webp", vp8x + listOf(0x10, 0x00, 0x00, 0x00)))
    assertFalse(animated("simple.webp", listOf(0x52, 0x49, 0x46, 0x46, 0x16, 0x00, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x20, 0x0A, 0x00, 0x00, 0x00, 0x12)))
    assertTrue(animated("image.gif", listOf(0x47, 0x49, 0x46, 0x38, 0x39, 0x61)))
  }

  private fun encoded(format: String): ByteArray {
    val out = ByteArrayOutputStream()
    ImageIO.write(BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB), format, out)
    return out.toByteArray()
  }

  private val webpHeader = listOf(0x52, 0x49, 0x46, 0x46, 0x36, 0xC8, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x58).map { it.toByte() }.toByteArray()
}
