package chat.simplex.app

import chat.simplex.common.platform.ImageFileType
import chat.simplex.common.platform.isImageFile
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import kotlin.test.Test
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

  private fun encoded(format: String): ByteArray {
    val out = ByteArrayOutputStream()
    ImageIO.write(BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB), format, out)
    return out.toByteArray()
  }

  private val webpHeader = listOf(0x52, 0x49, 0x46, 0x46, 0x36, 0xC8, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x58).map { it.toByte() }.toByteArray()
}
