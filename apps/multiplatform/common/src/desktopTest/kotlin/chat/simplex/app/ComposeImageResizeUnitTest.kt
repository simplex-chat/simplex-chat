package chat.simplex.app

import androidx.compose.ui.graphics.toComposeImageBitmap
import chat.simplex.common.platform.resizeImageToDataSize
import chat.simplex.common.views.helpers.MAX_IMAGE_SIZE
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO
import kotlin.math.abs
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ComposeImageResizeUnitTest {
  private fun noise(width: Int, height: Int): BufferedImage {
    val img = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val random = Random(7)
    for (y in 0 until height) for (x in 0 until width) img.setRGB(x, y, random.nextInt(0x1000000))
    return img
  }

  private fun decode(bytes: ByteArray): BufferedImage = ImageIO.read(ByteArrayInputStream(bytes))

  @Test
  fun largeNoiseJpegIsShrunkUnderMaxImageSizeKeepingAspectRatio() {
    val stream = resizeImageToDataSize(noise(3000, 2000).toComposeImageBitmap(), usePng = false, maxDataSize = MAX_IMAGE_SIZE)
    assertTrue(stream.size() <= MAX_IMAGE_SIZE, "size ${stream.size()}")
    val result = decode(stream.toByteArray())
    assertTrue(result.width < 3000, "width ${result.width}")
    assertTrue(abs(result.height - result.width * 2 / 3) <= 2, "${result.width}x${result.height}")
  }

  @Test
  fun largeNoisePngIsShrunkUnderMaxImageSize() {
    val stream = resizeImageToDataSize(noise(1200, 800).toComposeImageBitmap(), usePng = true, maxDataSize = MAX_IMAGE_SIZE)
    assertTrue(stream.size() <= MAX_IMAGE_SIZE, "size ${stream.size()}")
    val bytes = stream.toByteArray()
    assertEquals(listOf(0x89, 0x50, 0x4E, 0x47), bytes.take(4).map { it.toInt() and 0xFF })
    assertTrue(decode(bytes).width < 1200)
  }

  @Test
  fun smallImageKeepsItsSize() {
    val stream = resizeImageToDataSize(noise(100, 60).toComposeImageBitmap(), usePng = false, maxDataSize = MAX_IMAGE_SIZE)
    val result = decode(stream.toByteArray())
    assertEquals(100, result.width)
    assertEquals(60, result.height)
  }
}
