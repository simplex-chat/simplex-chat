package chat.simplex.app

import chat.simplex.common.views.helpers.decodeBoundedBufferedImage
import chat.simplex.common.views.helpers.imageSampleSize
import chat.simplex.common.views.helpers.sourceDimensionsWithinLimits
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.IOException
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ImageDecodeBoundsTest {
  @Test
  fun testSampleSizeKeepsImagesWithinTargetDimension() {
    assertEquals(1, imageSampleSize(4320, 4320, 4320)) // at the target -> no subsampling
    assertEquals(2, imageSampleSize(4321, 100, 4320))  // one side just over -> halved
    assertEquals(2, imageSampleSize(8640, 8640, 4320))
    assertEquals(4, imageSampleSize(16384, 16384, 4320))
  }

  @Test
  fun testSourceDimensionsRejectAbsurdSizes() {
    assertTrue(sourceDimensionsWithinLimits(16384, 16384)) // generous upper bound, downsampled later
    assertFalse(sourceDimensionsWithinLimits(16385, 1))
    assertFalse(sourceDimensionsWithinLimits(0, 1))
  }

  @Test
  fun testDecodeRejectsImageExceedingSourceLimit() {
    // Rejected at the dimension check before any full-size allocation
    assertFailsWith<IOException> { decodeBoundedBufferedImage(encodePng(16385, 1), 4320) }
  }

  @Test
  fun testDecodeDownsamplesLargeDimensionImageInsteadOfRejecting() {
    // Image-bomb shaped: large declared width (within source cap), tiny encoded size.
    // Must decode (not reject) AND be downsampled - proves subsampling is honored, so memory stays bounded.
    val image = decodeBoundedBufferedImage(encodePng(16384, 8), 4320)
    assertTrue(image.width <= 4320, "expected downsampled width, got ${image.width}")
  }

  private fun encodePng(width: Int, height: Int): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    return ByteArrayOutputStream().use { out ->
      ImageIO.write(image, "png", out)
      out.toByteArray()
    }
  }
}
