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
  fun testSampleSizeKeepsSmallerSideAtTarget() {
    // At or below the target on both sides -> no subsampling
    assertEquals(1, imageSampleSize(4320, 4320, 4320))
    // Portrait phone screenshot at the chat-render target: the narrow side is already below the target, so it is kept
    // at full resolution instead of being over-subsampled (this was the reported blurry-preview regression)
    assertEquals(1, imageSampleSize(1179, 2556, 1000))
    // Only the larger side is over the target while the smaller side is tiny -> kept at full resolution
    assertEquals(1, imageSampleSize(4321, 100, 4320))
    // Both sides well above the target -> halved until the smaller side approaches the target
    assertEquals(2, imageSampleSize(8640, 8640, 4320))
    assertEquals(4, imageSampleSize(4000, 4000, 1000))
  }

  @Test
  fun testSampleSizeCapsDecodedPixelsForExtremeAspectRatios() {
    // Smaller-side semantics alone would keep these at full resolution (sampleSize 1, since the narrow side is below
    // the target), decoding ~131 MB (thumbnail) / ~566 MB (full-screen) rasters. The decoded-pixel ceiling forces
    // extra subsampling so memory stays bounded, even though the narrow side then drops below the target.
    assertEquals(2, imageSampleSize(16384, 1999, 1000)) // 16384x1999 (~32.7 MP) -> 8192x999 (~8 MP)
    assertEquals(4, imageSampleSize(16384, 8639, 4320)) // 16384x8639 (~141 MP) -> 4096x2159 (~8.8 MP)
    // A large near-square image is bounded to the ceiling rather than left at the smaller-side target
    assertEquals(4, imageSampleSize(16384, 16384, 4320)) // 8192x8192 (~67 MP) would exceed the ceiling -> 4096x4096
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
  fun testDecodeAcceptsElongatedImageWithinSourceLimit() {
    // Long, thin image at the source-dimension boundary: the raster is small, so it is decoded at full size
    // (like Android) rather than over-subsampled to fit the larger side under the target
    val image = decodeBoundedBufferedImage(encodePng(16384, 8), 4320)
    assertEquals(16384, image.width)
  }

  @Test
  fun testDecodeDownsamplesLargeImageInsteadOfRejecting() {
    // Large image with both sides above the target: must decode (not reject) AND be subsampled - proves subsampling
    // is honored so the decoded raster stays bounded, while keeping the smaller side at or above the target.
    val image = decodeBoundedBufferedImage(encodePng(2400, 2400), 1000)
    assertTrue(image.width < 2400, "expected downsampled width, got ${image.width}")
    assertTrue(image.width >= 1000, "expected smaller side kept at or above target, got ${image.width}")
  }

  private fun encodePng(width: Int, height: Int): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    return ByteArrayOutputStream().use { out ->
      ImageIO.write(image, "png", out)
      out.toByteArray()
    }
  }
}
