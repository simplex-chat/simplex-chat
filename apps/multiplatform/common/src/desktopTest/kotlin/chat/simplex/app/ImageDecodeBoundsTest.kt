package chat.simplex.app

import androidx.compose.ui.graphics.toAwtImage
import chat.simplex.common.platform.base64ToBitmap
import chat.simplex.common.views.helpers.decodeBoundedBufferedImage
import chat.simplex.common.views.helpers.imageSampleSize
import chat.simplex.common.views.helpers.sourceDimensionsWithinLimits
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.util.Base64
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
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
    assertEquals(4, imageSampleSize(16384, 1999, 1000))
    assertEquals(4, imageSampleSize(16384, 1139, 1000))
    assertEquals(4, imageSampleSize(16384, 8639, 4320)) // 16384x8639 (~141 MP) -> 4096x2159 (~8.8 MP)
    // A large near-square image is bounded to the ceiling rather than left at the smaller-side target
    assertEquals(4, imageSampleSize(16384, 16384, 4320)) // 8192x8192 (~67 MP) would exceed the ceiling -> 4096x4096
  }

  @Test
  fun testSourceDimensionsRejectAbsurdSizes() {
    assertTrue(sourceDimensionsWithinLimits(8192, 8192))
    assertTrue(sourceDimensionsWithinLimits(16384, 4096))
    assertFalse(sourceDimensionsWithinLimits(16384, 4097))
    assertFalse(sourceDimensionsWithinLimits(16385, 1))
    assertFalse(sourceDimensionsWithinLimits(0, 1))
  }

  @Test
  fun testDecodeRejectsImageExceedingSourceLimit() {
    // Rejected at the dimension check before any full-size allocation
    assertFailsWith<IOException> { decodeBoundedBufferedImage(encodePng(16385, 1), 4320) }
  }

  @Test
  fun testDecodeRejectsProgressiveJpegOverSourcePixelLimit() {
    assertFailsWith<IOException> { decodeBoundedBufferedImage(progressiveJpegHeaders(8193, 8192), 1000) }
    assertEquals(16, decodeBoundedBufferedImage(progressiveJpegHeaders(16, 16), 1000).width)
  }

  @Test
  fun testDecodeRendersLimitedNumberOfProgressiveJpegScans() {
    fun pixel(data: ByteArray) = decodeBoundedBufferedImage(data, 1000).raster.getSample(0, 0, 0)
    val unchanged = pixel(progressiveJpegChangedByLastScan(0, changed = false))
    assertTrue(pixel(progressiveJpegChangedByLastScan(0)) < unchanged)
    assertTrue(pixel(progressiveJpegChangedByLastScan(9)) < unchanged)
    assertEquals(unchanged, pixel(progressiveJpegChangedByLastScan(1000)))
  }

  @Test
  fun testBase64ImageRendersLimitedNumberOfProgressiveJpegScans() {
    fun pixel(data: ByteArray) = base64ToBitmap(Base64.getEncoder().encodeToString(data)).toAwtImage().getRGB(0, 0)
    val unchanged = pixel(progressiveJpegChangedByLastScan(0, changed = false))
    assertNotEquals(unchanged, pixel(progressiveJpegChangedByLastScan(9)))
    assertEquals(unchanged, pixel(progressiveJpegChangedByLastScan(1000)))
  }

  @Test
  fun testBase64ImageIsDecodedAtPreviewSize() {
    assertEquals(500, base64ToBitmap(Base64.getEncoder().encodeToString(encodePng(500, 500))).width)
    assertEquals(540, base64ToBitmap(Base64.getEncoder().encodeToString(encodePng(4320, 4320))).width)
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

  @Test
  fun testDecodeAcceptsOnlyFormatsSentByApps() {
    for (format in listOf("png", "jpg", "gif")) {
      assertEquals(2, decodeBoundedBufferedImage(encodeImage(2, 2, format), 4320).width)
    }
    assertFailsWith<IOException> { decodeBoundedBufferedImage(encodeImage(2, 2, "bmp"), 4320) }
    val bmp = ByteArray(54).also {
      "BM".toByteArray().copyInto(it)
      it[2] = 54; it[10] = 54; it[14] = 40
      it[18] = 2; it[22] = 2; it[26] = 1
      it[30] = 5
      it[34] = -1; it[35] = -1; it[36] = -1; it[37] = 0x7f
    }
    assertFailsWith<IOException> { decodeBoundedBufferedImage(bmp, 4320) }
  }

  private fun progressiveJpegHeaders(width: Int, height: Int): ByteArray =
    jpeg(
      0xFF, 0xD8,
      *segment(0xDB, 0, *IntArray(64) { 1 }),
      *segment(0xC2, 8, height shr 8, height and 0xFF, width shr 8, width and 0xFF, 3, 1, 0x11, 0, 2, 0x11, 0, 3, 0x11, 0),
      *segment(0xC4, 0, 1, *IntArray(16)),
      *segment(0xDA, 3, 1, 0, 2, 0, 3, 0, 0, 0, 0),
    )

  private fun progressiveJpegChangedByLastScan(scansBefore: Int, changed: Boolean = true): ByteArray =
    jpeg(
      0xFF, 0xD8,
      *segment(0xDB, 0, *IntArray(64) { 16 }),
      *segment(0xC2, 8, 0, 8, 0, 8, 1, 1, 0x11, 0),
      *segment(0xC4, 0, 1, *IntArray(15), 0),
      *segment(0xC4, 1, 1, *IntArray(15), 4),
      *IntArray(scansBefore).flatMap { segment(0xDA, 1, 1, 0, 0, 0, 0).asList() }.toIntArray(),
      *segment(0xDA, 1, 1, if (changed) 0x10 else 0, 0, 0, 0),
      0xFF, 0xD9,
    )

  private fun segment(marker: Int, vararg data: Int): IntArray = intArrayOf(0xFF, marker, 0, data.size + 2, *data)

  private fun jpeg(vararg bytes: Int): ByteArray = bytes.map { it.toByte() }.toByteArray()

  private fun encodePng(width: Int, height: Int): ByteArray = encodeImage(width, height, "png")

  private fun encodeImage(width: Int, height: Int, format: String): ByteArray {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    return ByteArrayOutputStream().use { out ->
      ImageIO.write(image, format, out)
      out.toByteArray()
    }
  }
}
