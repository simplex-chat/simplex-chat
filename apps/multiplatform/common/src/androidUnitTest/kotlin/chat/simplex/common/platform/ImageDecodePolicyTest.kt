package chat.simplex.common.platform

import java.io.ByteArrayInputStream
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ImageDecodePolicyTest {
  @Test
  fun boundedSamplingCoversDimensionsOrientationAndPixelEdges() {
    data class Case(val width: Int, val height: Int, val target: Int, val sample: Int?)

    listOf(
      Case(800, 600, MAX_THUMBNAIL_DIMENSION, 1),
      Case(100_000, 1_000, MAX_THUMBNAIL_DIMENSION, null),
      Case(1_000, 100_000, MAX_THUMBNAIL_DIMENSION, null),
      Case(16_384, 1, MAX_IMAGE_DIMENSION, 1),
      Case(16_385, 1, MAX_IMAGE_DIMENSION, null),
      Case(4_320, 4_320, MAX_IMAGE_DIMENSION, 1),
      Case(4_321, 4_320, MAX_IMAGE_DIMENSION, 2),
      Case(5_000, 5_000, MAX_IMAGE_DIMENSION, 2),
      Case(16_383, 2_279, MAX_THUMBNAIL_DIMENSION, 2),
    ).forEach { (width, height, target, sample) ->
      assertEquals(sample, boundedImageSampleSize(width, height, target), "$width x $height")
    }

    assertNull(boundedImageSampleSize(0, 100, MAX_IMAGE_DIMENSION))
    assertNull(boundedImageSampleSize(100, -1, MAX_IMAGE_DIMENSION))
    val sample = boundedImageSampleSize(16_383, 2_279, MAX_THUMBNAIL_DIMENSION)!!
    val pixels = ((16_383L + sample - 1) / sample) * ((2_279L + sample - 1) / sample)
    assertTrue(pixels <= 18_662_400L)
  }

  @Test
  fun limitingStreamCapsEveryReadOperation() {
    val backing = ByteArrayInputStream(ByteArray(32))
    val limited = LimitedInputStream(backing, 8)
    assertEquals(8, limited.available())
    assertEquals(5L, limited.skip(5))
    assertEquals(2, limited.read(ByteArray(2)))
    assertEquals(0, limited.read(ByteArray(0)))
    assertEquals(0, limited.read())
    assertEquals(-1, limited.read())
    assertEquals(24, backing.available())
  }
}
