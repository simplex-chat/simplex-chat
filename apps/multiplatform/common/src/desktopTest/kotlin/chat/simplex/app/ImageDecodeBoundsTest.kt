package chat.simplex.app

import chat.simplex.common.views.helpers.imageDimensionsWithinLimits
import chat.simplex.common.views.helpers.validateImageDataWithinLimits
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.IOException
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ImageDecodeBoundsTest {
  @Test
  fun testImageDimensionsAllowBoundary() {
    assertTrue(imageDimensionsWithinLimits(4320, 4320))
  }

  @Test
  fun testImageDimensionsRejectOversizedWidth() {
    assertFalse(imageDimensionsWithinLimits(4321, 1))
  }

  @Test
  fun testImageValidationRejectsOversizedEncodedImage() {
    val image = BufferedImage(4321, 1, BufferedImage.TYPE_INT_RGB)
    val data = ByteArrayOutputStream().use { out ->
      ImageIO.write(image, "png", out)
      out.toByteArray()
    }

    assertFailsWith<IOException> {
      validateImageDataWithinLimits(data)
    }
  }
}
