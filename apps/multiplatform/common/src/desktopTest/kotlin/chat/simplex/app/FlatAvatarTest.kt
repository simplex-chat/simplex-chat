package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.luminance
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.ProfileImage
import org.jetbrains.skia.Bitmap
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Base64
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class FlatAvatarTest {
  private val placed = mutableMapOf<String, Placed>()
  private val photo = "data:image/png;base64," + Base64.getEncoder().encodeToString(
    ByteArrayOutputStream().also { ImageIO.write(BufferedImage(8, 8, BufferedImage.TYPE_INT_RGB), "png", it) }.toByteArray()
  )

  private fun inkAcrossMiddle(image: String?, toEdge: Boolean): IntRange {
    val radius = appPrefs.profileImageCornerRadius.get()
    val rendered = try {
      appPrefs.profileImageCornerRadius.set(50f)
      renderScene(width = 40, height = 40) {
        Box(Modifier.size(40.dp).background(Color.White).padding(6.dp)) {
          Box(Modifier.record(placed, "avatar")) {
            ProfileImage(27.dp, image, color = Color.Black, toEdge = toEdge)
          }
        }
      }
    } finally {
      appPrefs.profileImageCornerRadius.set(radius)
    }
    val bitmap = Bitmap.makeFromImage(rendered)
    val box = placed.getValue("avatar")
    val ink = (0 until 40).filter { x -> Color(bitmap.getColor(x, box.y + box.height / 2)).luminance() < 0.5f }
    return (ink.first() - box.x)..(ink.last() - box.x)
  }

  @Test
  fun avatarsDrawnToEdgeFillTheirBox() {
    assertEquals(0..26, inkAcrossMiddle(null, toEdge = true))
    assertEquals(0..26, inkAcrossMiddle(photo, toEdge = true))
  }

  @Test
  fun avatarsDrawnAsNowLeaveTwelfthOfBox() {
    for (image in listOf(null, photo)) {
      val ink = inkAcrossMiddle(image, toEdge = false)
      assertTrue(ink.first >= 2 && ink.last <= 24, "ink $ink")
    }
  }
}
