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
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.views.helpers.ChatInfoImage
import chat.simplex.common.views.helpers.ProfileImage
import org.jetbrains.skia.Bitmap
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Base64
import javax.imageio.ImageIO
import kotlin.test.Test
import kotlin.test.assertEquals

class FlatAvatarTest {
  private val placed = mutableMapOf<String, Placed>()
  private val photo = "data:image/png;base64," + Base64.getEncoder().encodeToString(
    ByteArrayOutputStream().also { ImageIO.write(BufferedImage(8, 8, BufferedImage.TYPE_INT_RGB), "png", it) }.toByteArray()
  )

  private fun inkAcrossMiddle(avatar: @androidx.compose.runtime.Composable () -> Unit): IntRange {
    val radius = appPrefs.profileImageCornerRadius.get()
    val rendered = try {
      appPrefs.profileImageCornerRadius.set(50f)
      renderScene(width = 40, height = 40) {
        Box(Modifier.size(40.dp).background(Color.White).padding(6.dp)) {
          Box(Modifier.record(placed, "avatar")) { avatar() }
        }
      }
    } finally {
      appPrefs.profileImageCornerRadius.set(radius)
    }
    val bitmap = Bitmap.makeFromImage(rendered)
    val box = placed.getValue("avatar")
    val ink = (0 until 40).filter { x -> Color(bitmap.getColor(x, box.y + box.height / 2)).luminance() < 0.97f }
    return (ink.first() - box.x)..(ink.last() - box.x)
  }

  @Test
  fun avatarsFillTheirBox() {
    val channel = ChatInfo.Group(GroupInfo.sampleData.copy(useRelays = true), null)
    for (chatInfo in listOf(ChatInfo.Direct.sampleData, ChatInfo.Group.sampleData, ChatInfo.Local.sampleData, channel)) {
      assertEquals(0..26, inkAcrossMiddle { ChatInfoImage(chatInfo, 27.dp, Color.Black) }, chatInfo.chatType.name)
    }
    assertEquals(0..26, inkAcrossMiddle { ProfileImage(27.dp, photo) })
  }
}
