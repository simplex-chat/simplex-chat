package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.item.*
import kotlin.math.abs
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TerminalChatItemRowTest {
  private val density = Density(1f)
  private val cardGap = with(density) { TERMINAL_PARAGRAPH_GAP.roundToPx() }
  private val avatarGap = with(density) { TERMINAL_HEADING_BOTTOM_GAP.roundToPx() }
  private val alice = ChatItemSender("alice", null, null, own = false, image = null)
  private val placed = mutableMapOf<String, Placed>()
  private val aliceWithAvatar = ChatItemSender("alice", null, null, own = false) { Box(Modifier.size(27.dp).record(placed, "avatar")) }

  private fun messagePlaced(sender: ChatItemSender?, gutter: Dp = 0.dp, topGap: Dp = 0.dp, fontScale: Float = 1f, message: @Composable (Modifier) -> Unit): Placed {
    renderScene {
      CompositionLocalProvider(LocalTerminalLayout provides true, LocalDensity provides Density(1f, fontScale)) {
        Box(Modifier.width(300.dp).record(placed, "row")) {
          TerminalChatItemRow(sender, gutter, topGap) { message(Modifier.record(placed, "message")) }
        }
      }
    }
    return placed.getValue("message").relativeTo(placed.getValue("row"))
  }

  private fun messageTop(sender: ChatItemSender?, message: @Composable (Modifier) -> Unit): Int = messagePlaced(sender, message = message).y

  private fun avatar(): Placed = placed.getValue("avatar").relativeTo(placed.getValue("row"))

  @Test
  fun cardRightBelowNameTakesParagraphGap() {
    val textTop = messageTop(alice) { Box(it.fixed(100, 22, 17)) }
    assertEquals(textTop + cardGap, messageTop(alice) { Box(it.messageCard().fixed(100, 40, null)) })
  }

  @Test
  fun cardBelowTextTakesNoGapUnderName() {
    val textTop = messageTop(alice) { Box(it.fixed(100, 22, 17)) }
    val cardSecond = messageTop(alice) {
      Column(it) {
        Box(Modifier.fixed(100, 22, 17))
        Box(Modifier.messageCard().fixed(100, 40, null))
      }
    }
    assertEquals(textTop, cardSecond)
  }

  @Test
  fun cardWithoutNameTakesNoGap() {
    assertEquals(0, messageTop(null) { Box(it.messageCard().fixed(100, 40, null)) })
  }

  @Test
  fun messagesStartAtRowStartBelowNameLine() {
    val nameLine = messageTop(alice) { Box(it.fixed(100, 22, 17)) }
    val message = messagePlaced(aliceWithAvatar, gutter = 39.dp) { Box(it.fixed(100, 22, 17)) }
    val avatar = avatar()
    assertEquals(0, message.x)
    assertEquals(avatar.y - (nameLine - avatar.height) / 2 + nameLine, message.y)
  }

  @Test
  fun avatarCentredOnNameLine() {
    val nameLine = messageTop(alice) { Box(it.fixed(100, 22, 17)) }
    val message = messagePlaced(aliceWithAvatar, gutter = 39.dp, topGap = 20.dp) { Box(it.fixed(100, 22, 17)) }
    val avatar = avatar()
    assertTrue(abs(2 * avatar.y + avatar.height - (2 * message.y - nameLine)) <= 1, "avatar at ${avatar.y}, name line of $nameLine above ${message.y}")
  }

  @Test
  fun avatarTopIsHalfStepBelowGapAboveRow() {
    messagePlaced(aliceWithAvatar, gutter = 39.dp, topGap = 20.dp) { Box(it.fixed(100, 22, 17)) }
    assertEquals(with(density) { (20.dp + TERMINAL_HEADING_BOTTOM_GAP.toDp()).roundToPx() }, avatar().y)
    messagePlaced(aliceWithAvatar, gutter = 39.dp, topGap = 0.dp) { Box(it.fixed(100, 22, 17)) }
    assertEquals(avatarGap, avatar().y)
  }

  @Test
  fun cardBelowAvatarTakesParagraphGapBelowNameLine() {
    val text = messageTop(aliceWithAvatar) { Box(it.fixed(100, 22, 17)) }
    val card = messageTop(aliceWithAvatar) { Box(it.messageCard().fixed(100, 40, null)) }
    assertEquals(text + cardGap, card)
  }

  @Test
  fun textBelowNameLineTallerThanAvatarStartsAtItsBottom() {
    val nameLine = messagePlaced(alice, fontScale = 2f) { Box(it.fixed(100, 22, 17)) }.y
    val message = messagePlaced(aliceWithAvatar, gutter = 39.dp, fontScale = 2f) { Box(it.fixed(100, 22, 17)) }
    assertTrue(nameLine > 27, "name line $nameLine")
    assertEquals(with(Density(1f, 2f)) { TERMINAL_HEADING_BOTTOM_GAP.roundToPx() } + nameLine, message.y)
  }

  @Test
  fun textWithoutAvatarTakesNoGap() {
    assertEquals(22, messageTop(alice) { Box(it.fixed(100, 22, 17)) })
  }
}
