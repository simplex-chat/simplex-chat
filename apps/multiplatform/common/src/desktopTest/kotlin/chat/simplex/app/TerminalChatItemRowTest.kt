package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.item.*
import kotlin.test.Test
import kotlin.test.assertEquals

class TerminalChatItemRowTest {
  private val density = Density(1f)
  private val cardGap = with(density) { TERMINAL_PARAGRAPH_GAP.roundToPx() }
  private val alice = ChatItemSender("alice", null, null, own = false, image = null)
  private val placed = mutableMapOf<String, Placed>()

  private fun messageTop(sender: ChatItemSender?, message: @Composable (Modifier) -> Unit): Int {
    renderScene {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        Box(Modifier.width(300.dp).record(placed, "row")) {
          TerminalChatItemRow(sender, gutter = 0.dp, topGap = 0.dp) { message(Modifier.record(placed, "message")) }
        }
      }
    }
    return placed.getValue("message").relativeTo(placed.getValue("row")).y
  }

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
}
