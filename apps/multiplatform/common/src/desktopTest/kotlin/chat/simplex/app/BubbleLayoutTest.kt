package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.size
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.views.chat.item.*
import kotlin.math.roundToInt
import kotlin.test.Test
import kotlin.test.assertEquals

class BubbleLayoutTest {
  private val density = Density(1f)
  private val inset = with(density) { BUBBLE_TEXT_INSET.roundToPx() }
  private val placed = mutableMapOf<String, Placed>()

  private fun metaInBubble(metaHeight: Int, metaBaseline: Int?): Placed {
    renderScene {
      Box(Modifier.size(200.dp, 100.dp).record(placed, "bubble"), contentAlignment = Alignment.BottomEnd) {
        Box(Modifier.bubbleMeta()) {
          Box(Modifier.record(placed, "meta").fixed(60, metaHeight, metaBaseline))
        }
      }
    }
    return placed.getValue("meta").relativeTo(placed.getValue("bubble"))
  }

  @Test
  fun timeBaselineInsetAboveBubbleBottom() {
    val meta = metaInBubble(17, 13)
    assertEquals(100 - inset, meta.y + 13)
  }

  @Test
  fun ticksWithoutTimeWhereTheyStandNextToTime() {
    val meta = metaInBubble(17, null)
    val timeBaseline = ((17 + with(density) { 12.sp.toPx() } * INTER_CAP_HEIGHT) / 2).roundToInt()
    assertEquals(100 - inset, meta.y + timeBaseline)
  }

  @Test
  fun textCapsAndLastBaselineInsetFromEdges() {
    renderScene {
      Box(Modifier.record(placed, "part").bubbleTextPadding(16.sp)) {
        Column(Modifier.record(placed, "text")) {
          Box(Modifier.fixed(100, 22, 18))
          Box(Modifier.fixed(100, 22, 18))
        }
      }
    }
    val part = placed.getValue("part")
    val text = placed.getValue("text").relativeTo(part)
    val firstBaseline = with(density) { (BUBBLE_TEXT_INSET.value + 16 * INTER_CAP_HEIGHT).sp.roundToPx() }
    assertEquals(Placed(12, firstBaseline - 18, 100, 44), text)
    assertEquals(inset, part.height - (text.y + 22 + 18))
    assertEquals(124, part.width)
  }

  @Test
  fun noBottomInsetBeforeAnotherPartOfTheBand() {
    renderScene {
      Box(Modifier.record(placed, "part").bubbleTextPadding(12.sp, bottom = 0.sp)) {
        Box(Modifier.record(placed, "text").fixed(100, 15, 12))
      }
    }
    val part = placed.getValue("part")
    val text = placed.getValue("text").relativeTo(part)
    assertEquals(text.y + 15, part.height)
  }

  @Test
  fun timeOnTextBaselineInCardRow() {
    renderScene {
      Row(Modifier.record(placed, "row")) {
        Box(Modifier.alignByBaseline().record(placed, "text").fixed(100, 22, 17))
        Box(bubbleMetaOnBaseline()) {
          Box(Modifier.record(placed, "meta").fixed(60, 16, 12))
        }
      }
    }
    val row = placed.getValue("row")
    assertEquals(placed.getValue("text").relativeTo(row).y + 17, placed.getValue("meta").relativeTo(row).y + 12)
  }
}
