package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.item.CHAT_IMAGE_LAYOUT_ID
import chat.simplex.common.views.chat.item.PriorityLayout
import kotlin.test.Test
import kotlin.test.assertEquals

class PriorityLayoutTest {
  private val placed = mutableMapOf<String, Placed>()

  private fun layOut(limitToPriorityWidth: Boolean, direction: LayoutDirection = LayoutDirection.Ltr): Map<String, Placed> {
    renderScene(direction) {
      Box(Modifier.width(300.dp)) {
        PriorityLayout(Modifier.record(placed, "layout"), CHAT_IMAGE_LAYOUT_ID, limitToPriorityWidth) {
          Box(Modifier.width(280.dp).height(30.dp).record(placed, "header"))
          Box(Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID).record(placed, "media").size(200.dp, 100.dp))
          Box(Modifier.fillMaxWidth().height(40.dp).record(placed, "caption"))
        }
      }
    }
    val layout = placed.getValue("layout")
    return listOf("layout", "header", "media", "caption").associateWith { placed.getValue(it).relativeTo(layout) }
  }

  @Test
  fun terminalElementsAtTheirOwnWidth() {
    val r = layOut(limitToPriorityWidth = false)
    assertEquals(Placed(0, 0, 300, 170), r["layout"])
    assertEquals(Placed(0, 0, 280, 30), r["header"])
    assertEquals(Placed(0, 30, 200, 100), r["media"])
    assertEquals(Placed(0, 130, 300, 40), r["caption"])
  }

  @Test
  fun classicElementsAtMediaWidth() {
    val r = layOut(limitToPriorityWidth = true)
    assertEquals(Placed(0, 0, 200, 170), r["layout"])
    assertEquals(Placed(0, 0, 200, 30), r["header"])
    assertEquals(Placed(0, 30, 200, 100), r["media"])
    assertEquals(Placed(0, 130, 200, 40), r["caption"])
  }

  @Test
  fun terminalElementsAtStartEdgeRightToLeft() {
    val r = layOut(limitToPriorityWidth = false, direction = LayoutDirection.Rtl)
    assertEquals(Placed(20, 0, 280, 30), r["header"])
    assertEquals(Placed(100, 30, 200, 100), r["media"])
    assertEquals(Placed(0, 130, 300, 40), r["caption"])
  }

  @Test
  fun classicPlacementUnmirroredRightToLeft() {
    val r = layOut(limitToPriorityWidth = true, direction = LayoutDirection.Rtl)
    assertEquals(Placed(0, 0, 200, 30), r["header"])
    assertEquals(Placed(0, 30, 200, 100), r["media"])
  }
}
