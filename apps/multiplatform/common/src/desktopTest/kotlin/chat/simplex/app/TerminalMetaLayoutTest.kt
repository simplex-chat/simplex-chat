package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.item.*
import kotlin.test.Test
import kotlin.test.assertEquals

class TerminalMetaLayoutTest {
  private val density = Density(1f)
  private val rowWidth = 300
  private val metaWidth = 60
  private val metaHeight = 16
  private val metaBaseline = 12
  private val metaBoxWidth = metaWidth + 12
  private val gap = with(density) { TERMINAL_META_GAP.roundToPx() }
  private val blockGap = with(density) { TERMINAL_BLOCK_BOTTOM_GAP.roundToPx() }
  private val placed = mutableMapOf<String, Placed>()

  private class Result(val row: Placed, val content: Placed, val meta: Placed?, val overlay: Placed?, val endButton: Placed?, val drawn: Set<String>)

  private fun layOut(
    direction: LayoutDirection = LayoutDirection.Ltr,
    metaReserved: Boolean = true,
    overlay: Boolean = false,
    endButton: Boolean = false,
    content: @Composable (Modifier) -> Unit
  ): Result {
    val drawn = mutableSetOf<String>()
    renderScene(direction) {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        Box(Modifier.width(rowWidth.dp).record(placed, "row")) {
          ItemWithMeta(
            meta = { Box(Modifier.record(placed, "meta").drawBehind { drawn += "meta" }.fixed(metaWidth, metaHeight, metaBaseline)) },
            overlayMeta = if (overlay) { { Box(Modifier.record(placed, "overlay").drawBehind { drawn += "overlay" }.fixed(metaWidth, metaHeight, metaBaseline)) } } else null,
            metaReserved = metaReserved,
            endButton = if (endButton) { { Box(Modifier.record(placed, "endButton").fixed(22, 22, null)) } } else null
          ) { content(Modifier.record(placed, "content")) }
        }
      }
    }
    val row = placed.getValue("row")
    return Result(Placed(0, 0, row.width, row.height), placed.getValue("content").relativeTo(row), placed["meta"]?.relativeTo(row), placed["overlay"]?.relativeTo(row), placed["endButton"]?.relativeTo(row), drawn)
  }

  @Test
  fun metaBaselineOnContentLastBaseline() {
    val r = layOut(metaReserved = false) { Box(it.fixed(100, 40, 30)) }
    assertEquals(Placed(0, 0, 100, 40), r.content)
    assertEquals(Placed(rowWidth - metaBoxWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40, r.row.height)
  }

  @Test
  fun contentNarrowedByMetaWithoutReserve() {
    val r = layOut(metaReserved = false) { Box(it.fillMaxWidth().height(40.dp)) }
    assertEquals(Placed(0, 0, rowWidth - metaBoxWidth, 40), r.content)
  }

  @Test
  fun metaBesideLastLineWithRoom() {
    val r = layOut { Box(it.messageLastLine().fixed(100, 40, 30)) }
    assertEquals(Placed(rowWidth - metaBoxWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40, r.row.height)
  }

  @Test
  fun metaUnderLastLineWithoutRoom() {
    val r = layOut { Box(it.messageLastLine().fixed(260, 40, 30)) }
    assertEquals(Placed(rowWidth - metaBoxWidth, 40 + gap, metaWidth, metaHeight), r.meta)
    assertEquals(40 + gap + metaHeight, r.row.height)
  }

  @Test
  fun blockGapBelowLastBlockWithMetaBeside() {
    val r = layOut { Box(it.messageLastBlock().fixed(100, 40, 30)) }
    assertEquals(Placed(rowWidth - metaBoxWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40 + blockGap, r.row.height)
  }

  @Test
  fun metaAboveBottomOfContentWithoutBaseline() {
    val r = layOut(metaReserved = false) { Box(it.fixed(100, 50, null)) }
    assertEquals(Placed(rowWidth - metaBoxWidth, 50 - gap - metaHeight, metaWidth, metaHeight), r.meta)
    assertEquals(50, r.row.height)
  }

  @Test
  fun rowAtLeastAsTallAsMeta() {
    val r = layOut { Box(it.fixed(100, 10, 8)) }
    assertEquals(Placed(rowWidth - metaBoxWidth, 0, metaWidth, metaHeight), r.meta)
    assertEquals(metaHeight, r.row.height)
  }

  @Test
  fun overlayMetaAtContentEndWhenRowIsFull() {
    val r = layOut(metaReserved = false, overlay = true) { Box(it.fixed(260, 100, null)) }
    assertEquals(Placed(260 - metaBoxWidth, 100 - gap - metaHeight, metaWidth, metaHeight), r.overlay)
    assertEquals(setOf("overlay"), r.drawn)
    assertEquals(100, r.row.height)
  }

  @Test
  fun overlayMetaAtTopOfShortContent() {
    val r = layOut(metaReserved = false, overlay = true) { Box(it.fixed(260, 10, null)) }
    assertEquals(Placed(260 - metaBoxWidth, 0, metaWidth, metaHeight), r.overlay)
    assertEquals(setOf("overlay"), r.drawn)
    assertEquals(metaHeight, r.row.height)
  }

  @Test
  fun overlayMetaOnMediaUnderWiderQuote() {
    val r = layOut(metaReserved = false, overlay = true) {
      Column(it.width(IntrinsicSize.Max)) {
        PriorityLayout(Modifier, CHAT_IMAGE_LAYOUT_ID, limitToPriorityWidth = true) {
          Box(Modifier.width(290.dp).height(30.dp))
          Box(Modifier.layoutId(CHAT_IMAGE_LAYOUT_ID).record(placed, "media").size(260.dp, 100.dp))
        }
      }
    }
    val media = placed.getValue("media").relativeTo(placed.getValue("row"))
    assertEquals(Placed(media.x + media.width - metaBoxWidth, media.y + media.height - gap - metaHeight, metaWidth, metaHeight), r.overlay)
    assertEquals(setOf("overlay"), r.drawn)
  }

  @Test
  fun endButtonCentredAboveTimeLine() {
    val r = layOut(endButton = true) {
      Column(it.fillMaxWidth()) {
        Box(Modifier.fixed(80, 24, 18))
        Box(Modifier.fixed(100, 40, 30))
      }
    }
    assertEquals(Placed(0, 0, rowWidth - 12 - 22, 64), r.content)
    assertEquals(Placed(rowWidth - metaBoxWidth, 54 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(Placed(rowWidth - 12 - 22, (54 - metaBaseline - 22) / 2, 22, 22), r.endButton)
    assertEquals(64, r.row.height)
  }

  @Test
  fun metaBesideLastLineRightToLeft() {
    val r = layOut(direction = LayoutDirection.Rtl) { Box(it.messageLastLine().fixed(100, 40, 30)) }
    assertEquals(Placed(rowWidth - 100, 0, 100, 40), r.content)
    assertEquals(Placed(metaBoxWidth - metaWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40, r.row.height)
  }
}
