package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.layout
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInRoot
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.item.*
import kotlin.math.roundToInt
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

  private data class Placed(val x: Int, val y: Int, val width: Int, val height: Int)
  private class Result(val row: Placed, val content: Placed, val meta: Placed?, val overlay: Placed?, val drawn: Set<String>)

  private fun Modifier.fixed(width: Int, height: Int, baseline: Int?): Modifier = layout { measurable, _ ->
    val placeable = measurable.measure(Constraints.fixed(width, height))
    layout(width, height, if (baseline == null) emptyMap() else mapOf(FirstBaseline to baseline, LastBaseline to baseline)) { placeable.place(0, 0) }
  }

  private fun Modifier.record(into: MutableMap<String, Placed>, name: String): Modifier = onGloballyPositioned {
    val p = it.positionInRoot()
    into[name] = Placed(p.x.roundToInt(), p.y.roundToInt(), it.size.width, it.size.height)
  }

  private fun layOut(
    direction: LayoutDirection = LayoutDirection.Ltr,
    metaReserved: Boolean = true,
    overlay: Boolean = false,
    content: @Composable (Modifier) -> Unit
  ): Result {
    val placed = mutableMapOf<String, Placed>()
    val drawn = mutableSetOf<String>()
    val scene = ImageComposeScene(width = 400, height = 400, density = density, layoutDirection = direction) {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        Box(Modifier.width(rowWidth.dp).record(placed, "row")) {
          ItemWithMeta(
            meta = { Box(Modifier.record(placed, "meta").drawBehind { drawn += "meta" }.fixed(metaWidth, metaHeight, metaBaseline)) },
            overlayMeta = if (overlay) { { Box(Modifier.record(placed, "overlay").drawBehind { drawn += "overlay" }.fixed(metaWidth, metaHeight, metaBaseline)) } } else null,
            metaReserved = metaReserved
          ) { content(Modifier.record(placed, "content")) }
        }
      }
    }
    scene.render()
    scene.close()
    val row = placed.getValue("row")
    fun relative(p: Placed?) = p?.let { Placed(it.x - row.x, it.y - row.y, it.width, it.height) }
    return Result(Placed(0, 0, row.width, row.height), relative(placed["content"])!!, relative(placed["meta"]), relative(placed["overlay"]), drawn)
  }

  @Test
  fun metaBaselineOnContentLastBaseline() {
    val r = layOut(metaReserved = false) { Box(it.fixed(100, 40, 30)) }
    assertEquals(Placed(0, 0, 100, 40), r.content)
    assertEquals(Placed(rowWidth - metaBoxWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40, r.row.height)
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
  fun metaBesideLastLineRightToLeft() {
    val r = layOut(direction = LayoutDirection.Rtl) { Box(it.messageLastLine().fixed(100, 40, 30)) }
    assertEquals(Placed(rowWidth - 100, 0, 100, 40), r.content)
    assertEquals(Placed(metaBoxWidth - metaWidth, 30 - metaBaseline, metaWidth, metaHeight), r.meta)
    assertEquals(40, r.row.height)
  }
}
