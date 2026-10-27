package chat.simplex.app

import androidx.compose.ui.unit.*
import chat.simplex.common.views.chat.item.chatItemMenuPlacement
import kotlin.test.*

class ChatItemMenuPlacementTest {
  private fun placement(anchor: IntRect, window: IntSize = IntSize(400, 800)) =
    chatItemMenuPlacement(anchor, window, preferredWidth = 250, margin = 8, gap = 4, topInset = 24, minHeight = 96)

  @Test
  fun shortMenusPreferBelowAndFollowTheBubbleEdge() {
    val received = placement(IntRect(30, 100, 250, 180))
    assertEquals(IntOffset(30, 184), received.position(IntSize(250, 300), sent = false))
    val sent = placement(IntRect(180, 100, 380, 180))
    assertEquals(IntOffset(130, 184), sent.position(IntSize(250, 300), sent = true))
  }

  @Test
  fun bottomMessagesOpenAboveInsteadOfClampingOverTheSource() {
    val menu = placement(IntRect(30, 660, 280, 730))
    val position = menu.position(IntSize(250, 300), sent = false)
    assertEquals(IntOffset(30, 356), position)
    assertTrue(position.y + 300 + menu.gap <= menu.anchor.top)
  }

  @Test
  fun longMenuScrollsInTheLargerFreeRegion() {
    val menu = placement(IntRect(30, 350, 280, 430))
    assertEquals(358, menu.maxHeight)
    val position = menu.position(IntSize(menu.width, menu.maxHeight), sent = false)
    assertEquals(434, position.y)
    assertEquals(menu.viewport.bottom, position.y + menu.maxHeight)
  }

  @Test
  fun keyboardSizedWindowRecomputesAvailableHeightAndFlipsAbove() {
    val anchor = IntRect(30, 260, 280, 320)
    val full = placement(anchor)
    assertEquals(324, full.position(IntSize(250, 200), false).y)
    val resized = placement(anchor, IntSize(400, 420))
    assertEquals(224, resized.maxHeight)
    assertEquals(32, resized.position(IntSize(250, 224), false).y)
  }

  @Test
  fun narrowWindowCapsWidthBeforeApplyingBubbleAlignment() {
    val menu = placement(IntRect(100, 100, 170, 150), IntSize(180, 360))
    assertEquals(164, menu.width)
    assertEquals(8, menu.position(IntSize(menu.width, 100), true).x)
    assertEquals(8, menu.position(IntSize(menu.width, 100), false).x)
  }

  @Test
  fun tallNarrowBubbleLeavesTheMenuAlongsideIt() {
    val menu = placement(IntRect(8, 35, 110, 780))
    assertEquals(1, menu.side)
    assertEquals(IntOffset(114, 35), menu.position(IntSize(250, 300), false))
    val sent = placement(IntRect(290, 35, 390, 780))
    assertEquals(-1, sent.side)
    assertEquals(36, sent.position(IntSize(250, 300), true).x)
  }

  @Test
  fun viewportFillingBubbleHasAnExplicitAccessibleFallback() {
    val menu = placement(IntRect(8, 35, 390, 780))
    assertEquals(menu.viewport.height, menu.maxHeight)
    val size = IntSize(menu.width, 300)
    val offset = menu.position(size, false)
    assertEquals(492, offset.y)
    assertTrue(offset.x >= menu.viewport.left && offset.x + size.width <= menu.viewport.right)
    assertTrue(offset.y >= menu.viewport.top && offset.y + size.height <= menu.viewport.bottom)
  }

  @Test
  fun ordinaryPlacementsNeverOverlapTheSourceAcrossWindowSizes() {
    for (window in listOf(IntSize(400, 800), IntSize(800, 400), IntSize(260, 420))) {
      for (top in 50 until window.height - 60 step 25) {
        val menu = placement(IntRect(30, top, window.width - 30, top + 50), window)
        val size = IntSize(menu.width, minOf(500, menu.maxHeight))
        val offset = menu.position(size, true)
        assertTrue(offset.y + size.height <= menu.anchor.top || offset.y >= menu.anchor.bottom)
        assertTrue(offset.y >= menu.viewport.top && offset.y + size.height <= menu.viewport.bottom)
        assertTrue(offset.x >= menu.viewport.left && offset.x + size.width <= menu.viewport.right)
      }
    }
  }
}