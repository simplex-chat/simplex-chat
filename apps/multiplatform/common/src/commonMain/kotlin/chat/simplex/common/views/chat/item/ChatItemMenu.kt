package chat.simplex.common.views.chat.item

import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.TransformOrigin
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.LayoutCoordinates
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.*
import androidx.compose.ui.window.Popup
import androidx.compose.ui.window.PopupPositionProvider
import androidx.compose.ui.window.PopupProperties
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.views.helpers.DefaultDropdownMenu
import kotlin.math.*

// Spec: spec/client/chat-view.md#message-menu-placement
@Stable
internal class ChatItemMenuAnchor {
  private var coordinates: LayoutCoordinates? = null
  private var tracking = false
  var bounds by mutableStateOf<IntRect?>(null)
    private set

  fun update(coordinates: LayoutCoordinates) {
    this.coordinates = coordinates
    if (tracking) capture()
  }

  fun track(enabled: Boolean) {
    tracking = enabled
    if (enabled) capture() else bounds = null
  }

  private fun capture() {
    bounds = coordinates?.takeIf { it.isAttached }?.chatAnimationBounds()?.roundToIntRect()
  }
}

internal data class ChatItemMenuPlacement(
  val anchor: IntRect,
  val viewport: IntRect,
  val width: Int,
  val maxHeight: Int,
  val gap: Int,
  val side: Int = 0,
) {
  fun position(size: IntSize, sent: Boolean): IntOffset {
    val x = when (side) {
      -1 -> anchor.left - gap - size.width
      1 -> anchor.right + gap
      else -> if (sent) anchor.right - size.width else anchor.left
    }.coerceIn(viewport.left, max(viewport.left, viewport.right - size.width))
    val below = anchor.bottom + gap
    val above = anchor.top - gap - size.height
    val maxY = max(viewport.top, viewport.bottom - size.height)
    val y = when {
      side != 0 -> anchor.top.coerceIn(viewport.top, maxY)
      below + size.height <= viewport.bottom -> max(viewport.top, below)
      above >= viewport.top -> min(maxY, above)
      else -> {
        // A source filling the viewport cannot have a disjoint menu. Keep all
        // actions reachable and cover as little of that source as possible.
        val candidates = listOf(below.coerceIn(viewport.top, maxY), above.coerceIn(viewport.top, maxY))
        candidates.minBy { max(0, min(it + size.height, anchor.bottom) - max(it, anchor.top)) }
      }
    }
    return IntOffset(x, y)
  }
}

internal fun chatItemMenuPlacement(
  anchor: IntRect,
  window: IntSize,
  preferredWidth: Int,
  margin: Int,
  gap: Int,
  topInset: Int,
  minHeight: Int,
): ChatItemMenuPlacement {
  val left = min(margin, window.width / 2)
  val top = min(topInset + margin, window.height)
  val viewport = IntRect(left, top, max(left, window.width - margin), max(top, window.height - margin))
  val width = preferredWidth.coerceIn(0, viewport.width)
  val above = (anchor.top - gap - viewport.top).coerceIn(0, viewport.height)
  val below = (viewport.bottom - anchor.bottom - gap).coerceIn(0, viewport.height)
  val vertical = max(above, below)
  val side = when {
    vertical >= min(minHeight, viewport.height) -> 0
    anchor.left - gap - viewport.left >= width -> -1
    viewport.right - anchor.right - gap >= width -> 1
    else -> 0
  }
  val height = if (side != 0 || vertical < min(minHeight, viewport.height)) viewport.height else vertical
  return ChatItemMenuPlacement(anchor, viewport, width, height, gap, side)
}

@Composable
internal fun ChatItemDropdownMenu(
  showMenu: MutableState<Boolean>,
  sent: Boolean,
  anchor: ChatItemMenuAnchor,
  content: @Composable () -> Unit,
) {
  if (!appPlatform.isAndroid) {
    DefaultDropdownMenu(showMenu, dropdownMenuItems = content)
    return
  }
  val expanded = remember { MutableTransitionState(false) }
  expanded.targetState = showMenu.value
  if (!expanded.currentState && !expanded.targetState) return
  DisposableEffect(anchor) {
    anchor.track(true)
    onDispose { anchor.track(false) }
  }
  val density = LocalDensity.current
  val margin = with(density) { 8.dp.roundToPx() }
  val gap = with(density) { 4.dp.roundToPx() }
  val width = with(density) { 250.dp.roundToPx() }
  val minHeight = with(density) { 96.dp.roundToPx() }
  val topInset = WindowInsets.statusBars.getTop(density)
  val positionProvider = remember(sent, margin, gap, width, minHeight, topInset) {
    object : PopupPositionProvider {
      override fun calculatePosition(anchorBounds: IntRect, windowSize: IntSize, layoutDirection: LayoutDirection, popupContentSize: IntSize): IntOffset =
        chatItemMenuPlacement(anchorBounds, windowSize, width, margin, gap, topInset, minHeight).position(popupContentSize, sent)
    }
  }
  val transition = updateTransition(expanded)
  val alpha by transition.animateFloat(transitionSpec = { tween(120) }) { if (it) 1f else 0f }
  Popup(
    popupPositionProvider = positionProvider,
    onDismissRequest = { showMenu.value = false },
    properties = PopupProperties(focusable = true),
  ) {
    Layout(content = {
      Surface(
        shape = RoundedCornerShape(25.dp),
        elevation = 8.dp,
        modifier = Modifier.graphicsLayer {
          this.alpha = alpha
          // Keep the horizontal edge aligned with the source during expansion.
          transformOrigin = TransformOrigin(if (sent) 1f else 0f, 0.5f)
          scaleX = 0.96f + 0.04f * alpha
          scaleY = scaleX
        },
      ) {
        Column(Modifier.verticalScroll(rememberScrollState()).padding(vertical = 12.dp)) {
          content()
        }
      }
    }) { measurables, constraints ->
      val placement = chatItemMenuPlacement(
        anchor.bounds ?: IntRect.Zero,
        IntSize(constraints.maxWidth, constraints.maxHeight),
        width, margin, gap, topInset, minHeight,
      )
      val menu = measurables.single().measure(Constraints(
        minWidth = placement.width,
        maxWidth = placement.width,
        maxHeight = placement.maxHeight,
      ))
      layout(menu.width, menu.height) { menu.place(0, 0) }
    }
  }
}