package chat.simplex.app.ui.harness

import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.semantics.SemanticsNode

fun tapPointInNode(node: SemanticsNode): Offset {
  val bounds = node.boundsInRoot
  val minTarget = node.layoutInfo.density.density * MIN_TOUCH_TARGET_DP
  val nested = clickableDescendants(node).map { touchTarget(it.boundsInRoot, minTarget) }
  val c = bounds.center
  if (nested.none { it.contains(c) }) return c - bounds.topLeft
  val x = widestFreeMidpoint(bounds.left, bounds.right, nested.filter { c.y >= it.top && c.y < it.bottom }.map { it.left to it.right })
  if (x != null) return Offset(x, c.y) - bounds.topLeft
  val y = widestFreeMidpoint(bounds.top, bounds.bottom, nested.filter { c.x >= it.left && c.x < it.right }.map { it.top to it.bottom })
  return Offset(c.x, y ?: c.y) - bounds.topLeft
}

private fun clickableDescendants(node: SemanticsNode): List<SemanticsNode> =
  node.children.flatMap { if (it.config.contains(SemanticsActions.OnClick)) listOf(it) else clickableDescendants(it) }

private fun widestFreeMidpoint(start: Float, end: Float, covered: List<Pair<Float, Float>>): Float? {
  var free = listOf(start to end)
  for ((a, b) in covered) {
    free = free.flatMap { (s, e) -> listOf(s to minOf(e, a), maxOf(s, b) to e).filter { it.second > it.first } }
  }
  return free.maxByOrNull { it.second - it.first }?.let { (it.first + it.second) / 2 }
}

private fun touchTarget(r: Rect, min: Float): Rect {
  val dx = ((min - r.width) / 2).coerceAtLeast(0f)
  val dy = ((min - r.height) / 2).coerceAtLeast(0f)
  return Rect(r.left - dx, r.top - dy, r.right + dx, r.bottom + dy)
}

private const val MIN_TOUCH_TARGET_DP = 48f
