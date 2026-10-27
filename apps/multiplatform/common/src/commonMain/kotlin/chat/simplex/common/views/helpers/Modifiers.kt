package chat.simplex.common.views.helpers

import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsDraggedAsState
import androidx.compose.foundation.layout.offset
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.layout
import androidx.compose.ui.platform.LocalLayoutDirection
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.LayoutDirection
import kotlinx.coroutines.flow.collect
import kotlin.math.abs
import kotlin.math.roundToInt

fun Modifier.badgeLayout() =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)

    // based on the expectation of only one line of text
    val minPadding = placeable.height / 4

    val width = maxOf(placeable.width + minPadding, placeable.height)
    layout(width, placeable.height) {
      placeable.place((width - placeable.width) / 2, 0)
    }
  }

@Composable
fun SwipeToDismissModifier(
  state: DismissState,
  directions: Set<DismissDirection> = setOf(DismissDirection.EndToStart, DismissDirection.StartToEnd),
  swipeDistance: Float,
  onThresholdReached: (() -> Unit)? = null,
): Modifier {
  val interactionSource = remember { MutableInteractionSource() }
  val dragging by interactionSource.collectIsDraggedAsState()
  val thresholdCallback by rememberUpdatedState(onThresholdReached)
  LaunchedEffect(state, swipeDistance) {
    var notified = false
    snapshotFlow { dragging to state.offset.value }.collect { (active, offset) ->
      if (!active) {
        notified = false
      } else if (!notified && abs(offset) >= swipeDistance * 0.99f) {
        notified = true
        thresholdCallback?.invoke()
      }
    }
  }
  val isRtl = LocalLayoutDirection.current == LayoutDirection.Rtl
  val anchors = mutableMapOf(0f to DismissValue.Default)
  if (DismissDirection.StartToEnd in directions) anchors += swipeDistance to DismissValue.DismissedToEnd
  if (DismissDirection.EndToStart in directions) anchors += -swipeDistance to DismissValue.DismissedToStart
  return Modifier.swipeable(
    state = state,
    anchors = anchors,
    thresholds = { _, _ -> FractionalThreshold(0.99f) },
    orientation = Orientation.Horizontal,
    reverseDirection = isRtl,
    interactionSource = interactionSource,
  ).offset { IntOffset(state.offset.value.roundToInt(), 0) }
}
