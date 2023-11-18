package com.sd.lib.compose.wheel_picker

import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.unit.Dp
import kotlinx.coroutines.runBlocking

actual fun Modifier.wheelPickerScroll(
  state: FWheelPickerState,
  count: Int,
  itemSize: Dp,
  isVertical: Boolean,
  reverseLayout: Boolean): Modifier {
  return pointerInput(Unit) {
    awaitEachGesture {
      do {
        val event = awaitPointerEvent()

        if (event.type == PointerEventType.Scroll) {
          // set index based on scroll direction
          val c = event.changes.firstOrNull()
          if (c != null) {
            val dir = c.scrollDelta.y
            var i = state.currentIndex

            if (dir == -1.0f) {
              // scroll wheel moved up
              i = (state.currentIndex-1).coerceAtLeast(0)
            } else if (dir == 1.0f) {
              // scroll wheel moved down
              i = (state.currentIndex+1).coerceAtMost(count-1)
            }

            runBlocking {
              state.scrollToIndex(i)
            }
          }
        }
      } while (event.changes.any { it.pressed })
    }
  }
}
