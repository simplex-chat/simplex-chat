package com.sd.lib.compose.wheel_picker

import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp

actual fun Modifier.wheelPickerScroll(
  state: FWheelPickerState,
  count: Int,
  itemSize: Dp,
  isVertical: Boolean,
  reverseLayout: Boolean): Modifier {
  val nestedScrollConnection = remember(state) {
    WheelPickerNestedScrollConnection(state)
  }.apply {
    this.isVertical = isVertical
    this.itemSizePx = with(LocalDensity.current) { itemSize.roundToPx() }
    this.reverseLayout = reverseLayout
  }

  return nestedScroll(nestedScrollConnection)
}
