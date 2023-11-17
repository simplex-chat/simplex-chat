// Fork of https://github.com/zj565061763/compose-wheel-picker to make multi-platform

package com.sd.lib.compose.wheel_picker

import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.runBlocking

@Composable
actual fun WheelPicker(
  isVertical: Boolean,
  count: Int,
  state: FWheelPickerState,
  modifier: Modifier,
  key: ((index: Int) -> Any)?,
  itemSize: Dp,
  unfocusedCount: Int,
  userScrollEnabled: Boolean,
  reverseLayout: Boolean,
  debug: Boolean,
  focus: @Composable () -> Unit,
  contentWrapper: @Composable FWheelPickerContentWrapperScope.(index: Int) -> Unit,
  content: @Composable FWheelPickerContentScope.(index: Int) -> Unit,
) {
  require(count >= 0) { "require count >= 0" }
  require(unfocusedCount >= 1) { "require unfocusedCount >= 1" }

  state.debug = debug
  LaunchedEffect(state, count) {
    state.notifyCountChanged(count)
  }

  val nestedScrollConnection = remember(state) {
    WheelPickerNestedScrollConnection(state)
  }.apply {
    this.isVertical = isVertical
    this.itemSizePx = with(LocalDensity.current) { itemSize.roundToPx() }
    this.reverseLayout = reverseLayout
  }

  val totalSize = remember(itemSize, unfocusedCount) {
    itemSize * (unfocusedCount * 2 + 1)
  }

  val contentWrapperScope = remember(state) {
    val contentScope = WheelPickerContentScopeImpl(state)
    FWheelPickerContentWrapperScopeImpl(contentScope)
  }.apply {
    this.content = content
  }

  Box(
    modifier = modifier
      .pointerInput(Unit) {
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

                state.setCurrentIndexInternal(i)
                runBlocking {
                  state.scrollToIndex(i)
                }
              }
            }
          } while (event.changes.any { it.pressed })
        }
      }
      .run {
        if (totalSize > 0.dp) {
          if (isVertical) {
            height(totalSize).widthIn(40.dp)
          } else {
            width(totalSize).heightIn(40.dp)
          }
        } else {
          this
        }
      },
    contentAlignment = Alignment.Center,
  ) {

    val lazyListScope: LazyListScope.() -> Unit = {
      repeat(unfocusedCount) {
        item {
          ItemSizeBox(
            isVertical = isVertical,
            itemSize = itemSize,
          )
        }
      }

      items(
        count = count,
        key = key,
      ) { index ->
        ItemSizeBox(
          isVertical = isVertical,
          itemSize = itemSize,
        ) {
          contentWrapperScope.contentWrapper(index)
        }
      }

      repeat(unfocusedCount) {
        item {
          ItemSizeBox(
            isVertical = isVertical,
            itemSize = itemSize,
          )
        }
      }
    }

    if (isVertical) {
      LazyColumn(
        state = state.lazyListState,
        horizontalAlignment = Alignment.CenterHorizontally,
        reverseLayout = reverseLayout,
        userScrollEnabled = userScrollEnabled,
        modifier = Modifier.matchParentSize(),
        content = lazyListScope,
      )
    } else {
      LazyRow(
        state = state.lazyListState,
        verticalAlignment = Alignment.CenterVertically,
        reverseLayout = reverseLayout,
        userScrollEnabled = userScrollEnabled,
        modifier = Modifier.matchParentSize(),
        content = lazyListScope,
      )
    }

    ItemSizeBox(
      modifier = Modifier.align(Alignment.Center),
      isVertical = isVertical,
      itemSize = itemSize,
    ) {
      focus()
    }
  }
}