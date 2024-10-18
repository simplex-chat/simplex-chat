package chat.simplex.common.platform

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.FlingBehavior
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.unit.Dp
import chat.simplex.common.views.chatlist.NavigationBarBackground
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.filter
import kotlin.math.absoluteValue

@Composable
actual fun LazyColumnWithScrollBar(
  modifier: Modifier,
  state: LazyListState?,
  contentPadding: PaddingValues,
  reverseLayout: Boolean,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  flingBehavior: FlingBehavior,
  userScrollEnabled: Boolean,
  additionalBarHeight: State<Dp>?,
  content: LazyListScope.() -> Unit
) {
  val state = state ?: LocalAppBarHandler.current?.listState ?: rememberLazyListState()
  val connection = LocalAppBarHandler.current?.connection
  LaunchedEffect(Unit) {
    if (reverseLayout) {
      snapshotFlow { state.layoutInfo.visibleItemsInfo.lastOrNull()?.offset ?: 0 }
        .collect { scrollPosition ->
          val offset = connection?.appBarOffset
          if (offset != null) {
            connection.appBarOffset = if (state.layoutInfo.visibleItemsInfo.lastOrNull()?.index == state.layoutInfo.totalItemsCount - 1) {
              state.layoutInfo.viewportEndOffset - scrollPosition.toFloat() - state.layoutInfo.afterContentPadding
            } else {
              // show always when last item is not visible
              -1000f
            }
            //Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
          }
        }
    } else {
      snapshotFlow { state.firstVisibleItemScrollOffset }
        .filter { state.firstVisibleItemIndex == 0 }
        .collect { scrollPosition ->
          val offset = connection?.appBarOffset
          if (offset != null && (offset + scrollPosition + state.layoutInfo.afterContentPadding).absoluteValue > 1) {
            connection.appBarOffset = -scrollPosition.toFloat()
            //Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
          }
        }
    }
  }
  if (connection != null) {
    LazyColumn(modifier.nestedScroll(connection), state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled) {
      content()
    }
  } else {
    LazyColumn(modifier, state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled) {
      content()
    }
  }
}

@Composable
actual fun ColumnWithScrollBar(
  modifier: Modifier,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  state: ScrollState?,
  maxIntrinsicSize: Boolean,
  content: @Composable() (ColumnScope.() -> Unit)
) {
  val modifier = modifier.imePadding()
  val state = state ?: LocalAppBarHandler.current?.scrollState ?: rememberScrollState()
  val connection = LocalAppBarHandler.current?.connection
  LaunchedEffect(Unit) {
    snapshotFlow { state.value }
      .collect { scrollPosition ->
        val offset = connection?.appBarOffset
        if (offset != null && (offset + scrollPosition).absoluteValue > 1) {
          connection.appBarOffset = -scrollPosition.toFloat()
//          Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
      }
  }
  Box(Modifier.fillMaxHeight()) {
    if (connection != null) {
      Column(
        if (maxIntrinsicSize) {
          modifier.nestedScroll(connection).verticalScroll(state).height(IntrinsicSize.Max)
        } else {
          modifier.nestedScroll(connection).verticalScroll(state)
        }, verticalArrangement, horizontalAlignment
      ) {
        Spacer(Modifier.statusBarsPadding().padding(top = AppBarHeight * fontSizeSqrtMultiplier))
        content()
        Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.systemBars))
      }
    } else {
      Column(
        if (maxIntrinsicSize) {
          modifier.verticalScroll(state).height(IntrinsicSize.Max)
        } else {
          modifier.verticalScroll(state)
        }, verticalArrangement, horizontalAlignment
      ) {
        content()
        Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.systemBars))
      }
    }
    NavigationBarBackground()
  }
}
