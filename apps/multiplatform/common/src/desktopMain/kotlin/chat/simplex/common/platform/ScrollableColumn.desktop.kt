package chat.simplex.common.platform

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.AnimationVector1D
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.FlingBehavior
import androidx.compose.foundation.interaction.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.*
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
  content: LazyListScope.() -> Unit
) {
  val scope = rememberCoroutineScope()
  val scrollBarAlpha = remember { Animatable(0f) }
  val scrollJob: MutableState<Job> = remember { mutableStateOf(Job()) }
  val scrollModifier = remember {
    Modifier
      .pointerInput(Unit) {
        detectCursorMove {
          scope.launch {
            scrollBarAlpha.animateTo(1f)
          }
          scrollJob.value.cancel()
          scrollJob.value = scope.launch {
            delay(1000L)
            scrollBarAlpha.animateTo(0f)
          }
        }
      }
  }
  val state = state ?: LocalAppBarHandler.current?.listState ?: rememberLazyListState()
  val connection = LocalAppBarHandler.current?.connection
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on lazy column state
  // (only first visible row is useful because LazyColumn doesn't have absolute scroll position, only relative to row)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    snapshotFlow { state.firstVisibleItemScrollOffset }
      .filter { state.firstVisibleItemIndex == 0 }
      .collect { scrollPosition ->
        val offset = connection?.appBarOffset
        if (reverseLayout) {
          // always show app bar in reverse layout
          connection?.appBarOffset = -1000f
        } else if (offset != null && ((offset + scrollPosition).absoluteValue > 1 || scrollBarDraggingState.value)) {
          connection.appBarOffset = -scrollPosition.toFloat()
//          Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
      }
  }
  Box(if (connection != null) Modifier.nestedScroll(connection) else Modifier) {
    LazyColumn(modifier.then(scrollModifier), state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
      DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, reverseLayout, scrollBarDraggingState)
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
  val scope = rememberCoroutineScope()
  val scrollBarAlpha = remember { Animatable(0f) }
  val scrollJob: MutableState<Job> = remember { mutableStateOf(Job()) }
  val scrollModifier = remember {
    Modifier
      .pointerInput(Unit) {
        detectCursorMove {
          scope.launch {
            scrollBarAlpha.animateTo(1f)
          }
          scrollJob.value.cancel()
          scrollJob.value = scope.launch {
            delay(1000L)
            scrollBarAlpha.animateTo(0f)
          }
        }
      }
  }
  val state = state ?: LocalAppBarHandler.current?.scrollState ?: rememberScrollState()
  val connection = LocalAppBarHandler.current?.connection
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on column state
  // (exact scroll position is available but in Int, not Float)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    snapshotFlow { state.value }
      .collect { scrollPosition ->
        val offset = connection?.appBarOffset
        if (offset != null && ((offset + scrollPosition).absoluteValue > 1 || scrollBarDraggingState.value)) {
          connection.appBarOffset = -scrollPosition.toFloat()
//          Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
      }
  }
  Box(if (connection != null) Modifier.nestedScroll(connection) else Modifier) {
    Column(
      if (maxIntrinsicSize) {
        modifier.verticalScroll(state).height(IntrinsicSize.Max).then(scrollModifier)
      } else {
        modifier.verticalScroll(state).then(scrollModifier)
      },
      verticalArrangement, horizontalAlignment) {
      if (connection != null) {
        Spacer(Modifier.padding(top = AppBarHeight * fontSizeSqrtMultiplier))
      }
      content()
    }
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
      DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, false, scrollBarDraggingState)
    }
  }
}

@Composable
fun DesktopScrollBar(adapter: androidx.compose.foundation.v2.ScrollbarAdapter, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean, updateDraggingState: MutableState<Boolean> = remember { mutableStateOf(false) }) {
  val scope = rememberCoroutineScope()
  val interactionSource = remember { MutableInteractionSource() }
  val isHovered by interactionSource.collectIsHoveredAsState()
  val isDragged by interactionSource.collectIsDraggedAsState()
  LaunchedEffect(isHovered, isDragged) {
    scrollJob.value.cancel()
    updateDraggingState.value = isDragged
    if (isHovered || isDragged) {
      scrollBarAlpha.animateTo(1f)
    } else {
      scrollJob.value = scope.launch {
        delay(1000L)
        scrollBarAlpha.animateTo(0f)
      }
    }
  }
  VerticalScrollbar(
    modifier = modifier.graphicsLayer { alpha = scrollBarAlpha.value }
      .onPointerEvent(PointerEventType.Enter) {
        scrollJob.value.cancel()
        scope.launch {
          scrollBarAlpha.animateTo(1f)
        }
      },
    reverseLayout = reversed,
    style = LocalScrollbarStyle.current.copy(
      thickness = if (isHovered || isDragged) 10.dp else 6.dp,
      shape = RoundedCornerShape(if (isHovered || isDragged) 5.dp else 3.dp),
      unhoverColor = if (MaterialTheme.colors.isLight) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.7f) else MaterialTheme.colors.onBackground.mixWith(MaterialTheme.colors.background, 0.3f),
      hoverColor = if (MaterialTheme.colors.isLight) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.6f) else MaterialTheme.colors.onBackground.mixWith(MaterialTheme.colors.background, 0.4f)
    ),
    adapter = adapter,
    interactionSource = interactionSource
  )
}
