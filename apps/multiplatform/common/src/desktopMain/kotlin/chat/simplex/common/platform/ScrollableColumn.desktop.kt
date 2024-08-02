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
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.detectCursorMove
import chat.simplex.common.views.helpers.mixWith
import kotlinx.coroutines.*

@Composable
actual fun LazyColumnWithScrollBar(
  modifier: Modifier,
  state: LazyListState,
  contentPadding: PaddingValues,
  reverseLayout: Boolean,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  flingBehavior: FlingBehavior,
  userScrollEnabled: Boolean,
  content: LazyListScope.() -> Unit
) {
  if (appPlatform.isAndroid) {
    LazyColumn(modifier, state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
  } else {
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
    Box {
      LazyColumn(modifier.then(if (appPlatform.isDesktop) scrollModifier else Modifier), state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
      Box(Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
        DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.align(Alignment.CenterEnd).fillMaxHeight(), scrollBarAlpha, scrollJob, reverseLayout)
      }
    }
  }
}

@Composable
actual fun ColumnWithScrollBar(
  modifier: Modifier,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  state: ScrollState,
  content: @Composable ColumnScope.() -> Unit
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
  Column(modifier.verticalScroll(state).then(scrollModifier), verticalArrangement, horizontalAlignment, content)
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
    DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.align(Alignment.CenterEnd).fillMaxHeight(), scrollBarAlpha, scrollJob, false)
  }
}

@Composable
fun DesktopScrollBar(adapter: androidx.compose.foundation.v2.ScrollbarAdapter, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean) {
  val scope = rememberCoroutineScope()
  val interactionSource = remember { MutableInteractionSource() }
  val isHovered by interactionSource.collectIsHoveredAsState()
  val isDragged by interactionSource.collectIsDraggedAsState()
  LaunchedEffect(isHovered, isDragged) {
    scrollJob.value.cancel()
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
