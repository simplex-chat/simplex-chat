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
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.filter
import kotlin.math.*

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
  additionalBarOffset: State<Dp>?,
  additionalTopBar: State<Boolean>,
  chatBottomBar: State<Boolean>,
  fillMaxSize: Boolean,
  content: LazyListScope.() -> Unit
) {
  val handler = LocalAppBarHandler.current
  require(handler != null) { "Using LazyColumnWithScrollBar and without AppBarHandler is an error. Use LazyColumnWithScrollBarNoAppBar instead" }

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
  val state = state ?: handler.listState
  val connection = handler.connection
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on lazy column state
  // (only first visible row is useful because LazyColumn doesn't have absolute scroll position, only relative to row)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    if (reverseLayout) {
      snapshotFlow { state.layoutInfo.visibleItemsInfo.lastOrNull()?.offset ?: 0 }
        .collect { scrollPosition ->
            connection.appBarOffset = if (state.layoutInfo.visibleItemsInfo.lastOrNull()?.index == state.layoutInfo.totalItemsCount - 1) {
              state.layoutInfo.viewportEndOffset - scrollPosition.toFloat() - state.layoutInfo.afterContentPadding
            } else {
              // show always when last item is not visible
              -1000f
            }
            //Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
    } else {
      snapshotFlow { state.firstVisibleItemScrollOffset }
        .filter { state.firstVisibleItemIndex == 0 }
        .collect { scrollPosition ->
          val offset = connection.appBarOffset
          if ((offset + scrollPosition + state.layoutInfo.afterContentPadding).absoluteValue > 1 || scrollBarDraggingState.value) {
              connection.appBarOffset = -scrollPosition.toFloat()
              //Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
          }
        }
    }
  }
  val modifier = if (fillMaxSize) Modifier.fillMaxSize().then(modifier) else modifier
  Box(Modifier.copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).nestedScroll(connection)) {
    LazyColumn(modifier.then(scrollModifier), state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
    ScrollBar(reverseLayout, state, scrollBarAlpha, scrollJob, scrollBarDraggingState, additionalBarOffset, additionalTopBar, chatBottomBar)
  }
}

@Composable
actual fun LazyColumnWithScrollBarNoAppBar(
  modifier: Modifier,
  state: LazyListState?,
  contentPadding: PaddingValues,
  reverseLayout: Boolean,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  flingBehavior: FlingBehavior,
  userScrollEnabled: Boolean,
  additionalBarOffset: State<Dp>?,
  additionalTopBar: State<Boolean>,
  chatBottomBar: State<Boolean>,
  maxHeight: State<Dp>?,
  containerAlignment: Alignment,
  content: LazyListScope.() -> Unit,
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
  val state = state ?: rememberLazyListState()
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on lazy column state
  // (only first visible row is useful because LazyColumn doesn't have absolute scroll position, only relative to row)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  Box(contentAlignment = containerAlignment) {
    LazyColumn(modifier.then(scrollModifier), state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
    Box(if (maxHeight?.value != null) Modifier.height(maxHeight.value).fillMaxWidth() else Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
      DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, reverseLayout, scrollBarDraggingState)
    }
  }
}

@Composable
private fun ScrollBar(
  reverseLayout: Boolean,
  state: LazyListState,
  scrollBarAlpha: Animatable<Float, AnimationVector1D>,
  scrollJob: MutableState<Job>,
  scrollBarDraggingState: MutableState<Boolean>,
  additionalBarHeight: State<Dp>?,
  additionalTopBar: State<Boolean>,
  chatBottomBar: State<Boolean>,
) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val topBarPadding = if (additionalTopBar.value) AppBarHeight * fontSizeSqrtMultiplier else 0.dp
  val padding = if (additionalBarHeight != null) {
    PaddingValues(top = topBarPadding + if (oneHandUI.value && chatBottomBar.value) 0.dp else AppBarHeight * fontSizeSqrtMultiplier, bottom = additionalBarHeight.value)
  } else if (reverseLayout) {
    PaddingValues(bottom = AppBarHeight * fontSizeSqrtMultiplier)
  } else {
    PaddingValues(top = if (oneHandUI.value && chatBottomBar.value) 0.dp else AppBarHeight * fontSizeSqrtMultiplier)
  }
  Box(Modifier.fillMaxSize().padding(padding), contentAlignment = Alignment.CenterEnd) {
    DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, reverseLayout, scrollBarDraggingState)
  }
}

@Composable
actual fun ColumnWithScrollBar(
  modifier: Modifier,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  state: ScrollState?,
  maxIntrinsicSize: Boolean,
  fillMaxSize: Boolean,
  content: @Composable() (ColumnScope.() -> Unit)
) {
  val handler = LocalAppBarHandler.current
  require(handler != null) { "Using ColumnWithScrollBar and without AppBarHandler is an error. Use ColumnWithScrollBarNoAppBar instead" }

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
  val state = state ?: handler.scrollState
  val connection = handler.connection
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on column state
  // (exact scroll position is available but in Int, not Float)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    snapshotFlow { state.value }
      .collect { scrollPosition ->
        val offset = connection.appBarOffset
        if ((offset + scrollPosition).absoluteValue > 1 || scrollBarDraggingState.value) {
          connection.appBarOffset = -scrollPosition.toFloat()
//          Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
      }
  }
  val modifier = if (fillMaxSize) Modifier.fillMaxSize().then(modifier) else modifier
  Box(Modifier.nestedScroll(connection)) {
    val oneHandUI = remember { derivedStateOf { if (appPrefs.onboardingStage.state.value == OnboardingStage.OnboardingComplete) appPrefs.oneHandUI.state.value else false } }
    val padding = if (oneHandUI.value) PaddingValues(bottom = AppBarHeight * fontSizeSqrtMultiplier) else PaddingValues(top = AppBarHeight * fontSizeSqrtMultiplier)
    Column(
      if (maxIntrinsicSize) {
        modifier.copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).verticalScroll(state).height(IntrinsicSize.Max).then(scrollModifier)
      } else {
        modifier.then(scrollModifier).copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).verticalScroll(state)
      },
      verticalArrangement, horizontalAlignment
    ) {
      Spacer(if (oneHandUI.value) Modifier.padding(top = DEFAULT_PADDING + 5.dp) else Modifier.padding(padding))
      content()
      if (oneHandUI.value) {
        Spacer(Modifier.padding(padding))
      }
    }
    Box(Modifier.fillMaxSize().padding(padding), contentAlignment = Alignment.CenterEnd) {
      DesktopScrollBar(rememberScrollbarAdapter(state), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, false, scrollBarDraggingState)
    }
  }
}

@Composable
actual fun ColumnWithScrollBarNoAppBar(
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
  val state = state ?: rememberScrollState()
  // When scroll bar is dragging, there is no scroll event in nested scroll modifier. So, listen for changes on column state
  // (exact scroll position is available but in Int, not Float)
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  Box {
    Column(
      if (maxIntrinsicSize) {
        modifier.verticalScroll(state).height(IntrinsicSize.Max).then(scrollModifier)
      } else {
        modifier.then(scrollModifier).verticalScroll(state)
      },
      verticalArrangement, horizontalAlignment) {
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
