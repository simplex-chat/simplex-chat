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
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chatlist.NavigationBarBackground
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
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
  additionalBarOffset: State<Dp>?,
  additionalTopBar: State<Boolean>,
  chatBottomBar: State<Boolean>,
  fillMaxSize: Boolean,
  content: LazyListScope.() -> Unit
) {
  val handler = LocalAppBarHandler.current
  require(handler != null) { "Using LazyColumnWithScrollBar and without AppBarHandler is an error. Use LazyColumnWithScrollBarNoAppBar instead" }

  val state = state ?: handler.listState
  val connection = handler.connection
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
          if ((offset + scrollPosition + state.layoutInfo.afterContentPadding).absoluteValue > 1) {
            connection.appBarOffset = -scrollPosition.toFloat()
            //Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
          }
        }
    }
  }
  LazyColumn(
    if (fillMaxSize) {
      Modifier.fillMaxSize().copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).then(modifier).nestedScroll(connection)
    } else {
      Modifier.copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).then(modifier).nestedScroll(connection)
    },
    state,
    contentPadding,
    reverseLayout,
    verticalArrangement,
    horizontalAlignment,
    flingBehavior,
    userScrollEnabled
  ) {
    content()
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
  content: LazyListScope.() -> Unit
  ) {
  val state = state ?: rememberLazyListState()
  LazyColumn(modifier, state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled) {
    content()
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

  val modifier = if (fillMaxSize) Modifier.fillMaxSize().then(modifier).imePadding() else modifier.imePadding()
  val state = state ?: handler.scrollState
  val connection = handler.connection
  LaunchedEffect(Unit) {
    snapshotFlow { state.value }
      .collect { scrollPosition ->
        val offset = connection.appBarOffset
        if ((offset + scrollPosition).absoluteValue > 1) {
          connection.appBarOffset = -scrollPosition.toFloat()
//          Log.d(TAG, "Scrolling position changed from $offset to ${connection.appBarOffset}")
        }
      }
  }
  val oneHandUI = remember { derivedStateOf { if (appPrefs.onboardingStage.state.value == OnboardingStage.OnboardingComplete) appPrefs.oneHandUI.state.value else false } }
  Box(Modifier.fillMaxHeight()) {
    Column(
      if (maxIntrinsicSize) {
        Modifier.copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).then(modifier).nestedScroll(connection).verticalScroll(state).height(IntrinsicSize.Max)
      } else {
        Modifier.copyViewToAppBar(remember { appPrefs.appearanceBarsBlurRadius.state }.value, LocalAppBarHandler.current?.graphicsLayer).then(modifier).nestedScroll(connection).verticalScroll(state)
      }, verticalArrangement, horizontalAlignment
    ) {
      if (oneHandUI.value) {
        Spacer(Modifier.padding(top = DEFAULT_PADDING + 5.dp).windowInsetsTopHeight(WindowInsets.statusBars))
        content()
        Spacer(Modifier.navigationBarsPadding().padding(bottom = AppBarHeight * fontSizeSqrtMultiplier))
      } else {
        Spacer(Modifier.statusBarsPadding().padding(top = AppBarHeight * fontSizeSqrtMultiplier))
        content()
        Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.systemBars))
      }
    }
    if (!oneHandUI.value) {
      NavigationBarBackground(false, false)
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
  val modifier = modifier.imePadding()
  val state = state ?: rememberScrollState()
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Box(Modifier.fillMaxHeight()) {
    Column(
      if (maxIntrinsicSize) {
        modifier.verticalScroll(state).height(IntrinsicSize.Max)
      } else {
        modifier.verticalScroll(state)
      }, verticalArrangement, horizontalAlignment
    ) {
      if (oneHandUI.value) {
        Spacer(Modifier.windowInsetsTopHeight(WindowInsets.systemBars))
        content()
      } else {
        content()
        Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.systemBars))
      }
    }
    if (!oneHandUI.value) {
      NavigationBarBackground(false, false)
    }
  }
}
