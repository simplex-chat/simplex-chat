package chat.simplex.common.platform

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.FlingBehavior
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.*

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
  val state = state ?: LocalAppBarHandler.current?.listState ?: rememberLazyListState()
  val connection = LocalAppBarHandler.current?.connection
  Box(if (connection != null) Modifier.nestedScroll(connection) else Modifier) {
    LazyColumn(modifier, state, contentPadding, reverseLayout, verticalArrangement, horizontalAlignment, flingBehavior, userScrollEnabled, content)
  }
}

@Composable
actual fun ColumnWithScrollBar(
  modifier: Modifier,
  verticalArrangement: Arrangement.Vertical,
  horizontalAlignment: Alignment.Horizontal,
  state: ScrollState?,
  content: @Composable ColumnScope.() -> Unit
) {
  val state = state ?: LocalAppBarHandler.current?.scrollState ?: rememberScrollState()
  val connection = LocalAppBarHandler.current?.connection
  Box(if (connection != null) Modifier.nestedScroll(connection) else Modifier) {
    Column(modifier.verticalScroll(state), verticalArrangement, horizontalAlignment, content)
  }
}
