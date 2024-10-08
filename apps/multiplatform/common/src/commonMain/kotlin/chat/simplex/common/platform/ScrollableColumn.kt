package chat.simplex.common.platform

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.FlingBehavior
import androidx.compose.foundation.gestures.ScrollableDefaults
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
expect fun LazyColumnWithScrollBar(
  modifier: Modifier = Modifier,
  state: LazyListState? = null,
  contentPadding: PaddingValues = PaddingValues(0.dp),
  reverseLayout: Boolean = false,
  verticalArrangement: Arrangement.Vertical =
    if (!reverseLayout) Arrangement.Top else Arrangement.Bottom,
  horizontalAlignment: Alignment.Horizontal = Alignment.Start,
  flingBehavior: FlingBehavior = ScrollableDefaults.flingBehavior(),
  userScrollEnabled: Boolean = true,
  content: LazyListScope.() -> Unit
)

@Composable
expect fun ColumnWithScrollBar(
  modifier: Modifier = Modifier,
  verticalArrangement: Arrangement.Vertical = Arrangement.Top,
  horizontalAlignment: Alignment.Horizontal = Alignment.Start,
  state: ScrollState? = null,
  // set true when you want to show something in the center with respected .fillMaxSize()
  maxIntrinsicSize: Boolean = false,
  content: @Composable ColumnScope.() -> Unit
)
