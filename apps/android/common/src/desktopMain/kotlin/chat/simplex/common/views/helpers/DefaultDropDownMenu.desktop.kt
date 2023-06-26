package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier

actual interface DefaultExposedDropdownMenuBoxScope {
  @Composable
  actual fun DefaultExposedDropdownMenu(
    expanded: Boolean,
    onDismissRequest: () -> Unit,
    modifier: Modifier,
    content: @Composable ColumnScope.() -> Unit
  ) {
    Column {
      content()
    }
  }
}

@Composable
actual fun DefaultExposedDropdownMenuBox(
  expanded: Boolean,
  onExpandedChange: (Boolean) -> Unit,
  modifier: Modifier,
  content: @Composable DefaultExposedDropdownMenuBoxScope.() -> Unit
) {
  if (expanded) {
    val obj = remember { object : DefaultExposedDropdownMenuBoxScope {} }
    obj.content()
  }
}