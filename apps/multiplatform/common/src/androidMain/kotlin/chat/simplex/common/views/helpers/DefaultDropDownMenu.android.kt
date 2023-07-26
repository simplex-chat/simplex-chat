package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.PopupProperties

actual interface DefaultExposedDropdownMenuBoxScope {
  @Composable
  actual fun DefaultExposedDropdownMenu(
    expanded: Boolean,
    onDismissRequest: () -> Unit,
    modifier: Modifier,
    content: @Composable ColumnScope.() -> Unit
  ) {
    DropdownMenu(expanded, onDismissRequest, modifier, content = content)
  }

  @Composable
  fun DropdownMenu(
    expanded: Boolean,
    onDismissRequest: () -> Unit,
    modifier: Modifier = Modifier,
    offset: DpOffset = DpOffset(0.dp, 0.dp),
    properties: PopupProperties = PopupProperties(focusable = true),
    content: @Composable ColumnScope.() -> Unit
  ) {
    androidx.compose.material.DropdownMenu(expanded, onDismissRequest, modifier, offset, properties, content)
  }
}

@Composable
actual fun DefaultExposedDropdownMenuBox(
  expanded: Boolean,
  onExpandedChange: (Boolean) -> Unit,
  modifier: Modifier,
  content: @Composable DefaultExposedDropdownMenuBoxScope.() -> Unit
) {
  val scope = remember { object : DefaultExposedDropdownMenuBoxScope {} }
  androidx.compose.material.ExposedDropdownMenuBox(expanded, onExpandedChange, modifier, content = {
    scope.content()
  })
}
