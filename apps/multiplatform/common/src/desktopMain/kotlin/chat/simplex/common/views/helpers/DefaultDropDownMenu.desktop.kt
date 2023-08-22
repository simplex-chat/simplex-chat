package chat.simplex.common.views.helpers

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.DropdownMenu
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.dp

actual fun Modifier.onRightClick(action: () -> Unit): Modifier = contextMenuOpenDetector { action() }

actual interface DefaultExposedDropdownMenuBoxScope {
  @Composable
  actual fun DefaultExposedDropdownMenu(
    expanded: Boolean,
    onDismissRequest: () -> Unit,
    modifier: Modifier,
    content: @Composable ColumnScope.() -> Unit
  ) {
     DropdownMenu(expanded, onDismissRequest, offset = DpOffset(0.dp, (-40).dp)) {
       Column {
         content()
       }
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
  val obj = remember { object : DefaultExposedDropdownMenuBoxScope {} }
  Box(Modifier
    .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { onExpandedChange(!expanded) })
  ) {
    obj.content()
  }
}
