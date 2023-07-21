package chat.simplex.common.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.dp

expect interface DefaultExposedDropdownMenuBoxScope {
  @Composable
  open fun DefaultExposedDropdownMenu(
    expanded: Boolean,
    onDismissRequest: () -> Unit,
    modifier: Modifier = Modifier,
    content: @Composable ColumnScope.() -> Unit
  )
}

@Composable
expect fun DefaultExposedDropdownMenuBox(
  expanded: Boolean,
  onExpandedChange: (Boolean) -> Unit,
  modifier: Modifier = Modifier,
  content: @Composable DefaultExposedDropdownMenuBoxScope.() -> Unit
)

@Composable
fun DefaultDropdownMenu(
  showMenu: MutableState<Boolean>,
  offset: DpOffset = DpOffset(0.dp, 0.dp),
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    DropdownMenu(
      expanded = showMenu.value,
      onDismissRequest = { showMenu.value = false },
      modifier = Modifier
        .widthIn(min = 250.dp)
        .background(MaterialTheme.colors.surface)
        .padding(vertical = 4.dp),
      offset = offset,
    ) {
      dropdownMenuItems?.invoke()
    }
  }
}

@Composable
fun DefaultExposedDropdownMenuBoxScope.DefaultExposedDropdownMenu(
  expanded: MutableState<Boolean>,
  modifier: Modifier = Modifier,
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    DefaultExposedDropdownMenu(
      modifier = Modifier
        .widthIn(min = 200.dp)
        .background(MaterialTheme.colors.surface)
        .then(modifier),
      expanded = expanded.value,
      onDismissRequest = {
        expanded.value = false
      }
    ) {
      dropdownMenuItems?.invoke()
    }
  }
}