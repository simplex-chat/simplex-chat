package chat.simplex.common.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.dp

@Composable
fun DefaultDropdownMenu(
  showMenu: MutableState<Boolean>,
  modifier: Modifier = Modifier,
  offset: DpOffset = DpOffset(0.dp, 0.dp),
  onClosed: State<() -> Unit> = remember { mutableStateOf({}) },
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    DropdownMenu(
      expanded = showMenu.value,
      onDismissRequest = { showMenu.value = false },
      modifier = modifier
        .widthIn(min = 250.dp)
        .background(MaterialTheme.colors.surface)
        .padding(vertical = 4.dp),
      offset = offset,
    ) {
      dropdownMenuItems?.invoke()
        DisposableEffect(Unit) {
          onDispose {
            onClosed.value()
          }
      }
    }
  }
}

@Composable
fun ExposedDropdownMenuBoxScope.DefaultExposedDropdownMenu(
  expanded: MutableState<Boolean>,
  modifier: Modifier = Modifier,
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    ExposedDropdownMenu(
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