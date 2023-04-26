package chat.simplex.app.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.*

@Composable
fun DefaultDropdownMenu(
  showMenu: MutableState<Boolean>,
  offset: DpOffset = DpOffset(0.dp, 0.dp),
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    colors = MaterialTheme.colors.copy(surface = MaterialTheme.colors.surface),
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    DropdownMenu(
      expanded = showMenu.value,
      onDismissRequest = { showMenu.value = false },
      Modifier
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
fun ExposedDropdownMenuBoxScope.DefaultExposedDropdownMenu(
  expanded: MutableState<Boolean>,
  modifier: Modifier = Modifier,
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  MaterialTheme(
    colors = MaterialTheme.colors.copy(surface = if (isInDarkTheme()) Color(0xFF080808) else MaterialTheme.colors.background),
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(25.dp)))
  ) {
    ExposedDropdownMenu(
      modifier = Modifier.widthIn(min = 200.dp).then(modifier),
      expanded = expanded.value,
      onDismissRequest = {
        expanded.value = false
      }
    ) {
      dropdownMenuItems?.invoke()
    }
  }
}