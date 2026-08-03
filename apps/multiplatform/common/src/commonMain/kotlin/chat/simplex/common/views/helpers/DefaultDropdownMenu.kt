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
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.appPlatform

@Composable
fun DefaultDropdownMenu(
  showMenu: MutableState<Boolean>,
  modifier: Modifier = Modifier,
  offset: DpOffset = DpOffset(0.dp, 0.dp),
  onClosed: State<() -> Unit> = remember { mutableStateOf({}) },
  dropdownMenuItems: (@Composable () -> Unit)?
) {
  val desktop = appPlatform.isDesktop
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(if (desktop) 10.dp else 25.dp))),
    typography = if (desktop) {
      MaterialTheme.typography.copy(
        body1 = MaterialTheme.typography.body1.copy(fontSize = 13.sp),
        body2 = MaterialTheme.typography.body2.copy(fontSize = 12.sp)
      )
    } else {
      MaterialTheme.typography
    }
  ) {
    DropdownMenu(
      expanded = showMenu.value,
      onDismissRequest = { showMenu.value = false },
      modifier = modifier
        .widthIn(min = if (desktop) 180.dp else 250.dp)
        .background(MaterialTheme.colors.surface)
        .padding(vertical = if (desktop) 2.dp else 4.dp),
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
  val desktop = appPlatform.isDesktop
  MaterialTheme(
    shapes = MaterialTheme.shapes.copy(medium = RoundedCornerShape(corner = CornerSize(if (desktop) 10.dp else 25.dp))),
    typography = if (desktop) MaterialTheme.typography.copy(body1 = MaterialTheme.typography.body1.copy(fontSize = 13.sp)) else MaterialTheme.typography
  ) {
    ExposedDropdownMenu(
      modifier = Modifier
        .widthIn(min = if (desktop) 180.dp else 200.dp)
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
