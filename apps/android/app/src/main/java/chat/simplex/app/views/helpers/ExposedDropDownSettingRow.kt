package chat.simplex.app.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ExpandLess
import androidx.compose.material.icons.outlined.ExpandMore
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.HighOrLowlight

@Composable
fun <T> ExposedDropDownSettingRow(
  title: String,
  values: List<Pair<T, String>>,
  selection: State<T>,
  label: String? = null,
  icon: ImageVector? = null,
  iconTint: Color = HighOrLowlight,
  enabled: State<Boolean> = mutableStateOf(true),
  onSelected: (T) -> Unit
) {
  Row(
    Modifier.fillMaxWidth().padding(vertical = 10.dp),
    verticalAlignment = Alignment.CenterVertically,
  ) {
    var expanded by remember { mutableStateOf(false) }

    if (icon != null) {
      Icon(
        icon,
        "",
        Modifier.padding(end = 8.dp),
        tint = iconTint
      )
    }
    Text(title, Modifier.weight(1f), color = if (enabled.value) Color.Unspecified else HighOrLowlight)

    ExposedDropdownMenuBox(
      expanded = expanded,
      onExpandedChange = {
        expanded = !expanded && enabled.value
      }
    ) {
      Row(
        Modifier.padding(start = 10.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.End
      ) {
        val maxWidth = with(LocalDensity.current){ 180.sp.toDp() }
        Text(
          values.first { it.first == selection.value }.second + (if (label != null) " $label" else ""),
          Modifier.widthIn(max = maxWidth),
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = HighOrLowlight
        )
        Spacer(Modifier.size(12.dp))
        Icon(
          if (!expanded) Icons.Outlined.ExpandMore else Icons.Outlined.ExpandLess,
          generalGetString(R.string.icon_descr_more_button),
          tint = HighOrLowlight
        )
      }
      ExposedDropdownMenu(
        modifier = Modifier.widthIn(min = 200.dp),
        expanded = expanded,
        onDismissRequest = {
          expanded = false
        }
      ) {
        values.forEach { selectionOption ->
          DropdownMenuItem(
            onClick = {
              onSelected(selectionOption.first)
              expanded = false
            }
          ) {
            Text(
              selectionOption.second + (if (label != null) " $label" else ""),
              maxLines = 1,
              overflow = TextOverflow.Ellipsis,
            )
          }
        }
      }
    }
  }
}
