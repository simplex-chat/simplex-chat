package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.res.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import dev.icerock.moko.resources.ImageResource

@Composable
fun <T> ExposedDropDownSetting(
  values: List<Pair<T, String>>,
  selection: State<T>,
  textColor: Color = MaterialTheme.colors.secondary,
  fontSize: TextUnit = 16.sp,
  label: String? = null,
  enabled: State<Boolean> = mutableStateOf(true),
  minWidth: Dp = 200.dp,
  maxWidth: Dp = with(LocalDensity.current) { 180.sp.toDp() },
  onSelected: (T) -> Unit
) {
  val expanded = remember { mutableStateOf(false) }
  ExposedDropdownMenuBox(
    expanded = expanded.value,
    onExpandedChange = {
      expanded.value = !expanded.value && enabled.value
    }
  ) {
    Row(
      Modifier.padding(start = 10.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.End
    ) {
      Text(
        values.first { it.first == selection.value }.second + (if (label != null) " $label" else ""),
        Modifier.widthIn(max = maxWidth),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = textColor,
        fontSize = fontSize,
      )
      Spacer(Modifier.size(12.dp))
      Icon(
        if (!expanded.value) painterResource(MR.images.ic_expand_more) else painterResource(MR.images.ic_expand_less),
        generalGetString(MR.strings.icon_descr_more_button),
        tint = MaterialTheme.colors.secondary
      )
    }
    DefaultExposedDropdownMenu(
      modifier = Modifier.widthIn(min = minWidth),
      expanded = expanded,
    ) {
      values.forEach { selectionOption ->
        DropdownMenuItem(
          onClick = {
            onSelected(selectionOption.first)
            expanded.value = false
          },
          contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)
        ) {
          Text(
            selectionOption.second + (if (label != null) " $label" else ""),
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = MenuTextColor,
            fontSize = fontSize,
          )
        }
      }
    }
  }
}

@Composable
fun <T> ExposedDropDownSettingWithIcon(
  values: List<Triple<T, ImageResource, String>>,
  selection: State<T>,
  fontSize: TextUnit = 16.sp,
  iconSize: Dp = 40.dp,
  listIconSize: Dp = 30.dp,
  iconColor: Color = MenuTextColor,
  enabled: State<Boolean> = mutableStateOf(true),
  minWidth: Dp = 200.dp,
  onSelected: (T) -> Unit
) {
  val expanded = remember { mutableStateOf(false) }
  ExposedDropdownMenuBox(
    expanded = expanded.value,
    onExpandedChange = {
      expanded.value = !expanded.value && enabled.value
    }
  ) {
    Row(
      Modifier.padding(start = 10.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.End
    ) {
      val choice = values.first { it.first == selection.value }
      Icon(painterResource(choice.second), choice.third, Modifier.size(iconSize), tint = iconColor)
    }
    DefaultExposedDropdownMenu(
      modifier = Modifier.widthIn(min = minWidth),
      expanded = expanded,
    ) {
      values.forEach { selectionOption ->
        DropdownMenuItem(
          onClick = {
            onSelected(selectionOption.first)
            expanded.value = false
          },
          contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)
        ) {
          Icon(painterResource(selectionOption.second), selectionOption.third, Modifier.size(listIconSize))
          Spacer(Modifier.width(15.dp))
          Text(
            selectionOption.third,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = MenuTextColor,
            fontSize = fontSize,
          )
        }
      }
    }
  }
}

@Composable
fun <T> ExposedDropDownSettingRow(
  title: String,
  values: List<Pair<T, String>>,
  selection: State<T>,
  textColor: Color = MaterialTheme.colors.secondary,
  label: String? = null,
  icon: Painter? = null,
  iconTint: Color = MaterialTheme.colors.secondary,
  enabled: State<Boolean> = mutableStateOf(true),
  minWidth: Dp = 200.dp,
  maxWidth: Dp = with(LocalDensity.current) { 180.sp.toDp() },
  onSelected: (T) -> Unit
) {
  SettingsActionItemWithContent(icon, title, iconColor = iconTint, disabled = !enabled.value) {
    ExposedDropDownSetting(values, selection ,textColor, label = label, enabled = enabled, minWidth = minWidth, maxWidth = maxWidth, onSelected = onSelected)
  }
}
