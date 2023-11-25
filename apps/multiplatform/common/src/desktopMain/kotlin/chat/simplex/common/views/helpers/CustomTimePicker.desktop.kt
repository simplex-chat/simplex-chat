package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun CustomTimePickerOrDropdown(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits>,
  confirmButtonAction: (Int) -> Unit,
  confirmButtonText: String,
  selectedUnit: MutableState<CustomTimeUnit>,
  selectedUnitValues: MutableState<List<Int>>,
  selectedDuration: MutableState<Int>,
) {
  val showTimes = rememberSaveable { mutableStateOf(false) }
  val showDurations = rememberSaveable { mutableStateOf(false) }

  val durations = selectedUnitValues.value.map {
    Pair(it, it.toString())
  }

  val timeUnits = timeUnitsLimits.map {
    Pair(it.timeUnit, it.timeUnit.text)
  }

  Column(
    modifier = Modifier
      .fillMaxWidth(),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Row (
      modifier = Modifier.padding(DEFAULT_PADDING).weight(1f),
      horizontalArrangement = Arrangement.Center,
      verticalAlignment = Alignment.CenterVertically
    ) {
      // durations
      CustomTimePickerDropdown(
        modifier = Modifier.weight(1f),
        expanded = showDurations,
        items = durations,
        selection = selectedDuration,
        defaultString = selectedDuration.value.toString(),
        onUpdate = { x -> selectedDuration.value = x }
      )

      // times
      CustomTimePickerDropdown(
        modifier = Modifier.weight(1f),
        expanded = showTimes,
        items = timeUnits,
        selection = selectedUnit,
        defaultString = selectedUnit.value.text,
        onUpdate = { x -> selectedUnit.value = x }
      )
    }

    TextButton(
      onClick = { confirmButtonAction(selection.value) }
    ) {
      Text(
        confirmButtonText,
        fontSize = 18.sp,
        color = MaterialTheme.colors.primary
      )
    }
  }

}

@Composable
fun <T> CustomTimePickerDropdown(
  modifier: Modifier,
  expanded: MutableState<Boolean>,
  items: List<Pair<T, String>>,
  selection: MutableState<T>,
  defaultString: String,
  onUpdate: (T) -> Unit
) {
  Column(
    modifier = modifier.padding(DEFAULT_PADDING)
  ) {
    ExposedDropdownMenuBox(
      expanded = expanded.value,
      onExpandedChange = {
        expanded.value = !expanded.value
      }
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.Center
      ) {
        Text(
          text = defaultString,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
        Spacer(Modifier.size(4.dp))
        Icon(
          if (!expanded.value) painterResource(MR.images.ic_expand_more) else painterResource(MR.images.ic_expand_less),
          generalGetString(MR.strings.invite_to_group_button),
          modifier = Modifier.padding(start = 8.dp),
          tint = MaterialTheme.colors.secondary
        )
      }
    }

    DefaultDropdownMenu(expanded) {
      items.forEach { selectionOption ->
        DropdownMenuItem(
          onClick = {
            selection.value = selectionOption.first
            onUpdate(selectionOption.first)
            expanded.value = false
          },
          contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)
        ) {
          Text(
            selectionOption.second,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
          )
        }
      }
    }
  }
}