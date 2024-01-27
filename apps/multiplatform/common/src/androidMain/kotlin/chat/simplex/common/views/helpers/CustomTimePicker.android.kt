package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.CustomTimeUnit
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import com.sd.lib.compose.wheel_picker.*

@Composable
actual fun CustomTimePicker(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits>
) {
  fun getUnitValues(unit: CustomTimeUnit, selectedValue: Int): List<Int> {
    val unitLimits = timeUnitsLimits.firstOrNull { it.timeUnit == unit } ?: TimeUnitLimits.defaultUnitLimits(unit)
    val regularUnitValues = (unitLimits.minValue..unitLimits.maxValue).toList()
    return regularUnitValues + if (regularUnitValues.contains(selectedValue)) emptyList() else listOf(selectedValue)
  }

  val (unit, duration) = CustomTimeUnit.toTimeUnit(selection.value)
  val selectedUnit: MutableState<CustomTimeUnit> = remember { mutableStateOf(unit) }
  val selectedDuration = remember { mutableStateOf(duration) }
  val selectedUnitValues = remember { mutableStateOf(getUnitValues(selectedUnit.value, selectedDuration.value)) }
  val isTriggered = remember { mutableStateOf(false) }

  LaunchedEffect(selectedUnit.value) {
    // on initial composition, if passed selection doesn't fit into picker bounds, so that selectedDuration is bigger than selectedUnit maxValue
    // (e.g., for selection = 121 seconds: selectedUnit would be Second, selectedDuration would be 121 > selectedUnit maxValue of 120),
    // selectedDuration would've been replaced by maxValue - isTriggered check prevents this by skipping LaunchedEffect on initial composition
    if (isTriggered.value) {
      val maxValue = timeUnitsLimits.firstOrNull { it.timeUnit == selectedUnit.value }?.maxValue
      if (maxValue != null && selectedDuration.value > maxValue) {
        selectedDuration.value = maxValue
        selectedUnitValues.value = getUnitValues(selectedUnit.value, selectedDuration.value)
      } else {
        selectedUnitValues.value = getUnitValues(selectedUnit.value, selectedDuration.value)
        selection.value = selectedUnit.value.toSeconds * selectedDuration.value
      }
    } else {
      isTriggered.value = true
    }
  }

  LaunchedEffect(selectedDuration.value) {
    selection.value = selectedUnit.value.toSeconds * selectedDuration.value
  }

  Row(
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.spacedBy(0.dp)
  ) {
    Column(Modifier.weight(1f)) {
      val durationPickerState = rememberFWheelPickerState(selectedUnitValues.value.indexOf(selectedDuration.value))
      FVerticalWheelPicker(
        count = selectedUnitValues.value.count(),
        state = durationPickerState,
        unfocusedCount = 2,
        focus = {
          FWheelPickerFocusVertical(dividerColor = MaterialTheme.colors.primary)
        }
      ) { index ->
        Text(
          selectedUnitValues.value[index].toString(),
          fontSize = 18.sp,
          color = MaterialTheme.colors.primary
        )
      }
      LaunchedEffect(durationPickerState) {
        snapshotFlow { durationPickerState.currentIndex }
          .collect {
            selectedDuration.value = selectedUnitValues.value[it]
          }
      }
    }
    Column(Modifier.weight(1f)) {
      val unitPickerState = rememberFWheelPickerState(timeUnitsLimits.indexOfFirst { it.timeUnit == selectedUnit.value })
      FVerticalWheelPicker(
        count = timeUnitsLimits.count(),
        state = unitPickerState,
        unfocusedCount = 2,
        focus = {
          FWheelPickerFocusVertical(dividerColor = MaterialTheme.colors.primary)
        }
      ) { index ->
        Text(
          timeUnitsLimits[index].timeUnit.text,
          fontSize = 18.sp,
          color = MaterialTheme.colors.primary
        )
      }
      LaunchedEffect(unitPickerState) {
        snapshotFlow { unitPickerState.currentIndex }
          .collect {
            selectedUnit.value = timeUnitsLimits[it].timeUnit
          }
      }
    }
  }
}
