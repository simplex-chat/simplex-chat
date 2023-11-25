package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import chat.simplex.common.model.CustomTimeUnit

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
  CustomTimePicker(
    selection,
    timeUnitsLimits,
    confirmButtonAction,
    confirmButtonText,
    selectedUnit,
    selectedUnitValues,
    selectedDuration
  )
}