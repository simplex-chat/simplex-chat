package chat.simplex.common.views.helpers

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.input.key.key
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import com.sd.lib.compose.wheel_picker.*

@Composable
fun CustomTimePicker(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits
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

data class TimeUnitLimits(
  val timeUnit: CustomTimeUnit,
  val minValue: Int = 1,
  val maxValue: Int
) {
  companion object {
    fun defaultUnitLimits(unit: CustomTimeUnit): TimeUnitLimits {
      return when (unit) {
        CustomTimeUnit.Second -> TimeUnitLimits(CustomTimeUnit.Second, maxValue = 120)
        CustomTimeUnit.Minute -> TimeUnitLimits(CustomTimeUnit.Minute, maxValue = 120)
        CustomTimeUnit.Hour -> TimeUnitLimits(CustomTimeUnit.Hour, maxValue = 72)
        CustomTimeUnit.Day -> TimeUnitLimits(CustomTimeUnit.Day, maxValue = 60)
        CustomTimeUnit.Week -> TimeUnitLimits(CustomTimeUnit.Week, maxValue = 12) // TODO in 5.2 - 54
        CustomTimeUnit.Month -> TimeUnitLimits(CustomTimeUnit.Month, maxValue = 3) // TODO in 5.2 - 12
      }
    }

    val defaultUnitsLimits: List<TimeUnitLimits>
      get() = listOf(
        defaultUnitLimits(CustomTimeUnit.Second),
        defaultUnitLimits(CustomTimeUnit.Minute),
        defaultUnitLimits(CustomTimeUnit.Hour),
        defaultUnitLimits(CustomTimeUnit.Day),
        defaultUnitLimits(CustomTimeUnit.Week),
        defaultUnitLimits(CustomTimeUnit.Month)
      )
  }
}

@Composable
fun CustomTimePickerDialog(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits,
  title: String,
  confirmButtonText: String,
  confirmButtonAction: (Int) -> Unit,
  cancel: () -> Unit
) {
  DefaultDialog(
    onDismissRequest = { cancel() }
  ) {
    Surface(
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    ) {
      Box(
        contentAlignment = Alignment.Center
      ) {
        Column(
          modifier = Modifier.padding(DEFAULT_PADDING),
          verticalArrangement = Arrangement.spacedBy(6.dp),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
          ) {
            Text(" ") // centers title
            Text(
              title,
              fontSize = 16.sp,
              color = MaterialTheme.colors.secondary
            )
            Icon(
              painterResource(MR.images.ic_close),
              generalGetString(MR.strings.icon_descr_close_button),
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier
                .size(25.dp)
                .clickable { cancel() }
            )
          }

          CustomTimePicker(
            selection,
            timeUnitsLimits
          )

          TextButton(onClick = { confirmButtonAction(selection.value) }) {
            Text(
              confirmButtonText,
              fontSize = 18.sp,
              color = MaterialTheme.colors.primary
            )
          }
        }
      }
    }
  }
}

@Composable
fun DropdownCustomTimePickerSettingRow(
  selection: MutableState<Int?>,
  propagateExternalSelectionUpdate: Boolean = false,
  label: String,
  dropdownValues: List<Int?>,
  customPickerTitle: String,
  customPickerConfirmButtonText: String,
  customPickerTimeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits,
  onSelected: (Int?) -> Unit
) {
  fun getValues(selectedValue: Int?): List<DropdownSelection> =
    dropdownValues.map { DropdownSelection.DropdownValue(it) } +
        (if (dropdownValues.contains(selectedValue)) listOf() else listOf(DropdownSelection.DropdownValue(selectedValue))) +
        listOf(DropdownSelection.Custom)

  val dropdownSelection: MutableState<DropdownSelection> = remember { mutableStateOf(DropdownSelection.DropdownValue(selection.value)) }
  val values: MutableState<List<DropdownSelection>> = remember { mutableStateOf(getValues(selection.value)) }
  val showCustomTimePicker = remember { mutableStateOf(false) }

  fun updateValue(selectedValue: Int?) {
    values.value = getValues(selectedValue)
    dropdownSelection.value = DropdownSelection.DropdownValue(selectedValue)
    onSelected(selectedValue)
  }

  if (propagateExternalSelectionUpdate) {
    LaunchedEffect(selection.value) {
      values.value = getValues(selection.value)
      dropdownSelection.value = DropdownSelection.DropdownValue(selection.value)
    }
  }

  ExposedDropDownSettingRow(
    label,
    values.value.map { sel: DropdownSelection ->
      when (sel) {
        is DropdownSelection.DropdownValue -> sel to timeText(sel.value)
        DropdownSelection.Custom -> sel to generalGetString(MR.strings.custom_time_picker_custom)
      }
    },
    dropdownSelection,
    onSelected = { sel: DropdownSelection ->
      when (sel) {
        is DropdownSelection.DropdownValue -> updateValue(sel.value)
        DropdownSelection.Custom -> showCustomTimePicker.value = true
      }
    }
  )

  if (showCustomTimePicker.value) {
    val selectedCustomTime = remember { mutableStateOf(selection.value ?: 86400) }
    CustomTimePickerDialog(
      selectedCustomTime,
      timeUnitsLimits = customPickerTimeUnitsLimits,
      title = customPickerTitle,
      confirmButtonText = customPickerConfirmButtonText,
      confirmButtonAction = { time ->
        updateValue(time)
        showCustomTimePicker.value = false
      },
      cancel = {
        dropdownSelection.value = DropdownSelection.DropdownValue(selection.value)
        showCustomTimePicker.value = false
      }
    )
  }
}

private sealed class DropdownSelection {
  data class DropdownValue(val value: Int?): DropdownSelection()
  object Custom: DropdownSelection()

  override fun equals(other: Any?): Boolean =
    other is DropdownSelection &&
        when (other) {
          is DropdownValue -> this is DropdownValue && this.value == other.value
          is Custom -> this is Custom
        }

  override fun hashCode(): Int =
    // DO NOT REMOVE the as? cast as it will turn them into recursive hashCode calls
    // https://youtrack.jetbrains.com/issue/KT-31239
    when (this) {
      is DropdownValue -> (this as? DropdownValue).hashCode()
      is Custom -> (this as? Custom).hashCode()
    }
}
