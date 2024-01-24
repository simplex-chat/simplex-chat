package chat.simplex.common.views.helpers

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import chat.simplex.common.model.CustomTimeUnit
import chat.simplex.common.model.timeText
import chat.simplex.res.MR

@Composable
expect fun CustomTimePicker(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits
)

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
        CustomTimeUnit.Week -> TimeUnitLimits(CustomTimeUnit.Week, maxValue = 52)
        CustomTimeUnit.Month -> TimeUnitLimits(CustomTimeUnit.Month, maxValue = 12)
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

fun showCustomTimePickerDialog(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits,
  title: String,
  confirmButtonText: String,
  confirmButtonAction: (Int) -> Unit,
  cancel: () -> Unit
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = title,
    onDismissRequest = cancel
  ) {
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      CustomTimePicker(
        selection,
        timeUnitsLimits
      )
      SectionItemView({
        AlertManager.shared.hideAlert()
        confirmButtonAction(selection.value)
      }
      ) {
        Text(
          confirmButtonText,
          Modifier.fillMaxWidth(),
          textAlign = TextAlign.Center,
          color = MaterialTheme.colors.primary
        )
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
        DropdownSelection.Custom -> {
          val selectedCustomTime = mutableStateOf(selection.value ?: 86400)
          showCustomTimePickerDialog(
            selectedCustomTime,
            timeUnitsLimits = customPickerTimeUnitsLimits,
            title = customPickerTitle,
            confirmButtonText = customPickerConfirmButtonText,
            confirmButtonAction = ::updateValue,
            cancel = {
              dropdownSelection.value = DropdownSelection.DropdownValue(selection.value)
            }
          )
        }
      }
    }
  )
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
