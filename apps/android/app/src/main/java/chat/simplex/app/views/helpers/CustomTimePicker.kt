package chat.simplex.app.views.helpers

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import chat.simplex.app.R
import chat.simplex.app.model.CustomTimeUnit
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import com.sd.lib.compose.wheel_picker.*

@Composable
fun CustomTimePicker(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits> = TimeUnitLimits.defaultUnitsLimits
) {
  fun getUnitValues(unit: CustomTimeUnit, selectedValue: Int): List<Int> {
    val unitLimits = timeUnitsLimits.firstOrNull { it.timeUnit == unit } ?: TimeUnitLimits.defaultUnitLimits(unit)
    val unitValues = (unitLimits.minValue..unitLimits.maxValue).toList()
    return unitValues + if (unitValues.contains(selectedValue)) emptyList() else listOf(selectedValue)
  }

  val (unit, duration) = CustomTimeUnit.toTimeUnit(selection.value)
  val selectedUnit: MutableState<CustomTimeUnit> = remember { mutableStateOf(unit) }
  val selectedDuration = remember { mutableStateOf(duration) }
  val selectedUnitValues = remember { mutableStateOf(getUnitValues(selectedUnit.value, selectedDuration.value)) }

  LaunchedEffect(selectedUnit.value) {
    val maxValue = timeUnitsLimits.firstOrNull { it.timeUnit == selectedUnit.value }?.maxValue
    if (maxValue != null && selectedDuration.value > maxValue) {
      selectedDuration.value = maxValue
      selectedUnitValues.value = getUnitValues(selectedUnit.value, selectedDuration.value)
    } else {
      selectedUnitValues.value = getUnitValues(selectedUnit.value, selectedDuration.value)
      selection.value = selectedUnit.value.toSeconds * selectedDuration.value
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
  Dialog(onDismissRequest = cancel) {
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
              painterResource(R.drawable.ic_close),
              generalGetString(R.string.icon_descr_close_button),
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
