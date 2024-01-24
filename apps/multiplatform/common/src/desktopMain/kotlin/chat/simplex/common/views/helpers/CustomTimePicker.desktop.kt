package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import chat.simplex.common.model.CustomTimeUnit
import chat.simplex.common.ui.theme.DEFAULT_PADDING

@Composable
actual fun CustomTimePicker(
  selection: MutableState<Int>,
  timeUnitsLimits: List<TimeUnitLimits>
) {
  val unit = remember {
    var res: CustomTimeUnit = CustomTimeUnit.Second
    val found = timeUnitsLimits.asReversed().any {
      if (selection.value >= it.minValue * it.timeUnit.toSeconds && selection.value <= it.maxValue * it.timeUnit.toSeconds) {
        res = it.timeUnit
        selection.value = (selection.value / it.timeUnit.toSeconds).coerceIn(it.minValue, it.maxValue) * it.timeUnit.toSeconds
        true
      } else {
        false
      }
    }
    if (!found) {
      // If custom interval doesn't fit in any category, set it to 1 second interval
      selection.value = 1
    }
    mutableStateOf(res)
  }
  val values = remember(unit.value) {
    val limit = timeUnitsLimits.first { it.timeUnit == unit.value }
    val res = ArrayList<Pair<Int, String>>()
    for (i in limit.minValue..limit.maxValue) {
      val seconds = i * limit.timeUnit.toSeconds
      val desc = i.toString()
      res.add(seconds to desc)
    }
    if (res.none { it.first == selection.value }) {
      // Doesn't fit into min..max, put it equal to the closest value
      selection.value = selection.value.coerceIn(res.first().first, res.last().first)
      //selection.value = res.last { it.first <= selection.value }.first
    }
    res
  }
  val units = remember {
    val res = ArrayList<Pair<CustomTimeUnit, String>>()
    for (unit in timeUnitsLimits) {
      res.add(unit.timeUnit to unit.timeUnit.text)
    }
    res
  }

  Row(
    Modifier.padding(bottom = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceEvenly
  ) {
    ExposedDropDownSetting(
      values,
      selection,
      textColor = MaterialTheme.colors.onBackground,
      enabled = remember { mutableStateOf(true) },
      onSelected = { selection.value = it }
    )
    Spacer(Modifier.width(DEFAULT_PADDING))
    ExposedDropDownSetting(
      units,
      unit,
      textColor = MaterialTheme.colors.onBackground,
      enabled = remember { mutableStateOf(true) },
      onSelected = {
        selection.value = selection.value / unit.value.toSeconds * it.toSeconds
        unit.value = it
      }
    )
  }
}
