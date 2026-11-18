package chat.simplex.common.views.helpers

import androidx.compose.animation.animateColorAsState
import androidx.compose.animation.core.spring
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.HapticPattern
import chat.simplex.common.platform.RichHaptics

@Composable
fun DefaultSwitch(
  checked: Boolean,
  onCheckedChange: ((Boolean) -> Unit)?,
  modifier: Modifier = Modifier,
  enabled: Boolean = true,
  interactionSource: MutableInteractionSource = remember { MutableInteractionSource() },
  colors: SwitchColors = SwitchDefaults.colors(
    checkedThumbColor = MaterialTheme.colors.primary,
    uncheckedThumbColor = MaterialTheme.colors.secondary,
    checkedTrackAlpha = 0.0f,
    uncheckedTrackAlpha = 0.0f,
  )
) {
  val trackColor by animateColorAsState(
    targetValue = if (checked) {
      MaterialTheme.colors.primary.copy(alpha = 0.3f)
    } else {
      MaterialTheme.colors.secondary.copy(alpha = 0.3f)
    },
    animationSpec = spring(dampingRatio = 0.85f, stiffness = 450f)
  )
  val density = LocalDensity.current
  val size = with(density) { Size(40.dp.toPx(), 26.dp.toPx()) }
  val offset = with(density) { Offset(4.dp.toPx(), 11.dp.toPx()) }
  val radius = with(density) { 13.dp.toPx() }

  Switch(
    checked = checked,
    onCheckedChange = onCheckedChange?.let { callback ->
      { newValue ->
        RichHaptics.perform(
          if (newValue) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF
        )
        callback(newValue)
      }
    },
    modifier = modifier.drawBehind {
      drawRoundRect(
        color = trackColor,
        size = size,
        topLeft = offset,
        cornerRadius = CornerRadius(radius, radius)
      )
    },
    colors = colors,
    enabled = enabled,
    interactionSource = interactionSource,
  )
}
