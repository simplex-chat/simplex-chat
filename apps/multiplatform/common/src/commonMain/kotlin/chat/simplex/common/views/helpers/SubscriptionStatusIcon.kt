package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.Box
import androidx.compose.material.Icon
import androidx.compose.ui.graphics.Color
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun SubscriptionStatusIcon(
  color: Color,
  variableValue: Float,
  modifier: Modifier = Modifier
) {
  @Composable
  fun ZeroIcon() {
    Icon(painterResource(MR.images.ic_radiowaves_up_forward_4_bar), null, tint = color.copy(alpha = 0.33f), modifier = modifier)
  }

  when {
    variableValue <= 0f -> ZeroIcon()
    variableValue > 0f && variableValue <= 0.25f -> Box {
      ZeroIcon()
      Icon(painterResource(MR.images.ic_radiowaves_up_forward_1_bar), null, tint = color, modifier = modifier)
    }

    variableValue > 0.25f && variableValue <= 0.5f -> Box {
      ZeroIcon()
      Icon(painterResource(MR.images.ic_radiowaves_up_forward_2_bar), null, tint = color, modifier = modifier)
    }

    variableValue > 0.5f && variableValue <= 0.75f -> Box {
      ZeroIcon()
      Icon(painterResource(MR.images.ic_radiowaves_up_forward_3_bar), null, tint = color, modifier = modifier)
    }

    else -> Icon(painterResource(MR.images.ic_radiowaves_up_forward_4_bar), null, tint = color, modifier = modifier)
  }
}
