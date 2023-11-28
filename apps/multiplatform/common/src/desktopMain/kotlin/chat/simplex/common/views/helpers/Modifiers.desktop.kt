package chat.simplex.common.views.helpers

import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier

@Composable
actual fun SwipeToDismissModifier(
  state: DismissState,
  directions: Set<DismissDirection>,
  swipeDistance: Float,
): Modifier {
  return Modifier
}
