package chat.simplex.app.views.helpers

import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.layout

fun Modifier.badgeLayout() =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)

    // based on the expectation of only one line of text
    val minPadding = placeable.height / 4

    val width = maxOf(placeable.width + minPadding, placeable.height)
    layout(width, placeable.height) {
      placeable.place((width - placeable.width) / 2, 0)
    }
  }
