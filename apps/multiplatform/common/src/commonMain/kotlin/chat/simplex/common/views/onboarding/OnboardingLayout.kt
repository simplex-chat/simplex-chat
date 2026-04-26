package chat.simplex.common.views.onboarding

import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.*
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp

/**
 * A layout for onboarding screens: image + content + spacer + button.
 * The spacer shrinks first (down to [minSpacerHeight]), then the image shrinks.
 * Button is always at the bottom.
 */
@Composable
fun OnboardingShrinkingLayout(
    modifier: Modifier = Modifier,
    topPadding: Dp = 0.dp,
    minSpacerHeight: Dp = 20.dp,
    image: @Composable () -> Unit,
    content: @Composable () -> Unit,
    button: @Composable () -> Unit
) {
    Layout(
        contents = listOf(image, content, button),
        modifier = modifier
    ) { (imageMeasurables, contentMeasurables, buttonMeasurables), constraints ->
        val width = constraints.maxWidth
        val height = constraints.maxHeight
        val childConstraints = constraints.copy(minWidth = 0, minHeight = 0)

        // 1. Measure fixed content (texts) and button first
        val contentPlaceable = contentMeasurables.first().measure(childConstraints)
        val buttonPlaceable = buttonMeasurables.first().measure(childConstraints)
        val minSpacer = minSpacerHeight.roundToPx()

        // 2. Image gets remaining after top padding + content + button + minimum spacer
        val topPad = topPadding.roundToPx()
        val reservedHeight = topPad + contentPlaceable.height + buttonPlaceable.height + minSpacer
        val imageMaxHeight = (height - reservedHeight).coerceAtLeast(0)
        val imagePlaceable = imageMeasurables.first().measure(
            childConstraints.copy(maxWidth = width, maxHeight = imageMaxHeight)
        )

        // 3. Spacer fills whatever is left between content and button
        val usedHeight = topPad + imagePlaceable.height + contentPlaceable.height + buttonPlaceable.height
        val spacerHeight = (height - usedHeight).coerceAtLeast(minSpacer)

        // 4. Place: image centered horizontally, rest below
        layout(width, height) {
            var y = topPad
            imagePlaceable.placeRelative((width - imagePlaceable.width) / 2, y)
            y += imagePlaceable.height
            contentPlaceable.placeRelative((width - contentPlaceable.width) / 2, y)
            y += contentPlaceable.height
            y += spacerHeight
            buttonPlaceable.placeRelative((width - buttonPlaceable.width) / 2, y)
        }
    }
}
