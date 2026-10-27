package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.compose.ui.ImageComposeScene
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.layout
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInRoot
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.use
import org.jetbrains.skia.Image
import kotlin.math.roundToInt

data class Placed(val x: Int, val y: Int, val width: Int, val height: Int)

fun Modifier.record(into: MutableMap<String, Placed>, name: String): Modifier = onGloballyPositioned {
  val p = it.positionInRoot()
  into[name] = Placed(p.x.roundToInt(), p.y.roundToInt(), it.size.width, it.size.height)
}

fun Modifier.fixed(width: Int, height: Int, baseline: Int?): Modifier = layout { measurable, _ ->
  val placeable = measurable.measure(Constraints.fixed(width, height))
  layout(width, height, if (baseline == null) emptyMap() else mapOf(FirstBaseline to baseline, LastBaseline to baseline)) { placeable.place(0, 0) }
}

fun renderScene(direction: LayoutDirection = LayoutDirection.Ltr, width: Int = 400, height: Int = 400, frames: Int = 1, content: @Composable () -> Unit): Image =
  ImageComposeScene(width = width, height = height, density = Density(1f), layoutDirection = direction, content = content).use { scene ->
    (0 until frames).map { scene.render(it * 1_000_000_000L) }.last()
  }

fun Placed.relativeTo(origin: Placed) = Placed(x - origin.x, y - origin.y, width, height)
