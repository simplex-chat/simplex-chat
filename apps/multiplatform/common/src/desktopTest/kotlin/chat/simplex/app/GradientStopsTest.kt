package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.size
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.lightStops
import org.jetbrains.skia.Bitmap
import kotlin.math.abs
import kotlin.math.roundToInt
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

private const val GRADIENT_WIDTH = 400
private const val GRADIENT_HEIGHT = 40
private const val CHANNEL_TOLERANCE = 0.03f

class GradientStopsTest {
  @Test
  fun testDesktopGradientStopsAreSrgb() {
    for ((position, color) in lightStops + darkStops) {
      assertEquals(ColorSpaces.Srgb, color.colorSpace, "stop at $position")
    }
  }

  @Test
  fun testLightGradientShowsItsStopColors() {
    assertStopColorsRendered(lightStops)
  }

  @Test
  fun testDarkGradientShowsItsStopColors() {
    assertStopColorsRendered(darkStops)
  }

  private fun assertStopColorsRendered(stops: Array<Pair<Float, Color>>) {
    val bitmap = Bitmap.makeFromImage(renderScene(width = GRADIENT_WIDTH, height = GRADIENT_HEIGHT) {
      Box(Modifier.size(GRADIENT_WIDTH.dp, GRADIENT_HEIGHT.dp).background(Brush.horizontalGradient(*stops)))
    })
    for ((position, color) in stops) {
      val x = (position * (GRADIENT_WIDTH - 1)).roundToInt()
      val pixel = Color(bitmap.getColor(x, GRADIENT_HEIGHT / 2))
      val channels = listOf(color.red to pixel.red, color.green to pixel.green, color.blue to pixel.blue)
      assertTrue(
        channels.all { (expected, rendered) -> abs(expected - rendered) <= CHANNEL_TOLERANCE },
        "stop at $position: expected $color, rendered $pixel at x $x"
      )
    }
  }
}
