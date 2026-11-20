package chat.simplex.app

import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.layOutDesktopText
import chat.simplex.common.ui.theme.Inter
import kotlin.test.Test
import kotlin.test.assertTrue
import kotlin.time.measureTime

class FallbackSearchTest {
  @Test
  fun longGraphemeResolvesInLinearTime() {
    val text = "a" + Char(0x0301).toString().repeat(20_000) + String(Character.toChars(0x1E944))
    val style = TextStyle(
      fontFamily = Inter, fontSize = 16.sp, lineHeight = 22.sp,
      lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Center, LineHeightStyle.Trim.None)
    )
    val elapsed = measureTime { layOutDesktopText(AnnotatedString(text), style, Density(1f), field = false) }
    assertTrue(elapsed.inWholeSeconds < 10, "layout took $elapsed")
  }
}
