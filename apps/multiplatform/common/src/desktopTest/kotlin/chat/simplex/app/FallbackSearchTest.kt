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
  private val style = TextStyle(
    fontFamily = Inter, fontSize = 16.sp, lineHeight = 22.sp,
    lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Center, LineHeightStyle.Trim.None)
  )

  @Test
  fun longGraphemeResolvesInLinearTime() {
    val text = "a" + Char(0x0301).toString().repeat(20_000) + String(Character.toChars(0x1E944))
    val elapsed = measureTime { layOutDesktopText(AnnotatedString(text), style, Density(1f), field = false) }
    assertTrue(elapsed.inWholeSeconds < 10, "layout took $elapsed")
  }

  @Test
  fun missingCharactersQueryBounded() {
    val text = (0xF0000 until 0xF0000 + 6000).joinToString("") { String(Character.toChars(it)) }
    val first = measureTime { layOutDesktopText(AnnotatedString(text), style, Density(1f), field = false) }
    assertTrue(first.inWholeMilliseconds < 3000, "first layout took $first")
    val second = measureTime { layOutDesktopText(AnnotatedString(text), style, Density(1f), field = false) }
    assertTrue(second.inWholeMilliseconds < 1000, "second layout took $second")
  }
}
