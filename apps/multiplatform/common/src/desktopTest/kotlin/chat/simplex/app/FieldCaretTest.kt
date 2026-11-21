package chat.simplex.app

import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.Paragraph
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.createFontFamilyResolver
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.layOutDesktopText
import chat.simplex.common.ui.theme.Inter
import kotlin.test.Test
import kotlin.test.assertTrue

class FieldCaretTest {
  private val density = Density(1f)

  private fun caretHeight(text: String, style: TextStyle): Float {
    val laidOut = layOutDesktopText(AnnotatedString(text), style, density, field = true)
    val paragraph = Paragraph(
      laidOut.text.text, laidOut.style, Constraints(maxWidth = 400), density, createFontFamilyResolver(),
      spanStyles = laidOut.text.spanStyles
    )
    return paragraph.getCursorRect(text.length).height
  }

  @Test
  fun caretKeepsFontHeightAfterOneCharacter() {
    for (style in listOf(TextStyle(fontFamily = Inter, fontSize = 16.sp), TextStyle(fontFamily = Inter, fontSize = 16.sp, lineHeight = 22.sp))) {
      for (text in listOf("a", "Hello\nW")) {
        val height = caretHeight(text, style)
        assertTrue(height >= 16f, "caret height $height for \"$text\" with $style")
      }
    }
  }
}
