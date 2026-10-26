package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.Paragraph
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.createFontFamilyResolver
import androidx.compose.ui.text.style.TextDirection
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.Inter
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.item.LocalTerminalLayout
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.chat.item.reserveSpaceForMeta
import chat.simplex.common.views.chat.item.terminalReserveSpaceForMeta
import kotlinx.datetime.Clock
import kotlin.math.abs
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TerminalMetaCopyTest {
  private val resolver = createFontFamilyResolver()
  private val iconSpace = " \u00A0\u00A0\u00A0"
  private val sentEdited = CIMeta.getSample(1, Clock.System.now(), "", status = CIStatus.SndSent(SndCIStatusProgress.Complete), itemEdited = true)
  private val receivedTimed = CIMeta.getSample(2, Clock.System.now(), "", status = CIStatus.RcvRead(), itemTimed = CITimed(30, null))
  private val sentViaProxy = CIMeta.getSample(3, Clock.System.now(), "", status = CIStatus.SndSent(SndCIStatusProgress.Complete), sentViaProxy = true)

  private fun copy(meta: CIMeta, density: Density, direction: LayoutDirection, showViaProxy: Boolean = false) =
    terminalReserveSpaceForMeta(
      meta, null, null, null, Color.Gray, showStatus = true, showEdited = true, showViaProxy = showViaProxy, showTimestamp = true,
      density = density, bodyFontSize = 16.sp, direction = direction
    )

  private fun paragraph(text: AnnotatedString, density: Density, textDirection: TextDirection, width: Int) =
    Paragraph(
      text = text.text,
      style = TextStyle(fontFamily = Inter, fontSize = 16.sp, textDirection = textDirection),
      constraints = Constraints(maxWidth = width),
      density = density,
      fontFamilyResolver = resolver,
      spanStyles = text.spanStyles
    )

  private fun Paragraph.bounds(range: IntRange): Rect =
    range.map { getBoundingBox(it) }.reduce { a, b -> Rect(minOf(a.left, b.left), minOf(a.top, b.top), maxOf(a.right, b.right), maxOf(a.bottom, b.bottom)) }

  @Test
  fun classicReserveUnchanged() {
    assertEquals(iconSpace + iconSpace + "\u00A0" + sentEdited.timestampText, reserveSpaceForMeta(sentEdited, null, null, Color.Gray, showTimestamp = true))
    assertEquals(iconSpace + shortTimeText(30) + "\u00A0" + iconSpace + "\u00A0" + receivedTimed.timestampText, reserveSpaceForMeta(receivedTimed, null, null, Color.Gray, showTimestamp = true))
    assertEquals(iconSpace + iconSpace + iconSpace, reserveSpaceForMeta(sentEdited, null, null, Color.Gray, showTimestamp = false))
    assertEquals(iconSpace + iconSpace + "\u00A0" + sentViaProxy.timestampText, reserveSpaceForMeta(sentViaProxy, null, null, Color.Gray, showViaProxy = true, showTimestamp = true))
  }

  @Test
  fun copyHasClassicSpacesAndDirectionMark() {
    val ltr = copy(sentEdited, Density(1f), LayoutDirection.Ltr)
    assertEquals("\u2060\u200E" + (iconSpace + iconSpace + "\u00A0" + "\u00A0" + sentEdited.timestampText).replace(' ', '\u00A0'), ltr.text)
    val rtl = copy(receivedTimed, Density(1f), LayoutDirection.Rtl)
    assertEquals("\u2060\u200F" + (iconSpace + shortTimeText(30) + "\u00A0" + iconSpace + "\u00A0" + receivedTimed.timestampText).replace(' ', '\u00A0'), rtl.text)
    val proxy = copy(sentViaProxy, Density(1f), LayoutDirection.Ltr, showViaProxy = true)
    assertEquals("\u2060\u200E" + (iconSpace + "\u00A0" + iconSpace + "\u00A0" + "\u00A0" + sentViaProxy.timestampText).replace(' ', '\u00A0'), proxy.text)
  }

  @Test
  fun iconSpacesKeepWidthAcrossFontScales() {
    val ts = sentEdited.timestampText
    val widths = listOf(0.7f, 0.75f, 1f, 1.3f).associateWith { s ->
      val density = Density(1f, s)
      val text = copy(sentEdited, density, LayoutDirection.Ltr)
      val p = paragraph(text, density, TextDirection.Ltr, 1000)
      val timeStart = text.length - ts.length
      assertTrue(p.getHorizontalPosition(2, true) < 0.5f, "marks at scale $s: ${p.getHorizontalPosition(2, true)} px")
      val icons = p.getHorizontalPosition(timeStart, true) - p.getHorizontalPosition(2, true)
      val time = p.getHorizontalPosition(text.length, true) - p.getHorizontalPosition(timeStart, true)
      icons to time
    }
    val (icons1, time1) = widths.getValue(1f)
    for ((s, w) in widths) {
      assertTrue(abs(w.first - icons1) < 0.5f, "icon spaces at scale $s: ${w.first} px, at scale 1: $icons1 px")
      assertTrue(abs(w.second / time1 - s) < 0.05f, "time at scale $s: ${w.second} px, at scale 1: $time1 px")
    }
  }

  @Test
  fun ruleOnlyMessageAsWideAsCopy() {
    val placed = mutableMapOf<String, Placed>()
    renderScene {
      SimpleXTheme {
        CompositionLocalProvider(LocalTerminalLayout provides true, LocalDensity provides Density(1f)) {
          Box(Modifier.width(IntrinsicSize.Max)) {
            MarkdownText(
              "---", listOf(FormattedText("---")), listOf(MarkdownBlock.Rule(SegmentRange(0, 1))), meta = sentEdited,
              toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION, blockLayout = true, modifier = Modifier.record(placed, "rule")
            )
          }
        }
      }
    }
    val copyWidth = paragraph(copy(sentEdited, Density(1f), LayoutDirection.Ltr), Density(1f), TextDirection.Ltr, 1000).maxIntrinsicWidth
    assertTrue(abs(placed.getValue("rule").width - copyWidth) <= 1f, "rule ${placed.getValue("rule").width} px, copy $copyWidth px")
  }

  @Test
  fun copyAfterSpacesStaysWithLastWord() {
    val density = Density(1f)
    val head = "The bot replied at once"
    val text = buildAnnotatedString {
      append("$head   ")
      append(copy(sentEdited, density, LayoutDirection.Ltr))
    }
    val oneLine = paragraph(text, density, TextDirection.Ltr, 10000)
    val width = (oneLine.getHorizontalPosition(head.length + 3, true) + oneLine.maxIntrinsicWidth) / 2
    val p = paragraph(text, density, TextDirection.Ltr, width.toInt())
    assertEquals(2, p.lineCount)
    assertEquals(p.getLineForOffset(head.lastIndexOf(' ') + 1), p.getLineForOffset(text.length - 1))
  }

  @Test
  fun copyStaysAtLineEndAfterWordInOtherDirection() {
    val cases = listOf(
      "שלום לכולם, זו הפסקה האחרונה של ההודעה" to LayoutDirection.Ltr,
      "This is the last paragraph of the message" to LayoutDirection.Rtl,
    )
    for ((head, direction) in cases) {
      val density = Density(1f)
      val text = buildAnnotatedString {
        append(head)
        append(copy(sentEdited, density, direction))
      }
      val p = paragraph(text, density, if (direction == LayoutDirection.Ltr) TextDirection.Ltr else TextDirection.Rtl, 230)
      val lastLine = p.lineCount - 1
      val lineStart = p.getLineStart(lastLine)
      assertTrue(lineStart in 1 until head.length, "$direction: last line start $lineStart")
      assertEquals(lastLine, p.getLineForOffset(text.length - 1), "$direction: time line")
      val words = p.bounds(lineStart until head.length)
      val time = p.bounds(head.length + 2 until text.length)
      if (direction == LayoutDirection.Ltr) {
        assertTrue(time.left >= words.right - 0.5f, "$direction: copy ${time.left}..${time.right}, words ${words.left}..${words.right}")
      } else {
        assertTrue(time.right <= words.left + 0.5f, "$direction: copy ${time.left}..${time.right}, words ${words.left}..${words.right}")
      }
    }
  }
}
