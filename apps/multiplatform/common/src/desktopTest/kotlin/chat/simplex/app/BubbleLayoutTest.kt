package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.luminance
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.HighOrLowlight
import chat.simplex.common.ui.theme.Inter
import chat.simplex.common.views.chat.item.*
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import kotlin.math.roundToInt
import kotlin.test.Test
import kotlin.test.assertEquals

class BubbleLayoutTest {
  private val density = Density(1f)
  private val inset = with(density) { BUBBLE_TEXT_INSET.roundToPx() }
  private val placed = mutableMapOf<String, Placed>()

  private fun metaInBubble(metaHeight: Int, metaBaseline: Int?): Placed {
    renderScene {
      Box(Modifier.size(200.dp, 100.dp).record(placed, "bubble"), contentAlignment = Alignment.BottomEnd) {
        Box(Modifier.bubbleMeta()) {
          Box(Modifier.record(placed, "meta").fixed(60, metaHeight, metaBaseline))
        }
      }
    }
    return placed.getValue("meta").relativeTo(placed.getValue("bubble"))
  }

  @Test
  fun timeBaselineInsetAboveBubbleBottom() {
    val meta = metaInBubble(17, 13)
    assertEquals(100 - inset, meta.y + 13)
  }

  @Test
  fun ticksWithoutTimeWhereTheyStandNextToTime() {
    val meta = metaInBubble(17, null)
    val timeBaseline = ((17 + with(density) { 12.sp.toPx() } * INTER_CAP_HEIGHT) / 2).roundToInt()
    assertEquals(100 - inset, meta.y + timeBaseline)
  }

  @Test
  fun textCapsAndLastBaselineInsetFromEdges() {
    renderScene {
      Box(Modifier.record(placed, "part").bubbleTextPadding(16.sp)) {
        Column(Modifier.record(placed, "text")) {
          Box(Modifier.fixed(100, 22, 18))
          Box(Modifier.fixed(100, 22, 18))
        }
      }
    }
    val part = placed.getValue("part")
    val text = placed.getValue("text").relativeTo(part)
    val firstBaseline = with(density) { (BUBBLE_TEXT_INSET + (16 * INTER_CAP_HEIGHT).sp.toDp()).roundToPx() }
    val side = with(density) { BUBBLE_SIDE_INSET.roundToPx() }
    assertEquals(Placed(side, firstBaseline - 18, 100, 44), text)
    assertEquals(inset, part.height - (text.y + 22 + 18))
    assertEquals(100 + 2 * side, part.width)
  }

  @Test
  fun insetsKeepTheirSizeAtLargerFont() {
    renderScene {
      CompositionLocalProvider(LocalDensity provides Density(1f, 1.5f)) {
        Box(Modifier.record(placed, "part").bubbleTextPadding(16.sp)) {
          Box(Modifier.record(placed, "text").fixed(100, 33, 27))
        }
      }
    }
    val part = placed.getValue("part")
    val text = placed.getValue("text").relativeTo(part)
    assertEquals((11 + 16 * 1.5f * INTER_CAP_HEIGHT).roundToInt(), text.y + 27)
    assertEquals(inset, part.height - (text.y + 27))
  }

  @Test
  fun noBottomInsetBeforeAnotherPartOfTheBand() {
    renderScene {
      Box(Modifier.record(placed, "part").bubbleTextPadding(12.sp, bottom = 0.dp)) {
        Box(Modifier.record(placed, "text").fixed(100, 15, 12))
      }
    }
    val part = placed.getValue("part")
    val text = placed.getValue("text").relativeTo(part)
    assertEquals(text.y + 15, part.height)
  }

  @Test
  fun timeOnTextBaselineInCardRow() {
    renderScene {
      Row(Modifier.record(placed, "row")) {
        Box(Modifier.alignByBaseline().record(placed, "text").fixed(100, 22, 17))
        Box(bubbleMetaOnBaseline()) {
          Box(Modifier.record(placed, "meta").fixed(60, 16, 12))
        }
      }
    }
    val row = placed.getValue("row")
    assertEquals(placed.getValue("text").relativeTo(row).y + 17, placed.getValue("meta").relativeTo(row).y + 12)
  }

  @Test
  fun timeOnLastBaselineAtCardEnd() {
    renderScene {
      Box(Modifier.width(IntrinsicSize.Min)) {
        MetaOnLastBaseline({ Box(Modifier.record(placed, "meta").fixed(60, 16, 12)) }) {
          Column(Modifier.record(placed, "card")) {
            Box(Modifier.fixed(220, 40, 30))
            Box(Modifier.fixed(220, 22, 17))
          }
        }
      }
    }
    assertEquals(Placed(220 - 60, 40 + 17 - 12, 60, 16), placed.getValue("meta").relativeTo(placed.getValue("card")))
  }

  @Test
  fun timeDrawnOnTextBaseline() {
    val texts = listOf<@Composable () -> TextStyle>(
      { TextStyle(fontFamily = Inter, fontSize = 16.sp).merge(bubbleTextStyle(16.sp, HighOrLowlight)) },
      { TextStyle(fontFamily = Inter, fontSize = 13.sp, lineHeight = 18.sp, lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Center, LineHeightStyle.Trim.None)).merge(bubbleLineHeight(18.sp)) },
      { TextStyle(fontFamily = Inter, fontSize = 16.sp).merge(bubbleLineHeight(16.sp * INTER_LINE_HEIGHT)) },
    )
    for (fontScale in listOf(0.7f, 0.8f, 0.9f, 1f, 1.1f, 1.2f, 1.3f, 1.4f, 1.5f)) {
      texts.forEachIndexed { i, textStyle ->
        val image = renderScene {
          CompositionLocalProvider(LocalBubbleLayout provides true, LocalDensity provides Density(1f, fontScale)) {
            Row(Modifier.background(Color.White)) {
              Text(
                buildAnnotatedString { append("11"); withStyle(SpanStyle(fontSize = 12.sp)) { append("11") } },
                Modifier.alignByBaseline().record(placed, "text"),
                Color.Black,
                style = textStyle()
              )
              Text("11", Modifier.alignByBaseline().record(placed, "time"), Color.Black, style = TextStyle(fontFamily = Inter, fontSize = 12.sp).merge(chatTextStyle(12.sp, HighOrLowlight)))
            }
          }
        }
        assertEquals(lastInkRow(image, placed.getValue("text")), lastInkRow(image, placed.getValue("time")), "text $i at font scale $fontScale")
      }
    }
  }

  private fun lastInkRow(image: Image, area: Placed): Int {
    val bitmap = Bitmap.makeFromImage(image)
    return (area.y until area.y + area.height).last { y -> (area.x until area.x + area.width).any { x -> Color(bitmap.getColor(x, y)).luminance() < 0.5f } }
  }

  @Test
  fun fileIconInkBottomOnSizeLineBaseline() {
    renderScene {
      Row(Modifier.record(placed, "row")) {
        Box(fileIconOnBaseline(40.sp).record(placed, "icon").fixed(40, 42, null))
        Box(Modifier.alignBy(LastBaseline).record(placed, "text").fixed(100, 36, 32))
      }
    }
    val row = placed.getValue("row")
    val inkInset = with(density) { (40.sp * FILE_ICON_INK_INSET).toPx().toInt() }
    assertEquals(placed.getValue("text").relativeTo(row).y + 32, placed.getValue("icon").relativeTo(row).y + 42 - inkInset)
  }
}
