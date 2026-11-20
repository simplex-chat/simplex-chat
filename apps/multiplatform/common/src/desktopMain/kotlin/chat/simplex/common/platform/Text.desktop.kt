package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.input.TransformedText
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.platform.Font
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.isSpecified
import androidx.compose.ui.unit.sp
import androidx.compose.ui.unit.takeOrElse
import chat.simplex.common.ui.theme.EmojiFont
import chat.simplex.common.ui.theme.Inter
import chat.simplex.res.*
import org.jetbrains.skia.FontMgr
import org.jetbrains.skia.FontStyle
import org.jetbrains.skia.Typeface
import org.jetbrains.skia.paragraph.FontCollection
import org.jetbrains.skia.paragraph.ParagraphBuilder
import org.jetbrains.skia.paragraph.ParagraphStyle
import org.jetbrains.skia.paragraph.TypefaceFontProvider
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.util.concurrent.ConcurrentHashMap
import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.max
import kotlin.math.roundToInt
import org.jetbrains.skia.Font as SkFont
import org.jetbrains.skia.paragraph.TextStyle as SkTextStyle

@Composable
actual fun platformText(text: AnnotatedString, style: TextStyle): PlatformText {
  val density = LocalDensity.current
  return remember(text, style, density) { layOutDesktopText(text, style, density) }
}

@Composable
actual fun platformTextFieldStyle(text: String, style: TextStyle, visualTransformation: VisualTransformation): PlatformTextFieldStyle {
  val density = LocalDensity.current
  return remember(text, style, visualTransformation, density) {
    val shown = visualTransformation.filter(AnnotatedString(text)).text
    PlatformTextFieldStyle(
      layOutDesktopText(shown, style, density).style,
      VisualTransformation { input ->
        val transformed = visualTransformation.filter(input)
        TransformedText(layOutDesktopText(transformed.text, style, density).text, transformed.offsetMapping)
      }
    )
  }
}

private const val MARKER_HEIGHT_PX = 0.52f
private const val MARKER_PX = 0.2f
private val DEFAULT_FONT_SIZE = 14.sp

fun layOutDesktopText(text: AnnotatedString, style: TextStyle, density: Density): PlatformText {
  val s = text.text
  val effective = text.spanStyles.filter { it.start == 0 }.fold(style) { acc, range -> acc.merge(range.item) }
  val fontSizePx = with(density) { effective.fontSize.takeOrElse { DEFAULT_FONT_SIZE }.toPx() }
  val lineHeightPx = if (style.lineHeight.isSp) with(density) { style.lineHeight.toPx() } else null
  val typeface = typefaceOf(effective.fontFamily)
  val lineHeightStyle = style.lineHeightStyle ?: LineHeightStyle.Default
  val trimBoth = lineHeightStyle.trim == LineHeightStyle.Trim.Both
  val proportional = lineHeightStyle.alignment == LineHeightStyle.Alignment.Proportional
  val centered = lineHeightStyle.alignment == LineHeightStyle.Alignment.Center && lineHeightStyle.trim == LineHeightStyle.Trim.None
  val hasEmptyLine = s.isEmpty() || s.startsWith('\n') || s.endsWith('\n') || s.contains("\n\n")
  val marker = when {
    hasEmptyLine -> null
    lineHeightPx == null -> SpanStyle(fontSize = with(density) { MARKER_PX.toSp() })
    typeface == null -> null
    proportional && (lineHeightPx > fontSizePx || trimBoth) -> SpanStyle(fontSize = with(density) { (MARKER_HEIGHT_PX / heightPerPx(typeface)).toSp() })
    centered -> centeredMarker(text, effective, typeface, fontSizePx, lineHeightPx, density)
    else -> null
  }
  val builder = AnnotatedString.Builder(text)
  var laidOutStyle = style
  if (marker != null) {
    builder.addStyle(marker, 0, 0)
  } else {
    builder.addStyle(SpanStyle(), 0, 0)
    if (s.isEmpty()) {
      if (lineHeightPx != null && !proportional) {
        laidOutStyle = style.copy(lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Proportional, lineHeightStyle.trim))
      }
    } else if (lineHeightPx != null && typeface != null) {
      val compensated = compensatedLineHeightPx(lineHeightPx, fontSizePx, typeface, style.lineHeightStyle)
      laidOutStyle = style.copy(lineHeight = with(density) { compensated.toSp() })
    }
  }
  if (desktopPlatform.isMac() || desktopPlatform.isWindows()) addFallbackStyles(builder, text, style, density)
  return PlatformText(builder.toAnnotatedString(), laidOutStyle)
}

private fun centeredMarker(text: AnnotatedString, effective: TextStyle, typeface: Typeface, fontSizePx: Float, lineHeightPx: Float, density: Density): SpanStyle? {
  val dominant = dominantTypeface(text.text, typeface)
  if (SkFont(dominant, fontSizePx).use { it.metrics.leading } > 0f) return null
  val exactHeight = heightPerPx(dominant) * fontSizePx
  val fraction = exactHeight - floor(exactHeight)
  val markerPx = if (fraction >= 0.5f || fraction < 0.05f) fontSizePx else fontSizePx * (ceil(exactHeight) - 0.49f) / exactHeight
  if (lineHeightPx <= markerPx) return null
  val family = when {
    dominant.uniqueId == typeface.uniqueId -> effective.fontFamily
    desktopPlatform.isWindows() && dominant.familyName == WINDOWS_EMOJI_FAMILY -> flatEmojiFamily
    else -> fontFamilyOf(dominant)
  }
  return SpanStyle(fontSize = with(density) { markerPx.toSp() }, fontFamily = family)
}

private fun dominantTypeface(s: String, typeface: Typeface): Typeface {
  val counts = HashMap<Int, Int>()
  val typefaces = HashMap<Int, Typeface>()
  var i = 0
  while (i < s.length) {
    val cp = s.codePointAt(i)
    i += Character.charCount(cp)
    if (Character.isWhitespace(cp) || isEmojiJoiner(cp)) continue
    val run = if (covers(typeface, cp)) typeface else fallbackTypeface(cp) ?: continue
    typefaces[run.uniqueId] = run
    counts[run.uniqueId] = (counts[run.uniqueId] ?: 0) + 1
  }
  return counts.maxByOrNull { it.value }?.let { typefaces[it.key] } ?: typeface
}

private val fontFamilies = ConcurrentHashMap<Int, FontFamily>()

private fun fontFamilyOf(typeface: Typeface): FontFamily =
  fontFamilies.getOrPut(typeface.uniqueId) { FontFamily(androidx.compose.ui.text.platform.Typeface(typeface)) }

private fun compensatedLineHeightPx(lineHeightPx: Float, fontSizePx: Float, typeface: Typeface, lineHeightStyle: LineHeightStyle?): Float {
  val metrics = SkFont(typeface, fontSizePx).use { it.metrics }
  val fontHeight = metrics.descent - metrics.ascent + metrics.leading
  val roundedFontHeight = fontHeight.roundToInt()
  if (roundedFontHeight >= fontHeight) return lineHeightPx
  val adjustedPx = when ((lineHeightStyle ?: LineHeightStyle.Default).alignment) {
    LineHeightStyle.Alignment.Top, LineHeightStyle.Alignment.Center, LineHeightStyle.Alignment.Bottom ->
      lineHeightPx - fontHeight + roundedFontHeight
    else -> (lineHeightPx - metrics.leading) * roundedFontHeight / (metrics.descent - metrics.ascent)
  }
  return if (lineHeightPx > fontSizePx) max(adjustedPx, fontSizePx * 1.0001f) else adjustedPx
}

private fun addFallbackStyles(builder: AnnotatedString.Builder, text: AnnotatedString, style: TextStyle, density: Density) {
  val s = text.text
  val bounds = (text.spanStyles.flatMap { listOf(it.start, it.end) } + listOf(0, s.length)).filter { it in 0..s.length }.distinct().sorted()
  var emojiStart = -1
  var trackStart = -1
  var trackPx = 0f
  fun endTracking(at: Int) {
    if (trackStart >= 0) builder.addStyle(SpanStyle(letterSpacing = with(density) { trackPx.toSp() }), trackStart, at)
    trackStart = -1
  }
  fun endEmoji(at: Int) {
    val family = flatEmojiFamily
    if (emojiStart >= 0 && family != null) builder.addStyle(SpanStyle(fontFamily = family), emojiStart, at)
    emojiStart = -1
  }
  for (k in 0 until bounds.size - 1) {
    val segStart = bounds[k]
    val segEnd = bounds[k + 1]
    if (segStart == segEnd) continue
    val segStyle = text.spanStyles.filter { it.start <= segStart && it.end >= segEnd && it.start < it.end }.fold(style) { acc, range -> acc.merge(range.item) }
    val segTypeface = typefaceOf(segStyle.fontFamily)
    val fontSizePx = with(density) { segStyle.fontSize.takeOrElse { DEFAULT_FONT_SIZE }.toPx() }
    val letterSpacingPx = with(density) {
      when {
        segStyle.letterSpacing.isSp -> segStyle.letterSpacing.toPx()
        segStyle.letterSpacing.isEm -> segStyle.letterSpacing.value * fontSizePx
        else -> 0f
      }
    }
    var i = segStart
    while (i < segEnd) {
      val cp = s.codePointAt(i)
      val n = Character.charCount(cp)
      val covered = segTypeface != null && covers(segTypeface, cp)
      val runTypeface = if (covered) segTypeface else if (segTypeface == null) null else fallbackTypeface(cp)
      if (desktopPlatform.isWindows()) {
        val emoji = if (isEmojiJoiner(cp)) emojiStart >= 0 else !covered && runTypeface?.familyName == WINDOWS_EMOJI_FAMILY
        if (emoji && emojiStart < 0) emojiStart = i
        if (!emoji) endEmoji(i)
      } else {
        val tracking = if (runTypeface != null && hasTracking(runTypeface)) trackingPx(runTypeface, cp, fontSizePx) else 0f
        if (tracking != 0f) {
          val spacing = letterSpacingPx - tracking
          if (trackStart >= 0 && spacing != trackPx) endTracking(i)
          if (trackStart < 0) { trackStart = i; trackPx = spacing }
        } else {
          endTracking(i)
        }
      }
      i += n
    }
    endTracking(segEnd)
  }
  endEmoji(s.length)
}

private fun isEmojiJoiner(cp: Int) = cp == 0x200D || cp in 0xFE00..0xFE0F || cp == 0x20E3 || cp in 0xE0020..0xE007F

private const val WINDOWS_EMOJI_FAMILY = "Segoe UI Emoji"

private val coverage = ConcurrentHashMap<Long, Boolean>()

private fun covers(typeface: Typeface, cp: Int): Boolean =
  coverage.getOrPut((typeface.uniqueId.toLong() shl 32) or cp.toLong()) { typeface.getUTF32Glyph(cp).toInt() != 0 }

private val fallbackTypefaces = ConcurrentHashMap<Int, Any>()

private fun fallbackTypeface(cp: Int): Typeface? =
  fallbackTypefaces.getOrPut(cp) { FontMgr.default.matchFamilyStyleCharacter(null, FontStyle.NORMAL, null, cp) ?: Unit } as? Typeface

private val trackingTables = ConcurrentHashMap<Int, Boolean>()

private fun hasTracking(typeface: Typeface): Boolean =
  trackingTables.getOrPut(typeface.uniqueId) { typeface.tableTags.contains("trak") }

private val trackings = ConcurrentHashMap<Pair<Int, Int>, Float>()

private fun trackingPx(typeface: Typeface, cp: Int, fontSizePx: Float): Float =
  trackings.getOrPut(typeface.uniqueId to (fontSizePx * 100).roundToInt()) {
    val provider = TypefaceFontProvider().apply { registerTypeface(typeface, "tracking") }
    val fonts = FontCollection().apply { setAssetFontManager(provider) }
    val textStyle = SkTextStyle().setFontSize(fontSizePx).setFontFamilies(arrayOf("tracking"))
    val paragraphStyle = ParagraphStyle().apply { this.textStyle = textStyle }
    val sample = String(Character.toChars(cp)).repeat(4)
    val paragraph = ParagraphBuilder(paragraphStyle, fonts).apply { pushStyle(textStyle); addText(sample) }.build().apply { layout(Float.MAX_VALUE) }
    val advance = SkFont(typeface, fontSizePx).use { it.getWidths(shortArrayOf(typeface.getUTF32Glyph(cp)))[0] }
    paragraph.maxIntrinsicWidth / 4 - advance
  }

private val flatEmojiFamily: FontFamily? by lazy {
  colrVersion0Copy(WINDOWS_EMOJI_FAMILY)?.let { FontFamily(Font("$WINDOWS_EMOJI_FAMILY COLR v0", it)) }
}

private fun colrVersion0Copy(family: String): ByteArray? {
  val typeface = FontMgr.default.matchFamilyStyle(family, FontStyle.NORMAL) ?: return null
  val tables = typeface.tableTags.sorted().map { tag ->
    val data = typeface.getTableData(tag)?.bytes ?: return null
    if (tag == "COLR" && data.size >= 2) {
      data[0] = 0
      data[1] = 0
    }
    tag to data
  }
  val out = ByteArrayOutputStream()
  val d = DataOutputStream(out)
  var searchRange = 1
  var entrySelector = 0
  while (searchRange * 2 <= tables.size) {
    searchRange *= 2
    entrySelector++
  }
  d.writeInt(0x00010000)
  d.writeShort(tables.size)
  d.writeShort(searchRange * 16)
  d.writeShort(entrySelector)
  d.writeShort(tables.size * 16 - searchRange * 16)
  var offset = 12 + tables.size * 16
  for ((tag, data) in tables) {
    val padded = data.copyOf((data.size + 3) / 4 * 4)
    var checksum = 0L
    for (i in padded.indices step 4) {
      val word = ((padded[i].toLong() and 255) shl 24) or ((padded[i + 1].toLong() and 255) shl 16) or ((padded[i + 2].toLong() and 255) shl 8) or (padded[i + 3].toLong() and 255)
      checksum = (checksum + word) and 0xffffffffL
    }
    d.write(tag.toByteArray(Charsets.US_ASCII))
    d.writeInt(checksum.toInt())
    d.writeInt(offset)
    d.writeInt(data.size)
    offset += padded.size
  }
  for ((_, data) in tables) {
    d.write(data)
    repeat((4 - data.size % 4) % 4) { d.write(0) }
  }
  return out.toByteArray()
}

private fun heightPerPx(typeface: Typeface): Float =
  SkFont(typeface, 100f).use { it.metrics }.let { (it.descent - it.ascent + it.leading) / 100f }

private val interTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MR.fonts.inter_regular.file.absolutePath) }

private val emojiTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MRdesktopMain.fonts.notocoloremoji_regular.file.absolutePath) }

private val monospaceTypeface: Typeface? by lazy {
  firstInstalledTypeface(
    linux = listOf("Noto Sans Mono", "DejaVu Sans Mono", "Consolas"),
    windows = listOf("Consolas"),
    mac = listOf(".AppleSystemUIFontMonospaced", "Menlo", "Courier")
  )
}

private val sansSerifTypeface: Typeface? by lazy {
  firstInstalledTypeface(
    linux = listOf("Noto Sans", "DejaVu Sans", "Arial"),
    windows = listOf("Segoe UI", "Arial"),
    mac = listOf(".AppleSystemUIFont", "Helvetica Neue", "Helvetica")
  )
}

private fun firstInstalledTypeface(linux: List<String>, windows: List<String>, mac: List<String>): Typeface? {
  val families = when {
    desktopPlatform.isLinux() -> linux
    desktopPlatform.isWindows() -> windows
    else -> mac
  }
  return families.firstNotNullOfOrNull { family ->
    FontMgr.default.matchFamily(family).use { set -> if (set.count() > 0) set.matchStyle(FontStyle.NORMAL) else null }
  }
}

private fun typefaceOf(family: FontFamily?): Typeface? = when (family) {
  null, Inter -> interTypeface
  FontFamily.Monospace -> monospaceTypeface
  FontFamily.Default, FontFamily.SansSerif -> sansSerifTypeface
  EmojiFont -> emojiTypeface
  else -> null
}
