package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TransformedText
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.platform.Font
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.EmojiFont
import chat.simplex.common.ui.theme.Inter
import chat.simplex.res.*
import org.jetbrains.skia.FontMgr
import org.jetbrains.skia.FontSlant
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
import androidx.compose.ui.text.font.FontStyle as ComposeFontStyle
import org.jetbrains.skia.Font as SkFont
import org.jetbrains.skia.paragraph.TextStyle as SkTextStyle

@Composable
actual fun platformText(text: AnnotatedString, style: TextStyle): PlatformText {
  val density = LocalDensity.current
  return remember(text, style, density) { layOutDesktopText(text, style, density, field = false) }
}

@Composable
actual fun platformTextFieldStyle(text: String, style: TextStyle, visualTransformation: VisualTransformation): PlatformTextFieldStyle {
  val density = LocalDensity.current
  val fieldTransformation = remember(style, visualTransformation, density) {
    VisualTransformation { input ->
      val transformed = visualTransformation.filter(input)
      TransformedText(layOutDesktopText(transformed.text, style, density, field = true).text, transformed.offsetMapping)
    }
  }
  val fieldStyle = remember(text, style, visualTransformation, density) {
    layOutDesktopText(visualTransformation.filter(AnnotatedString(text)).text, style, density, field = true).style
  }
  return PlatformTextFieldStyle(fieldStyle, fieldTransformation)
}

private const val PROPORTIONAL_MARKER_HEIGHT_PX = 0.52f
private const val UNSET_LINE_HEIGHT_MARKER_PX = 0.2f
private const val ROUNDED_UP_HEIGHT_OFFSET = 0.49f
private const val NEGLIGIBLE_HEIGHT_FRACTION = 0.05f
private const val ABOVE_FONT_SIZE = 1.0001f
private const val MAX_COPIED_FONT_BYTES = 8 shl 20
private const val HHEA_LINE_GAP = 8
private const val OS2_TYPO_LINE_GAP = 72
private const val EMOJI_PRESENTATION = 0xFE0F
private const val KEYCAP = 0x20E3
private const val WINDOWS_EMOJI_FAMILY = "Segoe UI Emoji"
private const val MAC_EMOJI_FAMILY = "Apple Color Emoji"
private const val EMOJI_LOCALE = "und-Zsye"
private val DEFAULT_FONT_SIZE = 16.sp
private val LINE_BREAKS = Regex("\r\n|[\n\r\u000B\u000C\u0085  ]")

fun layOutDesktopText(text: AnnotatedString, style: TextStyle, density: Density, field: Boolean): PlatformText {
  val s = text.text
  val effective = text.spanStyles.filter { it.start == 0 }.fold(style) { acc, range -> acc.merge(range.item) }
  val fontSizePx = fontSizePx(effective.fontSize, density)
  val lineHeightPx = lineHeightPx(style.lineHeight, fontSizePx, density)
  val typeface = typefaceOf(effective.fontFamily)
  val lineHeightStyle = style.lineHeightStyle ?: LineHeightStyle.Default
  val trimBoth = lineHeightStyle.trim == LineHeightStyle.Trim.Both
  val proportional = lineHeightStyle.alignment == LineHeightStyle.Alignment.Proportional
  val centered = lineHeightStyle.alignment == LineHeightStyle.Alignment.Center && lineHeightStyle.trim == LineHeightStyle.Trim.None
  val hasEmptyLine = s.split(LINE_BREAKS).any { it.isEmpty() }
  val marker = when {
    field || hasEmptyLine -> null
    lineHeightPx == null -> SpanStyle(fontSize = with(density) { UNSET_LINE_HEIGHT_MARKER_PX.toSp() })
    typeface == null -> null
    proportional && (lineHeightPx > fontSizePx || trimBoth) -> SpanStyle(fontSize = with(density) { (PROPORTIONAL_MARKER_HEIGHT_PX / heightPerPx(typeface)).toSp() })
    centered -> centeredMarker(s, effective, typeface, fontSizePx, lineHeightPx, density)
    else -> null
  }
  var laidOutStyle = style
  if (marker == null && lineHeightPx != null) {
    if (s.isEmpty()) {
      if (!proportional) laidOutStyle = style.copy(lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Proportional, lineHeightStyle.trim))
    } else if (typeface != null) {
      val compensated = compensatedLineHeightPx(lineHeightPx, fontSizePx, typeface, lineHeightStyle)
      laidOutStyle = style.copy(lineHeight = with(density) { compensated.toSp() })
    }
  }
  val fallbackStyles = if (desktopPlatform.isMac() || desktopPlatform.isWindows()) fallbackStyles(text, style, density) else emptyList()
  if (marker == null && fallbackStyles.isEmpty()) return PlatformText(text, laidOutStyle)
  val builder = AnnotatedString.Builder(text)
  builder.addStyle(marker ?: SpanStyle(), 0, 0)
  for (range in fallbackStyles) builder.addStyle(range.item, range.start, range.end)
  return PlatformText(builder.toAnnotatedString(), laidOutStyle)
}

private fun fontSizePx(fontSize: TextUnit, density: Density): Float = with(density) {
  when {
    fontSize.isSp -> fontSize.toPx()
    fontSize.isEm -> DEFAULT_FONT_SIZE.toPx() * fontSize.value
    else -> DEFAULT_FONT_SIZE.toPx()
  }
}

private fun lineHeightPx(lineHeight: TextUnit, fontSizePx: Float, density: Density): Float? = with(density) {
  when {
    lineHeight.isSp -> lineHeight.toPx()
    lineHeight.isEm -> lineHeight.value * fontSizePx
    else -> null
  }
}

private fun centeredMarker(s: String, effective: TextStyle, typeface: Typeface, fontSizePx: Float, lineHeightPx: Float, density: Density): SpanStyle? {
  val dominant = dominantTypeface(s, typeface)
  val metrics = SkFont(dominant, fontSizePx).use { it.metrics }
  val exactHeight = metrics.descent - metrics.ascent
  val fraction = exactHeight - floor(exactHeight)
  val markerPx = if (fraction >= 0.5f || fraction < NEGLIGIBLE_HEIGHT_FRACTION) fontSizePx else fontSizePx * (ceil(exactHeight) - ROUNDED_UP_HEIGHT_OFFSET) / exactHeight
  if (lineHeightPx <= markerPx) return null
  val family = when {
    metrics.leading > 0f -> gaplessFamily(dominant) ?: return null
    dominant.uniqueId == typeface.uniqueId -> effective.fontFamily
    desktopPlatform.isWindows() && dominant.familyName == WINDOWS_EMOJI_FAMILY -> flatEmojiFamily ?: fontFamilyOf(dominant)
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
    val n = Character.charCount(cp)
    if (!Character.isWhitespace(cp) && !isEmojiJoiner(cp)) {
      val run = runTypeface(s, i, cp, n, typeface, FontStyle.NORMAL)
      if (run != null) {
        typefaces[run.uniqueId] = run
        counts[run.uniqueId] = (counts[run.uniqueId] ?: 0) + 1
      }
    }
    i += n
  }
  return counts.entries.sortedWith(compareByDescending<Map.Entry<Int, Int>> { it.value }.thenBy { if (it.key == typeface.uniqueId) 0 else 1 })
    .firstOrNull()?.let { typefaces[it.key] } ?: typeface
}

private fun compensatedLineHeightPx(lineHeightPx: Float, fontSizePx: Float, typeface: Typeface, lineHeightStyle: LineHeightStyle): Float {
  val metrics = SkFont(typeface, fontSizePx).use { it.metrics }
  val fontHeight = metrics.descent - metrics.ascent + metrics.leading
  val roundedFontHeight = fontHeight.roundToInt()
  if (roundedFontHeight >= fontHeight) return lineHeightPx
  val adjustedPx = when (lineHeightStyle.alignment) {
    LineHeightStyle.Alignment.Top, LineHeightStyle.Alignment.Center, LineHeightStyle.Alignment.Bottom ->
      lineHeightPx - fontHeight + roundedFontHeight
    else -> (lineHeightPx - metrics.leading) * roundedFontHeight / (metrics.descent - metrics.ascent)
  }
  return if (lineHeightPx > fontSizePx) max(adjustedPx, fontSizePx * ABOVE_FONT_SIZE) else adjustedPx
}

private fun runTypeface(s: String, i: Int, cp: Int, n: Int, segTypeface: Typeface, fontStyle: FontStyle): Typeface? {
  val next = if (i + n < s.length) s.codePointAt(i + n) else -1
  val presentation = next == EMOJI_PRESENTATION && covers(segTypeface, cp) && !covers(segTypeface, EMOJI_PRESENTATION)
  if (covers(segTypeface, cp) && !presentation) return segTypeface
  return if (isEmojiSequenceStart(s, i, cp, n)) emojiFallbackTypeface(cp, fontStyle)
  else fallbackTypeface(if (presentation) EMOJI_PRESENTATION else cp, fontStyle)
}

private fun isEmojiSequenceStart(s: String, i: Int, cp: Int, n: Int): Boolean {
  if (!inRanges(EMOJI_RANGES, cp)) return false
  if (!inRanges(EMOJI_COMPONENT_RANGES, cp)) return true
  val next = if (i + n < s.length) s.codePointAt(i + n) else -1
  if (cp in 0x1F1E6..0x1F1FF) return next in 0x1F1E6..0x1F1FF
  val afterNext = if (next != -1 && i + n + Character.charCount(next) < s.length) s.codePointAt(i + n + Character.charCount(next)) else -1
  return next == EMOJI_PRESENTATION && afterNext == KEYCAP
}

private fun inRanges(ranges: IntArray, cp: Int): Boolean {
  var lo = 0
  var hi = ranges.size / 2 - 1
  while (lo <= hi) {
    val mid = (lo + hi) ushr 1
    when {
      cp < ranges[2 * mid] -> hi = mid - 1
      cp > ranges[2 * mid + 1] -> lo = mid + 1
      else -> return true
    }
  }
  return false
}

private fun skiaFontStyle(style: SpanStyle): FontStyle =
  FontStyle((style.fontWeight ?: FontWeight.Normal).weight, 5, if (style.fontStyle == ComposeFontStyle.Italic) FontSlant.ITALIC else FontSlant.UPRIGHT)

private fun fallbackStyles(text: AnnotatedString, style: TextStyle, density: Density): List<AnnotatedString.Range<SpanStyle>> {
  val s = text.text
  val out = mutableListOf<AnnotatedString.Range<SpanStyle>>()
  val bounds = (text.spanStyles.flatMap { listOf(it.start, it.end) } + listOf(0, s.length)).filter { it in 0..s.length }.distinct().sorted()
  var emojiStart = -1
  var trackStart = -1
  var trackPx = 0f
  fun endTracking(at: Int) {
    if (trackStart >= 0 && at > trackStart) out += AnnotatedString.Range(SpanStyle(letterSpacing = with(density) { trackPx.toSp() }), trackStart, at)
    trackStart = -1
  }
  fun endEmoji(at: Int) {
    if (emojiStart >= 0 && at > emojiStart) {
      val family = flatEmojiFamily
      if (family != null) out += AnnotatedString.Range(SpanStyle(fontFamily = family), emojiStart, at)
    }
    emojiStart = -1
  }
  for (k in 0 until bounds.size - 1) {
    val segStart = bounds[k]
    val segEnd = bounds[k + 1]
    if (segStart == segEnd) continue
    val segStyle = text.spanStyles.filter { it.start <= segStart && it.end >= segEnd && it.start < it.end }.fold(style) { acc, range -> acc.merge(range.item) }
    val segTypeface = typefaceOf(segStyle.fontFamily) ?: continue
    val fontStyle = skiaFontStyle(segStyle.toSpanStyle())
    val fontSizePx = fontSizePx(segStyle.fontSize, density)
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
      if (desktopPlatform.isWindows()) {
        val emoji = if (emojiStart >= 0 && (isEmojiJoiner(cp) || inRanges(EMOJI_COMPONENT_RANGES, cp))) true
        else runTypeface(s, i, cp, n, segTypeface, fontStyle).let { it != null && it.uniqueId != segTypeface.uniqueId && it.familyName == WINDOWS_EMOJI_FAMILY }
        if (emoji && emojiStart < 0) emojiStart = i
        if (!emoji) endEmoji(i)
      } else {
        val run = runTypeface(s, i, cp, n, segTypeface, fontStyle)
        val tracking = if (run != null && hasTracking(run)) trackingPx(run, fontSizePx) else 0f
        if (tracking != 0f) {
          val spacing = letterSpacingPx - tracking
          if (trackStart >= 0 && spacing != trackPx) endTracking(i)
          if (trackStart < 0) {
            trackStart = i
            trackPx = spacing
          }
        } else {
          endTracking(i)
        }
      }
      i += n
    }
    endTracking(segEnd)
  }
  endEmoji(s.length)
  return out
}

private fun isEmojiJoiner(cp: Int) = cp == 0x200D || cp in 0xFE00..0xFE0F || cp == KEYCAP || cp in 0xE0020..0xE007F

private val coverage = ConcurrentHashMap<Long, Boolean>()

private fun covers(typeface: Typeface, cp: Int): Boolean =
  coverage.getOrPut((typeface.uniqueId.toLong() shl 32) or cp.toLong()) { typeface.getUTF32Glyph(cp).toInt() != 0 }

private val fallbackTypefaces = ConcurrentHashMap<Long, Any>()

private fun fallbackTypeface(cp: Int, fontStyle: FontStyle): Typeface? =
  fallbackTypefaces.getOrPut((fontStyle.weight.toLong() shl 33) or (fontStyle.slant.ordinal.toLong() shl 32) or cp.toLong()) {
    FontMgr.default.matchFamilyStyleCharacter(null, fontStyle, null, cp) ?: Unit
  } as? Typeface

private val emojiFallbackTypefaces = ConcurrentHashMap<Long, Any>()

private fun emojiFallbackTypeface(cp: Int, fontStyle: FontStyle): Typeface? =
  emojiFallbackTypefaces.getOrPut((fontStyle.weight.toLong() shl 33) or (fontStyle.slant.ordinal.toLong() shl 32) or cp.toLong()) {
    val typeface = if (desktopPlatform.isMac()) {
      FontMgr.default.matchFamilyStyle(MAC_EMOJI_FAMILY, FontStyle.NORMAL)
    } else {
      FontMgr.default.matchFamilyStyleCharacter(null, fontStyle, arrayOf(EMOJI_LOCALE), cp)
    }
    typeface ?: Unit
  } as? Typeface

private val trackingTables = ConcurrentHashMap<Int, Boolean>()

private fun hasTracking(typeface: Typeface): Boolean =
  trackingTables.getOrPut(typeface.uniqueId) { typeface.tableTags.contains("trak") }

private val trackings = ConcurrentHashMap<Pair<Int, Int>, Float>()

private fun trackingPx(typeface: Typeface, fontSizePx: Float): Float =
  trackings.getOrPut(typeface.uniqueId to (fontSizePx * 100).roundToInt()) {
    val sample = trackingSample(typeface) ?: return@getOrPut 0f
    val provider = TypefaceFontProvider().apply { registerTypeface(typeface, "tracking") }
    val fonts = FontCollection().apply { setAssetFontManager(provider) }
    val textStyle = SkTextStyle().setFontSize(fontSizePx).setFontFamilies(arrayOf("tracking"))
    val paragraphStyle = ParagraphStyle().apply { this.textStyle = textStyle }
    val paragraph = ParagraphBuilder(paragraphStyle, fonts).apply { pushStyle(textStyle); addText(String(Character.toChars(sample)).repeat(4)) }.build().apply { layout(Float.MAX_VALUE) }
    val advance = SkFont(typeface, fontSizePx).use { it.getWidths(shortArrayOf(typeface.getUTF32Glyph(sample)))[0] }
    paragraph.maxIntrinsicWidth / 4 - advance
  }

private fun trackingSample(typeface: Typeface): Int? =
  listOf('H'.code, 0x6C34, 0x3042, 0xAC00).firstOrNull { covers(typeface, it) }

private val flatEmojiFamily: FontFamily? by lazy {
  if (!desktopPlatform.isWindows()) return@lazy null
  val typeface = FontMgr.default.matchFamilyStyle(WINDOWS_EMOJI_FAMILY, FontStyle.NORMAL) ?: return@lazy null
  fontCopy(typeface, Int.MAX_VALUE) { tag, data ->
    if (tag == "COLR" && data.size >= 2) {
      data[0] = 0
      data[1] = 0
    }
  }?.let { FontFamily(Font("$WINDOWS_EMOJI_FAMILY COLR v0", it)) }
}

private val gaplessFamilies = ConcurrentHashMap<Int, Any>()

private fun gaplessFamily(typeface: Typeface): FontFamily? =
  gaplessFamilies.getOrPut(typeface.uniqueId) {
    fontCopy(typeface, MAX_COPIED_FONT_BYTES) { tag, data ->
      if (tag == "hhea" && data.size >= HHEA_LINE_GAP + 2) {
        data[HHEA_LINE_GAP] = 0
        data[HHEA_LINE_GAP + 1] = 0
      }
      if (tag == "OS/2" && data.size >= OS2_TYPO_LINE_GAP + 2) {
        data[OS2_TYPO_LINE_GAP] = 0
        data[OS2_TYPO_LINE_GAP + 1] = 0
      }
    }?.let { FontFamily(Font("${typeface.familyName} ${typeface.uniqueId} gapless", it)) } ?: Unit
  } as? FontFamily

private fun fontCopy(typeface: Typeface, maxBytes: Int, patch: (String, ByteArray) -> Unit): ByteArray? {
  val tags = typeface.tableTags.sorted()
  if (tags.sumOf { typeface.getTableSize(it).toLong() } > maxBytes) return null
  val tables = tags.map { tag ->
    val data = typeface.getTableData(tag)?.bytes ?: return null
    patch(tag, data)
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
  d.writeInt(if (tables.any { it.first == "CFF " || it.first == "CFF2" }) 0x4F54544F else 0x00010000)
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

private val fontFamilies = ConcurrentHashMap<Int, FontFamily>()

private fun fontFamilyOf(typeface: Typeface): FontFamily =
  fontFamilies.getOrPut(typeface.uniqueId) { FontFamily(androidx.compose.ui.text.platform.Typeface(typeface, "fallback ${typeface.uniqueId}")) }

private fun heightPerPx(typeface: Typeface): Float =
  SkFont(typeface, 100f).use { it.metrics }.let { (it.descent - it.ascent + it.leading) / 100f }

private val interTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MR.fonts.inter_regular.file.absolutePath) }

private val emojiTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MRdesktopMain.fonts.notocoloremoji_regular.file.absolutePath) }

private val unsetFamilyTypeface: Typeface? by lazy {
  FontCollection().apply { setDefaultFontManager(FontMgr.default) }.findTypefaces(arrayOf("sans-serif"), FontStyle.NORMAL).firstOrNull()
}

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
  null -> unsetFamilyTypeface
  Inter -> interTypeface
  FontFamily.Monospace -> monospaceTypeface
  FontFamily.Default, FontFamily.SansSerif -> sansSerifTypeface
  EmojiFont -> emojiTypeface
  else -> null
}

private val EMOJI_RANGES = intArrayOf(
  0x23, 0x23, 0x2A, 0x2A, 0x30, 0x39, 0xA9, 0xA9, 0xAE, 0xAE, 0x203C, 0x203C,
  0x2049, 0x2049, 0x2122, 0x2122, 0x2139, 0x2139, 0x2194, 0x2199, 0x21A9, 0x21AA, 0x231A, 0x231B,
  0x2328, 0x2328, 0x23CF, 0x23CF, 0x23E9, 0x23F3, 0x23F8, 0x23FA, 0x24C2, 0x24C2, 0x25AA, 0x25AB,
  0x25B6, 0x25B6, 0x25C0, 0x25C0, 0x25FB, 0x25FE, 0x2600, 0x2604, 0x260E, 0x260E, 0x2611, 0x2611,
  0x2614, 0x2615, 0x2618, 0x2618, 0x261D, 0x261D, 0x2620, 0x2620, 0x2622, 0x2623, 0x2626, 0x2626,
  0x262A, 0x262A, 0x262E, 0x262F, 0x2638, 0x263A, 0x2640, 0x2640, 0x2642, 0x2642, 0x2648, 0x2653,
  0x265F, 0x2660, 0x2663, 0x2663, 0x2665, 0x2666, 0x2668, 0x2668, 0x267B, 0x267B, 0x267E, 0x267F,
  0x2692, 0x2697, 0x2699, 0x2699, 0x269B, 0x269C, 0x26A0, 0x26A1, 0x26A7, 0x26A7, 0x26AA, 0x26AB,
  0x26B0, 0x26B1, 0x26BD, 0x26BE, 0x26C4, 0x26C5, 0x26C8, 0x26C8, 0x26CE, 0x26CF, 0x26D1, 0x26D1,
  0x26D3, 0x26D4, 0x26E9, 0x26EA, 0x26F0, 0x26F5, 0x26F7, 0x26FA, 0x26FD, 0x26FD, 0x2702, 0x2702,
  0x2705, 0x2705, 0x2708, 0x270D, 0x270F, 0x270F, 0x2712, 0x2712, 0x2714, 0x2714, 0x2716, 0x2716,
  0x271D, 0x271D, 0x2721, 0x2721, 0x2728, 0x2728, 0x2733, 0x2734, 0x2744, 0x2744, 0x2747, 0x2747,
  0x274C, 0x274C, 0x274E, 0x274E, 0x2753, 0x2755, 0x2757, 0x2757, 0x2763, 0x2764, 0x2795, 0x2797,
  0x27A1, 0x27A1, 0x27B0, 0x27B0, 0x27BF, 0x27BF, 0x2934, 0x2935, 0x2B05, 0x2B07, 0x2B1B, 0x2B1C,
  0x2B50, 0x2B50, 0x2B55, 0x2B55, 0x3030, 0x3030, 0x303D, 0x303D, 0x3297, 0x3297, 0x3299, 0x3299,
  0x1F004, 0x1F004, 0x1F0CF, 0x1F0CF, 0x1F170, 0x1F171, 0x1F17E, 0x1F17F, 0x1F18E, 0x1F18E, 0x1F191, 0x1F19A,
  0x1F1E6, 0x1F1FF, 0x1F201, 0x1F202, 0x1F21A, 0x1F21A, 0x1F22F, 0x1F22F, 0x1F232, 0x1F23A, 0x1F250, 0x1F251,
  0x1F300, 0x1F321, 0x1F324, 0x1F393, 0x1F396, 0x1F397, 0x1F399, 0x1F39B, 0x1F39E, 0x1F3F0, 0x1F3F3, 0x1F3F5,
  0x1F3F7, 0x1F4FD, 0x1F4FF, 0x1F53D, 0x1F549, 0x1F54E, 0x1F550, 0x1F567, 0x1F56F, 0x1F570, 0x1F573, 0x1F57A,
  0x1F587, 0x1F587, 0x1F58A, 0x1F58D, 0x1F590, 0x1F590, 0x1F595, 0x1F596, 0x1F5A4, 0x1F5A5, 0x1F5A8, 0x1F5A8,
  0x1F5B1, 0x1F5B2, 0x1F5BC, 0x1F5BC, 0x1F5C2, 0x1F5C4, 0x1F5D1, 0x1F5D3, 0x1F5DC, 0x1F5DE, 0x1F5E1, 0x1F5E1,
  0x1F5E3, 0x1F5E3, 0x1F5E8, 0x1F5E8, 0x1F5EF, 0x1F5EF, 0x1F5F3, 0x1F5F3, 0x1F5FA, 0x1F64F, 0x1F680, 0x1F6C5,
  0x1F6CB, 0x1F6D2, 0x1F6D5, 0x1F6D7, 0x1F6DC, 0x1F6E5, 0x1F6E9, 0x1F6E9, 0x1F6EB, 0x1F6EC, 0x1F6F0, 0x1F6F0,
  0x1F6F3, 0x1F6FC, 0x1F7E0, 0x1F7EB, 0x1F7F0, 0x1F7F0, 0x1F90C, 0x1F93A, 0x1F93C, 0x1F945, 0x1F947, 0x1F9FF,
  0x1FA70, 0x1FA7C, 0x1FA80, 0x1FA88, 0x1FA90, 0x1FABD, 0x1FABF, 0x1FAC5, 0x1FACE, 0x1FADB, 0x1FAE0, 0x1FAE8,
  0x1FAF0, 0x1FAF8,
)

private val EMOJI_COMPONENT_RANGES = intArrayOf(
  0x23, 0x23, 0x2A, 0x2A, 0x30, 0x39, 0x200D, 0x200D, 0x20E3, 0x20E3, 0xFE0F, 0xFE0F,
  0x1F1E6, 0x1F1FF, 0x1F3FB, 0x1F3FF, 0x1F9B0, 0x1F9B3, 0xE0020, 0xE007F,
)
