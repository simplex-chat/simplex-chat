package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.TransformedText
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.EmojiFont
import chat.simplex.common.ui.theme.Inter
import chat.simplex.res.*
import org.jetbrains.skia.BreakIterator
import org.jetbrains.skia.Data
import org.jetbrains.skia.FontMetrics
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
import java.text.Normalizer
import java.util.TreeMap
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import kotlin.math.abs
import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.max
import kotlin.math.min
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
    if (desktopPlatform.isLinux() || visualTransformation is PasswordVisualTransformation) {
      visualTransformation
    } else {
      var last: Pair<AnnotatedString, AnnotatedString>? = null
      VisualTransformation { input ->
        val transformed = visualTransformation.filter(input)
        val laidOut = last?.takeIf { it.first == transformed.text }?.second
          ?: layOutDesktopText(transformed.text, style, density, field = true).text.also { last = transformed.text to it }
        TransformedText(laidOut, transformed.offsetMapping)
      }
    }
  }
  val empty = text.isEmpty()
  val fieldStyle = remember(empty, style, visualTransformation, density) {
    val laidOutStyle = layOutDesktopText(AnnotatedString(if (empty) "" else "x"), style, density, field = true).style
    val maskTracking = if (visualTransformation is PasswordVisualTransformation && desktopPlatform.isMac()) {
      layOutDesktopText(AnnotatedString(visualTransformation.mask.toString()), style, density, field = true).text.spanStyles.firstOrNull()?.item
    } else null
    if (maskTracking == null) laidOutStyle else laidOutStyle.merge(maskTracking)
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
private const val ZERO_WIDTH_JOINER = 0x200D
private val VARIATION_SELECTORS = 0xFE00..0xFE0F
private val TAGS = 0xE0020..0xE007F
private val REGIONAL_INDICATORS = 0x1F1E6..0x1F1FF
private val SKIN_TONES = 0x1F3FB..0x1F3FF
private val HAIR_COMPONENTS = 0x1F9B0..0x1F9B3
private val UNSHAPED_TYPES = setOf(Character.CONTROL, Character.FORMAT, Character.LINE_SEPARATOR, Character.PARAGRAPH_SEPARATOR)
private val SPACE_FALLBACKS = ((0x2000..0x200A) + listOf(0xA0, 0x202F, 0x205F, 0x3000)).toSet()
private val FORMAT_SINCE_UNICODE_14 = intArrayOf(0x890, 0x891, 0x13439, 0x1343F)
private const val NEGLIGIBLE_TRACKING_PX = 1f / 4096
private const val NON_BREAKING_HYPHEN = 0x2011
private const val HYPHEN = 0x2010
private const val NO_LIGATURES = "liga 0"
private const val INLINE_CONTENT_TAG = "androidx.compose.foundation.text.inlineContent"
private const val WINDOWS_EMOJI_FAMILY = "Segoe UI Emoji"
private const val MAC_EMOJI_FAMILY = "Apple Color Emoji"
private const val EMOJI_LOCALE = "und-Zsye"
private val DEFAULT_FONT_SIZE = 16.sp
private val HARD_BREAKS = setOf('\n', '\u000B', '\u000C', ' ', ' ')

internal fun layOutDesktopText(text: AnnotatedString, style: TextStyle, density: Density, field: Boolean): PlatformText {
  val s = text.text
  val effective = text.spanStyles.filter { it.start == 0 }.fold(style) { acc, range -> acc.merge(range.item) }
  val fontSizePx = fontSizePx(effective.fontSize, density)
  val lineHeightPx = unitPx(style.lineHeight, fontSizePx, density)
  val typeface = typefacesOf(effective.fontFamily).firstOrNull()
  val lineHeightStyle = style.lineHeightStyle ?: LineHeightStyle.Default
  val trimBoth = lineHeightStyle.trim == LineHeightStyle.Trim.Both
  val proportional = lineHeightStyle.alignment == LineHeightStyle.Alignment.Proportional
  val centered = lineHeightStyle.alignment == LineHeightStyle.Alignment.Center && lineHeightStyle.trim == LineHeightStyle.Trim.None
  val segments by lazy { styledSegments(text, style) }
  val fonts by lazy { characterFonts(text, segments) }
  val marker = when {
    field || hasEmptyLine(s) -> null
    lineHeightPx == null -> SpanStyle(fontSize = with(density) { UNSET_LINE_HEIGHT_MARKER_PX.toSp() })
    typeface == null -> null
    proportional && (lineHeightPx > fontSizePx || trimBoth) -> SpanStyle(fontSize = with(density) { (PROPORTIONAL_MARKER_HEIGHT_PX / heightPerPx(typeface)).toSp() })
    centered -> centeredMarker(s, fonts, effective, typeface, fontSizePx, lineHeightPx, density)
    else -> null
  }
  var laidOutStyle = style
  if (marker == null && lineHeightPx != null) {
    if (s.isEmpty()) {
      if (!proportional && !field) laidOutStyle = style.copy(lineHeightStyle = LineHeightStyle(LineHeightStyle.Alignment.Proportional, lineHeightStyle.trim))
    } else if (typeface != null) {
      val compensated = compensatedLineHeightPx(lineHeightPx, fontSizePx, typeface, lineHeightStyle)
      laidOutStyle = style.copy(lineHeight = with(density) { compensated.toSp() })
    }
  }
  val fallbackStyles = if (desktopPlatform.isMac() || desktopPlatform.isWindows()) fallbackStyles(s, segments, fonts, density) else emptyList()
  if (marker == null && fallbackStyles.isEmpty()) return PlatformText(text, laidOutStyle)
  val builder = AnnotatedString.Builder(text)
  builder.addStyle(marker ?: SpanStyle(), 0, 0)
  for (range in fallbackStyles) builder.addStyle(range.item, range.start, range.end)
  return PlatformText(builder.toAnnotatedString(), laidOutStyle)
}

private fun hasEmptyLine(s: String): Boolean {
  var lineStart = 0
  for (i in 0..s.length) {
    if (i == s.length || s[i] in HARD_BREAKS) {
      val length = i - lineStart
      if (length == 0 || (length == 1 && s[lineStart] == '\r')) return true
      lineStart = i + 1
    }
  }
  return false
}

private fun fontSizePx(fontSize: TextUnit, density: Density): Float = with(density) {
  when {
    fontSize.isSp -> fontSize.toPx()
    fontSize.isEm -> DEFAULT_FONT_SIZE.toPx() * fontSize.value
    else -> DEFAULT_FONT_SIZE.toPx()
  }
}

private fun unitPx(value: TextUnit, fontSizePx: Float, density: Density): Float? = with(density) {
  when {
    value.isSp -> value.toPx()
    value.isEm -> value.value * fontSizePx
    else -> null
  }
}

private fun centeredMarker(s: String, fonts: Array<Typeface?>, effective: TextStyle, typeface: Typeface, fontSizePx: Float, lineHeightPx: Float, density: Density): SpanStyle? {
  val dominant = dominantTypeface(s, fonts, typeface)
  val metrics = metricsOf(dominant, fontSizePx)
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

private fun dominantTypeface(s: String, fonts: Array<Typeface?>, typeface: Typeface): Typeface {
  val counts = HashMap<Int, Int>()
  val typefaces = HashMap<Int, Typeface>()
  var i = 0
  while (i < s.length) {
    val cp = s.codePointAt(i)
    val run = fonts[i]
    if (run != null && !Character.isWhitespace(cp) && !isEmojiContinuation(s, i, cp)) {
      typefaces[run.uniqueId] = run
      counts[run.uniqueId] = (counts[run.uniqueId] ?: 0) + 1
    }
    i += Character.charCount(cp)
  }
  return counts.entries.sortedWith(compareByDescending<Map.Entry<Int, Int>> { it.value }.thenBy { if (it.key == typeface.uniqueId) 0 else 1 })
    .firstOrNull()?.let { typefaces[it.key] } ?: typeface
}

private fun compensatedLineHeightPx(lineHeightPx: Float, fontSizePx: Float, typeface: Typeface, lineHeightStyle: LineHeightStyle): Float {
  val metrics = metricsOf(typeface, fontSizePx)
  val fontHeight = metrics.descent - metrics.ascent + metrics.leading
  val roundedFontHeight = fontHeight.roundToInt()
  if (roundedFontHeight >= fontHeight) return lineHeightPx
  val adjustedPx = if (lineHeightStyle.alignment == LineHeightStyle.Alignment.Proportional) {
    (lineHeightPx - metrics.leading) * roundedFontHeight / (metrics.descent - metrics.ascent)
  } else {
    lineHeightPx - fontHeight + roundedFontHeight
  }
  return if (lineHeightPx > fontSizePx) max(adjustedPx, fontSizePx * ABOVE_FONT_SIZE) else min(adjustedPx, fontSizePx)
}

private class StyledSegment(val start: Int, val end: Int, val style: TextStyle) {
  val typefaces: List<Typeface> by lazy { typefacesOf(style.fontFamily) }
  val fontStyle: FontStyle by lazy { skiaFontStyle(style.toSpanStyle()) }
}

private fun styledSegments(text: AnnotatedString, style: TextStyle): List<StyledSegment> {
  val length = text.length
  val spans = text.spanStyles.mapIndexedNotNull { index, range ->
    val start = range.start.coerceIn(0, length)
    val end = range.end.coerceIn(0, length)
    if (start < end) Triple(index, start, end) else null
  }
  val starts = spans.groupBy { it.second }
  val ends = spans.groupBy { it.third }
  val bounds = (spans.flatMap { listOf(it.second, it.third) } + listOf(0, length)).distinct().sorted()
  val active = TreeMap<Long, SpanStyle>()
  val out = mutableListOf<StyledSegment>()
  for (k in 0 until bounds.size - 1) {
    val bound = bounds[k]
    ends[bound]?.forEach { active.remove((it.second.toLong() shl 32) or it.first.toLong()) }
    starts[bound]?.forEach { active[(it.second.toLong() shl 32) or it.first.toLong()] = text.spanStyles[it.first].item }
    out += StyledSegment(bound, bounds[k + 1], active.values.fold(style) { acc, item -> acc.merge(item) })
  }
  return out
}

private fun characterFonts(text: AnnotatedString, segments: List<StyledSegment>): Array<Typeface?> {
  val s = text.text
  val fonts = arrayOfNulls<Typeface>(s.length)
  val inlined = BooleanArray(s.length)
  for (range in text.getStringAnnotations(INLINE_CONTENT_TAG, 0, s.length)) inlined.fill(true, range.start, range.end)
  val graphemeStarts by lazy {
    val starts = BooleanArray(s.length + 1)
    BreakIterator.makeCharacterInstance().use { graphemes ->
      graphemes.setText(s)
      var boundary = graphemes.first()
      while (boundary != BreakIterator.DONE) {
        starts[boundary] = true
        boundary = graphemes.next()
      }
    }
    starts
  }
  for (segment in segments) {
    val first = segment.typefaces.firstOrNull() ?: continue
    if (coversCharacters(first, s, segment.start, segment.end, inlined)) {
      fonts.fill(first, segment.start, segment.end)
      continue
    }
    val fallbacks = mutableListOf<Typeface>()
    var start = segment.start
    while (start < segment.end) {
      var end = start + 1
      while (end < segment.end && !graphemeStarts[end]) end++
      if (!inlined[start]) {
        val typeface = segment.typefaces.firstOrNull { coversGrapheme(it, s, start, end, inlined) }
          ?: fallbacks.firstOrNull { coversGrapheme(it, s, start, end, inlined) }
          ?: graphemeFallback(s, start, end, segment.fontStyle, inlined)?.also { fallbacks += it }
        fonts.fill(typeface, start, end)
      }
      start = end
    }
  }
  for (i in inlined.indices) if (inlined[i]) fonts[i] = null
  return fonts
}

private fun coversGrapheme(typeface: Typeface, s: String, start: Int, end: Int, inlined: BooleanArray): Boolean {
  if (coversCharacters(typeface, s, start, end, inlined)) return true
  if ((start until end).any { inlined[it] }) return false
  val grapheme = s.substring(start, end)
  return listOf(Normalizer.Form.NFC, Normalizer.Form.NFD).any { form ->
    val normalized = Normalizer.normalize(grapheme, form)
    normalized != grapheme && coversCharacters(typeface, normalized, 0, normalized.length, null)
  }
}

private fun coversCharacters(typeface: Typeface, s: String, start: Int, end: Int, inlined: BooleanArray?): Boolean {
  var i = start
  while (i < end) {
    val cp = s.codePointAt(i)
    if (inlined?.get(i) != true && !isUnshaped(cp) && !shapes(typeface, cp)) return false
    i += Character.charCount(cp)
  }
  return true
}

private fun shapes(typeface: Typeface, cp: Int): Boolean = when {
  covers(typeface, cp) -> true
  cp in SPACE_FALLBACKS -> covers(typeface, ' '.code)
  cp == NON_BREAKING_HYPHEN -> covers(typeface, HYPHEN)
  else -> false
}

private fun graphemeFallback(s: String, start: Int, end: Int, fontStyle: FontStyle, inlined: BooleanArray): Typeface? {
  var i = start
  while (i < end) {
    val cp = s.codePointAt(i)
    val n = Character.charCount(cp)
    if (!isUnshaped(cp)) {
      val typeface = fallbackTypeface(cp, fontStyle, i == start && isEmojiSequenceStart(s, i, cp, n))
      if (typeface != null && coversGrapheme(typeface, s, start, end, inlined)) return typeface
    }
    i += n
  }
  return null
}

private fun isUnshaped(cp: Int): Boolean =
  Character.getType(cp).toByte() in UNSHAPED_TYPES || inRanges(DEFAULT_IGNORABLE_RANGES, cp) || inRanges(FORMAT_SINCE_UNICODE_14, cp)

private fun isEmojiSequenceStart(s: String, i: Int, cp: Int, n: Int): Boolean {
  if (!inRanges(EMOJI_RANGES, cp)) return false
  if (!inRanges(EMOJI_COMPONENT_RANGES, cp)) return true
  val next = if (i + n < s.length) s.codePointAt(i + n) else -1
  if (next in REGIONAL_INDICATORS) return true
  val afterNext = if (next != -1 && i + n + Character.charCount(next) < s.length) s.codePointAt(i + n + Character.charCount(next)) else -1
  return next == EMOJI_PRESENTATION && afterNext == KEYCAP
}

private fun isEmojiContinuation(s: String, i: Int, cp: Int): Boolean = when {
  cp == ZERO_WIDTH_JOINER || cp == KEYCAP -> true
  cp in VARIATION_SELECTORS || cp in TAGS || cp in SKIN_TONES || cp in HAIR_COMPONENTS -> true
  cp in REGIONAL_INDICATORS -> precedingRegionalIndicators(s, i) % 2 == 1
  else -> false
}

private fun precedingRegionalIndicators(s: String, i: Int): Int {
  var count = 0
  var j = i
  while (j > 0) {
    val cp = s.codePointBefore(j)
    if (cp !in REGIONAL_INDICATORS) break
    count++
    j -= Character.charCount(cp)
  }
  return count
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

private fun fallbackStyles(s: String, segments: List<StyledSegment>, fonts: Array<Typeface?>, density: Density): List<AnnotatedString.Range<SpanStyle>> {
  val out = mutableListOf<AnnotatedString.Range<SpanStyle>>()
  var emojiStart = -1
  var trackStart = -1
  var trackPx = 0f
  var trackLigatures: String? = null
  fun endTracking(at: Int) {
    if (trackStart >= 0 && at > trackStart) {
      out += AnnotatedString.Range(SpanStyle(letterSpacing = with(density) { trackPx.toSp() }, fontFeatureSettings = trackLigatures), trackStart, at)
    }
    trackStart = -1
  }
  fun endEmoji(at: Int) {
    if (emojiStart >= 0 && at > emojiStart) {
      val family = flatEmojiFamily
      if (family != null) out += AnnotatedString.Range(SpanStyle(fontFamily = family), emojiStart, at)
    }
    emojiStart = -1
  }
  for (segment in segments) {
    if (segment.typefaces.isEmpty()) {
      endEmoji(segment.start)
      continue
    }
    val fontSizePx = fontSizePx(segment.style.fontSize, density)
    val letterSpacingPx = unitPx(segment.style.letterSpacing, fontSizePx, density) ?: 0f
    var i = segment.start
    while (i < segment.end) {
      val cp = s.codePointAt(i)
      val run = fonts[i]
      if (desktopPlatform.isWindows()) {
        val emoji = (emojiStart >= 0 && isEmojiContinuation(s, i, cp)) ||
          (run != null && run !in segment.typefaces && factsOf(run).windowsEmoji)
        if (emoji && emojiStart < 0) emojiStart = i
        if (!emoji) endEmoji(i)
      } else {
        val tracking = if (run != null && factsOf(run).tracked) trackingPx(run, fontSizePx) else 0f
        if (tracking != 0f) {
          val spacing = letterSpacingPx - tracking
          if (trackStart >= 0 && spacing != trackPx) endTracking(i)
          if (trackStart < 0) {
            trackStart = i
            trackPx = spacing
            trackLigatures = if (letterSpacingPx > 0f && spacing <= 0f) NO_LIGATURES else null
          }
        } else {
          endTracking(i)
        }
      }
      i += Character.charCount(cp)
    }
    endTracking(segment.end)
  }
  endEmoji(s.length)
  return out
}

private fun sizeKey(typeface: Typeface, sizePx: Float): Long =
  (typeface.uniqueId.toLong() shl 32) or (sizePx.toRawBits().toLong() and 0xffffffffL)

private fun covers(typeface: Typeface, cp: Int): Boolean = typeface.getUTF32Glyph(cp).toInt() != 0

private class TypefaceFacts(val tracked: Boolean, val windowsEmoji: Boolean) {
  val trackings = ConcurrentHashMap<Float, Float>()
}

private val typefaceFacts = ConcurrentHashMap<Typeface, TypefaceFacts>()

private fun factsOf(typeface: Typeface): TypefaceFacts =
  typefaceFacts.getOrPut(typeface) {
    TypefaceFacts(
      tracked = typeface.tableTags.contains("trak"),
      windowsEmoji = desktopPlatform.isWindows() && typeface.familyName == WINDOWS_EMOJI_FAMILY
    )
  }

private val fontMetrics = ConcurrentHashMap<Long, FontMetrics>()

private fun metricsOf(typeface: Typeface, sizePx: Float): FontMetrics =
  fontMetrics.getOrPut(sizeKey(typeface, sizePx)) { SkFont(typeface, sizePx).use { it.metrics } }

private val fallbackTypefaces = ConcurrentHashMap<Long, Any>()

private fun fallbackTypeface(cp: Int, fontStyle: FontStyle, emoji: Boolean): Typeface? {
  val key = (if (emoji) 1L shl 48 else 0L) or (fontStyle.weight.toLong() shl 34) or (fontStyle.slant.ordinal.toLong() shl 32) or cp.toLong()
  val system = fallbackTypefaces.getOrPut(key) {
    when {
      emoji && desktopPlatform.isMac() -> FontMgr.default.matchFamilyStyle(MAC_EMOJI_FAMILY, FontStyle.NORMAL)
      emoji -> FontMgr.default.matchFamilyStyleCharacter(null, fontStyle, arrayOf(EMOJI_LOCALE), cp)
      else -> FontMgr.default.matchFamilyStyleCharacter(null, fontStyle, null, cp)
    } ?: Unit
  } as? Typeface
  return system ?: (listOfNotNull(interTypeface, emojiTypeface) + createdTypefaces).firstOrNull { covers(it, cp) }
}

private val createdTypefaces = CopyOnWriteArrayList<Typeface>()

private fun trackingPx(typeface: Typeface, fontSizePx: Float): Float =
  factsOf(typeface).trackings.getOrPut(fontSizePx) {
    val sample = trackingSample(typeface) ?: return@getOrPut 0f
    val advance = SkFont(typeface, fontSizePx).use { it.getWidths(shortArrayOf(typeface.getUTF32Glyph(sample)))[0] }
    TypefaceFontProvider().use { provider ->
      provider.registerTypeface(typeface, "tracking")
      FontCollection().use { fonts ->
        fonts.setAssetFontManager(provider)
        SkTextStyle().use { textStyle ->
          textStyle.setFontSize(fontSizePx).setFontFamilies(arrayOf("tracking"))
          ParagraphStyle().use { paragraphStyle ->
            paragraphStyle.textStyle = textStyle
            ParagraphBuilder(paragraphStyle, fonts).use { builder ->
              builder.pushStyle(textStyle)
              builder.addText(String(Character.toChars(sample)).repeat(4))
              builder.build().use { paragraph ->
                paragraph.layout(Float.MAX_VALUE)
                (paragraph.maxIntrinsicWidth / 4 - advance).takeIf { abs(it) >= NEGLIGIBLE_TRACKING_PX } ?: 0f
              }
            }
          }
        }
      }
    }
  }

private fun trackingSample(typeface: Typeface): Int? =
  listOf('H'.code, 0x6C34, 0x3042, 0xAC00).firstOrNull { covers(typeface, it) }

private val flatEmojiFamily: FontFamily? by lazy {
  if (!desktopPlatform.isWindows()) return@lazy null
  val typeface = FontMgr.default.matchFamilyStyle(WINDOWS_EMOJI_FAMILY, FontStyle.NORMAL) ?: return@lazy null
  val colr = typeface.getTableData("COLR")?.use { it.bytes } ?: return@lazy null
  if (colr.size < 4 || (colr[0].toInt() == 0 && colr[1].toInt() == 0) || (colr[2].toInt() == 0 && colr[3].toInt() == 0)) return@lazy null
  patchedFamily(typeface, Int.MAX_VALUE) { tag, data ->
    if (tag == "COLR" && data.size >= 2) {
      data[0] = 0
      data[1] = 0
    }
  }
}

private val gaplessFamilies = ConcurrentHashMap<Int, Any>()

private fun gaplessFamily(typeface: Typeface): FontFamily? =
  gaplessFamilies.getOrPut(typeface.uniqueId) {
    patchedFamily(typeface, MAX_COPIED_FONT_BYTES) { tag, data ->
      if (tag == "hhea" && data.size >= HHEA_LINE_GAP + 2) {
        data[HHEA_LINE_GAP] = 0
        data[HHEA_LINE_GAP + 1] = 0
      }
      if (tag == "OS/2" && data.size >= OS2_TYPO_LINE_GAP + 2) {
        data[OS2_TYPO_LINE_GAP] = 0
        data[OS2_TYPO_LINE_GAP + 1] = 0
      }
    } ?: Unit
  } as? FontFamily

private fun patchedFamily(typeface: Typeface, maxBytes: Int, patch: (String, ByteArray) -> Unit): FontFamily? {
  val bytes = fontCopy(typeface, maxBytes, patch) ?: return null
  return Data.makeFromBytes(bytes).use { FontMgr.default.makeFromData(it) }?.let { fontFamilyOf(it) }
}

private fun fontCopy(typeface: Typeface, maxBytes: Int, patch: (String, ByteArray) -> Unit): ByteArray? {
  val tags = typeface.tableTags.sorted()
  if (tags.sumOf { typeface.getTableSize(it).toLong() } > maxBytes) return null
  val tables = tags.map { tag ->
    val data = typeface.getTableData(tag)?.use { it.bytes } ?: return null
    patch(tag, data)
    tag to data
  }
  val out = ByteArrayOutputStream(12 + tables.size * 16 + tables.sumOf { (it.second.size + 3) / 4 * 4 })
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
    var checksum = 0L
    for (i in data.indices) checksum = (checksum + ((data[i].toLong() and 255) shl (24 - i % 4 * 8))) and 0xffffffffL
    d.write(tag.toByteArray(Charsets.US_ASCII))
    d.writeInt(checksum.toInt())
    d.writeInt(offset)
    d.writeInt(data.size)
    offset += (data.size + 3) / 4 * 4
  }
  for ((_, data) in tables) {
    d.write(data)
    repeat((4 - data.size % 4) % 4) { d.write(0) }
  }
  return out.toByteArray()
}

private val fontFamilies = ConcurrentHashMap<Int, FontFamily>()

private fun fontFamilyOf(typeface: Typeface): FontFamily =
  fontFamilies.getOrPut(typeface.uniqueId) {
    createdTypefaces += typeface
    FontFamily(androidx.compose.ui.text.platform.Typeface(typeface, "fallback ${typeface.uniqueId}"))
  }

private fun heightPerPx(typeface: Typeface): Float =
  metricsOf(typeface, 100f).let { (it.descent - it.ascent + it.leading) / 100f }

private val interTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MR.fonts.inter_regular.file.absolutePath) }

private val emojiTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MRdesktopMain.fonts.notocoloremoji_regular.file.absolutePath) }

private val systemFonts: FontCollection by lazy { FontCollection().apply { setDefaultFontManager(FontMgr.default) } }

private val unsetFamilyTypefaces: List<Typeface> by lazy { installedTypefaces(listOf("sans-serif")) }

private val monospaceTypefaces: List<Typeface> by lazy {
  installedTypefaces(
    when {
      desktopPlatform.isLinux() -> listOf("Noto Sans Mono", "DejaVu Sans Mono", "Consolas")
      desktopPlatform.isWindows() -> listOf("Consolas")
      else -> listOf(".AppleSystemUIFontMonospaced", "Menlo", "Courier")
    }
  )
}

private val sansSerifTypefaces: List<Typeface> by lazy {
  installedTypefaces(
    when {
      desktopPlatform.isLinux() -> listOf("Noto Sans", "DejaVu Sans", "Arial")
      desktopPlatform.isWindows() -> listOf("Segoe UI", "Arial")
      else -> listOf(".AppleSystemUIFont", "Helvetica Neue", "Helvetica")
    }
  )
}

private fun installedTypefaces(families: List<String>): List<Typeface> =
  systemFonts.findTypefaces(families.toTypedArray(), FontStyle.NORMAL).filterNotNull()

private fun typefacesOf(family: FontFamily?): List<Typeface> = when (family) {
  null -> unsetFamilyTypefaces
  Inter -> listOfNotNull(interTypeface)
  FontFamily.Monospace -> monospaceTypefaces
  FontFamily.Default, FontFamily.SansSerif -> sansSerifTypefaces
  EmojiFont -> listOfNotNull(emojiTypeface)
  else -> emptyList()
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

private val DEFAULT_IGNORABLE_RANGES = intArrayOf(
  0xAD, 0xAD, 0x34F, 0x34F, 0x61C, 0x61C, 0x17B4, 0x17B5, 0x180B, 0x180E,
  0x200B, 0x200F, 0x202A, 0x202E, 0x2060, 0x206F, 0xFE00, 0xFE0F, 0xFEFF, 0xFEFF,
  0xFFF0, 0xFFF8, 0x1D173, 0x1D17A, 0xE0000, 0xE0FFF,
)

private val EMOJI_COMPONENT_RANGES = intArrayOf(
  0x23, 0x23, 0x2A, 0x2A, 0x30, 0x39, 0x200D, 0x200D, 0x20E3, 0x20E3, 0xFE0F, 0xFE0F,
  0x1F1E6, 0x1F1FF, 0x1F3FB, 0x1F3FF, 0x1F9B0, 0x1F9B3, 0xE0020, 0xE007F,
)
