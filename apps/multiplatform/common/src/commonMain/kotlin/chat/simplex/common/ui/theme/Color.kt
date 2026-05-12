package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.views.helpers.PresetWallpaper
import chat.simplex.common.views.helpers.WallpaperType
import chat.simplex.common.views.helpers.mixWith
import kotlin.math.*

fun oklch(L: Float, C: Float, H: Float, alpha: Float = 1f): Color {
  val hRad = H * (Math.PI.toFloat() / 180f)
  val targetSpace = if (appPlatform.isDesktop) ColorSpaces.Srgb else ColorSpaces.DisplayP3
  return Color(L, C * cos(hRad), C * sin(hRad), alpha, ColorSpaces.Oklab).convert(targetSpace)
}

/** Extract oklch components (L, C, H) from a Color. Round-trip safe with oklch(). */
fun Color.toOklch(): FormulaSlot {
  val lab = convert(ColorSpaces.Oklab)
  val L = lab.component1()
  val a = lab.component2()
  val b = lab.component3()
  val C = sqrt(a * a + b * b)
  val H = if (C < 1e-6f) 0f else (atan2(b, a) * 180f / PI.toFloat()).let { if (it < 0) it + 360f else it }
  return FormulaSlot(L, C, H)
}

val Indigo = Color(0xFF9966FF)
val SimplexBlue = oklch(0.6320536f, 0.2017874f, 254.0879f)  // If this value changes also need to update #0088ff in string resource files
val SimplexGreen = oklch(0.7871495f, 0.1979258f, 146.6814f) // #ff4dda67
val SecretColor = oklch(0.5998708f, 0f, 0f, 0.2509804f) // #40808080
val LightGray = oklch(0.9615242f, 0.005440391f, 274.9652f) // #fff1f2f6
val DarkGray = oklch(0.2928853f, 0.003884885f, 264.5058f) // #ff2b2c2e
val HighOrLowlight = oklch(0.6265517f, 0.005036114f, 34.30441f) // #ff8b8786
val MessagePreviewDark = Color(179, 175, 174, 255)
val MessagePreviewLight = Color(49, 45, 44, 255)
val ToolbarLight = Color(220, 220, 220, 12)
val ToolbarDark = Color(80, 80, 80, 12)
val SettingsSecondaryLight = Color(200, 196, 195, 90)
val GroupDark = Color(80, 80, 80, 60)
val IncomingCallLight = Color(239, 237, 236, 255)
val WarningOrange = Color(255, 127, 0, 255)
val WarningYellow = Color(255, 192, 0, 255)
val FileLight = Color(191, 194, 199, 255)
val FileDark = Color(94, 94, 98, 255)

val MenuTextColor: Color @Composable get () = if (isInDarkTheme()) LocalContentColor.current.copy(alpha = 0.8f) else Color.Black
val NoteFolderIconColor: Color @Composable get() = MaterialTheme.appColors.primaryVariant2

/** Background color for panels (top app bar, bottom nav, status bar overlay).
 *  When current wallpaper is a preset and theme is LIGHT or DARK, panel gets a subtle hue tint
 *  matching the wallpaper. Otherwise falls back to the existing bg.mixWith(onBg, 0.97f) elevation.
 *  BLACK and SIMPLEX themes are not tinted (BLACK keeps pure dark, SIMPLEX has its own custom panel). */
@Composable
fun panelBackgroundColor(): Color {
  return currentWallpaperPanelTint()
    ?: MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
}

@Composable
private fun currentWallpaperPanelTint(): Color? {
  val state = CurrentColors.collectAsState().value
  val type = state.wallpaper.type as? WallpaperType.Preset ?: return null
  val preset = PresetWallpaper.from(type.filename) ?: return null
  val hue = preset.hue(state.base)
  return when (state.base) {
    DefaultTheme.LIGHT -> oklch(1.0f, 0.03f, hue)
    DefaultTheme.DARK -> oklch(0.1822f, 0.01f, hue)
    else -> null
  }
}

// ===== Gamut boundary (analytical, Cardano) =====

/** Max chroma in Display P3 gamut for given oklch L and H (degrees). O(1), exact. */
fun maxChromaP3(L: Float, H: Float): Float = maxChromaForMatrix(L, H, P3_FROM_LMS)

/** Max chroma in sRGB gamut for given oklch L and H (degrees). O(1), exact. */
fun maxChromaSRGB(L: Float, H: Float): Float = maxChromaForMatrix(L, H, SRGB_FROM_LMS)

/** Max chroma for current platform's gamut. */
fun maxChroma(L: Float, H: Float): Float = if (appPlatform.isDesktop) maxChromaSRGB(L, H) else maxChromaP3(L, H)

// oklab → LMS coefficients: l_ = L + a*ka + b*kb
private val LMS_FROM_LAB = arrayOf(
  floatArrayOf(1f,  0.3963377774f,  0.2158037573f),
  floatArrayOf(1f, -0.1055613458f, -0.0638541728f),
  floatArrayOf(1f, -0.0894841775f, -1.2914855480f),
)

// LMS³ → linear Display P3
private val P3_FROM_LMS = arrayOf(
  floatArrayOf( 3.1281105148f, -2.2570749853f,  0.1293047593f),
  floatArrayOf(-1.0911282009f,  2.4132668169f, -0.3221681599f),
  floatArrayOf(-0.0260136845f, -0.5080276339f,  1.5333166364f),
)

// LMS³ → linear sRGB
private val SRGB_FROM_LMS = arrayOf(
  floatArrayOf( 4.0767416621f, -3.3077115913f,  0.2309699292f),
  floatArrayOf(-1.2684380046f,  2.6097574011f, -0.3413193965f),
  floatArrayOf(-0.0041960863f, -0.7034186147f,  1.7076147010f),
)

private fun maxChromaForMatrix(L: Float, H: Float, matrix: Array<FloatArray>): Float {
  val hRad = H * (PI.toFloat() / 180f)
  val cosH = cos(hRad); val sinH = sin(hRad)

  // Each LMS component is linear in C: l_(C) = p + q*C
  val p = FloatArray(3); val q = FloatArray(3)
  for (i in 0..2) {
    p[i] = LMS_FROM_LAB[i][0] * L
    q[i] = LMS_FROM_LAB[i][1] * cosH + LMS_FROM_LAB[i][2] * sinH
  }

  // Each RGB channel = cubic in C. Solve channel=0 and channel=1.
  var minC = Float.MAX_VALUE
  for (ch in 0..2) {
    val (m0, m1, m2) = matrix[ch]
    val a = m0*q[0]*q[0]*q[0] + m1*q[1]*q[1]*q[1] + m2*q[2]*q[2]*q[2]
    val b = 3f*(m0*q[0]*q[0]*p[0] + m1*q[1]*q[1]*p[1] + m2*q[2]*q[2]*p[2])
    val d = 3f*(m0*q[0]*p[0]*p[0] + m1*q[1]*p[1]*p[1] + m2*q[2]*p[2]*p[2])
    val e = m0*p[0]*p[0]*p[0] + m1*p[1]*p[1]*p[1] + m2*p[2]*p[2]*p[2]

    // channel = 0
    for (root in solveCubic(a, b, d, e)) {
      if (root > 1e-6f && root < minC && allChannelsInGamut(root, p, q, matrix, ch)) minC = root
    }
    // channel = 1
    for (root in solveCubic(a, b, d, e - 1f)) {
      if (root > 1e-6f && root < minC && allChannelsInGamut(root, p, q, matrix, ch)) minC = root
    }
  }
  return if (minC == Float.MAX_VALUE) 0f else minC
}

private fun allChannelsInGamut(C: Float, p: FloatArray, q: FloatArray, matrix: Array<FloatArray>, skipCh: Int): Boolean {
  val l = (p[0] + q[0] * C).let { it * it * it }
  val m = (p[1] + q[1] * C).let { it * it * it }
  val s = (p[2] + q[2] * C).let { it * it * it }
  for (ch in 0..2) {
    if (ch == skipCh) continue
    val v = matrix[ch][0] * l + matrix[ch][1] * m + matrix[ch][2] * s
    if (v < -1e-6f || v > 1f + 1e-6f) return false
  }
  return true
}

private fun solveCubic(a: Float, b: Float, c: Float, d: Float): FloatArray {
  if (abs(a) < 1e-10f) {
    if (abs(b) < 1e-10f) {
      return if (abs(c) < 1e-10f) floatArrayOf() else floatArrayOf(-d / c)
    }
    val disc = c * c - 4f * b * d
    if (disc < 0f) return floatArrayOf()
    val sq = sqrt(disc)
    return floatArrayOf((-c + sq) / (2f * b), (-c - sq) / (2f * b))
  }
  val p = b / a; val q = c / a; val r = d / a
  val p1 = q - p * p / 3f
  val q1 = r - p * q / 3f + 2f * p * p * p / 27f
  val disc = q1 * q1 / 4f + p1 * p1 * p1 / 27f

  return when {
    disc > 1e-10f -> {
      val sqD = sqrt(disc)
      val u = cbrt(-q1 / 2f + sqD)
      val v = cbrt(-q1 / 2f - sqD)
      floatArrayOf(u + v - p / 3f)
    }
    abs(disc) <= 1e-10f -> {
      if (abs(q1) < 1e-10f) floatArrayOf(-p / 3f)
      else {
        val u = cbrt(-q1 / 2f)
        floatArrayOf(2f * u - p / 3f, -u - p / 3f)
      }
    }
    else -> {
      val m = 2f * sqrt(-p1 / 3f)
      val theta = acos(3f * q1 / (p1 * m)) / 3f
      floatArrayOf(
        m * cos(theta) - p / 3f,
        m * cos(theta - 2f * PI.toFloat() / 3f) - p / 3f,
        m * cos(theta - 4f * PI.toFloat() / 3f) - p / 3f
      )
    }
  }
}

private fun cbrt(x: Float): Float {
  return if (x >= 0f) x.toDouble().pow(1.0 / 3.0).toFloat()
  else -((-x).toDouble().pow(1.0 / 3.0).toFloat())
}

// ===== Theme color formula =====

data class FormulaSlot(val L: Float, val C: Float, val H: Float) {
  fun toColor(): Color = oklch(L, C, H)
  fun toCodeString(): String = "oklch(${L}f, ${C}f, ${H}f)"
}

data class FormulaResult(
  val background: FormulaSlot,
  val pattern: FormulaSlot,
  val sentMessage: FormulaSlot,
  val sentQuote: FormulaSlot,
  val receivedMessage: FormulaSlot,
  val receivedQuote: FormulaSlot,
)

// ─── LIGHT ───

fun generateSchemeLight(
  hue: Float,
  bgL: Float,
  bgC: Float,
  step: Float,
  patternDepth: Float = 2.5f,
  patternChroma: Float? = null,
  receivedTint: Float = 0.005f,
  saturationScale: Float = 1f,
  contrastScale: Float = 1f,
  patternIntensity: Float = 1f,
): FormulaResult {
  val effBgC = bgC * saturationScale
  val effStep = step * contrastScale
  val effDepth = patternDepth * patternIntensity
  val effPatternC = patternChroma?.let { it * patternIntensity }
  val maxBg = maxChroma(bgL, hue)
  val satRatio = if (maxBg > 0f) effBgC / maxBg else 0f
  val loRatio = 0.35f

  data class Raw(val name: String, val L: Float)
  val slots = listOf(
    Raw("receivedMessage", 1f - receivedTint),
    Raw("receivedQuote", 1f - effStep),
    Raw("sentMessage", bgL + effStep / 3f),
    Raw("background", bgL),
    Raw("sentQuote", bgL - effStep),
    Raw("pattern", bgL - effDepth * effStep),
  )

  val computed = mutableMapOf<String, FormulaSlot>()
  for (slot in slots) {
    val maxC = maxChroma(slot.L, hue)
    val c = when (slot.name) {
      "receivedMessage" -> maxC
      "background" -> effBgC
      "pattern" -> if (effPatternC != null) min(effPatternC, maxC) else {
        if (slot.L >= bgL) maxC * satRatio
        else {
          val t = if (effStep > 0f) min(1f, (bgL - slot.L) / (2.5f * effStep)) else 0f
          maxC * (satRatio - (satRatio - loRatio) * t)
        }
      }
      else -> if (slot.L >= bgL) maxC * satRatio
      else {
        val t = if (effStep > 0f) min(1f, (bgL - slot.L) / (2.5f * effStep)) else 0f
        maxC * (satRatio - (satRatio - loRatio) * t)
      }
    }
    computed[slot.name] = FormulaSlot(slot.L, min(c, maxC), hue)
  }

  return FormulaResult(
    background = computed["background"]!!,
    pattern = computed["pattern"]!!,
    sentMessage = computed["sentMessage"]!!,
    sentQuote = computed["sentQuote"]!!,
    receivedMessage = computed["receivedMessage"]!!,
    receivedQuote = computed["receivedQuote"]!!,
  )
}

// ─── DARK / SIMPLEX / BLACK ───

private data class DarkSlotDef(val lightnessMult: Float, val cluster: String, val intraFactor: Float)

private val DARK_SLOTS = mapOf(
  "background"      to DarkSlotDef(0f,    "muted", 0.90f),
  "receivedMessage"  to DarkSlotDef(2.0f,  "muted", 1.00f),
  "receivedQuote"    to DarkSlotDef(3.0f,  "muted", 1.15f),
  "pattern"         to DarkSlotDef(2.5f,  "color", 0.90f),
  "sentMessage"      to DarkSlotDef(3.5f,  "color", 1.00f),
  "sentQuote"        to DarkSlotDef(5.0f,  "color", 1.15f),
)

private val BLACK_SLOTS = mapOf(
  "background"      to DarkSlotDef(0f,     "muted", 1.0f),
  "receivedMessage"  to DarkSlotDef(3.0f,   "muted", 1.0f),
  "receivedQuote"    to DarkSlotDef(5.5f,   "muted", 1.0f),
  "sentMessage"      to DarkSlotDef(6.0f,   "color", 1.0f),
  "sentQuote"        to DarkSlotDef(8.25f,  "color", 1.362f),
  "pattern"         to DarkSlotDef(9.0f,   "color", 1.490f),
)

fun generateSchemeDark(
  hue: Float,
  bgL: Float = 0.166f,
  step: Float = 0.038f,
  mutedChroma: Float = 0.020f,
  colorChroma: Float = 0.063f,
  saturationScale: Float = 1f,
  contrastScale: Float = 1f,
  patternIntensity: Float = 1f,
): FormulaResult = generateDarkFromSlots(hue, bgL, step, mutedChroma, colorChroma, DARK_SLOTS, false, saturationScale, contrastScale, patternIntensity)

fun generateSchemeBlack(
  hue: Float,
  step: Float = 0.04f,
  colorChroma: Float = 0.0522f,
  saturationScale: Float = 1f,
  contrastScale: Float = 1f,
  patternIntensity: Float = 1f,
): FormulaResult = generateDarkFromSlots(hue, 0f, step, 0f, colorChroma, BLACK_SLOTS, true, saturationScale, contrastScale, patternIntensity)

private fun generateDarkFromSlots(
  hue: Float, bgL: Float, step: Float,
  mutedChroma: Float, colorChroma: Float,
  slotDefs: Map<String, DarkSlotDef>,
  patternPinsToP3: Boolean,
  saturationScale: Float, contrastScale: Float, patternIntensity: Float,
): FormulaResult {
  val effStep = step * contrastScale

  // Baseline chroma per slot
  val baselineC = mutableMapOf<String, Float>()
  for ((name, def) in slotDefs) {
    val clusterC = if (def.cluster == "muted") mutedChroma else colorChroma
    baselineC[name] = clusterC * def.intraFactor
  }
  val bgCAnchor = baselineC["background"]!!

  val computed = mutableMapOf<String, FormulaSlot>()
  for ((name, def) in slotDefs) {
    var lMult = def.lightnessMult
    var baseC = baselineC[name]!!
    if (name == "pattern") {
      lMult *= patternIntensity
      baseC = bgCAnchor + (baseC - bgCAnchor) * patternIntensity
    }
    val L = bgL + lMult * effStep
    val C = when {
      name == "background" -> bgCAnchor
      name == "pattern" && patternPinsToP3 -> maxChroma(L, hue)
      else -> bgCAnchor + (baseC - bgCAnchor) * saturationScale
    }.let { min(it, maxChroma(L, hue)) }
    computed[name] = FormulaSlot(L, C, hue)
  }

  return FormulaResult(
    background = computed["background"]!!,
    pattern = computed["pattern"]!!,
    sentMessage = computed["sentMessage"]!!,
    sentQuote = computed["sentQuote"]!!,
    receivedMessage = computed["receivedMessage"]!!,
    receivedQuote = computed["receivedQuote"]!!,
  )
}
