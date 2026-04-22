package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import kotlin.math.cos
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.sin

fun oklch(L: Float, C: Float, H: Float, alpha: Float = 1f): Color {
  val hRad = H * (Math.PI.toFloat() / 180f)
  val cosH = cos(hRad)
  val sinH = sin(hRad)

  fun linearP3(c: Float): Triple<Float, Float, Float> {
    val a = c * cosH
    val b = c * sinH
    // oklab → LMS (Ottosson 2021)
    val l_ = L + 0.3963377774f * a + 0.2158037573f * b
    val m_ = L - 0.1055613458f * a - 0.0638541728f * b
    val s_ = L - 0.0894841775f * a - 1.2914855480f * b
    val l = l_ * l_ * l_
    val m = m_ * m_ * m_
    val s = s_ * s_ * s_
    // LMS → linear Display P3
    return Triple(
      3.1281105148f * l - 2.2570749853f * m + 0.1293047593f * s,
      -1.0911282009f * l + 2.4132668169f * m - 0.3221681599f * s,
      -0.0260136845f * l - 0.5080276339f * m + 1.5333166364f * s
    )
  }

  fun inGamut(r: Float, g: Float, b: Float) = r in 0f..1f && g in 0f..1f && b in 0f..1f

  // linear P3 → gamma-encoded P3 (same transfer function as sRGB)
  fun gammaEncode(x: Float): Float =
    if (x >= 0.0031308f) 1.055f * min(x, 1f).pow(1f / 2.4f) - 0.055f
    else 12.92f * max(x, 0f)

  var (r, g, b) = linearP3(C)
  if (!inGamut(r, g, b)) {
    var lo = 0f; var hi = C
    while (hi - lo > 1e-5f) {
      val mid = (lo + hi) / 2
      val (mr, mg, mb) = linearP3(mid)
      if (inGamut(mr, mg, mb)) { lo = mid; r = mr; g = mg; b = mb }
      else hi = mid
    }
  }

  return Color(
    red = gammaEncode(r),
    green = gammaEncode(g),
    blue = gammaEncode(b),
    alpha = alpha,
    colorSpace = ColorSpaces.DisplayP3
  )
}

val Indigo = Color(0xFF9966FF)
val SimplexBlue = Color(0, 136, 255, 255)  // If this value changes also need to update #0088ff in string resource files
val SimplexGreen = Color(77, 218, 103, 255)
val SecretColor = Color(0x40808080)
val LightGray = Color(241, 242, 246, 255)
val DarkGray = Color(43, 44, 46, 255)
val HighOrLowlight = Color(139, 135, 134, 255)
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
