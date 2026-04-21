package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import chat.simplex.common.views.helpers.mixWith
import kotlin.math.cos
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.sin

/** Create a Display P3 Color from oklch components. H in degrees. */
fun oklch(L: Float, C: Float, H: Float, alpha: Float = 1f): Color {
  val hRad = H * (Math.PI.toFloat() / 180f)
  val a = C * cos(hRad)
  val b = C * sin(hRad)
  // oklab → LMS (Ottosson 2021)
  val l_ = L + 0.3963377774f * a + 0.2158037573f * b
  val m_ = L - 0.1055613458f * a - 0.0638541728f * b
  val s_ = L - 0.0894841775f * a - 1.2914855480f * b
  val l = l_ * l_ * l_
  val m = m_ * m_ * m_
  val s = s_ * s_ * s_
  // LMS → linear Display P3 (direct, no sRGB clamping)
  val r =  3.1281105148f * l - 2.2570749853f * m + 0.1293047593f * s
  val g = -1.0911282009f * l + 2.4132668169f * m - 0.3221681599f * s
  val bl = -0.0260136845f * l - 0.5080276339f * m + 1.5333166364f * s
  // linear P3 → gamma-encoded P3 (same transfer function as sRGB)
  fun gammaEncode(x: Float) = if (x >= 0.0031308f) 1.055f * x.pow(1f / 2.4f) - 0.055f else 12.92f * x
  return Color(
    red = gammaEncode(r.coerceIn(0f, 1f)),
    green = gammaEncode(g.coerceIn(0f, 1f)),
    blue = gammaEncode(bl.coerceIn(0f, 1f)),
    alpha = alpha,
    colorSpace = ColorSpaces.DisplayP3
  )
}

val Purple200 = Color(0xFFBB86FC)
val Purple500 = Color(0xFF6200EE)
val Purple700 = Color(0xFF3700B3)
val Teal200 = Color(0xFF03DAC5)
val Gray = Color(0x22222222)
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
