package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import kotlin.math.cos
import kotlin.math.sin

fun oklch(L: Float, C: Float, H: Float, alpha: Float = 1f): Color {
  val hRad = H * (Math.PI.toFloat() / 180f)
  return Color(L, C * cos(hRad), C * sin(hRad), alpha, ColorSpaces.Oklab).convert(ColorSpaces.DisplayP3)
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
