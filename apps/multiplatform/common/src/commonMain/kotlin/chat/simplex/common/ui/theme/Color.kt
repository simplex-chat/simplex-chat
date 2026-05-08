package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.colorspace.ColorSpaces
import chat.simplex.common.views.helpers.PresetWallpaper
import chat.simplex.common.views.helpers.WallpaperType
import chat.simplex.common.views.helpers.mixWith
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

/** Background color for panels (top app bar, bottom nav, status bar overlay).
 *  When current wallpaper is a preset and theme is LIGHT or DARK, panel gets a subtle hue tint
 *  matching the wallpaper. Otherwise falls back to the existing bg.mixWith(onBg, 0.97f) elevation.
 *  BLACK and SIMPLEX themes are not tinted (BLACK keeps pure dark, SIMPLEX has its own custom panel). */
@Composable
fun panelBackgroundColor(): Color =
  currentWallpaperPanelTint()
    ?: MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)

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
