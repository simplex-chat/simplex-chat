package chat.simplex.app.ui.theme

import android.app.UiModeManager
import android.content.Context
import androidx.compose.foundation.background
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.dp
import chat.simplex.app.SimplexApp
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.Serializable

enum class DefaultTheme {
  SYSTEM, LIGHT, DARK, BLUE;

  // Call in only with base theme, not SYSTEM
  fun hasChangedPrimary(colors: Colors): Boolean {
    return when (this) {
      SYSTEM -> return false
      LIGHT -> colors.primary != LightColorPalette.primary
      DARK -> colors.primary != DarkColorPalette.primary
      BLUE -> colors.primary != BlueColorPalette.primary
    }
  }
}

@Serializable
data class ThemeColors(
  val primary: String? = null
) {
  fun toColors(base: DefaultTheme): Colors = when (base) {
    DefaultTheme.LIGHT -> LightColorPalette.copy(primary = primary?.colorFromReadableHex() ?: LightColorPalette.primary)
    DefaultTheme.DARK -> DarkColorPalette.copy(primary = primary?.colorFromReadableHex() ?: DarkColorPalette.primary)
    DefaultTheme.BLUE -> BlueColorPalette.copy(primary = primary?.colorFromReadableHex() ?: BlueColorPalette.primary)
    // shouldn't be here
    DefaultTheme.SYSTEM -> LightColorPalette.copy(primary = primary?.colorFromReadableHex() ?: LightColorPalette.primary)
  }
}

private fun String.colorFromReadableHex(): Color =
  Color(this.replace("#", "").toLongOrNull(16) ?: Color.White.toArgb().toLong())

@Serializable
data class ThemeOverrides (
  val base: DefaultTheme,
  val colors: ThemeColors
)

fun Modifier.themedBackground(baseTheme: DefaultTheme = CurrentColors.value.base, shape: Shape = RectangleShape): Modifier {
  return if (baseTheme == DefaultTheme.BLUE) {
    this.background(brush = Brush.linearGradient(listOf(Color(0xff0C0B13), Color(0xff151D36)), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f)), shape = shape)
  } else {
    this.background(color = CurrentColors.value.colors.background, shape = shape)
  }
}

val DEFAULT_PADDING = 20.dp
val DEFAULT_SPACE_AFTER_ICON = 4.dp
val DEFAULT_PADDING_HALF = DEFAULT_PADDING / 2
val DEFAULT_BOTTOM_PADDING = 48.dp
val DEFAULT_BOTTOM_BUTTON_PADDING = 20.dp

val DarkColorPalette = darkColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexBlue,
  secondary = DarkGray,
//  background = Color.Black,
  surface = Color(0xFF121212),
//  background = Color(0xFF121212),
//  surface = Color(0xFF121212),
  error = Color.Red,
  onBackground = Color(0xFFFFFBFA),
  onSurface = Color(0xFFFFFBFA),
//  onError: Color = Color.Black,
)
val LightColorPalette = lightColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexBlue,
  secondary = LightGray,
  error = Color.Red,
//  background = Color.White,
  surface = Color.White,
//  onPrimary = Color.White,
//  onSecondary = Color.Black,
//  onBackground = Color.Black,
//  onSurface = Color.Black,
)

val BlueColorPalette = darkColors(
  primary = Color(0xff70F0F9),  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = Color(0xff298AE7),
  secondary = Color(0xff2C464D),
  background = Color(0xff111528),
  //  surface = Color.Black,
  //  background = Color(0xFF121212),
  surface = Color(0xFF1C1C22),
  error = Color.Red,
  onBackground = Color(0xFFFFFBFA),
  onSurface = Color(0xFFFFFBFA),
  //  onError: Color = Color.Black,
)

val CurrentColors: MutableStateFlow<ThemeManager.ActiveTheme> = MutableStateFlow(ThemeManager.currentColors(isInNightMode()))

// Non-@Composable implementation
private fun isInNightMode() =
  (SimplexApp.context.getSystemService(Context.UI_MODE_SERVICE) as UiModeManager).nightMode == UiModeManager.MODE_NIGHT_YES

@Composable
fun isInDarkTheme(): Boolean = !CurrentColors.collectAsState().value.colors.isLight

@Composable
fun SimpleXTheme(darkTheme: Boolean? = null, content: @Composable () -> Unit) {
  LaunchedEffect(darkTheme) {
    // For preview
    if (darkTheme != null)
      CurrentColors.value = ThemeManager.currentColors(darkTheme)
  }
  val systemDark = isSystemInDarkTheme()
  LaunchedEffect(systemDark) {
    if (SimplexApp.context.chatModel.controller.appPrefs.currentTheme.get() == DefaultTheme.SYSTEM.name && CurrentColors.value.colors.isLight == systemDark) {
      // Change active colors from light to dark and back based on system theme
      ThemeManager.applyTheme(DefaultTheme.SYSTEM.name, systemDark)
    }
  }
  val theme by CurrentColors.collectAsState()
  MaterialTheme(
    colors = theme.colors,
    typography = Typography,
    shapes = Shapes,
    content = content
  )
}
