package chat.simplex.app.ui.theme

import android.app.UiModeManager
import android.content.Context
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.app.SimplexApp
import kotlinx.coroutines.flow.MutableStateFlow

enum class DefaultTheme {
  SYSTEM, DARK, LIGHT
}

val DEFAULT_PADDING = 16.dp
val DEFAULT_SPACE_AFTER_ICON = 4.dp
val DEFAULT_PADDING_HALF = DEFAULT_PADDING / 2

val DarkColorPalette = darkColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexGreen,
  secondary = DarkGray,
//  background = Color.Black,
//  surface = Color.Black,
//  background = Color(0xFF121212),
//  surface = Color(0xFF121212),
//  error = Color(0xFFCF6679),
  onBackground = Color(0xFFFFFBFA),
  onSurface = Color(0xFFFFFBFA),
//  onError: Color = Color.Black,
)
val LightColorPalette = lightColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexGreen,
  secondary = LightGray,
//  background = Color.White,
//  surface = Color.White
//  onPrimary = Color.White,
//  onSecondary = Color.Black,
//  onBackground = Color.Black,
//  onSurface = Color.Black,
)

val CurrentColors: MutableStateFlow<Pair<Colors, DefaultTheme>> = MutableStateFlow(ThemeManager.currentColors(isInNightMode()))

// Non-@Composable implementation
private fun isInNightMode() =
  (SimplexApp.context.getSystemService(Context.UI_MODE_SERVICE) as UiModeManager).nightMode == UiModeManager.MODE_NIGHT_YES

@Composable
fun isInDarkTheme(): Boolean = !CurrentColors.collectAsState().value.first.isLight

@Composable
fun SimpleXTheme(darkTheme: Boolean? = null, content: @Composable () -> Unit) {
  LaunchedEffect(darkTheme) {
    // For preview
    if (darkTheme != null)
      CurrentColors.value = ThemeManager.currentColors(darkTheme)
  }
  val systemDark = isSystemInDarkTheme()
  LaunchedEffect(systemDark) {
    if (CurrentColors.value.second == DefaultTheme.SYSTEM && CurrentColors.value.first.isLight == systemDark) {
      // Change active colors from light to dark and back based on system theme
      ThemeManager.applyTheme(DefaultTheme.SYSTEM.name, systemDark)
    }
  }
  val theme by CurrentColors.collectAsState()
  MaterialTheme(
    colors = theme.first,
    typography = Typography,
    shapes = Shapes,
    content = content
  )
}