package chat.simplex.app.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import kotlinx.coroutines.flow.MutableStateFlow

enum class DefaultTheme {
  SYSTEM, DARK, LIGHT
}

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

val CurrentColors: MutableStateFlow<Pair<Colors, DefaultTheme>> = MutableStateFlow(ThemeManager.currentColors(true))

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