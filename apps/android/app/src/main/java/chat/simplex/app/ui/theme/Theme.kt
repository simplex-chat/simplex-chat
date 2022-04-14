package chat.simplex.app.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.*
import androidx.compose.runtime.Composable

private val DarkColorPalette = darkColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexGreen,
  secondary = DarkGray,
//  background = Color.Black,
//  surface = Color.Black,
//  background = Color(0xFF121212),
//  surface = Color(0xFF121212),
//  error = Color(0xFFCF6679),
//  onPrimary = Color.Black,
//  onSecondary = Color.Black,
//  onBackground = Color.White,
//  onSurface = Color.White,
//  onError: Color = Color.Black,
)
private val LightColorPalette = lightColors(
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

@Composable
fun SimpleXTheme(darkTheme: Boolean = isSystemInDarkTheme(), content: @Composable () -> Unit) {
  val colors = if (darkTheme) {
    DarkColorPalette
  } else {
    LightColorPalette
  }

  MaterialTheme(
    colors = colors,
    typography = Typography,
    shapes = Shapes,
    content = content
  )
}