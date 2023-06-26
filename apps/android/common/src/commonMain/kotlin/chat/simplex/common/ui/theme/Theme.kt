package chat.simplex.common.ui.theme

import androidx.compose.foundation.background
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController
import chat.simplex.common.platform.isInNightMode
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import com.icerockdev.library.MR

enum class DefaultTheme {
  SYSTEM, LIGHT, DARK, SIMPLEX;

  // Call it only with base theme, not SYSTEM
  fun hasChangedAnyColor(colors: Colors, appColors: AppColors): Boolean {
    val palette = when (this) {
      SYSTEM -> return false
      LIGHT -> LightColorPalette
      DARK -> DarkColorPalette
      SIMPLEX -> SimplexColorPalette
    }
    val appPalette = when (this) {
      SYSTEM -> return false
      LIGHT -> LightColorPaletteApp
      DARK -> DarkColorPaletteApp
      SIMPLEX -> SimplexColorPaletteApp
    }
    return colors.primary != palette.primary ||
        colors.primaryVariant != palette.primaryVariant ||
        colors.secondary != palette.secondary ||
        colors.secondaryVariant != palette.secondaryVariant ||
        colors.background != palette.background ||
        colors.surface != palette.surface ||
        appColors != appPalette
  }
}

data class AppColors(
  val title: Color,
  val sentMessage: Color,
  val receivedMessage: Color
)

enum class ThemeColor {
  PRIMARY, PRIMARY_VARIANT, SECONDARY, SECONDARY_VARIANT, BACKGROUND, SURFACE, TITLE, SENT_MESSAGE, RECEIVED_MESSAGE;

  fun fromColors(colors: Colors, appColors: AppColors): Color {
    return when (this) {
      PRIMARY -> colors.primary
      PRIMARY_VARIANT -> colors.primaryVariant
      SECONDARY -> colors.secondary
      SECONDARY_VARIANT -> colors.secondaryVariant
      BACKGROUND -> colors.background
      SURFACE -> colors.surface
      TITLE -> appColors.title
      SENT_MESSAGE -> appColors.sentMessage
      RECEIVED_MESSAGE -> appColors.receivedMessage
    }
  }

  val text: String
    get() = when (this) {
      PRIMARY -> generalGetString(MR.strings.color_primary)
      PRIMARY_VARIANT -> generalGetString(MR.strings.color_primary_variant)
      SECONDARY -> generalGetString(MR.strings.color_secondary)
      SECONDARY_VARIANT -> generalGetString(MR.strings.color_secondary_variant)
      BACKGROUND -> generalGetString(MR.strings.color_background)
      SURFACE -> generalGetString(MR.strings.color_surface)
      TITLE -> generalGetString(MR.strings.color_title)
      SENT_MESSAGE -> generalGetString(MR.strings.color_sent_message)
      RECEIVED_MESSAGE -> generalGetString(MR.strings.color_received_message)
    }
}

@Serializable
data class ThemeColors(
  @SerialName("accent")
  val primary: String? = null,
  @SerialName("accentVariant")
  val primaryVariant: String? = null,
  val secondary: String? = null,
  val secondaryVariant: String? = null,
  val background: String? = null,
  @SerialName("menus")
  val surface: String? = null,
  val title: String? = null,
  val sentMessage: String? = null,
  val receivedMessage: String? = null,
) {
  fun toColors(base: DefaultTheme): Colors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPalette
      DefaultTheme.DARK -> DarkColorPalette
      DefaultTheme.SIMPLEX -> SimplexColorPalette
      // shouldn't be here
      DefaultTheme.SYSTEM -> LightColorPalette
    }
    return baseColors.copy(
      primary = primary?.colorFromReadableHex() ?: baseColors.primary,
      primaryVariant = primaryVariant?.colorFromReadableHex() ?: baseColors.primaryVariant,
      secondary = secondary?.colorFromReadableHex() ?: baseColors.secondary,
      secondaryVariant = secondaryVariant?.colorFromReadableHex() ?: baseColors.secondaryVariant,
      background = background?.colorFromReadableHex() ?: baseColors.background,
      surface = surface?.colorFromReadableHex() ?: baseColors.surface,
    )
  }

  fun toAppColors(base: DefaultTheme): AppColors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPaletteApp
      DefaultTheme.DARK -> DarkColorPaletteApp
      DefaultTheme.SIMPLEX -> SimplexColorPaletteApp
      // shouldn't be here
      DefaultTheme.SYSTEM -> LightColorPaletteApp
    }
    return baseColors.copy(
      title = title?.colorFromReadableHex() ?: baseColors.title,
      sentMessage = sentMessage?.colorFromReadableHex() ?: baseColors.sentMessage,
      receivedMessage = receivedMessage?.colorFromReadableHex() ?: baseColors.receivedMessage,
    )
  }

  fun withFilledColors(base: DefaultTheme): ThemeColors {
    val c = toColors(base)
    val ac = toAppColors(base)
    return ThemeColors(
      primary = c.primary.toReadableHex(),
      primaryVariant = c.primaryVariant.toReadableHex(),
      secondary = c.secondary.toReadableHex(),
      secondaryVariant = c.secondaryVariant.toReadableHex(),
      background = c.background.toReadableHex(),
      surface = c.surface.toReadableHex(),
      title = ac.title.toReadableHex(),
      sentMessage = ac.sentMessage.toReadableHex(),
      receivedMessage = ac.receivedMessage.toReadableHex()
    )
  }
}

private fun String.colorFromReadableHex(): Color =
  Color(this.replace("#", "").toLongOrNull(16) ?: Color.White.toArgb().toLong())

private fun Color.toReadableHex(): String = "#" + Integer.toHexString(toArgb())

@Serializable
data class ThemeOverrides (
  val base: DefaultTheme,
  val colors: ThemeColors
) {
  fun withUpdatedColor(name: ThemeColor, color: String): ThemeOverrides {
    return copy(colors = when (name) {
      ThemeColor.PRIMARY -> colors.copy(primary = color)
      ThemeColor.PRIMARY_VARIANT -> colors.copy(primaryVariant = color)
      ThemeColor.SECONDARY -> colors.copy(secondary = color)
      ThemeColor.SECONDARY_VARIANT -> colors.copy(secondaryVariant = color)
      ThemeColor.BACKGROUND -> colors.copy(background = color)
      ThemeColor.SURFACE -> colors.copy(surface = color)
      ThemeColor.TITLE -> colors.copy(title = color)
      ThemeColor.SENT_MESSAGE -> colors.copy(sentMessage = color)
      ThemeColor.RECEIVED_MESSAGE -> colors.copy(receivedMessage = color)
    })
  }
}

fun Modifier.themedBackground(baseTheme: DefaultTheme = CurrentColors.value.base, shape: Shape = RectangleShape): Modifier {
  return if (baseTheme == DefaultTheme.SIMPLEX) {
    this.background(brush = Brush.linearGradient(
      listOf(
        CurrentColors.value.colors.background.darker(0.4f),
        CurrentColors.value.colors.background.lighter(0.4f)
      ),
      Offset(0f, Float.POSITIVE_INFINITY),
      Offset(Float.POSITIVE_INFINITY, 0f)
    ), shape = shape)
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
  secondary = HighOrLowlight,
  secondaryVariant = DarkGray,
//  background = Color.Black,
  surface = Color(0xFF222222),
//  background = Color(0xFF121212),
//  surface = Color(0xFF121212),
  error = Color.Red,
  onBackground = Color(0xFFFFFBFA),
  onSurface = Color(0xFFFFFBFA),
//  onError: Color = Color.Black,
)
val DarkColorPaletteApp = AppColors(
  title = SimplexBlue,
  sentMessage = Color(0x1E45B8FF),
  receivedMessage = Color(0x20B1B0B5)
)

val LightColorPalette = lightColors(
  primary = SimplexBlue,  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = SimplexBlue,
  secondary = HighOrLowlight,
  secondaryVariant = LightGray,
  error = Color.Red,
//  background = Color.White,
  surface = Color.White,
//  onPrimary = Color.White,
//  onSecondary = Color.Black,
//  onBackground = Color.Black,
//  onSurface = Color.Black,
)
val LightColorPaletteApp = AppColors(
  title = SimplexBlue,
  sentMessage = Color(0x1E45B8FF),
  receivedMessage = Color(0x20B1B0B5)
)

val SimplexColorPalette = darkColors(
  primary = Color(0xFF70F0F9),  // If this value changes also need to update #0088ff in string resource files
  primaryVariant = Color(0xFF1298A5),
  secondary = HighOrLowlight,
  secondaryVariant = Color(0xFF2C464D),
  background = Color(0xFF111528),
  //  surface = Color.Black,
  //  background = Color(0xFF121212),
  surface = Color(0xFF121C37),
  error = Color.Red,
//  onBackground = Color(0xFFFFFBFA),
//  onSurface = Color(0xFFFFFBFA),
  //  onError: Color = Color.Black,
)
val SimplexColorPaletteApp = AppColors(
  title = Color(0xFF267BE5),
  sentMessage = Color(0x1E45B8FF),
  receivedMessage = Color(0x20B1B0B5)
)

val CurrentColors: MutableStateFlow<ThemeManager.ActiveTheme> = MutableStateFlow(ThemeManager.currentColors(isInNightMode()))

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
    if (ChatController.appPrefs.currentTheme.get() == DefaultTheme.SYSTEM.name && CurrentColors.value.colors.isLight == systemDark) {
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
