package chat.simplex.common.ui.theme

import androidx.compose.foundation.background
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import chat.simplex.res.MR

enum class DefaultTheme {
  SYSTEM, LIGHT, DARK, SIMPLEX;

  // Call it only with base theme, not SYSTEM
  fun hasChangedAnyColor(colors: Colors, appColors: AppColors, appWallpaper: AppWallpaper): Boolean {
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
        appColors != appPalette ||
        appWallpaper.background != null ||
        appWallpaper.tint != null
  }
}

@Stable
class AppColors(
  title: Color,
  sentMessage: Color,
  receivedMessage: Color
) {
  var title by mutableStateOf(title, structuralEqualityPolicy())
    internal set
  var sentMessage by mutableStateOf(sentMessage, structuralEqualityPolicy())
    internal set
  var receivedMessage by mutableStateOf(receivedMessage, structuralEqualityPolicy())
    internal set

  fun copy(
    title: Color = this.title,
    sentMessage: Color = this.sentMessage,
    receivedMessage: Color = this.receivedMessage,
  ): AppColors = AppColors(
    title,
    sentMessage,
    receivedMessage,
  )

  override fun toString(): String {
    return buildString {
      append("AppColors(")
      append("title=$title, ")
      append("sentMessage=$sentMessage, ")
      append("receivedMessage=$receivedMessage")
      append(")")
    }
  }
}

@Stable
class AppWallpaper(
  background: Color? = null,
  tint: Color? = null,
  type: BackgroundImageType? = null,
) {
  var background by mutableStateOf(background, structuralEqualityPolicy())
    internal set
  var tint by mutableStateOf(tint, structuralEqualityPolicy())
    internal set
  var type by mutableStateOf(type, structuralEqualityPolicy())
    internal set

  fun copy(
    background: Color? = this.background,
    tint: Color? = this.tint,
    type: BackgroundImageType? = this.type,
  ): AppWallpaper = AppWallpaper(
    background,
    tint,
    type,
  )

  override fun toString(): String {
    return buildString {
      append("AppWallpaper(")
      append("background=$background, ")
      append("tint=$tint, ")
      append("type=$type")
      append(")")
    }
  }
}

enum class ThemeColor {
  PRIMARY, PRIMARY_VARIANT, SECONDARY, SECONDARY_VARIANT, BACKGROUND, SURFACE, TITLE, SENT_MESSAGE, RECEIVED_MESSAGE, WALLPAPER_BACKGROUND, WALLPAPER_TINT;

  fun fromColors(colors: Colors, appColors: AppColors, appWallpaper: AppWallpaper): Color? {
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
      WALLPAPER_BACKGROUND -> appWallpaper.background
      WALLPAPER_TINT -> appWallpaper.tint
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
      WALLPAPER_BACKGROUND -> generalGetString(MR.strings.color_wallpaper_background)
      WALLPAPER_TINT -> generalGetString(MR.strings.color_wallpaper_tint)
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
  fun toColors(base: DefaultTheme, backgroundTheme: ThemeColors?): Colors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPalette
      DefaultTheme.DARK -> DarkColorPalette
      DefaultTheme.SIMPLEX -> SimplexColorPalette
      // shouldn't be here
      DefaultTheme.SYSTEM -> LightColorPalette
    }
    return baseColors.copy(
      primary = primary?.colorFromReadableHex() ?: backgroundTheme?.primary?.colorFromReadableHex() ?: baseColors.primary,
      primaryVariant = primaryVariant?.colorFromReadableHex() ?: backgroundTheme?.primaryVariant?.colorFromReadableHex() ?: baseColors.primaryVariant,
      secondary = secondary?.colorFromReadableHex() ?: backgroundTheme?.secondary?.colorFromReadableHex() ?: baseColors.secondary,
      secondaryVariant = secondaryVariant?.colorFromReadableHex() ?: backgroundTheme?.secondaryVariant?.colorFromReadableHex() ?: baseColors.secondaryVariant,
      background = background?.colorFromReadableHex() ?: backgroundTheme?.background?.colorFromReadableHex() ?: baseColors.background,
      surface = surface?.colorFromReadableHex() ?: backgroundTheme?.surface?.colorFromReadableHex() ?: baseColors.surface,
    )
  }

  fun toAppColors(base: DefaultTheme, backgroundTheme: ThemeColors?): AppColors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPaletteApp
      DefaultTheme.DARK -> DarkColorPaletteApp
      DefaultTheme.SIMPLEX -> SimplexColorPaletteApp
      // shouldn't be here
      DefaultTheme.SYSTEM -> LightColorPaletteApp
    }
    return baseColors.copy(
      title = title?.colorFromReadableHex() ?: backgroundTheme?.title?.colorFromReadableHex() ?: baseColors.title,
      sentMessage = sentMessage?.colorFromReadableHex() ?: backgroundTheme?.sentMessage?.colorFromReadableHex() ?: baseColors.sentMessage,
      receivedMessage = receivedMessage?.colorFromReadableHex() ?: backgroundTheme?.receivedMessage?.colorFromReadableHex() ?: baseColors.receivedMessage,
    )
  }

  fun withFilledColors(base: DefaultTheme, backgroundTheme: ThemeColors?): ThemeColors {
    val c = toColors(base, backgroundTheme)
    val ac = toAppColors(base, backgroundTheme)
    return ThemeColors(
      primary = c.primary.toReadableHex(),
      primaryVariant = c.primaryVariant.toReadableHex(),
      secondary = c.secondary.toReadableHex(),
      secondaryVariant = c.secondaryVariant.toReadableHex(),
      background = c.background.toReadableHex(),
      surface = c.surface.toReadableHex(),
      title = ac.title.toReadableHex(),
      sentMessage = ac.sentMessage.toReadableHex(),
      receivedMessage = ac.receivedMessage.toReadableHex(),
    )
  }
}

@Serializable
data class ThemeWallpaper (
  val preset: String? = null,
  val scale: Float = 1f,
  val scaleType: BackgroundImageScaleType? = null,
  val background: String? = null,
  val tint: String? = null,
  val image: String? = null,
  val imageFile: String? = null,
) {
  fun toAppWallpaper(): AppWallpaper {
    return AppWallpaper(
      background = background?.colorFromReadableHex(),
      tint = tint?.colorFromReadableHex(),
      type = if (preset != null) {
        BackgroundImageType.Repeated(filename = preset, scale)
      } else if (imageFile != null) {
        BackgroundImageType.Static(
          filename = imageFile,
          scale,
          scaleType ?: BackgroundImageScaleType.FILL
        )
      } else
        null
    )
  }

  fun withFilledWallpaperBase64(): ThemeWallpaper {
    val aw = toAppWallpaper()
    val type = aw.type
    return ThemeWallpaper(
      image = if (type is BackgroundImageType.Static && type.image != null) resizeImageToStrSize(type.image!!, 5_000_000) else null,
      imageFile = null,
      preset = if (type is BackgroundImageType.Repeated) type.filename else null,
      scale = if (type is BackgroundImageType.Repeated) type.scale else if (type != null) (type as BackgroundImageType.Static).scale else 1f,
      scaleType = if (type is BackgroundImageType.Static) type.scaleType else null,
      background = aw.background?.toReadableHex(),
      tint = aw.tint?.toReadableHex(),
    )
  }

  fun withFilledWallpaperPath(): ThemeWallpaper {
    val aw = toAppWallpaper()
    val type = aw.type
    return ThemeWallpaper(
      image = null,
      imageFile = if (type is BackgroundImageType.Static) type.filename else null,
      preset = if (type is BackgroundImageType.Repeated) type.filename else null,
      scale = if (type is BackgroundImageType.Repeated) type.scale else if (type != null) (type as BackgroundImageType.Static).scale else 1f,
      scaleType = if (type is BackgroundImageType.Static) type.scaleType else null,
      background = aw.background?.toReadableHex(),
      tint = aw.tint?.toReadableHex(),
    )
  }

 fun importFromString(): ThemeWallpaper =
   if (preset == null && image != null) {
     // Need to save image from string and to save its path
     try {
       val parsed = base64ToBitmap(image)
       val filename = saveBackgroundImage(parsed)
       copy(image = filename)
     } catch (e: Exception) {
       ThemeWallpaper()
     }
   } else this

  companion object {
    fun from(type: BackgroundImageType, background: String?, tint: String?): ThemeWallpaper {
      return ThemeWallpaper(
        image = null,
        imageFile = if (type is BackgroundImageType.Static) type.filename else null,
        preset = if (type is BackgroundImageType.Repeated) type.filename else null,
        scale = if (type is BackgroundImageType.Repeated) type.scale else (type as BackgroundImageType.Static).scale,
        scaleType = if (type is BackgroundImageType.Static) type.scaleType else null,
        background = background,
        tint = tint,
      )
    }
  }
}

@Serializable
data class ThemeOverrides (
  val base: DefaultTheme = CurrentColors.value.base,
  val colors: ThemeColors = ThemeColors(),
  val wallpaper: ThemeWallpaper = ThemeWallpaper(),
) {
  fun withUpdatedColor(name: ThemeColor, color: String?): ThemeOverrides {
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
      ThemeColor.WALLPAPER_BACKGROUND -> colors.copy()
      ThemeColor.WALLPAPER_TINT -> colors.copy()
    }, wallpaper = when (name) {
      ThemeColor.WALLPAPER_BACKGROUND -> wallpaper.copy(background = color)
      ThemeColor.WALLPAPER_TINT -> wallpaper.copy(tint = color)
      else -> wallpaper.copy()
    }
    )
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

val DEFAULT_START_MODAL_WIDTH = 388.dp
val DEFAULT_MIN_CENTER_MODAL_WIDTH = 590.dp
val DEFAULT_END_MODAL_WIDTH = 388.dp
val DEFAULT_MAX_IMAGE_WIDTH = 500.dp

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
  sentMessage = SentMessageColor,
  receivedMessage = Color(0x20B1B0B5),
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
  sentMessage = SentMessageColor,
  receivedMessage = Color(0x20B1B0B5),
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
  sentMessage = SentMessageColor,
  receivedMessage = Color(0x20B1B0B5),
)

val CurrentColors: MutableStateFlow<ThemeManager.ActiveTheme> = MutableStateFlow(ThemeManager.currentColors(isInNightMode()))

@Composable
fun isInDarkTheme(): Boolean = !CurrentColors.collectAsState().value.colors.isLight

@Composable
expect fun isSystemInDarkTheme(): Boolean

internal val LocalAppColors = staticCompositionLocalOf { LightColorPaletteApp }
internal val LocalAppWallpaper = staticCompositionLocalOf { AppWallpaper() }

val MaterialTheme.appColors: AppColors
  @Composable
  @ReadOnlyComposable
  get() = LocalAppColors.current

fun AppColors.updateColorsFrom(other: AppColors) {
  title = other.title
  sentMessage = other.sentMessage
  receivedMessage = other.receivedMessage
}

val MaterialTheme.wallpaper: AppWallpaper
  @Composable
  @ReadOnlyComposable
  get() = LocalAppWallpaper.current

fun reactOnDarkThemeChanges(isDark: Boolean) {
  if (ChatController.appPrefs.currentTheme.get() == DefaultTheme.SYSTEM.name && CurrentColors.value.colors.isLight == isDark) {
    // Change active colors from light to dark and back based on system theme
    ThemeManager.applyTheme(DefaultTheme.SYSTEM.name, isDark)
  }
}

@Composable
fun SimpleXTheme(darkTheme: Boolean? = null, content: @Composable () -> Unit) {
  LaunchedEffect(darkTheme) {
    // For preview
    if (darkTheme != null)
      CurrentColors.value = ThemeManager.currentColors(darkTheme)
  }
  val systemDark = isSystemInDarkTheme()
  LaunchedEffect(systemDark) {
    reactOnDarkThemeChanges(systemDark)
  }
  val theme by CurrentColors.collectAsState()
  MaterialTheme(
    colors = theme.colors,
    typography = Typography,
    shapes = Shapes,
    content = {
      CompositionLocalProvider(
        LocalContentColor provides MaterialTheme.colors.onBackground,
        LocalAppColors provides theme.appColors,
        LocalAppWallpaper provides theme.wallpaper,
        content = content)
    }
  )
}

@Composable
fun SimpleXThemeOverride(theme: ThemeManager.ActiveTheme, content: @Composable () -> Unit) {
  MaterialTheme(
    colors = theme.colors,
    typography = Typography,
    shapes = Shapes,
    content = {
      val rememberedAppColors = remember {
        // Explicitly creating a new object here so we don't mutate the initial [appColors]
        // provided, and overwrite the values set in it.
        theme.appColors.copy()
      }.apply { updateColorsFrom(theme.appColors) }
      CompositionLocalProvider(
        LocalContentColor provides MaterialTheme.colors.onBackground,
        LocalAppColors provides rememberedAppColors,
        LocalAppWallpaper provides theme.wallpaper,
        content = content)
    }
  )
}
