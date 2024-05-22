package chat.simplex.common.ui.theme

import androidx.compose.foundation.background
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import chat.simplex.res.MR
import kotlinx.serialization.Transient
import java.util.UUID

enum class DefaultTheme {
  LIGHT, DARK, SIMPLEX, BLACK;

  companion object {
    const val SYSTEM_THEME_NAME: String = "SYSTEM"
  }

  val themeName: String
    get() = name

  val mode: DefaultThemeMode get() = if (this == LIGHT) DefaultThemeMode.LIGHT else DefaultThemeMode.DARK

  // Call it only with base theme, not SYSTEM
  fun hasChangedAnyColor(colors: Colors, appColors: AppColors, appWallpaper: AppWallpaper): Boolean {
    val palette = when (this) {
      LIGHT -> LightColorPalette
      DARK -> DarkColorPalette
      SIMPLEX -> SimplexColorPalette
      BLACK -> BlackColorPalette
    }
    val appPalette = when (this) {
      LIGHT -> LightColorPaletteApp
      DARK -> DarkColorPaletteApp
      SIMPLEX -> SimplexColorPaletteApp
      BLACK -> BlackColorPaletteApp
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

@Serializable
enum class DefaultThemeMode {
  @SerialName("light") LIGHT,
  @SerialName("dark") DARK
}

@Stable
class AppColors(
  title: Color,
  primaryVariant2: Color,
  sentMessage: Color,
  sentQuote: Color,
  receivedMessage: Color,
  receivedQuote: Color,
) {
  var title by mutableStateOf(title, structuralEqualityPolicy())
    internal set
  var primaryVariant2 by mutableStateOf(primaryVariant2, structuralEqualityPolicy())
    internal set
  var sentMessage by mutableStateOf(sentMessage, structuralEqualityPolicy())
    internal set
  var sentQuote by mutableStateOf(sentQuote, structuralEqualityPolicy())
    internal set
  var receivedMessage by mutableStateOf(receivedMessage, structuralEqualityPolicy())
    internal set
  var receivedQuote by mutableStateOf(receivedQuote, structuralEqualityPolicy())
    internal set

  fun copy(
    title: Color = this.title,
    primaryVariant2: Color = this.primaryVariant2,
    sentMessage: Color = this.sentMessage,
    sentQuote: Color = this.sentQuote,
    receivedMessage: Color = this.receivedMessage,
    receivedQuote: Color = this.receivedQuote,
  ): AppColors = AppColors(
    title,
    primaryVariant2,
    sentMessage,
    sentQuote,
    receivedMessage,
    receivedQuote,
  )

  override fun toString(): String {
    return buildString {
      append("AppColors(")
      append("title=$title, ")
      append("primaryVariant2=$primaryVariant2, ")
      append("sentMessage=$sentMessage, ")
      append("sentQuote=$sentQuote, ")
      append("receivedMessage=$receivedMessage, ")
      append("receivedQuote=$receivedQuote")
      append(")")
    }
  }
}

@Stable
class AppWallpaper(
  background: Color? = null,
  tint: Color? = null,
  type: BackgroundImageType = BackgroundImageType.Empty,
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
    type: BackgroundImageType = this.type,
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
  PRIMARY, PRIMARY_VARIANT, SECONDARY, SECONDARY_VARIANT, BACKGROUND, SURFACE, TITLE, SENT_MESSAGE, SENT_QUOTE, RECEIVED_MESSAGE, RECEIVED_QUOTE, PRIMARY_VARIANT2, WALLPAPER_BACKGROUND, WALLPAPER_TINT;

  fun fromColors(colors: Colors, appColors: AppColors, appWallpaper: AppWallpaper): Color? {
    return when (this) {
      PRIMARY -> colors.primary
      PRIMARY_VARIANT -> colors.primaryVariant
      SECONDARY -> colors.secondary
      SECONDARY_VARIANT -> colors.secondaryVariant
      BACKGROUND -> colors.background
      SURFACE -> colors.surface
      TITLE -> appColors.title
      PRIMARY_VARIANT2 -> appColors.primaryVariant2
      SENT_MESSAGE -> appColors.sentMessage
      SENT_QUOTE -> appColors.sentQuote
      RECEIVED_MESSAGE -> appColors.receivedMessage
      RECEIVED_QUOTE -> appColors.receivedQuote
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
      PRIMARY_VARIANT2 -> generalGetString(MR.strings.color_primary_variant2)
      SENT_MESSAGE -> generalGetString(MR.strings.color_sent_message)
      SENT_QUOTE -> generalGetString(MR.strings.color_sent_quote)
      RECEIVED_MESSAGE -> generalGetString(MR.strings.color_received_message)
      RECEIVED_QUOTE -> generalGetString(MR.strings.color_received_quote)
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
  @SerialName("accentVariant2")
  val primaryVariant2: String? = null,
  val sentMessage: String? = null,
  @SerialName("sentReply")
  val sentQuote: String? = null,
  val receivedMessage: String? = null,
  @SerialName("receivedReply")
  val receivedQuote: String? = null,
) {
  companion object {
    fun from(colors: Colors, appColors: AppColors): ThemeColors =
      ThemeColors(
        primary = colors.primary.toReadableHex(),
        primaryVariant = colors.primaryVariant.toReadableHex(),
        secondary = colors.secondary.toReadableHex(),
        secondaryVariant = colors.secondaryVariant.toReadableHex(),
        background = colors.background.toReadableHex(),
        surface = colors.surface.toReadableHex(),
        title = appColors.title.toReadableHex(),
        primaryVariant2 = appColors.primaryVariant2.toReadableHex(),
        sentMessage = appColors.sentMessage.toReadableHex(),
        sentQuote = appColors.sentQuote.toReadableHex(),
        receivedMessage = appColors.receivedMessage.toReadableHex(),
        receivedQuote = appColors.receivedQuote.toReadableHex(),
      )
  }
}

@Serializable
data class ThemeWallpaper (
  val preset: String? = null,
  val scale: Float? = null,
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
          scaleType
        )
      } else {
        BackgroundImageType.Empty
      }
    )
  }

  // LALAL
  fun withFilledWallpaperBase64(): ThemeWallpaper {
    val aw = toAppWallpaper()
    val type = aw.type
    return ThemeWallpaper(
      image = if (type is BackgroundImageType.Static && type.image != null) resizeImageToStrSize(type.image!!, 5_000_000) else null,
      imageFile = null,
      preset = if (type is BackgroundImageType.Repeated) type.filename else null,
      scale = if (type is BackgroundImageType.Repeated) type.scale else if (type is BackgroundImageType.Static) type.scale else 1f,
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
      scale = if (scale == null) null else if (type is BackgroundImageType.Repeated) type.scale else if (type is BackgroundImageType.Static) scale else null,
      scaleType = if (scaleType == null) null else if (type is BackgroundImageType.Static) type.scaleType else null,
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
       copy(image = null, imageFile = filename)
     } catch (e: Exception) {
       Log.e(TAG, "Error while parsing/copying the image: ${e.stackTraceToString()}")
       ThemeWallpaper()
     }
   } else this

  companion object {
    fun from(type: BackgroundImageType, background: String?, tint: String?): ThemeWallpaper {
      return ThemeWallpaper(
        image = null,
        imageFile = if (type is BackgroundImageType.Static) type.filename else null,
        preset = if (type is BackgroundImageType.Repeated) type.filename else null,
        scale = if (type is BackgroundImageType.Repeated) type.scale else if (type is BackgroundImageType.Static) type.scale else null,
        scaleType = if (type is BackgroundImageType.Static) type.scaleType else null,
        background = background,
        tint = tint,
      )
    }
  }
}

@Serializable
data class ThemesFile(
  val themes: List<ThemeOverrides> = emptyList()
)

@Serializable
data class ThemeOverrides (
  val themeId: String = UUID.randomUUID().toString(),
  val base: DefaultTheme,// = DefaultTheme.LIGHT,
  val colors: ThemeColors = ThemeColors(),
  val wallpaper: ThemeWallpaper? = null,
) {

  fun isSame(type: BackgroundImageType?, themeName: String): Boolean =
    (
        (wallpaper?.preset != null && type is BackgroundImageType.Repeated && wallpaper.preset == type.filename) ||
        (wallpaper?.imageFile != null && type is BackgroundImageType.Static) ||
        (wallpaper?.preset == null && wallpaper?.imageFile == null && (type == BackgroundImageType.Empty || type == null))
    ) && base.themeName == themeName

  fun withUpdatedColor(name: ThemeColor, color: String?): ThemeOverrides {
    return copy(
      colors = when (name) {
        ThemeColor.PRIMARY -> colors.copy(primary = color)
        ThemeColor.PRIMARY_VARIANT -> colors.copy(primaryVariant = color)
        ThemeColor.SECONDARY -> colors.copy(secondary = color)
        ThemeColor.SECONDARY_VARIANT -> colors.copy(secondaryVariant = color)
        ThemeColor.BACKGROUND -> colors.copy(background = color)
        ThemeColor.SURFACE -> colors.copy(surface = color)
        ThemeColor.TITLE -> colors.copy(title = color)
        ThemeColor.PRIMARY_VARIANT2 -> colors.copy(primaryVariant2 = color)
        ThemeColor.SENT_MESSAGE -> colors.copy(sentMessage = color)
        ThemeColor.SENT_QUOTE -> colors.copy(sentQuote = color)
        ThemeColor.RECEIVED_MESSAGE -> colors.copy(receivedMessage = color)
        ThemeColor.RECEIVED_QUOTE -> colors.copy(receivedQuote = color)
        ThemeColor.WALLPAPER_BACKGROUND -> colors.copy()
        ThemeColor.WALLPAPER_TINT -> colors.copy()
      }, wallpaper = when (name) {
        ThemeColor.WALLPAPER_BACKGROUND -> wallpaper?.copy(background = color)
        ThemeColor.WALLPAPER_TINT -> wallpaper?.copy(tint = color)
        else -> wallpaper?.copy()
      }
    )
  }

  fun toColors(base: DefaultTheme, perChatTheme: ThemeColors?, perUserTheme: ThemeColors?, presetWallpaperTheme: ThemeColors?): Colors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPalette
      DefaultTheme.DARK -> DarkColorPalette
      DefaultTheme.SIMPLEX -> SimplexColorPalette
      DefaultTheme.BLACK -> BlackColorPalette
    }
    return baseColors.copy(
      primary = perChatTheme?.primary?.colorFromReadableHex() ?: perUserTheme?.primary?.colorFromReadableHex() ?: colors.primary?.colorFromReadableHex() ?: presetWallpaperTheme?.primary?.colorFromReadableHex() ?: baseColors.primary,
      primaryVariant = perChatTheme?.primaryVariant?.colorFromReadableHex() ?: perUserTheme?.primaryVariant?.colorFromReadableHex() ?: colors.primaryVariant?.colorFromReadableHex() ?: presetWallpaperTheme?.primaryVariant?.colorFromReadableHex() ?: baseColors.primaryVariant,
      secondary = perChatTheme?.secondary?.colorFromReadableHex() ?: perUserTheme?.secondary?.colorFromReadableHex() ?: colors.secondary?.colorFromReadableHex() ?: presetWallpaperTheme?.secondary?.colorFromReadableHex() ?: baseColors.secondary,
      secondaryVariant = perChatTheme?.secondaryVariant?.colorFromReadableHex() ?: perUserTheme?.secondaryVariant?.colorFromReadableHex() ?: colors.secondaryVariant?.colorFromReadableHex() ?: presetWallpaperTheme?.secondaryVariant?.colorFromReadableHex() ?: baseColors.secondaryVariant,
      background = perChatTheme?.background?.colorFromReadableHex() ?: perUserTheme?.background?.colorFromReadableHex() ?: colors.background?.colorFromReadableHex() ?: presetWallpaperTheme?.background?.colorFromReadableHex() ?: baseColors.background,
      surface = perChatTheme?.surface?.colorFromReadableHex() ?: perUserTheme?.surface?.colorFromReadableHex() ?: colors.surface?.colorFromReadableHex() ?: presetWallpaperTheme?.surface?.colorFromReadableHex() ?: baseColors.surface,
    )
  }

  fun toAppColors(base: DefaultTheme, perChatTheme: ThemeColors?, perChatWallpaperType: BackgroundImageType?, perUserTheme: ThemeColors?, perUserWallpaperType: BackgroundImageType?, presetWallpaperTheme: ThemeColors?): AppColors {
    val baseColors = when (base) {
      DefaultTheme.LIGHT -> LightColorPaletteApp
      DefaultTheme.DARK -> DarkColorPaletteApp
      DefaultTheme.SIMPLEX -> SimplexColorPaletteApp
      DefaultTheme.BLACK -> BlackColorPaletteApp
    }

    val sentMessageFallback = colors.sentMessage?.colorFromReadableHex() ?: presetWallpaperTheme?.sentMessage?.colorFromReadableHex() ?: baseColors.sentMessage
    val sentQuoteFallback = colors.sentQuote?.colorFromReadableHex() ?: presetWallpaperTheme?.sentQuote?.colorFromReadableHex() ?: baseColors.sentQuote
    val receivedMessageFallback = colors.receivedMessage?.colorFromReadableHex() ?: presetWallpaperTheme?.receivedMessage?.colorFromReadableHex() ?: baseColors.receivedMessage
    val receivedQuoteFallback = colors.receivedQuote?.colorFromReadableHex() ?: presetWallpaperTheme?.receivedQuote?.colorFromReadableHex() ?: baseColors.receivedQuote
    return baseColors.copy(
      title = perChatTheme?.title?.colorFromReadableHex() ?: perUserTheme?.title?.colorFromReadableHex() ?: colors.title?.colorFromReadableHex() ?: presetWallpaperTheme?.title?.colorFromReadableHex() ?: baseColors.title,
      primaryVariant2 = perChatTheme?.primaryVariant2?.colorFromReadableHex() ?: perUserTheme?.primaryVariant2?.colorFromReadableHex() ?: colors.primaryVariant2?.colorFromReadableHex() ?: presetWallpaperTheme?.primaryVariant2?.colorFromReadableHex() ?: baseColors.primaryVariant2,
      sentMessage = if (perChatTheme?.sentMessage != null) perChatTheme.sentMessage.colorFromReadableHex()
        else if (perUserTheme != null && (perChatWallpaperType == null || perUserWallpaperType == null || perChatWallpaperType.sameType(perUserWallpaperType))) perUserTheme.sentMessage?.colorFromReadableHex() ?: sentMessageFallback
        else sentMessageFallback,
      sentQuote = if (perChatTheme?.sentQuote != null) perChatTheme.sentQuote.colorFromReadableHex()
        else if (perUserTheme != null && (perChatWallpaperType == null || perUserWallpaperType == null || perChatWallpaperType.sameType(perUserWallpaperType))) perUserTheme.sentQuote?.colorFromReadableHex() ?: sentQuoteFallback
        else sentQuoteFallback,
      receivedMessage = if (perChatTheme?.receivedMessage != null) perChatTheme.receivedMessage.colorFromReadableHex()
        else if (perUserTheme != null && (perChatWallpaperType == null || perUserWallpaperType == null || perChatWallpaperType.sameType(perUserWallpaperType))) perUserTheme.receivedMessage?.colorFromReadableHex() ?: receivedMessageFallback
        else receivedMessageFallback,
      receivedQuote = if (perChatTheme?.receivedQuote != null) perChatTheme.receivedQuote.colorFromReadableHex()
        else if (perUserTheme != null && (perChatWallpaperType == null || perUserWallpaperType == null || perChatWallpaperType.sameType(perUserWallpaperType))) perUserTheme.receivedQuote?.colorFromReadableHex() ?: receivedQuoteFallback
        else receivedQuoteFallback,
    )
  }

  fun toAppWallpaper(themeOverridesForType: BackgroundImageType?, perChatTheme: ThemeModeOverride?, perUserTheme: ThemeModeOverride?, materialBackgroundColor: Color): AppWallpaper {
    val mainType = when {
      themeOverridesForType != null -> themeOverridesForType
      // type can be null if override is empty `"wallpaper": "{}"`, in this case no wallpaper is needed, empty.
      // It's not null to override upper level wallpaper
      perChatTheme?.wallpaper != null -> perChatTheme.wallpaper.toAppWallpaper().type
      perUserTheme?.wallpaper != null -> perUserTheme.wallpaper.toAppWallpaper().type
      else -> wallpaper?.toAppWallpaper()?.type ?: return AppWallpaper()
    }
    val first: ThemeWallpaper? = if (mainType.sameType(perChatTheme?.wallpaper?.toAppWallpaper()?.type)) perChatTheme?.wallpaper else null
    val second: ThemeWallpaper? = if (mainType.sameType(perUserTheme?.wallpaper?.toAppWallpaper()?.type)) perUserTheme?.wallpaper else null
    val third: ThemeWallpaper? = if (mainType.sameType(this.wallpaper?.toAppWallpaper()?.type)) this.wallpaper else null

    return AppWallpaper(type = when (mainType) {
        is BackgroundImageType.Repeated -> mainType.copy(
          scale = mainType.scale ?: first?.scale ?: second?.scale ?: third?.scale
        )
        is BackgroundImageType.Static -> mainType.copy(
          scale = if (themeOverridesForType == null) mainType.scale ?: first?.scale ?: second?.scale ?: third?.scale else second?.scale ?: third?.scale ?: mainType.scale,
          scaleType = if (themeOverridesForType == null) mainType.scaleType ?: first?.scaleType ?: second?.scaleType ?: third?.scaleType else second?.scaleType ?: third?.scaleType ?: mainType.scaleType,
          filename = if (themeOverridesForType == null) mainType.filename else first?.imageFile ?: second?.imageFile ?: third?.imageFile ?: mainType.filename,
        )
        is BackgroundImageType.Empty -> mainType
      },
      background = (first?.background ?: second?.background ?: third?.background)?.colorFromReadableHex() ?: mainType.defaultBackgroundColor(base, materialBackgroundColor),
      tint = (first?.tint ?: second?.tint ?: third?.tint)?.colorFromReadableHex() ?: mainType.defaultTintColor(base)
    )
  }

  fun withFilledColors(base: DefaultTheme, perChatTheme: ThemeColors?, perChatWallpaperType: BackgroundImageType?, perUserTheme: ThemeColors?, perUserWallpaperType: BackgroundImageType?, presetWallpaperTheme: ThemeColors?): ThemeColors {
    val c = toColors(base, perChatTheme, perUserTheme, presetWallpaperTheme)
    val ac = toAppColors(base, perChatTheme, perChatWallpaperType, perUserTheme, perUserWallpaperType, presetWallpaperTheme)
    return ThemeColors(
      primary = c.primary.toReadableHex(),
      primaryVariant = c.primaryVariant.toReadableHex(),
      secondary = c.secondary.toReadableHex(),
      secondaryVariant = c.secondaryVariant.toReadableHex(),
      background = c.background.toReadableHex(),
      surface = c.surface.toReadableHex(),
      title = ac.title.toReadableHex(),
      primaryVariant2 = ac.primaryVariant2.toReadableHex(),
      sentMessage = ac.sentMessage.toReadableHex(),
      sentQuote = ac.sentQuote.toReadableHex(),
      receivedMessage = ac.receivedMessage.toReadableHex(),
      receivedQuote = ac.receivedQuote.toReadableHex(),
    )
  }
}

fun List<ThemeOverrides>.getTheme(themeId: String?): ThemeOverrides? =
  firstOrNull { it.themeId == themeId }

fun List<ThemeOverrides>.getTheme(themeId: String?, type: BackgroundImageType?, base: DefaultTheme): ThemeOverrides? =
  firstOrNull { it.themeId == themeId || it.isSame(type, base.themeName)}

fun List<ThemeOverrides>.replace(theme: ThemeOverrides): List<ThemeOverrides> {
  val index = indexOfFirst { it.themeId == theme.themeId ||
      // prevent situation when two themes has the same type but different theme id (maybe something was changed in prefs by hand)
      it.isSame(BackgroundImageType.from(theme.wallpaper), theme.base.themeName)
  }
  return if (index != -1) {
    val a = ArrayList(this)
    a[index] = theme
    a
  } else {
    this + theme
  }
}

fun List<ThemeOverrides>.sameTheme(type: BackgroundImageType?, themeName: String): ThemeOverrides? = firstOrNull { it.isSame(type, themeName) }

@Serializable
data class ThemeModeOverrides (
  val light: ThemeModeOverride? = null,
  val dark: ThemeModeOverride? = null
) {
  fun preferredMode(darkTheme: Boolean): ThemeModeOverride? = when (darkTheme) {
    false -> light
    else -> dark
  }
}

@Serializable
data class ThemeModeOverride (
  val mode: DefaultThemeMode = CurrentColors.value.base.mode,
  val colors: ThemeColors = ThemeColors(),
  val wallpaper: ThemeWallpaper? = null,
) {

  @Transient
  val type = BackgroundImageType.from(wallpaper)

  fun withUpdatedColor(name: ThemeColor, color: String?): ThemeModeOverride {
    return copy(colors = when (name) {
      ThemeColor.PRIMARY -> colors.copy(primary = color)
      ThemeColor.PRIMARY_VARIANT -> colors.copy(primaryVariant = color)
      ThemeColor.SECONDARY -> colors.copy(secondary = color)
      ThemeColor.SECONDARY_VARIANT -> colors.copy(secondaryVariant = color)
      ThemeColor.BACKGROUND -> colors.copy(background = color)
      ThemeColor.SURFACE -> colors.copy(surface = color)
      ThemeColor.TITLE -> colors.copy(title = color)
      ThemeColor.PRIMARY_VARIANT2 -> colors.copy(primaryVariant2 = color)
      ThemeColor.SENT_MESSAGE -> colors.copy(sentMessage = color)
      ThemeColor.SENT_QUOTE -> colors.copy(sentQuote = color)
      ThemeColor.RECEIVED_MESSAGE -> colors.copy(receivedMessage = color)
      ThemeColor.RECEIVED_QUOTE -> colors.copy(receivedQuote = color)
      ThemeColor.WALLPAPER_BACKGROUND -> colors.copy()
      ThemeColor.WALLPAPER_TINT -> colors.copy()
    }, wallpaper = when (name) {
      ThemeColor.WALLPAPER_BACKGROUND -> wallpaper?.copy(background = color)
      ThemeColor.WALLPAPER_TINT -> wallpaper?.copy(tint = color)
      else -> wallpaper?.copy()
    }
    )
  }
  companion object {
    fun withFilledAppDefaults(mode: DefaultThemeMode, base: DefaultTheme): ThemeModeOverride =
      ThemeModeOverride(
        mode = mode,
        colors = ThemeOverrides(base = base).withFilledColors(base, null, null, null, null, null),
        wallpaper = ThemeWallpaper()
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
  primaryVariant2 = Color(0xFF18262E),
  sentMessage = Color(0xFF18262E),
  sentQuote = Color(0xFF1D3847),
  receivedMessage = Color(0x20B1B0B5),
  receivedQuote = Color(0x20B1B0B5), //LALAL
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
  primaryVariant2 = Color(0xFFE9F7FF),
  sentMessage = Color(0xFFE9F7FF),
  sentQuote = Color(0xFFD6F0FF),
  receivedMessage = Color(0x20B1B0B5),
  receivedQuote = Color(0x20B1B0B5),
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
  primaryVariant2 = Color(0xFF172941),
  sentMessage = Color(0xFF172941),
  sentQuote = Color(0xFF1C3A57),
  receivedMessage = Color(0x20B1B0B5),
  receivedQuote = Color(0x20B1B0B5),
)

val BlackColorPalette = darkColors(
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
val BlackColorPaletteApp = AppColors(
  title = SimplexBlue,
  primaryVariant2 = Color(0xFF18262E),
  sentMessage = Color(0xFF18262E),
  sentQuote = Color(0xFF1D3847),
  receivedMessage = Color(0x20B1B0B5),
  receivedQuote = Color(0x20B1B0B5),
)

var systemInDarkThemeCurrently: Boolean = isInNightMode()

val CurrentColors: MutableStateFlow<ThemeManager.ActiveTheme> = MutableStateFlow(ThemeManager.currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get()))

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
  primaryVariant2 = other.primaryVariant2
  sentMessage = other.sentMessage
  sentQuote = other.sentQuote
  receivedMessage = other.receivedMessage
  receivedQuote = other.receivedQuote
}

fun AppWallpaper.updateWallpaperFrom(other: AppWallpaper) {
  background = other.background
  tint = other.tint
  type = other.type
}

val MaterialTheme.wallpaper: AppWallpaper
  @Composable
  @ReadOnlyComposable
  get() = LocalAppWallpaper.current

fun reactOnDarkThemeChanges(isDark: Boolean) {
  systemInDarkThemeCurrently = isDark
  if (ChatController.appPrefs.currentTheme.get() == DefaultTheme.SYSTEM_THEME_NAME && CurrentColors.value.colors.isLight == isDark) {
    // Change active colors from light to dark and back based on system theme
    ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
  }
}

@Composable
fun SimpleXTheme(darkTheme: Boolean? = null, content: @Composable () -> Unit) {
// TODO: Fix preview working with dark/light theme

//  LaunchedEffect(darkTheme) {
//    // For preview
//    if (darkTheme != null)
//      CurrentColors.value = ThemeManager.currentColors(darkTheme, null, null, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
//  }
  val systemDark = rememberUpdatedState(isSystemInDarkTheme())
  LaunchedEffect(Unit) {
    // snapshotFlow vs LaunchedEffect reduce number of recomposes
    snapshotFlow { systemDark.value }
      .collect {
        reactOnDarkThemeChanges(systemDark.value)
      }
  }
  val theme by CurrentColors.collectAsState()
  LaunchedEffect(Unit) {
    // snapshotFlow vs LaunchedEffect reduce number of recomposes when user is changed or it's themes
    snapshotFlow { chatModel.currentUser.value?.uiThemes }
      .collect {
        ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)
      }
  }
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
      val rememberedWallpaper = remember {
        // Explicitly creating a new object here so we don't mutate the initial [wallpaper]
        // provided, and overwrite the values set in it.
        theme.wallpaper.copy()
      }.apply { updateWallpaperFrom(theme.wallpaper) }
      CompositionLocalProvider(
        LocalContentColor provides MaterialTheme.colors.onBackground,
        LocalAppColors provides rememberedAppColors,
        LocalAppWallpaper provides rememberedWallpaper,
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
      val rememberedWallpaper = remember {
        // Explicitly creating a new object here so we don't mutate the initial [wallpaper]
        // provided, and overwrite the values set in it.
        theme.wallpaper.copy()
      }.apply { updateWallpaperFrom(theme.wallpaper) }
      CompositionLocalProvider(
        LocalContentColor provides MaterialTheme.colors.onBackground,
        LocalAppColors provides rememberedAppColors,
        LocalAppWallpaper provides rememberedWallpaper,
        content = content)
    }
  )
}
