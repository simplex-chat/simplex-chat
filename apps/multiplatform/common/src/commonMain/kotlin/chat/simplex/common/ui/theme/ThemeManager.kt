package chat.simplex.common.ui.theme

import androidx.compose.material.Colors
import androidx.compose.runtime.MutableState
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.text.font.FontFamily
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.res.MR
import chat.simplex.common.platform.platform
import chat.simplex.common.views.helpers.*

// https://github.com/rsms/inter
// I place it here because IDEA shows an error (but still works anyway) when this declaration inside Type.kt
expect val Inter: FontFamily
expect val EmojiFont: FontFamily

object ThemeManager {
  private val appPrefs: AppPreferences = ChatController.appPrefs

  data class ActiveTheme(val name: String, val base: DefaultTheme, val colors: Colors, val appColors: AppColors, val wallpaper: AppWallpaper = AppWallpaper())

  private fun systemDarkThemeColors(): Pair<Colors, DefaultTheme> = when (appPrefs.systemDarkTheme.get()) {
    DefaultTheme.DARK.themeName -> DarkColorPalette to DefaultTheme.DARK
    DefaultTheme.SIMPLEX.themeName -> SimplexColorPalette to DefaultTheme.SIMPLEX
    else -> SimplexColorPalette to DefaultTheme.SIMPLEX
  }

  fun currentColors(darkForSystemTheme: Boolean, perChatTheme: ThemeModeOverride?, perUserTheme: ThemeModeOverrides?, appSettingsTheme: Map<String, ThemeOverrides>): ActiveTheme {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.themeName) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.themeName
    }
    val theme = appSettingsTheme[nonSystemThemeName]
    val modeOverrides = if (darkForSystemTheme) perUserTheme?.dark else perUserTheme?.light
    val baseTheme = when (nonSystemThemeName) {
      DefaultTheme.LIGHT.themeName -> ActiveTheme(DefaultTheme.LIGHT.themeName, DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp)
      DefaultTheme.DARK.themeName -> ActiveTheme(DefaultTheme.DARK.themeName, DefaultTheme.DARK, DarkColorPalette, DarkColorPaletteApp)
      DefaultTheme.SIMPLEX.themeName -> ActiveTheme(DefaultTheme.SIMPLEX.themeName, DefaultTheme.SIMPLEX, SimplexColorPalette, SimplexColorPaletteApp)
      else -> ActiveTheme(DefaultTheme.LIGHT.themeName, DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp)
    }

    if (theme == null && perUserTheme == null && perChatTheme == null) {
      return ActiveTheme(themeName, baseTheme.base, baseTheme.colors, baseTheme.appColors, baseTheme.wallpaper)
    }
    val wallpaperTheme = when {
      perChatTheme?.wallpaper != null -> if (perChatTheme.wallpaper.preset != null) PredefinedBackgroundImage.from(perChatTheme.wallpaper.preset)?.colors?.get(baseTheme.base) else null
      modeOverrides?.wallpaper != null -> if (modeOverrides.wallpaper.preset != null) PredefinedBackgroundImage.from(modeOverrides.wallpaper.preset)?.colors?.get(baseTheme.base) else null
      else -> if (theme?.wallpaper?.preset != null) PredefinedBackgroundImage.from(theme.wallpaper.preset)?.colors?.get(baseTheme.base) else null
    }
    val themeOrEmpty = theme ?: ThemeOverrides()
    return ActiveTheme(
      themeName,
      baseTheme.base,
      themeOrEmpty.toColors(themeOrEmpty.base, perChatTheme?.colors, wallpaperTheme, modeOverrides?.colors),
      themeOrEmpty.toAppColors(themeOrEmpty.base, perChatTheme?.colors, wallpaperTheme, modeOverrides?.colors),
      perChatTheme?.wallpaper?.toAppWallpaper() ?: modeOverrides?.wallpaper?.toAppWallpaper() ?: themeOrEmpty.wallpaper.toAppWallpaper()
    )
  }

  fun currentThemeOverridesForExport(darkForSystemTheme: Boolean, perChatTheme: ThemeModeOverride?, perUserTheme: ThemeModeOverrides?): ThemeOverrides {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.themeName) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.themeName
    }
    val overrides = appPrefs.themeOverrides.get().toMutableMap()
    val nonFilledTheme = overrides.getOrDefault(nonSystemThemeName, ThemeOverrides())
    val modeOverrides = if (darkForSystemTheme) perUserTheme?.dark else perUserTheme?.light
    val wallpaperTheme = when {
      perChatTheme?.wallpaper != null -> if (perChatTheme.wallpaper.preset != null) PredefinedBackgroundImage.from(perChatTheme.wallpaper.preset)?.colors?.get(nonFilledTheme.base) else null
      modeOverrides?.wallpaper != null -> if (modeOverrides.wallpaper.preset != null) PredefinedBackgroundImage.from(modeOverrides.wallpaper.preset)?.colors?.get(nonFilledTheme.base) else null
      else -> if (nonFilledTheme.wallpaper.preset != null) PredefinedBackgroundImage.from(nonFilledTheme.wallpaper.preset)?.colors?.get(nonFilledTheme.base) else null
    }
    return nonFilledTheme.copy(
      colors = nonFilledTheme.withFilledColors(CurrentColors.value.base, perChatTheme?.colors, modeOverrides?.colors, wallpaperTheme),
      wallpaper = nonFilledTheme.wallpaper.withFilledWallpaperBase64(modeOverrides)
    )
  }

  // colors, default theme enum, localized name of theme
  fun allThemes(darkForSystemTheme: Boolean): List<Triple<Colors, DefaultTheme, String>> {
    val allThemes = ArrayList<Triple<Colors, DefaultTheme, String>>()
    allThemes.add(
      Triple(
        if (darkForSystemTheme) systemDarkThemeColors().first else LightColorPalette,
        DefaultTheme.SYSTEM,
        generalGetString(MR.strings.theme_system)
      )
    )
    allThemes.add(
      Triple(
        LightColorPalette,
        DefaultTheme.LIGHT,
        generalGetString(MR.strings.theme_light)
      )
    )
    allThemes.add(
      Triple(
        DarkColorPalette,
        DefaultTheme.DARK,
        generalGetString(MR.strings.theme_dark)
      )
    )
    allThemes.add(
      Triple(
        SimplexColorPalette,
        DefaultTheme.SIMPLEX,
        generalGetString(MR.strings.theme_simplex)
      )
    )
    return allThemes
  }

  fun applyTheme(theme: String, darkForSystemTheme: Boolean) {
    appPrefs.currentTheme.set(theme)
    CurrentColors.value = currentColors(darkForSystemTheme, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    platform.androidSetNightModeIfSupported()
  }

  fun changeDarkTheme(theme: String, darkForSystemTheme: Boolean) {
    appPrefs.systemDarkTheme.set(theme)
    CurrentColors.value = currentColors(darkForSystemTheme, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun saveAndApplyThemeColor(baseTheme: DefaultTheme, name: ThemeColor, color: Color? = null, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val nonSystemThemeName = baseTheme.themeName
    var colorToSet = color
    if (colorToSet == null) {
      // Setting default color from a base theme
      colorToSet = when(nonSystemThemeName) {
        DefaultTheme.LIGHT.themeName -> name.fromColors(LightColorPalette, LightColorPaletteApp, AppWallpaper())
        DefaultTheme.DARK.themeName -> name.fromColors(DarkColorPalette, DarkColorPaletteApp, AppWallpaper())
        DefaultTheme.SIMPLEX.themeName -> name.fromColors(SimplexColorPalette, SimplexColorPaletteApp, AppWallpaper())
        // Will not be here
        else -> return
      }
    }
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides.getOrDefault(nonSystemThemeName, ThemeOverrides())
    overrides[nonSystemThemeName] = prevValue.withUpdatedColor(name, colorToSet?.toReadableHex())
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun applyThemeColor(name: ThemeColor, color: Color? = null, pref: MutableState<ThemeModeOverride>) {
    pref.value = pref.value.withUpdatedColor(name, color?.toReadableHex())
  }

  fun saveAndApplyBackgroundImage(baseTheme: DefaultTheme, type: BackgroundImageType?, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val nonSystemThemeName = baseTheme.themeName
    val overrides = pref.get().toMutableMap()
    var prevValue = overrides.getOrDefault(nonSystemThemeName, ThemeOverrides())
    // Overriding the whole theme on type change
    if (prevValue.wallpaper.imageFile != type?.filename && prevValue.wallpaper.preset != type?.filename) {
      prevValue = ThemeOverrides()
    }
    overrides[nonSystemThemeName] = prevValue.copy(wallpaper = if (type != null) ThemeWallpaper.from(type, prevValue.wallpaper.background, prevValue.wallpaper.tint) else ThemeWallpaper())
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun applyBackgroundImage(type: BackgroundImageType?, pref: MutableState<ThemeModeOverride>) {
    var prevValue = pref.value
    // Overriding the whole theme on type change
    if (prevValue.wallpaper?.imageFile != type?.filename && prevValue.wallpaper?.preset != type?.filename) {
      prevValue = ThemeModeOverride()
    }
    pref.value = prevValue.copy(wallpaper = if (type != null) ThemeWallpaper.from(type, prevValue.wallpaper?.background, prevValue.wallpaper?.tint) else ThemeWallpaper())
  }

  fun saveAndApplyThemeOverrides(theme: ThemeOverrides, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides.getOrDefault(theme.base.themeName, ThemeOverrides())
    overrides[theme.base.themeName] = prevValue.copy(colors = theme.colors, wallpaper = theme.wallpaper.importFromString())
    pref.set(overrides)
    appPrefs.currentTheme.set(theme.base.themeName)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun resetAllThemeColors(darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.themeName) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.themeName
    }
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides[nonSystemThemeName] ?: return
    overrides[nonSystemThemeName] = prevValue.copy(colors = ThemeColors(), wallpaper = prevValue.wallpaper.copy(background = null, tint = null))
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun String.colorFromReadableHex(): Color =
    Color(this.replace("#", "").toLongOrNull(16) ?: Color.White.toArgb().toLong())

  fun Color.toReadableHex(): String = "#" + if (this == Color.Transparent) "00ffffff" else Integer.toHexString(toArgb())
}
