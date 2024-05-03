package chat.simplex.common.ui.theme

import androidx.compose.material.Colors
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.text.font.FontFamily
import chat.simplex.common.model.*
import chat.simplex.res.MR
import chat.simplex.common.platform.platform
import chat.simplex.common.views.helpers.BackgroundImageType
import chat.simplex.common.views.helpers.generalGetString

// https://github.com/rsms/inter
// I place it here because IDEA shows an error (but still works anyway) when this declaration inside Type.kt
expect val Inter: FontFamily
expect val EmojiFont: FontFamily

object ThemeManager {
  private val appPrefs: AppPreferences = ChatController.appPrefs

  data class ActiveTheme(val name: String, val base: DefaultTheme, val colors: Colors, val appColors: AppColors, val wallpaper: AppWallpaper)

  private fun systemDarkThemeColors(): Pair<Colors, DefaultTheme> = when (appPrefs.systemDarkTheme.get()) {
    DefaultTheme.DARK.name -> DarkColorPalette to DefaultTheme.DARK
    DefaultTheme.SIMPLEX.name -> SimplexColorPalette to DefaultTheme.SIMPLEX
    else -> SimplexColorPalette to DefaultTheme.SIMPLEX
  }

  fun currentColors(darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides): ActiveTheme {
    val themeName = appPrefs.currentTheme.get()!!
    val themeOverrides = pref.get()

    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    val theme = themeOverrides[nonSystemThemeName]
    val baseTheme = when (nonSystemThemeName) {
      DefaultTheme.LIGHT.name -> Triple(DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp)
      DefaultTheme.DARK.name -> Triple(DefaultTheme.DARK, DarkColorPalette, DarkColorPaletteApp)
      DefaultTheme.SIMPLEX.name -> Triple(DefaultTheme.SIMPLEX, SimplexColorPalette, SimplexColorPaletteApp)
      else -> Triple(DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp)
    }
    if (theme == null) {
      return ActiveTheme(themeName, baseTheme.first, baseTheme.second, baseTheme.third, AppWallpaper())
    }
    return ActiveTheme(themeName, baseTheme.first, theme.colors.toColors(theme.base), theme.colors.toAppColors(theme.base), theme.wallpaper.toAppWallpaper())
  }

  fun currentThemeOverridesForExport(darkForSystemTheme: Boolean): ThemeOverrides {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    val overrides = appPrefs.themeOverrides.get().toMutableMap()
    val nonFilledTheme = overrides[nonSystemThemeName] ?: ThemeOverrides(base = CurrentColors.value.base, colors = ThemeColors(), wallpaper = ThemeWallpaper())
    return nonFilledTheme.copy(colors = nonFilledTheme.colors.withFilledColors(CurrentColors.value.base), wallpaper = nonFilledTheme.wallpaper.withFilledWallpaper())
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
    CurrentColors.value = currentColors(darkForSystemTheme, appPrefs.themeOverrides)
    platform.androidSetNightModeIfSupported()
  }

  fun changeDarkTheme(theme: String, darkForSystemTheme: Boolean) {
    appPrefs.systemDarkTheme.set(theme)
    CurrentColors.value = currentColors(darkForSystemTheme, appPrefs.themeOverrides)
  }

  fun saveAndApplyThemeColor(name: ThemeColor, color: Color? = null, darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    var colorToSet = color
    if (colorToSet == null) {
      // Setting default color from a base theme
      colorToSet = when(nonSystemThemeName) {
        DefaultTheme.LIGHT.name -> name.fromColors(LightColorPalette, LightColorPaletteApp, AppWallpaper())
        DefaultTheme.DARK.name -> name.fromColors(DarkColorPalette, DarkColorPaletteApp, AppWallpaper())
        DefaultTheme.SIMPLEX.name -> name.fromColors(SimplexColorPalette, SimplexColorPaletteApp, AppWallpaper())
        // Will not be here
        else -> return
      }
    }
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides[nonSystemThemeName] ?: ThemeOverrides(base = CurrentColors.value.base, colors = ThemeColors(), wallpaper = ThemeWallpaper())
    overrides[nonSystemThemeName] = prevValue.withUpdatedColor(name, colorToSet?.toReadableHex())
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, appPrefs.themeOverrides)
  }

  fun saveAndApplyBackgroundImage(type: BackgroundImageType?, darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides[nonSystemThemeName] ?: ThemeOverrides(base = CurrentColors.value.base, colors = ThemeColors(), wallpaper = ThemeWallpaper())
    overrides[nonSystemThemeName] = prevValue.copy(wallpaper = if (type != null) ThemeWallpaper.from(type, prevValue.wallpaper.background, prevValue.wallpaper.tint) else ThemeWallpaper())
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, appPrefs.themeOverrides)
  }

  fun saveAndApplyThemeOverrides(theme: ThemeOverrides, darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides[theme.base.name] ?: ThemeOverrides(base = CurrentColors.value.base, colors = ThemeColors(), wallpaper = ThemeWallpaper())
    overrides[theme.base.name] = prevValue.copy(colors = theme.colors, wallpaper = theme.wallpaper.importFromString())
    pref.set(overrides)
    appPrefs.currentTheme.set(theme.base.name)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, appPrefs.themeOverrides)
  }

  fun resetAllThemeColors(darkForSystemTheme: Boolean, pref: SharedPreference<Map<String, ThemeOverrides>> = appPrefs.themeOverrides) {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    val overrides = pref.get().toMutableMap()
    val prevValue = overrides[nonSystemThemeName] ?: return
    overrides[nonSystemThemeName] = prevValue.copy(colors = ThemeColors(), wallpaper = prevValue.wallpaper.copy(background = null, tint = null))
    pref.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight, appPrefs.themeOverrides)
  }

  fun String.colorFromReadableHex(): Color =
    Color(this.replace("#", "").toLongOrNull(16) ?: Color.White.toArgb().toLong())

  fun Color.toReadableHex(): String = "#" + Integer.toHexString(toArgb())
}
