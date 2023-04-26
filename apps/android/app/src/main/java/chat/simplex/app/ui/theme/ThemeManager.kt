package chat.simplex.app.ui.theme

import androidx.compose.material.Colors
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.views.helpers.generalGetString
import okhttp3.internal.toHexString

object ThemeManager {
  private val appPrefs: AppPreferences by lazy {
    SimplexApp.context.chatModel.controller.appPrefs
  }

  data class ActiveTheme(val name: String, val base: DefaultTheme, val colors: Colors)

  private fun systemDarkThemeColors(): Pair<Colors, DefaultTheme> = when (appPrefs.systemDarkTheme.get()) {
    DefaultTheme.DARK.name -> DarkColorPalette to DefaultTheme.DARK
    DefaultTheme.BLUE.name -> BlueColorPalette to DefaultTheme.BLUE
    else -> BlueColorPalette to DefaultTheme.BLUE
  }

  fun currentColors(darkForSystemTheme: Boolean): ActiveTheme {
    val themeName = appPrefs.currentTheme.get()!!
    val themeOverrides = appPrefs.themeOverrides.get()

    val theme = if (themeName != DefaultTheme.SYSTEM.name) {
      themeOverrides[themeName]
    } else {
      themeOverrides[if (darkForSystemTheme) appPrefs.systemDarkTheme.get() else DefaultTheme.LIGHT.name]
    }
    val baseTheme = when (themeName) {
      DefaultTheme.SYSTEM.name -> if (darkForSystemTheme) systemDarkThemeColors() else Pair(LightColorPalette, DefaultTheme.LIGHT)
      DefaultTheme.LIGHT.name -> Pair(LightColorPalette, DefaultTheme.LIGHT)
      DefaultTheme.DARK.name -> Pair(DarkColorPalette, DefaultTheme.DARK)
      DefaultTheme.BLUE.name -> Pair(BlueColorPalette, DefaultTheme.BLUE)
      else -> if (theme != null) theme.colors.toColors(theme.base) to theme.base else Pair(LightColorPalette, DefaultTheme.LIGHT)
    }
    if (theme == null) {
      return ActiveTheme(themeName, baseTheme.second, baseTheme.first)
    }
    return ActiveTheme(themeName, baseTheme.second, theme.colors.toColors(theme.base))
  }

  // colors, default theme enum, localized name of theme
  fun allThemes(darkForSystemTheme: Boolean): List<Triple<Colors, DefaultTheme, String>> {
    val allThemes = ArrayList<Triple<Colors, DefaultTheme, String>>()
    allThemes.add(
      Triple(
        if (darkForSystemTheme) systemDarkThemeColors().first else LightColorPalette,
        DefaultTheme.SYSTEM,
        generalGetString(R.string.theme_system)
      )
    )
    allThemes.add(
      Triple(
        LightColorPalette,
        DefaultTheme.LIGHT,
        generalGetString(R.string.theme_light)
      )
    )
    allThemes.add(
      Triple(
        DarkColorPalette,
        DefaultTheme.DARK,
        generalGetString(R.string.theme_dark)
      )
    )
    allThemes.add(
      Triple(
        BlueColorPalette,
        DefaultTheme.BLUE,
        generalGetString(R.string.theme_blue)
      )
    )
    return allThemes
  }

  fun applyTheme(theme: String, darkForSystemTheme: Boolean) {
    appPrefs.currentTheme.set(theme)
    CurrentColors.value = currentColors(darkForSystemTheme)
  }

  fun changeDarkTheme(theme: String, darkForSystemTheme: Boolean) {
    appPrefs.systemDarkTheme.set(theme)
    CurrentColors.value = currentColors(darkForSystemTheme)
  }

  fun saveAndApplyPrimaryColor(color: Color? = null, darkForSystemTheme: Boolean) {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = if (themeName != DefaultTheme.SYSTEM.name) {
      themeName
    } else {
      if (darkForSystemTheme) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.name
    }
    val color = color ?: when(themeName) {
      DefaultTheme.LIGHT.name -> LightColorPalette.primary
      DefaultTheme.DARK.name -> DarkColorPalette.primary
      DefaultTheme.BLUE.name -> BlueColorPalette.primary
      else -> (if (darkForSystemTheme) systemDarkThemeColors().first else LightColorPalette).primary
    }
    val overrides = appPrefs.themeOverrides.get().toMutableMap()
    val prevValue = overrides[nonSystemThemeName]
    val current = prevValue?.copy(colors = prevValue.colors.copy(primary = color.toReadableHex()))
      ?: ThemeOverrides(base = CurrentColors.value.base, colors = ThemeColors(primary = color.toReadableHex()))
    overrides[nonSystemThemeName] = current
    appPrefs.themeOverrides.set(overrides)
    CurrentColors.value = currentColors(!CurrentColors.value.colors.isLight)
  }
}

private fun Color.toReadableHex(): String = "#" + toArgb().toHexString()
