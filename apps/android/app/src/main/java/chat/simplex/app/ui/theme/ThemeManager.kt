package chat.simplex.app.ui.theme

import androidx.compose.material.Colors
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.views.helpers.generalGetString

object ThemeManager {
  private val appPrefs: AppPreferences by lazy {
    AppPreferences(SimplexApp.context)
  }

  fun currentColors(darkForSystemTheme: Boolean): Pair<Colors, DefaultTheme> {
    val theme = appPrefs.currentTheme.get()!!
    val systemThemeColors = if (darkForSystemTheme) DarkColorPalette else LightColorPalette
    val res = when (theme) {
      DefaultTheme.SYSTEM.name -> Pair(systemThemeColors, DefaultTheme.SYSTEM)
      DefaultTheme.DARK.name -> Pair(DarkColorPalette, DefaultTheme.DARK)
      DefaultTheme.LIGHT.name -> Pair(LightColorPalette, DefaultTheme.LIGHT)
      else -> Pair(systemThemeColors, DefaultTheme.SYSTEM)
    }
    return res.copy(first = res.first.copy(primary = Color(appPrefs.primaryColor.get())))
  }

  // colors, default theme enum, localized name of theme
  fun allThemes(darkForSystemTheme: Boolean): List<Triple<Colors, DefaultTheme, String>> {
    val allThemes = ArrayList<Triple<Colors, DefaultTheme, String>>()
    allThemes.add(
      Triple(
        if (darkForSystemTheme) DarkColorPalette else LightColorPalette,
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
    return allThemes
  }

  fun applyTheme(name: String, darkForSystemTheme: Boolean) {
    appPrefs.currentTheme.set(name)
    CurrentColors.value = currentColors(darkForSystemTheme)
  }

  fun saveAndApplyPrimaryColor(color: Color) {
    appPrefs.primaryColor.set(color.toArgb())
    CurrentColors.value = currentColors(!CurrentColors.value.first.isLight)
  }
}
