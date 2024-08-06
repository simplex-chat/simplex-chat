package chat.simplex.common.ui.theme

import androidx.compose.material.Colors
import androidx.compose.runtime.MutableState
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.text.font.FontFamily
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import java.io.File

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
    DefaultTheme.BLACK.themeName -> BlackColorPalette to DefaultTheme.BLACK
    else -> SimplexColorPalette to DefaultTheme.SIMPLEX
  }

  private fun nonSystemThemeName(): String {
    val themeName = appPrefs.currentTheme.get()!!
    return if (themeName != DefaultTheme.SYSTEM_THEME_NAME) {
      themeName
    } else {
      if (systemInDarkThemeCurrently) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.themeName
    }
  }

  fun defaultActiveTheme(appSettingsTheme: List<ThemeOverrides>): ThemeOverrides? {
    val nonSystemThemeName = nonSystemThemeName()
    val defaultThemeId = appPrefs.currentThemeIds.get()[nonSystemThemeName]
    return appSettingsTheme.getTheme(defaultThemeId)
  }

  fun defaultActiveTheme(perUserTheme: ThemeModeOverrides?, appSettingsTheme: List<ThemeOverrides>): ThemeModeOverride {
    val perUserTheme = if (!CurrentColors.value.colors.isLight) perUserTheme?.dark else perUserTheme?.light
    if (perUserTheme != null) {
      return perUserTheme
    }
    val defaultTheme = defaultActiveTheme(appSettingsTheme)
    return ThemeModeOverride(colors = defaultTheme?.colors ?: ThemeColors(), wallpaper = defaultTheme?.wallpaper)
  }

  fun currentColors(themeOverridesForType: WallpaperType?, perChatTheme: ThemeModeOverride?, perUserTheme: ThemeModeOverrides?, appSettingsTheme: List<ThemeOverrides>): ActiveTheme {
    val themeName = appPrefs.currentTheme.get()!!
    val nonSystemThemeName = nonSystemThemeName()
    val defaultTheme = defaultActiveTheme(appSettingsTheme)

    val baseTheme = when (nonSystemThemeName) {
      DefaultTheme.LIGHT.themeName -> ActiveTheme(DefaultTheme.LIGHT.themeName, DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp, AppWallpaper(type = PresetWallpaper.SCHOOL.toType(DefaultTheme.LIGHT)))
      DefaultTheme.DARK.themeName -> ActiveTheme(DefaultTheme.DARK.themeName, DefaultTheme.DARK, DarkColorPalette, DarkColorPaletteApp, AppWallpaper(type = PresetWallpaper.SCHOOL.toType(DefaultTheme.DARK)))
      DefaultTheme.SIMPLEX.themeName -> ActiveTheme(DefaultTheme.SIMPLEX.themeName, DefaultTheme.SIMPLEX, SimplexColorPalette, SimplexColorPaletteApp, AppWallpaper(type = PresetWallpaper.SCHOOL.toType(DefaultTheme.SIMPLEX)))
      DefaultTheme.BLACK.themeName -> ActiveTheme(DefaultTheme.BLACK.themeName, DefaultTheme.BLACK, BlackColorPalette, BlackColorPaletteApp, AppWallpaper(type = PresetWallpaper.SCHOOL.toType(DefaultTheme.BLACK)))
      else -> ActiveTheme(DefaultTheme.LIGHT.themeName, DefaultTheme.LIGHT, LightColorPalette, LightColorPaletteApp, AppWallpaper(type = PresetWallpaper.SCHOOL.toType(DefaultTheme.LIGHT)))
    }

    val perUserTheme = if (baseTheme.colors.isLight) perUserTheme?.light else perUserTheme?.dark
    val theme = (appSettingsTheme.sameTheme(themeOverridesForType ?: perChatTheme?.type ?: perUserTheme?.type ?: defaultTheme?.wallpaper?.toAppWallpaper()?.type, nonSystemThemeName) ?: defaultTheme)

    if (theme == null && perUserTheme == null && perChatTheme == null && themeOverridesForType == null) {
      return ActiveTheme(themeName, baseTheme.base, baseTheme.colors, baseTheme.appColors, baseTheme.wallpaper)
    }
    val presetWallpaperTheme = when {
      perChatTheme?.wallpaper != null -> if (perChatTheme.wallpaper.preset != null) PresetWallpaper.from(perChatTheme.wallpaper.preset)?.colors?.get(baseTheme.base) else null
      perUserTheme?.wallpaper != null -> if (perUserTheme.wallpaper.preset != null) PresetWallpaper.from(perUserTheme.wallpaper.preset)?.colors?.get(baseTheme.base) else null
      else -> if (theme?.wallpaper?.preset != null) PresetWallpaper.from(theme.wallpaper.preset)?.colors?.get(baseTheme.base) else null
    }
    val themeOrEmpty = theme ?: ThemeOverrides(base = baseTheme.base)
    val colors = themeOrEmpty.toColors(themeOrEmpty.base, perChatTheme?.colors, perUserTheme?.colors, presetWallpaperTheme)
    return ActiveTheme(
      themeName,
      baseTheme.base,
      colors,
      themeOrEmpty.toAppColors(themeOrEmpty.base, perChatTheme?.colors, perChatTheme?.type, perUserTheme?.colors, perUserTheme?.type, presetWallpaperTheme),
      themeOrEmpty.toAppWallpaper(themeOverridesForType, perChatTheme, perUserTheme, colors.background)
    )
  }

  fun currentThemeOverridesForExport(perChatTheme: ThemeModeOverride?, perUserTheme: ThemeModeOverrides?): ThemeOverrides {
    val current = currentColors(null, perChatTheme, perUserTheme, appPrefs.themeOverrides.get())
    val wType = current.wallpaper.type
    val wBackground = current.wallpaper.background
    val wTint = current.wallpaper.tint
    return ThemeOverrides(
      themeId = "",
      base = current.base,
      colors = ThemeColors.from(current.colors, current.appColors),
      wallpaper = if (wType !is WallpaperType.Empty) ThemeWallpaper.from(wType, wBackground?.toReadableHex(), wTint?.toReadableHex()).withFilledWallpaperBase64() else null
    )
  }

  fun applyTheme(theme: String) {
    appPrefs.currentTheme.set(theme)
    CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    platform.androidSetNightModeIfSupported()
    val c = CurrentColors.value.colors
    platform.androidSetStatusAndNavBarColors(c.isLight, c.background, !ChatController.appPrefs.reachableChatToolbar.get(), ChatController.appPrefs.reachableChatToolbar.get())
  }

  fun changeDarkTheme(theme: String) {
    appPrefs.systemDarkTheme.set(theme)
    CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun saveAndApplyThemeColor(baseTheme: DefaultTheme, name: ThemeColor, color: Color? = null, pref: SharedPreference<List<ThemeOverrides>> = appPrefs.themeOverrides) {
    val nonSystemThemeName = baseTheme.themeName
    val overrides = pref.get()
    val themeId = appPrefs.currentThemeIds.get()[nonSystemThemeName]
    val prevValue = overrides.getTheme(themeId) ?: ThemeOverrides(base = baseTheme)
    pref.set(overrides.replace(prevValue.withUpdatedColor(name, color?.toReadableHex())))
    val themeIds = appPrefs.currentThemeIds.get().toMutableMap()
    themeIds[nonSystemThemeName] = prevValue.themeId
    appPrefs.currentThemeIds.set(themeIds)
    CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    if (name == ThemeColor.BACKGROUND) {
      val c = CurrentColors.value.colors
      platform.androidSetStatusAndNavBarColors(c.isLight, c.background, false, false)
    }
  }

  fun applyThemeColor(name: ThemeColor, color: Color? = null, pref: MutableState<ThemeModeOverride>) {
    pref.value = pref.value.withUpdatedColor(name, color?.toReadableHex())
  }

  fun saveAndApplyWallpaper(baseTheme: DefaultTheme, type: WallpaperType?, pref: SharedPreference<List<ThemeOverrides>> = appPrefs.themeOverrides) {
    val nonSystemThemeName = baseTheme.themeName
    val overrides = pref.get()
    val theme = overrides.sameTheme(type, baseTheme.themeName)
    val prevValue = theme ?: ThemeOverrides(base = baseTheme)
    pref.set(overrides.replace(prevValue.copy(wallpaper = if (type != null && type !is WallpaperType.Empty) ThemeWallpaper.from(type, prevValue.wallpaper?.background, prevValue.wallpaper?.tint) else null)))
    val themeIds = appPrefs.currentThemeIds.get().toMutableMap()
    themeIds[nonSystemThemeName] = prevValue.themeId
    appPrefs.currentThemeIds.set(themeIds)
    CurrentColors.value = currentColors( null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun copyFromSameThemeOverrides(type: WallpaperType?, lowerLevelOverride: ThemeModeOverride?, pref: MutableState<ThemeModeOverride>): Boolean {
    val overrides = appPrefs.themeOverrides.get()
    val sameWallpaper = if (lowerLevelOverride?.type?.sameType(type) == true) lowerLevelOverride.wallpaper else overrides.sameTheme(type, CurrentColors.value.base.themeName)?.wallpaper
    if (sameWallpaper == null) {
      if (type != null) {
        pref.value = ThemeModeOverride(wallpaper = ThemeWallpaper.from(type, null, null).copy(scale = null, scaleType = null))
      } else {
        // Make an empty wallpaper to override any top level ones
        pref.value = ThemeModeOverride(wallpaper = ThemeWallpaper())
      }
      return true
    }
    var type = sameWallpaper.toAppWallpaper().type
    if (type is WallpaperType.Image && sameWallpaper.imageFile == type.filename) {
      // same image file. Needs to be copied first in order to be able to remove the file once it's not needed anymore without affecting main theme override
      val filename = saveWallpaperFile(File(getWallpaperFilePath(type.filename)).toURI())
      if (filename != null) {
        type = WallpaperType.Image(filename, type.scale, type.scaleType)
      } else {
        Log.e(TAG, "Error while copying wallpaper from global overrides to chat overrides")
        return false
      }
    }
    val prevValue = pref.value
    pref.value = prevValue.copy(
      colors = ThemeColors(),
      wallpaper = ThemeWallpaper.from(type, null, null).copy(scale = null, scaleType = null)
    )
    return true
  }

  fun applyWallpaper(type: WallpaperType?, pref: MutableState<ThemeModeOverride>) {
    val prevValue = pref.value
    pref.value = prevValue.copy(
      wallpaper = if (type != null)
        ThemeWallpaper.from(type, prevValue.wallpaper?.background, prevValue.wallpaper?.tint)
      else null
    )
  }

  fun saveAndApplyThemeOverrides(theme: ThemeOverrides, pref: SharedPreference<List<ThemeOverrides>> = appPrefs.themeOverrides) {
    val wallpaper = theme.wallpaper?.importFromString()
    val nonSystemThemeName = theme.base.themeName
    val overrides = pref.get()
    val prevValue = overrides.getTheme(null, wallpaper?.toAppWallpaper()?.type, theme.base) ?: ThemeOverrides(base = theme.base)
    if (prevValue.wallpaper?.imageFile != null) {
      File(getWallpaperFilePath(prevValue.wallpaper.imageFile)).delete()
    }
    pref.set(overrides.replace(prevValue.copy(base = theme.base, colors = theme.colors, wallpaper = wallpaper)))
    appPrefs.currentTheme.set(nonSystemThemeName)
    val currentThemeIds = appPrefs.currentThemeIds.get().toMutableMap()
    currentThemeIds[nonSystemThemeName] = prevValue.themeId
    appPrefs.currentThemeIds.set(currentThemeIds)
    CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun resetAllThemeColors(pref: SharedPreference<List<ThemeOverrides>> = appPrefs.themeOverrides) {
    val nonSystemThemeName = nonSystemThemeName()
    val themeId = appPrefs.currentThemeIds.get()[nonSystemThemeName] ?: return
    val overrides = pref.get()
    val prevValue = overrides.getTheme(themeId) ?: return
    pref.set(overrides.replace(prevValue.copy(colors = ThemeColors(), wallpaper = prevValue.wallpaper?.copy(background = null, tint = null))))
    CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }

  fun resetAllThemeColors(pref: MutableState<ThemeModeOverride>) {
    val prevValue = pref.value
    pref.value = prevValue.copy(colors = ThemeColors(), wallpaper = prevValue.wallpaper?.copy(background = null, tint = null))
  }

  fun removeTheme(themeId: String?) {
    val themes = ArrayList(appPrefs.themeOverrides.get())
    themes.removeAll { it.themeId == themeId }
    appPrefs.themeOverrides.set(themes)
  }

  fun String.colorFromReadableHex(): Color =
    Color(this.replace("#", "").toLongOrNull(16) ?: Color.White.toArgb().toLong())

  fun Color.toReadableHex(): String {
    val s = Integer.toHexString(toArgb())
    return when {
      this == Color.Transparent -> "#00ffffff"
      s.length == 1 -> "#ff$s$s$s$s$s$s"
      s.length == 2 -> "#ff$s$s$s"
      s.length == 3 -> "#ff$s$s"
      s.length == 4 && this.alpha == 0f -> "#0000$s" // 000088ff treated as 88ff
      s.length == 4 -> "#ff00$s"
      s.length == 6 && this.alpha == 0f -> "#00$s"
      s.length == 6 -> "#ff$s"
      else -> "#$s"
    }
  }
}
