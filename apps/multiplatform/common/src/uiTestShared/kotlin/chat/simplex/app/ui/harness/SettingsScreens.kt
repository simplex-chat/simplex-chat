package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.getPreferenceFilePath
import chat.simplex.common.platform.readThemeOverrides
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.ui.theme.ThemeOverrides
import chat.simplex.common.views.helpers.WallpaperType
import java.io.File

fun UiScope.developerTools(app: App) {
  app.settings()
  click(Ui.Settings.Version)
  click(Ui.Settings.DeveloperTools)
  waitFor(Ui.DevTools.Console)
}

fun UiScope.closeDeveloperTools(what: String = "the chat list", shown: UiScope.() -> Boolean = { exists(Ui.ChatList.Search) }) {
  escape()
  poll(what = "Developer tools to close") { !exists(Ui.DevTools.Console) && exists(Ui.Settings.DeveloperTools) }
  escape()
  poll(what = "App version to close") { !exists(Ui.Settings.DeveloperTools) && exists(Ui.Settings.Version) }
  escape()
  poll(what = "Your settings to close, showing $what") { !exists(Ui.Settings.Version) && shown() }
}

fun UiScope.chatConsole(app: App) {
  developerTools(app)
  click(Ui.DevTools.Console)
  waitFor(Ui.Chat.Message)
}

fun UiScope.helpAndSupport(app: App) {
  app.settings()
  click(Ui.Settings.Help)
  waitFor(Ui.Help.HowToUse)
}

fun UiScope.customizeTheme(app: App) {
  app.appearance()
  click(Ui.Appearance.Customize)
  waitFor(Ui.Appearance.ExportTheme)
}

fun UiScope.closeAppearance() {
  escape()
  poll(what = "Appearance to close") { !exists(Ui.Appearance.ColorMode) && exists(Ui.Settings.Appearance) }
  escape()
  poll(what = "Your settings to close") { !exists(Ui.Settings.Appearance) && exists(Ui.ChatList.Search) }
}

fun UiScope.consoleShows(label: String): Boolean = texts().any { it.endsWith(" $label") }

object StoredThemes {
  fun yamlBytes(): ByteArray? = File(getPreferenceFilePath("themes.yaml")).takeIf { it.exists() }?.readBytes()

  fun active(base: DefaultTheme): ThemeOverrides? {
    val id = appPrefs.currentThemeIds.get()[base.themeName] ?: return null
    return readThemeOverrides().firstOrNull { it.base == base && it.themeId == id }
  }
}

fun currentWallpaperPreset(): String? = (CurrentColors.value.wallpaper.type as? WallpaperType.Preset)?.filename
