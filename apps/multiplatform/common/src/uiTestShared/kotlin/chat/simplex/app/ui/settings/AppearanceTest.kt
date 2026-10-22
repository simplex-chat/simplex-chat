package chat.simplex.app.ui.settings

import androidx.compose.ui.graphics.Color
import chat.simplex.app.ui.harness.StoredThemes
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closeAppearance
import chat.simplex.app.ui.harness.currentWallpaperPreset
import chat.simplex.app.ui.harness.customizeTheme
import chat.simplex.app.ui.harness.jpegFile
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.yaml
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getWallpaperFilePath
import chat.simplex.common.platform.readThemeOverrides
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.theme.BlackColorPalette
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.ui.theme.ThemeColor
import chat.simplex.common.ui.theme.ThemeManager
import chat.simplex.common.ui.theme.ThemeOverrides
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.PresetWallpaper
import chat.simplex.common.views.helpers.WallpaperType
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

private const val CURRENT_THEME = "CurrentTheme"
private const val SYSTEM_DARK_THEME = "SystemDarkTheme"
private const val ONE_HAND_UI = "OneHandUI"
private const val RED_HEX = "#ffff0000"
private val FLOWERS = PresetWallpaper.FLOWERS.filename
private val SCHOOL = PresetWallpaper.SCHOOL.filename

class AppearanceTest : UiTest() {
  @Test fun appearance_settingsPersistAcrossRestart() = uiTest {
    val before = app("alice").appearance()
    assertEquals("System", before.colorModeText())
    assertEquals(DefaultTheme.SYSTEM_THEME_NAME, CurrentColors.value.name)
    assertEquals(1f, before.fontScale, 0.001f)
    if (appPlatform.isDesktop) assertEquals(1f, before.zoom, 0.001f)
    check(storedSetting("CurrentTheme") != "DARK" && storedSetting("FontScale") != "1.2" && storedSetting("DensityScale") != "1.5") { "the new values are stored before the change" }
    before.colorMode("DARK")
    poll(what = "the dark theme to be applied") { CurrentColors.value.name == DefaultTheme.DARK.themeName && CurrentColors.value.base == DefaultTheme.DARK }
    before.fontScale = 1.2f
    if (appPlatform.isDesktop) before.zoom = 1.5f
    poll(what = "theme, font scale and zoom to be stored") {
      storedSetting("CurrentTheme") == "DARK" && storedSetting("FontScale") == "1.2" && (!appPlatform.isDesktop || storedSetting("DensityScale") == "1.5")
    }
    val after = restart().appearance()
    assertEquals(DefaultTheme.DARK, CurrentColors.value.base)
    assertEquals("Dark", after.colorModeText())
    assertEquals(1.2f, after.fontScale, 0.001f)
    if (appPlatform.isDesktop) assertEquals(1.5f, after.zoom, 0.001f)
  }

  @Test fun appearance_colorMode_systemLightDark() = uiTest {
    val appearance = app("alice").appearance()
    check(CurrentColors.value.name == DefaultTheme.SYSTEM_THEME_NAME && storedSetting(CURRENT_THEME) != "LIGHT" && storedSetting(SYSTEM_DARK_THEME).let { it != "BLACK" && it != "SIMPLEX" }) {
      "the color mode before the change: ${CurrentColors.value.name}, stored ${storedSetting(CURRENT_THEME)} / ${storedSetting(SYSTEM_DARK_THEME)}"
    }
    appearance.colorMode("LIGHT")
    poll(what = "the light theme applied and stored") {
      CurrentColors.value.base == DefaultTheme.LIGHT && storedSetting(CURRENT_THEME) == "LIGHT" && appearance.colorModeText() == "Light"
    }
    appearance.darkModeColors(DefaultTheme.BLACK.themeName)
    poll(what = "Black stored as the dark mode colors") { storedSetting(SYSTEM_DARK_THEME) == "BLACK" }
    check(CurrentColors.value.base == DefaultTheme.LIGHT && storedSetting(CURRENT_THEME) == "LIGHT") {
      "choosing the dark mode colors in light mode changed the theme: ${CurrentColors.value.name}, stored ${storedSetting(CURRENT_THEME)}"
    }
    appearance.colorMode("DARK")
    poll(what = "dark mode applied with the Black colors") {
      CurrentColors.value.base == DefaultTheme.BLACK && CurrentColors.value.colors.background == BlackColorPalette.background &&
        storedSetting(CURRENT_THEME) == "BLACK" && appearance.colorModeText() == "Dark" && appearance.darkModeColorsText() == "Black"
    }
    appearance.darkModeColors(DefaultTheme.SIMPLEX.themeName)
    poll(what = "the SimpleX colors applied at once in dark mode") {
      CurrentColors.value.base == DefaultTheme.SIMPLEX && storedSetting(CURRENT_THEME) == "SIMPLEX" && storedSetting(SYSTEM_DARK_THEME) == "SIMPLEX"
    }
    appearance.colorMode("null")
    poll(what = "the System color mode applied and stored") {
      CurrentColors.value.name == DefaultTheme.SYSTEM_THEME_NAME && storedSetting(CURRENT_THEME) == DefaultTheme.SYSTEM_THEME_NAME && appearance.colorModeText() == "System"
    }
  }

  @Test fun appearance_wallpaperPreset_select() = uiTest {
    val appearance = app("alice").appearance()
    appearance.colorMode("LIGHT")
    poll(what = "the light theme with the default wallpaper") { CurrentColors.value.base == DefaultTheme.LIGHT && currentWallpaperPreset() == SCHOOL }
    check(StoredThemes.active(DefaultTheme.LIGHT)?.wallpaper?.preset != FLOWERS) { "the flowers wallpaper is stored before it was chosen" }
    click(Ui.Appearance.Wallpaper(FLOWERS))
    poll(what = "the flowers wallpaper applied and stored for the light theme") {
      currentWallpaperPreset() == FLOWERS && StoredThemes.active(DefaultTheme.LIGHT)?.wallpaper?.preset == FLOWERS
    }
    appearance.colorMode("DARK")
    poll(what = "the dark theme with its default wallpaper") { CurrentColors.value.base == DefaultTheme.DARK && currentWallpaperPreset() == SCHOOL }
    check(readThemeOverrides().none { it.base == DefaultTheme.DARK && it.wallpaper?.preset == FLOWERS }) { "the light theme's wallpaper was stored for the dark theme" }
    appearance.colorMode("LIGHT")
    poll(what = "the flowers wallpaper in the light theme again") { CurrentColors.value.base == DefaultTheme.LIGHT && currentWallpaperPreset() == FLOWERS }
    restart()
    poll(what = "the flowers wallpaper in the light theme after a restart") { CurrentColors.value.base == DefaultTheme.LIGHT && currentWallpaperPreset() == FLOWERS }
  }

  @Test fun appearance_themeDestination_perUser() = uiTest {
    val alice = app("alice")
    val userId = chatModel.currentUser.value!!.userId
    val dark = !CurrentColors.value.colors.isLight
    val yamlBefore = StoredThemes.yamlBytes()
    check(coreUiThemesPreset(userId, dark) == null) { "alice has a theme in the core before Apply to" }
    alice.appearance()
    click(Ui.Appearance.ApplyTo)
    click(Ui.Appearance.ApplyTo.option(userId))
    poll(what = "Apply to showing alice") { textOf(Ui.Appearance.ApplyTo) == "alice" }
    click(Ui.Appearance.Wallpaper(FLOWERS))
    poll(what = "the flowers wallpaper set as alice's theme and the save finished") {
      chatModel.currentUser.value?.uiThemes?.preferredMode(dark)?.wallpaper?.preset == FLOWERS && UiIdle.inFlight().isEmpty()
    }
    assertEquals("alice's wallpaper in the core", FLOWERS, coreUiThemesPreset(userId, dark))
    check(StoredThemes.yamlBytes().contentEquals(yamlBefore) && readThemeOverrides().none { it.wallpaper?.preset == FLOWERS }) {
      "the per-user wallpaper changed the app theme: ${readThemeOverrides()}"
    }
  }

  @Test fun appearance_reachableAppToolbars() = uiTest {
    val alice = app("alice")
    poll(what = "the new-chat button in the toolbar and no floating button") { exists(Ui.ChatList.NewChat) && !exists(Ui.ChatList.NewChatFab) }
    check(storedSetting(ONE_HAND_UI) != "false") { "Reachable app toolbars is stored as off before the change" }
    alice.appearance()
    click(Ui.Appearance.OneHandUI)
    poll(what = "Reachable app toolbars stored as off") { storedSetting(ONE_HAND_UI) == "false" }
    closeAppearance()
    poll(what = "the floating new-chat button instead of the toolbar button") { exists(Ui.ChatList.NewChatFab) && !exists(Ui.ChatList.NewChat) }
    alice.appearance()
    click(Ui.Appearance.OneHandUI)
    poll(what = "Reachable app toolbars stored as on") { storedSetting(ONE_HAND_UI) == "true" }
    closeAppearance()
    poll(what = "the toolbar new-chat button again") { exists(Ui.ChatList.NewChat) && !exists(Ui.ChatList.NewChatFab) }
  }

  @Test fun customizeTheme_editInterfaceColor() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val base = CurrentColors.value.base
    val defaultAccent = CurrentColors.value.colors.primary
    check(defaultAccent != Color.Red && StoredThemes.active(base)?.colors?.primary == null) { "the accent is red or stored before the change" }
    customizeTheme(alice)
    check(!exists(Ui.Appearance.ResetColors)) { "Reset colors is offered before any color was changed" }
    click(Ui.Appearance.Accent)
    replaceText(Ui.Appearance.ColorHex, "#ff0000")
    poll(what = "the red accent applied and stored in themes.yaml") { CurrentColors.value.colors.primary == Color.Red && StoredThemes.active(base)?.colors?.primary == RED_HEX }
    escape()
    poll(what = "Customize theme with Reset colors") { exists(Ui.Appearance.ResetColors) && !exists(Ui.Appearance.ColorHex) }
    click(Ui.Appearance.ResetColors)
    poll(what = "the default accent applied, removed from themes.yaml, and Reset colors gone") {
      CurrentColors.value.colors.primary == defaultAccent && StoredThemes.active(base)?.colors?.primary == null && !exists(Ui.Appearance.ResetColors)
    }
  }

  @Test fun customizeTheme_exportImportTheme() = uiTest {
    val alice = app("alice")
    val base = CurrentColors.value.base
    setAccentViaApi(base, Color.Red)
    customizeTheme(alice)
    val exported = File(exchangeDir(), "exported.theme")
    check(!exported.exists()) { "the export file exists before the export" }
    saveFileAs(exported) { click(Ui.Appearance.ExportTheme) }
    poll(what = "the exported theme file with the accent") { exported.exists() && "accent" in exported.readText() }
    val text = exported.readText()
    val theme = yaml.decodeFromString(ThemeOverrides.serializer(), text)
    check("themeId" !in text && theme.base == base && theme.colors.primary == RED_HEX) { "exported theme:\n$text" }
    click(Ui.Appearance.ResetColors)
    poll(what = "the accent reset before the import") { CurrentColors.value.colors.primary != Color.Red && StoredThemes.active(base)?.colors?.primary == null }
    openFile(exported) { click(Ui.Appearance.ImportTheme) }
    poll(what = "the imported red accent applied and stored, with its base theme selected") {
      CurrentColors.value.colors.primary == Color.Red && StoredThemes.active(base)?.colors?.primary == RED_HEX && storedSetting(CURRENT_THEME) == base.themeName
    }
    val yamlBefore = StoredThemes.yamlBytes()
    val invalid = File(exchangeDir(), "invalid.theme").apply { writeText("colors: [accent") }
    openFile(invalid) { click(Ui.Appearance.ImportTheme) }
    poll(what = "the Import theme error alert") { seesText("Import theme error") && AlertManager.shared.hasAlertsShown() }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the Import theme error alert to close") { !AlertManager.shared.hasAlertsShown() }
    check(StoredThemes.yamlBytes().contentEquals(yamlBefore) && CurrentColors.value.colors.primary == Color.Red) { "the invalid theme changed the stored themes" }
  }

  @Test fun appearance_wallpaperImage_import() = uiTest {
    val alice = app("alice")
    alice.appearance()
    val base = CurrentColors.value.base
    val image = jpegFile("wallpaper.jpg")
    waitFor(Ui.Appearance.ColorMode)
    check(CurrentColors.value.wallpaper.type !is WallpaperType.Image && StoredThemes.active(base)?.wallpaper?.imageFile == null && !exists(Ui.Appearance.RemoveImage)) {
      "an image wallpaper or `Remove image` before the import: ${CurrentColors.value.wallpaper.type}"
    }
    scrollTo(Ui.Appearance.WallpaperImage)
    openFile(image) { click(Ui.Appearance.WallpaperImage) }
    var name: String? = null
    poll(what = "the image applied as the wallpaper, copied to the wallpapers folder, stored for the theme, with `Remove image`") {
      name = (CurrentColors.value.wallpaper.type as? WallpaperType.Image)?.filename
      name?.let { n -> File(getWallpaperFilePath(n)).let { it.exists() && it.readBytes().contentEquals(image.readBytes()) } && StoredThemes.active(base)?.wallpaper?.imageFile == n } == true &&
        exists(Ui.Appearance.RemoveImage)
    }
    val copy = File(getWallpaperFilePath(name!!))
    click(Ui.Appearance.RemoveImage)
    poll(what = "the image wallpaper gone, the copied image deleted and the theme without it") {
      CurrentColors.value.wallpaper.type !is WallpaperType.Image && !copy.exists() && StoredThemes.active(base)?.wallpaper?.imageFile == null && !exists(Ui.Appearance.RemoveImage)
    }
  }

  private fun enableDeveloperToolsViaApi() = appPrefs.developerTools.set(true)

  private fun setAccentViaApi(base: DefaultTheme, color: Color) = ThemeManager.saveAndApplyThemeColor(base, ThemeColor.PRIMARY, color)

  private fun coreUiThemesPreset(userId: Long, dark: Boolean): String? =
    runBlocking { chatController.listUsers(null) }.first { it.user.userId == userId }.user.uiThemes?.preferredMode(dark)?.wallpaper?.preset
}
