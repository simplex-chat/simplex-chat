package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.ChatDataScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.WhatsNewScreen
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.ui.theme.DefaultThemeMode
import org.junit.Test

private const val CURRENT_THEME = "CurrentTheme"
private const val CHAT_STOPPED = "Chat is stopped"
private const val ABOUT_TEXT = "Private and secure messaging."

private class SettingsRow(val name: String, val key: UiKey, val title: String, val content: String) {
  fun shown(s: UiScope): Boolean = s.seesText(title) && s.seesText(content)
}

private val APPEARANCE = SettingsRow("Appearance", Ui.Settings.Appearance, "Appearance", "Color mode")
private val PRIVACY = SettingsRow("Your privacy", Ui.Settings.Privacy, "Your privacy", "More privacy")
private val HELP = SettingsRow("Help & support", Ui.Settings.Help, "Help & support", "How to use it")
private val CHAT_DATA = SettingsRow("Chat data", Ui.Settings.Database, "Chat data", "Database passphrase & export")
private val NETWORK = SettingsRow("Network & servers", Ui.Settings.Network, "Network & servers", "Advanced network settings")
private val CALLS = SettingsRow("Audio & video calls", Ui.Settings.Calls, "Your calls", "Always use relay")
private val VERSION = SettingsRow("the version row", Ui.Settings.Version, "App version", "Developer tools")
private val ROWS = listOf(APPEARANCE, PRIVACY, HELP, CHAT_DATA, NETWORK, CALLS, VERSION)

class SettingsRootTest : UiTest() {
  @Test fun settingsRoot_rowsOpenTheirScreens() = uiTest {
    val alice = app("alice")
    alice.settings()
    poll(what = "Your settings with all its rows") { seesText("Your settings") && ROWS.all { exists(it.key) } }
    check(ROWS.none { it.shown(this) }) { "a settings screen is shown before any row was clicked: ${ROWS.filter { it.shown(this) }.map { it.name }}" }
    for (row in ROWS) {
      click(row.key)
      poll(what = "${row.name} open in place of Your settings, and no other settings screen") {
        row.shown(this) && !exists(Ui.Settings.Appearance) && ROWS.none { it !== row && seesText(it.content) }
      }
      escape()
      poll(what = "Your settings again after closing ${row.name}") { exists(Ui.Settings.Appearance) && !seesText(row.content) }
    }
    escape()
    poll(what = "Your settings to close") { !exists(Ui.Settings.Appearance) && exists(Ui.ChatList.Search) }
  }

  @Test fun settingsRoot_itemsDisabledWhenChatStopped() = uiTest {
    val alice = app("alice")
    alice.settings()
    check(CHAT_STOPPED !in descriptionsOf(Ui.Settings.Database)) { "Chat data is marked as stopped while the chat runs" }
    click(Ui.Settings.Database)
    waitFor(Ui.Database.Management)
    ChatDataScreen(this).management()
    click(Ui.Database.RunChat)
    poll(what = "the Stop chat? alert with Stop") { seesText("Stop chat?") && "Stop" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the chat stopped on screen and in the core") { seesText(CHAT_STOPPED) && chatModel.chatRunning.value == false && !chatRunningInCore() }
    escape()
    poll(what = "Chat data again") { !exists(Ui.Database.RunChat) && exists(Ui.Database.Management) }
    escape()
    poll(what = "Your settings with Chat data marked as stopped") { exists(Ui.Settings.Privacy) && CHAT_STOPPED in descriptionsOf(Ui.Settings.Database) }
    for (row in listOf(PRIVACY, NETWORK, CALLS)) {
      click(row.key)
      settle()
      check(exists(Ui.Settings.Appearance) && !row.shown(this)) { "${row.name} opened while the chat is stopped" }
    }
    click(Ui.Settings.Migrate)
    settle()
    check(exists(Ui.Settings.Appearance) && !seesText("Migrate device")) { "Migrate to another device opened while the chat is stopped" }
    click(Ui.Settings.Help)
    poll(what = "Help & support opening while the chat is stopped") { exists(Ui.Help.HowToUse) && !exists(Ui.Settings.Appearance) }
    click(Ui.Help.HowToUse)
    settle()
    check(exists(Ui.Help.HowToUse) && !seesText("Welcome alice!")) { "How to use it opened while the chat is stopped" }
    click(Ui.Help.WhatsNew)
    settle()
    check(exists(Ui.Help.WhatsNew) && !WhatsNewScreen(this).shown()) { "What's new opened while the chat is stopped" }
    click(Ui.Help.About)
    poll(what = "About SimpleX Chat opening while the chat is stopped") { seesText(ABOUT_TEXT) && !exists(Ui.Help.HowToUse) }
  }

  @Test fun userPicker_colorModeSwitcher() = uiTest {
    val alice = app("alice")
    val systemMode = CurrentColors.value.base.mode
    val flipped = if (systemMode == DefaultThemeMode.LIGHT) DefaultThemeMode.DARK else DefaultThemeMode.LIGHT
    check(CurrentColors.value.name == DefaultTheme.SYSTEM_THEME_NAME && storedSetting(CURRENT_THEME).let { it == null || it == DefaultTheme.SYSTEM_THEME_NAME }) {
      "the color mode is not System before the switch: ${CurrentColors.value.name}, stored ${storedSetting(CURRENT_THEME)}"
    }
    alice.userPicker()
    click(Ui.UserPicker.ColorMode)
    poll(what = "the opposite color mode applied and stored") { CurrentColors.value.base.mode == flipped && storedSetting(CURRENT_THEME) == themeName(flipped) }
    click(Ui.UserPicker.ColorMode)
    poll(what = "the first color mode applied again, stored as an explicit theme") {
      CurrentColors.value.base.mode == systemMode && storedSetting(CURRENT_THEME) == themeName(systemMode)
    }
    rightClick(Ui.UserPicker.ColorMode)
    poll(what = "the System color mode applied and stored") {
      CurrentColors.value.name == DefaultTheme.SYSTEM_THEME_NAME && storedSetting(CURRENT_THEME) == DefaultTheme.SYSTEM_THEME_NAME
    }
  }

  private fun themeName(mode: DefaultThemeMode): String =
    if (mode == DefaultThemeMode.LIGHT) DefaultTheme.LIGHT.themeName else appPrefs.systemDarkTheme.get()!!
}
