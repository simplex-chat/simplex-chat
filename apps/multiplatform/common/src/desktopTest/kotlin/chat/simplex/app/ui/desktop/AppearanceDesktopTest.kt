package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closeAppearance
import chat.simplex.common.ui.Ui
import org.junit.Test
import java.util.Locale

private const val APP_LANGUAGE = "AppLanguage"

class AppearanceDesktopTest : UiTest() {
  @Test fun appearance_language_switch() = uiTest {
    val alice = app("alice")
    check(storedSetting(APP_LANGUAGE) == null && Locale.getDefault().language == "en") { "the app language before the change: ${storedSetting(APP_LANGUAGE)}, ${Locale.getDefault()}" }
    selectLanguage(alice, "de")
    poll(what = "German stored as the app language and applied") { storedSetting(APP_LANGUAGE) == "de" && Locale.getDefault().language == "de" }
    closeAppearance()
    assertSettingsStrings(alice, "Einstellungen", "Darstellung")
    selectLanguageFromOpenSettings("system")
    poll(what = "the app language cleared and the system language applied") { storedSetting(APP_LANGUAGE) == null && Locale.getDefault().language == "en" }
    closeAppearance()
    assertSettingsStrings(alice, "Settings", "Appearance")
  }

  private fun UiScope.selectLanguage(app: App, tag: String) {
    app.appearance()
    click(Ui.Appearance.Language)
    click(Ui.Appearance.Language.option(tag))
  }

  private fun UiScope.selectLanguageFromOpenSettings(tag: String) {
    click(Ui.Settings.Appearance)
    click(Ui.Appearance.Language)
    click(Ui.Appearance.Language.option(tag))
  }

  private fun UiScope.assertSettingsStrings(app: App, settings: String, appearance: String) {
    app.userPicker()
    poll(what = "the user picker's settings row reading `$settings`") { textOf(Ui.UserPicker.Settings) == settings }
    click(Ui.UserPicker.Settings)
    poll(what = "the Appearance row reading `$appearance`") { exists(Ui.Settings.Appearance) && textOf(Ui.Settings.Appearance) == appearance }
  }
}
