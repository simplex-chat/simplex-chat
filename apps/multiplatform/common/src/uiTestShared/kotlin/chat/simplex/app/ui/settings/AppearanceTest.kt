package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import org.junit.Assert.assertEquals
import org.junit.Test

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
}
