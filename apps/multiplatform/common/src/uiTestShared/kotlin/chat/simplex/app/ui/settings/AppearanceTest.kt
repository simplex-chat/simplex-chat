package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.platform.appPlatform
import org.junit.Assert.assertEquals
import org.junit.Test

class AppearanceTest : UiTest() {
  @Test fun appearance_settingsPersistAcrossRestart() = uiTest {
    val before = app("alice").appearance()
    before.colorMode("DARK")
    before.fontScale = 1.2f
    if (appPlatform.isDesktop) before.zoom = 1.5f
    poll(what = "theme, font scale and zoom to be stored") {
      storedSetting("CurrentTheme") == "DARK" && storedSetting("FontScale") == "1.2" && (!appPlatform.isDesktop || storedSetting("DensityScale") == "1.5")
    }
    val after = restart().appearance()
    assertEquals("Dark", after.colorModeText())
    assertEquals(1.2f, after.fontScale, 0.001f)
    if (appPlatform.isDesktop) assertEquals(1.5f, after.zoom, 0.001f)
  }
}
