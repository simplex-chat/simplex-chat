package chat.simplex.app

import chat.simplex.common.platform.isDarkThemeName
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LinuxThemeTest {
  @Test
  fun darkThemeNamesAreDark() {
    assertTrue(isDarkThemeName("Adwaita-dark"))
    assertTrue(isDarkThemeName("Yaru-Dark"))
    assertTrue(isDarkThemeName("Arc-Darker"))
  }

  @Test
  fun lightThemeNamesAreLight() {
    assertFalse(isDarkThemeName("Adwaita"))
    assertFalse(isDarkThemeName("Yaru"))
    assertFalse(isDarkThemeName(""))
  }
}
