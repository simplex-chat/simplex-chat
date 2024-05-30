package chat.simplex.app

import chat.simplex.common.ui.theme.*
import kotlin.test.Test
import kotlin.test.assertEquals

// use this command for testing:
// ./gradlew desktopTest
class ThemesTest {
  @Test
  fun testSkipDuplicates() {
    val r = ArrayList<ThemeOverrides>()
    r.add(ThemeOverrides("UUID", DefaultTheme.DARK))
    r.add(ThemeOverrides("UUID", DefaultTheme.DARK))
    r.add(ThemeOverrides("UUID", DefaultTheme.LIGHT))
    r.add(ThemeOverrides("UUID2", DefaultTheme.DARK))
    r.add(ThemeOverrides("UUID3", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper()))
    r.add(ThemeOverrides("UUID4", DefaultTheme.LIGHT, wallpaper = null))
    r.add(ThemeOverrides("UUID5", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "something")))
    r.add(ThemeOverrides("UUID5", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "something2")))
    r.add(ThemeOverrides("UUID6", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "something2")))
    r.add(ThemeOverrides("UUID7", DefaultTheme.DARK, wallpaper = ThemeWallpaper(preset = "something2")))
    r.add(ThemeOverrides("UUID8", DefaultTheme.DARK, wallpaper = ThemeWallpaper(imageFile = "image")))
    r.add(ThemeOverrides("UUID9", DefaultTheme.DARK, wallpaper = ThemeWallpaper(imageFile = "image2")))
    r.add(ThemeOverrides("UUID10", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(imageFile = "image")))
    assertEquals(
      r.skipDuplicates(), listOf(
        ThemeOverrides("UUID", DefaultTheme.DARK),
        ThemeOverrides("UUID3", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper()),
        ThemeOverrides("UUID5", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "something")),
        ThemeOverrides("UUID6", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "something2")),
        ThemeOverrides("UUID7", DefaultTheme.DARK, wallpaper = ThemeWallpaper(preset = "something2")),
        ThemeOverrides("UUID8", DefaultTheme.DARK, wallpaper = ThemeWallpaper(imageFile = "image")),
        ThemeOverrides("UUID10", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(imageFile = "image"))
      )
    )
  }
}
