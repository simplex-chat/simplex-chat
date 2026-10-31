package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.yaml
import chat.simplex.common.ui.theme.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.helpers.WallpaperType
import chat.simplex.common.views.helpers.getThemeFromUri
import kotlinx.serialization.encodeToString
import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue

class ThemeOverridesUnitTest {

  private val lightFlowers = ThemeOverrides("light-flowers", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "flowers"))
  private val darkFlowers = ThemeOverrides("dark-flowers", DefaultTheme.DARK, wallpaper = ThemeWallpaper(preset = "flowers"))
  private val lightImage = ThemeOverrides("light-image", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(imageFile = "a.jpg"))
  private val lightNoWallpaper = ThemeOverrides("light-none", DefaultTheme.LIGHT)

  private fun preset(filename: String) = WallpaperType.Preset(filename, null)

  private fun image(filename: String) = WallpaperType.Image(filename, null, null)

  @Test
  fun presetThemeMatchesSamePresetAndBase() {
    assertTrue(lightFlowers.isSame(preset("flowers"), DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun presetThemeDoesNotMatchAnotherPreset() {
    assertFalse(lightFlowers.isSame(preset("school"), DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun presetThemeDoesNotMatchAnotherBase() {
    assertFalse(lightFlowers.isSame(preset("flowers"), DefaultTheme.DARK.themeName))
  }

  @Test
  fun presetThemeDoesNotMatchImageOrEmpty() {
    assertFalse(lightFlowers.isSame(image("a.jpg"), DefaultTheme.LIGHT.themeName))
    assertFalse(lightFlowers.isSame(WallpaperType.Empty, DefaultTheme.LIGHT.themeName))
    assertFalse(lightFlowers.isSame(null, DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun imageThemeMatchesAnyImage() {
    assertTrue(lightImage.isSame(image("a.jpg"), DefaultTheme.LIGHT.themeName))
    assertTrue(lightImage.isSame(image("b.jpg"), DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun imageThemeDoesNotMatchPreset() {
    assertFalse(lightImage.isSame(preset("flowers"), DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun themeWithoutWallpaperMatchesEmptyAndNullOnly() {
    assertTrue(lightNoWallpaper.isSame(WallpaperType.Empty, DefaultTheme.LIGHT.themeName))
    assertTrue(lightNoWallpaper.isSame(null, DefaultTheme.LIGHT.themeName))
    assertFalse(lightNoWallpaper.isSame(preset("flowers"), DefaultTheme.LIGHT.themeName))
    assertFalse(lightNoWallpaper.isSame(image("a.jpg"), DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun themeWithEmptyWallpaperMatchesEmpty() {
    val emptyWallpaper = ThemeOverrides("light-empty", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper())
    assertTrue(emptyWallpaper.isSame(WallpaperType.Empty, DefaultTheme.LIGHT.themeName))
  }

  @Test
  fun replaceBySameThemeIdReplacesInPlace() {
    val updated = lightFlowers.copy(colors = ThemeColors(primary = "#ffff0000"))
    assertEquals(listOf(updated, darkFlowers), listOf(lightFlowers, darkFlowers).replace(updated))
  }

  @Test
  fun replaceByNewIdWithSameWallpaperAndBaseReplacesWithoutDuplicate() {
    val sameType = ThemeOverrides("other-id", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "flowers"))
    assertEquals(listOf(sameType, darkFlowers), listOf(lightFlowers, darkFlowers).replace(sameType))
  }

  @Test
  fun replaceWithNewWallpaperAppends() {
    val school = ThemeOverrides("light-school", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(preset = "school"))
    assertEquals(listOf(lightFlowers, darkFlowers, school), listOf(lightFlowers, darkFlowers).replace(school))
  }

  @Test
  fun replaceOfDarkThemeLeavesLightThemeUntouched() {
    val newDark = ThemeOverrides("new-dark", DefaultTheme.DARK, colors = ThemeColors(primary = "#ff00ff00"), wallpaper = ThemeWallpaper(preset = "flowers"))
    assertEquals(listOf(lightFlowers, newDark), listOf(lightFlowers, darkFlowers).replace(newDark))
  }

  @Test
  fun getThemeByTypeFindsNothingForAnotherBase() {
    assertNull(listOf(lightFlowers).getTheme(null, preset("flowers"), DefaultTheme.DARK))
  }

  @Test
  fun getThemeByTypeFindsThemeOfSameBase() {
    assertEquals(lightFlowers, listOf(lightFlowers, darkFlowers).getTheme(null, preset("flowers"), DefaultTheme.LIGHT))
    assertEquals(darkFlowers, listOf(lightFlowers, darkFlowers).getTheme(null, preset("flowers"), DefaultTheme.DARK))
  }

  @Test
  fun getThemeFindsByIdEvenWhenTypeDiffers() {
    assertEquals(darkFlowers, listOf(lightFlowers, darkFlowers).getTheme("dark-flowers", WallpaperType.Empty, DefaultTheme.LIGHT))
  }

  @Test
  fun getThemeByIdOnly() {
    assertEquals(darkFlowers, listOf(lightFlowers, darkFlowers).getTheme("dark-flowers"))
    assertNull(listOf(lightFlowers, darkFlowers).getTheme("missing"))
    assertNull(listOf(lightFlowers, darkFlowers).getTheme(null))
  }

  @Test
  fun sameThemePicksFirstMatch() {
    val secondImage = ThemeOverrides("light-image-2", DefaultTheme.LIGHT, wallpaper = ThemeWallpaper(imageFile = "b.jpg"))
    assertSame(lightImage, listOf(lightFlowers, lightImage, secondImage).sameTheme(image("b.jpg"), DefaultTheme.LIGHT.themeName))
    assertNull(listOf(lightFlowers, lightImage).sameTheme(image("b.jpg"), DefaultTheme.DARK.themeName))
  }

  @Test
  fun noOverridesMeansNoChangedColor() {
    assertFalse(DefaultTheme.LIGHT.hasChangedAnyColor(null))
    assertFalse(DefaultTheme.LIGHT.hasChangedAnyColor(lightNoWallpaper))
  }

  @Test
  fun changedInterfaceColorIsChangedColor() {
    assertTrue(DefaultTheme.LIGHT.hasChangedAnyColor(lightNoWallpaper.copy(colors = ThemeColors(primary = "#ffff0000"))))
    assertTrue(DefaultTheme.DARK.hasChangedAnyColor(darkFlowers.copy(colors = ThemeColors(receivedQuote = "#ff000000"))))
  }

  @Test
  fun wallpaperBackgroundOrTintIsChangedColor() {
    assertTrue(DefaultTheme.LIGHT.hasChangedAnyColor(lightFlowers.copy(wallpaper = ThemeWallpaper(preset = "flowers", background = "#ff000000"))))
    assertTrue(DefaultTheme.LIGHT.hasChangedAnyColor(lightFlowers.copy(wallpaper = ThemeWallpaper(preset = "flowers", tint = "#ff000000"))))
  }

  @Test
  fun wallpaperWithoutColorsIsNotChangedColor() {
    assertFalse(DefaultTheme.LIGHT.hasChangedAnyColor(lightFlowers))
    assertFalse(DefaultTheme.LIGHT.hasChangedAnyColor(lightImage.copy(wallpaper = ThemeWallpaper(imageFile = "a.jpg", scale = 2f))))
  }

  @Test
  fun updatingInterfaceColorKeepsWallpaper() {
    val updated = lightFlowers.withUpdatedColor(ThemeColor.PRIMARY, "#ffff0000")
    assertEquals(ThemeColors(primary = "#ffff0000"), updated.colors)
    assertEquals(lightFlowers.wallpaper, updated.wallpaper)
    assertEquals(ThemeColors(sentQuote = "#ff00ff00"), lightFlowers.withUpdatedColor(ThemeColor.SENT_QUOTE, "#ff00ff00").colors)
  }

  @Test
  fun resettingInterfaceColorClearsIt() {
    val changed = lightFlowers.copy(colors = ThemeColors(primary = "#ffff0000", title = "#ff000000"))
    assertEquals(ThemeColors(title = "#ff000000"), changed.withUpdatedColor(ThemeColor.PRIMARY, null).colors)
  }

  @Test
  fun updatingWallpaperColorChangesOnlyWallpaper() {
    val updated = lightFlowers.withUpdatedColor(ThemeColor.WALLPAPER_TINT, "#80000000")
    assertEquals(ThemeWallpaper(preset = "flowers", tint = "#80000000"), updated.wallpaper)
    assertEquals(ThemeColors(), updated.colors)
    assertEquals(ThemeWallpaper(preset = "flowers", background = "#ff111111"), lightFlowers.withUpdatedColor(ThemeColor.WALLPAPER_BACKGROUND, "#ff111111").wallpaper)
  }

  @Test
  fun updatingWallpaperColorWithoutWallpaperChangesNothing() {
    assertEquals(lightNoWallpaper, lightNoWallpaper.withUpdatedColor(ThemeColor.WALLPAPER_BACKGROUND, "#ff111111"))
  }

  @Test
  fun resetAllThemeColorsKeepsModeAndWallpaperType() {
    val pref = mutableStateOf(
      ThemeModeOverride(
        mode = DefaultThemeMode.DARK,
        colors = ThemeColors(primary = "#ffff0000", sentMessage = "#ff00ff00"),
        wallpaper = ThemeWallpaper(preset = "flowers", scale = 0.5f, background = "#ff000000", tint = "#80000000")
      )
    )
    ThemeManager.resetAllThemeColors(pref)
    assertEquals(ThemeModeOverride(mode = DefaultThemeMode.DARK, colors = ThemeColors(), wallpaper = ThemeWallpaper(preset = "flowers", scale = 0.5f)), pref.value)
  }

  @Test
  fun resetAllThemeColorsWithoutWallpaper() {
    val pref = mutableStateOf(ThemeModeOverride(mode = DefaultThemeMode.LIGHT, colors = ThemeColors(title = "#ff000000")))
    ThemeManager.resetAllThemeColors(pref)
    assertEquals(ThemeModeOverride(mode = DefaultThemeMode.LIGHT, colors = ThemeColors(), wallpaper = null), pref.value)
  }

  @Test
  fun colorsToReadableHex() {
    assertEquals("#ffff0000", Color.Red.toReadableHex())
    assertEquals("#ff000000", Color.Black.toReadableHex())
    assertEquals("#ffffffff", Color.White.toReadableHex())
    assertEquals("#00ffffff", Color.Transparent.toReadableHex())
    assertEquals("#000088ff", Color(0x000088ff).toReadableHex())
    assertEquals("#00123456", Color(0x00123456).toReadableHex())
    assertEquals("#80ff0000", Color(0x80FF0000).toReadableHex())
  }

  @Test
  fun readableHexRoundTrip() {
    listOf(Color.Red, Color.Black, Color.White, Color(0x80FF0000), Color(0xFF123456), Color(0x000088ff)).forEach {
      assertEquals(it, it.toReadableHex().colorFromReadableHex(), it.toReadableHex())
    }
  }

  @Test
  fun transparentDecodesAsTransparentWhite() {
    assertEquals(Color(0x00FFFFFF), Color.Transparent.toReadableHex().colorFromReadableHex())
  }

  @Test
  fun invalidHexDecodesAsWhite() {
    assertEquals(Color.White, "zz".colorFromReadableHex())
  }

  @Test
  fun exportedThemeImportsWithNewIdAndSameContent() {
    val theme = ThemeOverrides(
      themeId = "exported-id",
      base = DefaultTheme.DARK,
      colors = ThemeColors(primary = "#ffff0000", sentQuote = "#ff00ff00"),
      wallpaper = ThemeWallpaper(preset = "flowers", scale = 0.5f, tint = "#80000000")
    )
    val lines = yaml.encodeToString<ThemeOverrides>(theme).lines()
    assertTrue(lines.first().startsWith("themeId:"), lines.first())
    val imported = yaml.decodeFromString(ThemeOverrides.serializer(), lines.subList(1, lines.size).joinToString("\n"))
    assertNotEquals("exported-id", imported.themeId)
    assertEquals(theme.copy(themeId = imported.themeId), imported)
  }

  private fun importTheme(text: String): ThemeOverrides? {
    var result: ThemeOverrides? = null
    withTempDir { dir ->
      val file: Path = dir.resolve("simplex.theme")
      Files.writeString(file, text)
      result = getThemeFromUri(file.toUri(), withAlertOnException = false)
    }
    return result
  }

  @Test
  fun importsValidThemeFile() {
    val imported = importTheme("base: DARK\ncolors:\n  accent: \"#ffff0000\"\n  sentReply: \"#ff00ff00\"\n")
    assertEquals(DefaultTheme.DARK, imported?.base)
    assertEquals(ThemeColors(primary = "#ffff0000", sentQuote = "#ff00ff00"), imported?.colors)
  }

  @Test
  fun importKeepsWallpaperPreset() {
    val imported = importTheme("base: LIGHT\nwallpaper:\n  preset: \"flowers\"\n  scale: 0.5\n")
    assertEquals(ThemeWallpaper(preset = "flowers", scale = 0.5f), imported?.wallpaper)
  }

  @Test
  fun importIgnoresUnknownKeys() {
    assertEquals(DefaultTheme.SIMPLEX, importTheme("base: SIMPLEX\nunknownKey: 1\n")?.base)
  }

  @Test
  fun importOfInvalidThemeFileReturnsNull() {
    assertNull(importTheme("base: DARK\ncolors: [accent\n"), "malformed yaml")
    assertNull(importTheme(""), "empty file")
    assertNull(importTheme("colors:\n  accent: \"#ffff0000\"\n"), "no base theme")
  }
}
