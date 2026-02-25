# Theme Engine

## Table of Contents

1. [Overview](#1-overview)
2. [ThemeManager](#2-thememanager)
3. [Default Themes](#3-default-themes)
4. [Theme Types](#4-theme-types)
5. [Color System](#5-color-system)
6. [SimpleXTheme Composable](#6-simplextheme-composable)
7. [Platform Theme](#7-platform-theme)
8. [YAML Import/Export](#8-yaml-importexport)
9. [Source Files](#9-source-files)

## Executive Summary

The SimpleX Chat theme engine implements a four-level cascade: per-chat theme overrides take precedence over per-user overrides, which take precedence over global (app-settings) overrides, which take precedence over built-in presets. Four preset themes exist (LIGHT, DARK, SIMPLEX, BLACK), each defining a Material `Colors` palette and custom `AppColors` for chat-specific elements. Themes support wallpaper customization (preset patterns or custom images) with background and tint color overrides. Theme configuration is persisted as YAML and can be imported/exported. The `SimpleXTheme` composable wraps `MaterialTheme` with additional `CompositionLocal` providers for app colors and wallpaper.

---

## 1. Overview

Theme resolution follows a priority chain:

```
per-chat override > per-user override > global override > preset default
```

At each level, individual color properties can be overridden. Unspecified properties fall through to the next level. The resolution is performed by `ThemeManager.currentColors()`, which merges all levels into a single `ActiveTheme` containing Material `Colors`, `AppColors`, and `AppWallpaper`.

Wallpapers follow the same cascade, with additional support for preset wallpapers (built-in patterns like `SCHOOL`) and custom images. Wallpaper presets can define their own color overrides that sit between the global override and the base preset.

---

## 2. ThemeManager

[`ThemeManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt) (241 lines)

A singleton `object` that manages theme state, persistence, and resolution.

### Core resolution

**`currentColors()`** ([line 57](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L57)):

```kotlin
fun currentColors(
  themeOverridesForType: WallpaperType?,
  perChatTheme: ThemeModeOverride?,
  perUserTheme: ThemeModeOverrides?,
  appSettingsTheme: List<ThemeOverrides>
): ActiveTheme
```

This is the core resolution function. It:
1. Determines the non-system theme name (resolving `SYSTEM` to light or dark based on `systemInDarkThemeCurrently`).
2. Selects the base theme palette (LIGHT/DARK/SIMPLEX/BLACK).
3. Finds the matching `ThemeOverrides` from `appSettingsTheme` based on wallpaper type and theme name.
4. Selects the `perUserTheme` for the current light/dark mode.
5. Resolves wallpaper preset colors if applicable.
6. Merges all color layers via `toColors()`, `toAppColors()`, and `toAppWallpaper()`.

Returns `ActiveTheme(name, base, colors, appColors, wallpaper)`.

### Theme application

**`applyTheme()`** ([line 105](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L105)):

Persists the theme name, recalculates `CurrentColors`, and updates Android system bar appearance:

```kotlin
fun applyTheme(theme: String) {
  if (appPrefs.currentTheme.get() != theme) {
    appPrefs.currentTheme.set(theme)
  }
  CurrentColors.value = currentColors(null, null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  platform.androidSetNightModeIfSupported()
  platform.androidSetStatusAndNavigationBarAppearance(c.isLight, c.isLight)
}
```

**`changeDarkTheme()`** ([line 115](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L115)):

Sets the dark mode variant (DARK, SIMPLEX, or BLACK) and recalculates colors.

### Color and wallpaper modification

**`saveAndApplyThemeColor()`** ([line 120](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L120)):

Persists a single color change to the global theme overrides:
1. Gets or creates `ThemeOverrides` for the current base theme.
2. Calls `withUpdatedColor()` to update the specific `ThemeColor`.
3. Updates `currentThemeIds` mapping.
4. Recalculates `CurrentColors`.

**`applyThemeColor()`** ([line 132](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L132)):

In-memory-only color change (for per-chat/per-user theme editing before save).

**`saveAndApplyWallpaper()`** ([line 136](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L136)):

Persists wallpaper type change. Finds or creates matching `ThemeOverrides` (matching by wallpaper type + theme name), updates the wallpaper, and persists.

### Reset

**`resetAllThemeColors()` (global)** ([line 204](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L204)):

Resets all custom colors in the current global theme override to defaults. Preserves wallpaper but clears its background and tint overrides.

**`resetAllThemeColors()` (per-chat/per-user)** ([line 213](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L213)):

In-memory reset of a `ThemeModeOverride` state.

### Import/Export

**`saveAndApplyThemeOverrides()`** ([line 188](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L188)):

Imports a complete `ThemeOverrides` (from YAML). Handles wallpaper image import (base64 to file), replaces existing override for the same type, and applies.

**`currentThemeOverridesForExport()`** ([line 92](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L92)):

Exports the fully resolved current theme as a `ThemeOverrides` with all colors filled and wallpaper image embedded as base64.

### Utility

**`colorFromReadableHex()`** ([line 224](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L224)):

Parses `#AARRGGBB` hex string to `Color`.

**`toReadableHex()`** ([line 227](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L227)):

Converts `Color` to `#AARRGGBB` hex string with intelligent alpha handling.

---

## 3. Default Themes

[`Theme.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L26):

```kotlin
enum class DefaultTheme {
  LIGHT, DARK, SIMPLEX, BLACK;

  companion object {
    const val SYSTEM_THEME_NAME: String = "SYSTEM"
  }
}
```

| Theme | `mode` | Description |
|---|---|---|
| `LIGHT` | LIGHT | Standard light theme with white/light gray surfaces |
| `DARK` | DARK | Standard dark theme with dark gray surfaces |
| `SIMPLEX` | DARK | SimpleX branded dark theme with deep blue background and cyan accent |
| `BLACK` | DARK | AMOLED-optimized pure black theme |

`SYSTEM` is a virtual theme name that resolves to LIGHT or the configured dark variant at runtime.

`DefaultThemeMode` ([line 46](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L46)): `LIGHT` or `DARK`, serialized as `"light"` / `"dark"`.

---

## 4. Theme Types

### AppColors (line 51)

[`Theme.kt` L53](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L53):

```kotlin
@Stable
class AppColors(
  title: Color,
  primaryVariant2: Color,
  sentMessage: Color,
  sentQuote: Color,
  receivedMessage: Color,
  receivedQuote: Color,
)
```

Mutable state properties (for efficient recomposition) representing chat-specific colors not covered by Material's `Colors`.

### AppWallpaper (line 103)

[`Theme.kt` L106](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L106):

```kotlin
@Stable
class AppWallpaper(
  background: Color? = null,
  tint: Color? = null,
  type: WallpaperType = WallpaperType.Empty,
)
```

Represents the active wallpaper state with optional background color, tint overlay, and wallpaper type (Empty, Preset, or Image).

### ThemeColor (line 136)

Enum of all customizable color slots:

`PRIMARY`, `PRIMARY_VARIANT`, `SECONDARY`, `SECONDARY_VARIANT`, `BACKGROUND`, `SURFACE`, `TITLE`, `PRIMARY_VARIANT2`, `SENT_MESSAGE`, `SENT_QUOTE`, `RECEIVED_MESSAGE`, `RECEIVED_QUOTE`, `WALLPAPER_BACKGROUND`, `WALLPAPER_TINT`

Each has a `fromColors()` method to extract the current value and a `text` property for UI display.

### ThemeColors (line 178)

[`Theme.kt` L183](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L183):

Serializable data class with optional hex color strings for each slot. Uses `@SerialName` annotations for YAML compatibility (`accent` for `primary`, `accentVariant` for `primaryVariant`, `menus` for `surface`, etc.).

### ThemeWallpaper (line 218)

[`Theme.kt` L224](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L224):

```kotlin
@Serializable
data class ThemeWallpaper(
  val preset: String? = null,       // Preset wallpaper name
  val scale: Float? = null,         // Wallpaper scale factor
  val scaleType: WallpaperScaleType? = null,  // Fill/fit mode
  val background: String? = null,   // Background color hex
  val tint: String? = null,         // Tint overlay color hex
  val image: String? = null,        // Base64-encoded image (for import/export)
  val imageFile: String? = null,    // Local image file name
)
```

Key methods:
- `toAppWallpaper()`: Converts to runtime `AppWallpaper`.
- `withFilledWallpaperBase64()`: Embeds the image as base64 for export.
- `importFromString()`: Saves a base64 image to disk and returns a copy with `imageFile` set.
- `from(type, background, tint)`: Factory from `WallpaperType`.

### ThemeOverrides (line 297)

[`Theme.kt` L304](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L304):

```kotlin
@Serializable
data class ThemeOverrides(
  val themeId: String = UUID.randomUUID().toString(),
  val base: DefaultTheme,
  val colors: ThemeColors = ThemeColors(),
  val wallpaper: ThemeWallpaper? = null,
)
```

A complete theme override entry. Multiple can coexist (one per wallpaper type per base theme). The `themeId` is a UUID for identity tracking. Key methods:
- `isSame(type, themeName)`: Matches by wallpaper type and base theme.
- `withUpdatedColor(name, color)`: Returns a copy with one color changed.
- `toColors()`, `toAppColors()`, `toAppWallpaper()`: Merge with base theme and per-user/per-chat overrides.

### ThemeModeOverrides (line 467)

[`Theme.kt` L475](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L475):

```kotlin
@Serializable
data class ThemeModeOverrides(
  val light: ThemeModeOverride? = null,
  val dark: ThemeModeOverride? = null,
)
```

Container for per-user or per-chat overrides, with separate light and dark mode variants. Stored on the `User` model as `uiThemes`.

### ThemeModeOverride (line 478)

[`Theme.kt` L487](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L487):

```kotlin
@Serializable
data class ThemeModeOverride(
  val mode: DefaultThemeMode = CurrentColors.value.base.mode,
  val colors: ThemeColors = ThemeColors(),
  val wallpaper: ThemeWallpaper? = null,
)
```

A single mode's override with colors and wallpaper. Has `withUpdatedColor()` and `removeSameColors()` (strips colors that match base defaults).

---

## 5. Color System

Four built-in color palettes, each consisting of a Material `Colors` and an `AppColors`:

### DarkColorPalette ([line 634](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L634))

| Property | Value | Notes |
|---|---|---|
| `primary` | `SimplexBlue` | `#0088ff` |
| `surface` | `#222222` | |
| `sentMessage` | `#18262E` | Dark blue-gray |
| `receivedMessage` | `#262627` | Neutral dark |

### LightColorPalette ([line 656](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L656))

| Property | Value | Notes |
|---|---|---|
| `primary` | `SimplexBlue` | `#0088ff` |
| `surface` | `White` | |
| `sentMessage` | `#E9F7FF` | Light blue |
| `receivedMessage` | `#F5F5F6` | Near-white |

### SimplexColorPalette ([line 678](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L678))

| Property | Value | Notes |
|---|---|---|
| `primary` | `#70F0F9` | Cyan |
| `primaryVariant` | `#1298A5` | Dark cyan |
| `background` | `#111528` | Deep navy |
| `surface` | `#121C37` | Dark navy |
| `title` | `#267BE5` | Blue |

### BlackColorPalette ([line 701](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L701))

| Property | Value | Notes |
|---|---|---|
| `primary` | `#0077E0` | Darker blue |
| `background` | `#070707` | Near-black |
| `surface` | `#161617` | Very dark |
| `sentMessage` | `#18262E` | Same as Dark |
| `receivedMessage` | `#1B1B1B` | Very dark |

---

## 6. SimpleXTheme Composable

[`Theme.kt` line 773](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L773):

```kotlin
@Composable
fun SimpleXTheme(darkTheme: Boolean? = null, content: @Composable () -> Unit)
```

The root theme composable that wraps all app content:

1. **System dark mode tracking** ([line 781](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L781)): Uses `snapshotFlow` on `isSystemInDarkTheme()` to call `reactOnDarkThemeChanges()` when the system theme changes. This triggers `ThemeManager.applyTheme(SYSTEM)` if the app is in system theme mode.

2. **User theme tracking** ([line 790](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L790)): Monitors `chatModel.currentUser.value?.uiThemes` and re-applies the theme when the active user changes.

3. **MaterialTheme wrapping** ([line 797](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L797)): Provides `theme.colors` to `MaterialTheme`, plus custom `CompositionLocal` providers:
   - `LocalContentColor` -- set to `MaterialTheme.colors.onBackground`
   - `LocalAppColors` -- the `AppColors` instance (remembered and updated)
   - `LocalAppWallpaper` -- the `AppWallpaper` instance (remembered and updated)
   - `LocalDensity` -- scaled by `desktopDensityScaleMultiplier` and `fontSizeMultiplier`

4. **`SimpleXThemeOverride`** ([line 825](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L825)): A variant that accepts an explicit `ActiveTheme` for per-chat theme previews and overlays.

### CompositionLocal access

```kotlin
val MaterialTheme.appColors: AppColors    // via LocalAppColors
val MaterialTheme.wallpaper: AppWallpaper // via LocalAppWallpaper
```

### Global state

`CurrentColors` ([line 727](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L727)): A `MutableStateFlow<ActiveTheme>` that holds the current resolved theme. Updated by `ThemeManager.applyTheme()` and collected by `SimpleXTheme`.

`systemInDarkThemeCurrently` ([line 724](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L724)): Tracks the current system dark mode state.

---

## 7. Platform Theme

### isSystemInDarkTheme

**Android** ([`Theme.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/ui/theme/Theme.android.kt)):

```kotlin
@Composable
actual fun isSystemInDarkTheme(): Boolean = androidx.compose.foundation.isSystemInDarkTheme()
```

Delegates to the standard Compose function which reads `Configuration.uiMode`.

**Desktop** ([`Theme.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/ui/theme/Theme.desktop.kt)):

```kotlin
private val detector: OsThemeDetector = OsThemeDetector.getDetector()
  .apply { registerListener(::reactOnDarkThemeChanges) }

@Composable
actual fun isSystemInDarkTheme(): Boolean = try {
  detector.isDark
} catch (e: Exception) {
  false  // Fallback for macOS exceptions
}
```

Uses the [jSystemThemeDetector](https://github.com/Dansoftowner/jSystemThemeDetector) library (`OsThemeDetector`). The detector also registers a listener that calls `reactOnDarkThemeChanges()` proactively when the OS theme changes, ensuring the app responds even outside of composition.

### reactOnDarkThemeChanges

[`Theme.kt` line 763](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt#L763):

```kotlin
fun reactOnDarkThemeChanges(isDark: Boolean) {
  systemInDarkThemeCurrently = isDark
  if (appPrefs.currentTheme.get() == DefaultTheme.SYSTEM_THEME_NAME
      && CurrentColors.value.colors.isLight == isDark) {
    ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
  }
}
```

Only triggers a theme switch if the app is in SYSTEM mode and the current light/dark state disagrees with the OS.

---

## 8. YAML Import/Export

Theme overrides are persisted in `themes.yaml` (located in `preferencesDir`).

### readThemeOverrides

[`Files.kt` line 125](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L125):

```kotlin
fun readThemeOverrides(): List<ThemeOverrides>
```

1. Reads `themes.yaml` from `preferencesDir`.
2. Parses the YAML node tree.
3. Extracts the `themes` list.
4. Deserializes each entry as `ThemeOverrides`, skipping entries that fail to parse (with error logging).
5. Calls `skipDuplicates()` to remove entries with the same type+base combination.

### writeThemeOverrides

[`Files.kt` line 151](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L151):

```kotlin
fun writeThemeOverrides(overrides: List<ThemeOverrides>): Boolean
```

1. Serializes `ThemesFile(themes = overrides)` to YAML string.
2. Writes to a temporary file in `preferencesTmpDir`.
3. Atomically moves the temp file to `themes.yaml` using `Files.move` with `REPLACE_EXISTING`.
4. Thread-safe via `synchronized(lock)`.

### YAML format

```yaml
themes:
  - themeId: "uuid-string"
    base: "LIGHT"
    colors:
      accent: "#ff0088ff"
      background: "#ffffffff"
      sentMessage: "#ffe9f7ff"
    wallpaper:
      preset: "school"
      scale: 1.0
      background: "#ccffffff"
      tint: "#22000000"
```

Uses the [kaml](https://github.com/charleskorn/kaml) YAML library for serialization. `ThemeColors` uses `@SerialName` annotations for cross-platform YAML key compatibility (e.g., `accent` for `primary`, `menus` for `surface`).

---

## 9. Source Files

| File | Path | Lines | Description |
|---|---|---|---|
| `ThemeManager.kt` | [`common/src/commonMain/.../ui/theme/ThemeManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt) | 241 | Theme resolution, persistence, color/wallpaper management |
| `Theme.kt` | [`common/src/commonMain/.../ui/theme/Theme.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt) | 838 | Type definitions, color palettes, `SimpleXTheme` composable |
| `Theme.android.kt` | [`common/src/androidMain/.../ui/theme/Theme.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/ui/theme/Theme.android.kt) | 7 | Android `isSystemInDarkTheme` |
| `Theme.desktop.kt` | [`common/src/desktopMain/.../ui/theme/Theme.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/ui/theme/Theme.desktop.kt) | 26 | Desktop `isSystemInDarkTheme` via OsThemeDetector |
| `Files.kt` | [`common/src/commonMain/.../platform/Files.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt) | 191 | `readThemeOverrides()` (L124), `writeThemeOverrides()` (L150) |
