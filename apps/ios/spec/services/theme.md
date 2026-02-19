# SimpleX Chat iOS -- Theme Engine

> Technical specification for the theming system: ThemeManager, default themes, customization layers, wallpapers, and YAML export.
>
> Related specs: [State Management](../state.md) | [Architecture](../architecture.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

**Source:** [`ThemeManager.swift`](../../Shared/Theme/ThemeManager.swift) | [`AppearanceSettings.swift`](../../Shared/Views/UserSettings/AppearanceSettings.swift) | [`ThemeTypes.swift`](../../SimpleXChat/Theme/ThemeTypes.swift) | [`ChatWallpaperTypes.swift`](../../SimpleXChat/Theme/ChatWallpaperTypes.swift) | [`Theme.swift`](../../Shared/Theme/Theme.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [ThemeManager](#2-thememanager)
3. [Default Themes](#3-default-themes)
4. [Customization Layers](#4-customization-layers)
5. [Color System](#5-color-system)
6. [Wallpapers](#6-wallpapers)
7. [Chat Bubble Styling](#7-chat-bubble-styling)
8. [Color Scheme Mode](#8-color-scheme-mode)
9. [YAML Export/Import](#9-yaml-exportimport)

---

## 1. Overview

The theme engine provides a layered customization system where themes can be overridden at multiple levels: global defaults, per-user, and per-chat.

```
Theme Resolution Order (most specific wins):
┌─────────────────────┐
│  Per-chat override   │  apiSetChatUIThemes(chatId:, themes:)
├─────────────────────┤
│  Per-user override   │  apiSetUserUIThemes(userId:, themes:)
├─────────────────────┤
│  App settings theme  │  themeOverridesDefault (UserDefaults)
├─────────────────────┤
│  Base theme          │  Light / Dark / SimpleX / Black
└─────────────────────┘
```

The resolved theme is published as `AppTheme.shared` and consumed by all SwiftUI views via `@EnvironmentObject`.

---

## 2. [ThemeManager](../../Shared/Theme/ThemeManager.swift) (L15)

**File**: [`Shared/Theme/ThemeManager.swift`](../../Shared/Theme/ThemeManager.swift)

Static utility class that resolves the current theme by merging all customization layers.

### [ActiveTheme](../../Shared/Theme/ThemeManager.swift#L17)

The resolved theme output:

```swift
struct ActiveTheme: Equatable {
    let name: String            // Theme name (e.g., "light", "dark", "simplex", "black", "system")
    let base: DefaultTheme      // Base theme enum
    let colors: Colors          // Resolved color palette
    let appColors: AppColors    // App-specific colors (sent/received bubbles, etc.)
    var wallpaper: AppWallpaper // Resolved wallpaper
}
```

### Key Static Methods

| Method | Purpose | Line |
|--------|---------|------|
| [`applyTheme(_:)`](../../Shared/Theme/ThemeManager.swift#L124) | Apply a theme by name, updates `AppTheme.shared` | [L118](../../Shared/Theme/ThemeManager.swift#L124) |
| [`currentColors(...)`](../../Shared/Theme/ThemeManager.swift#L64) | Resolve full theme from all layers | [L60](../../Shared/Theme/ThemeManager.swift#L64) |
| [`defaultActiveTheme(_:)`](../../Shared/Theme/ThemeManager.swift#L48) | Get default theme override from app settings | [L45](../../Shared/Theme/ThemeManager.swift#L48) |
| [`currentThemeOverridesForExport(...)`](../../Shared/Theme/ThemeManager.swift#L105) | Get current overrides for YAML export | [L100](../../Shared/Theme/ThemeManager.swift#L105) |
| [`adjustWindowStyle()`](../../Shared/Theme/ThemeManager.swift#L136) | Adjust window style after theme change | [L129](../../Shared/Theme/ThemeManager.swift#L136) |
| [`changeDarkTheme(_:)`](../../Shared/Theme/ThemeManager.swift#L166) | Change the dark theme variant | [L159](../../Shared/Theme/ThemeManager.swift#L166) |
| [`saveAndApplyThemeColor(...)`](../../Shared/Theme/ThemeManager.swift#L173) | Save and apply a theme color override | [L165](../../Shared/Theme/ThemeManager.swift#L173) |
| [`applyThemeColor(...)`](../../Shared/Theme/ThemeManager.swift#L186) | Apply a theme color to a binding | [L178](../../Shared/Theme/ThemeManager.swift#L186) |
| [`saveAndApplyWallpaper(...)`](../../Shared/Theme/ThemeManager.swift#L191) | Save and apply a wallpaper change | [L182](../../Shared/Theme/ThemeManager.swift#L191) |
| [`copyFromSameThemeOverrides(...)`](../../Shared/Theme/ThemeManager.swift#L213) | Copy overrides from matching theme | [L204](../../Shared/Theme/ThemeManager.swift#L213) |
| [`applyWallpaper(...)`](../../Shared/Theme/ThemeManager.swift#L256) | Apply wallpaper to a binding | [L247](../../Shared/Theme/ThemeManager.swift#L256) |
| [`saveAndApplyThemeOverrides(...)`](../../Shared/Theme/ThemeManager.swift#L267) | Save and apply full theme overrides | [L257](../../Shared/Theme/ThemeManager.swift#L267) |
| [`resetAllThemeColors(_:)`](../../Shared/Theme/ThemeManager.swift#L288) | Reset all color overrides (CodableDefault) | [L277](../../Shared/Theme/ThemeManager.swift#L288) |
| [`resetAllThemeColors(_:)`](../../Shared/Theme/ThemeManager.swift#L302) | Reset all color overrides (Binding) | [L291](../../Shared/Theme/ThemeManager.swift#L302) |
| [`removeTheme(_:)`](../../Shared/Theme/ThemeManager.swift#L311) | Remove a saved theme by ID | [L299](../../Shared/Theme/ThemeManager.swift#L311) |

### Theme Resolution Algorithm

[`currentColors()`](../../Shared/Theme/ThemeManager.swift#L64) in `ThemeManager.swift`:

1. Determine base theme from `currentThemeDefault`:
   - If `"system"`: use light or dark based on [`systemInDarkThemeCurrently`](../../Shared/Theme/Theme.swift#L95)
   - Dark mode maps to `systemDarkThemeDefault` (Dark, SimpleX, or Black)
2. Get base color palette ([`LightColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L650), [`DarkColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L629), [`SimplexColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L671), [`BlackColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L692))
3. Look up app settings theme override (`themeOverridesDefault`)
4. Look up per-user theme override (`User.uiThemes`)
5. Look up per-chat theme override (from ChatInfo)
6. Look up wallpaper preset colors (if wallpaper has preset color overrides)
7. Merge layers: base <- app override <- preset wallpaper colors <- per-user <- per-chat
8. Return `ActiveTheme` with resolved colors, app colors, and wallpaper

---

## 3. Default Themes

Four built-in themes with pre-defined color palettes:

| Theme | Enum | Key Characteristics |
|-------|------|---------------------|
| **Light** | `DefaultTheme.LIGHT` | White background, standard colors |
| **Dark** | `DefaultTheme.DARK` | Dark gray background, light text |
| **SimpleX** | `DefaultTheme.SIMPLEX` | Brand purple accents, dark background |
| **Black** | `DefaultTheme.BLACK` | Pure black background (OLED), high contrast |

### [DefaultTheme](../../SimpleXChat/Theme/ThemeTypes.swift#L13) Enum

```swift
enum DefaultTheme {
    case LIGHT
    case DARK
    case SIMPLEX
    case BLACK

    static let SYSTEM_THEME_NAME = "SYSTEM"

    var themeName: String { ... }
    var mode: DefaultThemeMode { ... }  // .light or .dark
}
```

### Color Palettes

Each base theme defines two palette types:
- [`Colors`](../../SimpleXChat/Theme/ThemeTypes.swift#L44): Standard UI colors (primary, background, surface, error, onBackground, onSurface)
- [`AppColors`](../../SimpleXChat/Theme/ThemeTypes.swift#L90): App-specific colors (sentMessage, receivedMessage, title, primaryVariant2)

Palette instances:
- [`LightColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L650) / [`LightColorPaletteApp`](../../SimpleXChat/Theme/ThemeTypes.swift#L662)
- [`DarkColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L629) / [`DarkColorPaletteApp`](../../SimpleXChat/Theme/ThemeTypes.swift#L641)
- [`SimplexColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L671) / [`SimplexColorPaletteApp`](../../SimpleXChat/Theme/ThemeTypes.swift#L683)
- [`BlackColorPalette`](../../SimpleXChat/Theme/ThemeTypes.swift#L692) / [`BlackColorPaletteApp`](../../SimpleXChat/Theme/ThemeTypes.swift#L704)

---

## 4. Customization Layers

### Layer 1: App Settings Theme

Stored in `themeOverridesDefault` (UserDefaults). Contains `[ThemeOverrides]` -- an array of theme overrides, one per base theme.

#### [`ThemeOverrides`](../../SimpleXChat/Theme/ThemeTypes.swift#L385)

```swift
struct ThemeOverrides: Codable {
    var base: DefaultTheme
    var colors: ThemeColors?       // Color overrides
    var wallpaper: ThemeWallpaper? // Wallpaper setting
}
```

### Layer 2: Per-User Theme

Stored on the `User` object (`User.uiThemes: ThemeModeOverrides?`), persisted in the Haskell database via `apiSetUserUIThemes(userId:, themes:)`.

#### [`ThemeModeOverrides`](../../SimpleXChat/Theme/ThemeTypes.swift#L570)

```swift
struct ThemeModeOverrides: Codable {
    var light: ThemeModeOverride?
    var dark: ThemeModeOverride?
}
```

#### [`ThemeModeOverride`](../../SimpleXChat/Theme/ThemeTypes.swift#L585)

```swift
struct ThemeModeOverride: Codable {
    var mode: DefaultThemeMode?
    var colors: ThemeColors?
    var wallpaper: ThemeWallpaper?
    var type: WallpaperType?         // Computed from wallpaper
}
```

### Layer 3: Per-Chat Theme

Stored per-chat via `apiSetChatUIThemes(chatId:, themes:)`. Same `ThemeModeOverrides` structure.

### Override Merging

Colors are merged field-by-field: if a more-specific layer defines a color, it overrides; if nil, falls through to the next layer.

---

## 5. Color System

**File**: [`SimpleXChat/Theme/ThemeTypes.swift`](../../SimpleXChat/Theme/ThemeTypes.swift)

### [ThemeColors](../../SimpleXChat/Theme/ThemeTypes.swift#L230)

Overridable color definitions:

```swift
struct ThemeColors: Codable {
    var primary: String?           // Primary brand color
    var primaryVariant: String?    // Primary variant
    var secondary: String?         // Secondary color
    var secondaryVariant: String?  // Secondary variant
    var background: String?        // Main background
    var surface: String?           // Card/surface background
    var title: String?             // Title text color
    var primaryVariant2: String?   // Additional variant
    var sentMessage: String?       // Sent message bubble
    var sentQuote: String?         // Sent quote background
    var receivedMessage: String?   // Received message bubble
    var receivedQuote: String?     // Received quote background
}
```

Colors are stored as hex strings (e.g., `"#FF6600"`) and converted to SwiftUI `Color` values at resolution time.

### [Colors](../../SimpleXChat/Theme/ThemeTypes.swift#L44) (Resolved Palette)

```swift
struct Colors {
    var isLight: Bool
    var primary: Color
    var primaryVariant: Color
    var secondary: Color
    var secondaryVariant: Color
    var background: Color
    var surface: Color
    var error: Color
    var onBackground: Color
    var onSurface: Color
    // ... etc
}
```

### [AppColors](../../SimpleXChat/Theme/ThemeTypes.swift#L90) (Resolved App-Specific)

```swift
struct AppColors {
    var title: Color
    var primaryVariant2: Color
    var sentMessage: Color
    var sentQuote: Color
    var receivedMessage: Color
    var receivedQuote: Color
}
```

---

## 6. Wallpapers

**File**: [`SimpleXChat/Theme/ChatWallpaperTypes.swift`](../../SimpleXChat/Theme/ChatWallpaperTypes.swift)

### [Preset Wallpapers](../../SimpleXChat/Theme/ChatWallpaperTypes.swift#L13)

6 built-in wallpaper presets:

| Preset | ID | Description |
|--------|-----|-------------|
| Cats | `cats` | Cat-themed pattern |
| Flowers | `flowers` | Floral pattern |
| Hearts | `hearts` | Heart pattern |
| Kids | `kids` | Children's pattern |
| School | `school` | School/notebook pattern (default) |
| Travel | `travel` | Travel-themed pattern |

Each preset defines per-theme color tints (`PresetWallpaper.colors[DefaultTheme]`) that subtly adjust the color palette to complement the wallpaper.

### Custom Wallpapers

Users can set a custom image as wallpaper:
- Stored in `Documents/wallpapers/` directory
- Scaled and tiled to fill the chat background
- Custom wallpapers can be combined with color overrides

### [WallpaperType](../../SimpleXChat/Theme/ChatWallpaperTypes.swift#L311)

```swift
enum WallpaperType {
    case preset(filename: String, scale: Float?)   // Built-in wallpaper
    case image(filename: String, scale: Float?)    // Custom image
    case empty                                      // No wallpaper
}
```

### [AppWallpaper](../../SimpleXChat/Theme/ThemeTypes.swift#L142) (Resolved)

```swift
struct AppWallpaper {
    var background: Color?      // Background color override
    var tint: Color?            // Tint/overlay color
    var type: WallpaperType
}
```

---

## 7. Chat Bubble Styling

Configurable bubble appearance properties:

| Property | Description | Stored In |
|----------|-------------|-----------|
| `chatItemRoundness` | Corner radius of message bubbles | App settings |
| `chatItemTail` | Whether bubbles have a tail/arrow | App settings |
| Avatar corner radius | Roundness of profile avatars | App settings |

These are configured in [`Shared/Views/UserSettings/AppearanceSettings.swift`](../../Shared/Views/UserSettings/AppearanceSettings.swift) ([L25](../../Shared/Views/UserSettings/AppearanceSettings.swift#L26)).

---

## 8. Color Scheme Mode

### System Follow

When theme is set to `"system"` (DefaultTheme.SYSTEM_THEME_NAME):
- Light mode: uses `DefaultTheme.LIGHT` palette
- Dark mode: uses the configured dark theme (`systemDarkThemeDefault`), which can be Dark, SimpleX, or Black

### Forced Mode

Users can force light or dark mode regardless of system setting by selecting a specific theme other than "system".

### Detection

[`systemInDarkThemeCurrently`](../../Shared/Theme/Theme.swift#L95):

```swift
static var systemInDarkThemeCurrently: Bool {
    UIScreen.main.traitCollection.userInterfaceStyle == .dark
}
```

`ChatModel.currentUser` setter triggers [`ThemeManager.applyTheme()`](../../Shared/Theme/ThemeManager.swift#L124) to handle per-user theme overrides when switching users.

---

## 9. YAML Export/Import

Theme configurations can be exported as YAML for sharing:

### Export

[`ThemeManager.currentThemeOverridesForExport()`](../../Shared/Theme/ThemeManager.swift#L105) generates a `ThemeOverrides` representing the current resolved theme, which is then serialized to YAML using the Yams library.

### Import

YAML theme strings are parsed back into `ThemeOverrides` and applied as app settings theme overrides.

Key functions in [`AppearanceSettings.swift`](../../Shared/Views/UserSettings/AppearanceSettings.swift):

| Function | Purpose | Line |
|----------|---------|------|
| [`ImportExportThemeSection`](../../Shared/Views/UserSettings/AppearanceSettings.swift#L603) | UI section for import/export controls | [L600](../../Shared/Views/UserSettings/AppearanceSettings.swift#L603) |
| [`ThemeImporter`](../../Shared/Views/UserSettings/AppearanceSettings.swift#L640) | ViewModifier for YAML file import | [L636](../../Shared/Views/UserSettings/AppearanceSettings.swift#L640) |
| [`decodeYAML(_:)`](../../Shared/Views/UserSettings/AppearanceSettings.swift#L1150) | Parse YAML string into Decodable type | [L1145](../../Shared/Views/UserSettings/AppearanceSettings.swift#L1150) |
| [`encodeThemeOverrides(_:)`](../../Shared/Views/UserSettings/AppearanceSettings.swift#L1160) | Encode ThemeOverrides to YAML string | [L1154](../../Shared/Views/UserSettings/AppearanceSettings.swift#L1160) |

### Toolbar Material

[`ToolbarMaterial`](../../Shared/Views/UserSettings/AppearanceSettings.swift#L319) controls the navigation bar appearance:
- Configurable opacity/material (translucent, opaque)
- Stored in app settings

---

## Source Files

| File | Path | Key Definitions |
|------|------|-----------------|
| Theme manager | [`Shared/Theme/ThemeManager.swift`](../../Shared/Theme/ThemeManager.swift) | `ThemeManager` (L15), `ActiveTheme` (L17) |
| Theme types & colors | [`SimpleXChat/Theme/ThemeTypes.swift`](../../SimpleXChat/Theme/ThemeTypes.swift) | `DefaultTheme` (L13), `Colors` (L44), `AppColors` (L90), `AppWallpaper` (L142), `ThemeColors` (L230), `ThemeWallpaper` (L302), `ThemeOverrides` (L385), `ThemeModeOverrides` (L570), `ThemeModeOverride` (L585) |
| Wallpaper types | [`SimpleXChat/Theme/ChatWallpaperTypes.swift`](../../SimpleXChat/Theme/ChatWallpaperTypes.swift) | `PresetWallpaper` (L13), `WallpaperType` (L311) |
| Color utilities | [`SimpleXChat/Theme/Color.swift`](../../SimpleXChat/Theme/Color.swift) | Hex color conversion |
| App theme observable | [`Shared/Theme/Theme.swift`](../../Shared/Theme/Theme.swift) | `AppTheme` (L22), `CurrentColors` (L14), `systemInDarkThemeCurrently` (L95) |
| Appearance settings UI | [`Shared/Views/UserSettings/AppearanceSettings.swift`](../../Shared/Views/UserSettings/AppearanceSettings.swift) | `AppearanceSettings` (L26), `ToolbarMaterial` (L319), `ImportExportThemeSection` (L603) |
| Theme mode editor | `Shared/Views/Helpers/ThemeModeEditor.swift` | Theme mode selection UI |
| Haskell theme types | `../../src/Simplex/Chat/Types/UITheme.hs` | Server-side theme persistence |
