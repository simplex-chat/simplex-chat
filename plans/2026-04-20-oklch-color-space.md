# oklch color space for theme customization

## Table of contents

1. [Summary](#summary)
2. [Who benefits](#who-benefits)
3. [Problem](#problem)
4. [Current state](#current-state)
5. [Design: oklch as primary color model](#design-oklch-as-primary-color-model)
6. [Scope](#scope)
7. [Cross-platform design](#cross-platform-design)
8. [Backward compatibility](#backward-compatibility)
9. [Resolved decisions](#resolved-decisions)
10. [Open questions](#open-questions)

---

## Summary

Replace RGB/HSV color model with oklch as the primary color space for theme customization. Users get a perceptually uniform color picker where lightness, chroma, and hue behave predictably. Theme files store oklch values alongside hex for backward compatibility. Display P3 wide-gamut colors are supported on capable screens. Same file format and same behavior on iOS and Android.

---

## Who benefits

### Theme designers and color-conscious users

The primary audience is users who customize theme colors — adjusting accent colors, message bubble tints, wallpaper backgrounds. These users interact directly with the color picker and notice its limitations. In HSV, "shift the hue slightly warmer" unpredictably changes perceived brightness. In oklch, hue, lightness, and chroma are independent — the picker does what you expect.

Theme designers also create and share themes. oklch values are portable: a theme designed on an sRGB Android phone looks correct on a Display P3 iPhone, and vice versa. No platform-specific color drift.



### All users with Display P3 screens

iPhone 7+ (2016), flagship Android (2018+) — the majority of active users on modern hardware. They benefit automatically, without any action:

- Preset wallpaper palettes are stored in oklch and rendered in the widest gamut the screen supports. On P3 screens, preset colors appear slightly more vivid and true-to-intent.
- Shared themes created by designers with P3 screens preserve the full color range when viewed on other P3 devices.

The visual difference is subtle for pastel colors (low chroma) but noticeable for saturated accent colors. The benefit grows as more themes use the expanded gamut.

### The project

oklch aligns with CSS Color Level 4 (the current web standard), Tailwind CSS v4, and modern design systems (Radix, Open Props). It is a forward-looking technical foundation — not a proprietary choice that may need replacing later.

oklch is also the necessary foundation for the planned redesign of preset theme palettes. Redesigning them in oklch ensures that lightness relationships between message bubbles, backgrounds, and accents are predictable and consistent across all 6 wallpapers × 4 themes.

No other messaging app uses oklch for theme customization. This is a quiet differentiator: designers and color professionals will notice.

---

## Problem

### 1. HSV is not perceptually uniform

The current color picker uses HSV (hue, saturation, value). In HSV, perceived brightness depends on hue: yellow at S:100 V:100 looks much brighter than blue at the same values. When a user changes hue while keeping S and V constant, the perceived brightness jumps unpredictably. "Make it slightly warmer" requires guessing how to shift three interdependent channels.

### 2. No wide-gamut support

Modern phones (iPhone 7+, 2016; flagship Android) have Display P3 screens capable of showing ~25% more colors than sRGB. The app is locked to sRGB — users with capable hardware see the same limited palette as 2012-era screens.

---

## Current state

### Color picker

- **Android:** `ClassicColorPicker` from `com.godaddy.android:colorpicker` library. Uses `HsvColor` internally. Layout: square (saturation × value) + hue strip. Hex input field visible only in developer mode.
  - File: `Appearance.kt`, line 1264
- **iOS:** Native SwiftUI `ColorPicker`. Three tabs: grid, spectrum, sliders (RGB/HSB). Hex input field on the sliders tab, labeled "Display P3" on P3-capable devices. Already supports wide-gamut color selection.
  - File: `AppearanceSettings.swift`, line 1035

### Theme storage

- Format: YAML (`themes.yaml`)
- Colors: hex strings, `#AARRGGBB` (alpha, red, green, blue)
- Library: KAML 0.59.0 (Kotlin), Yams (iOS)
- Parser config: `strictMode = false` (Kotlin), default Codable (iOS) — both silently ignore unknown keys
- Files: `ThemeManager.kt:224-240` (Kotlin hex conversion), `Color.swift:49-130` (iOS hex conversion)

### Theme color slots

`ThemeColors` defines 12 color properties (all optional hex strings):

| Slot | YAML key | Purpose |
|------|----------|---------|
| primary | `accent` | Accent color |
| primaryVariant | `accentVariant` | Accent variant |
| primaryVariant2 | `accentVariant2` | Accent variant 2 |
| secondary | `secondary` | Secondary |
| secondaryVariant | `secondaryVariant` | Secondary variant |
| background | `background` | App background |
| surface | `menus` | Menus/surfaces |
| title | `title` | Title text |
| sentMessage | `sentMessage` | Sent message bubble |
| sentQuote | `sentReply` | Sent quote bubble |
| receivedMessage | `receivedMessage` | Received message bubble |
| receivedQuote | `receivedReply` | Received quote bubble |

Additionally, wallpapers have 2 color properties: `background` and `tint`.

Files: `Theme.kt:182-220` (Kotlin), `ThemeTypes.swift:230-298` (iOS)

### Preset wallpaper colors

6 presets (cats, flowers, hearts, kids, school, travel) × 4 themes (LIGHT, DARK, SIMPLEX, BLACK) × 6 color values (background, tint, sentMessage, sentQuote, receivedMessage, receivedQuote) = 144 hardcoded hex values.

Files: `ChatWallpaper.kt:25-242` (Kotlin), `ChatWallpaperTypes.swift` (iOS)

### Theme hierarchy

Three levels, each overrides the previous:

1. Global app theme (`appPrefs.themeOverrides`)
2. Per-user theme (`user.uiThemes`)
3. Per-chat theme (chat settings)

---

## Design: oklch as primary color model

### Why oklch

oklch has three perceptually independent axes:

- **L** (lightness): 0 = black, 1 = white. Perceptually uniform — equal steps in L produce equal perceived lightness differences regardless of position on the scale.
- **C** (chroma): 0 = gray, ~0.4 = maximum saturation. How colorful, independent of lightness.
- **H** (hue): 0–360° color wheel. Changing H at constant L and C produces no perceived brightness shift.

oklch covers the full range of both sRGB and Display P3. It is the color model used by CSS Color Level 4 specification, Tailwind CSS v4, and modern design systems (Radix, Open Props).

oklch is the cylindrical (polar) form of oklab. The relationship: `a = C × cos(H × π/180)`, `b = C × sin(H × π/180)` (H in degrees). This is relevant because Android Compose natively supports oklab — oklch↔oklab conversion is trivial trigonometry.

### Color picker

**Layout (identical on iOS and Android):**

```
┌─────────────────────┐ ┌──┐
│                     │ │  │
│   L (lightness)     │ │  │
│   ↕                 │ │ H│
│                     │ │  │
│        C (chroma) → │ │  │
│                     │ │  │
└─────────────────────┘ └──┘
      main square        hue
                        strip
```

- Main square: X axis = chroma (left: gray → right: vivid), Y axis = lightness (bottom: dark → top: light)
- Vertical strip: hue (0–360°)

Replaces:
- Android: GoDaddy `ClassicColorPicker` (HSV) → custom oklch picker
- iOS: native SwiftUI `ColorPicker` → custom oklch picker (see Open questions, #1 for alternatives)

The square shows the full oklch range. On sRGB screens, colors beyond sRGB gamut are clamped by the display — the slider continues to move but visible change stops. On Display P3 screens, more of the range is visible. The saved value is always the full oklch, regardless of screen capability.

**Developer mode fields:**

Two text input fields (visible only with developer tools enabled):

1. **oklch field** (primary): `oklch(0.95 0.03 85)` or with alpha: `oklch(0.95 0.03 85 / 0.8)`
2. **hex field** (secondary): `#FAF3E2` or with alpha: `#ccFAF3E2`

Editing either field updates the other and the picker position. Pasting a hex from Figma/Photoshop converts to oklch automatically.

**iOS option:** An optional toggle to switch to the native Apple ColorPicker for users who prefer it. Decision for founder.

### Theme file format

Dual storage — oklch primary, hex fallback:

```yaml
themes:
  - themeId: "abc-123"
    base: LIGHT
    colors:
      sentMessage: "#ffFAF3E2"
      sentMessageOklch: "oklch(0.96 0.03 85)"
      sentReply: "#ffF3E4D5"
      sentReplyOklch: "oklch(0.93 0.04 70)"
      receivedMessage: "#ffF8F0DE"
      receivedMessageOklch: "oklch(0.95 0.05 75)"
      receivedReply: "#ffEDE3D0"
      receivedReplyOklch: "oklch(0.91 0.06 72)"
    wallpaper:
      preset: "cats"
      background: "#ffFAF3E2"
      backgroundOklch: "oklch(0.96 0.03 85)"
      tint: "#ffEFDCA6"
      tintOklch: "oklch(0.89 0.08 90)"
```

**Key naming convention:** existing key + `Oklch` suffix. Each hex key gets a parallel oklch key. This preserves all existing keys and parsing logic.

**oklch is the source of truth. Hex is a lossy fallback.**

oklch can describe colors beyond sRGB (Display P3 and wider). Hex (`#AARRGGBB`) is limited to sRGB — it cannot represent P3 colors exactly. When a user picks a color outside sRGB, the hex key stores the nearest sRGB approximation, not the exact color. This is analogous to a JPEG preview alongside a RAW file.

New app versions never use hex for rendering — they read oklch and convert directly to the screen's native color space (sRGB or Display P3). Hex exists in the file solely so that old app versions (which don't understand oklch) can still display a reasonable approximation.

**Reading priority:**
- New app version: reads `*Oklch` keys, converts to screen color space. Hex keys ignored.
- Old app version: reads hex keys (sRGB approximation), silently ignores unknown `*Oklch` keys (verified in code: KAML `strictMode = false`, Swift Codable default behavior)

**Alpha in oklch:** CSS Color 4 syntax — `oklch(L C H / alpha)`. Alpha range 0–1. If omitted, alpha = 1.0.

**Import scenarios:**

| Source | Behavior |
|--------|----------|
| Old theme (hex only) | Auto-convert hex → oklch on import. Both keys stored. |
| New theme (oklch + hex) | Read oklch. Hex available as fallback. |
| Manual edit (user edits YAML) | If only oklch edited: hex regenerated on next save. If only hex edited: oklch regenerated. If both edited and conflict: oklch wins. |

### Display P3

Automatic — no user toggle, no settings.

- oklch values are converted to the widest color space the screen supports
- Display P3 screens: colors rendered in P3 gamut
- sRGB screens: colors clamped to sRGB (graceful degradation — slightly less saturated, not broken)

The oklch value in the theme file is display-independent. The same theme looks slightly more vivid on P3 screens and slightly less on sRGB screens — this is correct behavior, analogous to how ProPhoto RGB images render on different monitors.

### Hardcoded preset colors

144 hex values in `ChatWallpaper.kt` and `ChatWallpaperTypes.swift` are converted to oklch representation:

```kotlin
// Before
sentMessage = "#ffFAF3E2"

// After
sentMessage = "#ffFAF3E2"  // sRGB fallback
sentMessageOklch = "oklch(0.96 0.03 85)"
```

The conversion is lossless for all current preset colors (all are within sRGB gamut, low chroma C < 0.15).

---

## Scope

oklch applies to **user-customizable colors only** — everything that passes through the color picker and theme file:

- 12 ThemeColors slots (accent, background, menus, sent/received bubbles, etc.)
- 2 wallpaper colors (background, tint)
- 144 hardcoded preset wallpaper values (converted to oklch in code)

**Not affected:** hardcoded UI colors that are outside the theme system — shadows, dividers, status indicators, system element colors. These remain hex in code. Users cannot customize them.

When a user overrides all 12 theme slots, every themed element in the app uses oklch-sourced colors. Non-themed elements are unaffected.

**Rendering compatibility:** At runtime, oklch colors are converted to standard Color objects (sRGB or P3 RGB values). The rendering engine sees no difference between an oklch-sourced color and a hardcoded hex color — they are both RGB values on screen. No compatibility issues.

**Edge case:** If a user picks a high-chroma oklch color outside sRGB gamut (P3-only), it may appear more vivid than neighboring hardcoded sRGB elements on P3 screens. This is a design consideration, not a technical issue — the colors render correctly side by side.

---

## Cross-platform design

| Aspect | Android | iOS |
|--------|---------|-----|
| Color picker | Custom oklch picker (Compose) | Custom oklch picker (SwiftUI) |
| oklch → screen color | oklch → oklab (3 lines) → `ColorSpaces.Oklab` (native Compose) | oklch → linear sRGB (matrix multiply, ~15 lines) → `Color` |
| Display P3 | `ColorSpaces.DisplayP3` + manifest `colorMode` flag | `Color(.displayP3, red:green:blue:)` |
| Theme file | YAML via KAML | YAML via Yams |
| Hex conversion | `toReadableHex()` / `colorFromReadableHex()` in `ThemeManager.kt` | `toReadableHex()` / `colorFromReadableHex()` in `Color.swift` |
| External dependency | None (remove GoDaddy colorpicker) | None (native ColorPicker removed if custom picker chosen — see Open questions, #1) |

**Conversion pipeline — identical result on both platforms:**

```
oklch(L, C, H)
  → oklab: a = C × cos(H×π/180), b = C × sin(H×π/180)
  → linear sRGB: matrix multiply (Ottosson 2021)
  → gamma sRGB: transfer function (IEC 61966-2-1)
  → or Display P3: different matrix, same pipeline
```

One oklch value = one visual result everywhere. The math is deterministic — no platform-specific approximations.

---

## Backward compatibility

| Scenario | Result |
|----------|--------|
| New app exports theme → old app imports | Old app reads hex keys, ignores `*Oklch` keys. Full color fidelity in sRGB. |
| Old app exports theme → new app imports | New app reads hex keys, auto-converts to oklch. |
| New app → new app | Reads oklch keys. Hex keys ignored but kept in file for potential re-export to old versions. |
| User manually edits YAML, removes oklch keys | App falls back to hex, regenerates oklch on next save. |
| User manually edits YAML, removes hex keys | App reads oklch, regenerates hex on next save. |

**No migration step required.** Existing `themes.yaml` files are read as-is. Oklch keys are added on next save.

**Parser safety verified in code:**
- Kotlin KAML: `strictMode = false` in `SimpleXAPI.kt:6097` — unknown keys silently ignored
- iOS Swift Codable: default behavior ignores unknown keys
- Per-theme try-catch in `readThemeOverrides()` (`Files.kt:125-147`): a malformed individual theme logs error but doesn't break other themes

---

## Resolved decisions

1. **Color space: oklch.** Not CIE Lab (non-uniform in blue zone), not HSL (not perceptually uniform), not raw oklab (cylindrical form is more intuitive for users — hue is an angle, not two abstract axes).

2. **Dual storage (oklch + hex).** Backward compatible. Cost: ~14 extra key-value pairs per theme (color section roughly doubles in size). Benefit: zero migration, old/new versions interoperate.

3. **Custom oklch picker as primary on both platforms.** Consistency over native feel. One design to maintain. The oklch picker layout (square + strip) mirrors the current HSV layout — minimal learning curve. Whether iOS also offers a toggle to Apple's native picker is an open question (see Open questions, #1).

4. **Full oklch range in picker, no gamut clamping in UI.** sRGB screens show their maximum; the saved value preserves full intent. Display P3 screens show more. No artificial boundary drawn.

5. **No external color libraries.** oklch↔sRGB conversion is ~15 lines of math per platform. No attack surface, no dependency management, no version conflicts.

6. **Display P3 automatic, no user toggle.** The app renders in the widest gamut the hardware supports.

7. **oklch key naming: existing key + `Oklch` suffix.** `sentMessage` → `sentMessageOklch`. Flat structure (not nested objects) to preserve existing String-type parsing. Nested `{ hex, oklch }` would be a breaking change — old parsers expect String, not Map.

8. **Alpha slider: current behavior preserved.** Alpha slider shown only for colors that already support transparency (wallpaper tint, message bubbles). oklch alpha syntax follows CSS Color 4: `oklch(0.95 0.03 85 / 0.8)`.

9. **Color picker rendering: per-pixel computation, not platform gradients.** The L×C square and H strip are rendered as bitmaps where each pixel is individually computed from its oklch coordinates and converted to screen color. This ensures the picker looks identical on both platforms regardless of platform gradient interpolation behavior.

10. **Gamut indicator in developer mode.** A small label ("sRGB" or "Display P3") shown in developer mode next to the color picker. Lets designers know whether their screen shows the full oklch range or a clamped version.

---

## Open questions

1. **Custom oklch picker vs. native pickers on iOS.** Apple's native ColorPicker already supports Display P3 wide-gamut colors (confirmed: hex field labeled "Display P3" on P3 devices). It does NOT support oklch/LCH axes — it uses HSB sliders, which are not perceptually uniform. Three options:
   - **(a) Custom oklch picker on both platforms.** Users get perceptually uniform L/C/H axes everywhere. Requires building a new picker component for both iOS and Android. More work, but delivers the full oklch experience. Consistent cross-platform behavior.
   - **(b) Custom oklch picker + optional toggle to Apple's native picker on iOS.** oklch picker is primary. An option in settings lets users switch to the familiar Apple picker if they prefer. Accommodates users who rely on Apple's color management workflow (e.g., pasting exact P3 hex values from design tools). Android: custom oklch picker only (no native alternative exists).
   - **(c) Native pickers + oklch storage only.** Keep Apple's picker on iOS, build a P3-capable picker for Android. oklch is used for storage and theme files only — the picker UI uses HSB. Least work, P3 support achieved, but no perceptual uniformity in the picker.
   Recommendation: (a) or (b). If the primary goal is perceptual uniformity, (a). If accommodating iOS power users matters, (b). Option (c) defeats the perceptual uniformity goal.
