# Linux: crash on text that needs a bitmap or WOFF2 fallback font

## Problem

On Linux the desktop app ends with SIGSEGV in `SkTypeface_fontconfig::onOpenStream` when a character's fallback font is a PCF, BDF, PFR, Windows FNT, Type 42 or WOFF2 font.

## Fix

`apps/multiplatform/build.gradle.kts` resolves every `org.jetbrains.skiko` module requested at 0.9.4.2 (Skia m132) to 0.9.24 (Skia m138), on all desktop OSes. Skia m138 skips a font that its font scanner does not load.

- Compose 1.8.2 links against 0.9.24: every Skiko class, method and field that its jars reference exists in 0.9.24.
- The rule applies only to 0.9.4.2, so a Compose update that brings another Skiko version is not overridden; `SkikoVersionTest` fails then.
- Android is unchanged.

## Text rendering

Skia m138 adds the paragraph's default metrics to every line (`TextWrapper.cpp` of JetBrains Skia), so a line in a smaller span or another font can move by 1 px.

| OS | Previews that differ from m132 | Change |
|---|---|---|
| Windows | 25 of 262 | 1 px vertical shifts; Segoe UI Emoji drawn from its COLR v1 table |
| macOS | 23 of 262 | 1 px vertical shifts |
| Linux | 57 of 260 | 1 px vertical shifts and FreeType glyph pixels; lines with an emoji 1 px taller |

On Windows and macOS no preview changes size; on Linux the chat help is 2 px taller.

## Tests

- `FallbackFontTest` builds a fontconfig cache for one BDF font with `fc-cache`, then lays out U+2603 in a child JVM.
- `SkikoVersionTest` checks the resolved Skiko version.
- `Dockerfile.build` installs `fontconfig`, which provides `fc-cache` in CI.
- `BubbleLayoutTest.timeDrawnOnTextBaseline` asserts the offset of the time from the text baseline for each text style and font scale: 0 px, or 1 px either way in 6 of 27 cases.
- The 57 desktop snapshot references that differ by more than the comparison tolerance are re-recorded with Skiko 0.9.24.
