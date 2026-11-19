# Linux: crash on text that needs a fallback font in a bitmap or WOFF2 format

## Problem

On Linux the desktop app ends with SIGSEGV in `libskiko-linux-x64.so SkTypeface_fontconfig::onOpenStream(int*) const+0x21` when it lays out a character that the text's own font lacks and whose best fontconfig match is a PCF, BDF, PFR, Windows FNT, Type 42 or WOFF2 font that contains the character.

## Cause

Compose 1.8.2 uses Skiko 0.9.4.2, built with Skia m132. Its fontconfig font manager:
- accepts any readable file that fontconfig lists (`FontAccessible`);
- creates `SkTypeface_fontconfig` with `fProxy` from Skiko's FreeType without a null check;
- forwards `onOpenStream` and the other font-data calls to `fProxy`.

Skiko's FreeType has only the TrueType (with WOFF 1.0), CFF, Type 1 and CID drivers, so `fProxy` is null for the formats above, and the first forwarded call reads address 0.

Fontconfig lists such a font only from a cache written with the system FreeType, as `fc-cache` writes it: a cache that the app writes omits it.

## Fix

`apps/multiplatform/build.gradle.kts` resolves every `org.jetbrains.skiko` module requested at 0.9.4.2 to 0.9.24 (Skia m138), where:
- `FontAccessible` also requires that the font scanner loads the file;
- `SkTypeface_fontconfig::Make` returns null when it does not.

0.9.24 keeps the `SkiaLayerProperties` constructor that Compose 1.8.2 calls for every window; 0.9.37.3 removes it.

## Bubble baselines on Skia m138

Skia draws a line's glyphs at `floor(leading/2 - ascent + 0.5)` and reports its baseline as `round(descent - ascent + leading) - descent`. Desktop bubbles keep the two equal with whole-pixel line heights (`bubbleLineHeight`).

m138 adds the paragraph's empty metrics to every line. They are scaled by the rounded font height, so when they exceed the runs' metrics, `descent - ascent` is fractional and the drawn baseline is up to 1 px from the reported one.

`desktopDrawnBaselines` reports the drawn first and last baselines of a bubble text:

```mermaid
flowchart TD
  A[Text layout] --> B["line descent = line top + line height - reported baseline"]
  B --> C{"descent = empty descent of Inter at the style's size?"}
  C -->|yes| D[ascent = empty ascent]
  C -->|no| E[ascent = descent - line height]
  D --> F["drawn = line top + floor(leading/2 - ascent + 0.5)"]
  E --> F
  F --> G["FirstBaseline, LastBaseline += drawn - round(reported)"]
```

Texts:
- `MarkdownText`
- time, TTL and deleted timestamp in `CIMetaView`
- `HeaderText` and the quote sender in `FramedItemView`
- deleted, integrity error, marked deleted, decryption error, group invitation and file size texts

## Scope

- Desktop builds on all OSes use Skia m138; Android APKs are unchanged.
- `desktopDrawnBaselines` is the modifier itself on Android, and acts on desktop only under `LocalBubbleLayout` for Inter texts with a line height in sp.
- The rule applies only to 0.9.4.2, so a Compose update that brings another Skiko version is not overridden.
- Tests:
  - `FallbackFontTest` builds a fontconfig cache for one BDF font with `fc-cache`, then lays out U+2603 in a child JVM.
  - `BubbleLayoutTest.timeDrawnOnTextBaseline` checks the drawn rows of text and time at font scales 0.7 to 1.5.
- `Dockerfile.build` installs `fontconfig`, which provides `fc-cache` to the test in CI.
