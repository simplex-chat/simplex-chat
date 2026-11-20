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

## Text layout on Skia m138

Changes in m138 and in its fonts, and what restores the m132 result:

| Change | Effect | Restored by |
|---|---|---|
| Every line also takes the empty metrics of the paragraph's default style (`breakShapedTextIntoLines`, `TextWrapper::breakTextIntoLines`) | lines with fallback fonts, smaller spans or a line height are laid out taller or shifted | a zero-length span at offset 0 with a font size whose rounded height is 1 px |
| HarfBuzz applies the `trak` table (macOS: PingFang, the system font) | wider glyphs | letter spacing minus the tracking, on characters set in such a font |
| Segoe UI Emoji is drawn from its COLR v1 table (Windows) | emoji in text have gradients | a span with a copy of the font whose COLR version is 0 |
| FreeType 702e4a1d (Linux) | other pixels in `ñ`, `ç`, `Ц`, `Щ` and dots | none |

Compose merges every span that starts at offset 0 into the paragraph's default style (`ParagraphBuilder.makeOps`), and Skia takes the empty metrics from that style. A zero-length span at offset 0 ends at offset 0, so spans that start there merge into the style its end creates instead.

With a line height and Center alignment (Material's default style, trim None), Skia sizes the empty metrics to the whole line height around the default font's own offset, so a minimal default font shifts them instead of shrinking them. The zero-length span then takes the text's dominant font (the font of most characters) at the size where its height `I_d = ceil(I) - 0.49` rounds up; the empty metrics then lie inside that font's run box. A dominant font with a line gap is replaced by a copy whose `hhea` and `OS/2` line gaps are 0. Top and Bottom alignments take `H'`.

```mermaid
flowchart TD
  T["text, style"] --> E{"empty line?"}
  E -- no --> A{"line height and alignment"}
  A -- "none, or Proportional" --> M["zero-length span at 0: font size with rounded height 1 px"]
  A -- "Center, trim None" --> D["zero-length span at 0: dominant font, size with I rounded up"]
  A -- "Top, Bottom" --> C
  E -- yes --> C["line height H' for the style merged with the spans at 0"]
  D --> F
  M --> F["fallback spans: tracking (macOS), COLR v0 emoji (Windows)"]
  C --> F
  F --> B["BasicText / BasicTextField"]
```

`H'` (empty lines take the empty metrics, so a minimal default would collapse them):

| Line height style | `R >= I` | `R < I` |
|---|---|---|
| Proportional | `H` | `(H - leading) * R / (descent - ascent)` |
| Top, Center, Bottom, custom | `H` | `H - I + R` |

- `R = round(descent - ascent + leading)`, `I` the exact value.
- A result for `H` above the font size `F` stays at least `1.0001 * F`, and for `H` at or below `F` at most `F`: Compose lays out a line height at or below the font size with `HeightMode.DISABLE_ALL`.
- An empty text keeps `H`: Compose lays it out from its own metrics.
- A line height at or below the font size with a trim other than `Both` takes `H'` instead of the span.

`Text` in `chat.simplex.common.platform` applies the transform with the parameters of `androidx.compose.material.Text`; every file that calls `Text` imports it. `ClickableText`, `SelectableText` and the text fields (`platformTextFieldStyle`, through a visual transformation) apply it too.

## Scope

- Desktop builds on all OSes use Skia m138; Android APKs are unchanged, and `Text` calls the Material one there.
- Fonts with known metrics: Inter, the bundled emoji font, and the fonts Compose resolves for `FontFamily.Monospace` and `FontFamily.Default`. Other fonts take only the fallback spans.
- An empty text with Center alignment, outside text fields, takes Proportional alignment: m132 computed empty-paragraph metrics with the strut's proportional ratio.
- An empty line is a segment between Skia's hard breaks (LF, VT, FF, U+2028, U+2029) that is empty or a single CR. Text fields skip the check.
- Text fields take no zero-length span with a font size: Compose sizes the caret and the trimmed first and last lines from the paragraph's default font size (`SkiaParagraph.getCursorRect`). A field with a line height takes `H'`.
- The font of a grapheme (Skia's ICU character breaks) mirrors Skia's `OneLineShaper`; a font covers a grapheme when it covers each of its characters except controls and default ignorables:
  - the first installed font of the text's family that covers the grapheme (`FontFamily.Default`, `SansSerif` and `Monospace` resolve to Compose's alias lists, `GenericFontFamiliesMapping`);
  - otherwise the first fallback font already found in the same segment that covers it;
  - otherwise the fallback for its first character that covers it: an emoji sequence start (Unicode 15.1 Emoji and Emoji_Component, as ICU 74 in Skia m138) takes the emoji fallback (`und-Zsye`, Apple Color Emoji on macOS), any other character the system fallback for its weight and slant;
  - when the system has none, Inter or the bundled emoji font, as Skiko's `TypefaceFontProviderWithFallback::fallbackForChar`.
- Fonts are resolved once per text, for the dominant font and the fallback spans.
- An emoji run (Windows) continues over ZWJ, variation selectors, keycap, tags, skin tones, hair components and the second regional indicator of a pair; a segment in a font with unknown metrics ends it.
- The COLR v0 copy is made only when the font's COLR table has another version and v0 base glyph records.
- Segments are found by one sweep over span bounds; active spans merge in the order of their starts, then of their indexes, as in `ParagraphBuilder.makeOps`.
- Text fields keep the original visual transformation on Linux, where the transform adds no span, and for passwords, which Compose recognises by the `PasswordVisualTransformation` type (context menu, semantics).
- `Text(String)` keeps Compose's String path when the transform adds no span.
- Font copies are kept as native typefaces only.
- The dominant font counts each segment in its own family and skips emoji continuation characters.
- A text without a font family is measured with the typeface Skia finds for `sans-serif`; the default font size is 16 sp, as in Compose's `ParagraphBuilder`. Metrics take the first installed font of a family.
- A copy without line gap is made only for fonts up to 8 MB; larger fonts take `H'`.
- Font sizes and line heights in em resolve against 16 sp and the merged size; the app sets none in em.
- Remaining differences:
  - a blank line in a text with a line height, by the rounding of `R`;
  - Center alignment with a line height: a line without the dominant font, by the distance between the centres of the two fonts (0.35 px for Inter and Noto Color Emoji);
  - texts with a blank line, and text fields: a line set entirely in a fallback font, by up to the difference of its height from the default font's;
  - Proportional alignment with trim Both: Compose's first-line ascent and last-line descent (`getLineTop`, `getLineBottom`, cursor rects) are computed from the marker's font size; the drawing is unchanged;
  - macOS: glyphs of fonts with a `trak` table, half the tracking (0.17 px at 16 px) to the right;
  - Windows: emoji in bold text, by DirectWrite's bold simulation of the system emoji font; ❤️ in text set in the system font;
  - Linux glyph pixels.
- The rule applies only to 0.9.4.2, so a Compose update that brings another Skiko version is not overridden.
- Tests:
  - `FallbackFontTest` builds a fontconfig cache for one BDF font with `fc-cache`, then lays out U+2603 in a child JVM.
  - `BubbleLayoutTest.timeDrawnOnTextBaseline` checks the drawn rows of text and time at font scales 0.7 to 1.5.
  - `BubbleLayoutTest.linesDrawnWholeLineHeightApart` checks that two lines are drawn the whole-pixel line height apart at the same scales.
  - The desktop snapshots are compared with the references rendered with m132.
- `Dockerfile.build` installs `fontconfig`, which provides `fc-cache` to the test in CI.
