# Animated images on desktop

## The problem

`SimpleAndAnimatedImageView` on desktop drew a single `BitmapPainter` and carried the marker
`// LALAL make it animated too`. Android decodes animations with coil, iOS with SwiftyGif, and desktop showed
the first frame and stopped. `ImageFullScreenView` carried a matching marker over the image branch.

## Why this shape

**Skia's `Codec`, which skiko already puts on the desktop classpath.** No new dependency. It decodes both GIF
and animated WebP, reports per-frame durations and repeat counts, and supports random access into frames.

**Not `components-animatedimage`** (already a declared dependency, unused). Its `animate()`
ignores the result of `allocPixels` and decodes inside composition. A 35-byte GIF declaring 65535x65535 asks
for a 17GB raster; `allocPixels` returns false, and the following `readPixels` throws
`IllegalArgumentException` from inside the composition — a remote crash from anyone who can send a file. It
also decodes on the UI thread, measured at ~11ms per frame for a 1244x554 animation.

## Bounds

Everything below is decoded from bytes somebody else composed, so each bound answers a specific crafted
input, and anything outside them keeps showing the still image the chat already renders. Animation degrades
to a picture, never to an error, and failures are never alerted — an alert per malformed file would itself
let a sender disrupt the app.

| Bound | What it answers |
| --- | --- |
| raster measured in bytes, sides multiplied as `Long` | `65535 * 65535` overflows `Int` to a negative number and would pass a naive budget check |
| per-side cap, independent of the raster bound | 65535x32 is only 2.1MP and would otherwise animate with a 65535-pixel scanline |
| bytes per pixel read from the codec | the file chooses its colour type; the budget must not assume four bytes |
| file size checked before the bytes are copied natively | Skia copies the encoded bytes and scans them to count frames |
| magic-byte prefilter (`GIF8`, `RIFF....WEBP`) | photos are most of what a chat holds and none are animations; they never reach a second decoder |
| `allocPixels` result honoured | it reports failure by returning false, and reading into an unallocated bitmap throws |
| frame duration floor, and 100ms substituted for "no delay" | a 4.6MB GIF can hold 200 000 zero-delay frames |
| single exception boundary around every native call | the frame count and repeat count are read from the file too |

Long frame delays are honoured rather than clamped — they are the author's, and they cost nothing.

## Cost, and the optimisations that were rejected

Measured on a 1244x554 GIF and its WebP equivalent:

| | work/frame | CPU while playing |
| --- | --- | --- |
| typical GIF | 2.80 ms | 1.7% of one core |
| animated WebP | 4.19 ms | 2.5% of one core |
| 2000x891 GIF | 180.6 ms | 100% of one core |

Typical animations are close to free; the whole cost problem is the pathological tail. So an animation whose
frames take more than 100ms to decode, twice in a row, stops and keeps the still. Two in a row because this
is wall time: a single frame can overrun by being descheduled, and a busy machine should not turn a cheap
animation into a still.

Two optimisations were measured and **rejected**:

- **Decoding at display size.** Scaled decode is supported at arbitrary sizes, but it costs CPU rather than
  saving it: 2000x891 goes from 177.8ms to 300.3ms per frame (+68%) to save 59% of the raster — and it only
  engages on the files that are already the most expensive.
- **Half-depth pixels.** Skia refuses `RGB_565` and `ARGB_4444` for GIF outright. It works only for opaque
  WebP, at +11% decode for -50% raster, which does not justify a format-specific path.

What was kept: decoding is confined to two threads of the shared pool, so untrusted decode work cannot starve
the coroutines that deliver messages; and frames are only decoded while they can be seen — not while the app
sits in the tray, and not while the image is behind the privacy blur, where each frame would otherwise be
decoded, uploaded and then blurred away again for nobody. The chat list preview stays a still image for
the same reason: it is a 36dp box that the desktop layout keeps on screen the whole time, so animating it
would hold a raster and spend a frame of work per listed chat, without pause.

## Verification

- 20 000 fuzzed mutations (bit flips, truncations, header corruption) over a real corpus plus crafted hostile
  files: no exception escapes the structure, no hangs.
- Frames advance, per-frame delays are read correctly, and the loop wraps back to frame 0 after a full cycle
  with byte-identical pixels.
- An oversized animation is refused by the bounds and still renders through the existing still-image path.
- Unit tests cover the bounds as arithmetic; skiko's native library is not on the test runtime classpath.

## Deliberately not in this change

- **WebP still images do not decode on desktop at all.** `getLoadedImage` uses ImageIO, which has no WebP
  reader, so a received `.webp` never reaches this code and picking one to send is dropped. Separate fix.
- **The decode raster is left to the collector.** Releasing it explicitly needs to know which thread Compose
  Desktop draws on, and skiko uses a different redrawer per platform; guessing risks a use-after-free.
