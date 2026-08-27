# Animated images on desktop

## The problem

`SimpleAndAnimatedImageView` on desktop drew a single `BitmapPainter` and carried the marker
`// LALAL make it animated too`. Android decodes animations with coil, iOS with SwiftyGif, and desktop showed
the first frame and stopped. `ImageFullScreenView` carried a matching marker over the image branch.

## Why this shape

**Skia's `Codec`, which skiko already puts on the desktop classpath.** No new dependency. It decodes both GIF
and animated WebP, reports per-frame durations and repeat counts, and supports random access into frames.

**Not `components-animatedimage`** (declared and unused until this change removed it). Its `animate()`
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
| bytes per pixel read from the codec | the file chooses its color type; the budget must not assume four bytes |
| file size checked before the bytes are copied natively | Skia copies the encoded bytes and scans them to count frames |
| magic-byte prefilter (`GIF8`, `RIFF....WEBP`) | photos are most of what a chat holds and none are animations; they never reach a second decoder |
| `allocPixels` result honoured | it reports failure by returning false, and reading into an unallocated bitmap throws |
| frame count bound | counting the frames also builds a table of them, which a file of minimal frames makes several times its own size, and the codec holds it for as long as the animation plays |
| rebuilt frame chain bound | a frame the codec is given no prior frame for is rebuilt from its whole chain, and Skia recurses to do it: frames alternating their disposal make that chain as long as the file likes, and 8000 frames of it overflows the native stack and kills the app, which no catch can prevent. Real animations rebuild nothing at all |
| destination allocated with a premultiplied alpha type | the codec reports the alpha type of the first frame, and a frame that has alpha cannot be read into an opaque bitmap |
| frame duration floor, and 100ms substituted for delays of 10ms and less | the frames a file is allowed can all declare no delay at all, and Skia reports the usual "as fast as possible" delay of one centisecond as 10ms |
| a frame is waited out for what it cost as well as what it asks for | one very expensive frame among cheap ones owes nothing once the cheap ones have paid the debt off, and held 96.7% of a decoder thread indefinitely; waiting out the cost leaves any animation about half of one |
| an animation that owes too much for its frames stops on the one it reached | frames that alternate expensive with cheap are never slow twice in a row, so a count that resets never stops them |
| every native call that reads the file is inside an exception boundary | the frame count, the frame table and the repeat count are read from it too |

Long frame delays are honoured rather than clamped - they are the author's, and they cost only the codec, the
raster and the frame table staying alive while nothing decodes.

## Cost, and the optimisations that were rejected

A frame continues the one before it, and the codec has to be told that the bitmap already holds it. Without
that it decodes the whole chain back to the last independent frame, so a frame costs as much as its index and
a loop costs the square of the frame count. Measured over one loop of the GIFs in `images/`, decode only:

| | frames | chain re-decoded | prior frame reused |
| --- | --- | --- | --- |
| files.gif | 196 | 5.93 ms/frame | 0.06 ms |
| connection.gif | 240 | 9.22 ms/frame | 0.09 ms |
| groups.gif | 309 | 9.10 ms/frame | 0.05 ms |
| user-addresses.gif | 1041 | 25.92 ms/frame, worst 77 ms | 0.04 ms |

Pixels are identical either way. The cost of a frame is then its own, and an animation stops on the frame it
reached once it owes too much: a frame over 100ms counts double what a frame under it forgives. This is wall
time, so a single frame can overrun by being descheduled, and a busy machine should not turn a cheap
animation into a still - but a file whose frames alternate expensive and cheap is never slow twice in a row,
and a run of them is what a count that resets would miss. Measured on a 1920x1920 GIF of 400 such frames, which holds
67% of a core indefinitely against a count that resets. Frames tuned to stay just under the threshold owe
nothing at all, and one expensive frame among cheap enough ones owes nothing for long, which is why a frame
is also waited out for what it cost: a frame of 3s among four cheap ones drops from 96.7% of a decoder thread
to 49.3%, every frame at 99ms from 83.0% to 49.9%, and the GIFs in `images/` stay exactly where they were -
none of their frames decodes in as long as it asks to be shown, by three orders of magnitude.

Two optimisations were measured and **rejected**. Both were measured before the prior frame was reused, so
their per-frame figures are against a decode that was two orders of magnitude more expensive; the conclusions
are kept because they are about ratios, but the numbers are worth taking again:

- **Decoding at display size.** Scaled decode is supported at arbitrary sizes, but it costs CPU rather than
  saving it: 2000x891 goes from 177.8ms to 300.3ms per frame (+68%) to save 59% of the raster — and it only
  engages on the files that are already the most expensive.
- **Half-depth pixels.** Skia refuses `RGB_565` and `ARGB_4444` for GIF outright. It works only for opaque
  WebP, at +11% decode for -50% raster, which does not justify a format-specific path.

What was kept: decoding is confined to two threads of the shared pool, so untrusted decode work cannot starve
the long running calls that share it; and frames are only decoded while they can be seen — not while the app
is minimized or sits in the tray, not while the image is behind the privacy blur, where each frame would otherwise be
decoded, uploaded and then blurred away again for nobody, and not while a full screen modal covers the
chat, which is shown beside it rather than in place of it: the viewer would otherwise leave the same
animation decoding twice, and the rest of the chat decoding where nobody can see it. The chat list preview stays a still image for
the same reason: it is a 36sp box that the desktop layout keeps on screen the whole time, so animating it
would hold a raster and spend a frame of work per listed chat, without pause.

## Verification

- 20 000 fuzzed mutations (bit flips, truncations, header corruption) over a real corpus plus crafted hostile
  files: no exception escapes the structure, no hangs.
- Frames advance, per-frame delays are read correctly, and the loop wraps back to frame 0 after a full cycle
  with byte-identical pixels.
- An oversized animation is refused by the bounds and still renders through the existing still-image path.
- A GIF of 8000 frames alternating their disposal, which passes every other bound at an 8x8 raster, crashed
  the process with SIGSEGV before the chain bound and is refused by it now, while the GIFs in `images/`, a
  1920x1920 animation and a GIF disposing to what came before it all still play.
- Every frame of the GIFs in `images/` decodes with the prior frame reused, with pixels identical to decoding
  the chain, and a GIF whose first frame is opaque and disposed to the background decodes past its first frame
  only into a premultiplied destination.
- Unit tests cover every bound as arithmetic - the raster, the frame count, the rebuilt chains, the frame
  durations and the debt an expensive frame owes; skiko's native library is not on the test
  runtime classpath, so decoding is measured with the library added to a standalone classpath.

## Deliberately not in this change

- **WebP still images do not decode on desktop at all.** Desktop decodes images with ImageIO, which has no
  WebP reader, so a received `.webp` never loads and picking one to send is dropped. Both the chat item and
  the full screen viewer reach this code only after that decode has succeeded, so until that separate fix
  lands it is GIFs that animate in the app, and the WebP path here is exercised by measurement only.
- **The decode raster is left to the collector.** Releasing it explicitly needs to know which thread Compose
  Desktop draws on, and skiko uses a different redrawer per platform; guessing risks a use-after-free.
