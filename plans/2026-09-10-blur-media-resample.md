# Blur media by resampling the preview

## Problem

"Blur media" is a per-frame effect. Every frame a blurred image or video preview is drawn, a
Gaussian is convolved over an offscreen layer the size of the drawn media - about 500.dp wide, so
roughly 1500x1125 px and a 6 MB render target on a 3x-density phone, for each blurred item on
screen.

It also does nothing on Android 8.0 to 11: the setting reads Soft, Medium or Strong and the media
is drawn sharp.

And the media the blur hides is still fetched in full. An image scrolled past is read from disk,
decoded at its original resolution and kept in the image cache, only ever to be drawn as an
unrecognisable smear.

## Cause

`Modifier.blur` compiles to `graphicsLayer { renderEffect = BlurEffect(...) }`. In
`androidx.compose.ui:ui-android:1.8.2`, which is what this build resolves:

| layer implementation | API range | what `setRenderEffect` does           |
| -------------------- | --------- | ------------------------------------- |
| `GraphicsLayerV23`   | 23-28     | stores the field, never applies it     |
| `GraphicsLayerV29`   | 29+       | applies only when `SDK_INT >= 31`      |
| `GraphicsViewLayer`  | fallback  | applies only when `SDK_INT >= 31`      |

`minSdk` is 26, so on Android 8.0 to 11 the effect is silently dropped and the media is drawn
unblurred. The codebase already knows this - `SimpleXAPI.kt` has
`deviceSupportsBlur = ... androidApiLevel >= 32` - but that gates the app bar blur and the bar
alpha, never the media blur.

The radius is in *display* pixels: `BlurKt` converts it with `toPx` on the `GraphicsLayerScope`
before constructing the `BlurEffect`. The convolution therefore runs over the layer, not over the
source bitmap - its cost does not depend on the source resolution, and it is paid again on every
frame the layer is drawn.

The loading is independent of the blur. `CIImageView` calls `getLoadedImage`, which reads the file
bytes and decodes a 1000 px bitmap, and then hands the same bytes to Coil at `Size.ORIGINAL`.
`imageLoader` overrides neither `memoryCache` nor `memoryCachePolicy`, so Coil 2.6.0 installs its
default memory cache with strong references and retains those full-resolution bitmaps across items.

## Fix

A blur and a downscale discard the same thing: detail finer than their radius. So the preview is
resampled to about one pixel per radius and drawn stretched back to the item, with the bilinear
filtering `drawImage` uses by default. This runs once when the item composes rather than once per
frame, needs no `RenderEffect`, and so works on every supported Android version.

`BLURRED_MEDIA_WIDTH_DP` fixes the strength: 360 means "about one pixel per radius across a
360dp-wide view", so Soft, Medium and Strong resample to roughly 30, 15 and 7 px wide. It is the
one number to move if the result reads too soft or too sharp.

Resampling is also the better direction for a privacy control. A Gaussian is a convolution and is
in principle partly invertible; a 7 px bitmap does not contain the detail to recover.

While the blur hides the media, the file is left unread - no disk read, no 1000 px decode, no
full-resolution decode, no cache entry. `blurHidesMedia()` is the single definition that both the
modifier drawing the blur and the decision to load the file consult, so the two cannot disagree
about whether the media is on screen. Getting that wrong in either direction is a defect: one way
the app shows what it promised to hide, the other it hides an image it will never load.

## Bounds

The media dimensions come from the sender, so the descent is bounded rather than trusted. Its
first step caps both sides at 512 px and the output is capped at 360 px, so no image, whatever its
shape, produces a large intermediate. That first step samples rather than averages - reading every
pixel of a 4K video frame would stall composition - and the halving that follows averages away
most of what it aliases.

The arithmetic was fuzzed over 2527 dimension and radius combinations: no crash, no
non-termination, every dimension at least 1, largest single intermediate 1.00 MB, largest total
churn 1.49 MB. A zero radius cannot reach the function; a negative one - possible from imported
settings - yields a 1x1 image, so it fails toward more blur rather than less.

## Verification

Built as a desktop AppImage from this branch and run against a profile with images and videos.
Images and video previews render as a smooth blur with no visible blockiness, and the play button
stays suppressed until the media is revealed. The shipped class was checked to contain the new path
and no remaining reference to `BlurKt` or `BlurredEdgeTreatment`.

No before/after CPU figure is included. The only machine available had no GPU, so Skiko fell back
to software rasterisation; a comparison measured there would overstate the gain, because on real
hardware the old blur was largely GPU work. The claim this change rests on is structural - a
constant recomputed every frame is now computed once - not measured.

The Android 8.0 to 11 no-op is read from the bytecode of the resolved artifact and has not been
confirmed on a device of that vintage.

## Out of scope

iOS is unchanged. It has the same per-frame `.blur(radius:)` in `PrivacyBlur`, and `getLoadedImage`
there does no downsampling at all, so the same change applies - but as its own diff.

`ChatInfoImage.kt` still blurs blocked members' avatars with `Modifier.blur`, so that one remains a
no-op below API 31. It is a different feature and a different call site.

`CIVideoView` still extracts a full-resolution frame through the video player while the preview is
blurred, the way `CIImageView` used to. The same treatment applies; the player's lifecycle makes it
a larger change than this one.
