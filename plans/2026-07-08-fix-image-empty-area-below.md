# Fix empty area below wide images (v7.0.0-beta.3 regression)

## Problem

Since **v7.0.0-beta.3**, wide/landscape images in the chat render with an
**empty strip below** them. The image is drawn correctly at the top of its box
but the box reserves more vertical space than the image occupies, leaving a gap.
Portrait/tall images are unaffected — hence "some images."

Affects **Android and desktop** (the Compose `apps/multiplatform` UI). iOS is not
affected.

## Cause

The regression is commit `2c2337b07` (#7125) — the only image-sizing change
between beta.2 and beta.3. It replaced the framed image preview Box's
`aspectRatio` modifier with a directly-computed fixed height
(`apps/multiplatform/.../chat/item/CIImageView.kt`):

```kotlin
// beta.2 (interim crash fix)
Modifier.width(w).aspectRatio((previewBitmap.width / previewBitmap.height).coerceIn(1f / 2.33f, 2.33f))
// beta.3 (#7125)
Modifier.width(w).height(w * (previewBitmap.height / previewBitmap.width).coerceAtMost(2.33f))
```

The box width is:

```kotlin
val w = if (previewBitmap.width * 0.97 <= previewBitmap.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH
```

- **Tall/portrait** images use `w = imageViewFullWidth() * 0.75f`, and
  `imageViewFullWidth() = min(DEFAULT_MAX_IMAGE_WIDTH, window − 100.dp)` — so `w`
  is always ≤ the available width. The box never has to shrink. No gap.
- **Wide/landscape** images use the **fixed** `w = DEFAULT_MAX_IMAGE_WIDTH = 500.dp`,
  which is *not* reduced by the window width.

On any screen or bubble narrower than ~500dp (all phones, many desktop windows),
`Modifier.width(500.dp)` is **clamped down** to the available width by Compose's
`SizeModifier` (it constrains the requested size into the incoming constraints;
`PriorityLayout` in `FramedItemView.kt` measures the image with `maxWidth` = the
bubble's available width). But `.height(w * ratio)` was computed from the
**un-clamped** `w = 500`, so it does **not** shrink. The inner image
(`ContentScale.FillWidth`, top-aligned via `Alignment.TopEnd`) fills the clamped
width and is therefore shorter than the too-tall box:

```
box height   = 500 * ratio            (from unclamped w)
image height = clampedWidth * ratio   (FillWidth to the real width)
empty strip  = (500 − clampedWidth) * ratio
```

**Why it's a regression and not present before:** the old `.width(w).aspectRatio(r)`
self-corrected. When the box width was clamped to the available width,
`aspectRatio` recomputed the height proportionally, so the box always matched the
image. Switching to a fixed `.height()` (computed once from the nominal `w`) broke
that coupling.

They could not simply revert to `.aspectRatio()`: it reintroduces the original
#7123 crash. `FramedItemView`'s `Modifier.width(IntrinsicSize.Max)` triggers
`aspectRatio`'s `maxIntrinsicWidth = height × ratio`, which overflows Compose's
18-bit `Constraints` packing for extreme ratios (`4000×1` → `1165 × 4000` ≫ 262142).

## Fix

Keep `Modifier.width(w)` (this is what preserves the fixed intrinsic **width** `w`
that both avoids the crash and drives `FramedItemView`'s text-width adaptation),
but derive the box **height** from the *actual, already-clamped* width via a small
`Modifier.layout {}` — restoring the self-correcting behaviour without `aspectRatio`
(`CIImageView.kt`):

```kotlin
val w = if (previewBitmap.width * 0.97 <= previewBitmap.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH
Modifier.width(w).layout { measurable, constraints ->
  val width = constraints.maxWidth.coerceAtMost(w.roundToPx().coerceAtLeast(0))
  val height = (width * (previewBitmap.height.toFloat() / previewBitmap.width.toFloat()).coerceAtMost(2.33f)).roundToInt().coerceAtMost(constraints.maxHeight)
  val placeable = measurable.measure(Constraints.fixed(width, height))
  layout(width, height) { placeable.place(0, 0) }
}
```

New imports: `androidx.compose.ui.layout.layout`, `androidx.compose.ui.unit.Constraints`,
`kotlin.math.roundToInt`. The change is deliberately surgical: the only semantic difference
from #7125 is that the height multiplies the **measured** `width` instead of the nominal `w`.
`roundToInt` (not truncation) matches the px rounding `.height(Dp)` did in #7125.

### Why `coerceAtMost(w)` and `coerceAtLeast(0)` are load-bearing (crash-safety)

`width = constraints.maxWidth.coerceAtMost(w.roundToPx().coerceAtLeast(0))` bounds the
width at `w` in every case, which is what keeps `Constraints.fixed(width, height)` inside
Compose's 18-bit (262142 px) packing limit and prevents the #7123 overflow from recurring:

- **Real measure pass:** `width(w)` (a fixed-size `SizeNode`) already hands the block a
  bounded `maxWidth ≤ w` — the screen-clamped width. `coerceAtMost(w)` is then a no-op and
  `width` = that clamped width, exactly what removes the strip.
- **Unbounded intrinsic pass:** a plain `Modifier.layout {}` re-runs its lambda for intrinsic
  queries, and `constraints.maxWidth` there is `Constraints.Infinity` (`Int.MAX_VALUE`). A
  bare `constraints.maxWidth` would feed `Infinity` into `height = width × ratio` →
  `Constraints.fixed` throws (the exact earlier-observed regression). `coerceAtMost(w)`
  collapses `Infinity` back to `w`, so `height ≤ w × 2.33` — always finite. (In this tree the
  outer `width(w)` masks intrinsics so the lambda is never actually invoked unbounded, but the
  clamp makes the block correct regardless.)
- **`coerceAtLeast(0)`** mirrors what `Modifier.width()`'s `SizeNode` does internally: when the
  desktop window is narrower than the 100dp padding in `imageViewFullWidth()` (or 0 at window
  init), `w` is negative, and `Constraints.fixed` requires non-negative — so without the clamp
  a portrait image in a tiny window would crash where `.width(w)` silently rendered 0-width.

`width` and `height` are therefore always in `[0, w × 2.33]`.

### Why this is correct at every aspect ratio

- **Fits within `DEFAULT_MAX_IMAGE_WIDTH`** (wide window, or any tall image): the
  measured width equals the nominal `w`, so `height = w × heightRatio` — **pixel-identical
  to beta.3**. No visible change for the cases that already looked right.
- **Wider than the available width** (wide image on a phone/narrow window):
  `width = constraints.maxWidth` is the clamped width, `height = clampedWidth × heightRatio`
  — matches the `FillWidth` image exactly. **Empty strip gone.**
- **Tall images** (`h / w > 2.33`): `heightRatio` is capped at `2.33`, so the box is
  `width × 2.33·width`; the `FillWidth` image is taller and is cropped by the box
  (top-aligned) — same crop cap as before. `coerceAtMost(constraints.maxHeight)` keeps
  the `PriorityLayout` height ceiling honoured.
- **No crash:** the intrinsic width is still `w` (from `Modifier.width(w)`, unaffected
  by the inner `layout`), so `FramedItemView`'s `IntrinsicSize.Max` never multiplies by a
  ratio. The `width = height × ratio` derivation that overflowed `Constraints` is gone.

Both dimensions are always finite: `width(w)` gives the inner `layout` a bounded
`maxWidth`, and `height ≤ width × 2.33 ≤ maxHeight`.

## Scope / non-goals

- Only the `!smallView` framed image Box is changed. The chat-list `smallView`
  preview is a fixed square, and all other image/video/link paths size with
  `.width(...)` + `ContentScale` (no ratio-derived box), so none of them show the
  gap or the crash.
- Same file is `commonMain`, so the one change covers **Android and desktop**.
- Not touched: the bitmap decoders' guards (`Images.android.kt` / `Images.desktop.kt`)
  and the still-open "symmetric wide guard" defense-in-depth follow-up noted in
  `plans/2026-06-23-wide-image-crash.md` — out of scope for this display fix.

## iOS

iOS already computes the height directly from the laid-out width
(`height = w × heightRatio`, `heightRatio = min(h / w, 2.33)`,
`apps/ios/SimpleXChat/ImageUtils.swift`) and lays out with `CGFloat` frames, so it
never had the clamped-width gap. No iOS change required; this brings Android/desktop
back into line.

## Verification

- Build Android arm64 debug APK (`bash ~/build/android.sh`) and Linux x86_64
  AppImage (`bash ~/build/linux.sh`) from branch `nd/fix-image-width`.
- Manual: send/view a landscape image (~3:1 and ~4:3) on a phone / narrow desktop
  window — the empty strip below should be gone and the image should look as it did
  before beta.3. Confirm a very wide panorama still renders as a natural thin strip
  (no letterbox, no crash) and a very tall image is still cropped at 2.33.
