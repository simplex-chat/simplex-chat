# Fix crash on opening a chat containing an extremely wide image

## Problem

Sending/receiving an image with an extreme aspect ratio (reproduced with a
**4000×1** image) makes the chat **unopenable**: every render of the chat throws

```
java.lang.IllegalArgumentException: Can't represent a width of 4660000 and height of 1165 in Constraints
    at androidx.compose.foundation.layout.AspectRatioNode.measure(AspectRatio.kt:117)
    ...
    at chat.simplex.common.views.chat.item.FramedItemViewKt$PriorityLayout$1$1.measure(FramedItemView.kt:482)
```

Because the exception fires during measurement on every frame, the chat cannot
be opened again without clearing it. Affects **Android and desktop** (the Compose
`apps/multiplatform` UI). iOS is unaffected (see below).

## Cause

The framed image preview Box sizes itself with `Modifier.aspectRatio(...)` driven
by the image's real proportions
(`apps/multiplatform/.../chat/item/CIImageView.kt`):

```kotlin
// before
Modifier.width(w).aspectRatio((previewBitmap.width.toFloat() / previewBitmap.height.toFloat()).coerceAtLeast(1f / 2.33f))
```

The ratio was clamped only on the **low** side (`coerceAtLeast(1f / 2.33f)`,
added in #6959 to stop very *tall* previews overflowing the caption). The **high**
side was left unbounded. For a 4000×1 image the ratio is `4000`.

During Compose's intrinsic-measurement pass, `AspectRatioNode` derives a fixed
`width = height × ratio`. Compose `Constraints` pack each dimension into at most
18 bits, so the maximum representable value is **262142 px**. With the observed
intrinsic height of 1165 px, `1165 × 4000 = 4,660,000` — ~18× over the limit —
and `Constraints.fixed(...)` throws. Any ratio above roughly `262142 / height`
(≈ 225 at this height) overflows; tall images never hit this because the existing
lower bound already caps their ratio.

The bitmap decoders (`Images.android.kt`, `Images.desktop.kt`) only made this
reachable: they reject pathologically *tall* images
(`outHeight > outWidth * 256`) and any dimension `> 4320`, but have **no
symmetric wide guard**, so a 4000×1 image (width 4000 ≤ 4320, not tall) decodes
and reaches the unbounded `aspectRatio`.

## Fix

An initial fix (already merged) added a symmetric upper bound to the ratio,
`coerceIn(1f / 2.33f, 2.33f)`. That stops the crash but reshapes every image
wider than 2.33:1 — including legitimate panoramas — to 2.33:1. This change
**supersedes** it: stop routing the box height through `aspectRatio` and compute
it **directly**, so the dangerous `width = height × ratio` derivation never
happens. The wide side is then left at its **natural ratio** (no upper clamp);
only the tall side is capped at `2.33`, exactly as before. This mirrors what the
iOS app already does (`height = w × heightRatio`, `heightRatio = min(h / w, 2.33)`):

```kotlin
// before (merged interim fix)
Modifier.width(w).aspectRatio((previewBitmap.width.toFloat() / previewBitmap.height.toFloat()).coerceIn(1f / 2.33f, 2.33f))
// after
Modifier.width(w).height(w * (previewBitmap.height.toFloat() / previewBitmap.width.toFloat()).coerceAtMost(2.33f))
```

With this form the box width is pinned (`≤ DEFAULT_MAX_IMAGE_WIDTH = 500.dp`) and
the height is `w × min(h / w, 2.33)`, which is always in `(0, w × 2.33]` — at most
`≈ 1165.dp`. Both dimensions are therefore always far inside the 262142 px
`Constraints` limit **at any aspect ratio and any screen density**, so the crash
is structurally impossible rather than merely bounded. A very wide image renders
at its true proportions (a thin strip) instead of being reshaped to 2.33:1.

This does not disturb the framed item's text-width adaptation: that uses
`.width(IntrinsicSize.Max)` (`FramedItemView.kt`), which reads the image box's
intrinsic **width** — still pinned to `w` — so only the wide side's height
derivation changes.

## Why compute height directly (vs clamping the ratio)

The earlier candidate fix clamped the ratio (`coerceIn(1f / 2.33f, 2.33f)`). That
is safe, but it reshapes every image wider than 2.33:1 — including legitimate
panoramas — to 2.33:1, because `ContentScale.FillWidth` cannot fill the
over-tall box and the image letterboxes. Computing the height directly removes
the overflow-prone code path entirely *and* preserves the natural shape of wide
images, so there is no display trade-off to accept. It also makes the Compose and
iOS image-sizing logic parallel.

## Scope / non-goals

- Only the `!smallView` framed Box derived its size from the image ratio; it now
  computes height directly. The chat-list `smallView` preview is locked to a fixed
  `36.sp` square, and all other image/video/link paths size with `.width(...)` +
  `ContentScale` (no `aspectRatio`), so none of them can hit this overflow.
- One follow-up was identified but intentionally left out to keep the diff
  minimal: a **symmetric wide guard** in the bitmap decoders
  (`outWidth > outHeight * 256`, mirroring the existing tall guard in
  `Images.android.kt` / `Images.desktop.kt`) for defense-in-depth, so any future
  consumer rendering a decoded bitmap is protected at the source.

## iOS

iOS is **not** affected by the crash, and the fix above brings the two platforms
into alignment. iOS already sizes the preview by computing the height directly —
`height = w × heightRatio`, `heightRatio = min(size.height / size.width, 2.33)`
(`apps/ios/SimpleXChat/ImageUtils.swift`) — which is exactly the form now used on
Android/desktop. (Even before, SwiftUI lays out with `CGFloat` frames and has no
`Constraints` packing limit, so a 4000×1 image yielded a valid sub-pixel-height
frame rather than throwing.) No iOS change is required.
