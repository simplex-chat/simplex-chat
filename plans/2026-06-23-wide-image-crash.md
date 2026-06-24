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

Add the symmetric **upper** bound to the clamp, mirroring the existing lower
bound and the tall-image height cap already enforced in `PriorityLayout`
(`FramedItemView.kt`: `maxImageHeight = constraints.maxWidth * 2.33f`):

```kotlin
// after
Modifier.width(w).aspectRatio((previewBitmap.width.toFloat() / previewBitmap.height.toFloat()).coerceIn(1f / 2.33f, 2.33f))
```

This is the minimal one-line change. With the cap, the box width is pinned
(`≤ DEFAULT_MAX_IMAGE_WIDTH = 500.dp`) and the height-driven width becomes
`height × 2.33 ≈ 2714 px` — three orders of magnitude inside the 262142 limit —
so the measurement can never overflow at any screen density. `2.33` is the
project's single "most-extreme allowed image proportion" constant: an image is
now clamped to at most 2.33:1 in **either** direction, the same rule already
applied to tall images.

## Why 2.33 (and not a larger cap)

`2.33` is provably safe by construction because it ties the wide bound to the
same proportion the layout already guarantees for height, independent of density.
A larger cap (e.g. 50–200) would preserve the natural shape of genuine panoramas
but relies on an assumption about the maximum measured height and loses the
symmetry with the tall-image rule. The trade-off accepted here is that wide
images between 2.33:1 and the crash threshold now display at 2.33:1 (the very
wide remainder shown as a thin strip with `ContentScale.FillWidth`) rather than
at their natural ratio — a cosmetic change in exchange for a guaranteed-safe,
consistent fix.

## Scope / non-goals

- Only the `!smallView` framed Box uses a media-derived `aspectRatio`; it is now
  clamped. The chat-list `smallView` preview is locked to a fixed `36.sp` square,
  and all other image/video/link paths size with `.width(...)` + `ContentScale`
  (no `aspectRatio`), so none of them can hit this overflow.
- Two follow-ups were identified but intentionally left out to keep the diff
  minimal: (1) a **symmetric wide guard** in the bitmap decoders
  (`outWidth > outHeight * 256`) for defense-in-depth across all consumers, and
  (2) extracting the duplicated `2.33` literal (now in `CIImageView.kt` and
  `FramedItemView.kt`) into a shared `MAX_IMAGE_ASPECT_RATIO` constant.

## iOS

iOS is **not** affected by the crash. It carries the same lopsided logic —
`heightRatio` (`apps/ios/SimpleXChat/ImageUtils.swift`) caps only the tall side
(`min(size.height / size.width, 2.33)`) — but SwiftUI lays out with `CGFloat`
frames and has no `Constraints` packing limit, so a 4000×1 image yields a valid
(sub-pixel height) frame instead of throwing. No iOS change is required for the
crash; bounding the wide side there would only be a cosmetic parity tweak.
