# Fix tall image preview overlapping caption text

Branch: `nd/fix-image-text-overlap` · commit `0a3dcd249` · analogous to iOS PR [#6732](https://github.com/simplex-chat/simplex-chat/pull/6732).

## 1. Problem statement

For a tall image (height/width ≳ 2.33), the chat-bubble caption text was rendered on top of the bottom of the image instead of cleanly below it. The image looked like it had a semi-transparent text watermark across its lower section. Reproduced on Android and desktop with any sufficiently tall image; never reproduced on images with `height ≤ 2.33 × width`.

iOS already had the analogous fix (PR #6732, commit `b24d003a8`); Android/desktop did not.

## 2. Solution summary

One line in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.kt`. The image preview Box's `Modifier.aspectRatio(width / height)` is wrapped with `.coerceAtLeast(1f / 2.33f)` so the aspect ratio cannot go below the floor at which the layout starts to break.

```diff
-          Modifier.width(w).aspectRatio(previewBitmap.width.toFloat() / previewBitmap.height.toFloat())
+          Modifier.width(w).aspectRatio((previewBitmap.width.toFloat() / previewBitmap.height.toFloat()).coerceAtLeast(1f / 2.33f))
```

Total diff: 1 file, +1 / −1.

## 3. Root cause

`PriorityLayout` (`FramedItemView.kt:480`, added in #6726) caps image-region height at `constraints.maxWidth × 2.33f` and passes that as `maxHeight` to its image child:

```kotlin
val maxImageHeight = (constraints.maxWidth * 2.33f).toInt().coerceAtMost(constraints.maxHeight)
val imageConstraints = constraints.copy(maxHeight = maxImageHeight)
val imagePlaceable = … .measure(imageConstraints)
```

The image child is `CIImageView`'s outer Box, modified (line 185 pre-fix) by `Modifier.width(w).aspectRatio(previewBitmap.width / previewBitmap.height)`. Compose's `aspectRatio` modifier picks a layout size satisfying both the parent's constraints AND the requested ratio — by trying `tryMaxWidth`, `tryMaxHeight`, `tryMinWidth`, `tryMinHeight` in order and returning the first satisfying `IntSize`.

When the natural ratio is below `1/2.33`, every candidate violates one bound:

- `tryMaxWidth`: implied `height = W / ratio > W × 2.33 = parentMaxHeight` → fails the height cap.
- `tryMaxHeight`: implied `width = parentMaxHeight × ratio < W` → fails the fixed `.width(W)` lower bound.
- `tryMinWidth` / `tryMinHeight`: same failures.

`findSize()` returns `IntSize.Zero`, and `aspectRatio` falls through to passing the parent's UNBOUNDED-height-shape constraints down (`wrappedConstraints = constraints` — see Compose Foundation's `AspectRatioNode`). The inner `Image` (`.width(W).contentScale=FillWidth`) then sizes itself by intrinsic aspect, with `height = W × imgH/imgW`, drawing past the layout box `clipToBounds` would have given it. The text below then renders on the same vertical strip as the painter's overflow — visible as overlap.

`PriorityLayout`'s height cap (added in #6726) prevents the original crash but does not, on its own, prevent this visual overflow, because the cap propagates through `imageConstraints` to a modifier that silently drops it.

## 4. The fix in detail

`coerceAtLeast(1f / 2.33f)` raises the ratio's floor to exactly the value where Compose's `tryMaxWidth` succeeds:

- At `ratio = 1/2.33`, implied `height = W / (1/2.33) = W × 2.33 ≤ parentMaxHeight` (since `W ≤ parentMaxWidth` and `parentMaxHeight = parentMaxWidth × 2.33`).
- `findSize` returns `(W, W × 2.33)`. `wrappedConstraints` becomes `Constraints.fixed(W, W × 2.33)` — both dimensions fixed.
- Inner `Image`'s `paint` modifier sees `hasFixedDimens=true` and returns the constraints unchanged. Image bounds = `(W, W × 2.33)`.
- `Image`'s built-in `clipToBounds` clips the painter's `FillWidth` overflow to the bounded layout. `PriorityLayout` reads `imagePlaceable.measuredHeight = W × 2.33` and places the caption text immediately below.

For images at or above the floor (ratio ≥ 1/2.33), `coerceAtLeast` is a no-op — the ratio is unchanged, the layout is unchanged, no behavioural diff. The change only takes effect for images that triggered the bug.

`coerceAtLeast` (not `maxOf`) matches the idiom already used at `FramedItemView.kt:480` (`.coerceAtMost(constraints.maxHeight)`) — both clamp at the bound where the layout starts to break. Reads as "ensure the ratio is at least 1/2.33".

## 5. Why this specific shape

- **Why preserve the existing expression untouched.** The pre-fix `previewBitmap.width.toFloat() / previewBitmap.height.toFloat()` is the aspect-ratio computation. Wrapping it `(...).coerceAtLeast(1f / 2.33f)` makes the diff purely additive — a reviewer reading the patch sees "the existing ratio is now floored", with zero risk that the inner expression silently changed. Diff is one line, character-minimal.

- **Why no `MAX_IMAGE_HEIGHT_RATIO` constant.** Two use sites of `2.33f` after the fix (`PriorityLayout` line 480 and the new clamp). `good-code-v5.md`: *"Three similar lines are better than a premature abstraction."* If a third site appears (link preview, video preview, etc.) the constant earns its place. Until then, local duplication mirrors the convention already in `PriorityLayout`.

- **Why no edit to `CIVideoView`.** The screenshot showed only the image-preview bug. iOS PR #6732 also touched `CIVideoView` and `CILinkView`, but on Android/desktop the video preview's outer Box has no `aspectRatio` modifier at all — it is sized by its inner `VideoPreviewImageView`'s `.width(width).FillWidth`, which is paint-clamped by `imageConstraints.maxHeight` and reports a correct layout size. If a tall-video overlap is actually reported in the wild, the fix is a separate commit; speculatively replicating the iOS scope would expand the diff and review surface beyond what the bug requires.

- **Why no `fillMaxSize + ContentScale.Crop` rewrite of the inner `Image`.** A previous iteration of this fix did that to mirror iOS's `scaledToFill` change. It works, but is structurally redundant: once the outer Box's `aspectRatio` is bounded, the inner `Image`'s existing `paint` modifier (`hasFixedDimens=true → return constraints`) and `clipToBounds` already produce the same visual result. The rewrite was extra structure, not bug-fix; reverted.

- **Why no `PriorityLayout` constant rename.** `2.33f` is already inline at line 480 and works as-is. Extracting it to `MAX_IMAGE_HEIGHT_RATIO` would be a rename bundled with a bugfix — `good-code-v5.md`: *"a rename in a diff signals a meaningful change to the reviewer — a gratuitous rename wastes reviewer attention and can mask real changes."* Out of scope.

- **Why `coerceAtLeast(1f / 2.33f)` and not `coerceAtLeast(0.43f)`.** The form `1f / 2.33f` makes the relationship to `PriorityLayout`'s `2.33f` height multiplier visually explicit. `0.43f` would be opaque and would drift independently if either value changed.

## 6. Verification

Manual sanity (Android debug APK):

- Send a tall screenshot (height ≫ width) with a caption → caption now sits below the cropped image preview, no overlap.
- Send an image where `height ≤ 2.33 × width` → preview unchanged from pre-fix (the clamp is a no-op for these).
- Tap the cropped preview → fullscreen viewer opens the full image at native aspect (the clamp only affects the inline preview, not the viewer).

## 7. Risk and rollback

- **Blast radius** is the single-Box modifier in `CIImageView` for non-`smallView` mode. `smallView` (used by `ChatPreviewView` thumbnails) takes the `else Modifier` branch and is untouched.
- The clamp is a no-op for images that did not trigger the bug, so regression risk on non-tall images is zero by construction.
- Rollback: `git revert 0a3dcd249` and force-push the branch (or just drop the commit before merge).
