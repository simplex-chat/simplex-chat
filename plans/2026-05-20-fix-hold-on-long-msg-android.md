# Fix chat item long-press menu and ripple shape

Branch: `nd/fix-hold-on-long-msg-android` · PR [#6997](https://github.com/simplex-chat/simplex-chat/pull/6997) · issue [#6991](https://github.com/simplex-chat/simplex-chat/issues/6991).

## 1. Problem statement

Two issues with the chat-item bubble on the multiplatform UI:

- **Android (#6991):** long-pressing the lower part of a very tall text message did not open the select/copy/reply context menu. Long-press on the top/middle worked. Reproduced with a long multi-line message (~150+ lines — e.g. 5000 random bytes as hex); never reproduced on short messages. Occurs **only with the message tail enabled** (bubble shape); with the tail preference disabled, messages use a plain rounded-rectangle shape and the bug does not reproduce. iOS unaffected.
- **Desktop:** the chat-item press ripple, in some cases, rendered as a rectangle instead of following the rounded bubble shape.

## 2. Solution summary

One function — `Modifier.clipChatItem` in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt`. It clipped the chat item with `Modifier.clip(shape)` for every shape style. It now clips the **bubble** (`GenericShape`) in the draw pass with `drawWithCache` + `clipPath`, and keeps `Modifier.clip` for the **`RoundRect`** shape, which is unaffected by the bug (§3).

```kotlin
return when (style) {
  is ShapeStyle.Bubble -> {
    val shape = chatItemShape(cornerRoundness, LocalDensity.current, style.tailVisible, chatItem?.chatDir?.sent == true)
    this.drawWithCache {
      val path = Path().apply { addOutline(shape.createOutline(size, layoutDirection, this@drawWithCache)) }
      onDrawWithContent { clipPath(path) { this@onDrawWithContent.drawContent() } }
    }
  }
  is ShapeStyle.RoundRect -> this.clip(RoundedCornerShape(style.radius * cornerRoundness))
}
```

Net diff: 1 file (`ChatItemView.kt`), +20 / −5 — the `clipChatItem` function restructured plus two imports.

## 3. Root cause

`Modifier.clip(shape)` is defined in Compose as `graphicsLayer(shape = shape, clip = true)`. A clipping graphics layer restricts **both** drawing **and** pointer hit-test to the shape.

`clipChatItem` is the first (outermost) modifier on the chat-bubble `Column`, and that same `Column` carries the `combinedClickable` long-press handler. So the layer's hit-test region gates every press on the bubble.

- **Android:** for a very tall chat item the layer's hit-test region does not cover the bubble's lower portion — a press there is never delivered to `combinedClickable`, so the long-press menu does not open. This is specific to the bubble's `GenericShape` clip: with the tail disabled the item is clipped with a `RoundedCornerShape`, which hit-tests correctly. The exact reason the `GenericShape` clip's hit-test falls short on tall content was not isolated; the fix does not depend on it (see §4).
- **Desktop:** the layer's clip did not always extend to the `combinedClickable` press ripple, so the ripple drew to its own rectangular bounds instead of the bubble shape.

## 4. The fix

For the bubble shape, `clipChatItem` clips with a draw modifier instead of a graphics layer. `drawWithCache` builds the shape's `Path` once per size change; `onDrawWithContent { clipPath(path) { drawContent() } }` wraps the whole content draw — bubble background, text, and the press ripple — in a canvas clip.

A draw modifier affects **only drawing**. It is not a layout or pointer-input node and has no effect on hit-test. Therefore:

- the bubble and ripple are still clipped to the shape — visually identical to `Modifier.clip`;
- pointer hit-test is no longer clipped — `combinedClickable` receives presses anywhere in the `Column`'s bounds, fixing the Android long-press;
- the canvas `clipPath` clips the ripple reliably, fixing the rectangular desktop ripple.

The `RoundRect` shape keeps `Modifier.clip`: it hit-tests correctly (no bug) and keeps its antialiased outline clip. Scoping by shape — rather than draw-clipping every shape — leaves every non-bubble chat item (service/event messages, tails-off messages, old Android) byte-for-byte unchanged.

## 5. Alternatives rejected

- **Remove `clipChatItem` from the bubble `Column`.** Fixes the Android long-press, but the press ripple loses its shape and renders as a rectangle. Intermediate state during development; replaced.
- **Draw-pass clip for every shape, unconditionally.** Also correct and a hair simpler (no `when`), but it needlessly moves the `RoundRect` shape off `Modifier.clip`'s antialiased outline clip onto a canvas `clipPath` — a behaviour change with no benefit, since `RoundRect` has no bug. Scoping to the bubble shape keeps `RoundRect` unchanged.
- **Keep `Modifier.clip`, move `combinedClickable` off the clipped `Column`.** A larger structural change to the chat-item layout tree; the draw-pass clip fixes both issues without moving anything.

## 6. Verification

- **Android** (debug APK): long-press on the lower half of a 150+-line message opens the context menu; top/middle still work; the tap ripple stays bubble-shaped; swipe-to-reply and link tap/long-press are unaffected.
- **Desktop** (Linux AppImage): the chat-item press ripple follows the bubble shape (rounded corners and tail), not a rectangle — confirmed against a build without the fix.
- The bubble draw-pass clip above was verified on those Android and desktop builds; this revision additionally keeps `Modifier.clip` for the `RoundRect` shape, which is the unchanged pre-fix behaviour.

## 7. Risk and rollback

- Blast radius: the `Bubble` branch of `clipChatItem`. The `RoundRect` branch is unchanged (`Modifier.clip` as before), so service/event items, tails-off messages and old-Android items are untouched. For the bubble, drawing is clipped identically; the single behavioural change is that pointer hit-test on the bubble is no longer shape-clipped — benign (bubble corners are transparent; a rectangular hit area is a marginally larger touch target).
- iOS is a separate codebase and is untouched.
- Rollback: revert the fix commit on the branch, or drop it before merge.
