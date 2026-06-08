# Fix: IndexOutOfBoundsException when uploading media with an undecodable preview

## Symptom

```
java.lang.IndexOutOfBoundsException: Index 6 out of bounds for length 6
    at java.util.ArrayList.get(ArrayList.java:434)
    at chat.simplex.common.views.chat.ComposeViewKt.ComposeView$sendMessageAsync(ComposeView.kt:827)
    ...
```

The crash fires when sending a batch of picked media (e.g. 7 items) in which at least
one item produces no preview bitmap — most commonly a corrupted or unusual video whose
first frame cannot be extracted.

## Root cause

`ComposePreview.MediaPreview` carries two parallel lists that are assumed to be
**equal-length and index-aligned**:

```kotlin
class MediaPreview(val images: List<String>, val content: List<UploadContent>)
```

Both consumers cross-index one list by the other's index, so the invariant is load-bearing:

- `ComposeImageView` (preview row) iterates `media.images` and reads `media.content[index]`.
- `ComposeView.sendMessageAsync` iterates `preview.content` and reads `preview.images[index]`
  (the `MCImage` / `MCVideo` preview string). This is the crash site.

`processPickedMedia` built the two lists out of step:

- `imagesPreview` was appended **only when `bitmap != null`**.
- `content` was appended **unconditionally** for videos, and for animated images that
  passed the size check — regardless of whether a preview bitmap was produced.

`getBitmapFromVideo` returns `PreviewAndDuration(null, …)` whenever Android's
`MediaMetadataRetriever` cannot extract a frame, **even when no exception is thrown**
(`Utils.android.kt:351`). So a single undecodable video appends to `content` but not to
`images`, leaving `content.size == images.size + 1`. Iterating `content` then indexes
`images[lastIndex+1]` → `Index N out of bounds for length N`.

This is a pre-existing bug in the shared media picker; it is unrelated to any in-flight
feature work and reproduces on both Android and Desktop (both use the `commonMain`
`processPickedMedia`).

## Fix

Keep `content` and `imagesPreview` strictly paired at the source. The `when` now yields an
`UploadContent?` instead of mutating `content` inside its branches, and both lists are
appended together, gated on a non-null preview bitmap:

```kotlin
if (bitmap != null && uploadContent != null) {
  content.add(uploadContent)
  imagesPreview.add(resizeImageToStrSize(bitmap, maxDataSize = 14000))
}
```

Each iteration now adds exactly zero or one entry to **both** lists, so the
equal-length / index-aligned invariant holds by construction.

### Behavior change

Media that yields no decodable preview frame is now **skipped** rather than enqueued.
Previously such a video crashed the send; now only the bad item is dropped and the rest of
the picked batch sends normally (the loop evaluates each URI independently).

The skip is **not silent**. A skipped video shows `showVideoDecodingException()`, gated on
`AlertManager.hasAlertsShown()` so the alert neither stacks across several bad items in one
batch nor duplicates the one `getBitmapFromVideo` already shows on its exception path. The
genuinely silent gap this closes is the video path that returns a null frame **without**
throwing (Android `getFrameAtTime` returns null; Desktop snapshot times out) — that path
previously produced no alert and then crashed on send.

Image decode failures need no new alert here: `getBitmapFromUri` is already called for every
image (animated or not) with `withAlertOnException = !hasAlertsShown()`, so a null image
bitmap is surfaced before this point. Only the video null-frame case lacked any notice.

## Why this approach

- **Fixes the invariant at its origin** rather than papering over it at the two read
  sites. Guarding `images[index]` in `sendMessageAsync` would stop the crash but leave the
  preview row (`ComposeImageView`) silently mismatched and the actual media set ambiguous.
- **Minimal, surgical diff** confined to `processPickedMedia`; no API/type changes, no new
  placeholder assets, no touch to the read sites.
- **Cross-platform by construction**: the change lives in `commonMain`, so Android and
  Desktop are both covered. iOS has a separate Swift compose implementation and is out of
  scope for this fix.

## Other `MediaPreview` construction sites (verified aligned)

- `cs.preview` → single-element `listOf(mc.image)` / `listOf(content)` (edit path): aligned.
- `constructFailedMessage` takes `last()` of each list: aligned if the input was aligned.

## Test notes

Manual repro: pick a multi-item batch including a corrupted/zero-frame video and send.
- Before: `IndexOutOfBoundsException` on send.
- After: the undecodable item is dropped; remaining media sends normally.
