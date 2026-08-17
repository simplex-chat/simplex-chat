# Fix desktop drag-and-drop of videos attached as files

Branch: `nd/fix-video-drag-and-drop` · base: `master`.

## Problem

On desktop, dragging a video file into a chat attaches it as a generic file (paperclip + filename) instead of as a video (thumbnail + duration). Dragging an image works. Picking the same video via "Gallery → Video" attaches it correctly — so only the drag-and-drop routing is wrong.

## Fix

One file: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt`. Recognise videos as media in `onFilesAttached`'s classifier.

```diff
 fun MutableState<ComposeState>.onFilesAttached(uris: List<URI>) {
-  val groups =  uris.groupBy { isImage(it) }
-  val images = groups[true] ?: emptyList()
+  val groups = uris.groupBy { isImage(it) || isVideoUri(it) }
+  val media = groups[true] ?: emptyList()
   val files = groups[false] ?: emptyList()
-  if (images.isNotEmpty()) {
-    CoroutineScope(Dispatchers.IO).launch { processPickedMedia(images, null) }
+  if (media.isNotEmpty()) {
+    CoroutineScope(Dispatchers.IO).launch { processPickedMedia(media, null) }
   } else if (files.isNotEmpty()) {
     processPickedFile(uris.first(), null)
   }
 }
+
+private fun isVideoUri(uri: URI): Boolean {
+  val name = getFileName(uri)?.lowercase() ?: return false
+  return name.endsWith(".mov") || name.endsWith(".avi") || name.endsWith(".mp4") ||
+      name.endsWith(".mpg") || name.endsWith(".mpeg") || name.endsWith(".mkv")
+}
```

Total diff: 1 file, +11 / −5.

## Cause

`onFilesAttached` classified URIs by `isImage` only — non-images (including videos) fell through to `processPickedFile`, producing a `FilePreview`. The downstream `processPickedMedia` already handles video correctly (its `else` branch builds `UploadContent.Video`); the classifier above it just never reached that branch. The existing `isVideo` in `Videos.desktop.kt` is `desktopMain`-only and not visible from `ComposeView.kt` in `commonMain` — the structural gap that left the classifier video-blind. The inline `isVideoUri` uses the cross-platform `getFileName`, so the same fix also corrects the paste path (`onFilesPasted` at `ComposeView.kt:1378`).

## Risk

One file, no interface change. Image and non-media drops are bit-identical. Video extension list is now duplicated with `Videos.desktop.kt`; adding a new format means updating both — accepted as the cost of a single-file fix. iOS unaffected. Rollback: revert the commit.
