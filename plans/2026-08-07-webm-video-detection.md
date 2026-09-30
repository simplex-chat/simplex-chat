# Send dropped `.webm` as video only when it has a video track

## Problem

Dragging a `.webm` file onto the desktop compose area attaches it as a plain file instead of embedding it as a video with a preview frame and duration. Every other video container the app recognises (`.mov`, `.avi`, `.mp4`, `.mpg`, `.mpeg`, `.mkv`) embeds. The same omission hides `.webm` from the "Attach → video" file picker, so the only way to send one is "Choose file", which sends it as a document.

## Cause

`isVideoUri` (`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt:298`) classifies attachments by file extension and does not list `.webm`; the desktop picker filter `isVideo` (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/Videos.desktop.kt:5`) repeats the same list with the same omission. `onFilesAttached` groups the dropped URIs by `isImage(it) || isVideoUri(it)`, so a `.webm` fails both predicates, falls into the files group and reaches `processPickedFile`, which builds a `ComposePreview.FilePreview`.

Adding the extension to both lists is not sufficient on its own. Unlike the other containers, `.webm` is used about as often for audio alone as for video — it is `MediaRecorder`'s default audio container, and Opus/Vorbis in WebM is widespread on the web. An audio-only file classified as video reaches the video branch of `processPickedMedia`, where `getBitmapFromVideo` finds no video track, returns a null preview and raises the "video decoding" alert; the item is then skipped and nothing is attached at all (`ComposeView.kt:366-376`). That is strictly worse than the file attachment the same drop produced before.

## Fix

Add `.webm` to both extension lists, and for `.webm` alone decide from the file's content rather than its name. A new `expect suspend fun hasVideoTrack(uri)` (`views/helpers/Utils.kt`) reports whether the container declares a video track, reading metadata only and never decoding a frame. On desktop it is implemented with libvlc's media parse (`platform/VideoPlayer.desktop.kt`), which signals completion with an event rather than a poll, so no frame-decoding budget is needed; measured at 12-346 ms across VP8, VP9, AV1, alpha and a 42 MB file, with a 3 s timeout as a guard against a stuck parse. On Android it uses `MediaMetadataRetriever.METADATA_KEY_HAS_VIDEO`. Either implementation answering "no", or failing, attaches the file as a file, which is always safe.

`onFilesAttached` consults it only when a `.webm` is actually among the dropped URIs; every other attachment keeps the original synchronous code path on the caller thread, so the change adds no latency and no threading difference to images, documents or the other video containers. Files with a video track are sent as video, the rest as files.

The content check is applied only where the user has not said how the file should be sent — drag & drop and paste. An explicitly picked video is still trusted: selecting an audio-only `.webm` through "Attach → video" raises the existing decoding error, which matches how the other containers already behave.
