# Desktop: video playback hangs after a preview snapshot stalls

Branch: `nd/fix-video` · final code commit `4c7073bdc` · PR [#6983](https://github.com/simplex-chat/simplex-chat/pull/6983).

## 1. Problem statement

On Desktop with several videos in a chat, clicking the play button on the second (or any subsequent) video does nothing. The first video plays normally; later ones present a play button that responds to the click but never starts playback. No error dialog appears in the UI. `stderr` shows libvlc and libavcodec noise:

```
[h264 @ 0x...] get_buffer() failed
[h264 @ 0x...] thread_get_buffer() failed
[h264 @ 0x...] decode_slice_header error
[h264 @ 0x...] no frame!
... main video output error: Failed to grab a snapshot
```

The bug appeared after PR [#6924](https://github.com/simplex-chat/simplex-chat/pull/6924) (`ab2d03630`), which switched the preview helper player from the shared `vlcFactory` to a dedicated `vlcPreviewFactory` with `--avcodec-hw=none`. Hardware-accelerated decoding had previously masked the underlying fragility. Scope: Desktop only.

## 2. Root cause

Two compounding defects in `VideoPlayer.desktop.kt`, surfaced by `#6924`:

### 2a. Synchronous `snapshots().get()` blocks the shared `playerThread` indefinitely

`getBitmapFromVideo` ran inside `withContext(playerThread.asCoroutineDispatcher())` — the same single-thread executor used by `play()`/`stop()` for playback. Its loop polls vlcj's snapshot API:

```kotlin
while (snap == null && start + 1500 > System.currentTimeMillis()) {
  snap = player.snapshots()?.get()
  delay(50)
}
```

The 1500 ms wall-clock guard only fires *between* calls. `player.snapshots()?.get()` is a synchronous JNI call that, when libvlc cannot produce a frame, waits indefinitely. While it blocks, `playerThread` is held: every queued `playerThread.execute { videoPlaying.value = start(...) }` from a subsequent `play()` click sits in the queue and never runs.

This was confirmed by instrumented printlns: after the first video's preview entered the snapshot loop, the second video's `play()` body executed (UI thread println fires), but its lambda submitted to `playerThread.execute` produced no `lambda started` print — because `playerThread` was stuck inside the JNI call.

### 2b. Helper-player pool reuse exhausts the software h264 buffer pool

`getOrCreateHelperPlayer()` returns a `CallbackMediaPlayerComponent` from `helperPlayersPool`, recycling it across preview generations. With `vlcFactory` (hardware-accelerated by default), this was harmless — the GPU buffer pool was large with different lifecycle semantics. After `#6924` switched the helper to `vlcPreviewFactory` (`--avcodec-hw=none`), libavcodec frames from the previous run were not released cleanly across `stop` + `startPaused`, and the second decoder ran out of buffers (`get_buffer() failed`). The vout never produced a frame, which is the trigger for the hang in 2a.

## 3. Solution summary

`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/VideoPlayer.desktop.kt` — single file, +8 / −6 lines. Helper-player pool is preserved as-is.

1. **Replace the polling `snapshots().get()` loop with a `CallbackVideoSurface` capture wrapped in `withTimeoutOrNull`.** The existing `SkiaBitmapVideoSurface` (already used for full-screen playback rendering) is attached to the helper player before `media().startPaused(...)`. Its `RenderCallback.display()` runs as soon as libvlc decodes the first frame, populating `surface.bitmap`. `getBitmapFromVideo` polls `surface.bitmap.value` from inside `withTimeoutOrNull(1500L) { ... }`; the wait is now structurally bounded — the synchronous JNI call is gone. Frame is converted to `BufferedImage` via `ImageBitmap.toAwtImage()` for the existing orientation-correction code path. This addresses 2a directly: a helper that fails to decode (2b) no longer holds the dispatcher.

2. **Move preview generation to a dedicated executor.** A new `previewThread = Executors.newSingleThreadExecutor()` runs `getBitmapFromVideo`. Defense in depth: even if 1500 ms of preview work overlaps with a play click, playback's `playerThread` is free to service it.

The pool is intentionally not touched. Removing it loses the factory-warmup amortization across distinct video URIs without addressing the actual hang (which is in the synchronous snapshot API, not in player reuse).

## 4. Alternatives considered (and rejected)

- **Drop the helper-player pool (initial attempt, commit `4a964c661`).** Replaces every preview's helper with a fresh `CallbackMediaPlayerComponent`. Fixes the symptom by sidestepping pool reuse, but costs the factory-warmup benefit and does not address the underlying blocking JNI call — a single corrupt video could still hang preview generation indefinitely (just on a fresh helper). Superseded by the surface-capture approach.
- **Keep the pool, reset the helper between uses.** vlcj has no clean reset API; would require `media().release()` + manual re-attach. More code, fragile, doesn't address 2a.
- **Wrap `snapshots().get()` in a coroutine timeout on a separate IO thread.** `withTimeoutOrNull` cannot cancel a blocked JNI call; the IO thread leaks until libvlc returns (which may be never).
- **Revert PR #6924.** Restores the masking effect of hardware-accelerated decoding but reintroduces whatever the PR was guarding against, and leaves both 2a and 2b in place.
