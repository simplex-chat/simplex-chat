# Desktop: video playback hangs after a preview snapshot stalls

Branch: `nd/fix-video` · code commit `4a964c661`.

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

### 2a. Helper-player pool reuse exhausts the software h264 buffer pool

`getOrCreateHelperPlayer()` (line 283 before the fix) returned a `CallbackMediaPlayerComponent` from `helperPlayersPool`, recycling it across preview generations:

```kotlin
private fun getOrCreateHelperPlayer(): CallbackMediaPlayerComponent =
  helperPlayersPool.removeFirstOrNull()
    ?: CallbackMediaPlayerComponent(
         MediaPlayerSpecs.callbackMediaPlayerSpec().apply { withFactory(vlcPreviewFactory) }
       )
private fun putHelperPlayer(player: CallbackMediaPlayerComponent) = helperPlayersPool.add(player)
```

After the first preview, `player.stop()` was called and the component was put back in the pool. The next call to `getBitmapFromVideo` reused the same component on the same `vlcPreviewFactory` libvlc instance — and the same libavcodec h264 software decoder pool. With `--avcodec-hw=none`, libavcodec frames decoded by the previous run were never released cleanly across `stop` + `startPaused`, and the second decoder ran out of buffers (`get_buffer() failed`). The vout therefore never got a frame to display, and `libvlc_video_take_snapshot` logged `"Failed to grab a snapshot"`.

This was harmless before `#6924` because the previous helper used `vlcFactory` (default = hardware-accelerated when available). Hardware decoding has a much larger buffer pool with different lifecycle semantics — pool reuse never drained it. Switching to software-only decoding revealed the latent fragility.

### 2b. Synchronous `snapshots().get()` blocks the shared `playerThread` indefinitely

`getBitmapFromVideo` ran inside `withContext(playerThread.asCoroutineDispatcher())` — the same single-thread executor used by `play()`/`stop()` for playback. Its loop polls vlcj's snapshot API:

```kotlin
while (snap == null && start + 1500 > System.currentTimeMillis()) {
  snap = player.snapshots()?.get()
  delay(50)
}
```

The 1500 ms wall-clock guard only fires *between* calls. `player.snapshots()?.get()` is a synchronous JNI call that, when libvlc cannot produce a frame (because of 2a), waits indefinitely. While it blocks, `playerThread` is held: every queued `playerThread.execute { videoPlaying.value = start(...) }` from a subsequent `play()` click sits in the queue and never runs.

This was confirmed by instrumented printlns: after the first video's preview entered the snapshot loop, the second video's `play()` body executed (UI thread println fires), but its lambda submitted to `playerThread.execute` produced no `lambda started` print — because `playerThread` was stuck inside the JNI call.

## 3. Solution summary

`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/VideoPlayer.desktop.kt` — single file, +6 / −7 lines.

1. **Drop the helper-player pool.** `getOrCreateHelperPlayer` / `putHelperPlayer` / `helperPlayersPool` are removed. A new `createHelperPlayer()` returns a fresh `CallbackMediaPlayerComponent` per call. The two call sites in `getBitmapFromVideo` follow `player.stop()` with `player.release()` instead of returning the component to the pool. Each preview now runs against a fresh libvlc decoder context — no stale buffer-pool state carries across previews. Cost: `previewsAndDurations` already caches the result per URI, so each video file pays the component-creation cost at most once.

2. **Move preview generation to a dedicated executor.** A new `previewThread = Executors.newSingleThreadExecutor()` is added next to the existing `playerThread`. `getBitmapFromVideo`'s `withContext` switches to `previewThread.asCoroutineDispatcher()`. Preview generation no longer competes with playback for the same single-thread dispatcher: even if the snapshot JNI call blocks (e.g., on a corrupt video), `playerThread` stays free, and clicking play on any other video continues to work.

The two fixes are complementary. (1) addresses the root cause of the hang for the observed workload (multiple sequential previews on software decoder). (2) is defensive: `snapshots().get()` is a blocking JNI call with no timeout we control, and any future condition that makes a single preview hang must not be allowed to take playback down with it.

## 4. Alternatives considered (and rejected)

- **Fix 1B — keep the pool, reset the helper between uses.** vlcj has no clean reset API; would require `media().release()` + manual re-attach. More code, fragile, doesn't address the executor-sharing problem.
- **Fix 2A — replace `snapshots().get()` with a `CallbackVideoSurface` capture of the first `display()` frame.** Architecturally cleanest (no blocking JNI at all) but ~30 line refactor. Worth considering for a follow-up; out of scope for the immediate regression fix.
- **Fix 2B — wrap `snapshots().get()` in a coroutine timeout on a separate IO thread.** Doesn't actually cancel the JNI call; threads leak indefinitely.
- **Fix 5 — revert PR #6924.** Restores the masking effect of hardware-accelerated decoding but reintroduces whatever security/stability concern motivated `--avcodec-hw=none`. Pool reuse remains a latent footgun.

The chosen combination (1A + 3A) is the smallest patch that fixes the user-visible regression and prevents recurrence from the same root mechanism.
