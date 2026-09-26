# Desktop: app freezes when a video is stopped while its format is being negotiated

## Problem

On desktop, playing a video in the chat and then opening it fullscreen (sometimes after a few attempts) leaves the fullscreen view black. The whole app stops responding: the back button, other views and the window close button all do nothing. A process restart is the only way out.

It reproduces with a video that makes VLC renegotiate its output format repeatedly (the log is full of `main filter error: Failed to create video converter`). Test videos that negotiate once did not reproduce it.

## Evidence

Reproduced on a build of `master` (VLC 3.0.23 bundled) by looping: play inline, click to stop, click to open fullscreen, back. It froze on the second iteration. Two `jcmd Thread.print` dumps taken 2 s apart were identical:

| Thread | Where it is |
|---|---|
| `pool-3-thread-1` (`VideoPlayer.playerThread`) | `libvlc_media_player_stop` ← `VideoPlayer.stop()` (`VideoPlayer.desktop.kt:117`) |
| `Thread-10825` (vlc decoder thread in a JNA callback) | `libvlc_video_get_track` ← `SkiaBitmapBufferFormatCallback.getBufferFormat` (`SkiaBitmapVideoSurface.kt:83`, `player.video().track()`) |
| `AWT-EventQueue-0` | `libvlc_media_player_get_time` ← `VideoPlayer.currentPosition` ← progress coroutine on `Dispatchers.Main` (`VideoPlayer.desktop.kt:94`) |

The JVM needs `-XX:+StartAttachListener` for `jcmd` to attach, because the Haskell RTS takes over `SIGQUIT`.

## Cause

In VLC 3.0.23:

- `libvlc_media_player_stop` (`lib/media_player.c:1065`) takes `lock_input` and, while holding it, calls `release_input_thread`, which ends in `input_Stop` + `input_Close` (`:183-184`). These wait for the input thread, and its decoder threads, to exit.
- `libvlc_video_get_track` (`lib/video.c:628`) and `libvlc_media_player_get_time` (`lib/media_player.c:1358`) both go through `libvlc_get_input_thread`, which takes the same `lock_input` (`lib/media_player.c:198`).

`getBufferFormat` runs on the decoder thread during format negotiation. When `stop()` arrives at that moment, `stop` holds the lock and waits for the decoder thread, and the decoder thread waits for the lock in `video().track()`. The UI thread's 50 ms `get_time` poll then blocks on the same lock, which freezes the app.

The `track()` call was added in #7391, which fixed stretched AV1 playback by using the track size instead of the padded size that libvlc passes.

## Fix

Stop calling `video().track()` in `getBufferFormat`. Take the track from `media().info().videoTracks()` only, using the `singleOrNull()` fallback the code already used before VLC had selected a track.

Why the remaining calls cannot deadlock the same way:

- `release_input_thread` is called only from `stop` (`:1066`) and `set_media` (`:886`), which hold only `lock_input` while waiting, and from `destroy` (`:819`), which holds no lock.
- `media()` → `libvlc_media_player_get_media` takes `object_lock` (`:922`), which is never held while waiting for the input thread.
- `info()` → `libvlc_media_tracks_get` takes only the media item's lock (`lib/media.c:967`).

Behaviour change: none for a file with one video track, which is every video SimpleX sends. A file with several video tracks gets the size libvlc passed, as it did before #7391.

## Alternatives considered

- **No libvlc calls in the callback at all.** Read the track size on `playerThread` before playback and hand it to the surface. This is more code and adds a metadata read before every play, and the lock analysis above shows it buys nothing for this deadlock.
- **Crop the padded frame instead of asking for the track size.** This does not work: vlc scales the picture to whatever size the callback returns, which is why AV1 was stretched before #7391.
- **Size from vlcj elementary-stream events.** These can arrive after the format has been negotiated, so the first frames would use the padded size.
- **Moving the progress polling and click handlers' libvlc calls off the UI thread.** This would stop any future libvlc stall from freezing the whole app, but it is a separate change across several files and is left out of this bugfix.
