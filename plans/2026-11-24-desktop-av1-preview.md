# Desktop: attach AV1 videos as videos

## 1. Problem

The desktop app rejects every AV1 video attached as a video with "Decoding error: The video cannot be decoded". H.264 videos attach.

## 2. Cause

libVLC 3.0.23 (simplex-chat/vlc v3.0.23-2, revision 92ddd6d); vlcj 4.8.3. In `getBitmapFromVideo` (VideoPlayer.desktop.kt), the preview player is started with `startPaused`, and one frame is awaited for 1.5 s. vlcj `startPaused` starts the media with `:start-paused`, then calls `setPosition(0)`.

```mermaid
sequenceDiagram
  participant App as getBitmapFromVideo
  participant In as input
  participant Dec as decoder
  participant Vout as video output
  App->>In: start with :start-paused
  In->>Dec: pause
  Note over Dec: no video output yet: pause not passed on (decoder.c:1574-1585)
  Note over Dec: decoder wait ended by the pause (decoder.c:2307-2312): picture not marked forced (decoder.c:1045-1050)
  App->>In: setPosition(0)
  In->>Dec: flush while paused: frame countdown set (decoder.c:2136-2141)
  Dec->>Vout: created unpaused (video_output.c:1601)
  Dec->>Vout: first picture
  Note over Vout: H.264: first picture marked forced by avcodec (avcodec/video.c:1430-1432): displayed
  Note over Vout: AV1: no picture marked forced by dav1d: late picture dropped (video_output.c:850-876)
  Note over Dec: paused again: no further pictures (decoder.c:1594)
  App->>App: no frame in 1.5 s: null preview
  Note over App: "Decoding error" alert (ComposeView.kt, processPickedMedia)
```

The plugins and the code path are the same in the Linux, macOS and Windows builds of v3.0.23-2; the failure was reproduced on Linux only.

## 3. Change

`vlcPreviewFactory` (RecAndPlay.desktop.kt) is created with `--no-drop-late-frames`. The option is read only at video_output.c:1600 and used only at video_output.c:850 and 864.

Users of `vlcPreviewFactory`:

| Caller | Effect |
|---|---|
| `getBitmapFromVideo`, attaching a video | the first picture is passed to the callback |
| `getBitmapFromVideo`, received video previews | the first picture is passed to the callback |
| `hasVideoTrack` | none: parsing only, no video output |

`vlcFactory` is used for playback and is unchanged.

Unverified: on macOS, the videotoolbox decoder is not disabled by `--avcodec-hw=none` and marks no first picture forced (videotoolbox.m), so H.264 previews there may be dropped the same way; the change applies to them too.

## 4. Acceptance

| Check | Base | Change |
|---|---|---|
| AV1 1080x1920 + xHE-AAC, pasted into a chat | "Decoding error" | preview shown, sent as video |
| AV1 360x640 and 1080x1920, without audio | "Decoding error" | preview shown |
| H.264 1080x1920 | preview shown | preview shown |
