# Desktop: attach AV1 videos as videos

## 1. Problem

The desktop app rejects every AV1 video attached as a video with "Decoding error: The video cannot be decoded". H.264 videos attach.

## 2. Cause

libVLC 3.0.23 (simplex-chat/vlc v3.0.23-2, revision 92ddd6d). `getBitmapFromVideo` (VideoPlayer.desktop.kt) starts the preview player paused and waits 1.5 s for one frame.

```mermaid
sequenceDiagram
  participant App as getBitmapFromVideo
  participant In as input
  participant Dec as decoder
  participant Vout as video output
  App->>In: startPaused
  In->>Dec: pause
  Note over Dec: vout absent: pause not sent (decoder.c:1574-1585)
  Note over Dec: decoder wait ends while paused (decoder.c:2307-2310): no picture forced (decoder.c:1045-1050)
  Dec->>Vout: create, unpaused (video_output.c:1601)
  Dec->>Vout: first pictures
  Note over Vout: H.264: avcodec forces its first picture (avcodec/video.c:1430-1432): displayed
  Note over Vout: AV1: dav1d forces none: late pictures dropped (video_output.c:850-876)
  Note over Dec: paused: no further pictures
  App->>App: no frame in 1.5 s: "Decoding error"
```

## 3. Change

`vlcPreviewFactory` (RecAndPlay.desktop.kt) is created with `--no-drop-late-frames`. The option is read only at video_output.c:1600 and used only at video_output.c:850 and 864.

Users of `vlcPreviewFactory`:

| Caller | Effect |
|---|---|
| `getBitmapFromVideo`, attaching a video | the first picture reaches the callback |
| `getBitmapFromVideo`, received video previews | the first picture reaches the callback |
| `hasVideoTrack` | none: parsing only, no video output |

Playback uses `vlcFactory`, unchanged.

## 4. Acceptance

| Check | Base | Change |
|---|---|---|
| AV1 1080x1920 + xHE-AAC, pasted into a chat | "Decoding error" | preview shown, sent as video |
| AV1 360x640 and 1080x1920, without audio | "Decoding error" | preview shown |
| H.264 1080x1920 | preview shown | preview shown |
