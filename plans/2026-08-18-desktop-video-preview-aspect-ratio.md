# Desktop: video preview and playback stretched for AV1

## Problem

Videos sent from the desktop app arrive with the wrong aspect ratio. It is most visible with
AV1, and only at some resolutions. The preview image sent with the message carries the wrong
dimensions, so the distortion is seen by every recipient on every platform, not only by the
sender. Desktop playback is distorted in the same way.

## Cause

The preview frame and the playback surface both come from `SkiaBitmapVideoSurface`, which
allocates its bitmap from the size libvlc passes to the vmem buffer format callback.

That size is the size the decoder padded the picture to, not the size of the picture. dav1d
pads to a multiple of 128, so a 1920x1080 AV1 video is reported as 1920x1152. libvlc then sets
the visible area of the output format to whatever size the callback returns, which makes the
converter stretch the picture to fill it — the buffer holds a stretched frame, not a padded one.

Measured against the bundled VLC 3.0.21, comparing the resulting bitmap with the true picture:

| source        | AV1 bitmap | error           | H264 bitmap | error  |
| ------------- | ---------- | --------------- | ----------- | ------ |
| 1920x1080     | 1920x1152  | 6.7% too tall   | 1920x1090   | +0.9%  |
| 1280x720      | 1280x768   | 6.7% too tall   | 1280x738    | +2.5%  |
| 640x360       | 640x384    | 6.7% too tall   | 640x386     | +7.2%  |
| 1080x1350     | 1152x1408  | 2.2% too wide   | 1088x1378   | -1.3%  |
| 1080x1080     | 1152x1152  | none            | 1088x1090   | +0.2%  |
| 1024x768      | 1024x768   | none            | 1024x770    | +0.3%  |

Sizes already on the 128 grid are unaffected, which is why the report was "only at certain
dimensions". H264 pads much less, so it was there all along but barely visible.

Android and iOS are not affected: they use `MediaMetadataRetriever` and return cropped frames.

## Fix

Ask libvlc for the size of the track being played instead of accepting the padded size, and
fall back to the padded size when the track is not known. The media player is captured in
`attach`, which always runs before the format callback, because it is what registers the
native callbacks in the first place.

Two details the implementation depends on, both observed rather than assumed:

- The format is negotiated more than once, and vlc has not selected the track on the first
  calls (`video().track()` returns -1 there), so the single-track fallback is load-bearing.
- The first listed video track is not necessarily the one being decoded. Matching on the
  playing track id keeps a file with several video tracks correct; an earlier revision using
  the first track made such a file worse than before the fix (320x240 instead of 1920x1080).

Fixing it at the buffer format keeps the preview and playback correct from one change, and
costs nothing: libvlc already runs a converter to fill the buffer, so it is only given the
right destination size. Rescaling the snapshot afterwards was considered and rejected — it
leaves playback distorted and resamples the frame twice.

## Verification

- The buffer produced with the fix is pixel identical to the frame decoded by ffmpeg
  (PSNR inf) for both landscape and portrait clips.
- The real compiled class driven against the bundled VLC 3.0.21 produces correct bitmaps
  where the current code does not: 1920x1152 -> 1920x1080, 1152x1408 -> 1080x1350,
  1280x768 -> 1280x720, 768x384 -> 641x361.
- 33 clips covering AV1/H264/VP9, mp4/mkv/webm, rotated, anamorphic, multi-track, cover art,
  odd and 1x1 sizes, audio only and corrupt files, on both VLC 3.0.21 and 3.0.23.
- No buffer/format tearing across repeated, pooled and concurrent playback; with the fix every
  negotiation returns the same size, where before they disagreed.

## Out of scope

- `getBitmapFromVideo` reads the orientation from the first video track and has the same
  first-track assumption.
- Sample aspect ratio is ignored throughout, so anamorphic video is still shown with square
  pixels. Unchanged by this fix.
