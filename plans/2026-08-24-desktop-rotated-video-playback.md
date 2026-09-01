# Desktop: rotated videos squashed on playback, preview rotated twice, snapshot crash

## Problem

A video carrying rotation metadata - what a phone records in portrait - is squashed when it
plays in a chat item on desktop, while its preview looks correct. Re-sending such a video from
desktop produces a preview that is wrong for every recipient. Attaching one can take the app
down with a SIGSEGV inside skia. Separately, any video smaller than the width of the message
item plays at its own size in the middle of the item instead of filling it, though its preview
fills the width.

## Cause

Three defects, of different ages, on the path a frame takes from libvlc to the screen.

**The buffer is sized from the wrong dimensions.** `SkiaBitmapVideoSurface` asks libvlc for a
buffer of `track.width() x track.height()`. The track carries the size before rotation, but vlc
applies the display matrix before the frame reaches the vmem callback, so the picture arriving
is transposed with respect to the buffer, and vlc stretches it to fill. Measured with a
1920x1080 HEVC file whose display matrix is -90:

| source                     | value                |
| -------------------------- | -------------------- |
| size libvlc offers          | 1088x1920 (rotated)  |
| `track.width()/height()`    | 1920x1080 (coded)    |
| buffer requested before fix | 1920x1080            |

Asking for the track size was introduced to drop the decoder's padding (#7391); it is right for
an unrotated video and wrong for a rotated one, because it discards the orientation libvlc had
already applied.

**The preview is oriented twice.** `previewAndDuration` takes a snapshot from the same surface
and then rotates it by hand. The snapshot arrives at 1080x1920 and already upright - dumping it
to a PNG confirms the content, not just the dimensions - and the manual rotation turns it back
to 1920x1080. Before this change the two errors cancelled: a wrongly shaped buffer plus a
manual rotation produced a preview that looked right, which is why the preview was correct while
playback was not.

**The snapshot races the render callback.** The render callback installs pixels into the shared
bitmap on the event thread; the snapshot converted it on the preview thread. The format is
renegotiated several times per file, so a resize between `installPixels` and `readPixels` makes
skia read past the end of the buffer:

```
SIGSEGV ... C [libskiko-linux-x64.so+0x1e7807] sse2::load_8888(...)
  at org.jetbrains.skia.Bitmap.readPixels
  at chat.simplex.common.platform.VideoPlayer$Companion$getBitmapFromVideo$2$snap$1
```

**Small videos do not fill the item.** The preview is drawn with `ContentScale.FillWidth` and
the playback surface with `ContentScale.Fit`. `Fit` never exceeds the height of the box, so a
320x240 video stays at its own size, centred, while its preview fills the width. This is only
visible for sources narrower than the item.

## Fix

Swap the requested width and height for the four transposed orientations, so the buffer matches
the picture vlc delivers, and keep the track size otherwise so the padding fix still holds.
Drop the manual orientation handling from the preview, since the frame is already upright. Read
the snapshot on the event thread, where the render callback writes it. Draw the inline playback
surface with `FillWidth`, as its preview is drawn.

## Bounds

The dimensions come from a received file, so they are attacker-chosen and are treated as such.

- Both track sides are used or neither. One side from the track beside the other from libvlc's
  padded size never described the same picture, and transposing such a pair compounds it.
- The requested area is capped, scaling down and keeping the aspect where both sides can shrink;
  a side pinned at 1 takes the whole budget on the other side instead, since scaling cannot keep
  the aspect of a 2000000000x1 declaration and hold the area at once. An unbounded request is an
  out-of-memory from a message: 16000x16000 is 1 GB of RV32, requested from vlc and copied into
  a java array of the same size. The cap also keeps `width * height * 4` inside an `Int`.
- Neither side can be zero, so a 1x4000 or 4000x1 file cannot produce an empty buffer.
- The sides are only swapped when the track's own sides are used. The size libvlc passes is
  already rotated, so swapping that pair would recreate the squash for a file that declares a
  rotation and a zero-sized track.
- A frame is dropped rather than displayed when it does not fill the bitmap skia is told to
  read, when the format it was rendered with is not the one the bitmap was sized by, and before
  any buffer has been allocated. The checks and the copy run inside the render callback, on
  vlc's thread: the native buffer is only guaranteed to exist for the duration of the callback,
  so code deferred to another thread would read through a pointer vlc may have freed on a format
  change. Only the copied frame is handed to the event thread.
- The bitmap is published only when skia reports that it took the pixels, and a snapshot that
  cannot be converted is logged and left empty rather than thrown into callers that have no
  handler for it.

## Testing

Fifteen files covering 320x240 to 3840x2160, square, odd, and 1234x567 sizes, h264, vp9, av1
and hevc, unrotated and 90/180/270, plus 1x4000, 4000x1 and 16000x16000. Checked that rotated
videos play upright and preview upright, that a re-sent video keeps its shape, that attaching
does not crash, that a 320x240 video fills the item, that the AV1 padding fix still holds, and
that the 16000x16000 file is scaled to the cap instead of allocating a gigabyte.

## Android

None of these reach android. The buffer format callback is desktop only - android renders
through exoplayer's `StyledPlayerView`, with no buffer for us to size - and its preview comes
from `MediaMetadataRetriever.getFrameAtTime`, which returns an oriented frame and is not
rotated again. The event thread race is skia and swing. Android already fills the item width
with `RESIZE_MODE_FIXED_WIDTH`, which is what the `FillWidth` change gives desktop.

## Not addressed

`CIVideoView` bounds the item's aspect ratio above at 2.33 but not below, so a 4000x1 video
still lays out with a height that rounds to zero. The snapshot's `invokeAndWait` is not
cancellable, so its 1.5s timeout cannot interrupt a wedged event thread. Both are outside the
functions this change touches.
