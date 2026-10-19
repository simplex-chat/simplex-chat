# Blur media thumbnails in reply quotes and the chat list

## Problem

"Blur media" hides images and videos in a chat until they are tapped (on desktop, hovered), but
the same media is shown sharp in two other places:

- the thumbnail of a quoted image or video in a reply (`FramedItemView.ciQuoteView`, 68 dp), in
  every chat, search result, report and support chat;
- the chat list, where the last message's image, video or link preview is drawn as a 36 sp
  thumbnail next to the message text (`ChatPreviewView.chatItemContentPreview`).

The chat list is on screen most of the time, so it shows exactly what the setting promised to
hide.

## Cause

Small views were excluded on purpose: every call passed `privacyBlur(enabled = !smallView, ...)`
and `desktopModifyBlurredState(!smallView, ...)`, the quote thumbnail never called them, and the
chat-list link preview was a plain `Image`. The chat-list video views (`SmallVideoView`,
`SmallVideoViewEncrypted`) created their own `remember { mutableStateOf(false) }` blur state, so
they could not be blurred even if the modifier were enabled.

Enabling the existing blur for small views is not enough on its own, because
`ImageBitmap.blurredBy` (#7483) resamples the preview to `400 / radius` pixels across the whole
image, a width calibrated for media drawn about 360 dp wide. Drawn into a 36 sp thumbnail, 33 px
at Soft is almost sharp.

## Fix

`privacyBlur` takes `fullSize` in place of `enabled`. Existing call sites keep passing
`!smallView` or `true`, so in-chat media takes the same code as before: the same `remember`ed
resample, drawn with `drawWithContent`. Small views (`fullSize = false`):

- are resampled relative to their drawn width: `blurredBy` gets the drawn width, and the pixel
  count is `400 * drawnWidth / 360 / radius`, so a thumbnail is blurred as much on screen as
  media in a chat. The default width is 360, which gives exactly master's `400 / radius` for
  in-chat media;
- are drawn as the centre crop of the image scaled by `ContentScale.Crop`, clipped to the view,
  so the blurred thumbnail covers the same region as the revealed one and is blurred evenly in
  both directions; a whole image squeezed into a square would be blurred up to 4x less along
  its long side;
- use `drawWithCache`, so the blur is computed once per size and not on every frame.

`enabled` is removed from `desktopModifyBlurredState`: once small views are blurred every caller
passes `true`. It stays on `blurHidesMedia`, which still has a real caller (`CIImageView`).

Wiring:

- quote thumbnails get one blur state per quote, the item's menu state, and the same reveal as
  chat media: a tap reveals, a second tap scrolls to the quoted message, long-press opens the
  item menu;
- the chat-list link preview gets a remembered bitmap and blur state;
- `SmallVideoView` and `SmallVideoViewEncrypted` take the item's real blur state, and their play
  buttons, which open the video full screen, are hidden while it is blurred.

## Leaks closed by the change

Blurring the chat list exposed three ways for a thumbnail to be shown, or stay shown, without
the user revealing it:

- the chat list is keyed by chat, not by message, so state remembered for one last message was
  reused for the next: a new image arriving in a chat whose previous image was revealed would be
  shown revealed. `key(ci.id)` around `chatItemContentPreview` gives each message its own state;
- the chat list passed CIImageView and CIVideoView a throwaway `showMenu` that their long-press
  and right-click set to `true` and nothing reset. On desktop `desktopModifyBlurredState` stops
  re-blurring on hover-out while `showMenu` is true, so a right-clicked thumbnail stayed sharp.
  The chat list now passes `noMenu`, which a `LaunchedEffect` resets as soon as it is set: a
  menu that closes at once;
- `chatViewScrollState` is a global written only by the chat view's list, and its collector was
  cancelled without a final value when a chat closed. Leaving a chat while its list was still
  flinging left it `true`, and every reveal in the chat list was undone at once by the Android
  re-blur, until another chat was opened. The collector now writes `false` on completion.

## Bounds

The preview dimensions come from the sender. `base64ToBitmap` never returns a bitmap with a zero
side (it falls back to an error bitmap), so the crop scale is finite. A zero-size view gives a
zero crop, which `blurredBy` clamps to one pixel. `blurredBy` keeps its existing bounds: the
first step caps both sides at 512 px and the output height at 400 px; a very wide preview widens
the resample to at most its own width while its height drops to a few pixels. The doubling
ascent still targets 200 px, so a thumbnail holds a bitmap of up to 384 px (about 590 KB) - more
than it needs at 36 sp, harmless.

## Base

The change is built on `stable` after #7483 (blur media by resampling), because the small-view
blur extends #7483's resample. On stable, `SimpleAndAnimatedImageView` has no `blurred`
parameter, which came from #7365 (animated images) on master; neither #7483 nor this change
depends on it.

## Verification

Read-only so far; nothing on this branch has been built or run.

- In-chat media: `(400f * 360 / 360 / r).toInt()` equals master's `400 / r` for every positive
  radius, and the `fullSize` path is master's code re-indented.
- The centre crop was checked by simulating Compose's `ContentScale.Crop` / `Alignment.Center`
  placement against the blur's crop for 125,388 combinations of preview size (1 to 10000 px,
  extreme aspects), view size (0 to 600 px) and measurement mode: no pixel differed.
- `Modifier.kt` was compiled with Kotlin 2.1.20 and the Compose 1.8.2 plugin against the 1.8.2
  desktop jars, app references stubbed; the bytecode shows the `drawWithCache` lambda is
  remembered on the preview and radius, so recomposition does not re-blur.
- Independent adversarial reviews, on master and again on the stable base, ended with two
  consecutive passes finding no defect.

To do before merging: build desktop and Android, and check with a test profile receiving an
image, a video and a link as a chat's last message, and a reply quoting each, at Soft, Medium and
Strong.

## Out of scope

- Keyboard and TalkBack: the outer click of a blurred video opens it full screen without
  revealing it first. This already happens in a chat on stable and master; fixing it changes
  in-chat behaviour, so it is a separate change. TalkBack also cannot reveal a blurred chat-list
  image.
- Turning blur on does not re-blur media already on screen on Android (desktop does); chat-list
  rows pick it up when recomposed from scratch.
- Chat-list reveals re-blur when a chat is scrolled, not when the chat list is.
- At 36 sp, Medium and Strong both resample to a single pixel: a flat colour.
- Chat-link cards (contact and group profile images), compose-bar previews of your own media and
  links, and the full-screen gallery (which shows neighbouring media sharp when swiped) are
  unchanged.
- iOS has the same gaps in quotes and the chat list; it follows as a separate change.
