# Fix: video preview expands into a long empty black area

## Problem

On Android, once a video message finishes downloading, its in-chat preview grows
into a tall empty black area below the thumbnail. It appears the moment the file
download completes (before playback), and is worst for landscape videos.

## Root cause

The video message renders into the shared `CHAT_IMAGE_LAYOUT_ID` box measured by
`PriorityLayout` (`FramedItemView.kt`). Unlike `CIImageView`, `CIVideoView` never
gave that box a definite size — it inherited the height of its tallest child.

Once downloaded, the tallest child is the player surface. On Android that is a
`StyledPlayerView` with `RESIZE_MODE_FIXED_WIDTH`. Before playback the player is
unprepared (the media source is only set in `VideoPlayer.start()`, called from
`play()`), so it reports no video size and its `AspectRatioFrameLayout` keeps
aspect ratio 0 and does **not** shrink its height.

This was latent until #6726 ("constrain image sizes for previews"). That change
bounded the box to `maxHeight = 2.33 × width` to tame extreme image aspect
ratios. Before it, the box was measured with unbounded height, so on Android the
unprepared surface collapsed to 0 (`getDefaultSize` with an `UNSPECIFIED` spec)
and contributed no height — accidentally masking the missing-size flaw. After
#6726 the surface expands to fill the now-bounded `2.33 × width`, showing as a
long black strip behind the shorter thumbnail.

The later image fixes (#7123 / #7125 / #7223) all touched `CIImageView` only, so
video was never given a definite box size and kept the defect.

## Fix

Give `CIVideoView`'s `CHAT_IMAGE_LAYOUT_ID` box a definite size derived from the
preview aspect ratio, mirroring `CIImageView`. Height is computed from the
*measured* (clamped) width via `Modifier.width(w).layout { … }`, not the nominal
width, so a wide video whose nominal `w = DEFAULT_MAX_IMAGE_WIDTH` (500dp) is
clamped on a narrower screen does not leave an empty strip below — the same
correction #7223 applied to images. Applied only for `!smallView`; the small
chat-list thumbnail (`smallView = true`) keeps its existing sizing.

With the box sized to the preview footprint, the player surface can no longer
expand past it (its bounded max height now equals the thumbnail height), and the
opaque preview drawn on top exactly covers it. When playing, `RESIZE_MODE_FIXED_WIDTH`
conforms the surface to the real video aspect, which matches the thumbnail.

## Alternatives considered

- **Force the Android `StyledPlayerView` aspect from the preview.** Android-only;
  treats one render backend and leaves the box height still child-dependent — a
  workaround at the wrong layer.
- **Only mount `PlayerView` while playing.** Larger behavioural change; risks the
  play/preview transition blinking the code already guards against; does not
  establish the container invariant.
- **`matchParentSize()` on the player.** Breaks: when playing, the preview is
  removed, so no sibling defines the box and it collapses to 0.
- **Naive `Modifier.width(w).height(w * ratio)`.** Reintroduces the exact #7223
  empty-strip bug for wide videos on screens narrower than 500dp.

The chosen fix restores the invariant "a media chat item occupies exactly the
media's aspect-ratio footprint" at the correct layer (the media container), for
all render states (pre-download / downloaded-idle / playing / encrypted) and both
platforms, using the same aspect source the player conforms to.

## Verification

- `android:assembleDebug` (arm64 debug APK) — BUILD SUCCESSFUL.
- Desktop `createDistributable` + AppImage (Linux x86_64) — BUILD SUCCESSFUL.
- Control APK built from the same base without the fix, to A/B the black area
  against the fixed build (both `7.0-beta.4`, differing only by this change).
