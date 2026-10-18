# Android: orientation locks stop tap-to-stop recording and loop "Migrate to another device"

## Problem

Two flows lock the screen orientation by writing `Activity.requestedOrientation`:

- Tap-to-stop voice recording: `LockToCurrentOrientationUntilDispose()` (`UI.android.kt`), called from `SendMsgView`.
- Migration, both directions: `androidLockPortraitOrientation()` (`SimplexApp.kt`), called from `MigrateFromDevice` and `MigrateToDevice`.

`MainActivity` handles only `uiMode` itself (`android:configChanges="uiMode"`), so any orientation or size change recreates it. On a display that is landscape by default (Waydroid in a landscape window, landscape tablets before Android 16, Chromebooks):

- Tapping record once flashes the screen and leaves a finished 0:00 voice message instead of a running recording. The recording lock maps rotation 0 to portrait (it assumes every display is portrait by default), so it asks for the other orientation, the activity is recreated, and `RecordVoiceView`'s `onDispose { rec.stop() }` stops the recording.
- "Migrate to another device" (and, by the same lock, presumably the onboarding "Migrate" button, whose screen is titled "Migrate here"; untested) never opens: the activity recreates in a loop. The screen asks for portrait, the activity is recreated, and the lock is released in `onDispose` during that recreation, so the app goes back to landscape, the activity is recreated again, the screen asks for portrait again, and so on. Reproduced where the portrait request reshapes the app window (Waydroid); expected but untested where the display rotates and goes back to landscape when the lock is released (landscape tablets or Chromebooks, e.g. with auto-rotate off; possibly phones held in landscape with auto-rotate on).

## Fix

Both locks now request `SCREEN_ORIENTATION_LOCKED`: keep the orientation the screen already has. It does not itself change the orientation, so the activity is not recreated (minSdk 26; LOCKED exists since API 18).

- Recording: replaces the rotation-to-orientation mapping.
- Migration: replaces the portrait lock. A phone held in landscape now stays landscape during migration (master asks it to turn to portrait; whether a phone with auto-rotate on settles or loops is untested). The portrait lock came with migration (#3854) and its comment ("good experience with calls") was copied from the call screen, which stopped forcing portrait in #5124; iOS already migrates in landscape, and the same QR code and scanner are shown unlocked in landscape elsewhere on Android.

Both locks are released, as before, when their screen leaves the composition.

Migration's large progress ring (upload and download) is sized to the window width, which in landscape made it about two screens tall. In landscape it is now sized to the window space between the top of its box (measured independent of scrolling) and the bars at the bottom (the navigation bar, plus the app bar in one-hand mode), and its box no longer fills the rest of the column (which centred the ring lower), so the ring and the percentage are visible without scrolling (except on very short windows, where the ring keeps a 120dp minimum); in landscape, when the ring is smaller than 300dp, the percentage and byte count shrink with it so they stay inside the ring at the default font size. Portrait and desktop keep their sizes. The spec (`spec/architecture.md`) is updated for the lock.

## Alternatives considered

- **Keep migration in portrait and skip the unlock while the activity is being recreated** (an earlier draft of this fix): entering migration on a landscape display still recreates the activity once, and a migration screen closed during a recreation (a crash, or Back during a configuration change) leaves portrait locked until migration or tap-to-stop recording is next used or the app is exited.
- **Handle orientation and size changes in `MainActivity`** (`configChanges`): stops both bugs, but also every recreation on rotation and resize for all users, and some code relies on it — the in-app language is applied in `MainActivity.onCreate` and would be lost after a rotation; `windowOrientation()` (passcode layout, user picker, onboarding cards, navigation bar colour) is read without being observed; media widths remembered from the window width keep the old width.
- **Keep the recorder alive across recreation**: would also fix hold-to-record on rotation, but is a much larger change and still lets the screen rotate mid-recording.

`androidLockPortraitOrientation` keeps its name to keep the diff to the lock itself.

## Large screens on Android 16

Apps targeting SDK 36 have portrait/landscape requests ignored on large screens (smallest width ≥ 600dp), so the old locks did nothing there. `SCREEN_ORIENTATION_LOCKED` is not one of the documented ignored values, so the new locks may take effect on those devices; this is not tested here (no Android 16 tablet).

## Not changed

- Hold-to-record still stops when the device rotates, as on master: rotation recreates the activity.
- A window resize while recording (split screen, desktop windows) still recreates the activity and stops the recording, as on master.

## Testing

Waydroid (Android 13, `wm size 1440x720` for a landscape display; master APK vs this PR's APK), counting `wm_on_create_called` for `MainActivity` in the events log: tap-to-stop recording (master 2 recreations, PR 0); recording while the display is resized (recreated on both, unchanged); Settings → "Migrate to another device" on a landscape display (master loops, PR 0 recreations, stays landscape, leaving releases the lock) and on a portrait display (PR 0 recreations); the upload progress ring in landscape with one-hand UI on (PR: whole ring and percentage visible) and in portrait (same as master). Regression: app language after a resize, theme switch (master by screenshots), passcode layout and prompt after a resize (same as master); language change (PR). Not run: onboarding "Migrate here" and its download ring, one-hand UI off, Android 15+, a resize during migration, physical rotation while recording, Android 16 large screens. A check on a real phone follows.
