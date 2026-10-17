# Desktop: notification sound

Status: draft rev 10 (after review round 7). All decisions taken 2026-10-17. Windows sound is gated by verification V1. Base: `origin/master` a932c8298. Paths are relative to `apps/multiplatform`.
Related plans: [turn off](2026-10-17-desktop-notifications-turn-off.md), [Windows click](2026-10-17-windows-notification-click.md). Research notes: `~/ntf-research/twoslices-findings.md`, `pipeline-findings.md`, `windows-findings.md`.

## Problem

A message arrives while the user is in another window, and nothing is heard:
- macOS and GNOME notifications from SimpleX are silent.
- The OSes give no easy way to attach a sound to SimpleX. On Windows, SimpleX is not even a stable entry in notification settings (#4201).

Reports:
- #3895 (open): "SimpleX Chat does not handle notification sounds at all"; users compare it to Signal and Telegram.
- #4201 (open): Windows: SimpleX is not listed under "Notifications & actions", and its "play sound" switch keeps turning itself back on.

## Facts

- FACT [two-slices 0.9.1–0.9.7 source, `~/ntf-research/twoslices`]: no backend has a sound option. There is no builder method or hint, and macOS `soundName` is never set.
- Backend selection:
  - FACT [two-slices `ServicesToasterFactory`, the app's classpath]: macOS → `NotificationCenterToaster`; Linux → `NotifyToaster` (`notify-send -i -t`, no hints).
  - INFERENCE [`twoslices-findings.md:48`]: Windows → `AWTToaster`, unless Snarl or Growl listens on port 23053 (then GNTP).
- INFERENCE (verify per OS):
  - macOS and GNOME are silent.
  - KDE follows the system notification settings.
  - Whether the Windows AWT balloon makes a sound is unknown: the JDK never sets `NIIF_NOSOUND`, but #4201 shows the per-app switch is unreliable. This is V1.
- FACT [common `NtfManager.kt:39-50`]: message notifications are gated by `cItem.showNotification`, chat mute (`ntfsEnabled`) and window focus/open chat.
  - Contact-connected and contact-request notifications (`NtfManager.kt:21-37`) have no mute or focus gate.
  - Call invitations older than 3 minutes become plain notifications (`CallManager.kt:20-21`).
- FACT [`NtfManager.desktop.kt:97-98`]: everything above reaches desktop `displayNotification`, whose first gate is the profile one, `user.showNotifications = activeUser || showNtfs` (`ChatModel.kt:1401`). So the **active** profile always notifies, hidden or not.
- FACT [`RecAndPlay.desktop.kt:106-107`, in `AudioPlayer.start`, which `play` calls]: playback stops all videos, **stops an ongoing voice recording**, and replaces playback. A notification sound must not use it.
- FACT [`RecAndPlay.desktop.kt:53, 77`]: `RecorderInterface.stopRecording` is non-null exactly while a voice message is being recorded.
- FACT [`ChatModel.kt:242-243`, `CallManager.kt:18`]: `chatModel.activeCall` is non-null during a call, and `chatModel.activeCallInvitation` is non-null while an incoming call rings.
- FACT [`Utils.kt:34, 41-44`]: `withBGApi` runs on one shared single-thread executor. Audio device initialisation must not run there.
- FACT [`SimpleXAPI.kt:2965, 3012, 3046`]: `displayNotification` is called both on the event receiver coroutine and inside `withBGApi`, so callers are concurrent.
- FACT [`pipeline-findings.md` §4]: the bundled JRE (Corretto 17, `java.desktop`; `libjsound` on Linux) supports `javax.sound.sampled.Clip` (WAV, not MP3) on all three OSes. Nothing in the app uses javax.sound today.

## Ideal final result

Every desktop OS plays one SimpleX notification sound, controlled by one in-app switch, and never more than one sound per few seconds.

## Options

1. **The app plays a bundled WAV with `javax.sound.sampled.Clip`** when a notification is shown, on macOS, Linux and Windows (chosen).
   - Independent of VLC, so it never stops playback or recording.
   - No dependency, about 40 lines.
   - The same rule on every OS.
   - Cost: the OS Do Not Disturb mode is not respected (D4).
2. **A separate VLC `MediaPlayer`.** Depends on VLC initialisation, with more moving parts. Rejected.
3. **OS-native sound**: our own macOS `NSUserNotification.soundName` via JNA, Linux `notify-send -h string:sound-name:…`, Windows toast audio.
   - Respects DND, but needs three mechanisms, bypasses two-slices on macOS and Linux, and has unverified `sound-name` support.
   - Rejected for v1; possible follow-up.

## Design (option 1)

Narrative: when desktop shows a chat notification, it also plays the notification sound. Not if sound is off, not if a sound started in the last 3 s, and not while recording or in a call.

1. **Asset**: `common/src/desktopMain/resources/media/notification.wav`, ≤ 0.5 s, mono, 16-bit signed PCM, 44.1 kHz, about 30–50 KB.
   - Source (D1): a CC0 sound, e.g. from Kenney's CC0 "Interface Sounds" pack or a CC0-filtered freesound.org entry, converted to WAV.
   - The source URL and licence go in the commit message and the PR.
   - Two or three candidates are offered for choosing by ear before implementation.
2. **Player**: `object NotificationSound` in `NtfManager.desktop.kt`, next to its only caller.
   - `fun play()` hands the work to its own single-thread daemon executor, never `withBGApi` or the caller's coroutine.
   - On that thread:
     - If the previous open attempt was less than `MIN_SOUND_INTERVAL = 3 s` ago (D3), return.
       - The timestamp `lastAttemptNanos: Long?` starts as `null` (never played) and is set only when an open is attempted, not on these early returns, so the window doesn't slide.
       - A failing device is then not reopened for every message in a flood. This is the only rate limit; it covers bursts and floods across any number of chats (#7116).
     - The 3 s check uses `System.nanoTime()`, so wall-clock changes don't matter. The check and the timestamp live only on this thread, so concurrent `play()` calls just enqueue.
     - Otherwise, open a new `Clip` from `AudioSystem.getAudioInputStream(NotificationSound::class.java.getResource("/media/notification.wav"))`. Close the `AudioInputStream` in `finally`, also when `Clip.open` throws. A missing resource (`getResource` returns null) disables the sound for the session. The URL overload avoids the mark/reset requirement of the stream overload, and resources are loaded the same way in `RecAndPlay.desktop.kt:281`.
     - Add a `LineListener` that, on `STOP`, hands `close()` to the same executor, so no audio line stays open between sounds. Then `start()`.
     - `UnsupportedAudioFileException`, or an `IOException` while decoding the asset (a broken asset): log once, disable for the session.
     - `LineUnavailableException`, or an `IllegalArgumentException` from `AudioSystem.getClip`/`getLine` (no matching line: no device, device busy, Bluetooth switching, a sound server restarting): skip this sound only. Log at most once per minute, so sound returns when the device does.
     - Any other `Throwable`, including an `IllegalArgumentException` from `Clip.open`: skip this sound, with a rate-limited log. Every attempt is wrapped, so nothing escapes to the executor.
     - Once `Clip.open` succeeded, any later failure closes the Clip, so no audio line is held.
     - No alerts in any case. The "disabled for the session" flag is `@Volatile`, because the Windows click plan reads it from other threads.
3. **Call site**: `NtfManager.desktop.kt` `displayNotification`, after the profile gate (`:98`), before `displayNotificationViaLib`. With the Windows click plan, it moves into `onShown`. The conditions stay visible:

   ```kotlin
   val recordingOrCall = RecorderInterface.stopRecording != null || chatModel.activeCall.value != null || chatModel.activeCallInvitation.value != null
   if (appPreferences.desktopNotificationSound.get() && !recordingOrCall) NotificationSound.play()
   ```

   The 3 s rule is the sound's own property, so it lives in `play()`, which is named for what it does: it plays unless a sound just played.
   With the Windows click plan, this call moves into the `onShown` passed to `displayNotificationViaLib`, with the same conditions. On Linux and macOS, it then runs right after `toast()` on the `withBGApi` thread.
4. **Not added to**: `notifyCallInvitation` (calls ring through `SoundPlayer`) and `showMessage` (critical errors).
5. **Windows**: the same code runs.
   - Toggle off (the default): this plan changes nothing on Windows. After the Windows click plan, toasts without `<audio>` play the Windows default sound under SimpleX's per-app switch. That is today's behaviour if V1 finds the balloon sounds, and a new sound if it doesn't.
   - Toggle on, with the Windows click plan: toasts carry `<audio silent="true"/>`, and the app's sound plays from the toast thread only after Windows accepted the toast (that plan's `onShown`). The result is one sound, under the in-app toggle and the 3 s limit, and none when SimpleX is off in Windows. Under Focus Assist, Windows accepts the toast without a popup, and the sound still plays (D4).
   - Toggle on, before that plan (two-slices balloon): **V1** is a test on Windows of whether the balloon makes its own sound. It runs on a fresh Windows user with default settings, with SimpleX's per-app sound switch both on and off. "Silent" means silent with the switch **on**.
     - If it is silent, ship as designed.
     - If it sounds, there would be two sounds. Then the toggle is hidden on Windows until the Windows click plan lands, and the Settings → Notifications section footer on Windows says sound follows Windows settings. The result also sets that plan's `BALLOON_SILENT` constant.
6. **Preference**: `val desktopNotificationSound = mkBoolPreference(SHARED_PREFS_DESKTOP_NOTIFICATION_SOUND, false)` (D5: off by default), key `"DesktopNotificationSound"`, declared next to the turn-off plan's `desktopNotifications`. Not in `hintPreferences` or `AppSettings`.
7. **UI**: in Settings → Notifications (the turn-off plan's desktop screen), on all desktop OSes (subject to V1):

   ```kotlin
   val prefs = chatModel.controller.appPrefs
   PreferenceToggle(stringResource(MR.strings.…sound), disabled = !notificationsOn, checked = remember { prefs.desktopNotificationSound.state }.value) { prefs.desktopNotificationSound.set(it) }
   ```

   - `notificationsOn = remember { prefs.desktopNotifications.state }.value`. `NotificationsSettingsView.kt` already reads preferences through `chatModel.controller.appPrefs` (`:28, 31`).
   - `PreferenceToggle(disabled =)` (`SettingsView.kt:437`) greys the label as well as the switch. `SettingsPreferenceItem(enabled =)` only fades the thumb (`SettingsView.kt:423-434`, `DefaultSwitch.kt:27`), which is nearly invisible when the switch is off.
   - If the turn-off plan is not merged first, this plan carries the screen-enable change, and the row is never disabled.
8. **strings.xml** base: the toggle title.
9. **Docs**:
   - `spec/services/notifications.md` §4: the sound rules, why not VLC, DND not respected.
   - `spec/state.md`: the notification preferences table (`:336-345`).
   - `product/views/settings.md`.

Feature off (`desktopNotificationSound = false`, the default) = today's app, plus the new toggle row.

## Threat model

- The sound is a bundled asset. No peer data selects or shapes it.
- Flooding by a contact, a group or many groups: at most one sound per 3 s overall.
- Muted chats and muted inactive profiles never sound, because the sound runs after the same gates as the notification. Mentions-only chats sound only for mentions.
- The active profile, including an active hidden one, sounds like any notification: the sound adds no information beyond the notification.
- Contact requests have no mute gate, so a stranger can trigger at most one sound per 3 s by sending requests to a public address. That is the same exposure as the existing notification.
- The microphone would record the chime into a voice message, so the app plays no sound while recording, in a call, or while a call rings. On Windows, WinRT message toasts are silenced then too. Call and error toasts are silenced while recording or in a call; a second call toast while one call rings keeps the Windows sound. A fallback balloon may still sound when `BALLOON_SILENT = false` (a Windows plan known limit).
- Bystanders hear that a message arrived. Off by default; the toggle is the control.
- An audio failure never touches the notification path. A broken or missing asset disables the sound for the session; a device error skips only that sound.

## Test plan

One build after the last review round. Recordings, plus before/after screenshots of Settings.
- Linux AppImage (headless Xorg, PulseAudio/PipeWire; `pactl list sink-inputs` shows a stream only while sounding):
  - Toggle off (default): silent, as master.
  - Toggle on:
    - A message to a non-open chat sounds.
    - Another message 1 s later is silent; one 4 s later sounds.
    - Messages to 10 chats in 1 s: one sound.
    - A contact request sounds.
    - Muted chat: silent. Focused open chat: silent.
    - While recording a voice message, in a call, or while a call rings: silent, and the recording is not interrupted.
    - No audio device: no crash, and the log is rate-limited. Sound returns once the device is back.
- macOS: by you, or skipped and reported as skipped.
- Windows (fork-CI MSI):
  - V1.
  - Toggle off: same sound behaviour as master.
  - Toggle on: the same checks as Linux.
  - With the Windows click plan: SimpleX off in Windows Settings → no sound.

## Decisions

- **D1. Sound asset.** DECIDED: a CC0 sound, with candidates offered for choosing by ear before implementation.
- **D2. Windows.** DECIDED: sound on Windows too, with the same mechanism. Delivery is gated by V1 (see Design §5).
- **D3. Rate limit.** DECIDED: one global 3 s minimum, no per-chat limit.
- **D4. Do Not Disturb.** DECIDED: accepted for v1 and documented; the in-app toggle is the control. Follow-up: OS-native sound. Also documented: a notification daemon configured to play its own sound (KDE, Snarl/Growl) can add a second sound; that is the user's OS configuration.
- **D5. Default.** DECIDED: **off**.
