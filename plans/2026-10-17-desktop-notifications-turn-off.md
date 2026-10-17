# Desktop: turn off notifications without muting chats

Status: draft rev 9 (after review round 6). All decisions taken 2026-10-17. Base: `origin/master` a932c8298. Paths are relative to `apps/multiplatform`.
Related plans: [sound](2026-10-17-desktop-notification-sound.md), [Windows click](2026-10-17-windows-notification-click.md). Research notes: `~/ntf-research/*.md`.

## Problem

On desktop, the only way to stop notifications is to mute chats. Mute is a per-chat setting stored in the chat database: it shows in the chat list, and it changes which unread counts the app tracks for inactive profiles. A user who wants no system notifications on this computer must mute everything, then later unmute everything.

Reports:
- #5009 (open): a full desktop off switch; the maintainer reply was "as a workaround you can mute all chats".
- #7116 (open): a flood of notifications at startup.

## Facts

- FACT [`SettingsView.kt:124-126`]: the Settings → Notifications row is shown only on `AppPlatform.ANDROID`.
- FACT [`NotificationsSettingsView.kt:37-58`]: `NotificationsSettingsLayout` already gates its only row with `appPlatform == AppPlatform.ANDROID` (`:47`), so desktop rows fit into the same screen.
- FACT [`PrivacySettings.kt:173-195`]: the only desktop notification setting today is Privacy → Notifications → "Show preview".
- FACT [`NtfManager.desktop.kt:97-98`]: message, contact-request, contact-connected and missed-call notifications (call invitations older than 3 minutes, `CallManager.kt:20-21`) all reach desktop `displayNotification`. Its first line is the gate `if (!user.showNotifications) return`.
- FACT [`CallManager.kt:15-18`, `NtfManager.desktop.kt:22-23`]: a fresh call invitation goes to `notifyCallInvitation`, a separate entry. The in-app call alert is created when `user.showNotifications` is true and the invitation is under 3 minutes old (`CallManager.kt:16-18`), and it rings on desktop (`IncomingCallAlertView.kt:31`).
- FACT [`ProcessedErrors.kt:37` → `NtfManager.desktop.kt:54`]: agent critical errors use `showMessage`. These are not chat notifications.
- FACT [`SimpleXAPI.kt:114, 332`]: Boolean preferences are `mkBoolPreference(key, default)`. The desktop-only key prefix is `Desktop…` (`:523-524`).
- FACT [`Appearance.desktop.kt:64`]: the template for a Boolean preference row is `SettingsPreferenceItem(icon = null, text, pref)`. It has an `enabled` parameter (`SettingsView.kt:423-434`).
- FACT [`DesktopTray.kt:50-81`, `ChatModel.kt:606-612`]: the tray dot and unread counts don't depend on notifications.
- FACT [`SimpleXAPI.kt:3073`]: when the desktop controls a mobile (remote host), the host's events also notify through the same path. The switch covers them too.

## Ideal final result

One switch on this computer, "Show notifications", that stops system notifications and changes nothing else. Unread counts, mute state, the tray dot and the in-app call alert stay as they are.

## Options

1. **A desktop Notifications screen** (recommended). Show Settings → Notifications on desktop as well, with a desktop-only toggle in `NotificationsSettingsLayout`. The sound plan adds its toggle there too.
   - Users look for notification settings under Settings → Notifications.
   - The screen is already platform-gated.
2. **A toggle in Privacy → Notifications**, next to "Show preview". A smaller diff, but an on/off switch is not privacy, and the sound toggle would have to live there too.
3. **A tray menu toggle.** The tray exists only with minimize-to-tray, and not at all on stock GNOME. Rejected as the only control.

## Design (option 1)

Narrative: when notifications are off on this computer, the desktop shows no system notification for chats. Everything else stays the same.

1. **`SimpleXAPI.kt` AppPreferences**: `val desktopNotifications = mkBoolPreference(SHARED_PREFS_DESKTOP_NOTIFICATIONS, true)`, declared with the other notification Booleans (after `canAskToEnableNotifications`, `:114`). Key: `"DesktopNotifications"`, next to `SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR`. Not in `hintPreferences` or `AppSettings`: the setting belongs to this computer.
2. **`NtfManager.desktop.kt` `displayNotification`**: extend the existing first-line gate to `if (!user.showNotifications || !appPreferences.desktopNotifications.get()) return`. Both premises sit in one visible place.
3. **`notifyCallInvitation`**: unchanged (D1). Fresh call invitations still show the system call notification.
4. **`SettingsView.kt:124`**: show the row on desktop too, keeping `disabled = stopped`, so it is unavailable while chat is stopped, as on Android. Desktop icon: `ic_notifications` / `ic_notifications_off`, from the new preference read through `remember { appPrefs.desktopNotifications.state }`, so the icon updates. Android keeps `ic_bolt` / `ic_bolt_off`, from `notificationsMode`.
5. **`NotificationsSettingsView.kt` `NotificationsSettingsLayout`**: under `appPlatform.isDesktop`, add `SettingsPreferenceItem(icon = null, stringResource(MR.strings.…), chatModel.controller.appPrefs.desktopNotifications)` (the global `chatModel`, since `NotificationsSettingsLayout` has no `chatModel` parameter, `:38-41`) and a `SectionTextFooter`. The footer says chats are not muted, unread counts still update, and incoming calls and app error notifications are still shown.
6. **`strings.xml`** base: title and footer.
7. **Docs**:
   - `spec/services/notifications.md` §4: the new gate, plus the line numbers it cites.
   - `spec/state.md`: the notification preferences table (`:336-345`).
   - `product/views/settings.md`: the desktop Notifications row.

Feature off (toggle on, the default) = today's app, plus a visible Settings → Notifications row on desktop.

## Interaction with the other plans

- Sound: it plays only when a notification is shown, so this switch silences it too. The sound toggle is disabled while notifications are off (`PreferenceToggle(disabled =)`, which greys the label too).
- Windows click: no notification means no toast or token. Fresh call invitations and app error messages still toast (D1).
- Landing order: this plan first, because it creates the desktop screen.

## Threat model

- The preference is local, set only through the UI. No peer input reaches it, and a peer can't observe it: receipts and unread state are unchanged.
- Risk: a user forgets that notifications are off and misses messages. The Settings row icon shows the state.

## Test plan

One build after the last review round (AppImage; plus the fork-CI MSI if built with the Windows plan):
- Toggle off: messages, a contact request, a contact-connected event and a missed call show no system notification, for the active profile, a background profile and a connected remote host.
  - Unread badges and the tray dot still update.
  - Mute state in the chat list is unchanged.
- Fresh call: the system call notification and the in-app ring still appear.
- Toggle on: identical to master (A/B screenshots).
- The setting survives a restart. Android is unchanged.

## Decisions

- **D1. Fresh call invitations.** DECIDED: still notified. Missed calls older than 3 minutes are plain notifications, and the switch suppresses them.
- **D2. Placement.** DECIDED: option 1, Settings → Notifications on desktop.
- **D3. Sound toggle while off.** DECIDED: shown, disabled.
