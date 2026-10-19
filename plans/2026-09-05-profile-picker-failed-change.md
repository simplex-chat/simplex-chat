# Profile pickers commit their state when the change failed

## Problem

Android/desktop, shared code:

1. New-chat profile picker (`NewChatView.kt`), "Incognito" tapped: the app-wide incognito default is set before `apiSetConnectionIncognito`. If the call fails (e.g. offline), it stays on, so the next one-time link or prepared-chat connection is incognito.
2. Same picker, another profile tapped: the incognito default is cleared before `apiChangeConnectionUser`, and the picker closes even if the call failed, as if the change worked.
3. Profile picker above the compose bar in a prepared (not yet connected) chat (`ComposeContextProfilePickerView.kt`): the app switches the active profile even when moving the chat to it failed, so the chat closes and the user lands on the other profile.

iOS, new-chat profile picker (`NewChatView.swift`): `apiChangeConnectionUser` returns nil when the user cancels the Retry alert (offline); the handler had no branch for that, so the "switching" status stayed and the picker remained behind its spinner.

## Fix

- Android/desktop: commit the incognito default, close the picker, and switch the active profile only after the call succeeded. The APIs show their own error alerts. Success paths are unchanged.
- iOS (separate commit): when the call returns nil, reset the switching status and the selected profile, as the error path does, without an alert.

## Alternatives considered

- Roll the incognito default back on failure: races with other writers of the setting.
- Make the Kotlin APIs throw, as iOS mostly does: changes four API contracts for a UI-only fix.
- `else return@withApi` in each branch instead of the `chatMoved` flag: equivalent, cosmetic.

## Not included

- When switching the active profile itself throws, the error escapes both Kotlin pickers, so their "Error switching profile" alert never appears (pre-existing).
- iOS compose-bar picker and iOS incognito toggle: not affected (the APIs throw, and the existing `catch` already rolls back).

## Order with #7329

#7329 moves the new-chat picker's profile-row handler into `selectProfile`; it should land after this PR, with the same guards applied there.

## Testing

Desktop AppImage, master vs this PR, offline via "Use SOCKS proxy" with nothing listening: (1) Incognito in the new-chat picker — master leaves the incognito default on, PR does not; (2) another profile — master closes the picker, PR keeps it open on the current profile; (3) prepared chat whose contact was deleted, pick another profile — master switches profile and closes the chat, PR stays. Regression: the same three picker actions online, on a normal prepared chat, succeed as before; the share sheet's profile picker on Android. iOS: not built here.
