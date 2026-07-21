# Fix: iOS notification for a chat invitation opens a white window instead of switching profile

Date: 2026-07-21
Branch: `nd/fix-ios-ntf-invitation-white-window`
File changed: `apps/ios/Shared/Model/NtfManager.swift`

## Symptom

Reported on iOS, both v6.5 and v7 beta. With more than one user profile, tapping a
"chat invitation" (contact request, "X wants to connect!") notification that belongs to a
profile other than the active one shows a blank/white window instead of switching to that
profile.

## Root cause (grounded in code)

`processNotificationResponse` (`NtfManager.swift:54-85`) does two independent things on a tap:

1. If the notification's `userInfo["userId"]` differs from the active user, it switches
   profile: `changeActiveUser(userId, viewPwd: nil)` (lines 59-63).
2. It opens a chat only in the final `else` branch, and only when
   `content.targetContentIdentifier` is non-nil (lines 77-84).

Two facts combine to produce the bug:

- A contact-request notification is created with `targetContentIdentifier: nil`
  (`SimpleXChat/Notifications.swift:39`). Its chat id lives in `userInfo["chatId"]` and is
  consulted only by the "Accept"-button branch (`action == ntfActionAcceptContact`,
  line 64), never on a plain body tap (default action). So none of the open-chat branches run.
- `changeActiveUser` -> `getUserChatData` -> `updateChats` (`ChatModel.swift:628-639`)
  replaces the `chats` array but never resets `chatModel.chatId`.

Navigation is driven purely by `chatModel.chatId != nil` (`ChatListView.swift:190-198`).
So when a chat is open in the previous profile and the user taps an invitation notification
for another profile, the profile is switched underneath a ChatView still bound to the old
profile's `chatId` (now absent from the new `chats`), and that view renders blank — the
"white window". Message notifications avoid this because their `else` branch calls
`loadOpenChat(targetContentIdentifier)`, which resets `chatId` to a valid chat.

This path is unique to notifications: the in-app profile switch (UserPicker) is only
reachable from the chat list, where `chatId` is already nil.

## Fix

When a notification tap switches the active user, clear the stale open chat so the UI lands
on the switched profile's chat list:

```swift
changeActiveUser(userId, viewPwd: nil)
chatModel.chatId = nil
```

## Why this is minimal and safe

- Scoped to the notification handler; `changeActiveUser` is untouched, so the `keepingChatId`
  flows (e.g. `ContextProfilePickerView`) are unaffected.
- Message notifications still open correctly: their branch re-sets `chatId` via
  `loadOpenChat` immediately after.
- Matches the reported expected behavior ("switch to the profile the notification came from")
  and aligns iOS with Android/Desktop.

## Cross-platform status

- iOS: buggy -> fixed.
- Android/Desktop: not affected. The shared `openChatAction`
  (`common/.../platform/NtfManager.kt:59-72`) switches profile, sets `clearOverlays = true`,
  and only opens `Direct`/`Group` chats, so an invitation falls through to the chat list.

## Verification note

Not runtime-verified here: building/running the iOS app requires Xcode (macOS), unavailable
in this Linux environment. Repro to confirm on device: with two profiles, open a chat in
profile A, then tap a contact-request notification for profile B — before the fix it shows a
blank chat; after, it lands on profile B's chat list.
