# Fix: iOS notification for a chat invitation opens a white window instead of opening the request

Date: 2026-07-21
Branch: `nd/fix-ios-ntf-invitation-white-window`
Files changed:
- `apps/ios/Shared/Model/NtfManager.swift`
- `apps/ios/Shared/Model/ChatModel.swift`
- `apps/ios/Shared/Views/ChatList/ChatListView.swift`
- `apps/ios/Shared/Views/ChatList/ChatListNavLink.swift`

## Symptom

Reported on iOS, both v6.5 and v7 beta. With more than one user profile, tapping a
"chat invitation" (contact request, "X wants to connect!") notification that belongs to a
profile other than the active one shows a blank/white window instead of switching to that
profile and letting the user act on the request.

## Root cause (grounded in code)

`processNotificationResponse` (`NtfManager.swift:54-88`) does two independent things on a tap:

1. If the notification's `userInfo["userId"]` differs from the active user, it switches
   profile: `changeActiveUser(userId, viewPwd: nil)`.
2. It opens a chat only in the final `else` branch, and only when
   `content.targetContentIdentifier` is non-nil.

Two facts combine to produce the bug:

- A contact-request notification is created with `targetContentIdentifier: nil`
  (`SimpleXChat/Notifications.swift:39`). Its chat id lives in `userInfo["chatId"]` and was
  consulted only by the "Accept"-button branch (`action == ntfActionAcceptContact`),
  never on a plain body tap (default action). So none of the open-chat branches ran.
- `changeActiveUser` -> `getUserChatData` -> `updateChats` replaces the `chats` array but
  never resets `chatModel.chatId`.

Navigation is driven purely by `chatModel.chatId != nil` (`ChatListView.swift`). So when a
chat is open in the previous profile and the user taps an invitation notification for another
profile, the profile is switched underneath a ChatView still bound to the old profile's
`chatId` (now absent from the new `chats`), and that view renders blank — the "white window".
Message notifications avoid this because their `else` branch calls
`loadOpenChat(targetContentIdentifier)`, which resets `chatId` to a valid chat.

### Why a contact request cannot just be "opened" like a message

Unlike a message chat, a contact request has **no openable chat view**. The notification
carries the legacy contact-request chat id (`contactRequest.id` = `"<@..."`,
`ChatTypes.swift:331`), and that `ChatInfo.contactRequest` row is not navigable via
`chatModel.chatId` — it renders as a `ContactRequestView` with an `onTapGesture` that shows an
accept/reject confirmation dialog (`ChatListNavLink.swift`). Setting `chatId` to it would
render blank. So the fix cannot open a chat; it has to surface that same dialog.

## Fix

Two parts, both scoped to the notification path:

1. **Avoid the blank window.** When a notification tap switches the active user, clear the
   stale open chat so the UI lands on the switched profile's chat list:

   ```swift
   changeActiveUser(userId, viewPwd: nil)
   chatModel.chatId = nil
   ```

2. **Open the request.** A plain body tap (default action) on a contact-request notification
   now surfaces the accept/reject dialog, instead of doing nothing:

   ```swift
   } else if action == UNNotificationDefaultActionIdentifier,
             case .contactRequest = chatModel.getChat(chatId)?.chatInfo {
       chatModel.chatId = nil
       chatModel.showingContactRequest = NTFContactRequest(chatId: chatId)
   }
   ```

   - `ChatModel` gains `@Published var showingContactRequest: NTFContactRequest?`.
   - `ChatListView` presents a model-driven `.confirmationDialog` bound to it.
   - The dialog buttons (Accept / Accept incognito / Reject) are extracted into a shared
     `contactRequestDialogButtons(_:)` view builder, reused by the existing chat-list row so
     the two dialogs cannot drift apart.
   - The `getChat` guard fires only when the request is already loaded — which
     `changeActiveUser` guarantees, because it loads the new profile's chats synchronously —
     so the dialog always has buttons (no empty Cancel-only dialog). The `Accept`-button
     branch (`ntfActionAcceptContact`) is unchanged and still auto-accepts.

This also fixes the same-profile case: previously a plain tap on a contact-request
notification for the *active* profile did nothing either; it now surfaces the dialog too.

## Why this is minimal and safe

- Scoped to the notification handler; `changeActiveUser` is untouched, so the `keepingChatId`
  flows (e.g. `ContextProfilePickerView`) are unaffected.
- No control-flow regression: every contact-request notification now enters the first `if`,
  but nothing is lost — contact requests always have `targetContentIdentifier == nil` and
  never match `ntfCallAction`, so the old fall-through `else` was already a no-op for them.
  Dismiss and other actions remain no-ops as before.
- Message notifications still open correctly: their branch re-sets `chatId` via
  `loadOpenChat` immediately after.
- No new/removed translation keys — the dialog reuses the chat-list row's existing strings.

## Cross-platform status

- iOS: buggy -> fixed.
- Android/Desktop: not affected. The shared `openChatAction`
  (`common/.../platform/NtfManager.kt:59-72`) switches profile, sets `clearOverlays = true`,
  and only opens `Direct`/`Group` chats, so an invitation falls through to the chat list.

## Review

Adversarially reviewed (control-flow, compile/type, SwiftUI presentation, threading, races)
across independent passes — no bugs, regressions, compile errors, or races found. The one
raised nit (a possible empty dialog when the request is not loaded) is eliminated by the
`getChat` guard at set-time.

## Verification note

Not runtime-verified here: building/running the iOS app requires Xcode (macOS), unavailable
in this Linux environment. Repro to confirm on device: with two profiles, open a chat in
profile A, then tap a contact-request notification for profile B — before the fix it shows a
blank chat; after, it lands on profile B's chat list with the accept/reject dialog shown.
