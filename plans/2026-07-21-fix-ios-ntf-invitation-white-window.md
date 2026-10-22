# iOS: blank window after a notification tap switches profile

## Problem

Reported on iOS (v6.5, v7 beta): with more than one profile and a chat open, tapping a contact-request notification ("X wants to connect!") of another profile shows a blank screen instead of that profile's chat list.

`processNotificationResponse` (`NtfManager.swift`) switches the active user (`changeActiveUser` → `getUserChatData` → `ChatModel.updateChats`), then opens a chat only when the notification has a `targetContentIdentifier`. A contact-request notification has none (`SimpleXChat/Notifications.swift`, `createContactRequestNtf`), so nothing else changes `chatModel.chatId`.

`updateChats` replaces `chats` but keeps `chatId`, which still names the previous profile's chat. The chat view stays pushed while `chatId != nil` (`ChatListView.swift`, `isActive` binding), and its destination is empty when `getChat(chatId)` is nil — the blank screen.

The same happens for every notification of another profile that opens no chat, e.g. a call notification tapped on its body, the "New contact request" and "New contact:" connection events, and the extension's "New messages" summary.

Android and desktop are not affected: Kotlin's `ChatsContext.updateChats` clears a `chatId` that is no longer in `chats` ("it was deleted in background after apiGetChats call"), and `ChatView` closes when its chat is missing.

## Fix

Do the same in iOS `ChatModel.updateChats`: after replacing `chats`, clear `chatId` if that chat is not in the new list. The chat view is popped and the user lands on the switched profile's chat list, as on Android and desktop.

`updateChats` callers: the profile switch and other chat-list loads (`getUserChatData` — also at start, after re-opening the database, after a desktop session and after creating a profile — and `getUserChatDataAsync`), returning to the foreground (`SimpleXApp.updateChats`), the reload after changing message TTL (`DatabaseView`), and `updateChats([])` when there is no user and in `LocalAuthView` (after it already cleared `chatId`). The profile picker in a prepared chat (`ContextProfilePickerView`) moves that chat to the new profile first and passes `keepingChatId`, so the chat is in the new list and stays open. In each, a `chatId` missing from the new list rendered the empty view until an event re-added that chat; now the chat closes. iOS loads up to the 5000 most recent chats (core `maxChats`); an open chat outside that window already rendered empty before this change, and now closes.

## Alternatives considered

- Clear `chatId` right after `changeActiveUser` in `processNotificationResponse` (+1 line): fixes the notification paths only; other reloads that can drop the open chat (e.g. a chat outside the 5000 most recent) still leave the empty view, and iOS keeps differing from Kotlin.
- Hide the symptom in the UI (`isActive` = `chatId != nil && getChat(chatId) != nil`): leaves `chatId` stale, and it is read elsewhere (e.g. `presentationOptions` suppresses the banner for the "open" chat).
- Show the accept/reject dialog on a contact-request tap (this PR's first version): since v6.4 a request is created together with a contact and appears in the chat list as that contact (with Accept/Reject in the chat), not as a `.contactRequest` row, so the dialog's lookup by the notification's `"<@N"` id only finds requests left from older versions. Dropped.
- Open the request's contact chat on tap: a UX change for both platforms (Kotlin doesn't do it either); out of scope.

## Out of scope

- The notification's "Accept" action looks up the same `"<@N"` chat, so on iOS it does nothing for requests created since v6.4 (Kotlin accepts by request id).
- Neither platform opens the request's chat on a tap.

## Testing

No iOS build here; verified by reading the code. Device checks for a maintainer: profiles A and B, B with an address and a contact (on another device), a third profile sending a request to B's address, a chat open in A, then tap (1) a contact-request notification for B, (2) a call notification for B on its body, (3) a message notification for B — master: blank screen in (1) and (2), PR: B's chat list in (1) and (2), B's chat in (3). In (3) `chatId` now goes from A's chat to nil (A's chat is popped) and about 250 ms later to B's chat (pushed), as on Android; master kept the screen pushed, blank for those 250 ms. Check it lands in B's chat on iOS 15 and 16+ (the push starts while the pop animates); if not, fall back to clearing `chatId` in `processNotificationResponse` only for notifications without a target chat. (2) needs CallKit off and the app in the background. Regression: a same-profile notification tap, the profile picker in a chat (`keepingChatId`), returning to the foreground after the app was suspended, with a chat open (and with a group member's not yet used contact opened via Message in member info: master blank, PR closes it).
