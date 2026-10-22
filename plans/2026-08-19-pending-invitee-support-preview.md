# Chat list: show a pending invitee's support messages in the group's preview

## Problem

A member waiting to be admitted (pending approval or review) receives no main-scope items, so #5909 made the group's chat-list row show their "Chat with admins" items. On top of #7072 (which added `ChatInfo.inMainChatList` and uses it when an item is added), the other paths still miss the row, on Android/desktop and iOS:

- Received edits, status changes and "marked deleted" of a support item never update the row: `upsertChatItem` only updates main-scope items.
- Full deletes never update the row, and deleting an unread support item leaves the row's unread count one too high: `removeChatItem` has the same check.
- On Android/desktop the invitee's own edits reach only the open "Chat with admins" view: `ComposeView` updates the active context only (#7072's `addSentChatItem` covers sending, not edits).
- Local events replace a message in the preview, because the newer timestamp wins: the "end-to-end encrypted" info and "member connected" events created when the invitee (already pending) connects to the host replace the welcome message; the pending-review event that follows, and "member connected" when each moderator connects later, replace a support message; and a message carrying the server timestamp (e.g. the directory's captcha with the `knocking` option) can be older than these and then does not replace them. The row falls back to "reviewed by admins", because these events have no message content.
- Media without a caption (e.g. the directory's image captcha) shows the "reviewed by admins" status as the text next to the media, because the row shows the status whenever the item has no text.

## Fix

- `upsertChatItem` and `removeChatItem` (Kotlin and iOS) update the row when `inMainChatList`; only a main-scope chat may still be added to the list. iOS gets the same `inMainChatList` property as Kotlin.
- `ChatModel.upsertSentChatItem` (Kotlin) also updates the main list when the active context is a support chat shown there, like #7072's `addSentChatItem`; `ComposeView` uses it for edits.
- `previewItem` (extracted from `addChatItem` on both platforms) chooses the row's item: for a pending invitee the onboarding events (pending review, "end-to-end encrypted" info, "member connected") do not replace a message, and a message replaces them, whatever their timestamps; otherwise (including removal, leaving and group deletion events) the newer item wins, as before.
- The preview row shows the message instead of the status when it has text or, with previews enabled, is content shown without text (media without a caption).
- iOS `upsertChatItem` replaces the preview with an item not loaded in the open chat only for main-scope items, so a support item cannot take the preview while the main chat is open.

For chats that are not pending, `inMainChatList` equals "main scope", so nothing changes for them. Unread counters stay in the primary context only (existing gates).

## Depends on #7072

This PR is stacked on #7072 and uses its `inMainChatList` and unread gate (`upsertSentChatItem` follows its `addSentChatItem`); merge #7072 first.

## Alternatives considered

- Newest item always wins (as before): the local onboarding events hide the welcome message and can hide the directory captcha.
- A message is never replaced by any content-less item: the removal, leaving and group deletion events would then not show.
- Only the pending-review event is special: the connection events arrive first and still replace the welcome message.
- Mirror sent edits inside `ChatsContext` instead of from the caller: the main list would then also handle every received update a second time; the caller-side mirror keeps the same structure as #7072's `addSentChatItem`.
- Make the core's chat-list query include a pending member's support items, so the preview also survives a restart: a core change (query plans, Android lib); a possible follow-up, not a replacement.
- Splitting `addChatItem` into three functions (the PR's first version): same behaviour, larger diff; only `previewItem` is extracted, as on iOS.

## Not included

- After a restart the row falls back to the main-scope preview (core query, see above).
- On Android/desktop, reading support messages does not clear the group row's unread badge until the group is reopened; on iOS reading subtracts the read count from the row's badge.
- After a restart or chat-list reload, the row's unread count is the core's main-scope count, so a later full delete of a support item that was already unread before the reload undercounts it by one (on iOS it can go below zero; Kotlin floors it at 0). This becomes reachable with this PR: before, deletes of support items did not touch the row.
- Separate bug: removing a member together with their messages does not update an open "Chat with admins".

## Testing

Desktop AppImages of #7072 and this PR, with a CLI peer owning a group with a welcome message, member admission review and full delete: a pending member's row keeps the welcome message and shows the owner's support messages; received edit and delete update the row and its unread badge; own edit and delete; a file without caption; previews off. Regression: an accepted member, a support chat when not pending, a direct chat, main-group unread counts, restart. iOS: not built here.
