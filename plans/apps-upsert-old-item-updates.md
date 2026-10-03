# Apps: an update of an earlier message is shown as a new message

## Problem

When the core reports an update of a chat item (`ChatItemUpdated`, file status events, delivery statuses, deletions that keep the item), both apps pass it to `upsertChatItem`. If the item is not among the items loaded in the open chat, `upsertChatItem` adds it at the bottom of the chat and reports it as added:

- Kotlin `ChatModel.upsertChatItem`: `addToChatItems(cItem)`, `itemAdded = true`; `chatItemUpdateNotify` then shows a notification (`ntfManager.notifyMessageReceived`), subject to `allowedToShowNotification()` or the chat not being open.
- iOS `ChatModel.upsertChatItem` / `_upsertChatItem`: inserts the item at the bottom, and for the open chat also replaces the chat preview with it; `chatItemSimpleUpdate` then adds a notification.

So an update of an old message, for example an edit, a moderation, a file status change, or the "file was sent before you connected to the sender" marker from #7638, makes that old message appear at the bottom of the open chat (until it is reopened), become the chat preview on iOS, and show a notification with its old text.

## Why the item is added at all

`ChatItemUpdated` can also report a new item: when an update arrives for a message the user never received (for example a live message whose first message was not received, or a message deleted locally), the core creates the item and reports it with `ChatItemUpdated` (`messageUpdate` and `groupMessageUpdate` in `Subscriber.hs`). Adding and notifying is correct for such items.

## Change

Chat item IDs only grow, so an item created by an update is newer than every other item in the chat, and an update of an earlier message is not. `upsertChatItem` on both platforms adds an item that is not loaded only if its ID is greater than both the chat preview's item ID and every loaded item's ID; otherwise the update is not applied to the loaded items, and the item is shown with its current state when that part of the chat is loaded. On iOS the open chat's preview is replaced by an item that is not loaded only if it is newer than the preview.

`addChatItem` (new items) is unchanged: on iOS it calls `_upsertChatItem` with `add: true`.

Not changed:

- A chat that is not in the chat list is still added with the item, as before: without its last item the app cannot tell whether the item is new.
- Android and desktop still notify for every `ChatItemUpdated` of a user that is not active (iOS does not).
- Live message placeholders have negative IDs (`-2`) and do not affect the comparison.
