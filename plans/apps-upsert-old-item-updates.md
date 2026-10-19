# Apps: an update of an earlier message is shown as a new message

## Problem

When the core reports an update of a chat item (`ChatItemUpdated`, file status events, delivery statuses, deletions that keep the item), both apps pass it to `upsertChatItem`. If the item is not among the items loaded in the open chat, `upsertChatItem` adds it at the bottom of the chat and reports it as added:

- Kotlin `ChatModel.ChatsContext.upsertChatItem`: `addToChatItems(cItem)`, `itemAdded = true`; for `ChatItemUpdated`, `chatItemUpdateNotify` then shows a notification (`ntfManager.notifyMessageReceived`), subject to `allowedToShowNotification()` or the chat not being open.
- iOS `ChatModel.upsertChatItem` / `_upsertChatItem`: inserts the item at the bottom, and for the open chat also replaces the chat preview with it; for `ChatItemUpdated` and file events, `chatItemSimpleUpdate` then adds a notification.

So an update of an old message, for example an edit, a moderation, a file status change, a delivery status change, or the "file was sent before you connected to the sender" marker from #7638, makes that old message appear at the bottom of the open chat (until it is reopened) and become the chat preview on iOS. Edits and the #7638 marker (`ChatItemUpdated`) also show a notification with the item's text on both platforms; file status changes do so only on iOS (Kotlin `chatItemSimpleUpdate` never notifies); moderation, deletions and delivery statuses never notify.

## Why the item is added at all

`ChatItemUpdated` can also report a new item: when an update arrives for a message the user never received (for example a live message whose first message was not received, or a message deleted locally), the core creates the item and reports it with `ChatItemUpdated` (`messageUpdate` and `groupMessageUpdate` in `Subscriber.hs`). Adding and notifying is correct for such items.

## Change

Chat item IDs only grow, so an item created by an update is newer than every other item in the chat, and an update of an earlier message is not. `upsertChatItem` on both platforms adds an item that is not loaded only if:

- the newest items of the chat are loaded (iOS `ItemsModel.lastItemsLoaded`; Kotlin the same check of `chatState.splits`, written inline because `lastItemsLoaded` in `ChatView.kt` is private), and
- its ID is greater than both the chat preview's item ID and every loaded item's ID.

Otherwise the update is not applied to the loaded items, and the item is shown with its current state when that part of the chat is loaded. On iOS the open chat's preview is replaced by an item that is not loaded only by the same rule.

While the newest items are not loaded (the chat was opened at the first unread item, or after a jump to a quoted or found item), neither the loaded items nor the preview bound the IDs of the items that are not loaded: in groups the preview is the item with the latest timestamp, not the highest ID, and after the last item is deleted the preview is a placeholder with ID -1. Once a new item is added at the bottom, the newest items count as loaded again; that is safe, because every earlier item has a lower ID than the new item. The same check applies to member support chats and reports, which use their own loaded state.

Status of a sent message before the send response: the core can report a status change of a message the user has just sent (`ChatItemsStatusesUpdated`) before the app adds that message from the send response, and a message received in between (`NewChatItems`) has a higher ID; while the newest items are not loaded, no such message is needed. The rule above drops that status update. So when `upsertChatItem` drops an update of an own sent item and its status is not a delivery receipt (`SndRcvd`), it keeps the status (Kotlin `ChatsContext.sndStatusesBeforeAdd`, iOS `ItemsModel.sndStatusesBeforeAdd`), and `addChatItem` applies the kept status to the item it adds when `shouldKeepOldSndCIStatus` prefers it, the same rule `upsertChatItem` uses for a loaded item. The kept statuses are of one chat only: they are cleared when a status of another chat is kept, and each is removed when its item is added. Other kept statuses come from updates of own messages that are not loaded, for example an error or a warning after a retry, or a group message reaching more members; they stay until a status of another chat is kept. Receipts, the usual status change of an old message, are not kept.

`addChatItem` otherwise is unchanged: on iOS it calls `_upsertChatItem` with `add: true`.

Not changed:

- A chat that is not in the chat list is still added with the item, as before: without its last item the app cannot tell whether the item is new.
- Android and desktop still notify for every `ChatItemUpdated` of a user that is not active (iOS does not).
- Live message placeholders have negative IDs (`-2`) and do not affect the comparison.
- If the user sends a message after the core created an item from an update but before the app received that update, the created item is no longer newer than every loaded item, so it is shown only when the chat is reloaded, without a notification. An item created from an update while the newest items of the chat are not loaded is likewise shown when they are loaded, without a notification. Only the core reporting such items as new items (`NewChatItems`) would close this; that is a core change.
- In groups, items are loaded in the order of their timestamps (`item_ts`), not their IDs. A message received late, for example while catching up after being offline, when members' messages arrive in any order, can be placed above more than a page of messages with later timestamps, and still have a higher ID than every loaded item and the preview. An update of it is still added at the bottom while the newest items are loaded. Only the core change above would remove this.
- While the user writes a live message, its placeholder is the last item, so the newest items count as loaded, as they do for `ChatView`, and only the ID rule applies.
- In the chat list, the preview of a sent message shows the status from the send response, as before, until the next status change.
