# Items received while a chat loads

## Problem

A message received while its chat is being opened is missing from the open chat until the chat is reopened. The chat row may also show an unread count lower than the number of unread messages.

## Cause

`processLoadedChat` (`ChatItemsLoader.kt`) installs a list built before the main-thread block that installs it: the page that `apiGetChat` read (Initial, and Around when it opens at an item), or the other paginations' page merged with the items read at the start of `processLoadedChat`.

`addChatItem` (`ChatModel.kt`) adds a received item to the list of the open chat only when `chatModel.chatId` is the item's chat. An item handled between the read and the install is dropped:
- when the chat is opening, `chatId` is set in the install block, so the item is not added;
- when the chat is open, the item is added and then overwritten by the installed list.

`addChatItem` increments the row's unread count for that item, and the Initial page then overwrites the row's stats with the stats it read.

## Fix

`ChatsContext` keeps a list of `ItemsAddedWhileLoading` records; `addChatItem` appends each item passed to it, with its `ChatInfo`, to every active record on the main thread.

`recordItemsAddedWhileLoading` (`ChatItemsLoader.kt`) starts a record before `apiGetChat` and stops it after the page is installed or the load ends:
- `apiLoadMessages`, for every pagination;
- `afterSetChatTTL` (`ChatInfoView.kt`), which loads an Initial page without `apiLoadMessages`.

`processLoadedChat` takes the record:
- each install block ends with `applyItemsAddedWhileLoading`, which adds the recorded items that belong to the context's scope and are not in the list, in the same way as `addChatItem`;
- the Initial page overwrites the row's stats only when the record has no item of the loaded chat.

## Scope

Android and desktop. iOS: separate pull request.
