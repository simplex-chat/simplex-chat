# Items received while a chat loads

## Problem

A message received while its chat is being opened is missing from the open chat until the chat is reopened. The chat row may also show an unread count lower than the number of unread messages.

## Cause

`processLoadedChat` (`ChatItemsLoader.kt`) installs a page that `apiGetChat` read before the install.

`addChatItem` and the other item updates of `ChatsContext` (`ChatModel.kt`) change the open list only when `chatModel.chatId` is the item's chat. An update handled between the read and the install is lost:
- when the chat is opening, `chatId` is set in the install, so the update is not applied;
- when the chat is open, the update is applied to the list that the install replaces (Initial, Around at an item) or that was read before a main-thread dispatch (other paginations).

The Initial page also overwrites the row's stats with the stats it read.

## Fix

`processLoadedChat` runs on the main thread from reading the current list to the install, so the merging paginations (Before, After, Around in the open chat, Last) read every update applied before.

While a page loads, `ChatsContext` records the list part of each item update in every active `ItemUpdatesWhileLoading`:
- `addChatItem`;
- `upsertChatItem`;
- `updateChatItem`;
- `removeChatItem`;
- `removeMemberItems`;
- `clearChat`;
- `markChatItemsRead`;
- `upsertGroupMember`;
- `groupChatItemsDeleted` (`SimpleXAPI.kt`), with `recordItemUpdate` for an item that is not in the list.

Counter updates run once, outside the recorded part.

`recordItemUpdatesWhileLoading` starts a record before `apiGetChat` and stops it when the load ends:
- `apiLoadMessages`, for every pagination;
- `afterSetChatTTL` (`ChatInfoView.kt`).

The pages that replace the list (Initial, Around at an item) re-run the recorded updates after the install, in order.

The Initial page keeps the row's stats when they changed while it loaded.

## Scope

Android and desktop. iOS: separate pull request.
