# Items received while a chat loads

## Problem

A message received while its chat is being opened is missing from the open chat until the chat is reopened. The chat row may also show an unread count lower than the number of unread messages.

## Cause

`processLoadedChat` (`ChatItemsLoader.kt`) installs a page that `apiGetChat` read before the install.

`addChatItem` and the other item updates of `ChatsContext` (`ChatModel.kt`) change the open list only when `chatModel.chatId` is the item's chat (for member updates, also the channel being created). An update handled between the read and the install is lost:
- when the chat is opening, `chatId` is set in the install, so the update is not applied;
- when the chat is open, the update is applied to the list that the install replaces (Initial, Around at an item) or that was read before a main-thread dispatch (other paginations).

The Initial page also overwrites the row's stats with the stats it read.

## Fix

`processLoadedChat` runs on the main thread from reading the current list to the install, so the merging paginations (Before, After, Around to a quoted item, Last) keep every item the list held before the merge.

While a page loads, `ChatsContext` records the list part of each item update in every active `ItemUpdatesWhileLoading`:
- `addChatItem`;
- `upsertChatItem`, as an update of an item in the list;
- `updateChatItem`;
- `removeChatItem`;
- `removeMemberItems`;
- `clearChat`;
- `markChatItemsRead` of listed items;
- `upsertGroupMember`;
- `groupChatItemsDeleted` (`SimpleXAPI.kt`), with `recordItemUpdate` for an item that is not in the list.

Chat-list counter updates (row stats, total unread, reports) run once, outside the recorded part; the open chat's counters change with the list.

`recordItemUpdatesWhileLoading` starts a record before `apiGetChat` and stops it when the load ends:
- `apiLoadMessages`, for every pagination;
- `afterSetChatTTL` (`ChatInfoView.kt`).

The pages that replace the list (Initial, Around at an item) and contain the newest item (`afterTotal == 0`) re-run the recorded updates after the install, in order. Older pages are followed by After pagination, which loads the newer items.

After the re-run, the Initial page adds to the row's unread count and mentions the difference between the unread items of the list and of the page (`addUnreadChanges`).

## Scope

Android and desktop. iOS: separate pull request.
