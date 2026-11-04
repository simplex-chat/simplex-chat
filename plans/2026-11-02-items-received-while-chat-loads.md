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

`processLoadedChat` runs on the main thread from reading the current list to the install, so the merging paginations (Before, After, Around to a quoted item, Last) read a list that includes every update applied to it during the load.

While a page loads, `ChatsContext` records the list part of each item update in every active `ItemUpdatesWhileLoading`:
- `addChatItem`;
- `upsertChatItem` and `updateChatItem`, as an update of an item in the list that keeps its read status and is skipped when the item is already deleted;
- `removeChatItem`;
- `removeMemberItems`;
- `clearChat`;
- `markChatItemsRead` with item ids; a mark of the whole chat as read is not recorded;
- `upsertGroupMember`;
- `groupChatItemsDeleted` (`SimpleXAPI.kt`), with `recordItemUpdate` for an item that is not in the list.

Chat-list counter updates (row stats, total unread, reports) run once, outside the recorded part (the Initial page then sets the row's stats, below); the open chat's counters change with the list.

`recordItemUpdatesWhileLoading` starts a record before `apiGetChat` and stops it when the load ends:
- `apiLoadMessages`, for every pagination;
- `afterSetChatTTL` (`ChatInfoView.kt`).

The pages that replace the list (Initial, Around at an item) re-run the recorded additions and then the other recorded updates after the install, each in order, when they contain the newest item (`afterTotal == 0`) and belong to the current remote host. On an older page, items received during the load are loaded by After pagination; updates of its items are not re-run.

In the primary context, the Initial page sets the row's stats after the re-run: the page's stats plus the difference between the unread items (and mentions) of the list and of the page. A missing row is added with these stats.

## Scope

Android and desktop. iOS: separate pull request.
