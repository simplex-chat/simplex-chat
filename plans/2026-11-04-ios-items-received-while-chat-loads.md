# iOS: items received while a chat loads

## Problem

A message received while its chat is being opened is missing from the open chat until the chat is reopened.

## Cause

`apiLoadMessages` (`ChatItemsLoader.swift`) installs a page that `apiGetChat` read before the install.

`addChatItem` and the other item updates of `ChatModel` (`ChatModel.swift`) change the open list only when `ChatModel.chatId` is the item's chat (`getCIItemsModel`). An update handled between the read and the install is lost:
- when the chat is opening, `chatId` is set in the install (Around at an item), after it (`loadOpenChatNoWait`) or by the 250 ms navigation timeout (`loadOpenChat`), so the update is not applied;
- when the chat is open, the update is applied to the list that the install replaces (Initial, Around at an item) or that was read before a main-actor step (other paginations).

## Fix

`processLoadedChat` (`ChatItemsLoader.swift`) runs on the main actor in one step from reading the current list to the install, so the merging paginations (Before, After, Around to a quoted item, Last) read a list that includes every update applied to it during the load.

While a page loads, `ChatModel` records the list part of each item update in every active `ItemUpdatesWhileLoading`, as a closure of the loading `ItemsModel` and the loaded chat's id:
- `addChatItem`, as an addition of an item missing from the list;
- `upsertChatItem` and `updateChatItem`, as a replacement of an item in the list that keeps its read status and is skipped when the item is already deleted;
- `removeChatItem`;
- `removeMemberItems`;
- `clearChat`;
- `markChatItemsRead`;
- `groupChatItemsDeleted` (`SimpleXAPI.swift`), with `recordItemUpdate` for an item that is not in the list.

A recorded update resolves its `ItemsModel` with `getCIItemsModel` for the loaded chat's id (or compares the chat id and the `ItemsModel` for the updates that check `chatId`) and applies only to the loading `ItemsModel`. Counters (row stats, total unread, reports), notifications and voice playback stop run once, outside the recorded part; the open chat's counters (`chatState`) change with the list.

`apiLoadMessages` starts a record before `apiGetChat` and stops it on an error or in the main-actor step of the install, for every pagination.

The pages that replace the list (Initial, Around at an item) re-run the recorded additions and then the other recorded updates in the install step, each in order, when they contain the newest item (`afterTotal == 0`). On an older page, items received during the load are loaded by After pagination; updates of its items are not re-run.

The row's stats are not set from the page (only `updateChatInfo`, or `addChat` of a missing row); they are not changed.

## Scope

iOS. Android and desktop: https://github.com/simplex-chat/simplex-chat/pull/7995.
