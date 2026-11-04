# iOS: items received while a chat loads

## Problem

A message received while its chat is being opened is missing from the open chat until the chat is reopened.

## Cause

A page read by `apiGetChat` is installed by `apiLoadMessages` (`ChatItemsLoader.swift`) after the read.

The open list is changed by `addChatItem` and the other item updates of `ChatModel` (`ChatModel.swift`) only when `ChatModel.chatId` is the item's chat (`getCIItemsModel`). An update handled between the read and the install is lost:
- while the chat is opening, `chatId` is set in the install (Around at an item), after it (`loadOpenChatNoWait`) or by the 250 ms navigation timer (`loadOpenChat`), so the update is not applied;
- while the chat is open, the update is applied to the list that is replaced by the install (Initial, Around at an item) or that was read before a main-actor step (other paginations).

## Fix

The current list is read and the page is installed by `processLoadedChat` (`ChatItemsLoader.swift`) in one main-actor step; the merging paginations (Before, After, Around to a quoted item, Last) read a list that includes every update applied to it during the load.

While a page loads, the list part of each item update is recorded by `ChatModel` in every active `ItemUpdatesWhileLoading`, as a closure of the loading `ItemsModel` and the loaded chat's id:
- `addChatItem`, as an addition of an item missing from the list;
- `upsertChatItem` and `updateChatItem`, as a replacement of an item in the list; the read and sent statuses of the listed item are kept, and the update is skipped when the listed item is deleted and the new item is not;
- `removeChatItem`;
- `removeMemberItems`;
- `clearChat`;
- `markChatItemsRead`;
- `groupChatItemsDeleted` (`SimpleXAPI.swift`), with `recordItemUpdate` for an item missing from the list.

The `ItemsModel` of a recorded update is resolved by `getCIItemsModel` for the loaded chat's id, or by a comparison of the chat id and the `ItemsModel` for the updates that check `chatId`; the update is applied only to the loading `ItemsModel`. Counters (row stats, total unread, reports), notifications and the voice playback stop are run once, outside the recorded part; the open chat's counters (`chatState`) are changed with the list.

A record is started by `apiLoadMessages` before `apiGetChat`, for every pagination.

In the install step of a page that replaces the list (Initial, Around at an item) and contains the newest item (`afterTotal == 0`), the recorded additions and then the other recorded updates are re-run, each in order. For an older page, items received during the load are loaded by After pagination; the recorded updates are discarded.

`ItemsModel.openingChatId` is set by `loadOpenChat` from the start of its navigation timer until `chatId` is set, and by `loadOpenChatNoWait` from its start until its completion step. When a replacing page of that chat is installed while `chatId` is not the loaded chat, the record is kept as `ItemsModel.openingItemUpdates`; each update recorded after the install is applied at once to that `ItemsModel` for the loaded chat. The record is stopped:
- in the install step, when it is not kept;
- in the step that sets `chatId`: the navigation timer of `loadOpenChat`, the completion step of `loadOpenChatNoWait`;
- at the start of the next load of the same `ItemsModel`, and when a new opening record of it is kept;
- on an `apiGetChat` error.

A missing row is added with the page's stats (`addChat`); an existing row receives only the page's chat info (`updateChatInfo`). The row's stats are left unchanged.

## Scope

iOS. Android and desktop: https://github.com/simplex-chat/simplex-chat/pull/7995.
