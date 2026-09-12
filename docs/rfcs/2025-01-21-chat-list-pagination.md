# Introduce pagination to chat list

## Problem

As the number of chats grows, the current client-side filtering solution is inefficient, leading to performance issues and high memory consumption. Current loaded chats are capped to last 5K.

## Solution

Paginated API for chats will address this problem

## Side effects of this change

1. Quite a few alerts and conditions are dependent on chat list size, those will need to be reviewed individually for each case (ex: address card, one hand ui, privacy notice with lock…)
2. Active chat needs to be managed separately, regardless of chat pagination results
3. Add group members, Share list, new chat sheet are all dependent on full list, they will also need to handle pagination and chat model param will need to be reset when it doesn't match expected selection in these lists
4. In order to support chat tags, lib reply needs to include information currently computed in the UI layer (what filters should be available, initial unread counters …)
5. `removeWallpaperFilesFromAllChats` is a function that iterates chat list an removes the wallpapers from all chats, 1 by 1, this doesn’t seem to call the backend, we need to move this logic to the lib
6. Privacy settings does iterate the chat list to know if there are contact and group receipts overrides in order to show an alert, I would create a new api for this that just returns the number of overrides
