# Fullscreen viewer: skip media of messages marked deleted

## Problem

In the fullscreen media viewer, swiping to the next or previous media shows images and videos of messages that the chat shows as "marked deleted", "moderated by X", "blocked" or "blocked by admin" (messages deleted by the sender for everyone, deleted by a moderator, or sent by a blocked member, when full deletion is off).

## Cause

The core keeps the content and the file of a message marked deleted, and sets `itemDeleted`. The chat hides such a message unless the user reveals it. The viewer selects neighbouring media by content type and downloaded file only:

- Android: `canShowMedia` in `providerForGallery` (ChatView.kt).
- iOS: `chatItemImage` in FullScreenMediaView.swift, the map passed to `nextChatItemData`.

## Fix

Both filters also require `itemDeleted` to be absent. On Android:

- the opened item is exempt;
- `item()` counts the item it starts from without the filter.

On iOS, `nextChatItemData` already starts from the current item and does not filter it.

## Scope

- Android and iOS: the platforms where the viewer changes media by swiping.
- Desktop: the same Kotlin filter; its viewer shows only the opened image or video.
- A revealed message opens in the viewer. On Android it stays reachable by swiping until the viewer closes; on iOS swiping back to it skips it.
- Media marked deleted while the viewer is open stays shown while it is the item the viewer counts from: the page on screen, or, on Android after a swipe back to the opened position, the page settled on before it. On Android the opened media stays reachable until the viewer closes.
