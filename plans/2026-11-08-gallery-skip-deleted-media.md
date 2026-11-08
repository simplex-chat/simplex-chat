# Fullscreen viewer: skip media of messages marked deleted

## Problem

In the fullscreen media viewer, swiping to the next or previous media shows images and videos of messages that the chat shows as "marked deleted", "moderated by X", "blocked" or "blocked by admin" (messages deleted by the sender for everyone, deleted by a moderator, or sent by a blocked member, when full deletion is off).

## Cause

The core keeps the content and the file of a message marked deleted, and sets `itemDeleted`. The chat hides such a message unless the user reveals it. The viewer selects neighbouring media by content type and downloaded file only:

- Android: `canShowMedia` in `providerForGallery` (ChatView.kt).
- iOS: `chatItemImage` in FullScreenMediaView.swift, the map passed to `nextChatItemData`.

## Fix

- Android: `providerForGallery` collects the ids of the items marked deleted when the viewer opens, except the opened item, and `canShowMedia` excludes them.
- iOS: `chatItemImage` requires `itemDeleted` to be absent.

```mermaid
flowchart LR
  Open[viewer opens on item X] --> Hidden[hiddenItemIds = items marked deleted, except X]
  Swipe[swipe] --> Item["item(): walk from the anchor"]
  Hidden --> Filter[canShowMedia: id not in hiddenItemIds, image or video, file loaded]
  Item --> Filter
  Filter --> Page[page shows the item]
```

## Scope

- Android and iOS: the platforms where the viewer changes media by swiping.
- Desktop: the same Kotlin filter; its viewer shows only the opened image or video.
- A revealed message opens in the viewer. On Android it stays reachable by swiping until the viewer closes; on iOS swiping back to it skips it.
- Media marked deleted while the viewer is open: on Android it stays reachable until the viewer closes, as before; on iOS swiping skips it.
