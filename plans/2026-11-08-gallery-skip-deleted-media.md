# Fullscreen viewer: skip media of messages marked deleted

## Problem

In the fullscreen media viewer, swiping to the next or previous media shows images and videos of messages that the chat shows as deleted: deleted by the sender for everyone (full deletion off), moderated, or from a blocked member.

## Cause

The core keeps the content and the file of a message marked deleted, and sets `itemDeleted`. The chat hides such a message unless the user reveals it. The viewer selects neighbouring media by content type and downloaded file only:

- Android: `canShowMedia` in `providerForGallery` (ChatView.kt).
- iOS: `chatItemImage` in FullScreenMediaView.swift, the map passed to `nextChatItemData`.

## Fix

Both filters also require `itemDeleted` to be absent. The media the user opened is not filtered, so a revealed message still opens.

## Scope

- Android and iOS: the platforms where the viewer changes media by swiping.
- Desktop: the same Kotlin filter; its viewer shows one media and does not change it.
- Media marked deleted while it is the page on screen stays shown until the user swipes away.
