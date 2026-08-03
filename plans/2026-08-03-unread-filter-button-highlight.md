# Highlight the chat list filter button while any chat is unread

## Problem

The unread filter button in the chat list search bar looks the same whether or not there is anything to filter to. The only way to find out is to tap it — and if nothing is unread, the list empties and shows "No unread chats". There is no passive signal that unread chats exist.

User tags already solve this for themselves: a tag chip carries a `●` badge when its chats have unread (`ChatListView.kt:1250`, driven by `unreadTags`). The unread filter button had no equivalent.

## Fix

Tint the filter icon with the accent colour while any chat is unread. Same icon, same size — only the colour changes, so it stays visually distinct from the filter's active state (a filled accent pill on Android/desktop, a filled circle glyph at 22pt on iOS).

| Filter | Unread chats | Android / desktop | iOS |
|---|---|---|---|
| off | none | grey lines | grey lines |
| off | some | **accent lines** | **accent lines** |
| on | — | white lines on accent pill | accent filled circle |

## Why `Chat.unreadTag`

The highlight uses `unreadTag` — the same property `ActiveFilter.Unread` matches on (`ChatListView.kt:1479`, `ChatListView.swift:555`). So the button is highlighted exactly when the filter has something to show, and the two cannot drift: they are the same symbol, not two copies of one rule. It also inherits the right mute semantics for free — a muted chat counts only when manually marked unread, and a mentions-only chat only on unread mentions.

The alternative, `users[].unreadCount`, was rejected: `changeUnreadCounter(user:by:)` increments it unconditionally, so muted chats would light the button while the filter showed nothing.

## Reactivity

Both platforms already depend on exactly this signal to render the filtered list itself, so no new machinery is needed:

- Android/desktop — every unread change replaces the chat in the `SnapshotStateList` (`chats[i] = chat.copy(chatStats = …)`), so reading the list recomposes the button.
- iOS — `_updateChat` assigns back into `@Published chats`, and the debounced counter path mutates `@Published users` (a struct array); either fires `ChatModel.objectWillChange`. Note the `UnreadCollector` 1s debounce means the highlight clears a beat after reading a chat, as the filtered list already does.

## Scope

Not touched:
- The contacts-list filter button (`NewChatSheet.kt`, `NewChatMenuButton.swift`) — despite the `showUnreadAndFavorites` name it filters favourites, not unread, so an unread highlight there would be wrong.
- The `ic_filter_list` icon in the "No unread chats" empty state — it only renders while the filter is on and empty, so it correctly stays grey.

The change is additive: every pre-existing state renders the identical colour, and exactly one new state is introduced.
