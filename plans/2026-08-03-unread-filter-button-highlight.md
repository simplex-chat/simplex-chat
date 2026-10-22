# Chat list: highlight the unread filter button while the profile has unread

## Problem

The unread filter button next to the chat list search field looks the same whether or not anything is unread, so it gives no hint whether switching the filter on would show anything.

## Change

Highlight the button while the active profile's unread count (`UserInfo.unreadCount`, the number the profile picker shows as the badge of other profiles) is above zero. Three states:

- Android/desktop (`ToggleFilterEnabledButton`): grey icon; accent icon on a faint accent pill with a faint accent ring (has unread); solid accent pill (filter on, unchanged).
- iOS (`toggleFilterButton`): `line.3.horizontal.decrease` grey; `line.3.horizontal.decrease.circle` in the accent colour, at the filter-on size (has unread); `…circle.fill` (filter on, unchanged).

Product docs: one line each in `apps/{ios,multiplatform}/product/views/chat-list.md`.

## Depends on #7457

The client maintains the profile count by a different rule than the core seeds it with, so on master it drifts (e.g. a message in a muted chat adds 1, though the seed leaves muted chats out). #7457 maintains it by the seed's rule for main-conversation items; this PR reads it, so it should be merged after #7457.

## Alternatives considered

- Follow the filter's own rule (`!chatDeleted && !contactCard && unreadTag` over the chat list; this PR's first version): matched the filter's rule (before the change stacked on #7457, which adds support chats needing attention to the filter), including chats marked unread by hand. Not chosen: the button follows the same number as the profile badge and needs no scan of the chat list.
- Count or any chat marked unread: closes the marked-unread gap but mixes two rules.
- Show the count on the button: the active profile's count is not shown on its own anywhere today, and a number next to the search field adds clutter for a yes/no hint.

## Known differences from the filter

- A chat marked unread by hand is listed by the filter but does not light the button (marking unread does not change the count).
- Member support chats ("Chat with admins", and moderators' chats with members): the count loaded from the core includes their unread items, but the client does not keep it in step with them (also with #7457), so the button can stay lit after they are read or not light for new ones, and on iOS reading them can turn it grey while other chats are unread, until the count is next reloaded from the core (e.g. restart, profile switch, opening the profile picker on iOS; with #7457 also on Android/desktop, and returning to the app on Android and iOS).
- Chats outside the 5000 most recent (the chat list limit) are counted, but the filter cannot list them.
- With the filter change stacked on #7457, a group whose only support-chat signal is, for a moderator, a member pending approval or review is listed by the filter but does not light the button: the count counts items, not pending members.

## Testing

Desktop AppImage, master vs this PR, with peers sending messages: the button lights when a message arrives in an unmuted chat and clears when the chat is read; filter on/off, the "No unread chats" state, search text, list chips, a second profile, the filter-on pill unchanged in light and dark theme; the muted-chat, "Mark unread" and restart cases recorded. Android APK (Waydroid), master vs this PR: highlight on a new message, cleared after reading, filter on/off, dark theme. iOS: not built here.
