# Profile unread count

## Problem

The core seeds each profile's unread count in `getUsersInfo` (`Store/Profiles.hs`). It counts `RcvNew` items by the chat's notification setting:
- every item in an unmuted chat;
- only mentions in a mentions-only group;
- nothing in a muted chat.

After the seed the client keeps the active profile's count up to date with its own deltas, and those used a different rule. The count drifted until the next seed (e.g. a profile switch, a restart, or opening the profile picker — always on iOS, on Android and desktop only when another field of the users differed):
- A new item in a muted or mentions-only chat added 1.
- On Android and desktop, mark read, clear and single-item decrements subtracted every unread item, whatever the chat's setting. On iOS, mark read and clear followed the mentions-only setting but subtracted every unread item for muted chats, and single-item reads subtracted the raw delta. After a restart that pushed the count below zero for muted chats.
- On Android and desktop `decreaseCounterInPrimaryContext` never decremented `unreadMentions`, so a mentions-only chat's mentions stayed unread in its stats.
- Deleting a chat left its unread in the count.
- Muting or unmuting a chat with unread items did not move the count.
- iOS: deleting a contact in "keep conversation" mode re-added the chat without its stats.
- Nothing re-read the count when the chat list was re-read: on resume, or after setting a chat item TTL (bulk deletion sends no per-item events).

Who sees the active profile's count: the desktop tray dot, the iOS app badge, and the Unread filter button of #7340. The profile picker shows only the other profiles.

Non-active profiles were already kept by the seed's rule (`ntfsEnabled`).

## Fix

- `Chat.userUnreadCount`: what a chat contributes by the seed's rule (`unreadCount` for All, `unreadMentions` for Mentions, 0 otherwise). iOS `totalUnreadCountForAllUsers` already computed this inline and now calls it.
- These sites read the contribution before and after, and the count moves by the difference (`changeUserUnreadCounter`): new item, mark read, clear, single-item decrement, chat removal, a settings change made in the app (mute/unmute), and the iOS "keep conversation" re-add. For a settings change both platforms read the listed chat, with the settings it had before the API call, because an item event processed during the call can already carry the new settings. On iOS `updateChatSettings` reads `ChatModel.getChat`, not the chat object it was passed, which is stale after a resume rebuilds the list; the iOS "keep conversation" re-add likewise takes the listed chat's stats.
- `decreaseCounterInPrimaryContext` also decrements `unreadMentions` when the item was a mention.
- The counts are re-read from the core with the chat list: on resume (Android and iOS) and after setting a chat item TTL (the active profile). Reading users is best effort, so a failure keeps the chats already fetched. The Kotlin picker now also applies count-only changes when it re-reads users; before, it replaced them only when another field differed (e.g. with hidden profiles), while iOS always did.
- Opening a chat on Android and desktop replaces its stats with the server's. When the chat was already listed and its contribution drops, the count drops by the difference; it never moves up here, because a chat from outside the 5000-chat window can be listed with empty stats while the seed already counted its unread items. A chat the load adds comes with the server's stats, which the seed already counted. A content-filtered initial load of a group returns zero stats when no unread item matches the filter (otherwise the full counts), so a load with a content filter does not move the count (`statsFiltered`). A search never reaches this path: the core rejects initial pagination with a search. iOS keeps the listed chat's stats on load, so it has nothing to move.

## Alternatives considered

- Gate only the existing increment by `ntfsEnabled`, as for non-active profiles. Muting with unread, chat removal and mention decrements stay wrong, and making each site right needs the same per-mode arithmetic that `userUnreadCount` gives.
- Compute the active count from the chat list (sum of `userUnreadCount`). One source of truth, but it leaves out support-scope items and chats outside the 5000-chat window, so the number changes when you switch profiles. Every consumer needs an active-profile special case, and iOS still needs a delta for its app badge.
- Re-read the counts from the core after each change, or have the core send per-profile counts in events. Two `COUNT` queries per profile per event, or an API change in the core and both apps.

## Not included

Each is a separate change:
- Support-chat items. The seed has no `group_scope_tag` condition, so it counts support-scope items that the client does not track. The count steps up by the outstanding support unread at each seed. Options: a main-scope-only seed in the core, or client-side tracking.
- iOS subtracts support-chat reads from the main chat. Reading in a support chat calls `markChatItemsRead` with the scoped chat info, which lowers the main chat's stats and the count.
- Chats outside the 5000-chat window. An event or a search lists such a chat with empty stats, while the seed counted its unread items. On iOS loading keeps the listed (empty) stats, so Mark read and Clear subtract nothing; reading the items one by one still subtracts, which takes the chat's stats below zero, and a later Mark read, Clear or deleting the chat adds them back, leaving the count too high until the next seed (Mark read and Clear did this on master too). On Android and desktop opening the chat installs the server's stats and reading it then subtracts; until it is opened nothing is subtracted.
- `replaceChat` installs a whole `chatStats` without moving the count: Kotlin `ChatListNavLinkView` `markChatRead` and `markChatUnread` (from a snapshot taken before the API call) and `ChatView` `markUnreadChatAsRead`, and iOS `SimpleXAPI`'s `receivedContactRequest` handler (from the server).
- Settings changes that arrive in events through `updateChatInfo` do not move the count.
- Items that arrive while a mute or unmute is in flight and already carry the new setting are counted under it, while the settings change counts them under the old one, so the count ends off by those items until the next seed.
- If a new item arrives between a chat load's query and its stats replacement on Android and desktop, the downward move can drop the count below the database until the next seed.
- A settings change computes its move from the chat's settings when the call started. If another settings call on the same chat, a chat load or a reseed applies the new setting while the call is in flight, the count moves twice and ends off until the next seed.
- iOS unread tag counts on a settings change still read the chat after the call, so they miss the change when an event already applied the new setting (pre-existing; the Kotlin side now uses the same before/after pair as the count).
- "Mark unread" is not counted, by design: the seed does not count it either.
- The Unread filter change (groups with unread support chats) was split out into a separate PR stacked on this one.

## Testing

Desktop. The active count is not visible here (no tray, and the picker shows other profiles), so read it through #7340's filter button on #7340 vs #7340 merged with this PR: lit means count > 0.
- A message in a muted chat: lit on #7340, grey with this PR.
- Mute a chat with unread: lit on #7340, grey with this PR.
- Delete a chat with unread: lit on #7340, grey with this PR.
- A mentions-only group: a plain message leaves the button grey with this PR, and a mention to the profile lights it.
- After a restart, mark a muted chat read, then receive a message in another (unmuted) chat: grey on #7340 (negative count), lit with this PR.
- After a restart, unmute a chat with unread: grey on #7340, lit with this PR.
- Master vs this PR, mentions-only group, a mention deleted by the sender: the Unread filter keeps the group on master and clears it on the PR.

Regression on master and the PR: unread badges and mark read/unread, mute and favorite, list chip dots, clear, "keep conversation" delete, a second profile's picker badge, the chat item TTL change, the Unread filter.

Android: on resume after new messages, the chat list shows them and the other profile's picker badge includes its new message, on both builds (the app stays alive in the background, so events keep the counts right on both builds and the re-read on resume has nothing to correct); no crashes.

iOS: reviewed by reading, not built.
