# Member support chats list: update from events instead of reloading all members

## Problem

A group owner with a large group opens the "Chat with members" list, then opens members' support chats one after another. The first chat opens quickly. Every later one takes several seconds to open.

## Cause

1. Only the top modal is composed (`ModalManager.showInView`, `ModalView.kt`). While a support chat is open over the list, `MemberSupportView` has left composition. It re-enters composition when the user goes back.
2. `MemberSupportView` has `LaunchedEffect(Unit) { setGroupMembers(...) }`, so every return to the list runs `apiListMembers`. That call loads the full member list of the group, with profiles and connections. iOS does the same in `.onAppear`.
3. The chat database has one connection (`DBStore.dbConnection :: MVar`), and every store operation takes it.
   - Going back to the list is instant, because the old members are still in memory.
   - The next `/_get chat #g(_support:m)` is queued behind the member query that is already running.
   - Cancelling the coroutine does not stop the core query.
4. Measured on the reporter's device with `/sql slow`: `getGroupMembers` took 1.6 s on average and 15 s at most, over 122 calls. The scoped `getGroupChat` queries in the same log took at most 37 ms each.

The first open is fast because the member load from opening the list has finished by the time the user taps.

## Why the list reloaded

The per-member support stats shown in the list (unread, member attention, mentions, last activity) were never applied from events. The full reload was the only way they got refreshed.

- **Receiving.** The core already sends the updated member when a support chat changes: `updateChatTsStats` re-reads the member, and the new item is sent as `GroupChat g (Just (GCSIMemberSupport (Just member')))` in `NewChatItems`. The app ignored it.
- **Marking read.** `APIChatItemsRead` computed the updated member in `updateGroupScopeUnreadStats` but discarded it, and returned `GroupChat gInfo' Nothing`.
- **Deleting.** Deletions in a support scope updated `GroupInfo` but kept the scope member from before the update.

## Fix

Load the list once per open group, then keep it current from what arrives.

**Core**
- `updateGroupScopeUnreadStats` returns the updated `GroupChatScopeInfo` together with `GroupInfo`.
- `APIChatItemsRead` returns `GroupChat gInfo' chatScopeInfo'`.
- `deleteGroupCIs` puts the updated scope member into each deletion's chat info.
- The group send response re-reads the support scope member after `saveSndChatItems` has updated `support_chat_ts`, instead of returning the member from before the send. If that read returns a store error, it falls back to the pre-send member rather than failing a send that has already happened.
- Internal items go through `createChatItems`, for example "new member pending review" (unread and attention +1). It now builds its items from the `ChatInfo` returned by `updateChatTsStats`, as `saveRcvChatItem'` already does, instead of the pre-update `toChatInfo cd`. Otherwise a new pending member would appear without a badge. This applies to every chat type: direct and main group chats now carry the updated `chatTs`, as received items already did.
- A moderation that arrives before its message creates the item and marks it deleted. The `ChatItemsDeleted` event now carries the group info and scope returned by creating the item, not those from before it.
- Opening a member's support chat for the first time sets `support_chat_ts` and now returns the re-read member, so the new chat appears in the list.
- Existing clients are unaffected: both apps strip the scope in `updateChatInfo`.

**Android/desktop, and iOS**
- New `upsertSupportChatMember(cInfo)`. When the chat info carries a support-chat member, it adds that member if absent. If the member is already present, it replaces only its `supportChat` stats and profile. The profile is copied because a member who is also a contact gets profile updates over the direct connection, and no group event carries them. Status and role keep coming from their dedicated events. That way an item event applied late, such as a leave item handled after `LeftMember`, cannot restore an old status.
- It is called for:
  - `NewChatItems` events;
  - `ChatItemsDeleted` events;
  - delete responses (items and reports);
  - send and forward responses (in `processSendMessageCmd` on both platforms);
  - the mark-read response;
  - the initial load of a support chat.
- `JoinedGroupMember` and `JoinedGroupMemberConnecting` are emitted right after the "new member pending review" item, and they carry the member with zero support stats. Their handlers keep the support stats already in the list, so they do not erase the badge the item event just set. On Kotlin that item event can also be applied after them; the merge covers either order.
- The member list loads only if `membersLoaded` is false. On both platforms the list also reloads when `membersLoaded` is reset while it is open and its group is still the open chat. Examples are `ChatView` recomposing after an Android configuration change, and the iOS reset on resume. The mention picker already uses this flag the same way, and it is reset when leaving the group.
- `apiListMembers` returns `null`/`nil` on error on both platforms, and the member load then keeps the current state, so a failed load does not mark members as loaded. iOS still runs the load's completion, so group info still opens.
- iOS clears the loaded members and resets `membersLoaded` when the open chat changes without going through the chat list (notification tap, "forwarded from", member info), as Kotlin already does. Because group info can also be opened from a message avatar without reloading members, the iOS list additionally reloads unless members were loaded for this group (`membersLoadedGroupId`).
- iOS resets `membersLoaded` when chats are refreshed on resume, because the notification extension may have changed support chats while the app was suspended.
- Kotlin `upsertGroupMember` also resets `membersLoaded` when it clears another group's stale members.
- Kotlin `setGroupMembers` now writes on the main thread, where all upserts run, so an upsert can no longer land between clearing the index and rebuilding it and add a duplicate. It writes its result only if the group is still the open chat (or the channel being created), as iOS `loadGroupMembers` already does. Without this check, a slow load from a previously opened channel could finish after a chat switch and mark another group's members as loaded. The old reload on every return hid that.
- The refresh button is removed. The list is kept current by the updates above.
- iOS also sends `objectWillChange` (only for the open group), because updating a `GMember` in place does not re-render or re-sort the list.

A member's first support message arrives as a `NewChatItems` event with that member, so a new support chat appears in the list without a reload.

## Known limitations

- A failed member load (`apiListMembers` error) is not retried while the list stays open; it is retried when the list is reopened.
- A full member load that is in flight when a support-chat update arrives overwrites that update with its snapshot. For example, the first list load can race a member's first support message. The member then reappears on their next message, when their chat is opened, or when the group is reopened.
- Support stats snapshots from different events and responses are applied in arrival order, so a rare reordering can briefly show an older count until the next update for that member.
- On iOS, handlers that update an existing member in place without publishing a change, such as "Mark read" from the context menu or accept, update the row but not the list order or filter until the next `ChatModel` change. The existing TODO in `deleteMemberSupportChat` describes the same mechanism. Previously, returning to the list also re-sorted it.
- The connection-state labels in rows (failed, disabled, inactive) come from `activeConn`. Neither app handles `ConnectionDisabled` or `ConnectionInactive`, so these labels now refresh only on the next full member event for that member (role, profile, connected) or when the group is reopened.

## Not addressed

- In channels (`useRelays`), opening a support chat runs the chat view's initialisation, which loads all members for relay groups on both platforms. So each support chat open in a channel still does a full member load. This was already the case before this change; the reported bug is in an ordinary group.

## Alternatives considered

- **Refresh only the viewed member on return** (`apiGroupMemberInfo`). Rejected: it still polls, and it misses changes to other members.
- **A core query returning only members with support chats** (`support_chat_ts IS NOT NULL`). This would also speed up the first list load. It is a larger API change and can follow separately.
- **A `(user_id, group_id, member_role)` index** for `getGroupModerators`, which runs on every support send. That is an independent change on a separate branch.
