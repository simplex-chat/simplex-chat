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
- Existing clients are unaffected: both apps strip the scope in `updateChatInfo`.

**Android/desktop, and iOS**
- New `upsertSupportChatMember(cInfo)`. When the chat info carries a support-chat member, it upserts that member into the group's members.
- It is called for:
  - `NewChatItems` events;
  - `ChatItemsDeleted` events;
  - send responses (Kotlin only);
  - the mark-read response;
  - the initial load of a support chat.
- The member list loads only if `membersLoaded` is false. The mention picker already uses this flag the same way, and it is reset when leaving the group.
- The refresh button is removed. The list is kept current by the updates above.
- iOS also sends `objectWillChange`, because updating a `GMember` in place does not re-render or re-sort the list.

A member's first support message arrives as a `NewChatItems` event with that member, so a new support chat appears in the list without a reload.

## Not covered

- On iOS, `apiSendMessages` returns bare `ChatItem`s without chat info. The list's last-activity order after the user's own reply updates only when the group is reopened.
- API-response deletions made by the user themselves are not hooked. Only deletion events are. Items the user deletes are normally already read.

## Alternatives considered

- **Refresh only the viewed member on return** (`apiGroupMemberInfo`). Rejected: it still polls, and it misses changes to other members.
- **A core query returning only members with support chats** (`support_chat_ts IS NOT NULL`). This would also speed up the first list load. It is a larger API change and can follow separately.
- **A `(user_id, group_id, member_role)` index** for `getGroupModerators`, which runs on every support send. That is an independent change on a separate branch.
