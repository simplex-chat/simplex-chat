# Member support chats: update the list from events

The member support list is updated from events instead of reloading all members, so opening a member's support chat no longer waits behind a member reload.

## Problem

A group owner with a large group opens the "Chat with members" list, then opens members' support chats one after another. The first chat opens quickly. Every later one takes several seconds to open.

## Cause

1. Only the top modal is composed (`ModalManager.showInView`, `ModalView.kt`). While a support chat is open over the list, `MemberSupportView` has left composition. It re-enters composition when the user goes back.
2. `MemberSupportView` had `LaunchedEffect(Unit) { setGroupMembers(...) }`, so every return to the list runs `apiListMembers`. That call loads the full member list of the group, with profiles and connections. iOS did the same in `.onAppear`.
3. The chat database has one connection (`DBStore.dbConnection :: MVar`), and every store operation takes it.
   - Going back to the list is instant, because the old members are still in memory.
   - The next `/_get chat #g(_support:m)` is queued behind the member query that is already running.
   - Cancelling the coroutine does not stop the core query.
4. Measured on the reporter's device with `/sql slow`: `getGroupMembers` took 1.6 s on average and 15 s at most, over 122 calls. The scoped `getGroupChat` queries in the same log took at most 37 ms each.

When the list is opened from the chat toolbar, the first open is fast because the member load from opening the list has finished by the time the user taps.

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
- The group send response carries the chat info returned by `updateChatTsStats`, via a new `saveSndChatItems'` that `saveSndChatItems` wraps, instead of the member from before the send; for main-chat sends it carries the updated `chatTs` too.
- Internal items go through `createChatItems`, for example "new member pending review" (unread +1, and attention +1 when the pending member is the sender). It now builds its items from the `ChatInfo` returned by `updateChatTsStats`, as `saveRcvChatItem'` already does, instead of the pre-update `toChatInfo cd`. Otherwise a new pending member would appear without a badge. This applies to every chat type: direct and main group chats now carry the updated `chatTs`, as received items already did.
- A moderation that arrives before its message creates the item and marks it deleted. The `ChatItemsDeleted` event now carries the scope returned by creating the item, not the one from before it.
- Opening a member's support chat for the first time sets `support_chat_ts` and now returns the re-read member, so the new chat appears in the list.
- Existing clients are unaffected: both apps strip the scope in `updateChatInfo`.

**Android/desktop, and iOS**
- New `upsertSupportChatMember(cInfo)`. When the chat info carries a support-chat member, it adds that member if absent. If the member is already present, it replaces only its `supportChat` stats and profile. The profile is copied because a member who is also a contact gets profile updates over the direct connection, and no group event carries them. Status and role keep coming from their dedicated events. That way an item event applied late, such as a leave item handled after `LeftMember`, cannot restore an old status.
- It is called for:
  - `NewChatItems` events;
  - `ChatItemsDeleted` events;
  - delete responses for items (the reports response carries no scope, as `APIDeleteReceivedReports` passes none, and the stats are not changed either);
  - send and forward responses (in `processSendMessageCmd` on both platforms);
  - the mark-read response;
  - the initial load of a support chat.
- `JoinedGroupMember` and `JoinedGroupMemberConnecting` are emitted right after the "new member pending review" item, and they carry the member as read before that item. Their handlers keep the support stats already in the list, so they do not erase the badge the item event just set. The same applies to the accept-member response, which carries the member from before the accept item updated `support_chat_ts`. On Kotlin that item event can also be applied after them; the merge covers either order. Other member events that follow a group event item (accepted by another moderator, connected, profile updated) need no merge: those items do not change support stats (`ciRequiresAttention` is false for them).
- The list loads members only when they are not loaded for its group. On Kotlin with the local host, the load runs in the list's `LaunchedEffect`, keyed on `membersLoaded`, so it restarts when it is reset (for example by `ChatView` after an Android configuration change); leaving the list cancels a load whose response has not arrived. With a remote host it loads on every open (see Known limitations). On iOS the list loads when it appears if `membersLoaded` is not set, so after the reset on resume it reloads the next time it appears.
- iOS resets `membersLoaded` when chats are refreshed on resume after the app was suspended, because the notification extension may have changed support chats while the app was suspended.
- Kotlin `upsertGroupMember` also resets `membersLoaded` when it clears another group's stale members for the open chat.
- On Kotlin, chat ids (`#<groupId>`) are not unique across remote hosts, so `upsertSupportChatMember` also checks that the update is for the current remote host, and `setGroupMembers` that the result is still for the open chat (or the channel being created) on the current remote host.
- Kotlin `setGroupMembers` stores a result that has arrived even when the screen that started the load was left (`NonCancellable`), so leaving the list as the members arrive does not leave them unloaded.
- On iOS, member info opened from a support chat holds a copy of the member taken when that chat was opened. Member info, block/unblock for me, security code verification and changing or aborting the receiving address now keep the list's support stats (`withLoadedSupportChat`) instead of writing back those of the member they started with; other fields are written as before. The old reload on return hid this. On Kotlin member info already reads the member from the model.
- The refresh button is removed. The list is kept current by the updates above.

A member's first support message arrives as a `NewChatItems` event with that member, so a new support chat appears in the list without a reload.

## Known limitations

- On Kotlin, the group's member list is shared with a channel being created (desktop can show both at once), and an update for the channel clears the other group's members, as before this change. `membersLoaded` is reset only when the cleared list is written for the open chat, so the open list of another group does not reload after each channel update; it stays empty until it is reopened or returned to, when it loads because the member list holds another group's members, or, with the local host, until the next update for a member of that group, which clears the channel's members and resets `membersLoaded`, so the list loads. That load drops the channel's relay members from the list until the next relay update.
- A failed member load stores an empty list marked as loaded, as before, but reopening the list no longer retries it (except with a remote host), so the list shows no members until group info or the group is reopened.
- On a remote host the list still loads all members on every return, as before, because an older host does not return the updated member on read, send and delete.
- A member load in flight when a support-chat update arrives overwrites that update with its snapshot. For example, the first list load can race a member's first support message. The member then reappears on their next message, when their chat is opened, or when the group is reopened.
- Support stats snapshots from different events and responses are applied in arrival order, so a rare reordering can briefly show an older count until the next update for that member. This includes the responses to role change, block for all, fix connection and, on Kotlin, changing or aborting the member's receiving address, which carry the member as read at the start of the command. On Kotlin, `NewChatItems` is applied asynchronously while other events and responses are applied directly, so a later update can be applied before an earlier item's stats and profile.
- On iOS, updating an existing member in place (including the new support-stats upserts) updates that row's badges, because rows observe their `GMember`, but not the list order or filter until the next `ChatModel` change. The support-stats upsert forces a re-render only when a member is added or an existing member gets their first support chat, so that the member appears in the list. The next `ChatModel` change usually follows soon after a read (the unread counter), so the list can now re-sort while a member's support chat is pushed from it. Rows keep their identity (`ForEach` by member id), and the row's leading swipe action is now built unconditionally with the condition inside it, so the row that holds the active `NavigationLink(isActive:)` is not rebuilt when its unread state flips. Whether re-sorting alone can pop the pushed chat on older iOS versions needs a device test.
- The connection-state labels in rows (failed, disabled, inactive) come from `activeConn`. Neither app handles `ConnectionDisabled` or `ConnectionInactive`, so these labels now refresh only when the member is re-read and written to the list, for example on the next full member event (role, profile, connected), after a role, block or fix-connection action, or when group info or the group is reopened.
- On Kotlin, security code verification and block/unblock for me upsert the member as read when the action started, and member info opened from the chat view upserts the member as read by its API calls, or the member it was opened with when they fail, so a support update that lands in between is overwritten until that member's next update.
- A member who is also a contact and changes their profile over the direct connection: the list shows the old name and image until that member's next support-chat update, when group info is opened, or when the group is reopened.
- On iOS, a list that is on screen when the app resumes is not reloaded until the user leaves it and comes back.
- On iOS, the reset on resume also makes the mention picker reload all members when an existing mention is edited after a resume, and returning to the list before its load finishes after a resume starts another full load.

## Alternatives considered

- **Refresh only the viewed member on return** (`apiGroupMemberInfo`). Rejected: it still polls, and it misses changes to other members.
