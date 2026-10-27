# A reported message is moderated: the report stays active in the open support chat (Android, desktop)

## Problem

A member reports a message and keeps "Chat with admins" open. A moderator then moderates the reported message. The core marks the member's report as deleted and sends `CEvtGroupChatItemsDeleted` with the report's id, but the report stays shown as active until the chat is reopened. The same happens to a moderator who has a member's support chat open (group info → Chats with members) when another moderator moderates the reported message.

Reproduced on master with the desktop app as the reporting member (a `ClassCastException` in the log) and as the viewing moderator (no exception, report not updated).

## Cause

Two defects in `groupChatItemsDeleted` (`SimpleXAPI.kt`), the only handler of this event:

1. `(cItem.chatDir as CIDirection.GroupRcv?)` is not a safe cast. `as T?` returns null only for a null value; for a sent item (`CIDirection.GroupSnd`) it throws `ClassCastException`. A member's own report is a sent item, and `markMessageReportsDeleted` has no `item_sent` filter, so the event contains it. The receiver loop catches and logs the exception, so the rest of the event is not processed: the remaining ids and `messagesChannel.trySend`. A member's report is sent in the member-support scope, so it is only found by the secondary-context loop, which is where the exception is thrown.
2. The secondary-context update passes `ChatInfo.Group(..., groupChatScope = null)` to `upsertChatItem`. A support chat is a `GroupChatScopeContext`, and `chatItemBelongsToScope` returns false for a null scope there, so the item is found in the list but never replaced.

## Fix

Two commits, one per defect, in the one function:

1. `as?` instead of `as ...?` at both sites. A sent item gets `CIDeleted.Moderated(member_)`, which is what the core returns when the chat is reloaded (`item_deleted_by_group_member_id` maps to `CIModerated` for any direction).
2. The secondary-context upsert uses `cInfo.copy(groupChatScope = chatsCtx.groupScopeInfo)`. The item has just been found by id in that context's own `chatItems`, so the context's own scope is the item's scope. For the Reports view `groupScopeInfo` is null, so that view is unchanged.

## Alternatives considered

- Match iOS (`is GroupRcv` check, so a sent item becomes plain `Deleted`): rejected. A sent report archived by a moderator would show "archived" when the event arrives and "archived by <moderator>" after a reload.
- Extract the duplicated `val deleted = ...` expression: rejected. It appears twice, and the diff-minimality rule favours the smaller diff.
- Filter sent ids out of the event in the core: rejected. The sent report must be updated, and it needs a core change and a rebuilt library.
- Build the scoped `ChatInfo` with a local `when` on `secondaryContextFilter`: rejected in favour of `ChatsContext.groupScopeInfo`, which already computes it and is used the same way in `ChatItemsLoader.kt`, `ComposeView.kt` and `ChatView.kt`.

## Scope

- `commonMain`: Android and desktop share the path. Nothing platform-specific.
- Core unchanged.
- On desktop and Android, "Archive all reports" from the chat list reaches the same handler (`archiveAllReportsForMe`), so an open member support chat is now updated at once there too (not covered by the recorded runs).

## Not fixed

- iOS shows a sent item as `.deleted` here, where Android and desktop now show it as moderated, and iOS's handler only walks the primary `ItemsModel`, so an open support chat or the Reports view there is not updated either (not tested).
- The reports counter is decremented once per id, including ids of sent reports. It is clamped at 0, and a member who reported and was later promoted to moderator can see it drop too far. This handler did the same before this change.
- When the item's author is the moderator, the UI sets `Deleted` where a reload shows `Moderated`.
- The Reports view context passes a null scope, so `upsertChatItem` may add a placeholder chat to that context's private chat list. No visible effect found.
- Other nullable casts of the same form (`FramedItemView.kt`, `ComposeView.kt`, `ChatInfoView.kt`) are guarded by earlier checks and cannot throw.

## Verification

Desktop AppImage on Linux, master build (`prebuild-master`) against this branch, group with an owner, two members and the app as a member or a moderator; the owner moderates through `/_delete member item`.

| Case | master | this branch |
|---|---|---|
| Member's own report, Chat with admins open, reported message moderated | report stays active; `ClassCastException` in the log | archived at once; no exception |
| Two reports, both moderated 4 s apart | both stay active; two exceptions | both archived |
| Moderator, member's support chat open, reported message moderated | report stays active | archived at once |
| Chat with admins closed while moderating, then reopened | archived | archived |
| Moderator, Reports view | archived | archived |
| Moderator moderates the reported message itself | archived | archived |
| Unreported message moderated | moderated, no exception | moderated, no exception |
| Restart | archived | archived |

Also tried on this branch: moderating while another member's support chat is open, three moderations in quick succession, moderating while scrolling, and opening Chat with admins only after the moderation. No exception, stale or duplicate item in any of them.

Not tested: Android on a device (the changed code is shared with desktop) and iOS.
