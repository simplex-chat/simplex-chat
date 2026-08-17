# Full delete on member removal under fullDelete preference

Plan for the next attempt at the change previously tried in PR #6831 (closed as too messy: the member row was deleted twice on one path, and the user's own membership row was deleted when the user was the one removed). The change is small: two SQL-function edits, one new chat-layer helper, an explicit fullDelete branch plus order swap in two backend handlers, and one in-memory removal branch in `removeMemberItems` on each UI platform.

## Problem

When a member is removed via `XGrpMemDel` with `withMessages = True` and the group's `fullDelete` preference is on for the deleter's role, the member's chat items are currently rewritten to `CIModerated` placeholders by `updateMemberCIsModerated`, and the member row is preserved by `deleteOrUpdateMemberRecord` when any item references it. The intent of `fullDelete` is physical deletion. The current behavior leaves placeholder rows and, because `deleteOrUpdateMemberRecord` runs before the items pass, the relay subpath of the latter deletes the member row first and the subsequent file collection returns nothing — files on disk leak. The same ordering bug exists on the moderator side (`APIRemoveMembers`).

## What changes

In `xGrpMemDel` (`src/Simplex/Chat/Library/Subscriber.hs:3157`), only on the branch where `withMessages = True` AND `groupFeatureMemberAllowed SGFFullDelete m gInfo`:

**Case A — the deleted member is the user themselves (`memId == membership.memberId`).** The user's own sent items (those with `group_member_id IS NULL AND item_sent = 1`) and their files are physically deleted. The `membership` row stays with status `GSMemRemoved`, so the group can still be loaded in the chat list and opened.

**Case B — the deleted member is somebody else.** The member's chat items and their files are physically deleted, then the `group_members` row is deleted. Historical system event items that referenced this member as `item_deleted_by_group_member_id` survive with NULL via the existing `ON DELETE SET NULL`.

The non-fullDelete branch, the `withMessages = False` branch, and the entire message-moderation path (`XMsgDel`, `APIDeleteMemberChatItem`, `deleteGroupCIs`, `markGroupCIsDeleted`, `createCIModeration`, `chat_item_moderations`) are not changed.

## Implementation

The whole change is two SQL-function edits in `Store/Messages.hs`, a new member-record helper in `Library/Internal.hs`, and explicit branching plus an order swap in both `xGrpMemDel` (recipient side) and `APIRemoveMembers` (moderator side).

**Edit 1 — rewrite `updateMemberCIsModerated` to physically delete.** Recommended rename: `deleteMemberCIs`. Keep the existing `memId == groupMemberId' membership` branch unchanged (the membership branch selects `WHERE group_member_id IS NULL AND item_sent = 1`; the other branch selects `WHERE group_member_id = ?`). Change the body from "UPDATE chat_items SET moderated content" to "physically delete chat_items + side-table rows analogous to `deleteGroupChatItem` in bulk": delete from `chat_item_messages`, `chat_item_versions`, `chat_item_reactions`, then `DELETE FROM chat_items`. The function loses the `byGroupMember`, `msgDir`, and `deletedTs` parameters since they were only used to construct the moderated content. The chat-layer wrappers `deleteGroupMemberCIs` and `deleteGroupMembersCIs` follow the same signature simplification.

**Edit 2 — extend `getGroupMemberFileInfo` to handle the membership case.** Today it queries `WHERE group_member_id = ?` only, so for Case A it returns nothing and files for the user's own sent items leak (this is a pre-existing bug on both the off and on paths — `markGroupMemberCIsDeleted_` also relies on this function to cancel in-progress transfers). Add the same `memId == groupMemberId' membership` branch as in `deleteMemberCIs`: for the membership case, query `WHERE group_member_id IS NULL AND item_sent = 1`. The only two callers (`deleteGroupMemberCIs_` and `markGroupMemberCIsDeleted_`) both benefit from the fix.

**Edit 3 — add `fullyDeleteMemberRecord` helper in `Library/Internal.hs` next to `deleteOrUpdateMemberRecord`.** Wraps `deleteSupportChatIfExists` + `deleteGroupMember`, returns updated `GroupInfo`. No `isRelay` branch and no `checkGroupMemberHasItems` query — the caller has already physically deleted the member's items, so the existence check would be a wasted query and the function communicates intent explicitly: unconditional row deletion. The `CM` wrapper plus an `IO` variant (`fullyDeleteMemberRecordIO`) mirror the shape of the existing `deleteOrUpdateMemberRecord` / `deleteOrUpdateMemberRecordIO`.

**Edit 4 — swap order and add explicit branching in `xGrpMemDel` Case B (the `else` branch).** Move `when withMessages $ deleteMessages gInfo'' deletedMember' SMDRcv` to run *before* the member-record decision, on the same `gInfo` (the new `deleteMessages` reads only `groupId` and `membership` from the passed `gInfo`). Replace the current member-record dispatch with an explicit branch:

```
gInfo' <- case deliveryScope of
  Just (DJSMemberSupport _) | shouldForward -> updateMemberRecordDeleted user gInfo deletedMember GSMemRemoved
  _ -> if withMessages && groupFeatureMemberAllowed SGFFullDelete m gInfo
         then fullyDeleteMemberRecord user gInfo deletedMember
         else deleteOrUpdateMemberRecord user gInfo deletedMember
```

`deleteMemberItem` (the RGE event creation) keeps its current position after `updatePublicGroupData`. Case A (the `then` branch) needs no order change — the membership row is never deleted there, and `deleteMessages` already runs in the right relative position.

The `DJSMemberSupport _ | shouldForward` subcase keeps its existing `updateMemberRecordDeleted` call regardless of fullDelete — the row is preserved for support-scope forwarding. Under fullDelete the items are still gone (the `deleteMessages` step ran first), the row stays.

**Edit 5 — mirror the swap and explicit branching in `APIRemoveMembers` (`src/Simplex/Chat/Library/Commands.hs:2834`).** Inside `deleteMemsSend`, compute `fullDelete = withMessages && groupFeatureUserAllowed SGFFullDelete gInfo` once. Move the items pass to before `delMember`: run `deleteMessages user gInfo memsToDelete` inside `deleteMemsSend` before the `withStoreBatch'` that calls `delMember`. Change `delMember` to branch explicitly:

```
delMember db m = do
  if fullDelete
    then void $ fullyDeleteMemberRecordIO db user gInfo m
    else void $ deleteOrUpdateMemberRecordIO db user gInfo m
  pure m {memberStatus = GSMemRemoved}
```

`deletePendingMember` flows through `deleteMemsSend` and inherits the new behavior. The outer line 2864 call (`when withMessages $ deleteMessages user gInfo' deleted`) collapses — items are already handled inside `deleteMemsSend` for current and pending members, and invited members (handled by `deleteInvitedMems`) have no chat items. Remove it.

**Edit 6 — extend `removeMemberItems` on both UI platforms to physically remove items from the in-memory list when `fullDelete.on`.** Today (iOS `apps/ios/Shared/Model/ChatModel.swift:814-846`, Kotlin `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt:699-734`) the function walks the in-memory items, identifies matches by direction and member id, and sets `itemDeleted = .moderated(...)`; under `fullDelete.on` it additionally rewrites content to `Snd/RcvModerated`. Items are never removed from `im.reversedChatItems` / `chatItems.value`. After the backend change, the chat_item rows are physically gone in DB while the UI keeps stale moderated placeholders until the next refetch — flicker. Extend the existing `fullDelete.on` branch so it also removes matching items from the in-memory list (iOS: drop them from `im.reversedChatItems`, decrement unread counters, stop voice playback on dropped items; Kotlin: `removeAllAndNotify { isMemberItem(it) }` equivalent, decrement counters, stop audio). The fullDelete-off branch is unchanged (still marks moderated in place).

The three callers — iOS `removeMember` in `GroupChatInfoView.swift:977`, Kotlin `removeMembers` in `GroupChatInfoView.kt:1316`, Kotlin `removeMember` in `GroupMemberInfoView.kt:339`, and the event handlers for `.deletedMember`/`.deletedMemberUser` in `SimpleXAPI.swift:2578-2596` and `SimpleXAPI.kt:2945-2973` — all converge on the same `removeMemberItems` function on each platform and inherit the new behavior automatically. The chat-list preview path inside `removeMemberItems` (the `else` branch that updates `chat.chatItems[0]`) also needs to drop the preview item under fullDelete so the chat list doesn't show a stale moderated last-message.

The `fullDelete.on` gate matches the backend's `groupFeatureMemberAllowed SGFFullDelete` / `groupFeatureUserAllowed SGFFullDelete` because FullDelete is a `GroupFeatureNoRoleI` feature — the role check collapses to the `.on` check.

## Anti-patterns from PR #6831 to avoid

No path may call `deleteGroupMember` twice. No path under Case A may delete the `membership` row — that row must survive. File info must be collected before any chat-item deletion, since `getGroupMemberFileInfo` reads `chat_items`. Do not rely on `ON DELETE SET NULL` to clean up the deleted member's authored items — they are deleted explicitly first. `fullyDeleteMemberRecord` is the only function that should call `deleteGroupMember` directly on the new path; do not duplicate that call in the handler.

## Tests

Add cases in `tests/ChatTests/Groups.hs` for: Case A (user removed by admin, fullDelete on — user's sent items and their files gone, `membership` row exists with `GSMemRemoved`, group still loadable); Case B (member removed by admin, fullDelete on — member's items and files gone, `group_members` row gone, system event items previously referencing the removed member now have NULL `item_deleted_by_group_member_id` and still display correctly); regression for fullDelete=off (items become `CIModerated` placeholders via `markMemberCIsDeleted`); regression for `withMessages = False` (items untouched, row handled by existing path); regression that message moderation under fullDelete=on still produces `CIModerated` placeholders, confirming the moderation path is unchanged. Verify the same Case A and Case B behaviors over both XGrpMemDel (recipient side, Subscriber.hs) and APIRemoveMembers (moderator side, Commands.hs).

UI checks for the manual smoke test: in a group with fullDelete on, remove a member with messages — that member's bubbles disappear immediately from the open chat view on both moderator's and recipients' devices, the chat list preview updates to the previous non-deleted message, and the unread/report counters decrement; with fullDelete off, the same removal produces moderated placeholders as today. Verify on iOS, Android, and Desktop.

## Open items for review

Naming of the rewritten `updateMemberCIsModerated`: `deleteMemberCIs` is the natural rename (the function physically deletes chat items associated with a member, handling the membership case internally). Naming of the new chat-layer helper: `fullyDeleteMemberRecord` (parallels `deleteOrUpdateMemberRecord`). Confirm or amend before implementation.
