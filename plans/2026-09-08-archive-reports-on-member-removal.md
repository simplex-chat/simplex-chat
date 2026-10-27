# Archive Reports When Removing a Member With Messages

## Context

"Remove member and delete messages" left reports untouched. Moderating a single message already archives the reports about it; removing the member who sent those messages did not, so reports stayed in the moderators' list pointing at messages that no longer existed.

Two distinct defects, both user-visible:

1. **Reports about the removed member survived.** They stayed active in the reports list and in the badge count.
2. **The reports badge got stuck.** After removing a member, the badge could show a non-zero count while the reports list was empty.

**Root cause (1).** The removal path (`deleteMessages` in `APIRemoveMembers` and in `xGrpMemDel`) calls only `deleteGroupMembersCIs` / `markGroupMembersCIsDeleted`, which affect only the removed member's *own* chat items, matched by `group_member_id = ?` (`markMemberCIsDeleted`, `deleteMemberCIs`). A report about that member is authored by the *reporter*, so its `group_member_id` differs and it is never matched. The moderation path does not have this gap — `delGroupChatItems` and `archiveMessageReports` call `markMessageReportsDeleted` and emit `CEvtGroupChatItemsDeleted`.

**Root cause (2).** Reports *filed by* the removed member are deleted with the member's support chat (`deleteSupportChatIfExists`, called from `deleteOrUpdateMemberRecordIO` and `fullyDeleteMemberRecordIO`), and also archived or deleted by `markMemberCIsDeleted` / `deleteMemberCIs` when messages are removed. Either way no event is emitted. `getGroupReportsCount_` counts received, non-deleted reports, so those rows are counted before removal and absent from the count afterwards, but the client is not notified. `removeMemberItems` cannot compensate: it iterates over the primary context's `chatItems`, and reports are stored in a member-support scope, so they are not in that list. Defect (2) predates this change; fixing (1) made it visible, because the list could now empty while the badge stayed high.

## Solution Summary

Add `markMemberReportsDeleted`, mirroring `markMessageReportsDeleted` but keyed on the member rather than on one message:

```sql
WHERE user_id = ? AND group_id = ? AND msg_content_tag = ? AND item_deleted = ?
  AND (quoted_member_id = ? OR group_member_id = ?)
RETURNING chat_item_id;
```

Call it from both `deleteMessages` helpers — the local command path and `xGrpMemDel` on receiving clients — and emit `CEvtGroupChatItemsDeleted` with the returned ids, so the badge updates without a reload on the removing moderator and on receiving clients. When the removed member is the local user, the `group_member_id = ?` branch matches nothing, because the user's sent items have no `group_member_id`; the badge is not affected, as it counts only received reports.

## Technical Design

### Why key on `quoted_member_id`

`quoted_member_id` is denormalised onto the report row at insert (populated from the incoming `MsgRef` in the item insert), not resolved by joining the quoted message. Deleting the member's messages therefore cannot affect the predicate — verified against the real schema by deleting the message first and confirming the report still matches.

The alternative — a subquery over the member's stored `shared_msg_id`s, mirroring `markMessageReportsDeleted`'s key exactly — was rejected: it requires running before the member's items are deleted under full-delete, and only covers messages still stored locally.

### Why the `OR group_member_id = ?` branch

This is the fix for defect (2). It adds the removed member's own reports to the returned ids so the emitted event decrements the badge for them.

**It changes no database state.** Those rows are already marked (or physically deleted) moments later by `markMemberCIsDeleted` / `deleteMemberCIs`, which have no `item_deleted` filter and set the same columns. Verified by running both paths with and without the branch and diffing the final table: identical on both the mark-delete and full-delete paths. The branch exists solely to widen the event payload.

The net effect is that the only DB-state change in this diff is: reports about the removed member get archived.

### Why the event is required

Without `CEvtGroupChatItemsDeleted`, the rows would be archived correctly but the badge would not move: `updateGroup` → `updateChat` → `updateChatInfo` replaces only `chatInfo` and never `chatStats`, so `reportsCount` is not refreshed by the removal response. The client would show a stale count until a full chat-list reload. The archiving is not moved into `deleteGroupMemberCIs` / `markGroupMemberCIsDeleted` and their plural variants: each has exactly one caller, one of the two `deleteMessages` helpers, and they would need a new `byGroupMember` parameter and a separate archiving call in both their full-delete and mark-deleted branches (four sites instead of two).

The existing handler `groupChatItemsDeleted` in `SimpleXAPI.kt` already decrements the counter once per returned id, so no client change is required for the counter.

### Ordering

The archiving runs **before** the member's items are deleted. It has to: the member's own reports must still be active to be matched.

The trade-off is that a store error in new code can now abort the removal after `x.grp.mem.del` has been sent. This was accepted deliberately. `deleteMessages` is already part of a sequence that can fail (`sendGroupMessages` → `saveSndChatItems` → `deleteMessages`), so this adds one more store operation to an already-exposed path rather than creating a new class of risk. The alternative — a separate read-only `SELECT` of the member's report ids before deletion, with the `UPDATE` after — is more code for the same outcome.

### Why no `item_sent` filter

`markMessageReportsDeleted` has none, and the existing moderation test asserts that the reporter's own sent report is marked on their device. Filtering to `item_sent = 0` would leave the reporter's copy active — the original bug, relocated.

This means ids of sent reports can be passed to the counter, which counts only received ones. Filing a report requires `memberRole == GroupMemberRole.Member` exactly (`ChatItemView.kt`), while removing a member requires `>= GRAdmin`, so no remover holds a self-authored report; on a plain member's device the count is already 0, and `coerceAtLeast(0)` keeps it at 0. The one residual case is a member promoted to moderator after filing a report — identical in the moderation path, and out of scope here.

### Inlined rather than a shared helper

Both call sites inline the two lines rather than sharing a new function. This matches how the codebase already handles this pattern: `delGroupChatItems` in `Commands.hs` inlines it, and `archiveMessageReports` in `Subscriber.hs` is a local `where` helper. Neither introduces a shared top-level function. The Subscriber side needs no `forM`/`concat`, having a single member.

## Locality

- Five files besides this plan. `Library/Internal.hs` is unchanged.
- `deleteMessages` has three call sites, all `when withMessages`-gated, so no other command or event reaches the new code.
- No protocol or type surface change — the existing `CEvtGroupChatItemsDeleted` is reused, so `Controller.hs`, the generated bot API docs, and the TypeScript/Python types are untouched.
- Groups with no reports return an empty id list, `unless (null ciIds)` suppresses the event, and behaviour is byte-identical. This is what keeps the pre-existing `messages=on` tests passing.
- `chat_query_plans.txt` gains exactly one entry (verified by set comparison against master, not by reading the diff).

## Testing

`testGroupMemberReportsRemoveMember` (`tests/ChatTests/Groups.hs`) covers both defects in one scenario: cath reports bob's message, bob reports cath's message, then alice removes bob with messages. It asserts that **two** reports are archived (`#jokes: 2 messages deleted by user`) — the one about bob and the one filed by him — which is the assertion that fails without the `OR group_member_id = ?` branch, since only one id would be emitted and the badge would stick.

Note that bob's own report row is not left marked as deleted: reports are stored in a member-support scope, and removing a member deletes the member's support chat items (`deleteSupportChatIfExists`, an explicit `DELETE` on `group_scope_group_member_id`). Cath's report, in her own scope, remains archived. Both outcomes leave zero active reports, which is what the badge must agree with.

Regression coverage relied on: `remove member with messages (full deletion is enabled)`, `remove member with messages mark deleted`, `remove member - delete messages of left/removed members`, and `should send report to group owner, admins and moderators, but not other users`.

## Known limitations

- **Removing a member destroys the reports they filed.** If a member reports abuse and is later removed for unrelated reasons, their report disappears from the moderators' list. This is existing behaviour of `deleteSupportChatIfExists`, which runs on every removal, with or without messages; this change only makes the badge agree with it when messages are removed. Whether reports should remain after their author is removed is a separate product decision.
- **Counter over-decrement for sent reports** in the promoted-member case described above, shared with the moderation path. A proper fix belongs with `getGroupReportsCount_`, not here.
- **Reports filed by a member removed without messages.** `deleteSupportChatIfExists` deletes them, but no event is emitted, because `markMemberReportsDeleted` runs only when messages are removed. The badge can show a stale count until the chat list is reloaded. A fix belongs where the support chat is deleted, in a separate change.
