# Full delete on member removal under fullDelete preference

Plan for the next attempt at the change previously tried in PR #6831 (closed as too messy: the member row was deleted twice on one path, and the user's own membership row was deleted when the user was the one removed). This is a small change scoped to the `xGrpMemDel` handler and a couple of helpers.

## Problem

When a member is removed via `XGrpMemDel` with `withMessages = True` and the group's `fullDelete` preference is on for the deleter's role, the member's chat items are currently rewritten to `CIModerated` placeholders by `updateMemberCIsModerated`, and the member row is preserved by `deleteOrUpdateMemberRecord` when any chat item references it. The intent of `fullDelete` is physical deletion. The current behavior leaves placeholder rows and, in the relay subpath of `deleteOrUpdateMemberRecord`, leaks files on disk.

## What changes

Inside `xGrpMemDel` (`src/Simplex/Chat/Library/Subscriber.hs:3157`), only on the branch where `withMessages = True` AND `groupFeatureMemberAllowed SGFFullDelete m gInfo`:

**Case A — the deleted member is the user themselves (`memId == membership.memberId`).** The user's own sent items (those with `group_member_id IS NULL AND item_sent = 1`) and their files are physically deleted. The `membership` row stays with status `GSMemRemoved`, so the group can still be loaded in the chat list and opened.

**Case B — the deleted member is somebody else.** The member's chat items and their files are physically deleted, then the `group_members` row is deleted. The current `deleteOrUpdateMemberRecord` "keep row if it has chat items" rule does not apply on this branch — chat items are deleted explicitly before the row, so no orphaned chat items with NULL `group_member_id` for the deleted member remain. Historical system event items that referenced this member as `item_deleted_by_group_member_id` survive with NULL via the existing `ON DELETE SET NULL`.

The non-fullDelete branch, the `withMessages = False` branch, and the entire message-moderation path (`XMsgDel`, `APIDeleteMemberChatItem`, `deleteGroupCIs`, `markGroupCIsDeleted`, `createCIModeration`, `chat_item_moderations`) are not changed.

## Implementation outline

Sequence for the fullDelete branch in both cases: (1) collect file info for the items that are about to be deleted; (2) `deleteCIFiles` to cancel in-progress transfers and remove files from disk — this must run before any row deletion because file paths live on `chat_items`; (3) delete the chat items explicitly; (4) Case B only: delete the `group_members` row, and run `deleteSupportChatIfExists` plus profile and incognito cleanup that the existing `deleteGroupMember` already performs.

For Case A, two new functions in `Store/Messages.hs`: `getUserSentGroupFileInfo` (queries `WHERE group_member_id IS NULL AND item_sent = 1`) and `deleteUserSentGroupChatItems` (deletes those items and their side-table rows analogous to `deleteGroupChatItem`); plus a new `deleteMembershipCIs` wrapper in `Library/Internal.hs` that composes them with `deleteCIFiles`.

For Case B, rewrite `deleteGroupMemberCIs` so that instead of calling `updateMemberCIsModerated` it: collects files via `getGroupMemberFileInfo`, runs `deleteCIFiles`, then runs `deleteSupportChatIfExists` followed by `deleteGroupMember`. Return the updated `GroupInfo` so it can replace the prior `deleteOrUpdateMemberRecord` call in the handler. In the handler, the fullDelete branch must replace `deleteOrUpdateMemberRecord` entirely on that path, not run in addition to it, so the row is deleted exactly once.

After this refactor, `updateMemberCIsModerated`, `deleteGroupMemberCIs_`, and `deleteGroupMembersCIs` are expected to be unused. Verify zero callers and delete them in the same PR.

## Anti-patterns from PR #6831 to avoid

No path may call `deleteGroupMember` twice. No path under Case A may call `deleteGroupMember` on `membership` — that row must survive. File info must be collected before any chat item deletion, since `getGroupMemberFileInfo` reads chat items. Do not rely on `ON DELETE SET NULL` to clean up the deleted member's own chat items — delete them explicitly. Preserve every side effect of `deleteGroupMember` (connection delete, profile cleanup, incognito cleanup) and the `deleteSupportChatIfExists` call that today is part of `deleteOrUpdateMemberRecord` / `updateMemberRecordDeleted`.

## Tests

Add cases in `tests/ChatTests/Groups.hs` for: Case A (user removed by admin, fullDelete on — user's sent items and their files are gone, `membership` row exists with `GSMemRemoved`, group is still in the chat list and loadable); Case B (member removed by admin, fullDelete on — member's items and files are gone, `group_members` row is gone, system event items previously referencing the removed member now have NULL `group_member_id` and still display correctly); regression for fullDelete=off (items become `CIModerated` placeholders, member row handled by `deleteOrUpdateMemberRecord` as today); regression for `withMessages = False` (items untouched, row handled by existing path); regression that message moderation under fullDelete=on still produces `CIModerated` placeholders, confirming the moderation path is unchanged.

## Open item

The `DJSMemberSupport _ | shouldForward` subcase in Case B today calls `updateMemberRecordDeleted ... GSMemRemoved` and explicitly keeps the member row alive because the support-scope forward still needs it. The plan defaults to preserving this exception even under fullDelete=ON: on the forward path, do not physically delete the row, behave as the non-fullDelete branch. If review concludes that subcase should also full-delete, flag before implementation.
