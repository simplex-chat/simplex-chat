# Group members: index by role for the moderator lookup

## Problem

Sending in a member support chat of a large group is slow, although a support message only goes to the moderators and the member.

## Cause

A support-chat send gets its recipients from `getGroupModerators` (`getGroupRecipients`, `Library/Internal.hs`): `groupMemberQuery ... WHERE m.user_id = ? AND m.group_id = ? AND ... AND m.member_role IN (?,?,?)`. No index includes `member_role`; the query uses `idx_group_members_group_id (user_id, group_id)`, so SQLite reads every member row of the group and filters on the role. On the reporter's device this query took 178 ms on average and 4.8 s at most, over 80 calls, and it holds the single database connection (`DBStore.dbConnection :: MVar`) while it runs, so every other store operation waits behind it.

## Fix

Migration `20260926_member_role_index` (SQLite and Postgres) replaces `idx_group_members_group_id (user_id, group_id)` with `idx_group_members_group_id_member_role (user_id, group_id, member_role)`. The new index has the old one as a prefix, so every query that used the old index can use the new one, and the number of indexes on `group_members` stays the same.

**Measured** on a copy of a real database with 20,031 members in one group (11 moderators or above), same query text, warm cache:

| Query | Before | After |
|---|---|---|
| `getGroupModerators` | 8.3 ms | 0.13 ms |
| `getGroupMembers` (all) | 204.6 ms | 198.4 ms |
| `getGroupMembersForExpiration` | 7.0 ms | 6.6 ms |
| lookup by `local_display_name` / `member_category` | 5.3 / 5.0 ms | 5.4 / 4.8 ms |
| `DELETE` all group members (rolled back) | 458 ms | 417 ms |

Only the role-filtered queries change; the other rows are within noise, because those queries read every member row either way.

## Plans

I compared `EXPLAIN QUERY PLAN` before and after for all 144 queries touching `group_members` in `chat_query_plans.txt`, with foreign keys on, on SQLite 3.39.2 (the version the apps bundle) and 3.40.1:
- 135 plans are unchanged.
- 4 now use `member_role` in the index: the three-role query (`getGroupModerators`, `getGroupRosterMembers`), the two-role query (`getGroupAdminsMods`), the `member_role = ?` query (`getGroupOnlyMembers`, `getGroupOwners`) and the relay query (`getGroupRelayMembers`). Single-role queries keep row-id order, so they need no sort.
- 5 read the same `(user_id, group_id)` range from the new index. The covering `DELETE` stays covering.
- None gains a scan or a temp B-tree sort.

## Row order

`getGroupMembers` and the three functions using the `member_role IN (...)` queries have no `ORDER BY`. Through the old index they returned members in insertion order; through the new one SQLite returns them grouped by role, which would change `/ms` output, break ordered test assertions and, in `getGroupRosterMembers`, change which members `buildGroupRoster` keeps under `maxGroupRosterSize`. These four functions now sort the result by `group_member_id` in Haskell, which restores the previous order exactly. `getGroupMemberIdByName` takes the first row of a name lookup that can match several rows (a removed member re-added under the same name), so it sorts by `group_member_id` too and keeps returning the oldest row, as before. `getGroupMembersForExpiration` is also unsorted now, but only deletes each member in turn. `getHostMemberId_` also takes the first row, of the host members; a group has several only in channels, where they are all relays with the same role, so the order among them is unchanged. For the four `groupMemberQuery` functions, an SQL `ORDER BY m.group_member_id` was rejected: without table statistics SQLite then chooses `idx_group_members_user_id (user_id)` to avoid the sort, which scans every membership of the user in all groups.

Keeping the old index alongside was also checked: SQLite then chooses the new index for 8 of the 9 queries; the covering `DELETE` keeps the old one, so keeping it would only add write cost.

## Schema dump test

On the down migration, `idx_group_members_group_id` is recreated at the end of `chat_schema.sql` instead of its original position, so `20260926_member_role_index` is added to `skipComparisonForDownMigrations` in `tests/SchemaDump.hs`.

## Cost

The one-off migration took 3.2 s to create the index and 0.9 s to drop the old one on a 520,155-member table (9 MB index). Role changes now also update the index, and role changes are rare.
