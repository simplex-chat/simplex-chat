# Group members: index by role for the moderator lookup

## Problem

Sending in a member support chat of a large group is slow, although a support message only goes to the moderators and the member.

## Cause

A support-chat send gets its recipients from `getGroupModerators` (`getGroupRecipients`, `Library/Internal.hs`): `groupMemberQuery ... WHERE m.user_id = ? AND m.group_id = ? AND ... AND m.member_role IN (?,?,?)`. No index includes `member_role`; the query uses `idx_group_members_group_id (user_id, group_id)`, so SQLite reads every member row of the group and filters on the role. On the reporter's device this query text took 178 ms on average and 4.8 s at most, over 80 calls (`getGroupRosterMembers` runs the same text with other roles, so some calls may be its), and it holds the single database connection (`DBStore.dbConnection :: MVar`) while it runs, so every other chat database operation waits behind it.

## Fix

Migration `20261018_member_role_index` (SQLite and Postgres) adds `idx_group_members_user_id_member_role (user_id, member_role, group_id)`. `idx_group_members_group_id` stays. No query changes.

Measured on SQLite 3.40.1 with 46,000 member rows, a 20,000-member group, same query text, warm cache, median of 25 runs:

| Query | Before | After |
|---|---|---|
| `getGroupModerators` | 10.7 ms | 0.15 ms |
| `getGroupAdminsMods` | 9.9 ms | 0.09 ms |
| `getGroupOwners` | 9.1 ms | 0.05 ms |

## Why this column order

The queries without a role filter must keep their plans, because they have no `ORDER BY` and return rows in the order of the index they read:

- `(user_id, group_id, member_role)`, replacing the old index: the queries filtering by `user_id` and `group_id` read the same range, but in role order. `getGroupMembers` (the `/ms` listing) and `getGroupMemberIdByName` (which takes the first of several rows with one name) change their results.
- `(user_id, group_id, member_role)` next to the old index: the planner rates both equally for those queries and picks one by position in the schema.
- `(group_id, member_role)` and other indexes led by `group_id`: they tie with `idx_group_members_group_id_index_in_group (group_id, index_in_group)` on the queries filtering by `group_id` alone.
- A partial index `WHERE member_role IN ('moderator','admin','owner')`: SQLite cannot match bound parameters against the index's list, so it is not used.

`(user_id, member_role, group_id)` does not tie for any current query: for a query without a role filter it offers at most its `user_id` prefix, which is costed worse than the two-column match of `idx_group_members_group_id` and is wider than `idx_group_members_user_id`; for a role-filtered query that also filters by `user_id` and `group_id` it matches all three columns. A future query reading only `user_id`, `member_role` and `group_id` could choose it as a covering index.

## Plans

`EXPLAIN QUERY PLAN` of the 150 queries touching `group_members` in `chat_query_plans.txt`, before and after, without table statistics (the app does not run `ANALYZE`), on SQLite 3.39.2 (the version the apps bundle) and 3.40.1, with the new index created first and last:

- 4 role-filtered query shapes use the new index on all three columns: `member_role IN (?,?,?)` (`getGroupModerators`, `getGroupRosterMembers`), `member_role IN (?,?)` (`getGroupAdminsMods`), `member_role = ?` (`getGroupOnlyMembers`, `getGroupOwners`) and the relay query (`getGroupRelayMembers`).
- The other two role-filtered app queries keep their plans: the support-scope member lookup by `group_id` and `index_in_group` (unique index) and the relay count by `group_id`.
- No other plan changes, including 4 test-only queries filtering by `member_role` alone.

## Row order

On SQLite, queries without a role filter read the same index as before and return the same rows in the same order. Single-role queries return rows in row-id order, as before.

On SQLite, the `member_role IN (...)` queries now return members grouped by role, in the text order of the role names (admin, member, moderator, owner) and by row id within a role, as any index with `member_role` before the row id would. Which members are returned does not change. Group messages are sent to admins and owners first and then to the other recipients, each in shuffled order (`sendGroupSignedMessages_`), and introductions to moderators are shuffled too (`introduceMember`), so the new order matters only where a list is used as it is:

- `getGroupModerators`: the order in which a relay forwards a support-chat message to moderators.
- `getGroupAdminsMods`: when a member joins a channel, the order in which the member receives the introductions of admins and moderators, and so the order in which it stores them.
- `getGroupRosterMembers`: the order of entries in a channel's roster (members, moderators and admins) and of the member role events for members reverted when one is applied. `buildGroupRoster` keeps the first `maxGroupRosterSize` (256), but there are never more: a role change that would exceed 256 is refused, and subscribers join as observers.

## Cost

One more index on `group_members`, updated when a member is added or deleted or changes role; creating it took 0.2–2 s on 46,000 rows in our runs.
