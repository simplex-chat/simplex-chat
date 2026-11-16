# Group members: two member lookup indexes

## Problem
In a large group, sending in a member support chat is slow, although a support message only goes to the
moderators and the member. Each group chat load also reads every member row of the group.
## Cause
- `getGroupModerators` (`Store/Groups.hs:1298`; support sends, `Library/Internal.hs:1953`, `:1963`) filters
  `member_role IN (?,?,?)`. No index has `member_role`: it reads all group rows via `idx_group_members_group_id`.
- Reporter's device: 178 ms average, 4.8 s max over 80 calls (`getGroupRosterMembers` runs the same text), while
  holding the single connection (`DBStore.dbConnection :: MVar`), so all other database operations wait.
- `getGroupInfoRow` (`Store/Shared.hs:950`) filters `mu.user_id = ?` (#7688, to avoid the quadratic
  `idx_group_members_contact_id` plan), so it reads all group rows: no index has `group_id` and `contact_id`.
## Fix
- Migration `20261027_member_role_index` adds the role index `(user_id, member_role, group_id)` (Postgres: led by
  `member_role`) and, after it, the contact index `(group_id, contact_id) WHERE contact_id IS NOT NULL`.
- `updatePublicMemberCount` (`Store/Groups.hs:2324`) counted relays by `group_id` and `member_role` only; it now
  also filters by `user_id` (all callers pass the group's user, every member row has it): it uses the role index.
- Plans: the 4 role-filtered shapes and the relay count use the role index. `getGroupInfoRow` (text unchanged) and
  `getConnectionEntityKeys` (`Store/Connections.hs:187`, per agent event) become `(group_id, contact_id)` lookups.
- The members returned do not change; single-role queries and queries without a role filter keep their order.
  On SQLite `member_role IN (...)` queries return members grouped by role (text order), then by row id. Only
  `getGroupAdminsMods` (joining channel member intros) and `getGroupRosterMembers` (roster, ≤256) use that order.
- Write cost: the role index is updated on member insert, delete and role change; the partial contact index
  only for rows with a contact (most member rows have none). Creation: 0.2–2 s and 0.4–0.7 s on ~46,000 rows.
## Approaches considered
- `(user_id, group_id, member_role)` replacing the old index: `/ms` changes order, `getGroupMemberIdByName` can
  return another member; next to it: same cost, plan picked by schema position.
- Led by `group_id`: ties with `idx_group_members_group_id_index_in_group`. Partial on moderator roles: unused.
- Postgres `(user_id, member_role, group_id)`: without statistics it ties with `idx_group_members_group_id` for
  `user_id = ? AND group_id = ?`, the newest index wins, so `/ms` listed members by role (2 tests failed). Led by
  `member_role`, it is used only with a role filter. `ORDER BY` in `getGroupMembers` would fix only `/ms`.
- Contact candidates A `(user_id, group_id, contact_id)`, B `(group_id, contact_id)`: up to 9 plans change.
- C `(contact_id, group_id)`, D `(user_id, contact_id, group_id)`, E `(contact_id, user_id, group_id)`: 3-6 plans.
- Partial F `(user_id,group_id,contact_id)`: does not improve `getConnectionEntityKeys`. Partial H
  `(group_id,user_id,contact_id)`: fast for `getGroupInfoRow`, no point lookup for `getConnectionEntityKeys`.
- Dropping `mu.user_id = ?`: removes the #7688 fallback for when the new index is not used.
- Chosen: the role index, next to `idx_group_members_group_id`, ties with no current query. The contact index
  changes 2 plans, both to point lookups, and is the narrowest; its `getGroupInfoRow` plan depends on index
  order, so the migration creates it last (`VACUUM` and `sqlcipher_export` keep the new plan).
## Tests
- Added: none; `chat_query_plans.txt` and both `chat_schema.sql` files updated.
- `EXPLAIN QUERY PLAN` of the 150 `group_members` queries and all 860 entries, no `ANALYZE`, SQLite 3.39.2 and
  3.40.1, new index first and last: only the changes above. Partial index use checked with bound parameters.
- 20,000-member group, SQLite 3.40.1, warm, median of 25 runs: `getGroupModerators` 10.7 → 0.15 ms,
  `getGroupAdminsMods` 9.9 → 0.09 ms, `getGroupOwners` 9.1 → 0.05 ms. 40,005-member group: relay count ~40 →
  ~1 ms, `getGroupInfoRow` 40.0 → 0.19 ms (sqlcipher CLI 3.39.2; Python 3.40.1: 9.1 → 0.32 ms), 2-member group
  0.24 → 0.24 ms; `getConnectionEntityKeys` (3,001 groups) 1.3–1.4 → 0.36–0.38 ms (Python: 1.55 → 0.48 ms).
- Schema tests (schema dump, down migrations, foreign-key lint) passed on the role-index-only version (PR body).
- Not run: Postgres plans (INFERENCE), the full Haskell suite (CI only), iOS, a real 20,000-member group.
## Open questions
1. Speed-up rests on lookup timings, not on a measured send. Measure on a phone? Postgres plans on a server?
2. Role-filtered queries return members grouped by role on SQLite. Acceptable?
3. `getRemovedMembersToCleanup` (`Store/Groups.hs:1375`) still reads all member rows of the user. Separate PR?
4. `M20261020_link_notices` (f39002a14) and `M20261021_badge_subscriptions` (f/badge-subscriptions) sort
   earlier: rename them if this merges first?
