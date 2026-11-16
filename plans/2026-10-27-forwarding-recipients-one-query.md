# Forwarding in large groups holds the chat database for seconds (SQLite)

## Problem
In a group without relays, the admin who forwards members' messages held the single chat database connection
on SQLite for about 3 s per forwarded message with 20,000 recipients (SQL only); every other database
operation waited (opening chats, sending, receiving). Channels (with relays): 1.5 s per bucket of 10,000.

## Cause
- `runDeliveryJobWorker` (Subscriber.hs:4302), `buildMemberList` (:4459): takes the sender's `MRIntroduced`
  indexes and loads them with `getGroupMembersByIndexes` (Store/Groups.hs:1252) or, for a support scope,
  `getSupportScopeMembersByIndexes` (:1266), in one `withStore'`.
- Postgres: one `index_in_group IN ?` query. SQLite: `mapM` over the indexes, one `groupMemberQuery` each.
- Each query already uses the unique `(group_id, index_in_group)` index; the cost is their number. 73–90% of
  the time is preparing the same 90-column, 4-table statement again for every member.
- Channels: `getGroupMembersByCursor` (Store/Delivery.hs:350) selects up to `deliveryBucketSize` (10,000) ids,
  then on SQLite loaded each with `getGroupMemberById`, one query each, in one transaction.

## Fix
- SQLite runs the Postgres predicate in one statement with the list as one JSON array (`encodeJSON`):
  `IN (SELECT value FROM json_each(?))`; channels keep `ORDER BY m.group_member_id ASC`, with no temp b-tree.
- Postgres query texts are unchanged. `getGroupMemberByIndex` and `getSupportScopeMemberByIndex` are removed.
- An empty introduced list (`buildMemberList`) or cursor page returns `[]` without a query, on both databases.
- Same members: missing rows were dropped by `rights` before; removed, left, pending and own rows are
  filtered by the caller as before; `getRelationsIndexes` has no duplicates. Order without relays does not
  matter: `deliver` (Subscriber.hs:4481) sends the body with the first connection, whatever the order
  (Postgres order was already unspecified). Channel order stays ascending, as `last mems` (:4409) requires.
- The channel load no longer filters `m.user_id`: the ids come from the group's own members (INFERENCE from
  the schema and id source; Postgres had no such filter). `chat_query_plans.txt` updated for all three texts.

## Approaches considered
C harness: SQLCipher 4 of the app's `direct-sqlcipher` (SQLite 3.39.2), app pragmas, full `groupMemberQuery`,
one transaction; copy of a 40,005-member group (ready connections, 30 moderators/admins), random ascending
index sets; median of 5; desktop x86-64, warm cache; SQL only, phones are slower. ms at 100/1k/5k/20k indexes:
- Before, one query per member: 14.3 / 199.4 / 798.6 / 2,973.4; support scope 15.0 / 176.6 / 798.4 / 3,068.9.
- Chosen, `json_each`: 1.3 / 51.9 / 123.3 / 263.1; support 0.3 / 16.6 / 34.8 / 53.5; constant query text.
- `IN (?,…)` per index, 500 or one chunk: 1.3–1.4 / 50–52 / 125–127 / 271–278; text varies by list length:
  one `DB.query` statistics entry and one `chat_query_plans.txt` entry per list length.
- `IN (?,…,?)` padded with `NULL` to a fixed size: constant text, but more code (padding, chunking).
- Statement prepared once, reused: 1.4 / 53.3 / 128.8 / 298.7; low-level API, bypasses `DB.query` statistics.
- Support via `getGroupModerators` + intersection: 0.6 ms with unmerged `member_role` index, 41–44 without;
  moves logic into the delivery worker, needs the scope member loaded and de-duplicated. Possible follow-up.
- Status/connection filter in SQL: same number of statements; the caller filters in memory already.
- Channels, id selection and member load in one statement: 343 ms vs 212 ms (temp b-tree; not re-run).
Support-scope rows had the scope member never in the index set: they measure lookup, not row fetching.

## Tests
- Added: none. `testChannels1RelayDeliverLoop` (tests/ChatTests/Groups.hs:9959, bucket sizes 1, 2, 3) covers
  paging, full and partial last buckets and cursor order. No test exceeds 10,000 members or checks timing.
- Not run: Haskell test suite and `chat_query_plans.txt` check (CI only). Not compiled here, so no new test.

## Open questions
- Channels: the other per-recipient work in `sendLoop` was not measured; relays may run on Postgres.
- Channel support scopes call `getGroupModerators` per message: 42 ms at 40,000 members without `member_role`.
- `SEGroupMemberNotFoundByIndex` (Store/Shared.hs:104) is no longer produced; kept for the API encoding.
- Add a test with a bucket size below the member count and a moderator or owner among recipients?
