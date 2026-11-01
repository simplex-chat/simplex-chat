# Unread counts of chat profiles

## Problem

Switching chat profiles is slow when any profile has many messages, also when switching to a profile with few chats.

## Cause

On every switch the apps send `/users` (`ListUsers`) before loading the chats: `changeActiveUser_` in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`, `changeActiveUserAsync_` in `apps/ios/Shared/Model/SimpleXAPI.swift`.

`getUsersInfo` (`src/Simplex/Chat/Store/Profiles.hs`) counts each profile's unread items with two queries, one joining `contacts`, one joining `groups`. Their plans use covering indexes narrowed by `user_id` only (`idx_chat_items_contacts`, `idx_chat_items_group_scope_stats_all`), so every chat item of every profile is read. `idx_chat_items_user_id_item_status (user_id, item_status)` narrows by status but contains neither `contact_id` nor `group_id`.

## Fix

Migration `20261101_chat_items_unread_index`, SQLite and Postgres:

```sql
DROP INDEX idx_chat_items_user_id_item_status;
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status, contact_id, group_id, user_mention);
```

Down migration: the index on `(user_id, item_status)`.

Both counts read only the profile's unread items: `SEARCH i USING COVERING INDEX idx_chat_items_user_id_item_status (user_id=? AND item_status=?)`. The query texts are unchanged.

Test `testUsersUnreadCountQueryPlans` (`tests/ChatTests/Direct.hs`, SQLite): runs `/users` and checks the plans of the two executed count queries.

## Scope

- `chat_query_plans.txt`: 21 plans change.
  - The 2 counts.
  - 11 chat list queries of contacts' unread counts (`getChatPreviews`): `(user_id=? AND item_status=? AND contact_id>?)` instead of `(user_id=? AND contact_id>?)`.
  - 3 per-contact queries with `user_id`, `contact_id` and `item_status`: the new index in place of `idx_chat_items_contacts`.
  - 3 message search queries of `getAllChatItems`: the new index in place of `idx_chat_items_groups_item_viewed`, both narrowed by `user_id` only.
  - 2 queries narrowed by `user_id` only (files of a profile; user deletion): `idx_chat_items_timed_delete_at` in place of the old index.
- `chat_schema.sql`: the index is listed last (SQLite); its columns change (Postgres).
- `tests/SchemaDump.hs`: the migration is in `skipComparisonForDownMigrations`, as the down migration lists the index last.
