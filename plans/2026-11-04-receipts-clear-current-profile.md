# Clearing receipt overrides: current profile only

## Problem

Settings → Privacy → "Send delivery receipts to": switching Contacts or Small groups and choosing "Enable for all" / "Disable for all" also resets the per-chat receipt settings of every other profile. The section footer states "These settings are for your current profile".

## Cause

`updateUserContactReceipts` and `updateUserGroupReceipts` (src/Simplex/Chat/Store/Profiles.hs) set `send_rcpts = NULL` in `contacts` and `groups` without a `user_id` condition when `clearOverrides` is set.

## Fix

- `updateUserContactReceipts`: `UPDATE contacts SET send_rcpts = NULL WHERE user_id = ?` with the target user's `userId`.
- `updateUserGroupReceipts`: `UPDATE groups SET send_rcpts = NULL WHERE user_id = ?` with the target user's `userId`.
- chat_query_plans.txt: both entries replaced; `SCAN` becomes `SEARCH ... USING INDEX sqlite_autoindex_contacts_2 (user_id=?)` and `sqlite_autoindex_groups_2 (user_id=?)`.
- Tests: `testClearReceiptsOverridesCurrentUser` (tests/ChatTests/Direct.hs) and `testClearGroupReceiptsOverridesCurrentUser` (tests/ChatTests/Groups.hs). A second profile has a receipts override that differs from its default; the first profile clears its overrides; the second profile still sends receipts by its override, and the first profile's override is cleared.

## Scope

- Commands: `/_set receipts contacts|groups <userId> on|off clear_overrides=on` and `/set receipts contacts|groups on|off clear_overrides=on`.
- SQLite and Postgres share the statements.
- Apps, API and schema unchanged.
- Overrides reset by earlier versions are not restored.
