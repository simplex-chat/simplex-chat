# Speed in large groups and in profiles with many messages

One pull request for three database fixes, each moved from its own pull request.

## Problems

1. In a large group, a send in a member support chat is slow and delays every other chat action: the moderator lookup reads every member row of the group while it holds the only database connection. Reporter's device: 178 ms on average, 4.8 s at most, over 80 lookups.
2. In a large group without relays, the admin's app holds the database for up to seconds on every forwarded message: on SQLite, each recipient is loaded by its own query. A channel bucket of 10,000 recipients: about 1.5 s.
3. A profile switch is slow when any profile has many messages: the unread counts of the profile list read every message of every profile. Desktop, 2,000,024 messages, cold cache: 43.9 s.

## Approach

| # | Source | Change | Measured, master → fix |
|---|---|---|---|
| 1 | https://github.com/Narasimha-sc/simplex-chat/pull/36 | migration `20261027_member_role_index`: a role index, and a partial index on `(group_id, contact_id)`; the relay count filters by `user_id` | moderator lookup, 20,000 members: 10.7 → 0.15 ms<br>group info row, 40,005 members: 40.0 → 0.19 ms<br>support send, desktop fixture: 0.392 → 0.204 s |
| 2 | https://github.com/Narasimha-sc/simplex-chat/pull/61 | SQLite loads the recipients of a forwarded message and of a channel bucket by one `json_each` query; an empty list runs no query | 1,000 recipients: 199.4 → 51.9 ms<br>20,000 recipients: 2,973 → 263 ms<br>channel bucket of 10,000: 1.5 s → 212 ms |
| 3 | https://github.com/simplex-chat/simplex-chat/pull/7969 | migration `20261101_chat_items_unread_index`: the index `(user_id, item_status)` extended by `contact_id`, `group_id` and `user_mention` | profile counts, 2,000,000 messages, warm: 0.52 → 0.0044 s<br>desktop cold switch: 43.9 → 0.36 s |

The three source branches are merged unchanged. The migrations follow `20261021_badge_subscriptions`, the last on master, in date order.

## Open questions

1. The role index is named and ordered differently on SQLite (`idx_group_members_user_id_member_role`) and Postgres (`idx_group_members_member_role`). Keep, or one name on both?
2. On SQLite, role-filtered queries return members grouped by role. Acceptable?
3. Support scopes in channels look up moderators per message; switch them to moderators and intersection on the new role index here, or later?
4. The migration of fix 3 on a 2.6 GB database: about 100 s, then the `VACUUM` that the apps run after any migration: 640 s, 2.4 GB peak memory. A separate issue before the next release?
5. Postgres plans for fixes 1 and 2 are not measured on a real database.

## Merge order

One pull request; within it, 1 (indexes), then 2 (queries), then 3 (profile counts).
