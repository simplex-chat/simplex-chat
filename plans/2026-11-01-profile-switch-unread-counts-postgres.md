# First unread message lookup on Postgres

## Problem

On Postgres, with the `GROUP BY user_id` of `getFirstUnreadItemId_` (`/_get items initial=`), the first unread message of a profile with 10,000 unread messages took 16 ms warm and 59 ms cold, against 0.1 ms on master.

## Cause

With `GROUP BY user_id`, the Postgres planner reads the unread messages of all profiles in `idx_chat_items_item_status`. Without it, the planner reads `chat_items_pkey` up to the first matching row, on master's and on the new index set alike.

## Fix

`GROUP BY user_id` is appended on SQLite only (`#if !defined(dbPostgres)`), as `FOR UPDATE` is appended on Postgres only in `getDirectChatItemLast`. On Postgres the query text equals master's.

## Scope

- `src/Simplex/Chat/Store/Messages.hs`: `getFirstUnreadItemId_`.
- `chat_query_plans.txt`: the SQLite query text of `getFirstUnreadItemId_` ends with ` GROUP BY user_id` after the quasi-quoted text; its plan is unchanged.
