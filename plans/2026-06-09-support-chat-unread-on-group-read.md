# Fix: support-chat unread stats not cleared after no-scope group read

Branch: `nd/fix-support-chat-unread-on-group-read` (off `origin/master`)

## Symptom

In a group with member admission ("chat with admins"), some members keep showing
as unread in the *chats with members* list even after the admin opens their support
chat and reads everything. Reopening never clears them. It is most visible for
members who have already been given a role (e.g. `member`), because pending members
always render the flag icon anyway, so the stuck badge is invisible for them.

## Root cause

The *chats with members* badge is driven by the per-member counters
`support_chat_items_unread / _member_attention / _mentions` on `group_members`
(surfaced as `GroupSupportChat`, `Types.hs:1087`). These are denormalized counters,
maintained by increment on receive and decrement on read.

Reading a group **without a scope** (`APIChatRead` with `scope = Nothing`,
`Commands.hs:1153-1166`) runs `updateGroupChatItemsRead` (`Store/Messages.hs:2078`),
whose `UPDATE` filtered only by `group_id`:

```sql
UPDATE chat_items SET item_status = RcvRead, ...
WHERE user_id = ? AND group_id = ? AND item_status = RcvNew
```

This flips **support-scope** items to `RcvRead` too, but never touches the
`support_chat_items_*` counters. The result is a permanent desync:

- the items are read (so the chat "looks" fully read), but
- the per-member counters stay > 0 (so the member stays "unread" in the list), and
- reopening the support chat can't fix it: the scoped read decrement
  (`updateGroupScopeUnreadStats`, `Store/Messages.hs:2202`) only fires on a real
  `RcvNew -> RcvRead` transition, and there are no `RcvNew` items left.

This is inconsistent with how the group's main unread **count** is computed, which
already excludes support items (`Store/Messages.hs:916`,
`... AND group_scope_tag IS NULL AND group_scope_group_member_id IS NULL`). The read
path simply forgot the same predicate.

Note: `member_attention` self-heals on open via `checkSupportChatAttention`
(`Commands.hs:593-613`), recomputing it from loaded items. `unread` and `mentions`
have **no** such correction, which is why the badge persists.

## Fix (Option A — prevent new corruption)

Add the main-scope predicate to the two queries in the no-scope group-read path so
that reading a group without a scope leaves support-scope items (and their
disappearing-message timers) entirely untouched:

- `updateGroupChatItemsRead` (`Store/Messages.hs`) — the read itself.
- `getGroupUnreadTimedItems`, `Nothing` branch (`Store/Messages.hs`) — its companion
  timed-items query.

```sql
AND group_scope_tag IS NULL AND group_scope_group_member_id IS NULL
```

Both lines are required and neither is droppable:

- Without the read filter, support items get marked read (the bug).
- Without the timed-items filter, a disappearing support message would have its
  delete timer started by a main-group read while still unread — a *new*
  inconsistency introduced by a half-fix. The no-scope `APIChatRead` handler
  (`Commands.hs:1160-1166`) calls `getGroupUnreadTimedItems` then
  `setGroupChatItemsDeleteAt` then starts deletion threads, so both must agree that
  support items are out of scope.

Support items continue to be read (and their counters decremented) the correct way:
via the scoped read paths (`updateSupportChatItemsRead` / `updateGroupChatItemsReadList`
with a `MemberSupport` scope), exercised when the support chat is opened.

### Why this is consistent / safe

- Selects exactly the main-scope rows, identical to the authoritative unread-count
  query (`Store/Messages.hs:916`). No main item is ever wrongly excluded — inserts
  always set `group_scope_tag` and `group_scope_group_member_id` together
  (`Store/Messages.hs:600-603`).
- `groups.members_require_attention` becomes *more* consistent, not less: the
  no-scope read no longer zeroes support items behind a still-positive group counter.
- The added predicate is constant, parameterized SQL — no injection surface — and is
  the existing codebase idiom (916 / 1214 / 1524 / 1745).

## Test

`tests/ChatTests/Groups.hs :: testScopedSupportUnreadStatsGroupReadNoScope`
(registered under the support-chat `describe` block).

A member sends one support message (`markRead = False`), the admin marks the whole
group read with `/_read chat #1` (no scope), then reads the item in scope with the
**per-item** read `/_read chat items #1(_support:2) <id>`. The per-item read only
decrements on a genuine `RcvNew -> RcvRead` transition, so:

- on master the no-scope read already consumed the item -> decrement is a no-op ->
  `unread: 1, require attention: 1` (test fails);
- with the fix the item is still unread -> decrement applies ->
  `unread: 0, require attention: 0` (test passes).

The full scoped read `/_read chat #1(_support:2)` is deliberately *not* used to
assert this, because `updateSupportChatItemsRead` hard-zeroes the counters
regardless of item state and would mask the bug.

## Performance

The added predicate makes both queries **faster**, not slower, and needs no new
index. Verified two ways.

### Query plan (EXPLAIN QUERY PLAN, via the checked-in plan snapshots)

Regenerating `chat_query_plans.txt` shows both statements switch index:

```
- SEARCH chat_items USING INDEX idx_chat_items_groups_user_mention
    (user_id=? AND group_id=? AND item_status=?)                         -- 3-column seek
+ SEARCH chat_items USING INDEX idx_chat_items_group_scope_stats_all
    (user_id=? AND group_id=? AND group_scope_tag=? AND
     group_scope_group_member_id=? AND item_status=?)                    -- 5-column seek
```

SQLite compiles `group_scope_tag IS NULL` / `group_scope_group_member_id IS NULL`
into equality constraints (`=?`), so the two new columns *deepen the index seek*
rather than adding a post-seek filter or scan. The read now uses
`idx_chat_items_group_scope_stats_all`
(`user_id, group_id, group_scope_tag, group_scope_group_member_id, item_status, …`,
`M20250721_indexes.hs`), which already exists — this is the same index the
authoritative unread-count query uses. No migration, no new index.

Note: master was **not** degraded here — it already got a full 3-column seek via
`idx_chat_items_groups_user_mention`. So this is a modest improvement, not the repair
of a regression. The plan file only records two hunks (the two flipped statements);
the `saveQueryPlans` test also churns an unrelated `group_members` count entry and the
whole `agent_query_plans.txt`, both of which were reverted to keep the diff to exactly
those two queries.

### Micro-benchmark (synthetic 200k-row group)

On a synthetic group of ~200k `chat_items` where ~25% are support-scope, running the
old vs. new `UPDATE` in alternating trials:

| Query           | mean wall-clock |
|-----------------|-----------------|
| old (no filter) | ~10.5 s         |
| new (scoped)    | ~7.4 s (≈ −29%) |

The speedup tracks the row reduction: the new query touches only the ~75% main-scope
rows, so it does ~25% less work and the tighter seek trims a bit more. This also
directly demonstrates the bug — the old query flips ~50k support rows to `RcvRead`
(without decrementing counters); the new one flips **0**.

Caveat: absolute timings were noisy (concurrent GHC builds on the box); only the
*relative* comparison across alternating trials is reliable, and it was consistent in
direction and magnitude across runs. The gain scales with the support-item share — a
group with no support chats sees no measurable change.

## Limitations (accepted, by decision)

1. **No retroactive repair.** This change only prevents *new* corruption. Rows already
   stuck (counter > 0 while the items are `RcvRead`) stay stuck until either the
   per-member "Mark read" action (`apiSupportChatRead` -> `updateSupportChatItemsRead`,
   which hard-zeroes) or, for `member_attention` only, the self-heal on open
   (`checkSupportChatAttention`). A one-shot migration recomputing the three counters
   from `chat_items` was considered and **intentionally not done**.

2. **`setUserChatsRead` is not fixed.** The global "mark all chats read"
   (`APIUserRead` -> `setUserChatsRead`, `Store/Direct.hs:640-646`) has the same flaw:
   its `UPDATE chat_items ... WHERE user_id = ? AND item_status = ?` has no chat-type
   and no scope filter, so it reads support items user-wide without decrementing the
   counters. It is **left as-is on purpose**: no GUI client invokes `APIUserRead`
   (it exists only as the Haskell command type `Controller.hs:357` and the CLI
   binding), so it is not reachable from Android / desktop / iOS — only the terminal
   CLI. The same one-line predicate would close it if a client ever exposes it.

3. **`getUpdateGroupItem` relies on a UI contract.** The per-item read
   (`updateGroupChatItemsReadList` -> `getUpdateGroupItem`, `Store/Messages.hs:2178`)
   has no scope filter on its `UPDATE` and its `Nothing` branch decrements nothing, so
   passing a support item ID with `scope = Nothing` would reproduce the desync. This is
   defended only by the in-code assumption ("we rely on UI to not pass item IDs from
   incorrect scope", `Store/Messages.hs:2162-2163`), not by the query. Not reachable in
   normal flow; left unchanged to keep the diff surgical.

## Files changed

- `src/Simplex/Chat/Store/Messages.hs` — two one-line scope predicates.
- `tests/ChatTests/Groups.hs` — one regression test + its `it` registration.

## Build & test run

Library compiled clean via `bash /home/user/build/linux.sh`. The full test suite was
subsequently built and run against this branch (968 examples). The regression test
`should not read support chat items when reading group without scope` passes with the
fix and fails when the fix is reverted (confirmed by rebuilding master and re-running),
so it genuinely catches the desync.

The suite had 8 failures on the run; each was classified as flaky/environmental,
by-design, or pre-existing on master — none attributable to this change. In particular
the timed-message failure (`both users have configured timed messages … restart`) was
reproduced identically on master with the fix reverted (3/3), proving it pre-existing;
it is a direct-chat test that never touches the group-scope read path.
