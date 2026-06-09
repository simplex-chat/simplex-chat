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

## Build

`bash /home/user/build/linux.sh` -> `SimpleX_Chat-x86_64-fix-support-chat-unread-on-group-read.AppImage`
(library compiled clean). The test-suite typecheck was OOM-killed during the library
rebuild before reaching the test modules; the test compiles by inspection against the
sibling `testScopedSupportUnreadStatsOnRead` but is not yet machine-verified.
