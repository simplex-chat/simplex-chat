# Orphan `prepared` connections after a failed connect via a link
## Problem
A failed connect via a long address, one-time or group link (server unreachable) leaves one row per link
that is not in the chat list, so it cannot be deleted from the UI. For address and group links its receive
queue on the user's own server is counted in server stats and subscribed again on every app start.
## Cause
- `connect'` writes the row (`createConnReqConnection`, `Library/Commands.hs:3970`), then runs `joinContact`
  (`:3971`), the only step that can fail. The row is `ConnPrepared`; with `preparedEntity_ = Nothing` (every
  `APIConnect`) `conn_type = contact`, no contact (`Store/Direct.hs:229`). `joinNewConn` has the same order.
- Keeping the row is intended: a retry with the same link finds it by `cReqHash` and reuses its keys
  and `xContactId` (#5018, `9a87f344b`; `testRetryConnectingViaContactLink`).
- The same commit added `conn_status != prepared` to `getContactConnectionChatPreviews_`; with
  `conn_type = contact AND contact_id IS NULL` it hides only this orphan and relay test rows.
- Prepared contacts and groups have a contact or a member. `APIConnect` with a short link (terminal `/c`, app
  fallback when the plan returns no link data) leaves the same row; a short link whose server is unreachable
  fails in `getShortLinkConnReq` before any row is written.
## Fix
1. Core: `AND relay_test = 0` (`Store/Messages.hs:1200`). The relay's test row (`accepted`,
   `Library/Subscriber.hs:1683`) is listed on master until `cleanupStaleRelayTestConns` (`Commands.hs:6133`;
   30 s after start, then every 30 min, rows older than 5 min). The tester's row (`:1743`) is `requested` from
   CONF to the relay test handler: milliseconds, or until the cleanup if the app is killed before the reply.
   Pre-existing; it comes first, else commit 2 lists the tester's prepared row for the whole relay test
   (up to the 40 s timeout).
2. Core: remove the `conn_status != ?` filter. `deleteChat CTContactConnection` (`:1390`) deletes a row of
   any status and deletes the agent connection first, which removes the queue. Both apps render `prepared`
   as not initiated. Existing orphans appear with no migration.
3. Android, desktop: `ContactConnectionInfoView.kt:42` sets `showingInvitation` only when `initiated`; before,
   a joined one-time link showed the peer's link under "Share this 1-time link" and offered to delete it on
   close, and commit 2 keeps such rows reachable. iOS creates it only in `createInvitation`.
- Limitation, left as is: delete during an in-flight join leaves a ghost row in the client until reload.
## Approaches considered
- Delete the connection when the join fails: a one-time link whose queue is already secured breaks on retry
  (`testRetryConnectingClientTimeout`); an address retry after a timeout sends a new `xContactId`, a
  duplicate request; existing orphans stay.
- Delete only on permanent errors: `HOST` is grouped with temporary errors (`temporaryOrHostError`).
- Expire such rows in the cleanup manager: drops the retry keys without telling the user.
- Chosen: list the rows (one predicate), delete as any pending connection; #5018 retry unchanged. Cost:
  every failed long-link connect leaves a visible "connecting" entry until deleted or retried.
## Tests
- Added: `testChatRelayTest` asserts `bob @@@ []` on the relay after a relay test (listed on master);
  `testFailedConnectViaLinksInChatList`: three failed joins listed, deleted, list empty;
  `testFailedConnectDeletesRcvQueue` (SQLite only): 1 `rcv_queues` row after the failed join, 0 after delete.
- Verified: old vs new predicate per row shape and `EXPLAIN QUERY PLAN` of three pagination variants
  (unchanged) on a SQLite schema copy; Postgres `relay_test` is `smallint DEFAULT 0 NOT NULL`; `/chats`
  passes `withPCC = False`; by reading, no existing test asserts a chat list after a failed raw-link connect.
- Not run: Haskell tests locally (CI only); `testFailedConnectDeletesRcvQueue` has never run. Not tested:
  iOS, Postgres, a relay test against a real chat relay in the GUI, delete while a join is in flight.
## Open questions
1. A failed row appears only after the next chat list load (no event contains a `PendingContactConnection`);
   a follow-up PR re-fetches the list without `getUserChatData`, which resets the tag filter?
2. Onboarding hides the list for a profile with no conversation (pre-existing, also for joined pending rows;
   `ChatListView.kt:452`, `OnboardingCards.kt:52`, iOS `ChatListView.swift:385`, `:420`); count them? (product)
3. A failed attempt reads like a pending one; UX PR with "connection not completed" for `prepared`
   (not "failed": prepared also while a join is in flight)?
4. The relay test case is covered only by the Haskell assertion; no GUI reproduction. Enough?
