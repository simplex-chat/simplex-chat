# Chat relay test connections in the chat list

## Problem
A chat relay test creates a temporary connection on both sides. On the relay, that connection is listed in the
chat list as a pending connection until the stale-test cleanup removes it, up to 35 minutes later.

## Cause
- The relay's test connection is inserted as `accepted` with no contact (`createRelayTestConnection ... ConnAccepted`,
  `Library/Subscriber.hs:1683`).
- The chat list query for pending connections (`getContactConnectionChatPreviews_`, `Store/Messages.hs:1184`) selects
  `conn_type = contact AND contact_id IS NULL` and excludes only `prepared`, so it lists the relay's row.
- `cleanupStaleRelayTestConns` (`Library/Commands.hs:6520`) deletes rows older than 5 minutes, on a pass 30 s after
  start and then every 30 minutes.
- The tester's row is created `prepared` (`Library/Commands.hs:1745`). Its status is set to `requested` on CONF, just
  before the test command deletes it, so it is listed for milliseconds, or until the cleanup if the app is killed in
  between.

## Fix
Add `AND relay_test = 0` to the chat list query.

## Approaches considered
- Delete the relay's row as soon as the test reply is sent: an interrupted test would still leave a listed row until
  the cleanup.
- Insert the relay's row with another status: other code reads that status; the flag that marks the row already exists.

## Scope
Core only; both apps get the list from this query. Without this change, removing the `prepared` filter (next change)
would list the tester's row for the whole test, so this change comes first.
