# Chat relay test connections in the chat list

## Problem
During a chat relay test, a temporary connection is created on both sides. On the relay, that connection is listed in
the chat list as a pending connection until it is removed by the stale-test cleanup, up to 35 minutes later.

## Cause
- The relay's test connection is inserted as `accepted` with `contact_id` NULL (`createRelayTestConnection ...
  ConnAccepted`, `Library/Subscriber.hs:1683`).
- In the pending-connection query (`getContactConnectionChatPreviews_`, `Store/Messages.hs:1184`), rows with
  `conn_type = contact AND contact_id IS NULL` are selected and only `prepared` rows are excluded, so the relay's row
  is listed.
- In `cleanupStaleRelayTestConns` (`Library/Commands.hs:6520`), rows older than 5 minutes are deleted, on a pass 30 s
  after start and then every 30 minutes.
- The tester's row is created as `prepared` (`Library/Commands.hs:1745`). Its status is set to `requested` on CONF,
  shortly before the row is deleted by the test command, so it is listed for milliseconds, or until the cleanup if the
  app is killed in between.

## Fix
`AND relay_test = 0` is added to the pending-connection query.

## Scope
Core only; both apps get the list from this query. This change is applied before the `prepared` filter is removed
(next change); otherwise the tester's row would be listed for the whole test.
