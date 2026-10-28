# Failed joins that were not delivered

## Problem
After a failed connect via a link, the connection is kept even when nothing reached the peer and no reply can arrive
for it. Such a row is listed as a pending connection until the user deletes it, and for address and group links its
receive queue on the user's own server is subscribed again on every app start.

## Cause
- In `connect'` (`Library/Commands.hs:3976`) and `joinNewConn` (`:3903`), the row written before the join is kept for
  every error.
- In simplexmq (pinned `c0c416daf`, `Client.hs`), some errors occur only before a command is written to a server:
  while a connection is established, its failure is `NEConnectError`, `NETLSError`, `NEUnknownCAError` or
  `NETimeoutError` (`:581`, `:619-627`), and `HOST` is returned when no transport host can be chosen (`:577-582`).
  A command already written fails with `NEFailedError` (`failPendingRequests`, `:657-661`) or `TIMEOUT` (`:1382-1402`).
- `AUTH` is returned when a queue does not exist or is secured with another key.
- For a one-time link, the peer's queue is secured (SKEY) before the reply queue is created and the confirmation is
  sent, and `snd_queues.status` is set to `Secured` after SKEY (`Agent.hs:4404-4411`, `:4424-4431`).

## Fix
In both first-attempt paths, for a connection without a prepared contact or group, the agent connection and the row
are deleted, as for a deletion by the user, when the join fails with one of:
- `BROKER _ HOST`;
- `BROKER _ (NETWORK e)`, `e` one of `NEConnectError`, `NETLSError`, `NEUnknownCAError`, `NETimeoutError`;
- `SMP _ AUTH`.

For a one-time link, the deletion also requires that no snd queue of the connection is `Secured`
(`getConnectionServers`). The error is returned to the user in all cases. Retries of a kept row are unchanged.

## Scope
Core only. Errors reported by a private-routing proxy are kept: the proxy encodes `NETWORK` without its detail
(`Protocol.hs:2254`), and it is decoded as `NEFailedError` (`:2263`). Rows written before this change are listed and
deleted by the user.
