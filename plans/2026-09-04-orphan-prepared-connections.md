# Connections left by failed link joins

## Problem
A failed connect via an old-format (long) contact address, one-time or group link, for example when its server is
unreachable, shows an error and leaves a connection that the chat list never shows, so the user cannot delete it.
For address and group links its receive queue on the user's own server is subscribed again on every app start.

## Cause
- `connect'` writes the row (`createConnReqConnection`, `Library/Commands.hs:3975`), then runs `joinContact` (`:3976`),
  the only step that can fail. The row is `prepared`, `conn_type = contact`, with no contact. A one-time link joined
  via `APIConnect` (`joinNewConn`, `:3887`) has the same order.
- The row is kept on purpose: a retry with the same link finds it by hash and reuses its keys and `xContactId`, so a
  join that reached the peer can still complete (#5018, `9a87f344b`; for address and group links since #6017, `524de4067`).
- #5018 also hid such rows from the chat list (`conn_status != prepared` in `getContactConnectionChatPreviews_`,
  `Store/Messages.hs:1184`). Its author's TODO said a failed join should be "filtered out of chat list" rather than
  deleted.

## Fix
Remove the `conn_status != ?` filter. A failed join is listed as a pending connection and deleted like one:
`deleteChat CTContactConnection` (`Library/Commands.hs:1392`) deletes the agent connection first, which removes the
queue. Both apps already render `prepared` as not initiated. Rows already in users' databases appear with no migration.

## Approaches considered
- Delete the connection when the join fails: a one-time link whose queue the peer already secured breaks on retry
  with new keys, and an address retry after a delivered request sends a duplicate one (#5018, #6017).
- Delete only on permanent errors: `HOST` is grouped with temporary errors (`temporaryOrHostError`).
- Keep the rows hidden and expire them in the cleanup manager: deletes a connection that may still complete later.
- Chosen: list them; the user decides when to give up. Cost: each failed long-link join leaves a pending entry until
  it is deleted or completes, and this reverses #5018's choice to hide them.

## Scope
Core only; both apps list what this query returns. In the apps, a short link with profile data leaves no such row:
an unreachable short-link server fails before anything is written, and a resolved one creates a prepared contact or
group. A short link whose plan has no profile data is joined like a long link (`ConnectPlan.kt:104`, `:170`,
`:265`), and so is the terminal `/connect` of a short link after it is resolved (`Library/Commands.hs:4701`); both
leave the same row and are fixed too. A SimpleX name address or a channel with relays goes through a prepared contact or group instead.
