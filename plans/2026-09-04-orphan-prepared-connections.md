# Connections left by failed link joins

## Problem
A failed connect via an old-format (long) contact address, one-time or group link, for example when its server is
unreachable, is reported by an error, and the connection written for it is not shown in the chat list, so the user
cannot delete it. For address and group links, its receive queue on the user's own server is subscribed again on every
app start.

## Cause
- In `connect'`, the row is written by `createConnReqConnection` (`Library/Commands.hs:3975`) before `joinContact`
  (`:3976`), the only call in which an error can occur. The row is `prepared`, `conn_type = contact`, with
  `contact_id` NULL. For a one-time link via `APIConnect`, `joinNewConn` (`:3887`) has the same order.
- On a retry with the same link, the row is found by hash, and its keys and `xContactId` are reused (#5018,
  `9a87f344b`; for address and group links since #6017, `524de4067`).
- `prepared` rows are excluded from the chat list by `conn_status != prepared` in `getContactConnectionChatPreviews_`
  (`Store/Messages.hs:1184`), added in #5018.

## Fix
The `conn_status != ?` filter is removed. A connection left by a failed join is listed as a pending connection and is
deleted like one: in `deleteChat CTContactConnection` (`Library/Commands.hs:1392`), the agent connection is deleted
first, and its receive queue with it. `prepared` is rendered as not initiated in both apps. Rows already stored in
users' databases are listed after the update, without a migration.

## Scope
Core only; both apps list the rows returned by this query. In the apps, a short link with profile data is resolved
before any row is written: if the short-link server is unreachable, an error is returned first, and a resolved link is
joined through a prepared contact or group. A short link whose plan lacks profile data is joined as a long link
(`ConnectPlan.kt:104`, `:170`, `:265`), and so is a short link resolved by the terminal `/connect`
(`Library/Commands.hs:4701`); in both cases the same row is written, and it is listed after this change. A SimpleX name
address and a channel with relays are joined through a prepared contact or group.
