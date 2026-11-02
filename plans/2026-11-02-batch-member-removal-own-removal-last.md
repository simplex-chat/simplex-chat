# Removed member processes its own removal last

## Problem

When an admin removes several members at once, every removed member except one sees "chat db error … FOREIGN KEY constraint failed", and in their copy of the group the other removed members stay connected.

## Cause

The admin's client sends one batch with an `XGrpMemDel` for each removed member to all current members, including the removed ones (`APIRemoveMembers`).
A removed member's client processes the batch in order.
On its own `XGrpMemDel`, the client deletes all its group connections (`deleteGroupConnections`), including the one the batch arrived on.
Each later event of the batch is saved with a `msg_deliveries` row that references the deleted connection, and the insert fails.

## Fix

The client moves the user's own `XGrpMemDel` events to the end of the batch before processing it (`MSG` handler of `processGroupMessage`, `Subscriber.hs`).
The other events keep their order.

## Scope

- Changed: group message batches received by a member.
- Unchanged: the sender (`APIRemoveMembers`); relay groups, where a removed member receives its removal as a single message.
- Not covered: an own removal wrapped in a legacy `XGrpMsgForward` event.
- Test: `testGroupRemoveMembers` in `tests/ChatTests/Groups.hs`.
