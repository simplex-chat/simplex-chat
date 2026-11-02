# Removed member processes its own removal last

## Problem

When an admin removes several members at once, every removed member except one sees "chat db error … FOREIGN KEY constraint failed", and in their copy of the group the other removed members stay connected.

## Cause

`APIRemoveMembers` sends one batch with an `XGrpMemDel` for each removed member to all current members, including the removed ones.
A removed member processes the batch in order.
Its own `XGrpMemDel` deletes all its group connections (`deleteGroupConnections`), including the one the batch arrived on.
Each later event of the batch is saved with a `msg_deliveries` row that references the deleted connection, and the insert fails.

## Fix

The group `MSG` handler in `Subscriber.hs` moves the user's own `XGrpMemDel` events to the end of the batch before processing it.
The other events are processed in their original order.

## Scope

- Changed: group message batches received by a member (`processGroupMessage`, `MSG`).
- Unchanged: the sender (`APIRemoveMembers`), relay groups (each removed member receives only its own removal from the relay), direct messages.
- Test: `testGroupRemoveMembers` in `tests/ChatTests/Groups.hs`.
