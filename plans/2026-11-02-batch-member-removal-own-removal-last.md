# Own removal processed after the rest of a received batch

## Problem

When an admin removes several members at once, every removed member except one sees "chat db error … FOREIGN KEY constraint failed", and in their copy of the group the removed members whose removals follow their own in the batch stay connected.

## Cause

The admin's client sends one batch with an `XGrpMemDel` for each removed member to all current members, including the removed ones (`APIRemoveMembers`).
A removed member's client processes the batch in order.
On its own `XGrpMemDel`, the client deletes all its group connections (`deleteGroupConnections`), including the connection on which the batch was received.
For each later event of the batch, the client inserts a `msg_deliveries` row that references the deleted connection; the insert fails, and the event is not processed.

## Fix

The client moves the user's own `XGrpMemDel` events to the end of the batch before processing it (`MSG` handler of `processGroupMessage`, `Subscriber.hs`).
The order of the other events is unchanged.

## Scope

- Changed: the processing order of a group message batch received by a member, in all groups.
- Unchanged: the sender (`APIRemoveMembers`).
- Not covered:
  - an own removal wrapped in a legacy `XGrpMsgForward` event;
  - removals split across several messages (`batchMessages` limits), where removals in messages after the own one are not processed.
- Test: `testGroupRemoveMembers` in `tests/ChatTests/Groups.hs`.
