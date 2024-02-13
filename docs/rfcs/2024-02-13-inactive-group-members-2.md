# Inactive group members (simplified)

[Original doc](./2023-11-21-inactive-group-members.md)

## Problem

Groups traffic is higher than necessary due to sending messages to inactive group members.

## Solution

### Improve connection deletion

- When leaving or deleting group, batch db operations to optimize performance.
- In agent - fix race where connection can be deleted while it has remaining pending messages.
  - Current agent logic is to immediately delete connection if it has no rcv queues left.
  - Simplest should be to make a smart version of `deleteConn` for this improvement, checking `snd_messages` table for remaining messages, and keep connection around in case there are.
  - While this may improve delivery of group leave and delete messages, it may as well have undesirable side effects for other use cases, as any pending messages will be sent prior to deleting connection. For example, user sends several messages on bad network, decides to delete contact, messages are still delivered when user is on good network before deletion, even though this contradicts user's intent and messages hadn't left user's device at the time of deletion. Considering this race when it happens is identical to simply leaving groups by deleting app, or deleting user profile only locally, it may be a bad idea to affect regular contact deletion for this use case.

### Track member inactivity

- Mark members as inactive on QUOTA errors, reset as active on QCONT
  - track `group_members.inactive` flag per group member
  - on SMP.QUOTA error agent to notify client with ERR CONN QUOTA (new ConnectionErrorType QUOTA)
  - on receiving QCONT agent to notify client (new event)
  - apart from QCONT, reset on any message or receipt
- Don't send to member if inactive
  - don't send only content messages (x.msg.new, etc.) and always send messages altering group state?
  - or don't send any messages?
- Track number of skipped messages per member and first skipped message
  - count `group_members.skipped_msg_cnt`
  - only count messages of same types/criteria that are included into history
  - track `group_members.skipped_first_shared_msg_id` (only content or including service messages?)
- Send XGrpMsgSkipped before next message
  - check `skipped_msg_cnt` > 0 and `skipped_first_shared_msg_id` is not null to only send once, reset after sending

```haskell
XGrpMsgSkipped :: SharedMsgId -> Int64 -> ChatMsgEvent 'Json -- from, count
```
