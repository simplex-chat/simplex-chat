# Inactive group members (simplified)

[Original doc](./2023-11-21-inactive-group-members.md)

## Problem

Groups traffic is higher than necessary due to sending messages to inactive group members.

## Solution

### Improve connection deletion

- When leaving or deleting group, batch db operations to optimize performance.
- In agent - fix race where connection can be deleted while it has remaining pending messages.

### Track member inactivity

- Mark members as inactive on QUOTA errors
- Re-enable as active on QCONT
- Track number of skipped messages per member
- Send XGrpMsgSkipped before next message
