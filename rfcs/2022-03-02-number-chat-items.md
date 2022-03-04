# Message replies and chat item sequential numbers

## Problem

Many chat features require referring to the previous chat items in the same conversation:

- item editing
- item deletion
- item reply
- delivery/read receipts
- any interactive features mutating chat item state
- group message integrity via DAG

The most in-demand feature is replies.

## Proposed solution

As group message integrity is needed not for chat items, but for messages, the updated proposal is to introduce a random, non-sequential message id, unique per conversation and per sender.

All above features would rely on this ID, e.g. reply would use the ID of the message that created the item.

We will add an optional property `msgId` into all chat messages (not only visible to the users) and `msgRef` into messages that need to reference other messages.

JTD schema for `msgId` property:

```yaml
type: string # base64 encoded 8 byte binary
```

JTD schema for `msgRef` property:

```yaml
properties:
  msgId: type: string
  sent: type: boolean # true if it is in reference to the item that the sender of the message originally sent, false for references to received items
optionalProperties:
  memberId: type: string # base64 member ID of the sender known to all group members for group chats
```

JTD for reply messages:

```yaml
properties:
  msgId: string
  event: enum: ["x.msg.new"]
  params:
    properties:
      content:
        properties:
          msgType: type: string
          text:  type: string
      replyTo:
        properties:
          msgRef: ref: msgRef
```

This format ensures that replies show as normal messages on the clients that do not support showing replies (replyTo will be ignored).

The only feature that would not work in case chatItem/chatItemRef is missing is navigating to the message to which message is in reply to.
