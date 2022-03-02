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

We need to introduce a sequential number for chat items, each sequence separate for a sending user.

We will add an optional property `chatItem` into messages that can create and `chatItemRef` into messages that refer to chat items (currently it is only x.msg.new and x.file messages that create items and we will add x.msg.reply, x.msg.delete and x.msg.update messages to support mentioned features).

JTD schema for `chatItem` property:

```yaml
properties:
  seqNo: type: int32
```

JTD schema for `chatItemRef` property:

```yaml
properties:
  seqNo: type: int32
  sent: type: boolean # true if it is in reference to the item that the sender of the message originally sent, false for references to received items
optionalProperties:
  memberId: type: string # base64 member ID of the sender known to all group members for group chats
```

JSON for reply messages:

```jsonc
"event": "x.msg.reply" // XMsgNew
"params":            // MsgContent
{
  "content": {
    "msgType": "text",
    // field "files" can be represented in content as contentType "file" with length prepended or as complex contentData
    "text": "<msg text>",
    "chatItem": {} // optional, for backwards compatibility
  },
  "replyTo": {
    "msgType": "text",
    "text": "<msg text>", // can be partial text to fit the block size
    "partial": true, // optional, if partial
    "chatItemRef": {} // optional, for backwards compatibility
  }
}
```

This format ensures that
- replies work on the clients that do not support showing replies (replyTo will be ignored)
- users can reply to messages sent from the clients not supporting replies

The only feature that would not work in case chatItem/chatItemRef is missing is navigating to the message to which message is in reply to.

Also - we could add reply feature without adding chatItem/chatItemRef - it seems to be orthogonal, and then add chatItem/chatItemRef later to enable navigation.
