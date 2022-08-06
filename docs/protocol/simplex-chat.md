# SimpleX Chat Protocol

## Abstract

SimpleX Chat Protocol is a protocol used to by SimpleX Chat clients to exchange messages. This protocol relies on a lower level SimpleX protocol - SimpleX Messaging Agent protocol. SimpleX Chat Protocol describes the format of messages and the client operations that should be performed when receiving such messages.

## Scope

The scope of SimpleX Chat Protocol is application level messages, both for chat functionality, related to the conversations between the clients, and extensible for any other application functions. Supported chat functions:

- direct and group messages
- message replies (quoting), forwarded messages and message deletions
- message attachments: images and files
- creating and managing chat groups
- invitation and signalling for audio/video WebRTC calls

## General message format

SimpleX Chat protocol supports two message formats:

- JSON-based format for chat and application messages.
- binary format for sending files or any other binary data.

### JSON format for chat and application messages

This document uses JTD schemas (RFC 8927) to define the properties of chat messages, with additional restrictions on message properties included in metadata member of JTD schemas. Message examples are also provided for simplicity, in case of any contradiction between examples and JTD schema the latter should be considered correct.

While JSON examples include whitespace, the SimpleX Chat Protocol clients should avoid using whitespace when encoding messages.

General message format is defined by this JTD schema:

```JSON
{
  "properties": {
    "event": {
      "type": "string"
    },
    "msgId": {
      "type": "string",
      "metadata": {
        "format": "base64url-encoded 12 bytes random message ID"
      }
    },
    "params": {
      "optionalProperties": {}
    }
  }
}
```

For example, this message defines a simple text message `"hello!"`:

```JSON
{
  "event": "x.msg.new",
  "msgId": "abcd",
  "params": {
    "content": {
      "type": "text",
      "text": "hello!"
    }
  }
}
```

`msgId` property is a 12 bytes base64url-encoded random message ID that the clients can use to reference the message in the future, e.g. when editing, quoting or deleting it.

`event` property is the type of the message that defines the semantics of the message and the allowed format of `params` property.

`params` property includes message data, as defined by `event`.

### Binary format for sending files

SimpleX Chat clients use separate connections to send files using a binary format. File chunk size should not be bigger than 15780 bytes to fit into 16kb (16384 bytes) transport block.

The syntax of each message used to send files is defined by the following ABNF notation:

```abnf
fileMessage = fileChunk / cancelFile
fileChunk = %s"F" chunkNo chunk
cancelFile = %s"C"
chunkNo = 4*4 OCTET ; 32bit-word sequential chunk number, starting from 1, in network encoding
chunk = 1*15780 OCTET ; file data, up to 15780 bytes
```

The first chunk number MUST be 1.

## Supported message types and SimpleX Chat sub-protocols

Message type is sent as a string in `"event"` property of the JSON message. General syntaxt of events is dot-separated words in Latin letters, with the first word (usually, a single letter) defining a protocol namespace, the second word defining sub-protocol, and other optional words defining a specific message. All SimpleX Chat Protocol messages related to chat functions are defined in `"x"` namespace.

Sub-protocol is a group of messages for related message functions - e.g. sending files, managing groups or negotiating WebRTC calls.

SimpleX Chat Protocol supports the following message types passed in `"event"` property of each message:

- `"x.contact"` - contact profile and additional data sent as part of contact request to a long-term contact address.
- `"x.info*"` - messages to send, update and de-duplicate contact profiles.
- `"x.msg.*"` - messages to send, update and delete content messages.
- `"x.file.*"` - messages to offer and accept files (see files sub-protocol)
- `"x.grp.*"` - messages used to manage groups (see group sub-protocol):
- `"x.call.*"` - messages to invite to WebRTC calls and send signalling messages
- `"x.ok"` - message sent during connection handshake

## Sub-protocol for contact profile

### Contact profile

This message has type `"x.info"`, it is sent by both sides of the connection during the connection handshake, and can be sent later as well when contact profile is updated.

- `"x.info"` - contact profile, exchanged during connection handshake and sent when profile is updated.
- `"x.info.probe"` - contact first "probe" to deduplicate contacts, e.g. when the contact to whom you already have a connection is invited to the group (see group sub-protocol).
- `"x.info.probe.check"` - second -> Right XInfoProbeCheck\*
- `"x.info.probe.ok"` -> Right XInfoProbeOk\*

## Sub-protocol for content messages

- `"x.msg.new"` - a new message with content
- `"x.msg.update"` - update of the previously sent message
- `"x.msg.del"` - request to delete previously sent message

## Sub-protocol for sending and receiving files

- `"x.file.acpt"` - accept file offered as message attachment, in case when file connection address was included in the message (used in direct conversations).
- `"x.file.acpt.inv"` - accept file and send connecting address (used in group conversations).
- `"x.file.cancel"` - notify recipient that sending file was cancelled, in response to accepting file.

## Sub-protocol for chat groups

- `"x.grp.inv"` - send invitation to join the group.
- `"x.grp.acpt"` - accept invitation to join the group.
- `"x.grp.mem.new"` - announcing new group member to all other members.
- `"x.grp.mem.intro"` - in
- `"x.grp.mem.inv"` -> Right XGrpMemInv\*
- `"x.grp.mem.fwd"` -> Right XGrpMemFwd\*
- `"x.grp.mem.info"` -> Right XGrpMemInfo\*
- `"x.grp.mem.con"` -> Right XGrpMemCon\*
- `"x.grp.mem.con.all"` -> Right XGrpMemConAll\*
- `"x.grp.mem.del"` -> Right XGrpMemDel\*
- `"x.grp.leave"` -> Right XGrpLeave\*
- `"x.grp.del"` -> Right XGrpDel\*
- `"x.grp.info"` -> Right XGrpInfo\*

## Sub-protocol for WebRTC calls

- `"x.call.inv"` -> Right XCallInv\*
- `"x.call.offer"` -> Right XCallOffer\*
- `"x.call.answer"` -> Right XCallAnswer\*
- `"x.call.extra"` -> Right XCallExtra\*
- `"x.call.end"` -> Right XCallEnd\*

## JTD Schema for SimpleX Chat messages

The schema below only defines chat messages in `"x"` namespace:

```JSON

```
