# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [Duplex connection](#duplex-connection)
- [Messages between SMP agents](#messages-between-smp-agents)
- [SMP agent commands](#smp-agent-commands)

## Abstract

SMP agent is a client-side process or library that communicates via SMP servers using [simplex messaging protocol (SMP)](./simplex-messaging.md).

SMP agent protocol has 3 main parts:

- the syntax and semantics of messages that SMP agents exchange between each other to negotiate establishing multiple unidirectional (simplex) encrypted queues on SMP server(s) to provide their users convenient interface to establish and operate duplex (bi-directional) connections, providing redundancy, queue and key rotation, notifications about any communication integrity violations.
- the syntax and semantics of the commands that should be sent over TCP or other sequential streaming protocol to the client-side agent that communicates with one or multiple SMP servers. This protocol allows to manage multiple simplex connections organised into groups defining duplex communication channels that can be used to build higher-level communication and application primitives.
- the syntax and contents of messages that users of SMP agents should send out-of-band ("invitation") to ensure [E2E encryption][1] integrity for the first SMP queue and protection against active attacks ([MITM attacks][2]).

## Duplex connection

**Creating duplex connection between Alice and Bob:**

![Creating duplex connection](/diagrams/duplex-messaging/duplex-creating.svg)

## Messages between SMP agents

Message syntax below is provided using [ABNF][1].

```abnf
decryptedSmpMessageBody = agentMsgHeader CRLF agentMessage CRLF contentMessageBody CRLF
agentMsgHeader = agentMsgSeqId SP agentTimestamp [SP previousMsgHash]
agentMsgSeqId = 1*DIGIT
contentMessageBody = *OCTET

agentSmpMessage = addQueueMsg / delQueueMsg / queueErrorMsg
                  / clientErrorMsg / contentMsg / acknowledgeMsg

addQueueMsg = %s"ADD" SP qInfo ; notification that recipient queue is added
; `qInfo` is the same as out-of-band message

delQueueMsg = %s"DEL" SP qNum ; notification that recipient queue will be deleted - sender shouldn't sent anything to it further

queueErrorMsg = %s "ERR" SP qNum SP queueErrorInfo
messageErrorInfo = skippedMsgErr / differentMsgErr / badMsgId / badHashErr / noMessagesErr
skippedMsgErr = %"SKIP" SP fromMsgSeqId SP toMsgSeqId
differentMsgErr = %"DIFF" SP msgSeqId
badMsgId = %"ID" SP msgSeqId ; ID is lower than the previous
badHashErr = %"HASH" SP msgSeqId
noMessagesErr = %s"EMPTY"

clientErrorMsg = %s"CERR" SP messageErrorInfo

contentMsg = %s"MSG" SP clientMsgSeqId

acknowledgeMsg = %s"ACK" SP clientMsgSeqId
```

### Communication between SMP agents

SMP agents communicate via SMP servers managing creating, deleting and operating SMP queues using SMP protocol. Each SMP message body once decrypted contains 3 lines, as defined by `decryptedSmpMessageBody` syntax:

- `agentMsgHeader` - agent message header that contains sequential client message ID (`clientMsgSeqId`), sequential agent message ID for a particular SMP queue, agent timestamp and the hash of the previous message.
- `agentSmpMessage` - command to the other SMP message:
  - to add/delete SMP queues to the connection (`addQueueMsg`, `delQueueMsg`)
  - to notify about any communication problems (`queueErrorMsg`, `messageErrorMsg`)
  - to send client messages (`contentMessage`)
- `contentMessageBody`

All these messages should be sent via all active SMP sender queues belonging to the duplex connection.

### First message

To secure connection

### Message to

## SMP agent commands

This part describes the transmissions between users and client-side SMP agents: commands that the users send to create and operate duplex connections and SMP agent responses and messages they deliver.

Commands syntax below is provided using [ABNF][1].

Each transmission between the user and SMP agent must have this format/syntax (after unwrapping it out of SMP message):

```abnf
transmission = (userCmd / agentMsg) CRLF
userCmd = create / alias
          / subscribe / unsubscribe
          / invite / connect / extend / accept
          / send / acknowledge
          / suspend / delete
          / status / info

agentMsg = connection / queueInfo / invitation / message / unsubscribed / ok / error

create = %s"NEW"; response is `connection` or `error`
connection = %s"ID" SP cAlias SP recipientQueuesInfo SP senderQueuesInfo SP cId

alias = %s"NAME" SP cAlias SP cName ; response is `ok` or `error`

status = %"STAT" SP cAlias ; response is `connection` or `error`
cAlias = cId / cName ; TODO add cName syntax - letters, numbers, "-" and "_"
cId = encoded

recipientQueuesInfo = rqInfo ["," recipientQueuesInfo]
senderQueuesInfo = sqInfo ["," senderQueuesInfo]

rqInfo = qNum "-" rqState ["-" %s"SUB"]
sqInfo = qNum "-" sqState
qId = encoded

sqState = %s"NONE" / %s"NEW / %s"CONFIRMED" / %s"ACTIVE"
rqState = sqState / %s"PENDING / %s"SECURED" / %s"DISABLED"
; see comments in ./core/definitions/src/Simplex/Messaging/Core.hs

info = %s"QINFO" SP cAlias SP qNum
queueInfo = %s"QUEUE" SP qInfo SP serverHost SP qId
qInfo = rqInfo / sqInfo

subscribe = %s"SUB" SP cAlias ; response `ok` or `error`
unsubscribe = %s"UNSUB" SP cAlias ; response `ok` or `error`

invite = %s"INVITE" SP cAlias ; response is `invitation` or `error`
invitation = %s"JOIN" SP qInfo

connect = %s"CONNECT" SP qInfo ; response is `connection` or `error`

extend = %s"EXTEND" SP cAlias SP qInfo

accept = %s"ACCEPT" SP cAlias ; response is `connection` or `error`

suspend = %s"OFF" SP cAlias ; can be executed by either side (unlike SMP)

delete = %s"DEL" SP cAlias ; can be executed by either side (unlike SMP)

send = %s"SEND" SP cAlias SP msgBody
; send syntax is similar to that of SMP protocol, but it is wrapped in SMP message
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary

message = %s"MSG" SP cAlias SP agentMsgSeqId SP timestamp SP agentTimestamp SP msgHashStatus SP binaryMsg
agentMsgSeqId = 1*DIGIT
agentTimestamp = date-time; RFC3339
msgHashStatus = %s"HASH" SP ("OK" / "ERR")

encoded = base64
```

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[3]: https://tools.ietf.org/html/rfc5234
