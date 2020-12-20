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

Message syntax below uses [ABNF][1].

```abnf
decryptedSmpMessageBody = agentMsgHeader CRLF agentMessage CRLF msgPadding
agentMsgHeader = agentMsgId SP agentTimestamp [SP previousMsgHash]
agentMsgId = 1*DIGIT ; sequential agent message ID set by sender

agentMessage = helloMsg / replyQueueMsg
               / suspendQueueMsg / deleteQueueMsg
               / clientMsg / acknowledgeMsg
               / messageErrorMsg 

msgPadding = *OCTET ; random bytes to get messages to the same size

helloMsg = %s"HELLO" SP signatureVerificationKey [SP %s"NO_ACK"]
; NO_ACK means that acknowledgements will NOT be sent in this connection by sending party
; to messages after `helloMsg` in reply queue
signatureVerificationKey = encoded

replyQueueMsg = %s"REPLY" SP qInfo ; `qInfo` is the same as in out-of-band message
; this message can only be sent by the second connection party

suspendQueueMsg = %s"PAUSE" ; notification that recipient queue will be suspended

deleteQueueMsg = %s"DEL" ; notification that recipient queue will be deleted

clientMsg = %s"MSG" SP size SP clientMsgBody CRLF ; CRLF is in addition to CRLF in decryptedSmpMessageBody
clientMsgBody = *OCTET

acknowledgeMsg = %s"ACK" SP agentMsgId ; this message should not be acknowledged by the receiving agent
```

### Communication between SMP agents

SMP agents communicate via SMP servers managing creating, deleting and operating SMP queues using SMP protocol. Each SMP message body once decrypted contains 3 lines, as defined by `decryptedSmpMessageBody` syntax:

- `agentMsgHeader` - agent message header that contains sequential agent message ID for a particular SMP queue, agent timestamp and the hash of the previous message.
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
duplexCommand = (userCmd / agentMsg) CRLF
userCmd = create / alias / list 
          / status / info
          / invite / accept
          / connect / reply
          / send / acknowledge
          / subscribe / unsubscribe
          / suspend / delete

agentMsg = connection / connectionStatus / queueInfo
           / invitation / confirmation
           / message / unsubscribed
           / ok / error

create = %s"CONN" SP srvHost [":" port] ["#" keyFingerprint] [SP %s"NO_ACK"] ; response is `connection` or `error`
port = 1*DIGIT
keyFingerprint = encoded
connection = %s"CONN" SP cAlias ; cAlias will be cId when in response to `create`

alias = %s"ALIAS" SP cId SP cName ; response is `ok` or `error`
cAlias = cId / cName ; TODO add cName syntax - letters, numbers, "-" and "_"
cId = encoded

list = %"LIST" [SP beforeConnection]; response - multiple `connection`s
beforeConnection = cAlias

status = %s"STAT" SP direction SP cAlias ; response is `queueStatus` or `error`
direction = %s"SND" / %s"RCV"
; queueStatus should be sent by agent whenever status changes as well as response to `status` command
queueStatus = %s"STAT" SP direction SP cAlias SP cId SP (rqState / sqState) [SP subscriptionState]
subscriptionState = %s"SUB=" onOff
onOff = %s"ON" / %s"OFF"

sqState = %s"NONE" / %s"NEW / %s"CONFIRMED" / %s"ACTIVE"
rqState = sqState / %s"PENDING / %s"SECURED" / %s"DISABLED"
; see comments in ./core/definitions/src/Simplex/Messaging/Core.hs

; TODO connection should store some information about sent and received messages
; e.g. the hash of the last message, to include/check in the next message
; sequential ID of the previous message, to include/check messages
; list of communication errors, e.g. missed messages can be re-sent and reconciled
; party(ies) that sent confirmation - to accept to the connection

subscribe = %s"SUB" SP cAlias ; response `ok` or `error`

unsubscribe = %s"UNSUB" SP cAlias ; response `ok` or `error`

invite = %s"INVITE" SP cAlias ; response is `invitation` or `error`
invitation = %s"JOIN" SP qInfo

connect = %s"CONNECT" SP qInfo ; response is `connection` or `error`

reply = %s"REPLY" SP srvHost [SP keyFingerprint] ; response is `ok` or `error`

confirmation = %s"CONF" SP cAlias SP partyId SP partyInfo

accept = %s"ACCEPT" SP cAlias SP partyId ; response is `ok` or `error`

suspend = %s"SUSPEND" SP cAlias ; can be sent by either party, response `ok` or `error`

delete = %s"DELETE" SP cAlias ; can be sent by either party, response `ok` or `error`

send = %s"SEND" SP cAlias SP msgBody
; send syntax is similar to that of SMP protocol, but it is wrapped in SMP message
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary

message = %s"MSG" SP cAlias SP agentMsgId SP srvTimestamp SP agentTimestamp SP msgStatus SP binaryMsg
agentMsgId = 1*DIGIT
srvTimestamp = date-time ; RFC3339
agentTimestamp = date-time
msgStatus = ok / messageError

messageError = %s"ERR" SP messageErrorType
messageErrorType = unknownMsgErr / prohibitedMsgErr / syntaxErr
                   / skippedMsgErr / badMsgIdErr
                   / badHashErr / noAckErr / badAckErr
                   / noMessagesErr
; TODO maybe some of these errors should not be sent to the agent of another party, only to the user?
unknownMsgErr = %"UNKNOWN" SP agentMsgId
prohibitedMsgErr = %"PROHIBITED" SP agentMsgId ; e.g. "HELLO" or "REPLY"
syntaxErr = %"SYNTAX" SP syntaxErrCode SP agentMsgId
skippedMsgErr = %"NO_ID" receivedMsgId SP missingFromMsgId SP missingToMsgId
badMsgIdErr = %"ID" SP receivedMsgId SP previousMsgId ; ID is lower than the previous
badHashErr = %"HASH" SP agentMsgId
noAckErr = %"NO_ACK" SP fromMsgId SP toMsgId ; message IDs of unacknowledged messages
badAckErr = %"ACK" SP agentMsgId ; acknowledgement received to the message that was not sent
syntaxErrCode = 1*DIGIT ; TODO

acknowledge = %s"ACK" SP cAlias SP agentMsgId

encoded = base64
```

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[3]: https://tools.ietf.org/html/rfc5234
