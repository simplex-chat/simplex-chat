# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [SMP Agent](#smp-agent)
- [SMP agent protocol components](#smp-agent-protocol-components)
- [Duplex connection](#duplex-connection)
- [Communication between SMP agents](#communication-between-smp-agents)
  - [Message syntax](#messages-between-smp-agents)
- [SMP agent commands](#smp-agent-commands)
- [Connection invitation](#connection-invitation)

## Abstract

The purpose of SMP agent protocol is to define the syntax and the semantics of communications over [simplex messaging protocol)](./simplex-messaging.md) protocol to provide bi-directional (duplex) connection to the users of SMP agents consisting of two separate unidirectional (simplex) SMP queues in a way that abstracts a way multiple steps required to establish such bi-directional connection.

## SMP Agent

SMP agent is a client-side process or library that communicates via SMP servers using [simplex messaging protocol (SMP)](./simplex-messaging.md) with other SMP agents according to the commands received from its users. While this protocol is a middle layer in SMP protocols stack (above SMP protocol but below any application level protocol) and intended to be used by client-side applications that need secure asynchronous bi-directional communication channels ("connections"), SMP agents can be used directly via telnet to have chat-like functionality in the terminal).

The agent must have persistent storage to manage the states of known "connections" and of the client-side information of two SMP queues that each connection consists of, and also the buffer of the most recent messages. The number of the messages that should be stored is implementation specific, depending on the error management approach that the agent implements, at the very least the agent must store two most recent messages - the last valid message (see message errors) and the last received message (whether it was valid or not).

## SMP agent protocol components

SMP agent protocol has 3 main parts:

- the syntax and semantics of messages that SMP agents exchange between each other to negotiate establishing two unidirectional (simplex) encrypted queues on SMP server(s) to provide their users a higher level interface than SMP protocol to establish and operate duplex (bi-directional) connections and notifications about any communication integrity violations (via hash validation, sequential message IDs and message acknowledgements send over SMP queues). Providing connection redundancy and connection rotation is out of scope - it can either be added to this protocol later or implemented as a higher level middle layer protocol.
- the syntax and semantics of the commands that should be sent over TCP or other sequential protocol by the client-side agent user to the agent that communicates with one or multiple SMP servers. This protocol allows to manage multiple connections, each consisting of two simplex SMP queues that can be used to build higher-level communication and application primitives.
- the syntax and semantics of the message that users of SMP agents should send out-of-band (as pre-shared key / "invitation") to ensure [E2E encryption][1] integrity for the first SMP queue and protection against active attacks ([MITM attacks][2]).

## Duplex connection

**Creating duplex connection between Alice and Bob:**

![Creating duplex connection](/diagrams/duplex-messaging/duplex-creating.svg)

## Communication between SMP agents

SMP agents communicate via SMP servers managing creation, deletion and operations of SMP queues using SMP protocol. Each SMP message body once decrypted contains 3 lines, as defined by `decryptedSmpMessageBody` syntax:

- `agentMsgHeader` - agent message header that contains sequential agent message ID for a particular SMP queue, agent timestamp and the hash of the previous message.
- `agentMessage` - a command/message to the other SMP agent:
  - to establish the connection with two SMP queues (`helloMsg`, `replyQueueMsg`)
  - to send an acknowledge user messages (`clientMsg`, `acknowledgeMsg`)
  - to notify another agent about queue deletion (`deleteQueueMsg`)
- `msgPadding` - an optional message padding to make all SMP messages having consistent size as additional privacy protection.

All these messages should be sent via all active SMP sender queues belonging to the duplex connection.

### Messages between SMP agents

Message syntax below uses [ABNF][1].

```abnf
decryptedSmpMessageBody = agentMsgHeader CRLF agentMessage CRLF msgPadding
agentMsgHeader = agentMsgId SP agentTimestamp [SP previousMsgHash]
agentMsgId = 1*DIGIT ; sequential agent message ID set by the sending agent

agentMessage = helloMsg / replyQueueMsg / deleteQueueMsg
               / clientMsg / acknowledgeMsg

msgPadding = *OCTET ; optional random bytes to get messages to the same size (as defined in SMP message size)

helloMsg = %s"HELLO" SP signatureVerificationKey [SP %s"NO_ACK"]
; NO_ACK means that acknowledgements will NOT be sent in this connection by sending party
; to messages starting from the second `helloMsg` in reply queue
signatureVerificationKey = encoded

replyQueueMsg = %s"REPLY" SP qInfo ; `qInfo` is the same as in out-of-band message
; this message can only be sent by the second connection party

deleteQueueMsg = %s"DEL" ; notification that recipient queue will be deleted
; no need to notify the other party about suspending queue separately, as it looks the same for the sender

clientMsg = %s"MSG" SP size SP clientMsgBody CRLF ; CRLF is in addition to CRLF in decryptedSmpMessageBody
size = 1*DIGIT
clientMsgBody = *OCTET

acknowledgeMsg = %s"ACK" SP agentMsgId SP ackStatus
; the acknowledgement message itself should NOT be acknowledged by the receiving agent

ackStatus = %s"OK" / ackError

ackError = %s"ERR" SP ackErrorType

ackErrorType = ackUnknownMsg / ackProhibitedMsg / ackSyntaxErr

ackUnknownMsg = %s"UNKNOWN"

ackProhibitedMsg = %"PROHIBITED" ; e.g. "HELLO" or "REPLY"

ackSyntaxErr = %"SYNTAX" SP syntaxErrCode
syntaxErrCode = 1*DIGIT ; TODO

```

## SMP agent commands

This part describes the transmissions between users and client-side SMP agents: commands that the users send to create and operate duplex connections and SMP agent responses and messages they deliver.

Commands syntax below is provided using [ABNF][1].

Each transmission between the user and SMP agent must have this format/syntax (after unwrapping it out of SMP message):

```abnf
duplexCommand = (userCmd / agentMsg) CRLF
userCmd = create / join / accept
          / subscribe / getStatus
          / send / acknowledge
          / suspend / delete

agentMsg = connection / connectionInvitation / confirmation
           / message / received / unsubscribed
           / queueStatus / ok / error

create = %s"NEW" SP smpServer [SP %s"A=" cName] [SP %s"ACK=" onOff]
; response is `connectionInvitation` or `error`
smpServer = srvHost [":" port] ["#" keyFingerprint]
srvHost = hostname ; TODO add RFC here
port = 1*DIGIT
keyFingerprint = encoded
connectionInvitation = %s"INV" SP cAlias SP qInfo

cId = encoded
cName = 1*(ALPHA / DIGIT / "_" / "-")
cAlias = cId / cName

connection = %s"CON" SP cAlias ; `cAlias` will be `cId` when in response to `create`

getStatus = %s"QST" SP cAlias SP direction ; response is `queueStatus` or `error`
direction = %s"SND" / %s"RCV"
; `queueStatus` should be sent by agent whenever queue status changes as well as response to `getStatus` command
queueStatus = %s"STAT" SP cAlias SP direction SP (rqState / sqState) [SP subscriptionState]
subscriptionState = %s"SUB" onOff
onOff = %s"ON" / %s"OFF"

sqState = %s"NONE" / %s"NEW / %s"CONFIRMED" / %s"ACTIVE"
rqState = sqState / %s"PENDING / %s"SECURED" / %s"DISABLED"
; see comments in ./core/definitions/src/Simplex/Messaging/Core.hs

; TODO connection should store some information about sent and received messages
; e.g. the hash of the last message, to include/check in the next message
; sequential ID of the previous message, to include/check messages
; list of communication errors, e.g. missed messages can be re-sent and reconciled
; party(ies) that sent confirmation - to accept to the connection

subscribe = %s"SUB" SP cAlias [SP onOff]; response `ok` or `error`

unsubscribed = %s"END" SP cAlias
; when another agent subscribes to the same SMP queue on the server

join = %s"JOIN" SP qInfo
                [SP (smpServer / %s"NO_REPLY")] ; reply queue SMP server
                ; server from qInfo is used by default
                [SP %s"A=" cName] [SP %s"ACK=" onOff]         
; response is `connection` or `error`

confirmation = %s"CONF" SP cAlias SP partyId SP partyInfo

accept = %s"LET" SP cAlias SP partyId ; response is `ok` or `error`

suspend = %s"OFF" SP cAlias ; can be sent by either party, response `ok` or `error`

delete = %s"DEL" SP cAlias ; can be sent by either party, response `ok` or `error`

send = %s"SEND" SP cAlias SP msgBody
; send syntax is similar to that of SMP protocol, but it is wrapped in SMP message
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary

message = %s"MSG" SP cAlias SP agentMsgId SP agentTimestamp SP srvTimestamp SP msgStatus SP binaryMsg
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
skippedMsgErr = %"NO_ID" SP missingFromMsgId SP missingToMsgId
badMsgIdErr = %"ID" SP previousMsgId ; ID is lower than the previous
badHashErr = %"HASH"

; these should not be sent with the messages
noAckErr = %"NO_ACK" SP fromMsgId SP toMsgId ; message IDs of unacknowledged messages
badAckErr = %"ACK" SP agentMsgId ; acknowledgement received to the message that was not sent

acknowledge = %s"ACK" SP cAlias SP agentMsgId

received = %s"RCVD" SP cAlias SP agentMsgId

ok = %s"OK"

error = %s"ERR" SP errorType

encoded = base64
```

## Connection invitation

Connection invitation `qInfo` is generated by SMP agent in response to `create` command (`"NEW"`), used by another party user with `connect` command (`"JOIN"`), and then another invitation is sent by the agent in `replyQueueMsg` and used by the first party agent to connect to the reply queue (the second part of the process is invisible to the users).

Connection invitation is a text with the following syntax:

```
qInfo = %s"smp::" smpServer "::" queueId "::" ephemeralPublicKey
; `smpServer` here MUST have `keyFingerprint` part
queueId = encoded
ephemeralPublicKey = encoded ; key for sender to encrypt messages
```

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[3]: https://tools.ietf.org/html/rfc5234
