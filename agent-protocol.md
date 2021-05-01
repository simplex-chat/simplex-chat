# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [SMP agent](#smp-agent)
- [SMP agent protocol components](#smp-agent-protocol-components)
- [Duplex connection](#duplex-connection)
- [Communication between SMP agents](#communication-between-smp-agents)
  - [Message syntax](#messages-between-smp-agents)
    - [HELLO message](#hello-message)
    - [REPLY message](#reply-message)
    - [MSG message](#msg-message)
- [SMP agent commands](#smp-agent-commands)
  - [Client commands and server responses](#client-commands-and-server-responses)
    - [NEW command and INV response](#new-command-and-inv-response)
    - [JOIN command](#join-command)
    - [CON notification](#con-notification)
    - [SUB command](#sub-command)
    - [SEND command and SENT response](#send-command-and-sent-response)
    - [MSG notification](#msg-notification)
    - [END notification](#end-notification)
    - [OFF command](#off-command)
    - [DEL command](#del-command)
- [Connection invitation](#connection-invitation)

## Abstract

The purpose of SMP agent protocol is to define the syntax and the semantics of communications between the client and the agent that connects to [SMP](./simplex-messaging.md) servers.

It provides:
- convenient protocol to create and manage a bi-directional (duplex) connection to the users of SMP agents consisting of two separate unidirectional (simplex) SMP queues, abstracting away multiple steps required to establish bi-directional connections.
- management of E2E encryption between SMP agents, generating ephemeral RSA keys for each connection.
- SMP command authentication on SMP servers, generating ephemeral RSA keys for each SMP queue.
- TCP transport handshake and encryption with SMP servers.
- validation of message integrity.

SMP agent protocols provides no encryption or any security on the client side - it is assumed that the agent is executed in the trusted and secure environment.

The future versions of this protocol could provide:
- managing redundant SMP queues with more than 1 queue in each direction.
- managing simple symmetric groups as a foundation for chat groups and device synchronization.
- agent cluster - synchronizing states of multiple agents.
- secure "synchronous" streams with symmetric message encryption and connection-level authentication (requires extending [SMP protocol](./simplex-messaging.md)) - it can be used, e.g., for file transfers.

## SMP agent

SMP agent is a client-side process or library that communicates via SMP servers using [simplex messaging protocol (SMP)](./simplex-messaging.md) with other SMP agents according to the commands received from its users. This protocol is a middle layer in SMP protocols stack (above SMP protocol but below any application level protocol) - it is intended to be used by client-side applications that need secure asynchronous bi-directional communication channels ("connections").

The agent must have a persistent storage to manage the states of known connections and of the client-side information of two SMP queues that each connection consists of, and also the buffer of the most recent messages. The number of the messages that should be stored is implementation specific, depending on the error management approach that the agent implements; at the very least the agent must store the hash and id of the last received message.

## SMP agent protocol components

SMP agent protocol has 3 main parts:

- the syntax and semantics of messages that SMP agents exchange between each other in order to:
  - negotiate establishing unidirectional (simplex) encrypted queues on SMP server(s)
  - exchange client messages and delivery notifications, providing sequential message IDs and message integrity (by including the hash of the previous message).
- the syntax and semantics of the commands (a higher level interface than SMP protocol) that are sent over TCP or other sequential protocol by agent clients to the agents. This protocol allows to create and manage multiple connections, each consisting of two simplex SMP queues.
- the syntax and semantics of the message that the clients of SMP agents should send out-of-band (as pre-shared "invitation" including SMP server, queue ID and encryption key) to ensure [E2E encryption][1] the integrity of SMP queues and protection against active attacks ([MITM attacks][2]).

## Duplex connection

**Creating duplex connection between Alice and Bob:**

![Creating duplex connection](/diagrams/duplex-messaging/duplex-creating.svg)

## Communication between SMP agents

SMP agents communicate via SMP servers managing creation, deletion and operations of SMP queues.

Agents can use SMP message client body (the part of the SMP message after header - see [SMP protocol](./simplex-messaging.md)) to transmit agent client messages and exchange messages between each other.

Each SMP message client body, once decrypted, contains 3 parts (one of them may include binary message body), as defined by `decryptedSmpMessageBody` syntax:

- `agentMsgHeader` - agent message header that contains sequential agent message ID for a particular SMP queue, agent timestamp (ISO8601) and the hash of the previous message.
- `agentMessage` - a command/message to the other SMP agent:
  - to establish the connection with two SMP queues (`helloMsg`, `replyQueueMsg`)
  - to send and to acknowledge user messages (`clientMsg`, `acknowledgeMsg`)
  - to notify another agent about queue deletion (`deleteQueueMsg`)
- `msgPadding` - an optional message padding to make all SMP messages have consistent size as an additional privacy protection measure.

### Messages between SMP agents

Message syntax below uses [ABNF][3] with [case-sensitive strings extension][4].

```abnf
decryptedSmpMessageBody = agentMsgHeader CRLF agentMessage CRLF msgPadding
agentMsgHeader = agentMsgId SP agentTimestamp SP previousMsgHash
agentMsgId = 1*DIGIT ; sequential agent message ID set by the sending agent

agentMessage = helloMsg / replyQueueMsg / deleteQueueMsg
               / clientMsg / acknowledgeMsg

msgPadding = *OCTET ; optional random bytes to get messages to the same size (as defined in SMP message size)

helloMsg = %s"HELLO" SP signatureVerificationKey [SP %s"NO_ACK"]
; NO_ACK means that acknowledgements to client messages will NOT be sent in this connection by the agent that sent `HELLO` message.
signatureVerificationKey = encoded ; base64 encoded

replyQueueMsg = %s"REPLY" SP qInfo ; `qInfo` is the same as in out-of-band message
; this message can only be sent by the second connection party

deleteQueueMsg = %s"DEL" ; notification that recipient queue will be deleted
; no need to notify the other party about suspending queue separately, as suspended and deleted queues are the same to the sender 
; NOT SUPPORTED with the current implementation

clientMsg = %s"MSG" SP size CRLF clientMsgBody CRLF ; CRLF is in addition to CRLF in decryptedSmpMessageBody
size = 1*DIGIT
clientMsgBody = *OCTET

acknowledgeMsg = %s"ACK" SP agentMsgId SP ackStatus
; NOT SUPPORTED with the current implementation

ackStatus = %s"OK" / ackError

ackError = %s"ERR" SP ackErrorType

ackErrorType = ackUnknownMsg / ackProhibitedMsg / ackSyntaxErr

ackUnknownMsg = %s"UNKNOWN"

ackProhibitedMsg = %"PROHIBITED" ; e.g. "HELLO" or "REPLY"

ackSyntaxErr = %"SYNTAX" SP syntaxErrCode
syntaxErrCode = 1*DIGIT ; TODO
```

#### HELLO message

This is the first message that both agents send after the respective SMP queue is secured by the receiving agent (see diagram). It contains the verification key that the sender will use to cryptographically sign the messages.

Sending agent might need to retry sending HELLO message, as it would not have any other confirmation that the queue is secured other than the success of sending this message with the signed SEND command of SMP protocol.

#### REPLY message

This is the message that is sent by the agent that received an out-of-band invitation to pass the invitation to the reply SMP queue to the agent that originated the connection (see diagram).

#### MSG message

This is the agent envelope used to send client messages once the connection is established. Do not confuse it with the MSG response from SMP server to the agent and MSG response from SMP agent to the client that are sent in different contexts.

## SMP agent commands

This part describes the transmissions between users and client-side SMP agents: commands that the users send to create and operate duplex connections and SMP agent responses and messages they deliver.

Commands syntax below is provided using [ABNF][3] with [case-sensitive strings extension][4].

Each transmission between the user and SMP agent must have this format/syntax:

```abnf
agentTransmission = [corrId] CRLF [cAlias] CRLF agentCommand

corrId = 1*(%x21-7F) ; any characters other than control/whitespace

cAlias = cId / cName
cId = encoded
cName = 1*(ALPHA / DIGIT / "_" / "-")

agentCommand = (userCmd / agentMsg) CRLF
userCmd = newCmd / joinCmd
          / acceptCmd / subscribeCmd
          / sendCmd / acknowledgeCmd
          / suspendCmd / deleteCmd

agentMsg = invitation / confirmation
          / connected / unsubscribed
          / message / sent / received
          / ok / error

newCmd = %s"NEW" SP smpServer [SP %s"NO_ACK"]
; response is `invitation` or `error`
smpServer = srvHost [":" port] ["#" keyFingerprint]
srvHost = hostname ; RFC1123, RFC5891
port = 1*DIGIT
keyFingerprint = encoded

invitation = %s"INV" SP qInfo

connected = %s"CON"

subscribeCmd = %s"SUB" ; response is `ok` or `error`

unsubscribed = %s"END"
; when another agent (or another client of the same agent)
; subscribes to the same SMP queue on the server

joinCmd = %s"JOIN" SP qInfo
                [SP (smpServer / %s"NO_REPLY")] ; reply queue SMP server
                ; server from qInfo is used by default
                [SP %s"NO_ACK"]         
; response is `connected` or `error`

confirmation = %s"CONF" SP partyId SP partyInfo
; currently not implemented

acceptCmd = %s"LET" SP partyId ; response is `ok` or `error`
; currently not implemented

suspendCmd = %s"OFF" ; can be sent by either party, response `ok` or `error`

deleteCmd = %s"DEL" ; can be sent by either party, response `ok` or `error`

sendCmd = %s"SEND" SP msgBody
; send syntax is similar to that of SMP protocol, but it is wrapped in SMP message
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary

sent = %s"SENT" SP agentMsgId

message = %s"MSG" SP msgIntegrity
          SP %s"R=" agentMsgId "," agentTimestamp ; receiving agent
          SP %s"B=" brokerMsgId "," srvTimestamp ; broker (server)
          SP %s"S=" agentMsgId "," agentTimestamp ; sending agent
          SP binaryMsg
agentMsgId = 1*DIGIT
srvTimestamp = date-time ; RFC3339
agentTimestamp = date-time
msgIntegrity = ok / messageError

messageError = %s"ERR" SP messageErrorType
messageErrorType = skippedMsgErr / badMsgIdErr / badHashErr

skippedMsgErr = %"IDS" SP missingFromMsgId SP missingToMsgId
badMsgIdErr = %"ID" SP previousMsgId ; ID is lower than the previous
badHashErr = %"HASH"

acknowledge = %s"ACK" SP agentMsgId ; ID assigned by receiving agent (in MSG "R")
; currently not implemented

received = %s"RCVD" SP agentMsgId ; ID assigned by sending agent (in SENT response)
; currently not implemented

ok = %s"OK"

error = %s"ERR" SP errorType

encoded = base64
```

### Client commands and server responses

#### NEW command and INV response

`NEW` command is used to create a connection and an invitation to be sent out-of-band to another protocol user. It should be used by the client of the agent that initiates creating a duplex connection.

`INV` response is sent by the agent to the client.

#### JOIN command

It is used to create a connection and accept the invitation received out-of-band. It should be used by the client of the agent that accepts the connection.

#### CON notification

It is sent by both agents managing duplex connection to their clients once the connection is established and ready to accept client messages.

#### SUB command

This command can be used by the client to resume receiving messages from the connection that was created in another TCP/client session. Agent response to this command can be `OK` or `ERR` in case connection does not exist (or can only be used to send connections - e.g. when the reply queue was not created).

#### SEND command and SENT response

`SEND` command is used to the client to send messages

`SENT` response is sent by the agent to confirm that the message was delivered to the SMP server. Message ID in this response is the sequential message number that includes both sent and received messages in the connection.

#### MSG notification

It is sent by the agent to the client when agent receives the message from the SMP server. It has message ID and timestamp from both the receiving and sending agents and from SMP server:
- recipient agent ID is intended to be used to refer to the message in the future.
- sender agent ID is intended to be used to identify any missed / skipped message(s)
- broker ID should be used to detect duplicate deliveries (it would happen if TCP connection is lost before the message is acknowledged by the agent - see [SMP protocol](./simplex-messaging.md))

#### END notification

It is sent by the agent to the client when agent receives SMP protocol `END` notification from SMP server. It indicates that another agent has subscribed to the same SMP queue on the server and the server terminated the subscription of the current agent.

#### OFF command

It is used to suspend the receiving SMP queue - sender will no longer be able to send the messages to the connection, but the recipient can retrieve the remaining messages. Agent response to this command can be `OK` or `ERR`. This command is irreversible.

#### DEL command

It is used to delete the connection and all messages in it, as well as the receiving SMP queue and all messages in it that were remaining on the server. Agent response to this command can be `OK` or `ERR`. This command is irreversible.

## Connection invitation

Connection invitation `qInfo` is generated by SMP agent in response to `newCmd` command (`"NEW"`), used by another party user with `joinCmd` command (`"JOIN"`), and then another invitation is sent by the agent in `replyQueueMsg` and used by the first party agent to connect to the reply queue (the second part of the process is invisible to the users).

Connection invitation is a text with the following syntax:

```
qInfo = %s"smp::" smpServer "::" queueId "::" ephemeralPublicKey
queueId = encoded
ephemeralPublicKey = encoded ; RSA key for sender to encrypt messages X509 base64 encoded
```

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[3]: https://tools.ietf.org/html/rfc5234
[4]: https://tools.ietf.org/html/rfc7405