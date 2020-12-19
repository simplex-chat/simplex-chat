# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [Messages between SMP agents](#messages-between-smp-agents)
- [SMP agent commands](#smp-agent-commands)

## Abstract

SMP agent protocol has 3 main parts:

- the syntax and semantics of messages that SMP agents exchange between each
  other to negotiate establishing multiple unidirectional (simplex) encrypted
  queues on SMP server(s) to provide their users convenient interface to
  establish and operate duplex (bi-directional) connections, providing
  redundancy, server, queue and key rotation, and notifications about any
  communication integrity violations.
- the syntax and semantics of the commands that should be sent over TCP or other
  sequential streaming protocol to the client-side agent that communicates with
  one or multiple SMP servers. This protocol allows to manage multiple simplex
  connections organised into groups defining duplex communication channels that
  can be used to build higher-level communication and application primitives.
- the syntax and contents of messages that users of SMP agents should send
  out-of-band ("invitation") to ensure [E2E encryption][1] integrity for the
  first SMP queue and protection against active attacks ([MITM attacks][2]).

## Messages between SMP agents

Message syntax below is provided using [ABNF][1].

```abnf
decryptedSmpMessageBody = agentMsgHeader CRLF agentMessage CRLF contentMessageBody CRLF
agentMsgHeader = agentMsgSeqId SP agentTimestamp SP previousMsgHash
agentMsgSeqId = 1*DIGIT
contentMessageBody = *OCTET

agentSmpMessage = confirmMsg / addQueueMsg / delQueueMsg / errorQueueMsg
                / badMessageMsg / contentMessage

confirmMsg = %s"KEY" SP senderKey
senderKey = encoded

addQueueMsg = %s"QADD" SP qInfo ; notification that recipient queue is added - same as out-of-band message

delQueueMsg = %s"QDEL" SP qNum ; notification that recipient queue will be deleted - sender shouldn't sent anything to it further

errorQueueMsg = %s "QERR" SP qNum SP queueErrorInfo
messageErrorInfo = skippedMsgErr / badHashErr / noMessagesErr
skippedMsgErr = %"SKIPPED" SP fromAgentMsgSeqId SP toAgentMsgSeqId
badHashErr = %"HASH" SP agentMsgSeqId
noMessagesErr = %s"NOMSG"

badMessageMsg = %s"MERR" SP messageErrorInfo

contentMessage = %s"MSG"
```

### First message

To secure connection

### Message to

## SMP agent commands

This part describes the transmissions between users and client-side SMP agents:
commands that the users send to create and operate duplex connections and SMP
agent responses and messages they deliver.

Commands syntax below is provided using [ABNF][1].

Each transmission between the user and SMP agent must have this format/syntax
(after unwrapping it out of SMP message):

```abnf
transmission = (userCmd / agentMsg) CRLF
userCmd = create / alias
          / subscribe / unsubscribe
          / invite / connect / extend / accept
          / send / acknowledge
          / suspend / delete
          / status / info

agentMsg = connection / connectionInfo / invitation / message / unsubscribed / ok / error

create = %s"NEW"; response is "connection" or "error"
connection = %s"ID" SP cAlias SP recipientQueuesInfo SP senderQueuesInfo SP cId

alias = %s"NAME" SP cAlias SP cName ; response is "ok" or "error"
status = %"STAT" SP cAlias ; response is "connection" or "error"
cId = encoded
cAlias = cId / cName ; TODO add cName syntax - letters, numbers, "-" and "_"

recipientQueuesInfo = rqInfo ["," recipientQueuesInfo]
senderQueuesInfo = sqInfo ["," senderQueuesInfo]

rqInfo = serverHost "-" qId "-" rqState "-" [%s"SUB"]
sqInfo = serverHost "-" qId "-" sqState "-"
qId = encoded

sqState = %s"NONE" / %s"NEW / %s"CONFIRMED" / %s"SECURED"
rqState = sqState / %s"PENDING / %s"DISABLED"
; see comments in ./core/definitions/src/Simplex/Messaging/Core.hs

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
