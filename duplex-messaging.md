# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [SMP agent commands](#smp-agent-commands)

## Abstract

Protocol describes the command that can be sent to the client that communicates with multiple SMP servers. This protocol allows to manage multiple simplex connections organised into groups defining duplex communication channels.

## SMP agent commands

Commands syntax below is provided using [ABNF][1].

Each transmission between the user and SMP agent must have this format/syntax
(after unwrapping it out of SMP message):

```abnf
transmission = (userCmd / agentMsg) CRLF
userCmd = create / alias / subscribe
          / invite / accept
          / send / acknowledge
          / status / info
          / suspend / delete
agentMsg = connection / connectionInfo / invitation / message / unsubscribed / ok / error

create = %s"NEW" SP serverUris ; response is "connection" or "error"
serverUris = serverUri ["," serverUris]

connection = %s"ID" SP cAlias SP recipientQueuesStatus SP senderQueuesStatus SP cId
alias = %s"NAME" SP cName SP cId ; response is "connection" or "error"
status = %"STAT" SP cAlias ; response is "connection" or "error"
cId = encoded
cAlias = cId / cName ; TODO add cName syntax - it's letters, numbers, "-" and "_"

; only needed for debugging, so maybe should be removed or maybe keys should be added
info = %s"INFO" SP cAlias ; response is "connectionInfo" or "error
connectionInfo = %s "IDS" queueId queueId

recipientQueuesStatus = rqStatus ["," recipientQueuesStatus]
senderQueuesStatus = sqStatus ["," senderQueuesStatus]

sqStatus = serverUri SP (%s"NONE" / %s"NEW / %s"CONFIRMED" / %s"SECURED")
rqStatus = serverUri SP (sqStatus / %s"PENDING / %s"DISABLED")
; see comments in ./core/definitions/src/Simplex/Messaging/Core.hs

invite = %s"INV" SP cAlias ; response is "invitation" or "error"
invitation = %s"JOIN" SP cInfo

accept = %s"ACC" SP cInfo ; response is "connection" or "error"

suspend = %s"OFF" SP cAlias ; unlike SMP, can be executed by either side

delete = %s"DEL" SP cAlias ; unlike SMP, can be executed by either side

send = %s"SEND" SP cAlias SP msgBody
; send syntax is similar to that of SMP protocol, but it is wrapped in SMP message
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary

message = %s"MSG" SP cAlias SP msgId SP timestamp SP binaryMsg
msgId = encoded
timestamp = date-time; RFC3339

encoded = base64
```

[1]: https://tools.ietf.org/html/rfc5234

