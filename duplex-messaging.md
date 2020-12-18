# SMP agent protocol - duplex communication over SMP protocol

## Table of contents

- [Abstract](#abstract)
- [Messages between SMP agents](#messages-between-smp-agents)
- [SMP agent commands](#smp-agent-commands)

## Abstract

SMP agent protocol has 3 main parts:
- the syntax and semantics of messages that SMP agents exchange between each other to negotiate establishing multiple unidirectional (simplex) encrypted queues on SMP server(s) to provide their users convenient interface to establish and operate duplex (bi-directional) connections, providing redundancy, server, queue and key rotation, and notifications about any communication integrity violations.
- the syntax and semantics of the commands that should be sent over TCP or other sequential streaming protocol to the client-side agent that communicates with one or multiple SMP servers. This protocol allows to manage multiple simplex connections organised into groups defining duplex communication channels that can be used to build higher-level communication and application primitives.
- the syntax and contents of messages that users of SMP agents should send out-of-band to ensure [E2E encryption][1] integrity for the first SMP queue and protection against active attacks ([MITM attacks][2]).

## SMP agent commands

This part describes the transmissions between users and client-side SMP agents: commands that the users send to create and operate duplex connections and SMP agent responses and messages they deliver.

Commands syntax below is provided using [ABNF][1].

Each transmission between the user and SMP agent must have this format/syntax
(after unwrapping it out of SMP message):

```abnf
transmission = (userCmd / agentMsg) CRLF
userCmd = addServer / deleteServer / killServer / listServers
          / create / alias / subscribe / suspend / delete
          / invite / accept / send / acknowledge
          / status / info

agentMsg = connection / connectionInfo / invitation / message / unsubscribed / ok / error

addServer = %s"SRV ADD" SP serverHost
deleteServer = %s"SRV DEL" SP serverHost
killServer = %s"SRV KILL" SP serverHost
listServers = %s"SRV LS" SP serverHost

create = %s"NEW"; response is "connection" or "error"

connection = %s"ID" SP cAlias SP recipientQueuesStatus SP senderQueuesStatus SP cId
alias = %s"NAME" SP cAlias SP cName ; response is "ok" or "error"
status = %"STAT" SP cAlias ; response is "connection" or "error"
cId = encoded
cAlias = cId / cName ; TODO add cName syntax - letters, numbers, "-" and "_"

; only needed for debugging, so maybe should be removed (or maybe encryption keys should be added)
info = %s"INFO" SP cAlias ; response is "connectionInfo" or "error
connectionInfo = %s "IDS" queueIds queueIds

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

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[3]: https://tools.ietf.org/html/rfc5234
