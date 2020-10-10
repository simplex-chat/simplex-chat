# Simplex messaging protocol (SMP)

## Table of contents

- [Abstract](#abstract)
- [Introduction](#introduction)
- [SMP Model](#smp-model)
- [Out-of-band messages](#out-of-band-messages)
- [Simplex connection](#simplex-connection)
- [SMP procedure](#smp-procedure)
- [SMP elements](#smp-elements)
- [SMP qualities and features](#smp-qualities-and-features)
- [Cryptographic algorithms](#cryptographic-algorithms)
- [Simplex connection IDs](#simplex-connection-ids)
- [Server privacy requirements](#server-privacy-requirements)
- [SMP commands](#smp-commands)
  - [Correlating responses with commands](#correlating-responses-with-commands)
  - [Command authentication](#command-authentication)
  - [Recipient commands](#recipient-commands)
    - [Create connection command](#create-connection-command)
    - [Subscribe to connection](#subscribe-to-connection)
    - [Secure connection command](#secure-connection-command)
    - [Delete message command](#delete-message-command)
    - [Suspend connection](#suspend-connection)
    - [Delete connection](#delete-connection)
  - [Sender commands](#sender-commands)
    - [Send message command](#send-message-command)
  - [Server messages](#server-messages)
    - [New connection response](#new-connection-response)
    - [Deliver connection message](#deliver-connection-message)
    - [Error responses](#error-responses)
    - [OK response](#ok-response)
- [Appendices](#appendices)
  - [Appendix A. Transport connection with the SMP server](#appendix-a)
  - [Appendix B. Sending out-of-band message](#appendix-b)

## Abstract

Simplex messaging protocol is a transport agnostic client-server protocol for
asynchronous distributed secure unidirectional message transmission.

It's designed with the focus on communication security and integrity, under the
assumption that any part of the message transmission network can be compromised.

It addresses the problems of existing communication protocols that undermine
communication security and privacy:

- Identity related problems:

  - visibility of user contacts to anybody observing messages
  - unsolicited messages (spam and abuse)
  - trademark issues (when usernames are used)
  - privacy issues (when phone numbers are used)
  - Participants' identities are known to the network. Depending on the identity
    type (e.g., phone number, DNS-based, username, uuid, public key, etc.) it
    creates different problems, but in all cases it exposes participants and
    their contacts graph to the network and also allows for unsolicited messages
    (spam and abuse).

- [MITM attack][1]. Any mechanism of the encryption key exchange via the same
  network is prone to this type of attack when the public keys of the
  participants are substituted with the public keys of the attacker intercepting
  communication. While some solutions have been proposed that complicate MITM
  attack (social millionaire, OTR), if the attacker understands the protocol and
  has intercepted and can substitute all information exchanged between the
  participants, it is still possible to substitute encryption keys. It means
  that the existing [E2EE][2] implementations in messaging protocols and
  platforms can be compromised by the attacked who compromised the server or
  communication channel.

## Introduction

The objective of Simplex Messaging Protocol (SMP) is to facilitate the secure
and private unidirectional transfer of messages from senders to recipients.

SMP is independent of the particular transmission system and requires only a
reliable ordered data stream channel. While this document describes transport
over TCP, other transports are also possible.

The protocol describes the set of commands that recipient and sender can
exchange with the SMP server to create and to operate a unidirectional
"connection" (a data abstraction identifying one of many communication channels
managed by the server) and to send messages from the sender to the recipient via
the SMP server.

More complex communication scenarios can be designed using multiple
connections - for example, a duplex communication channel can be made of 2
simplex connections.

Protocol is designed with the focus on privacy and security, to some extent
deprioritizing reliability by requiring that SMP servers only store messages
until they are delivered to the recipients and, in any case, for a limited
period of time. For communication scenarios requiring more reliable transmission
the users should use several SMP servers to pass each message and implement some
additional protocol (e.g., based on blockchain) to ensure that messages are not
removed, inserted or re-ordered - this is out of scope of this document.

SMP removes the need for participants' identities and provides [E2EE][2] without
the possibility of [MITM attack][1] attack relying on two pre-requisites:

- the users can establish a secure encrypted transport connection with the SMP
  server. [Appendix A](#appendix-a) has a possible simple protocol of such
  transport connection over TCP, but any other transport connection encryption
  protocol can be used.
- the recipient can pass a single message to the sender via pre-existing secure
  and private communication channel (out-of-band message) - the information in
  this message is used to establish the connection via SMP server that the
  sender will use to send the encrypted messages to the recipient.

## SMP Model

The SMP model has three communication participants: the recipient, the message
broker (SMP server) that is chosen and, possibly, controlled by the recipient,
and the sender.

SMP server manages multiple "simplex connections" - data records on the server
that identify communication channels from the senders to the recipients. The
same communicating party that is the sender in one connection, can be the
recipient in another - without exposing this fact to the server.

The connection record consists of 2 unique random IDs generated by the server,
one for the recipient and another for the sender, and 2 keys to authenticate the
recipient and the sender respectively. The users of SMP protocol must use a
unique key for each connection, to avoid the possibility of aggregating and
analysing their connections in case SMP server is compromised.

Creating and using this connection requires sending commands to the SMP server
from the recipient and the sender - they are described in detail in
[SMP commands](#smp-commands) section.

## Out-of-band messages

The out-of band invitation message is sent via some trusted alternative channel
by the recipient to the sender. This message is used to share the encryption
(a.k.a. "public") key that the sender will use to encrypt the messages (to be
decrypted by the recipient), sender connection ID, server address and any other
information necessary to establish secure encrypted connection with SMP server
(see [Appendix A](#appendix-a) for a simple transport protocol example).

The syntax of the message defined with [ABNF][8] is:

```
outOfBandMsg = "(" connId "," serverHost "," transportInfo ")"
connId = encoded ; defined below
serverHost = DQUOTE hostname DQUOTE ; RFC 1123, section 2.1
transportInfo = JSON ; for example, TCP port number and encryption key fingerprint
```

Defining the approach to out-of-band message passing is out of scope of the
simplex messaging protocol. See [Appendix B](#appendix-b) for one of the
possible practical approaches to passing out-of-band message.

## Simplex connection

The simplex connection is the main unit of SMP protocol. It is used by:

- the sender of the connection (who received out-of-band message) to send
  messages to the server using connection ID, signed by sender's key.
- the recepient of the connection (who created the connection and who sent
  out-of-band message) will use it to retrieve messages from the server, signing
  the commands by the recepient key.
- participant identities are not shared with the server - new unique keys and
  connection IDs are used for each connection.

This simplex connection can serve as a building block for more complex
communication network. For example, two (or more, for redundancy) simplex
connections can be used to create a duplex communication channel. Higher level
primitives that are only known to system participants in their client
applications can be created as well - user profiles, contacts, conversations,
groups and broadcasts. Simplex messaging servers have have the information about
the low-level simplex connections. In this way a high level of privacy and
security of the conversations is provided. Application level primitives are not
in scope of the simplex messaging protocol.

This approach is based on the concept of [unidirectional networks][4] that are
used for applications with high level of information security.

Access to each connection is controlled with unique (not shared with other
connections) assymetric key pairs, separate for sender and the recipient. The
sender and the receiver have private keys, and the server has associated public
keys to authnticate participants' commands by verifying the signatures.

The messages sent into the connection are encrypted and decrypted using another
key pair - the recepient has the private key and the sender has the associated
public key.

**Simplex connection diagram:**

![Simplex connection](/diagrams/simplex-messaging/simplex.svg)

Connection is defined by recipient ID `RID` unique for the server. It also has a
different unique sender ID `SID`. Sender key (`SK`) is used by the server to
verify sender's commands (identified by `SID`) to send messages. Recipient key
(`RK`) is used by the server to verify recipient's commands (identified by
`SID`) to retrieve messages.

The protocol uses different IDs for sender and recipient in order to provide an
additional connection privacy by complicating correlation of senders and
recipients commands sent over the network - even though they are encrypted using
server's public key, in case this key is compromised it would still be difficult
to correlate senders and recipients, it would require access to connections
records on the server.

## SMP procedure

The SMP procedure of creating a simplex connection on SMP server is explained
using participants Alice (the recipient) who wants to receive the messages from
Bob (the sender).

To create a simpelex connection Alice and Bob follow these steps:

1. Alice creates a simplex connection on the server:
   1. decides which SMP server to use (can be the same or different server that
      Alice uses for other connections) and opens secure encrypted transport
      connection to the chosen SMP server (see [Appendix A](#appendix-a)).
   2. generates a new random public/private key pair (encryption key - `EK`)
      that she did not use before for Bob to encrypt the messages.
   3. generates another new random public/private key pair (recepient key -
      `RK`) that she did not use before for her to sign commands and to decrypt
      the transmissions received from the server.
   4. sends the command to the server to create a simplex connection (see
      `create` in [Create connection command](#create-connection-command)). This
      command can either be anonymous or the server can be configured to use the
      signature field to authenticate the users who are allowed to create
      connections. This connection command contains previouisly generated uniqie
      "public" key `RK` that will be used to sign the following commands related
      to the same connection, for example to subscribe to the messages received
      to this connection or to update the connection, e.g. by setting the key
      required to send the messages (initially Alice creates the connection that
      accepts unsigned commands to send messages, so anybody could send the
      message via this connection if they knew the connection ID and server
      address).
   5. The server responds with connection IDs (`connResp`):
      - recipient ID `RID` for Alice to manage the connection and to receive the
        messages.
      - sender ID `SID` for Bob to send messages to the connection.
2. Alice sends an out-of-band message to Bob via the alternative channel that
   both Alice and Bob trust (see
   [Simplex messaging protocol abstract](#simplex-messaging-protocol-abstract)
   and [Appendix B](#appendix-b)). The message must include:
   - the unique "public" key (`EK`) that Bob must use to encrypt messages.
   - SMP server address and information to open secure encrypted transport
     connection (see [Appendix A](#appendix-a))
   - the sender connection ID `SID` for Bob to use.
3. Bob, having received the out-of-band message from Alice, accepts the
   connection:
   1. generates a new random public/private key pair (sender key - `SK`) that he
      did not use before for him to sign commands to Alice's server to send the
      messages.
   2. prepares the confirmation message for Alice to secure the connection. This
      message includes:
      - previously generated "public" key `SK` that will be used by Alice's
        server to authenticate Bob's commands to send messages, once the
        connection is secured.
      - optionally, any additional information (application specific, e.g. Bob's
        profile name and details).
   3. encrypts the confirmation body with the "public" key `EK` (that Alice
      provided via the out-of-band message).
   4. sends the encrypted message to the server with connection ID `SID` (see
      `send` in [Send message command](#send-message-command)) to confirm the
      connection. This message to confirm the connection must not be signed -
      signed messages will be rejected until Alice secures the connection
      (below).
4. Alice receives Bob's message from the server using recipient connection ID
   `RID` (possibly, via the same transport connection she already has opened -
   see `message` in [Deliver connection message](#deliver-connection-message)):
   1. she decrypts received message with "private" key `EK`.
   2. even though anybody could have sent the message to the connection with ID
      `SID` before it is secured (e.g. if communication is compromised), Alice
      would ignore all messages until the decryption succeeds (i.e. the result
      contains the expected message format). Optionally, in the client
      application, she also may identify Bob using the information provided, but
      it is out of scope of SMP protocol.
5. Alice secures the connection `RID` so only Bob can send messages to it (see
   `secure` in [Secure connection command](#secure-connection-command)):
   1. she sends the command with `RID` signed with "private" key `RK` to update
      the connection to only accept requests signed by "private" key `SK`
      provided by Bob.
   2. From this moment the server will accept only signed commands to `SID`, so
      only Bob will be able to send messages to the connection `SID`
      (corresponding to `RID` that Alice has).
   3. Once connection is secured, Alice deletes `SID` and `SK` - even if Alice's
      client is compromosed in the future, the attacker would not be able to
      send messages pretending to be Bob.
6. The simplex connection `RID` is now established on the server.

This flow is shown on the sequence diagram below.

**Creating simplex connection from Bob to Alice:**

![Creating connection](/diagrams/simplex-messaging/simplex-creating.svg)

Bob now can securely send messages to Alice:

1. Bob sends the message:
   1. he encrypts the message to Alice with "public" key `EK` (provided by
      Alice, only known to Alice and Bob, used only for one simplex connection).
   2. he signs the command to the server connection `SID` using the "private"
      key `SK` (that only he knows, used only for this connection).
   3. he sends the command to the server (see `send` in
      [Send message command](#send-message-command)), that the server will
      authenticate using the "public" key `SK` (that Alice earlier provided to
      the server).
2. Alice receives the message(s):
   1. she signs the command to the server to subscribe to the connection `RID`
      with the "private" key `RK` (see `subscribeCmd` in
      [Subscribe to connection](#subscribe-to-connection)).
   2. the server, having authenticated Alice's command with the "public" key
      `RK` that she provided, delivers Bob's message(s) (see `message` in
      [Deliver connection message](#deliver-connection-message)).
   3. she decrypts Bob's message(s) with the "private" key `EK` (that only she
      has).

This flow is show on sequence diagram below.

**Sending messages from Bob to Alice via simplex connection:**

![Using connection](/diagrams/simplex-messaging/simplex-using.svg)

**Simplex connection operation:**

![Simplex connection operations](/diagrams/simplex-messaging/simplex-op.svg)

Sequence diagram does not show E2EE - connection itself knows nothing about
encryption between sender and receiver.

A higher level protocol application protocol should define the semantics that
allow to use two simplex connections (or two sets of connections for redundancy)
for the bi-directional chat and for any other communication scenarios.

The SMP is intentionally unidirectional - it provides no answer to how Bob will
know that the transmission succeeded, and whether Alice received any messages.
There may be a situation when Alice wants to securely receive the messages from
Bob, but she does not want Bob to have any proof that she received any
messages - this low-level simplex messaging protocol can be used in this
scenario, as all Bob knows as a fact is that he was able to send one unsigned
message to the server that Alice provided, and now can only send messages signed
with the key `SK` that he sent to the server - it does not prove that any
message was received by Alice.

For practical purposes of bi-directional conversation, now that Bob can securely
send encrypted messages to Alice, Bob can establish the second simplex
connection that will allow Alice to send messages to Bob in the same way. If
both Alice and Bob have their respective uniqie "public" keys (Alice's and Bob's
`EK`s of two separate connections), the conversation can be both encrypted and
signed.

The established connections can also be used to change the encryption keys
providing [forward secrecy][5].

This protocol also can be used for off-the-record messaging, as Alice and Bob
can have multiple connections established between them and only information they
pass to each other allows proving their identity, so if they want to share
anything off-the-record they can initiate a new connection without linking it to
any other information they exchanged. As a result, this protocol provides better
anonymity and better protection from [MITM][1] than [OTR][6] protocol.

How simplex connections are used by the participants is not in scope of this low
level simplex messaging protocol.

## SMP qualities and features

The simplex messaging protocol:

- defines only message-passing protocol:
  - transport agnostic - the protocol does not define how clients connect to the
    servers and does not require persistent connections. While a generic term
    "command" is used, it can be implemented in various ways - TCP connection,
    HTTP requests, messages over (web)sockets, etc..
  - not semantic - the protocol does not assign any meaning to connections and
    messages. While on the application level the connections and messages can
    have different meaning (e.g., for messages: text or image chat message,
    message acknowledgement, participant profile information, status updates,
    changing "public" key to encrypt messages, changing servers, etc.), on the
    simplex messaging protocol level all the messages are binary and their
    meaning can only be interpreted by client applications and not by the
    servers - this interpretation is in scope of application level protocol and
    out of scope of this simplex messaging protocol.
- client-server architecture:
  - multiple servers, that can be deployed by the system users, can be used to
    send and retrieve messages.
  - servers do not communicate with each other and do not even "know" about
    other servers.
  - clients only communicate with servers (excluding the initial out-of-band
    message), so the message passing is asynchronous.
  - for each connection, the message recipient defines the server through which
    the sender should send messages.
  - while multiple servers and multiple connections can be used to pass each
    message, it is in scope of application level protocol(s), and out of scope
    of this simplex messaging protocol.
  - servers store messages only until they are retrieved by the recipients, and
    in any case, for a limited time.
  - servers are required to NOT store any message history or delivery log, but
    even if the server is compromised, it does not allow to decrypt the messages
    or to determine the list of connections established by any participant -
    this information is only stored on client devices.
- the only element provided by SMP servers is simplex connections:
  - each connection is created and managed by the connection recipient.
  - assymetric encryption is used to sign and verify the requests to send and
    receive the messages.
  - one unique "public" key is used for the servers to authenticate requests to
    send the messages into the connection, and another unique "public" key - to
    retrieve the messages from the connection. "Unique" here means that each
    "public" key is used only for one connection and is not used for any other
    context - effectively this key is not public and does not represent any
    participant identity.
  - both "public" keys are provided to the server by the connection recepient
    when the connection is established.
  - the "public" keys known to the server and used to authenticate commands from
    the participants are unrelated to the keys used to encrypt and decrypt the
    messages - the latter keys are also unique per each connection but they are
    only known to participants, not to the servers.
  - messaging graph can be asymmetric: Bob's ability to send messages to Alice
    does not automatically lead to the Alice's ability to send messages to Bob.
  - connections are identified by sender and recipient server IDs.

## Cryptographic algorithms

Simplex messaging clients need to cryptographically sign commands:

- with the recipient's key `RK` (server to verify):
  - to subscribe to connection.
  - to secure the connection.
  - to delete received messages.
  - to suspend the connection.
  - to delete the connection.
- with the sender's key `SK`:
  - to send messages (server to verify).

To sign and verify commands, clients and servers MUST use RSA-PSS algorythm
defined in [RFC3447][2].

To optinally sign and verify messages, clients SHOULD use RSA-PSS algorythm.

To encrypt and decrypt messages, clients and servers SHOULD use RSA-OAEP
algorythm defined in [RFC3447][2].

The reasons to use these algorithms:

- they are supported by WebCrypto API.
- they are newer versions than RSA-PKCS1-v1_5 encryption and signature schemes.
- they are more widely supported than ECC algorithms

Future versions of the protocol may allow different algorithms.

## Simplex connection IDs

Simplex messaging servers MUST generate 2 different IDs for each new
connection - for recipient (that created the connection) and for sender. It is
REQUIRED that:

- these IDs are different and unique within the server.
- based on 128-bit integers generated with cryptographically strong
  pseudo-random number generator.

## Server privacy requirements

Simplex messaging server implementations MUST NOT create, store or send to any
other servers:

- logs of the client commands and transport connections in the production
  environment.
- history of deleted connections, retrieved or removed messages.
- snapshots of the database they use to store connections and messages (instead
  simplex messaging clients must manage redundancy by using more than one
  simplex messaging server.
- any other information that may compromise privacy or [forward secrecy][4] of
  communication between clients using simplex messaging servers.

## SMP commands

Commands syntax below is provided using [ABNF][8].

Each transmission between the client and the server must have this format/syntax
(after the decryption):

```abnf
transmission = signed CRLF signature CRLF
signed = connId CRLF msg
msg = recipientCmd / send / serverMsg
recipientCmd = create / subscribe / secure / deleteMsg / suspend / delete
serverMsg = conn / ok / error / message
connId = (encoded " ") / "" ; empty connection ID is used with "create" command
signature = encoded / "" ; empty signature can be used with "create" and "send" commands
encoded = base64
```

`base64` encoding should be used with padding, as defined in section 4 of [RFC
4648][9]

The syntax of specific commands and responses is defined below.

### Correlating responses with commands

The server must send `conn`, `error` and `ok` responses in the same order within
each connection ID as the commands received in the transport connection, so that
they can be correlated by the clients.

If the transport connection is closed before some responses are sent, these
responses should be discarded.

### Command authentication

The SMP servers must athenticate all transmissions (excluding `create` and
`send` commands sent with empty signatures) by verifying the provided
signatures. Signature should be the hash of the first part `signed` (including
CRLF characters) of `transmission`, encrypted with the key associated with the
connection ID (sender's or recepient's, depending on which connection ID is
used).

### Recipient commands

Sending any of the commands in this section (other than `create`, that is sent
without connection ID) is only allowed with recipient's ID (`RID`). If sender's
ID is used the server must respond with `"ERROR AUTH"` response (see
[Error responses](#error-responses)).

#### Create connection command

This command is sent by the recipient to the SMP server to create the new
connection. The syntax is:

```abnf
create = %s"CREATE " recipientKey
recipientKey = encoded
```

If the connection is created successfully, the server must send `conn` response
with the recipient's and sender's connection IDs:

```abnf
conn = %s"CONN " recipientId " " senderId
recipientId = encoded
senderId = encoded
```

Once the connection is created, the recipient gets automatically subscribed to
receive the messages from that connection, until the transport connection is
closed. The `subscribe` command is needed only to start receiving the messages
from the existing connection when the new transport connection is opened.

`signature` part of `transmission` should an empty string; SMP servers can also
use it to authenticate users who are allowed to create simplex connections.

#### Subscribe to connection

When the simplex connection was not created in the current transport connection,
the recipient must use this command to start receiving messages from it:

```abnf
subscribe = %s"SUB"
```

If subscription is successful (`ok` response) the recipient will be receiving
the messages from this connection until the transport connection is closed -
there is no command to unsubscribe.

#### Secure connection command

This command is sent by the recipient to the server to add sender's key to the
connection:

```
secure = %s"SECURE " senderKey
senderKey = encoded
```

`senderKey` is received from the sender as part of the first message - see
[Send Message Command](#send-message-command).

Once the connection is secured only signed messages can be sent to it.

#### Delete message command

The recipient should send this command once the message was stored in the
client, to notify the server that the message should be deleted:

```abnf
deleteMsg = %s"DELMSG " msgId
msgId = encoded
```

Even if this command is not sent by the recipient, the servers should limit the
time of message storage, whether it was delivered to the recipient or not.

#### Suspend connection

The recipient can suspend connection prior to deleting it to make sure no
messages are lost:

```abnf
suspendCmd = %s"SUSPEND"
```

The server must respond with `"ERROR AUTH"` to any messages sent after the
connection was suspended (see [Error responses](#error-responses)).

The server must respond `ok` to this command only after all messages related to
the connection were delivered.

This command can be sent multiple times (in case transport connection was
interrupted and the response was not delivered), the server should still respond
`ok` even if the connection is already suspended.

There is no command to unsuspend the connection. Servers must delete suspended
connections that were not deleted after some period of time.

#### Delete connection

The recipient can delete the connection, whether it was suspended or not.

All undelivered messages will not be delivered - they will be deleted as soon as
command is received, before the response is sent.

```abnf
deleteCmd = %s"DELETE"
```

### Sender commands

Currently SMP defines only one command that can be used by sender - `send`
message. This command must be used with sender's ID, if recipient's ID is used
the server must respond with `"ERROR AUTH"` response (see
[Error responses](#error-responses)).

#### Send message command

This command is sent to the server by the sender both to confirm the connection
after the sender received out-of-band message from the recipient and to send
messages after the connection is secured:

```abnf
send = %s"SEND " msgBody
msgBody = stringMsg | binaryMsg
stringMsg = ":" string ; until CRLF in the transmission
string = *(%x01-09 / %x0B-0C / %x0E-FF %) ; any characters other than NUL, CR and LF
binaryMsg = size CRLF msgBody CRLF ; the last CRLF is in addition to CRLF
                        ; in the transmission
size = 1*DIGIT ; size in bytes
msgBody = *OCTET ; any content of specified size - safe for binary
```

`stringMsg` is allowed primarily to test SMP servers, e.g. via telnet.

The signature with this command must be empty, otherwise it must result in
`ERROR SYNTAX` response.

The first message is sent to confirm the connection - it should contain sender's
server key (see decrypted message syntax below) - this message must be sent
without signature.

Once connection is secured (see
[Secure connection command](#secure-connection-command)), messages must be sent
with signature.

The server must respond with `"ERROR AUTH"` response in the following cases:

- connection does not exist or suspended,
- connection is secured but the transmission does NOT have a signature,
- connection is NOT secured but the transmission has a signature.

Until the connection is secured, the server should accept any number of unsigned
messages - it both enables the legimate sender to resend the confirmation in
case of failure and also allows the simplex messaging client to ignore any
confirmation messages that may be sent by the attackers (assuming they could
have intercepted the connection ID in the server response, but do not have a
correct encryption key passed to sender in out-of-band message).

The body should be encrypted with the recipient's "public" key (`EK`); once
decrypted it must have this format:

```abnf
decryptedBody = reserved CRLF clientBody CRLF
reserved = senderKeyMsg / *VCHAR
senderKeyMsg = "KEY " senderKey
senderKey = encoded
clientBody = *OCTET
```

`reserved` for the initial unsigned message is used to transmit sender's server
key and can be used in future revision of the protocol for other purposes.

### Server messages

#### New connection response

Server must respond with this message when the new connection is created.

See its syntax in [Create connection command](#create-connection-command)

#### Deliver connection message

The server must deliver messages to all subscribed simplex connections on the
currently open transport connection. The syntax for the message delivery is:

```abnf
message = %s"MSG " msgId " " timestamp " " msgBody
msgId = encoded
timestamp = date-time; RFC3339
```

`msgId` - a unique within the server message ID based on 128-bit
cryptographically random integer.

`timestamp` - the UTC time when the server received the message from the sender,
must be in date-time format defined by [RFC 3339][10]

`msgBody` - string or binary message, see syntax in
[Send message command](#send-message-command)

#### Error responses

The server can respond with an error response in the following cases:

- unknown command name (`"CMD"`),
- incorrect command or transmission syntax (`"SYNTAX"`) - wrong format, number
  of parameters or incorrect format of parameters ,
- authentication error (`"AUTH"`) - incorrect signature, unknown or suspended
  connection, sender's ID is used in place of recipient's and vice versa, and
  some other cases (see [Send message command](#send-message-command))
- internal server error (`"INTERNAL"`).

The syntax for error responses:

```abnf
error = %s"ERROR " errorType
errorType = %s"CMD" / %s"SYNTAX" / %s"AUTH" / %s"INTERNAL"
```

Server implementations must aim to respond within the same time for each command
in all cases when `"ERROR AUTH"` response is required to prevent timing attacks
(e.g., the server should execute signature verification even when the connection
does not exist on the server).

### OK response

When the command is successfully executed by the server, it should respond with
OK response:

```abnf
ok = %s"OK"
```

## Appendices

### Appendix A.

Secure encrypted transport connection with the SMP server.

Both the recipient and the sender can use TCP or some other (possibly higher
level) transport protocol to communicate with the server.

Some protocol should be used to ecrypt the connection traffic - one simple
option that does not require any cetralized certificate authority is below.

When the connection is established, the server sends the binary encryption key
that the client should match with key or fingerprint available to them - if they
do not match, they should terminate the connection.

The client should respond with the symmetric key that will be used by both the
client and the server to encrypt all traffic in the connection - this key should
be encrypted with the public key initially sent by the server.

After the symmetric key is sent to the server, all communication should happen
in encrypted binary chunks having a fixed size of 4096 bytes irrespective of the
size of the command/message that should be sent. Smaller messages should be
padded, multiple commands/messages can be packed into a single chunk. If the
application using SMP needs to transmit a file or a larger message, it should be
broken down into fragments. The format of application level messages within SMP
commands is out of scope of this protocol.

### Appendix B.

Sending out-of-band message.

SMP does not prescribe the channel to pass out-of-band message - it should be
agreed by the client applications.

For practical purposes various solutions can be used, e.g. one of the versions
or the analogues of [QR code][3] (or their sequence) that is read via the
camera, either directly from the participant's device or via the video call.
Although a video call still allows for a highly sophisticated MITM attack, it
would require that in addition to compromising simplex connection to intercept
messages, the attacker also identifies and compromises the video connection in
another channel and substitutes the video in real time.

[1]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[2]: https://en.wikipedia.org/wiki/End-to-end_encryption
[3]: https://en.wikipedia.org/wiki/QR_code
[4]: https://en.wikipedia.org/wiki/Unidirectional_network
[5]: https://en.wikipedia.org/wiki/Forward_secrecy
[6]: https://en.wikipedia.org/wiki/Off-the-Record_Messaging
[8]: https://tools.ietf.org/html/rfc5234
[9]: https://tools.ietf.org/html/rfc4648#section-4
[10]: https://tools.ietf.org/html/rfc3339
