# Graph-chat protocol

A generic chat protocol for client applications that communicate via edge-messaging protocol


## Problems of the existing chat platforms and protocols

- Dependency on a single company/server to access and use chat. That creates implications for chat privacy, user profile resilience and data ownership.
- Visiblity of user profile information to chat system and other chat users. Chat users have limited control as to who can see and find their profile.
- Visibility of user contacts graph to chat server.
- [E2EE][1] can be compromised by [MITM][2] attack (see [edge-messaging protocol][3])
- identity related problems (also see [edge-messaging protocol][3]):
  - visibility of user contacts to anybody observing messages
  - unsolicited messages (spam and abuse)
  - trademark issues (when usernames are used)
  - privacy issues (when phone numbers are used)


## Graph-chat protocol abstract

TODO


## Creating duplex connection

Majority of chat scenarios requires duplex (bi-directional) connections between participants. Graph-chat protocol uses multiple unidirectional connections (graph edges) created on multiple edge-messaging servers to implement duplex connections.

Each duplex connection consists of one or multiple, for redundancy, pairs of unidirectional connections to connect chat participants or devices of the same participant - it is used for "contacts", "devices", "group participants", etc. For practical purposes of redundancy, chat clients can use 2-4 pairs of unidirectional connections.

The process described below establishes a duplex connection between Alice and Bob that has `n` unidirectional connections `Cai` (where `1 <= i <= n`) from Bob to Alice (created by Alice on her servers) and `n` unidirectional connections `Cbi` (where `1 <= i <= n`) from Alice to Bob (created by Bob on his servers).

The following symbols are used below:
- unidirectional edge-messaging connections:
  - `Cai` - Alice's edge-messaging connection number `i` (out of `n`) allowing Bob to send messages to Alice.
  - `Cbi` - Bob's edge-messaging connection number `i` (out of `n`) allowing Alice to send messages to Bob.
- keys created for Alice's connections:
  - `EKai` - Alice's assymetric key pair used:
    - by Bob to encrypt and by Alice to decrypt messages from Bob sent via `Cai`.
    - by Alice to sign and Bob to verify messages from Alice sent via `Cbi`.
  - `IDai` - client-generated ID of connection `Cai`
  - `RUai` - server-generated recipient connection URI of `Cai` (to be used by Alice to retrieve messages).
  - `SUai` - server-generated sender connection URI of `Cai` (to be used by Bob to send messages).
  - `RKai` - Alice's recipient key of `Cai`.
  - `SKai` - Bob's sender key of `Cai`.
- keys created for Bob's connections
  - `EKbi` - Bob's assymetric key pair used for:
    - Alice to encrypt and Bob to decrypt messages from Alice sent via `Cbi`.
    - Bob to sign and Alice to verify messages from Bob sent via `Cai`.
  - `IDbi` - client-generated ID of connection `Cbi`
  - `RUbi` - server-generated recipient connection URI of `Cbi` (to be used by Bob to retrive messages).
  - `SUbi` - server-generated sender connection URI of `Cbi` (to be used by Alice to send messages).
  - `RKbi` - Bob's recipient key of `Cbi`.
  - `SKbi` - Alice's sender key of `Cbi`.

To create a duplex connection initiated by Alice, Alice's and Bob's apps follow these steps.

1. Alice's app prepares unidirectional connections:
   1. it creates `n` unidirectional connections `Cai` with client-generated IDs `IDai` on different Alice's servers (see [edge-messaging protocol][3]) with different keys `RKai` (Alice's recipient keys) - edge-messaging server returns recipient and sender connection URIs `RUai` and `SUai`.
   2. optionally, Alice's app subsribes to receive messages from these new connections `Cai` ([edge-messaging server protocol][4] defines protocol to be used for subscriptions).
2. Alice's app sends a secure message to Bob's app:
   1. it prepares the message with the information needed to establish all connections `Cai`, including all encryption keys `EKai` and sender connection URIs (`SUai`).
   2. depending on the communication scenario, Alice's app sends this message to Bob's app in one of available secure ways (either out-of-band or via available secure duplex connections(s) - see [Duplex connection security level](#TODO)), depending on communication scenario:
     - if Bob is a new contact, the information is presented as a visual code (e.g. QR code(s)) to Bob's app - as out-of-band message needed to establish connections `Cai` (see [edge-messaging protocol][3] and [Adding direct contact](#adding-direct-contact)).
     - if Bob is added to group chat with Alice by some contact of Alice or Bob, this information can be passed through all possible chains of contacts between Alice and Bob in the group (to minimise the risk of MITM attack) (see [Group chat](#TODO)).
     - if Alice adds Bob as a contact via Bob's trusted contact John, who also has Alice as a trusted contact, this information will be passed via John (who has secure duplex connections with both Alice and Bob), in the same way as with Group chat (see [Trusted contacts](#TODO)).
     - if Alice already has Bob as a contact, and she wants to create a separate off-the-record chat with him, this information will be passed via their existing connection (see [Off-the-record chat](#TODO)).
     - etc. In all cases, the protocol defines the most secure possible way to pass the out-of-band (from the point of view of the new connections) message required by [edge-messaging protocol][3] to create unidirectional connections.
3. Bob's app accepts all Alice's connections:
   1. it receives the message from Alice's app, in a way defined by a specific chat scenario.
   2. it interprets the received information as edge-messaging protocol out-of-band messages to accept all unidirectional connections `Cai` (see [edge-messaging protocol][3]).
   3. it creates `n` new unidirectional connections `Cbi` on Bob's server with client-generated IDs `IDbi` on different Bob's servers with different keys `RKbi` (Bob's recipient keys) - edge-messaging server returns recipient and sender connection URIs `RUbi` and `SUbi`.
   4. optionally, Bob's app subsribes to receive messages from these new connection `Cbi` (see [edge-messaging server protocol][4]).
   5. it prepares required `n` responses, each with sender key `SKai` for connection `Cai`.
   6. as optional information to identify Bob, depending on communication scenario, his app includes Bob's user profile (that is only stored in graph-chat client and not visible to any server).
   7. as additional optional information, his app includes in each response the information required to establish a connection `Cbi` with Alice so that she can send messages to Bob - this includes public key `EKbi` and sender connection URIs `SUbi` in each corresponding message sent to connection `Cai`.
   8. his app now sends the unsigned requiests to Alice's connections `Cai` to URIs `SUai` (see [edge-messaging protocol][3]), to both confirm the connections `Cai` and to propose the new connections `Cbi`. As each message is encrypted by the key `EKai` of the connection `Cai` that only Alice can decrypt, it is safe to send it - from edge-messaging server point of view it is an out-of-band message.
4. Alice's app accepts Bob's app connections:
   1. it receives the messages from Bob.
   2. depending on chat scenario, Bob is identified and confirmed:
     - for new contact, Alice may visually identify Bob's user profile and accepts Bob as a contact.
     - for group participant, Alice's app will match known Bob's user profile ID with received user profile ID.
   3. it secures the connections `Cai` with keys `SKai` received from Bob - the connections are now established (see [edge-messaging protocol][3]).
   4. it accepts the connections `Cbi`, including in the response to Bob's server Alice's user profile and (as the additional information) the confirmation that the connections `Cai` are secured and can be used (see [edge-messaging protocol][3]).
   5. it sends the unsigned messages via connections `Cbi`.
   6. it adds Bob's duplex connection to the list of available duplex connections as "pending" (Alice cannot yet send messages to Bob, but Bob already can send messages to Alice). Possibly, the status of duplex connection can indicate that Bob already can send messages to Alice.
5. Bob's app adds duplex connection with Alice:
   1. it receives the initial messages via connections `Cbi`.
   2. it adds duplex connection with Alice in the list of available duplex connections as "pending" (to indicate that Alice cannot yet send messages). Possibly, the status of duplex connection can indicate that Bob already can send messages to Alice.
   3. it secures the connections `Cbi` - they are now established as well (see [edge-messaging protocol][3]).
   4. it sends a special message (message type "welcome") to Alice's app via duplex connection (i.e., via all connections `Cai`) to confirm that adding Alice's contact is completed (see [Sending messages via duplex connection](#TODO)).
6. Alice's app finalises adding duplex connection with Bob:
   1. it receives "welcome" message from Bob's app via duplex connection.
   2. it changes Bob's duplex connection status to "established".
   3. it send a special "welcome" message to Bob's app via duplex connection.
7. Bob's app finalises adding duplex connection with Alice:
   1. it receives "welcome" message from Alice's app via duplex connection.
   2. it changes Alice's duplex connection status "established".


## Adding direct contact

"Direct contact" is a term used for a duplex connection with another chat user established directly, not via another user. Establishing contact requires sending an out-of-band message - "visual code" (e.g. QR code(s)) is used for this purpose. 

For example, Alice and Bob want to have a conversation in chat app, exchanging messaged with each other. They both have graph-chat client and access to several edge-messaging servers (see [edge-messaging protocol][3]) that they can use to receive messages, and their graph-chat clients are configured to use these servers.

To chat in the app Alice needs to add Bob as "direct contact" to her contacts in the app.

1. Alice initiates adding "direct contact" in her graph-chat client app.
2. Alice shares "visual code" (e.g. QR code(s)) with Bob:
   2. her app prepares secure message to establish duplex connection with Bob's app (step 1 in [Creating duplex connection](#creating-duplex-connection)).
   3. her app prepares and displays a visual code with this message (step 2 in [Creating duplex connection](#creating-duplex-connection)).
   4. Alice now can share this visual code with Bob, either in person or via a video call (see [edge-messaging protocol][3]).
3. Bob reads "visual code" that Alice prepared via his app:
   1. he initiates adding "direct contact" via visual code in his graph-chat client app.
   2. his app interprets this visual code as secure message required to accept duplex connection with Alice and proceeds with creating duplex connection (step 3 in [Creating duplex connection](#creating-duplex-connection)).
4. Alice's app adds Bob as "direct contact":
   1. it proceeds with creating duplex connection.
   2. once "pending" duplex connection with Bob's app is created in Alice's app, Bob is added as "direct contact" with the status "pending" in Alice's app (step 4 in [Creating duplex connection](#creating-duplex-connection)).
5. Bob's app adds Alice as "direct contact":
   1. it proceeds with creating duplex connection.
   2. once "pending" duplex connection with Alice's app is created in Bob's app, Alice is added as "direct contact" with the status "pending" in Bob's app (step 5 in [Creating duplex connection](#creating-duplex-connection)).
6. Alice's app finalises adding Bob as "direct contact":
   1. it finalises adding duplex connection with Bob's app (step 6 in [Creating duplex connection](#creating-duplex-connection)).
   2. it changes Bob's "direct contact" status to "established".
7. Bob's app finalises adding Alice as "direct contact":
   1. it finalises adding duplex connection with Alice's app (step 7 in [Creating duplex connection](#creating-duplex-connection)).
   2. it changes Alice's "direct contact" status to "established".


## Connection and message types used in graph-chat protocol

##### Duplex connections

TODO

- "contact" - can be of type "person", "bot", "device", "organisation".
- "broadcast" - duplex connection, that by convention only allows to receive messages and send back control messages, but not the content messages. Profile of the recipient is not shared.


##### Messages

Control messages:

- "welcome" - sent and recieved by both participants when duplex connection is being established.
- "receipt" - acknowledging message receipt.
- "contact: update" - changing "contact" profile .
- "profile" - changing "contact 
- "gm: add" - sent to add unidirectional edge-messaging connection to graph-chat duplex connection.
- "gm: remove" - sent to remove unidirectional edge-messaging connection from graph-chat duplex connection.

Content messages:

- "text"
- "image"


[1]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[2]: https://en.wikipedia.org/wiki/End-to-end_encryption
[3]: edge-messaging.md
[4]: #TODO
