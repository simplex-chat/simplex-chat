# Graph-chat protocol

A generic chat protocol for client applications that communicate via graph-messaging protocol


## Problems of the existing chat platforms and protocols

- Dependency on a single company/server to access and use chat. That creates implications for chat privacy, user profile resilience and data ownership.
- Visiblity of user profile information to chat system and other chat users. Chat users have limited control as to what can see and find their profile.
- Visibility of user contacts graph to chat server.
- [E2EE][1] can be compromised by [MITM][2] attack (see [graph-messaging protocol][3])
- identity related problems (also see [graph-messaging protocol][3]):
  - visibility of user contacts to anybody observing messages
  - unsolicited messages (spam and abuse)
  - trademark issues (when usernames are used)
  - privacy issues (when phone numbers are used)


## Graph-chat protocol abstract


## How Alice and Bob use graph-chat protocol

Alice and Bob want to have a conversation in chat app, exchanging messaged with each other. They both have graph-chat client and access to several [graph-messaging servers][3] that they can use to receive messages, and their graph-chat clients are configured to use these servers.

The following symbols are used below:

- `AC` - graph-messaging connection allowing Bob to send messages to Alice.
- `BC` - graph-messaging connection allowing Alice to send messages to Bob.

The sequence below shows adding contact when both Alice and Bob use only one server each, but the protocol assumes that several connections on different servers are used by both of them to achieve redundancy (with different keys), and client apps have to remove message duplicates received via different connections - for simplicity it is not included in this flow.

To chat in the app Alice needs to add Bob to her contacts in the app.

1. Alice shares "visual code" (e.g. QR code(s)) to add Bob as contact in the graph-chat app:
   1. she initiates adding the contact in her graph-chat client app.
   2. her app creates a unidirectional connections `AC` on Alice's server (see [graph-messaging protocol][3]).
   3. servers respond with unique connection ID (`CID`) and URI.
   4. optionally, her app subsribes to be notified about messages from this new connection `AC` (see graph-messaging server protocol).
   5. her app prepares and displays a visual code with an out-of-band message needed to establish connection `AC` (see [graph-messaging protocol][3]).
   6. Alice now can share this visual code with Bob, either in person or via a video call (see [graph-messaging protocol][3]).
2. Bob reads "visual code" that Alice prepared via his app:
   1. he initiates adding contact via visual code in his graph-chat client app.
   2. his app interprets this visual code as an out-of-band message to accept a unidirectional connections `AC` (see [graph-messaging protocol][3]).
   3. his app creates a new unidirectional connections `BC` on Bob's server (see [graph-messaging protocol][3]).
   4. his app prepares required response with a key `SK` for connection `AC`.
   5. as optional information to identify Bob, his app includes Bob's user profile (that is only stored in graph-chat client and not visible to any server).
   6. as additional optional information, his app includes the information required to establish connection `BC` with Alice so that she can send messages to Bob.
   7. his app now send the unsigned requiest to Alice's connection `AC` (see [graph-messaging protocol][3]), to both confirm the connection `AC` and propose a new connection `BC`. As the message is encrypted by the key `EK` of the connection `AC` that only Alice can decrypt, it is safe to send it - from graph-messaging server point of view it is an out-of-band message.
3. Alice accepts Bob as contact:
   1. her app receives the message from Bob
   2. Alice visually identifies Bob's user profile and accepts Bob as a contact
   3. her app secures the connection `AC` (see [graph-messaging protocol][3])
   4. her app accepts the connection `BC`, including in the response message to Bob's server Alice's user profile and the confirmation that the connection `AC` is secured and can be used (see [graph-messaging protocol][3]).
   5. her app adds Bob to the list of contacts as "pending" (Alice cannot yet send messages to Bob).
   



[1]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[2]: https://en.wikipedia.org/wiki/End-to-end_encryption
[3]: graph-messaging.md