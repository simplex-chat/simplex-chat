# Federated chat system with E2EE and low risk of MITM attack

## Problems

The problems of existing chat solutions this system intends to solve:

- Dependency on a single company/server to access and use chat. That creates implications for chat privacy (see E2EE), resilience and data ownership.
- Existing [E2EE](https://en.wikipedia.org/wiki/End-to-end_encryption) implementations in chat platforms are easily compromised via the platform itself with [MITM attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) - as the key exchange happens via the same channel, the key can be substituted.
- Dependency on DNS (e.g. with XMPP protocol). DNS is needed for shared information access, connecting chat users to particular domains undermines users privacy and gives control over chat users communications to domain owners.
- Dependency on phone numbers. Most phone numbers either expose or allow to access private information about the user, including personal identity.
- User names. They must be unique, but they can violate existing trademarks and be reclaimed by trademark owners, lead to name squatting and can leak private information about the users.
- Visiblity of user profile information to the whole network.


## Solution

- No dependence on phone numbers, no user names and no DNS used to connect to users (chat servers may choose to be accessible via DNS, but it is not required)
- Using simple protocol, so system users can create independent server and client implementations
- Available open-source server implementations that can be easily deployed by any user with minimal expertise (e.g. on Heroku via web UI).
- Available open-source mobile client implementations so that system users can independently assess system security model.
- Only client applications store user profiles, connections to other user profiles and messages; servers do have access to any of this information (and are not supposed to store even encrypted messages).
- Multiple client applications and devices can be used by each user profile to communicate and share connections and message history.
- Key exchange and establishing connections between user profiles is done by sharing QR code via any independent communication channel (or directly via screen and camera), not provided by the system - to have much lower risk of key substitution in MITM attack. QR code contains the connection-specific public key.
- Unique public key is used for each user profile connection in order to:
  - reduce the risk of attacker posing as user's connection
  - avoid exposing all user connections to the servers
- Public keys used between connections are regularly updated to prevent decryption of the full message history in case when some servers or middle-men preserve message history and the current key is compromised.
- Users can repeat key exchange using QR code and alternative channel at any point to increase communication security.
- No single server in the system has visibility of all connections or messages of any user, as user profiles are identified by multiple rotating public keys, using separate key for each profile connection.
- Servers only store hashes of public keys, so that nobody can encrypt messages from a user profile connection, other than the user who has this public key (effectively making it a signature of the connected user). (Question: maybe connection key pair used to encrypt should be different from the key pair used to sign?)
- User profile (meta-data of the user including non-unique name / handle and optional additional data, e.g. avatar and status) is stored in the client apps and is shared only with accepted user profile connections.


## System components

- chat server
- chat client application


## System functionality

### Chat server

Chat servers can be either available to all users or only to users who have a valid server key.

Chat servers have to provide the following:

- store information about the chat server(s) where the message addressing a user profile with a specific hash of the public key can be sent messages to.
- receive messages from connected user profiles (identified by hashes of their public keys).
- forward messages to server(s) of recepients (identified by hashes of their public keys).
- send messages to client apps of connected recepients (identified by hashes of their public keys).


### Chat client application

Chat applications are installed by users on their devices

Client apps should provide the following:

- create and manage user profiles - user can have multiple profiles.
- allow switching between user profiles.
- share access to all or selected user profiles with other devices (optionally including existing connections and message histories).
- for each user profile:
  - generate and show QR code connection-specific user profile public key that can be shown on the screen and/or sent via alternative channel.
  - track whether profile public key was shared (assuming lower connection security if it wasn't read directly from the screen).
  - read QR code (via the camera) to establish connection with another user.
  - receive and accept connection requests (it would contain, in encrypted form (using recepient public key that was previously shared), requesting user public key to use to encrypt messages.
  - exchange user profiles data once connection is accepted.
  - send messages to connected user profiles (message would contain hashed sender-specific public key of the recepient and encrypted message) via the server(s) the sender is connected to.
  - receive messages from connected user profile (message will be decrypted in the client using private key associated with the sender).
  - optionally define the server(s) they trust and will connect to, including the servers that require server key to access.
  - optionally require that connected users connect to one of the servers they trust when sending messages to the user profile.
  - store history of all conversations encrypted using user client app password (or some other device specific encryption mechanism).


## Sequence diagrams

### Sending message

![Sending message](/diagrams/message.svg)