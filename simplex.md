# Federated chat system with [E2EE][1] and low risk of [MITM attack][2]

## Problems

Existing chat platforms and protocols have some or all of the following problems:

- Lack of privacy of the user profile and connections (meta-data privacy).
- No protection (or only optional protection) of [E2EE][1] implementations from MITM attacks.
- Unsolicited messages (spam and abuse).
- Lack of data ownership and protection.
- Complexity of usage for all non-centralized protocols to non-technical users.

The concentration of the communication in a small number of centralized platforms makes resolving these problems quite difficult.

## Proposed solution

Proposed stack of protocols solves these and other problems by making both messages and contacts accessible only on client devices, reducing the role of the servers to simple message brokers that only require authorization of messages sent to the queues, but do NOT require user authentication - not only the messages but also the metadata is protected.

See [SMP protocol][6] and [SMP agent protocol][8].

## Comparison with other protocols

|          | SimpleX chat | Signal, big platforms | XMPP, Matrix | P2P protocols |
|:-------- |:------------:|:---------------------:|:------------:|:-------------:|
| Requires global identity | No = private | Yes<sup>1</sup> | Yes<sup>2</sup> | Yes<sup>3</sup> |
| Possibility of MITM      | No = secure  | Yes<sup>4</sup> | Yes             | Yes             |
| Dependence on DNS        | No = resilient | Yes | Yes | No |
| Federation               | Yes | No | Yes | No<sup>5</sup> |
| Central component or other network-wide attack | No = resilient | Yes | Yes<sup>2</sup> | Yes<sup>6</sup> |

1. Usually based on a phone number, in some cases on usernames.
2. DNS based.
3. Public key or some other globally unique ID.
4. If operator’s servers are compromised.
5. While P2P networks are distributed, they are not federated - they operate as a single network.
6. P2P networks either have a central authority or the whole network can be compromised - see the next section.

## Comparison with [P2P][9] messaging protocols

There are several P2P chat/messaging protocols and implementations that aim to solve privacy and centralisation problem, but they have their own set of problems that makes them less reliable than the proposed chat system design, more complex to implement and analyse and more vulnerable to attacks.

1. [P2P][9] networks either have some centralized component, which makes them highly vulnerable, or, more commonly, use some variant of [DHT][10] to route messages/requests through the network. DHT implementations have complex designs that have to balance reliability, delivery guarantee and latency, and also have some other problems. The proposed chat system design has both higher delivery guarantee and low latency (the message is passed multiple times in parallel, through one node each time, using servers chosen by the recipient, while in P2P networks the message is passed through `O(log N)` nodes sequentially, using nodes chosen by the algorithm).

2. The proposed design, unlike most P2P networks, has no global identity of any form, even temporary.

3. P2P itself does not solve [MITM attack][2] problem, but most existing solutions do not use out-of-band messages for the initial key exchange. The proposed design uses out-of-band messages or, in some cases, pre-existing secure and trusted connections for the initial key exchange.

4. P2P implementations can be blocked by some Internet providers (like [BitTorrent][11]). The proposed design is transport agnostic - it can work over standard web protocols, and the servers can be deployed on the same domains as the websites.

5. All known P2P networks are likely to be vulnerable to [Sybil attack][12], because each node is discoverable, and the network operates as a whole. Known measures to reduce the probability of the Sybil attack either require a vulnerable centralized component or expensive [proof of work][13]. The proposed design, on the opposite, has no server discoverability - servers are not connected, not known to each other and to all clients. The chat network is fragmented and operates as multiple isolated connections. It makes Sybil attack on the whole simplex messaging network impossible - even if some servers are compromised, other parts of the network can operate normally, and affected clients can always switch to using other servers without losing contacts or messages.

6. P2P networks are likely to be vulnerable to [DRDoS attack][14]. In the proposed design clients only relay traffic from known trusted connection and cannot be used to reflect and amplify the traffic in the whole network.

## Network features

- No user identity known to system servers - no phone numbers, user names and no DNS are needed to authorize users to the network.
- Each user can be connected to multiple servers to ensure message delivery, even if some of the servers are compromised.
- No single server in the system has visibility of all connections or messages of any user, as user profiles are identified by multiple rotating public keys, using separate key for each profile connection.
- Uses standard asymmetric cryptographic protocols, so that system users can create independent server and client implementations complying with the protocols.
- Open-source server implementations that can be easily deployed by any user with minimal technical expertise (e.g. on Heroku via web UI).
- Open-source client implementations so that system users can independently assess system security model.
- Only client applications store user profiles, contacts of other user profiles, messages; servers do NOT have access to any of this information and (unless compromised) do NOT store encrypted messages or any logs.
- Multiple client applications and devices can be used by each user profile to communicate and to share connections and message history - the devices are not known to the servers.
- Initial key exchange and establishing connections between user profiles is done by sharing the invitation (e.g. QR code via any independent communication channel (or directly via screen and camera), system servers are NOT used for key exchange - to reduce risk of key substitution in [MITM attack][2]. QR code contains the connection-specific public key and other information needed to establish the connection.
- Connections between users can be established via shared trusted connections to simplify key exchange.
- Servers do NOT communicate with each other, they only communicate with client applications.
- Unique public key is used for each user profile connection in order to:
  - reduce the risk of attacker posing as user's connection;
  - avoid exposing all user connections to the servers.
- Unique public key is used to identify each connection participant to each server.
- Public keys used between connections are regularly rotated to prevent decryption of the full message history ([forward secrecy][4]) in case when some servers or middlemen preserve message history and the current key is compromised.
- Users can repeat key exchange using QR code and alternative channel at any point to increase communication security and trust.

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[4]: https://en.wikipedia.org/wiki/Forward_secrecy
[6]: https://github.com/simplex-chat/simplexmq/blob/master/protocol/simplex-messaging.md
[8]: https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md
[9]: https://en.wikipedia.org/wiki/Peer-to-peer
[10]: https://en.wikipedia.org/wiki/Distributed_hash_table
[11]: https://en.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://en.wikipedia.org/wiki/Proof_of_work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
