# Glossary

Choosing a private messenger requires the understanding of many technical terms, that many users, even quite technical, often misunderstand. This list is aiming to fill this knowledge gap. Please suggest any changes or additions.

While this glossary aims to be is objective and factual, it is not completely unbiased. We designed SimpleX in a way that we think is the most private, secure and resilient design for a communication network, and some definitions here reflect this view.

## Address portability

## Anonymous credentials

[Digital credential on Wikipedia](https://en.wikipedia.org/wiki/Digital_credential)

## Blockchain

[Wikipedia](https://en.wikipedia.org/wiki/Blockchain)

## Break-in recovery

[Post-compromise security](#post-compromise-security).

## Centralized network

Centralized networks are provided or controlled by a single entity. The examples are Threema, Signal, WhatsApp and Telegram. The advantage of that is that the provider can innovate faster, and to have a centralized approach to security. But the disadvantage is that this same provider can change or discontinue the service, sell or disclose all users data about who they are connected with.

## Content padding

[Message padding](#message-padding).

## Decentralized network

## Defense in depth

[Wikipedia](https://en.wikipedia.org/wiki/Defence_in_depth)

## Double ratchet algorithm

It is used by two parties to exchange [end-to-end encrypted](#end-to-end-encryption) messages. The parties will use some [key agreement protocol](#key-agreement-protocol) to agree on the initial shared secret key.

Double Ratchet algorithm provides [perfect forward secrecy](#perfect-forward-secrecy) and [post-compromise security](#post-compromise-security). It is [designed by Singal](https://signal.org/docs/specifications/doubleratchet), and used in SimpleX Chat and many other secure messengers. Most experts consider the state-of-the-art encryption protocol in message encryption.

## End-to-end encryption

Communication system where only the communicating parties can read the messages. It is designed to protect message content from any potential eavesdroppers – telecom and Internet providers, malicious actors, and also the provider of the communication service.

End-to-end encryption requires agreeing cryptographic keys between the sender and the recipient in a way that no eavesdroppers can access the agreed keys. See [key agreement protocol](#key-agreement-protocol).

[Wikipedia](https://en.wikipedia.org/wiki/End-to-end_encryption)

## Federated network

Federated network is provided by several entities that agree upon the standards and operate the network collectively. This allows the users to choose their provider, that will hold their account, their messaging history and contacts, and communicate with other providers' servers on behalf of the user. The examples are email, XMPP, Matrix and Mastodon.

The advantage of that design is that there is no single organization that all users depend on, and the standards are more difficult to change, unless it benefits all users. There are several disadvantages: 1) the innovation is slower, 2) each user account still depends on a single organization, and in most cases can't move to another provider without changing their network address – there is no [address portability](#address-portability), 3) the security and privacy are inevitably worse than with the centralized networks.

[Federation on Wikipedia](https://en.wikipedia.org/wiki/Federation_(information_technology))

## Forward secrecy

It is a feature of a [key agreement protocol](#key-agreement-protocol) that ensures that session keys will not be compromised even if long-term secrets used in the session key exchange are compromised. Forward secrecy protects past sessions against future compromises of session or long-term keys.

[Wikipedia](https://en.wikipedia.org/wiki/Forward_secrecy)

## Key agreement protocol

Key exchange is a process of agreeing cryptographic keys between the sender and the recipient(s) of the message. It is required for [end-to-end encryption](#end-to-end-encryption) to work.

[Wikipedia](https://en.wikipedia.org/wiki/Key-agreement_protocol)

## Key exchange

[Key agreement protocol](#key-agreement-protocol).

## Man-in-the-middle attack

[Wikipedia](https://en.wikipedia.org/wiki/Man-in-the-middle_attack).

## Merkle tree

[Wikipedia](https://en.wikipedia.org/wiki/Merkle_tree).

## Message padding

Padding is the process of adding data to the beginning or the end of a message prior to encryption. Padding conceals the actual message size from any eavesdroppers. SimpleX has several encryption layers, and prior to each encryption the content is padded to a fixed size.

[Wikipedia](https://en.wikipedia.org/wiki/Padding_(cryptography)).

## Onion routing

[Wikipedia](https://en.wikipedia.org/wiki/Onion_routing)

## Overlay network

[Wikipedia](https://en.wikipedia.org/wiki/Overlay_network)

## Pairwise pseudonymous identifier

Generalizing [the definition](https://csrc.nist.gov/glossary/term/pairwise_pseudonymous_identifier) from NIST Digital Identity Guidelines, it is an opaque unguessable identifier generated by a service used to access a resource by only one party.

In the context of SimpleX network, these are the identifiers generated by SMP relays to access anonymous messaging queues, with a separate identifier (and access credential) for each accessing party: recipient, sender and and optional notifications subscriber. The same approach is used by XFTP relays to access file chunks, with separate identifiers (and access credentials) for sender and each recipient.

## Peer-to-peer

Peer-to-peer (P2P) is the network architecture when participants have equal rights and communicate directly via a general purpose transport or overlay network. Unlike client-server architecture, all peers in a P2P network both provide and consume the resources. In the context of messaging, P2P architecture usually means that the messages are sent between peers, without user accounts or messages being stored on any servers. Examples are Tox, Briar, Cwtch and many others.

The advantage is that the participants do not depend on any servers. There are [multiple downsides](./SIMPLEX.md#comparison-with-p2p9-messaging-protocols) to that architecture, such as no asynchronous message delivery, the need for network-wide peer addresses, possibility of network-wide attacks, that are usually mitigated only by using a centralized authority. These disadvantages are avoided with [proxied P2P](#proxied-peer-to-peer) architecture.

[Wikipedia](https://en.wikipedia.org/wiki/Peer-to-peer).

## Perfect forward secrecy

[Forward secrecy](#forward-secrecy).

## Post-compromise security

The quality of the end-to-end encryption scheme allowing to recover security against a passive attacker who observes encrypted messages after compromising one (or both) of the parties. Also known as recovery from compromise or break-in recovery.

## Post-quantum cryptography

[Wikipedia](https://en.wikipedia.org/wiki/Post-quantum_cryptography)

## Privacy

[Wikipedia](https://en.wikipedia.org/wiki/Privacy)

## Proxied peer-to-peer

## Recovery from compromise

[Post-compromise security](#post-compromise-security).

## Security triangle

## User identity
