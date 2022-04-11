# SimpleX platform - motivation and comparison

## Problems

Existing chat platforms and protocols have some or all of the following problems:

- Lack of privacy of the user profile and contacts (meta-data privacy).
- No protection (or only optional protection) of [E2EE][1] implementations from MITM attacks via provider.
- Unsolicited messages (spam and abuse).
- Lack of data ownership and protection.
- Complexity of usage for all non-centralized protocols to non-technical users.

The concentration of the communication in a small number of centralized platforms makes resolving these problems quite difficult.

## Proposed solution

Proposed stack of protocols solves these problems by making both messages and contacts stored only on client devices, reducing the role of the servers to simple message relays that only require authorization of messages sent to the queues, but do NOT require user authentication - not only the messages but also the metadata is protected becuse users do not have any identifiers assiged to them - unlike with any other platforms.

See [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information on platform objectives and technical design.

## Comparison with other protocols

|                                                |    SimpleX chat    | Signal, big platforms |  XMPP, Matrix   |  P2P protocols  |
| :--------------------------------------------- | :----------------: | :-------------------: | :-------------: | :-------------: |
| Requires user identifiers                      |    No = private    |    Yes<sup>1</sup>    | Yes<sup>2</sup> | Yes<sup>3</sup> |
| Possibility of MITM                            |    No = secure     |    Yes<sup>4</sup>    |       Yes       |       Yes       |
| Dependence on DNS                              |   No = resilient   |          Yes          |       Yes       |       No        |
| Single operator or network                     | No = decentralized |          Yes          |       No        | Yes<sup>5</sup> |
| Central component or other network-wide attack |   No = resilient   |          Yes          | Yes<sup>2</sup> | Yes<sup>6</sup> |

1. Usually based on a phone number, in some cases on usernames.
2. DNS based.
3. Public key or some other globally unique ID.
4. If operator’s servers are compromised.
5. While P2P networks and cryptocurrency-based networks are distributed, they are not decentralized - they operate as a single network, with a single namespace of user addresses.
6. P2P networks either have a central authority or the whole network can be compromised - see the next section.

## Comparison with [P2P][9] messaging protocols

There are several P2P chat/messaging protocols and implementations that aim to solve privacy and centralisation problem, but they have their own set of problems that makes them less reliable than the proposed design, more complex to implement and analyse and more vulnerable to attacks.

1. [P2P][9] networks use some variant of [DHT][10] to route messages/requests through the network. DHT implementations have complex designs that have to balance reliability, delivery guarantee and latency. The proposeddesign has both better delivery guarantees and lower latency (the message is passed multiple times in parallel, through one node each time, using servers chosen by the recipient, while in P2P networks the message is passed through `O(log N)` nodes sequentially, using nodes chosen by the algorithm).

2. The proposed design, unlike most P2P networks, has no global user identitifiers of any kind, even temporary.

3. P2P itself does not solve [MITM attack][2] problem, and most existing solutions do not use out-of-band messages for the initial key exchange. The proposed design uses out-of-band messages or, in some cases, pre-existing secure and trusted connections for the initial key exchange.

4. P2P implementations can be blocked by some Internet providers (like [BitTorrent][11]). The proposed design is transport agnostic - it can work over standard web protocols, and the servers can be deployed on the same domains as the websites.

5. All known P2P networks are likely to be vulnerable to [Sybil attack][12], because each node is discoverable, and the network operates as a whole. Known measures to reduce the probability of the Sybil attack either require a centralized component or expensive [proof of work][13]. The proposed design, on the opposite, has no server discoverability - servers are not connected, not known to each other and to all clients. The SimpleX network is fragmented and operates as multiple isolated connections. It makes network-wide attacks on SimpleX network impossible - even if some servers are compromised, other parts of the network can operate normally, and affected clients can switch to using other servers without losing contacts or messages.

6. P2P networks are likely to be vulnerable to [DRDoS attack][14]. In the proposed design clients only relay traffic from known trusted connection and cannot be used to reflect and amplify the traffic in the whole network.

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[9]: https://en.wikipedia.org/wiki/Peer-to-peer
[10]: https://en.wikipedia.org/wiki/Distributed_hash_table
[11]: https://en.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://en.wikipedia.org/wiki/Proof_of_work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
