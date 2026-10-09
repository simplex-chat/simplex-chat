# The World's Most Secure Messaging

**Ultimate security**: SimpleX network uses the most secure end-to-end encryption, with continuous post-quantum key exchange to protect all messages and metadata.

**Unique privacy**: SimpleX network has no user profile IDs, not even random numbers or keys. It provides better privacy of your contacts, protecting who you talk with from network servers.

**No spam**: nobody can contact you unless you share 1-time link or long-term address.

**Data ownership**: only your device stores your profiles, contacts and messages. You can securely move your data to another device. Servers store encrypted messages only while your device is offline.

**Secure decentralization**: you control which servers to connect to. For security 4 different servers are used in each chat — they can't observe which IP addresses talk to each other.

#### How to connect to others

- Tap new chat button in the corner, then create 1-time link.
- Share the link with your contact via any other messenger or email - it is secure.
- Ask your contact to use the link in the app - click it after the app is installed or paste into the search field in the app.

## Why SimpleX is unique

### You have complete privacy

SimpleX protects the privacy of your profile, contacts and metadata, hiding it from SimpleX network servers and any observers.

Unlike any other existing messaging network, SimpleX has no identifiers assigned to the users — **not even random numbers**.

### You are protected from spam and abuse

Because you have no identifier or fixed address on the SimpleX network, nobody can contact you unless you share a one-time or temporary user address, as a QR code or a link.

### You control your data

SimpleX stores all user data on client devices in a **portable encrypted database format** — it can be transferred to another device.

The end-to-end encrypted messages are held temporarily on SimpleX relay servers until received, then they are permanently deleted.

### You own SimpleX network

The SimpleX network is fully decentralised and independent of any crypto-currency or any other network, other than the Internet.

You can **use SimpleX with your own servers** or with the servers provided by us — and still connect to any user.

# Full privacy of your identity, profile, contacts and metadata

Unlike other messaging networks, SimpleX has **no identifiers assigned to the users**. It does not rely on phone numbers, domain-based addresses (like email or XMPP), usernames, public keys or even random numbers to identify its users — SimpleX server operators don't know how many people use their servers.

To deliver messages SimpleX uses [pairwise anonymous addresses](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) of unidirectional message queues, separate for received and sent messages, usually via different servers.

This design protects the privacy of who you are communicating with, hiding it from SimpleX network servers and from any observers. To hide your IP address from the servers, you can **connect to SimpleX servers via Tor**.

# The best protection from spam and abuse

Because you have no identifier on the SimpleX network, nobody can contact you unless you share a one-time or temporary user address, as a QR code or a link.

Even with the optional user address, while it can be used to send spam contact requests, you can change or completely delete it without losing any of your connections.

# Ownership, control and security of your data

SimpleX Chat stores all user data only on client devices using a **portable encrypted database format** that can be exported and transferred to any supported device.

The end-to-end encrypted messages are held temporarily on SimpleX relay servers until received, then they are permanently deleted.

Unlike federated networks servers (email, XMPP or Matrix), SimpleX servers don't store user accounts, they only relay messages, protecting the privacy of both parties.

There are no identifiers or ciphertext in common between sent and received server traffic — if anybody is observing it, they cannot easily determine who communicates with whom, even if TLS is compromised.

# Fully decentralised — users own the SimpleX network

You can **use SimpleX with your own servers** and still communicate with people who use the servers preconfigured in the apps.

SimpleX network uses an [open protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) and provides [SDK to create chat bots](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript), allowing implementation of services that users can interact with via SimpleX Chat apps — we're really looking forward to seeing what SimpleX services you will build.

If you are considering developing for the SimpleX network, for example, the chat bot for SimpleX app users, or the integration of the SimpleX Chat library into your mobile apps, please [get in touch](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) for any advice and support.

## ­Funcționalități

Mesaje criptate E2E, cu formatare Markdown și editare

Imagini, videoclipuri și fișiere
criptate E2E

Grupuri descentralizate criptate E2E — doar utilizatorii știu că există

Mesaje vocale criptate E2E

Mesaje care dispar

Apeluri audio și video
criptate E2E

Stocare portabilă criptată a aplicației — mută profilul pe alt dispozitiv

Mod incognito —
unic în SimpleX Chat

## What makes SimpleX private

### Temporary anonymous pairwise identifiers

SimpleX uses temporary anonymous pairwise addresses and credentials for each user contact or group member.

It allows messages to be delivered without user profile identifiers, providing better meta-data privacy than alternatives.

### Schimb de chei în afara benzii

Many communication networks are vulnerable to MITM attacks by servers or network providers.

To prevent it SimpleX apps pass one-time keys out-of-band, when you share an address as a link or a QR code.

### 2 straturi de criptare end-to-end

Double-ratchet protocol —
OTR messaging with perfect forward secrecy and break-in recovery.

NaCL cryptobox in each queue to prevent traffic correlation between message queues if TLS is compromised.

### Verificarea integrității mesajului

To guarantee integrity the messages are sequentially numbered and include the hash of the previous message.

If any message is added, removed or changed the recipient will be alerted.

### Strat adițional de criptare pe server

Additional layer of server encryption for delivery to the recipient, to prevent the correlation between received and sent server traffic if TLS is compromised.

### Amestecare mesaje pentru a reduce corelația

SimpleX servers act as low latency mix nodes — the incoming and outgoing messages have different order.

### Transport TLS securizat și autentificat

Only TLS 1.2/1.3 with strong algorithms is used for client-server connections.

Server fingerprint and channel binding prevent MITM and replay attacks.

Connection resumption is disabled to prevent session attacks.

### Acces opțional prin Tor

To protect your IP address you can access the servers via Tor or some other transport overlay network.

To use SimpleX via Tor please install [Orbot app](https://guardianproject.info/apps/org.torproject.android/) and enable SOCKS5 proxy (or VPN [on iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### Unidirectional message queues

Each message queue passes messages in one direction, with the different send and receive addresses.

It reduces the attack vectors, compared with traditional message brokers, and available meta-data.

### Straturi multiple de padding al conținutului

SimpleX uses content padding for each encryption layer to frustrate message size attacks.

It makes messages of different sizes look the same to the servers and network observers.

## SimpleX Network

SimpleX Chat provides the best privacy by combining the advantages of P2P and federated networks.

### Unlike P2P networks

All messages are sent via the servers, both providing better metadata privacy and reliable asynchronous message delivery, while avoiding many .

# Comparație cu protocoalele de mesagerie P2P

[P2P](https://en.wikipedia.org/wiki/Peer-to-peer) messaging protocols and apps have various problems that make them less reliable than SimpleX, more complex to analyse, and vulnerable to several types of attack.

1. P2P networks rely on some variant of [DHT](https://en.wikipedia.org/wiki/Distributed_hash_table) to route messages. DHT designs have to balance delivery guarantee and latency. SimpleX has both better delivery guarantee and lower latency than P2P, because the message can be redundantly passed via several servers in parallel, using the servers chosen by the recipient. In P2P networks the message is passed through *O(log N)* nodes sequentially, using nodes chosen by the algorithm.
2. SimpleX design, unlike most P2P networks, has no global user identifiers of any kind, even temporary, and only uses temporary pairwise identifiers, providing better anonymity and metadata protection.
3. P2P does not solve [MITM attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) problem, and most existing implementations do not use out-of-band messages for the initial key exchange. SimpleX uses out-of-band messages or, in some cases, pre-existing secure and trusted connections for the initial key exchange.
4. P2P implementations can be blocked by some Internet providers (like [BitTorrent](https://en.wikipedia.org/wiki/BitTorrent)). SimpleX is transport agnostic — it can work over standard web protocols, e.g. WebSockets.
5. All known P2P networks may be vulnerable to [Sybil attack](https://en.wikipedia.org/wiki/Sybil_attack), because each node is discoverable, and the network operates as a whole. Known measures to mitigate it require either a centralized component or expensive [proof of work](https://en.wikipedia.org/wiki/Proof_of_work). SimpleX network has no server discoverability, it is fragmented and operates as multiple isolated sub-networks, making network-wide attacks impossible.
6. P2P networks may be vulnerable to [DRDoS attack](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent), when the clients can rebroadcast and amplify traffic, resulting in network-wide denial of service. SimpleX clients only relay traffic from known connections and cannot be used by an attacker to amplify the traffic in the whole network.

---

### Unlike federated networks

SimpleX relay servers do NOT store user profiles, contacts and delivered messages, do NOT connect to each other, and there is NO servers directory.

---

### SimpleX network

servers provide unidirectional queues to connect the users, but they have no visibility of the network connection graph — only the users do.

## SimpleX, explicat

1. Ce experimentează utilizatorii

2. Cum funcționează

3. Ce văd serverele

1. Ce experimentează utilizatorii

Poți crea contacte și grupuri, și poți avea conversații bidirecționale, ca în oricare altă aplicație de mesagerie.

Cum poate funcționa cu liste de așteptare unidirecționale și fără identificatori de profil ai utilizatorului?

2. Cum funcționează

Pentru fiecare conexiune, folosești două cozi de mesaje separate pentru a trimite și a primi mesaje prin servere diferite.

Serverele doar transmit mesaje într-un singur sens, fără a deține imaginea de ansamblu a conversațiilor sau a conexiunilor utilizatorului.

3. Ce văd serverele

Serverele au credențiale anonime separate pentru fiecare coadă și nu știu cui aparțin acestea.

Utilizatorii pot îmbunătăți și mai mult confidențialitatea metadatelor folosind Tor pentru a accesa serverele, prevenind astfel corelarea prin adresa IP.

## Comparison with other protocols

|  |  | Signal, big platforms | XMPP, Matrix | P2P protocols |
| --- | --- | --- | --- | --- |
| Requires global identity | No - private | Yes [1] | Yes [2] | Yes [3] |
| Possibility of MITM | No - secure [4] | Yes [5] | Yes | Yes |
| Dependence on DNS | No - resilient | Yes | Yes | No |
| Single or centralized network | No - decentralized | Yes | No - federated [6] | Yes [7] |
| Central component or other network-wide attack | No - resilient | Yes | Yes [2] | Yes [8] |

---

1. Usually based on a phone number, in some cases on usernames
2. DNS-based addresses
3. Public key or some other globally unique ID
4. SimpleX relays cannot compromise e2e encryption. Verify security code to mitigate attack on out-of-band channel
5. If operator's servers are compromised. Verify security code in Signal and some other apps to mitigate it
6. Does not protect users' metadata privacy
7. While P2P are distributed, they are not federated — they operate as a single network
8. P2P networks either have a central authority or the whole network can be compromised — [see here](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Comparison of end-to-end encryption security in different messengers

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Message padding | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Repudiation (deniability) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Forward secrecy | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Post-compromise security | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| 2-factor key exchange | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Post-quantum hybrid crypto | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar pads messages to the size rounded up to 1024 bytes, Signal - to 160 bytes
2. Repudiation does not include client-server connection.
3. It appears that the usage of cryptographic signatures compromises repudiation (deniability), but it needs to be clarified.
4. Multi-device implementation compromises post-compromise security of Double Ratchet — [see here](https://eprint.iacr.org/2021/626.pdf).
5. 2-factor key exchange is optional via security code verification.
6. Post-quantum key agreement is "sparse" — it protects only some of the ratchet steps.
