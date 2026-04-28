Revision 1, 2026-04-28

# SimpleX Channels: stateful information delivery and management

## Table of contents

- [Introduction](#introduction)
  - [What are SimpleX Channels](#what-are-simplex-channels)
  - [Channels as transport layer](#channels-as-transport-layer)
  - [Content visibility and participant privacy](#content-visibility-and-participant-privacy)
  - [In comparison](#in-comparison)
- [Architecture](#architecture)
  - [State and distribution](#state-and-distribution)
  - [Identity and ownership](#identity-and-ownership)
  - [Governance](#governance)
  - [Roles](#roles)
- [Cryptographic primitives](#cryptographic-primitives)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Signing scope: roster only, content optional](#signing-scope-roster-only-content-optional)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
  - [Stateful access and history navigation](#stateful-access-and-history-navigation)
  - [Transcript integrity](#transcript-integrity)
  - [End-to-end encrypted side conversations](#end-to-end-encrypted-side-conversations)
  - [Relay addition and removal](#relay-addition-and-removal)
  - [Governance evolution](#governance-evolution)
  - [Pre-moderation](#pre-moderation)
  - [Scheduled delivery](#scheduled-delivery)
  - [Link preview proxying](#link-preview-proxying)


## Introduction

### What are SimpleX Channels

SimpleX Channels are a stateful information delivery and management layer built on the [SimpleX network](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md). Where SMP queues provide stateless, unidirectional packet delivery between two endpoints, channels add persistence, state, and scalable distribution, enabling one-to-many publishing with cryptographic identity that is independent of the infrastructure operators.

[SimpleX Chat](https://simplex.chat) is the first application using channels. It presents them as a broadcast publication model similar to Telegram channels, where owners publish and subscribers read, add reactions and comment. But channels are not limited to this use case. They are a general-purpose layer for any application that needs to distribute and manage stateful information across a set of participants (feeds, telemetry collection, automated pipelines, coordination services, social media and other consumer-facing applications), using the same delivery and management mechanisms.

This document describes channels as a transport mechanism. SimpleX Chat uses the same term for its broadcast feature, which is the first application of this transport - but the same mechanism will also be used for large groups, and can support communities, wikis, forums, and other social media primitives.

The critical difference from conventional publish-subscribe systems is that channel identity and governance are controlled cryptographically by the channel owners, not by the infrastructure operators. Relays - the network nodes that forward and optionally cache channel content - can be added, removed, and replaced without changing the channel's identity, address, content, or cryptographic trust chain. A channel's relationship with its relays is transient; a channel's identity is permanent. This is unlike a website, whose identity (domain name) is controlled by a hosting provider and registrar. It is more like a set of owners' cryptographic keys that happen to have content distribution attached to it, where the authoritative record of this content is hosted on channel owners devices, and relays perform transmission and optional caching function similar to CDN infrastructure.

The channel owners hold full control of the channel - its identity, content, governance rules, and membership - through self-custody of cryptographic keys. No infrastructure operator, relay provider, or third party can freeze, seize, revoke, or alter a channel without the owner's keys. This is the same property that blockchain systems achieve for financial assets through network-wide consensus, but channels achieve it for information management through local authority and cryptographic signatures - without the cost of global consensus of a public ledger, and without sacrificing the privacy of participants.

### Channels as transport layer

The SimpleX network has three transport layers, each built on the one below:

1. **SMP** ([SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md)) - stateless, unidirectional packet delivery between two endpoints through SMP routers. Provides fixed-size blocks, 2-hop onion routing, and transport metadata protection. No user identifiers exist at this layer.

2. **SimpleX agents** ([agent protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - bidirectional, redundant connections between endpoints, with end-to-end post-quantum double ratchet encryption. The [SimpleX Chat Protocol](./simplex-chat.md) runs on top of this layer, providing direct messaging, group communication, and file transfer.

3. **Channels** - stateful, one-to-many information delivery and management with cryptographic ownership and programmable governance. This layer runs on top of chat and agent layer 2, and it is described in this document.

SMP provides point-to-point delivery. Chat protocol provides conversations. Channels provide publication, distribution, and management of stateful information. Just as SMP enables private messaging by providing transport without user identifiers, channels enable public communication while preserving the same privacy properties at the distribution layer.

The crucial architectural consequence of this layering is that channel relays are themselves SimpleX clients in the SMP network. A relay connects to SMP routers using the same protocol, the same 2-hop onion routing, and the same fixed-size transport blocks as any other SimpleX endpoint. Even though the SMP network can distinguish a relay from a person's phone by its transport patterns, it prevents relays from learning anything about other network endpoints. Relays don't even have a separate codebase - any CLI client can act as a chat relay without any modifications.

Channels therefore inherit all of SMP's transport privacy properties:

- **Relays cannot observe subscriber network addresses.** The relay sees SMP queue addresses, not IP addresses or network sessions. The subscriber's IP is known only to their SMP router, which cannot see the message content (encrypted at the agent layer).

- **SMP routers cannot see channel content.** Messages between relay and subscriber are end-to-end encrypted. The SMP router forwards fixed-size encrypted blocks without knowing whether they carry channel messages, direct messages, or anything else.

- **Participation in multiple channels is unlinkable.** Each channel connection uses independent SMP queues with separate cryptographic credentials. A relay serving multiple channels cannot link them; an SMP router carrying traffic to multiple relays cannot link them either.

No single point in the system sees both content and network identity. SMP routers see network addresses but not content. Relays see content but not network addresses.

### Content visibility and participant privacy

This transport layering produces a specific combination of properties for public communication that is not present in any other publishing system.

Any channel joinable via a public link must be considered completely public - the cost of joining through automated means has collapsed with large language models and is approaching zero. End-to-end encrypting such content provides no privacy; it only harms users by creating a false expectation and harms operators by making them unable to see what they deliver.

Channel content is therefore not end-to-end encrypted between owner and subscriber. Relays can read the messages they forward. Relay operators cannot alter or moderate channel content - the authoritative record is held by owners - but they can decide whether to deliver a given channel, and stop delivering channels they do not want to serve.

The achievable privacy property for public communication is participation privacy - protecting who reads and writes, not what. SimpleX Channels provide this because of the SMP transport: it carries no user identifiers, and relays are ordinary SMP clients. Subscribers connect without revealing their identity, network address, or any information that persists across channels. If an adversary joins a SimpleX channel, they see everything that was said, but cannot determine who said it or link any participant to anything outside the channel.

Other systems make the opposite choice: content encryption in exchange for participant identification. For public-link groups this is exactly backwards - the content encryption is meaningless (anyone can join and read), while the participant identification is the real harm.

### In comparison

SimpleX Channels occupy a distinct position in the design space of public and semi-public communication systems.

**Telegram channels** provide scalable one-to-many delivery with rich content management. The operator controls channel identity (usernames are revocable), has full access to both content and participant identity, and the infrastructure is centralized. Censorship operates through username seizure, channel banning, or operator cooperation. Channels cannot exist without Telegram's permission.

**Nostr relays** use cryptographic user's identity - a Nostr publisher's identity is a key pair, independent of any relay. But a single persistent key is used for everything: publishing, following, and identity. Relays see content, the user's key, and their IP address. All posts and follow lists are signed and non-repudiable, linked to the same key - making both publishing and reading activity traceable and undeniable.

**Signal groups** use end-to-end encryption with Sender Key distribution. Content is encrypted; the operator cannot see it. But the operator manages group state and can observe the membership graph. Groups are capped at 1,000 members, and there is no concept of a channel.

**Matrix rooms** provide federated, encrypted group communication. Server operators see room membership and metadata. Key distribution depends on server cooperation. Room identity is bound to the creating server's domain - if the server disappears, the room identity is lost.

**Mastodon / ActivityPub** provides federated public posting. Publisher identity is bound to a server domain - if the server disappears, the identity is lost. Server operators see all content and all follower relationships. No encryption of any kind.

SimpleX Channels make a different set of trade-offs:

| Property | Telegram | Nostr | Signal | Matrix | Mastodon | **SimpleX** |
|---|---|---|---|---|---|---|
| Content visible to operator | Yes | Yes | No | Configurable | Yes | **Yes** |
| Participant identity visible to operator | Yes | Yes | Yes | Yes | Yes | **No** |
| Channel identity independent of infrastructure | No | Yes | No | No | No | **Yes** |
| Sovereign ownership (no 3rd party can seize) | No | Yes | No | No | No | **Yes** |
| Programmable governance | No | No | No | No | No | **Planned** |
| Content deniability | No | No | Yes | Yes | No | **Yes (default)** |
| Scalable one-to-many delivery | Yes | Yes | No | Limited | Yes | **Yes** |

## Architecture

### State and distribution

The most important architectural property of SimpleX Channels is where authoritative state lives: on the owner's devices, not on relays, not on any server, and not on any shared ledger.

The authoritative record of a channel - its content history, member roster, profile, cryptographic keys, and governance rules - is held by channel owners on their own devices. No intermediary holds it, and no intermediary can alter it. Relays hold transient copies of this state for the purpose of distribution and optional caching. The relationship is analogous to origin servers and CDN edge nodes: the origin holds the truth, and the CDN distributes copies. CDN nodes come and go; the origin persists.

Where blockchain systems achieve sovereignty over shared state through network-wide consensus (every node validates every transaction), channels achieve it through cryptographic authority - the owner's signature is the only proof needed. Consensus is only required between channel owners, not across the entire network. This makes channels cheaper to operate, faster to respond, and private by default - the network does not need to know about the channel's existence, or validate its changes.

**How content flows:**

```
                     ┌──────────┐
                     │  Owner   │  <- authoritative state
                     └────┬─────┘
                          │
              ┌───────────┼───────────┐
              │           │           │
         ┌────▼───┐  ┌────▼───┐  ┌────▼───┐
         │Relay A │  │Relay B │  │Relay C │  <- cache / distribution
         └────┬───┘  └────┬───┘  └────┬───┘
              │           │           │
        ┌─────┼─────┐    ...    ┌─────┼─────┐
        │     │     │           │     │     │
       S1    S2    S3          S7    S8    S9   <- received copies
```

Content originates on the owner's device and flows through relays to subscribers. Each relay independently forwards to all of its subscribers. When multiple relays serve the same channel, each subscriber receives the same message from each relay it is connected to, and deduplicates at the client level.

Subscribers do not connect to owners or to each other - all communication passes through relays. This star topology provides better scalability that peer-to-peer SimpleX groups (adding subscribers requires only a new relay connection, not N connections to N existing members), subscriber privacy (a subscriber's connection metadata is known only to the relay it connects to), and moderate load on owners (each message is sent once per relay, not once per subscriber).

**Consequences of this data architecture:**

- **Loss of a relay is loss of a cache node, not loss of data.** If a relay disappears or is removed, the owner can send the same content through a replacement relay. No content is permanently lost. Subscribers experience temporary disruption, not data loss.

- **Loss of all owner devices is the catastrophic event.** It is the equivalent of losing the origin server with no backup. All relay caches become orphaned - they hold copies but can no longer receive new content or signed administrative updates. The channel's private keys are gone, so no new owners can be authorized and no signed messages can be produced. Having multiple owners (or single owner's devices) together with backups mitigates this risk.

- **Relays do not hold authoritative state.** A relay's local database is a delivery queue and optional content cache, not the channel's database. When a relay persists delivery tasks and jobs, that persistence serves delivery reliability - the ability to resume forwarding after a crash - not authoritative storage.

- **Disagreements between relays can be resolved by the origin.** If two relays deliver different content for the same message, the owner can serve as the authoritative tiebreaker (through any reachable relay). This is stronger than probabilistic majority-of-honest-relays reasoning: the origin settles cache inconsistency.

Subscribers hold their own received copies. For signed messages, these copies are independently verifiable - a subscriber can confirm that a roster change was signed by a legitimate owner without consulting the relay or the owner. For unsigned content messages, verification depends on cross-relay consistency or future transcript integrity mechanisms.

### Identity and ownership

A channel's identity is the SHA-256 hash of the genesis root public key, computed at creation time and never changed - even if relays are added, removed, or the channel link is rotated. This identity is self-authenticating: it is derived from a key pair that only the channel creator held. It is embedded in the channel's link, distributed in the channel profile to all members, and used as a binding prefix in all signed messages.

Subscribers validate that the identity in the link matches the identity in the profile, preventing link substitution. Profile updates that attempt to change the identity are rejected. Full validation that the identity matches the root key is deferred to a future protocol version that includes key rotation - see the [group identity binding RFC](../rfcs/2026-03-28-group-identity-binding.md).

Channel ownership is not tied to the root key directly. Instead, the root key authorizes owner keys through a signed chain. At creation, the owner generates a root key pair and a separate member key pair for signing. The member key is published as an owner authorization entry signed by the root key. Anyone retrieving the channel link can verify that the owner's signing key was authorized by the root key. New owners can be added by having any existing authorized key sign a new entry, forming a verifiable chain.

This model separates the channel's permanent identity (the root key hash) from the signing keys used for day-to-day operations. The root key is a bootstrap key - it certifies owners, then need not be used again. All owners are cryptographically indistinguishable to subscribers (they all have equally valid authorization chains), which - provided multiple owners were signed by the root key - conceals the creator's identity.

The channel link serves as an out-of-band trust anchor that cannot be tampered with by relays or SMP routers (they cannot modify link content). All members announce their signing keys on joining. Owner keys are verifiable against the link. The trust chain extends further: role changes (promoting members to admin, moderator) will be signed by owners and recorded as a linearly ordered roster log, with consistent sequencing across all owners, relays, and subscribers. This linearization prevents ambiguous roster states that would arise from concurrent unordered role changes. The result is a chain of trust from the channel link through owners to all elevated roles, preventing relay MITM on member introductions and role assignments. Out-of-band key verification for non-owner members will further extend this protection to E2E encrypted conversations within the channel.

### Governance

"Management" in "information delivery and management" refers not only to managing content but to managing the channel itself - who can make decisions, and how.

The low-level protocol supports multiple owners from the initial release. The application-level governance model evolves through a planned progression:

**Current (v6.5): Single owner.** One owner controls the channel. All administrative actions (profile changes, roster modifications, relay management) are decided by this single owner. The protocol-level owners chain supports verification of multiple entries, but the application creates and manages only one.

**Near-term (v7): Multiple owners, any-owner-decides.** Multiple owners share control of the channel. Any owner can independently make any administrative decision - add or remove members, change the profile, manage relays. This is the most common decision-making model in practice (equivalent to "all admins are equal" in most online platforms). No coordination between owners is required for any action.

**Future: Multisig and programmable governance.** Further stages include M-of-N multisig for administrative actions and, eventually, programmable governance rules defined as code in the channel's definition. The protocol must support these without prescribing a specific governance model.

### Roles

A channel has three classes of participant:

- **Owners** create the channel, control its identity and profile, manage the member roster, publish content, and hold the authoritative state on their devices. A channel must have at least one owner. Owners hold the private keys required to sign administrative messages and optionally used to sign content messages.

- **Relays** are distribution agents. They receive content from owners and other members who have posting rights, optionally cache it, and forward it to subscribers. They accept connection requests from new subscribers and introduce them to the channel. Relays cannot author messages of their own. They hold no authoritative state - only delivery queues and optional content caches. A channel must have at least one active relay.

- **Subscribers** connect to relays and receive content. They cannot send messages to the channel by default, but can be given privileges for groups and channels based on chat relays.

Additional roles (moderator, admin, member, author) exist in the hierarchy and are inherited from the group protocol.

Relays are SimpleX Chat clients, not special-purpose servers. They run the same client code as any other participant. A relay can be operated by anyone - a channel operator, a third-party service provider, or a self-hosted instance. Each relay creates its own contact address link, bound to the channel's identity, through which subscribers connect. The relay's relationship with the channel is transient - the owner can add and remove relays without changing the channel's identity or address.

For protocol-level detail - wire formats, message types, signing and verification mechanics, delivery pipeline - see [SimpleX Channels Protocol](./channels-protocol.md).


## Cryptographic primitives

Channel identity, ownership, and integrity rely on the following cryptographic primitives:

- **Ed25519** - used for channel identity (root key pair), owner authorization chain (`OwnerAuth` signatures binding owner keys to the root key), and signing of administrative messages (roster changes, profile updates, channel deletion) and optional signing of content messages. The signature binding prefix includes the channel's entity ID and the sender's member ID, preventing cross-channel replay.

- **SHA-256** - used to derive the channel's entity ID from the genesis root public key (`entityId = sha256(rootPubKey)`). This value is immutable and serves as the channel's permanent identity.

- **Double ratchet with post-quantum KEM** (inherited from [SimpleX agent layer](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - provides end-to-end encryption for all SMP transport between relay and subscriber, relay and owner, and between any two SimpleX endpoints. This is not channel-specific cryptography - it is the standard SimpleX transport encryption that channels inherit by being built on the agent layer. Future E2E encrypted side conversations (support scope, member DMs, private channels) will use the same double ratchet mechanism.

Content messages are not signed by default to preserve deniability - see [Signing scope](#signing-scope-roster-only-content-optional). Owners may opt into signing all content - it will be added in the future app release.


## Security

### Design objectives

The channel protocol is designed to achieve the following security objectives:

1. **Stable message delivery** between channel participants, resilient to individual relay failures.
2. **No possibility for a relay to substitute the channel** - the channel's identity is cryptographically bound to the link and profile controlled by channel owners.
3. **No possibility for a relay to impersonate an owner** - administrative messages require valid signatures.
4. **Prevention of relay-initiated roster manipulation** - member removal, role changes, and other roster modifications require valid owner signatures.
5. **Relay transience** - the owner can add and remove relays, including the last relay, without permanently losing the channel. Subscribers can restore connectivity by retrieving updated link data.
6. **Sender anonymity within multi-owner channels** - owners can publish as the channel, hiding which specific owner authored a message from subscribers.
7. **Participant privacy** - relay operators cannot determine subscriber identity or network address, and subscribers cannot determine each other's identity. This is inherited from the SMP transport layer.

### Signing scope: roster only, content optional

By default, only roster-modifying and administrative messages are signed. Content messages (`XMsgNew`, `XMsgUpdate`, `XMsgDel`, etc.) are not signed. This is a deliberate design choice for two reasons:

1. **Deniability.** Signing content creates non-repudiable proof of authorship. Anyone with the message bytes could prove who wrote a specific message. This is antithetical to SimpleX's privacy model, where communications should be deniable.

2. **Proportional defense.** Roster and profile changes are disruptive and irreversible. This includes member removal, role changes, and channel deletions. These must be authenticated at processing time. Content manipulation by a relay is detectable post-hoc through cross-relay consistency (when multiple independent relays forward the same content). Content delivery is not irreversible - a forged message can be flagged and corrected, and the authoritative record on the owner's device is unaffected.

A client change will allow owners to opt into signing all messages, including content. This can be a per-channel or per-message choice - some publishers want non-repudiable authorship (similar to Nostr), others prefer deniability.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and addresses threats specific to the channel layer.

**A single compromised relay**

*can:*

- Substitute unsigned content or selectively drop messages for its subscribers. Detectable by subscribers connected to other relays - the owner's version is authoritative. TODO: difference detection not yet implemented.
- Selectively target specific subscribers while delivering correctly to others.
- Ignore the "message from channel" directive, revealing which owner sent a message. Detectable out-of-band.
- Fabricate or hide subscriber connections, inflating or deflating counts. Detectable if subscribers are connected to other relays.

*cannot:*

- Undetectably substitute content - subscribers on honest relays receive the original.
- Alter the channel's authoritative state on the owner's device.
- Substitute the channel profile or impersonate an owner - these require valid signatures.
- Redirect subscribers to a different channel - the entity ID is validated across link and profile.
- Determine subscriber identity or network address - inherited from SMP transport.
- Correlate subscriber participation across channels - each connection uses independent SMP queues with the subscriber's independently chosen SMP router, so even collusion between a relay and the relay's own SMP router does not compromise the subscriber's connection through a different router.

**All relays compromised and colluding**

*can:*

- Undetectably substitute unsigned content for all subscribers, unless owners sign content messages.
- Prevent delivery of any messages, including signed ones (signing prevents substitution, not dropping).
- Fabricate or hide subscriber connections undetectably.

*cannot:*

- Forge signed administrative messages or substitute the channel profile.
- Alter the authoritative state on the owner's device.

**Loss of all owner devices**

The channel can have no new content, no administrative updates, no new owners. Relay caches continue delivering existing content but cannot be refreshed, and will eventually expire in the absense of the owner connection. Multiple owners and key backups mitigate this risk.

**A subscriber**

*can:*

- See all public content, by design.
- Join multiple times with different profiles, inflating counts.

*cannot:*

- Identify other subscribers, send messages to the channel (unless given posting rights), or forge messages of the owner or other subscribers.

**A passive network observer**

*can:*

- Observe communication with an SMP router, but not whether it is channel-related.

*cannot:*

- Determine which channel a subscriber uses, correlate channel activity with other SimpleX activity, or identify a relay as distinct from an ordinary user, other than by traffic volume. All inherited from SMP transport properties.

### Current gaps

1. **Cross-relay consistency detection.** Duplicate messages are silently deduplicated without hash comparison. Designed but not implemented.
2. **Link entity ID validation.** Deferred to a future version with key rotation. See [group identity binding RFC](../rfcs/2026-03-28-group-identity-binding.md).
3. **Multi-relay UX.** Protocol supports multiple relays per subscriber; no UX for monitoring relay-level delivery health. It will be added in v6.5.x.


## Future work

### Stateful access and history navigation

Currently, relays send recent cached history on join but do not support navigation or search. Planned: history pagination by timestamp or message ID, remote search against relay caches, and selective retrieval of specific message ranges. Relay operators can differentiate on cache depth and search capabilities.

### Transcript integrity

- **Opt-in content signing.** Per-channel or per-message choice to sign content, making it non-repudiable. This will be released in SimpleX Chat v7.
- **Subscriber transcript acknowledgment.** Subscribers periodically sign a digest of received history ("I've seen it" rather than "I've authored it"), enabling detection of relay manipulation through diverging digests.
- **Merkle tree signing.** Owner periodically publishes a signed Merkle root. Subscribers verify their copies against the owner's authoritative record.

### End-to-end encrypted side conversations

- **E2E encrypted support scope** between subscriber and moderator/owner.
- **E2E encrypted DMs between members** where channel settings permit, using standard SimpleX connection establishment.
- **Private channels** where the entire content stream is encrypted to authorized subscribers. The relay becomes a mere conduit that sees neither content nor identity.

### Relay addition and removal

Dynamic relay addition with cache population from existing relays or owner. Relay removal with subscriber migration. Relay rotation with continuity - new relay connects before old relay is removed. It will be added in v6.5.x.

### Governance evolution

- **Multiple owners (v7):** concurrent administrative authority, any owner acts independently.
- **Multisig:** M-of-N approval for administrative actions, with per-action quorums.
- **Programmable governance:** rules defined as code in the channel definition.

### Pre-moderation

Subscriber messages reviewed by moderators before becoming visible to all subscribers.

### Scheduled delivery

Messages scheduled for future delivery, cached by relay until the scheduled time.

### Link preview proxying

The relay loads link previews on behalf of the sender. The relay already sees message content, so it learns nothing new. Unlike the sender, its IP is not linked to any identity. Unlike external proxies (Signal, WhatsApp), it is not undermining an encryption promise - content is already visible to it by design.
