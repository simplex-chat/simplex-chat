Revision 1, 2026-04-28

# SimpleX Channels: stateful information delivery and management

## Table of contents

- [Introduction](#introduction)
  - [What are SimpleX Channels](#what-are-simplex-channels)
  - [Channels as transport layer](#channels-as-transport-layer)
  - [Content visibility and participant privacy](#content-visibility-and-participant-privacy)
  - [In comparison](#in-comparison)
  - [Non-goals](#non-goals)
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
- [Conclusion](#conclusion)


## Introduction

The SimpleX network provides private point-to-point communication without user or endpoint identifiers, but most speech that matters is public. Every existing platform that distributes content at scale identifies both publishers and their audiences to the operator - none protect participation privacy. SimpleX Chat supported peer-to-peer groups, but they cannot scale to large audiences. SimpleX Channels close this gap.

### What are SimpleX Channels

SimpleX Channels are a stateful information delivery and management layer built on the [SimpleX network](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md). SMP queues provide stateless, unidirectional packet delivery between two endpoints. Channels add persistence, state, and scalable distribution - enabling one-to-many publishing with cryptographic identity independent of infrastructure operators.

[SimpleX Chat](https://simplex.chat) is the first application, presenting channels as a broadcast publication model where owners publish and subscribers read, react, and comment. But channels are not limited to this use case - they are a general-purpose layer for distributing and managing stateful information (feeds, telemetry, automated pipelines, coordination services, social media). This document describes channels as a transport mechanism - the same mechanism will also be used for large groups, communities, wikis, forums, and other social media primitives.

The critical difference from conventional publish-subscribe systems is that channel identity and governance are controlled cryptographically by the channel owners, not by the infrastructure operators. Relays - SimpleX network clients that forward and optionally cache channel content - can be added, removed, and replaced without changing the channel's identity, address, content, or cryptographic trust chain. A channel's relationship with its relays is transient; its identity is permanent. The authoritative record of content is hosted on channel owners' devices; relays perform transmission and caching similar to CDN infrastructure.

Channel owners hold full control of the channel - its identity, content, governance rules, and membership - through self-custody of cryptographic keys. No infrastructure operator, relay provider, or third party can control or alter a channel without the owner's keys. Blockchain systems achieve a related property for financial assets - no third party can control holdings - through network-wide consensus. Channels achieve it through local authority and cryptographic signatures, without global consensus or a public ledger. Unlike blockchain state, channel state is mutable by the owner and not publicly verifiable by third parties.

### Channels as transport layer

The SimpleX network has three transport layers, each built on the one below:

1. **SMP** ([SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md)) - stateless, unidirectional packet delivery between two endpoints through SMP routers. Provides fixed-size blocks, 2-node onion routing, and transport metadata protection.

2. **SimpleX agents** ([agent protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - bidirectional, redundant connections between endpoints, with end-to-end post-quantum double ratchet encryption. The [SimpleX Chat Protocol](./simplex-chat.md) runs on top of this layer, providing direct messaging, group communication, and metadata delivery for file transfers via [XFTP protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/xftp.md).

3. **Channels** - stateful, one-to-many information delivery and management with cryptographic ownership and programmable governance. This layer runs on top of chat and agent layer 2, and it is described in this document.

No network-wide user profile identifiers exist at any of these layers. Just as SMP enables private messaging by providing transport without user identifiers, channels enable public communication while preserving participation privacy at the distribution layer.

Channel relays are themselves SimpleX clients in the SMP network, connecting to SMP routers using the same protocol, the same 2-node onion routing, and the same fixed-size transport blocks as any other endpoint. Even though the SMP network can distinguish a relay from a person's phone by its transport patterns, it prevents relays from learning anything about other network endpoints. In the case of SimpleX Chat, any CLI client can act as a chat relay without modifications.

Channels therefore inherit all of SMP's transport privacy properties:

- **Relays cannot observe subscriber network addresses.** The relay sees SMP queue addresses, not IP addresses or network sessions. The subscriber's IP is known only to their SMP router, which cannot see the message content (encrypted at the agent layer) or the IP addresses of whoever sends messages.

- **SMP routers cannot see channel content.** Messages between relay and subscriber are end-to-end encrypted. The SMP router forwards fixed-size encrypted blocks without knowing whether they carry channel messages, direct messages, or anything else.

- **Participation in multiple channels is unlinkable.** Each channel connection uses independent SMP queues with separate cryptographic credentials. Because of packet-level anonymity in 2-node routing, even if a subscriber uses the same SMP routers for all channels, the sending relays cannot determine this without collusion with those routers. Clients choose independently operated routers by default.

No single point in the system sees both content and network identity. SMP routers see network addresses but not content, and no single SMP router can see which endpoints are communicating because clients choose independently operated routers. Relays see content but not network addresses.

### Content visibility and participant privacy

Any channel joinable via a public link, whether encrypted or not, must be considered completely public - the cost of joining through automated means has collapsed with large language models and is approaching zero. End-to-end encrypting such content provides no privacy; it only undermines users' security by creating false expectations and increases infrastructure operators' risks by making them unable to see what they deliver. Private channels with encrypted content are a separate use case discussed in [Future work](#end-to-end-encrypted-side-conversations).

Content of public channels is therefore not end-to-end encrypted between owner and subscriber. Relays can read the messages they forward. Relay operators cannot undetectably alter channel content when multiple relays serve the channel, and cannot alter signed content at all - the authoritative state is held by owners. That each channel can use multiple chat relays provides both technical reliability and censorship resistance against any relay-specific content policies.

The achievable privacy property for public communication is participation privacy - protecting who reads and writes content. The SMP transport carries no user identifiers, and relays are ordinary SMP clients, so subscribers connect without revealing their identity, network address, or any information that persists across channels. If an adversary joins a SimpleX channel, they see everything that is sent, but cannot determine who sent it or link any participant to anything outside the channel.

Other systems make the opposite choice: content encryption in exchange for participant identification. For groups and channels joinable via public links this is the opposite of what is needed - the content encryption is meaningless (anyone can join and read), while the participant identification is the security threat.

### In comparison

**Telegram channels** - the operator controls channel identity (usernames are revocable), has full access to both content and participant identity. Channels cannot exist without Telegram's permission.

**Nostr relays** - a single persistent key is used for publishing, following, and identity. Relays see content, the user's key, and their IP address. All posts and follow lists are signed and non-repudiable, linked to the same key - making both publishing and reading activity traceable and undeniable.

**Signal groups** - content is end-to-end encrypted, but the operator manages group state and can observe the membership graph. Groups are capped at 1,000 members with no concept of a channel.

**Matrix rooms** - server operators see room membership and metadata. Room identity is bound to the creating server's domain - if the server disappears, the room identity is lost.

**Mastodon / ActivityPub** - publisher identity is bound to a server domain - if the server disappears, the identity is lost. Server operators see all content and all follower relationships. No encryption or privacy of any kind.

| Property | Telegram | Nostr | Signal | Matrix | Mastodon | **SimpleX** |
|---|---|---|---|---|---|---|
| Content visible to operator | Yes | Yes | No | Configurable | Yes | **Yes** |
| Participant identity visible to operator | Yes | Yes | Yes | Yes | Yes | **No** |
| Channel identity independent of infrastructure | No | Yes | No | No | No | **Yes** |
| Sovereign ownership (no 3rd party can seize) | No | Yes | No | No | No | **Yes** |
| Programmable governance | No | No | No | No | No | **Planned** |
| Cryptographic content deniability | No | No | Yes | Yes | No | **Yes (default)** |
| Scalable one-to-many delivery | Yes | Yes | No | Limited | Yes | **Yes** |

### Non-goals

Channels do not attempt to:

- **Encrypt public content from relay operators.** See [Content visibility and participant privacy](#content-visibility-and-participant-privacy).
- **Assign persistent identities to participants.** There are no usernames, public keys, or any identifiers that persist across channels or link activity across contexts.
- **Require network-wide consensus.** Channel state is authoritative on owner devices. The network does not validate channel transactions.
- **Guarantee immutability of content.** Channel state is fully controlled and mutable by owners, unlike blockchain state, which is immutable by design.

## Architecture

The introduction established what channels provide and why. This section describes how: where state lives, how identity and ownership work, how governance evolves, and what each participant does.

### State and distribution

The authoritative record of a channel - content, member roster, profile, cryptographic keys, governance rules - is held by channel owners on their own devices, not on relays, not on any server, and not on any shared ledger. Relays hold transient copies for distribution and optional caching, analogous to CDN edge nodes: the origin holds the truth, CDN nodes come and go. Consensus is only required between channel owners, not across the entire network.

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

Content originates on the owner's device and flows through relays to subscribers. Each relay independently forwards to all of its subscribers. Subscribers do not connect to owners or to each other - this provides better scalability than peer-to-peer SimpleX groups, where adding a member requires N new connections. When multiple relays serve the same channel, subscribers deduplicate at the client level.

**Failure modes:**

- **Loss of a relay is loss of a cache node, not loss of data.** The owner can send the same content through a replacement relay.

- **Loss of all owner devices is the catastrophic event** - relay caches become orphaned and the channel's private keys are gone. Multiple owners and backups mitigate this risk.

- **Disagreements between relays are resolved by the origin.** The owner's version is authoritative, settling cache inconsistency through any reachable relay.

Subscribers hold their own received copies. Signed messages are independently verifiable without consulting the relay or owner. Unsigned content depends on cross-relay consistency or future transcript integrity mechanisms.

### Identity and ownership

A channel's identity is the SHA-256 hash of the genesis root public key, computed at creation time and never changed - even if relays are added, removed, or the channel link is rotated. It is self-authenticating: derived from a key pair that only the channel creator held. It is embedded in the channel's link, distributed in the profile to all members, and used as a binding prefix in all signed messages.

Subscribers validate that the identity in the link matches the identity in the profile, preventing link substitution. Profile updates that attempt to change the identity are rejected. Full validation that the identity equals the hash of the root key is deferred: if current clients enforced this check, they would reject future rotated links as invalid. The identity is correctly managed today; validation will be enforced with the key rotation protocol. See the [group identity binding RFC](../rfcs/2026-03-28-group-identity-binding.md).

The root key does not sign messages directly. Instead, it authorizes owner keys through a signed chain. At creation, the owner generates a root key pair and a separate member key pair for signing. The member key is published as an authorization entry signed by the root key. New owners can be added by any previously authorized owner signing a new entry. Anyone retrieving the channel link can verify this chain without network access.

The root key is a bootstrap key - it certifies owners, then need not be used again. All owners are cryptographically indistinguishable to subscribers (they all have equally valid authorization chains), which - provided multiple owners were signed by the root key - conceals the creator's identity.

The channel link is the out-of-band trust anchor - relays and SMP routers cannot modify link content. All members announce their signing keys on joining. Owner keys are verifiable against the link. Role changes (promoting members to admin, moderator) are signed by owners at the protocol level.

A planned extension will record role changes as a linearly ordered signed roster log with consistent sequencing across all owners, relays, and subscribers. This linearization prevents ambiguous roster states from concurrent unordered changes, and creates a verifiable chain of trust from the channel link through owners to all elevated roles. Out-of-band key verification for non-owner members will further extend this to E2E encrypted conversations.

### Governance

"Management" in "information delivery and management" refers not only to managing content but to managing the channel itself - who can make decisions, and how.

The low-level protocol supports multiple owners from the initial release. The application-level governance model evolves through a planned progression:

**Current (v6.5): Single owner.** One owner controls the channel. All administrative actions (profile changes, roster modifications, relay management) are decided by this single owner. The protocol-level owners chain supports verification of multiple entries, but the application creates and manages only one.

**Near-term (v7): Multiple owners, any-owner-decides.** Multiple owners share control of the channel. Any owner can independently make any administrative decision - add or remove members, change the profile, manage relays. This is the most common decision-making model in practice (equivalent to "all admins are equal" in most online platforms). No coordination between owners is required for any action.

**Future: Multisig and programmable governance.** Further stages include M-of-N multisig for administrative actions and, eventually, programmable governance rules defined as code in the channel's definition. The protocol must support these without prescribing a specific governance model.

### Roles

- **Owners** create the channel, hold the authoritative state and private keys on their devices, publish content, and manage the member roster. Owners sign administrative messages and optionally content messages. A channel must have at least one owner.

- **Relays** receive content from owners and members with posting rights, optionally cache it, and forward it to subscribers. They accept new subscriber connections and introduce them to the channel owners. Relays cannot author messages. A channel must have at least one active relay. Relays are ordinary SimpleX clients - a relay can be operated by anyone (a channel operator, a third-party service provider, or a self-hosted instance) and each creates its own contact address link, bound to the channel's identity. The relay's relationship with the channel is transient - owners can add and remove relays without changing the channel's identity.

- **Subscribers** connect to relays and receive content. They cannot send messages by default, but can be given posting rights.

Additional roles (moderator, admin, member, author) exist in the hierarchy and are inherited from the group protocol. The owner-signed roster tracks the promoted set - members, moderators, and admins; subscribers are observers until an owner promotes them.

For protocol-level detail - wire formats, message types, signing and verification mechanics, delivery pipeline - see [SimpleX Channels Protocol](./channels-protocol.md).


## Cryptographic primitives

- **Ed25519** - channel identity (root key pair), owner authorization chain, and message signing. The signature binding prefix includes the channel's entity ID and the sender's member ID, preventing cross-channel replay.

- **SHA-256** - derives the channel's entity ID from the genesis root public key. Immutable, serves as the channel's permanent identity.

- **Double ratchet with post-quantum KEM** (inherited from [SimpleX agent layer](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - end-to-end encryption for all SMP transport. Not channel-specific - channels inherit it by being built on the agent layer. Future E2E side conversations (support scope, member DMs, private channels) will use the same mechanism.

Content messages are not signed by default to preserve cryptographic deniability - see [Signing scope](#signing-scope-roster-only-content-optional). Owners may opt into signing all content in a future release.


## Security

This section examines what the architectural properties protect against, where they hold, and where gaps remain.

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

By default, only roster-modifying and administrative messages are signed. Content messages are not signed. Two reasons:

1. **Cryptographic deniability.** Signing creates non-repudiable proof of authorship verifiable by any third party. Without signatures, no such proof exists - a relay could have fabricated any unsigned message.

2. **Proportional defense.** Changes to roster, channel profile, and permissions can be disruptive and irreversible - they must be authenticated at processing time. Content manipulation is detectable post-hoc through cross-relay consistency, and the authoritative record on the owner's device is unaffected.

Owners will be able to opt into signing content on a per-channel or per-message basis - some publishers want non-repudiable authorship, others prefer deniability.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and addresses threats specific to the channel layer.

**A single compromised relay**

*can:*

- Substitute unsigned content or selectively drop messages for its subscribers. Detectable by subscribers connected to other relays - the owner's version is authoritative. TODO: difference detection not yet implemented.
- Selectively target specific subscribers while delivering correctly to others.
- Ignore the "message from channel" directive, revealing which owner sent a message. Detectable out-of-band.
- Fabricate or hide subscriber connections, inflating or deflating counts. Detectable if subscribers are connected to other relays.
- Replay a previously valid roster - the owner-signed header plus its blob - to a *new* joiner, re-introducing a member who was later removed or demoted (the roster now carries plain members, not only moderators and admins). The owner signature binds the channel entity ID and the roster version, and the header's digest binds the blob to that header, so cross-channel and cross-version substitution remain blocked; but a same-group replay to a joiner that has not yet seen a newer version is not prevented. Existing members are protected by the monotonic roster version check - they reject any roster not strictly newer than the one already applied, so the replay reaches only a joiner with no prior roster state, and only until that joiner receives the current roster from another relay it connects to.
- Replay or reorder a genuine owner-signed role change (`x.grp.mem.role`). Role changes reach existing subscribers on this signed event - which carries the member's owner-pinned key and the roster version - rather than via a relay re-broadcast of the roster blob (the blob is served only to joiners and resumers; the relay no longer broadcasts it to subscribers on a change). The relay cannot forge the event, and the roster version carried on it - applied only if not lower than the subscriber's current version, then advanced - blocks re-elevating a **demoted or removed** member, because the demotion (`x.grp.mem.role` to observer) and the privileged removal (`x.grp.mem.del`, below) each carry a strictly higher version. A dropped or reordered event is otherwise no worse than withholding it: resume-serve re-snapshots the subscriber from the owner-signed blob.
- Re-elevate a **removed** member by replaying an earlier role change. A privileged removal (`x.grp.mem.del`) carries the roster version, so after `promote cath (Vn)` then `remove cath (Vn+1)` a subscriber is at `Vn+1` and a replayed `x.grp.mem.role(cath -> admin, Vn)` is rejected (`Vn < Vn+1`). The version is contiguous across the roster-affecting changes - promotions, demotions, and privileged removals - and the owner's blob excludes the removed member, so the removal also reaches new joiners.

*cannot:*

- Undetectably substitute content - subscribers on honest relays receive the original.
- Alter the channel's authoritative state on the owner's device.
- Substitute the channel profile or impersonate an owner - these require valid signatures.
- Redirect subscribers to a different channel - the entity ID is validated across link and profile.
- Determine subscriber identity or network address - inherited from SMP transport.
- Correlate subscriber participation across channels - each connection uses independent SMP queues. The subscriber chooses their SMP router independently, so collusion between a relay and the relay's SMP router does not compromise connections through a different router.

**All relays compromised and colluding**

*can:*

- Undetectably substitute unsigned content for all subscribers, unless owners sign content messages.
- Prevent delivery of any messages, including signed ones (signing prevents substitution, not dropping).
- Fabricate or hide subscriber connections undetectably.

*cannot:*

- Forge signed administrative messages or substitute the channel profile.
- Alter the authoritative state on the owner's device.

**Compromise of owner keys**

An attacker who obtains the root private key or an owner's member private key (through device compromise, backup theft, or coercion) can impersonate the owner and sign arbitrary administrative messages. This is a different threat from key loss - the channel continues operating, but under adversarial control. Mitigation depends on owner-side operational security and future multisig governance. For the threat model of the channel link itself (the trust anchor), see the [short links for groups RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2025-04-04-short-links-for-groups.md).

**Loss of all owner devices**

The channel can have no new content, no administrative updates, no new owners. Relay caches continue delivering existing content but cannot be refreshed, and will eventually expire in the absence of the owner connection. Multiple owners and key backups mitigate this risk.

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

- Determine which channel a subscriber uses, correlate channel activity with other SimpleX activity, or identify a relay as distinct from an ordinary user, other than by traffic volume. Inherited from SMP transport.

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
- **Private channels** where the entire content stream is encrypted to authorized subscribers. The relay becomes a conduit that sees neither content nor identity.

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

The relay loads link previews on behalf of the sender - it already sees message content, so it learns nothing new, and unlike the sender its IP is not linked to any identity.


## Conclusion

SimpleX Channels enable a publisher to reach an unlimited audience without any infrastructure operator knowing who that audience is. No third party can seize the channel because owners hold the keys and the authoritative state on their own devices - relays only cache and forward. Owner signatures protect content integrity and the trust chain extends to all administrative roles. These properties require a network without participant identifiers - they cannot be added to a system that has them.
