Revision 2, 2026-04-28

Evgeny Poberezkin

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
- [Protocol](#protocol)
  - [Channel creation](#channel-creation)
  - [Relay addition](#relay-addition)
  - [Subscriber connection](#subscriber-connection)
  - [Message signing](#message-signing)
  - [Message forwarding](#message-forwarding)
  - [Binary batch format](#binary-batch-format)
  - [Delivery pipeline](#delivery-pipeline)
  - [Message deduplication](#message-deduplication)
  - [Channel-as-sender messages](#channel-as-sender-messages)
  - [Member support scope](#member-support-scope)
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

#### What are SimpleX Channels

SimpleX Channels are a stateful information delivery and management layer built on the [SimpleX network](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md). Where SMP queues provide stateless, unidirectional packet delivery between two endpoints, channels add persistence, state, and scalable distribution, enabling one-to-many publishing with cryptographic identity that is independent of the infrastructure operators.

[SimpleX Chat](https://simplex.chat) is the first application using channels. It presents them as a broadcast publication model similar to Telegram channels, where owners publish and subscribers read, add reactions and comment. But channels are not limited to this use case. They are a general-purpose layer for any application that needs to distribute and manage stateful information across a set of participants (feeds, telemetry collection, automated pipelines, coordination services, social media and other consumer-facing applications), using the same delivery and management mechanisms.

The critical difference from conventional publish-subscribe systems is that channel identity and governance are controlled cryptographically by the channel owners, not by the infrastructure operators. Relays - the network nodes that forward and optionally cache channel content - can be added, removed, and replaced without changing the channel's identity, address, content, or cryptographic trust chain. A channel's relationship with its relays is transient; a channel's identity is permanent. This is unlike a website, whose identity (domain name) is controlled by a hosting provider and registrar. It is more like a set of owners' cryptographic keys that happen to have content distribution attached to it, where the authoritative record of this content is hosted on channel owners devices, and relays perform transmission and optional caching function similar to CDN infrastructure.

The channel owners hold full control of the channel - its identity, content, governance rules, and membership - through self-custody of cryptographic keys. No infrastructure operator, relay provider, or third party can freeze, seize, revoke, or alter a channel without the owner's keys. This is the same property that blockchain systems achieve for financial assets through network-wide consensus, but channels achieve it for information management through local authority and cryptographic signatures - without the cost of global consensus of a public ledger, and without sacrificing the privacy of participants.

#### Channels as transport layer

The SimpleX network has three transport layers, each built on the one below:

1. **SMP** ([SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md)) - stateless, unidirectional packet delivery between two endpoints through SMP routers. Provides fixed-size blocks, 2-hop onion routing, and transport metadata protection. No user identifiers exist at this layer.

2. **SimpleX agents** ([agent protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - bidirectional, redundant connections between endpoints, with end-to-end post-quantum double ratchet encryption. The [SimpleX Chat Protocol](./simplex-chat.md) runs on top of this layer, providing direct messaging, group communication, and file transfer.

3. **Channels** - stateful, one-to-many information delivery and management with cryptographic ownership and programmable governance. This layer runs on top of chat and agent layer 2, and it is described in this document.

SMP provides point-to-point delivery. Chat protocol provides conversations. Channels provide publication, distribution, and management of stateful information. Just as SMP enables private messaging by providing transport without user identifiers, channels enable public communication while preserving the same privacy properties at the distribution layer.

The crucial architectural consequence of this layering is that channel relays are themselves SimpleX clients in the SMP network. A relay connects to SMP routers using the same protocol, the same 2-hop onion routing, and the same fixed-size transport blocks as any other SimpleX endpoint. Even though the SMP network can distinguish a relay from a person's phone by its transport patterns, it prevents relays from learning anything about other network endpoints.

This means that channels inherit all of SMP's transport privacy properties:

- **Relays cannot observe subscriber network addresses.** A subscriber connects to a relay through SMP queues on SMP routers. The relay sees the SMP queue address, not the subscriber's IP address or network session. The subscriber's IP is known only to the SMP router they connect to, which in turn cannot see the message content (it is encrypted at the agent layer).

- **SMP routers cannot see channel content.** Messages between relay and subscriber are end-to-end encrypted at the agent layer. The SMP router forwards fixed-size encrypted blocks without knowing whether they contain channel messages, direct messages, or anything else.

- **Participation in multiple channels is unlinkable.** If a subscriber participates in multiple channels served by the same relay, the relay cannot determine this - each channel connection uses independent SMP queues with separate cryptographic credentials. The same is true at the SMP layer: if a subscriber's connections to different relays happen to pass through the same SMP router, the router cannot link them.

No single point in the system sees both content and network identity. SMP routers see network addresses but not content. Relays see content but not network addresses. This separation is not a feature added to the channel layer - it is a structural consequence of channels being built on top of SMP transport, where relays are ordinary clients rather than privileged infrastructure. In fact, relays don't even have a separate codebase - any CLI client can act like chat relay without any modifications.

#### Content visibility and participant privacy

This transport layering produces a specific combination of properties for public communication that is not present in any other publishing system.

Because channel content is not end-to-end encrypted between owner and subscriber (it is encrypted at the SMP transport layer, but relays decrypt and re-encrypt as they forward), relay operators can read the messages they handle. This is a deliberate choice. Relay operators must be able to see what they distribute in order to moderate it, comply with law where required, and maintain the quality of their service.

Because the SMP transport below carries no user identifiers, and relays are ordinary SMP clients, subscribers connect to channels without revealing who they are. The relay sees subscriber profiles, but those profiles link to no real identity - no phone number, no username, no public key that persists across channels - and no network address or session information.

This is the inverse of what every other public communication system provides. Conventional systems encrypt content so the operator cannot see it, and identify participants so the operator can route to them. SimpleX Channels make content visible and participants invisible. The inversion is not a design choice that could have been made differently - it follows from the fact that the SimpleX network has no user identifiers. Content visibility is a consequence of relays being forwarding nodes (they must see what they forward). Participant invisibility is a consequence of the SMP transport below (it has no identifiers to reveal).

The practical result: an adversary can join a channel as a subscriber and see everything that was said. They cannot determine who said it. They cannot link any participant to anything outside the channel. Meanwhile, the relay operator can see what they host, declare a content policy, respond to complaints by inspecting content, and cooperate with law enforcement where legally required, without breaking any promise, because the promise was about participants, not about content.

#### In comparison

SimpleX Channels occupy a distinct position in the design space of public and semi-public communication systems.

**Telegram channels** provide scalable one-to-many delivery with rich content management. The operator controls channel identity (usernames are revocable), has full access to both content and participant identity, and the infrastructure is centralized. Censorship operates through username seizure, channel banning, or operator cooperation. Channels cannot exist without Telegram's permission.

**Nostr relays** share the property of cryptographic author identity - a Nostr publisher's identity is a key pair, independent of any relay. But Nostr's architecture requires publishers to sign every message, making all content non-repudiable. Relays see both content and the publisher's persistent public key, which links all their activity across every relay. Subscribers must trust relay operators not to fabricate or withhold messages, because Nostr has no built-in mechanism for cross-relay consistency verification. Nostr also does not protect subscriber identity - relay operators see which public keys subscribe to which content.

**Signal groups** use the MLS (Messaging Layer Security) protocol for end-to-end encrypted group communication. Content is encrypted; the operator cannot see it. But the operator runs the Authentication Service that MLS requires, meaning participants must trust the operator for the encryption to mean anything. Group membership is visible to the operator. The operator can add members to groups (because the Authentication Service controls identity). Groups do not scale beyond several thousand members, and there is no concept of a channel - only symmetric groups where every member can speak.

**Matrix rooms** provide federated, encrypted group communication. Server operators see room membership and metadata. In encrypted rooms, content is end-to-end encrypted, but the key distribution depends on server cooperation. Federation means multiple operators see membership. Room identity is bound to the creating server's domain. If the server disappears, the room identity is lost.

**Mastodon / ActivityPub** provides federated public posting. Content is public by design. Publisher identity is bound to a server domain - if the server disappears, the identity is lost. Server operators see all content and all follower relationships. There is no encryption of any kind. Federation provides some censorship resistance (you can move servers), but identity portability is limited - followers must re-follow you on the new server.

SimpleX Channels make a different set of trade-offs:

| Property | Telegram | Nostr | Signal | Matrix | Mastodon | SimpleX |
|---|---|---|---|---|---|---|
| Content visible to operator | Yes | Yes | No | Configurable | Yes | Yes |
| Participant identity visible to operator | Yes | Yes | Yes | Yes | Yes | **No** |
| Channel identity independent of infrastructure | No | Yes | N/A | No | No | **Yes** |
| Sovereign ownership (no third party can seize) | No | Yes | No | No | No | **Yes** |
| Programmable governance | No | No | No | No | No | **Planned** |
| Cross-relay consistency verification | N/A | No | N/A | N/A | N/A | **Yes** |
| Content deniability (no proof of authorship) | No | No | Yes | Yes | No | **Yes (default)** |
| Scalable one-to-many delivery | Yes | Yes | No | Limited | Yes | **Yes** |
| Relay operator can moderate | Yes | Yes | No | Partial | Yes | **Yes** |


## Architecture

#### State and distribution

The most important architectural property of SimpleX Channels is where authoritative state lives: on the owner's devices, not on relays, not on any server, and not on any shared ledger.

The authoritative record of a channel - its content history, member roster, profile, cryptographic keys, and governance rules - is held by channel owners on their own devices. No intermediary holds it, and no intermediary can alter it. Relays hold transient copies of this state for the purpose of distribution and optional caching. The relationship is analogous to origin servers and CDN edge nodes: the origin holds the truth, and the CDN distributes copies. CDN nodes come and go; the origin persists.

Where blockchain systems achieve sovereignty over shared state through network-wide consensus (every node validates every transaction), channels achieve it through cryptographic authority - the owner's signature is the only proof needed. Consensus is only required between channel owners, not across the entire network. This makes channels dramatically cheaper to operate, faster to respond, and private by default - the network does not need to know about the channel's existence, let alone validate its transactions.

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

Subscribers do not connect to owners or to each other - all communication passes through relays. This star topology provides scalability (adding subscribers requires only a new relay connection, not N connections to N existing members), subscriber privacy (a subscriber's connection metadata is known only to the relay it connects to), and moderate load on owners (each message is sent once per relay, not once per subscriber).

**Consequences of this data architecture:**

- **Loss of a relay is loss of a cache node, not loss of data.** If a relay disappears or is removed, the owner can send the same content through a replacement relay. No content is permanently lost. Subscribers experience temporary disruption, not data loss.

- **Loss of all owner devices is the genuinely catastrophic event.** It is the equivalent of losing the origin server with no backup. All relay caches become orphaned - they hold copies but can no longer receive new content or signed administrative updates. The channel's private keys are gone, so no new owners can be authorized and no signed messages can be produced. This is the real single point of failure in the architecture.

- **Relays do not hold authoritative state.** A relay's local database is a delivery queue and optional content cache, not the channel's database. When a relay persists delivery tasks and jobs, that persistence serves delivery reliability - the ability to resume forwarding after a crash - not authoritative storage.

- **Disagreements between relays can be resolved by the origin.** If two relays deliver different content for the same message, the owner can serve as the authoritative tiebreaker (through any reachable relay). This is stronger than probabilistic majority-of-honest-relays reasoning: the origin settles cache inconsistency.

Subscribers hold their own received copies. For signed messages, these copies are independently verifiable - a subscriber can confirm that a roster change was signed by a legitimate owner without consulting the relay or the owner. For unsigned content messages, verification depends on cross-relay consistency or future transcript integrity mechanisms.

#### Identity and ownership

A channel's identity is a cryptographic value derived from the key pair generated at creation time:

```
entityId = sha256(genesisRootPubKey)
```

This entity ID is set at creation and never changes, even if relays are added, removed, or the channel link is rotated. It is self-authenticating - derived from a key pair that only the channel creator held at creation time. It is stored in the immutable part of the channel's short link data (`linkEntityId` in `FixedLinkData`), included in the channel's group profile (`publicGroupId` in `PublicGroupProfile`) distributed to all members, and used in the signature binding prefix for all signed messages.

Current clients validate that the entity ID from the link matches the entity ID in the group profile (preventing link substitution), and reject profile updates that attempt to change it. Full validation that `entityId == sha256(rootKey)` is deferred to a future protocol version that includes key rotation. See the [group identity binding RFC](../rfcs/2026-03-28-group-identity-binding.md) for the rationale.

**Owner authorization chain.** Channel ownership is not tied to the root key directly. Instead, the root key authorizes owner keys through a chain:

1. At creation, the owner generates a **root key pair** (Ed25519). The public key is embedded in the channel link's immutable fixed data. The private key is retained only by the creator.

2. The owner generates a separate **member key pair** for signing messages. The public key is published as an `OwnerAuth` entry, signed by the root key:

   ```
   OwnerAuth {
     ownerId    = memberId,          -- application-specific identifier
     ownerKey   = memberPubKey,      -- Ed25519 public key for message signing
     authOwnerSig = sign(rootPrivKey, ownerId || encodePubKey(ownerKey))
   }
   ```

3. The `OwnerAuth` entry is stored in the channel link's mutable user data. Anyone retrieving the link can verify that the owner's signing key was authorized by the root key.

4. Future owners can be added by having an existing authorized key sign a new `OwnerAuth` entry. This creates a chain: root -> owner1 -> owner2 -> ... Each link in the chain is verifiable without network access.

This model separates the channel's permanent identity (the root key hash) from the signing keys used for day-to-day operations. The root key is a bootstrap key - it certifies owners, then need not be used again. All owners are cryptographically indistinguishable to subscribers (they all have equally valid authorization chains), which conceals the creator's identity.

#### Governance

"Management" in "information delivery and management" refers not only to managing content but to managing the channel itself - who can make decisions, and how.

The low-level protocol supports multiple owners from the initial release. The application-level governance model evolves through a planned progression:

**Current (v6.5): Single owner.** One owner controls the channel. All administrative actions (profile changes, roster modifications, relay management) are decided by this single owner. The protocol-level `OwnerAuth` chain supports multiple entries, but the application creates and manages only one.

**Near-term (v7): Multiple owners, any-owner-decides.** Multiple owners share control of the channel. Any owner can independently make any administrative decision - add or remove members, change the profile, manage relays. This is the most common decision-making model in practice (equivalent to "all admins are equal" in most online platforms). No coordination between owners is required for any action.

**Future: Multisig.** Administrative actions require approval from M of N owners. This prevents unilateral action by a single compromised or rogue owner - for example, requiring 2-of-3 owners to approve channel deletion or ownership transfers. The design space here is broader than simple thresholds: different action types may require different quorums, and different owners may hold different voting weight.

**Future: Code-based channel articles.** The governance model itself becomes programmable - defined as code that is part of the channel's definition. Channel articles can encode arbitrary management structures: shareholding with weighted votes, executive powers for specific roles, hierarchical approval chains, term limits, or any other governance model that can be expressed as rules over the owner authorization chain. This is the most general form - multisig is a special case, and single-owner is a degenerate case.

This is analogous to what smart contracts achieve for on-chain governance, but without requiring a blockchain. A smart contract encodes governance rules that are enforced by network-wide consensus - every node on the network validates every governance action, which is expensive, public, and slow. Channel articles encode the same kinds of rules, but they are enforced by the channel's owners and verified by its subscribers - the only parties who need to care. The governance scope is the channel, not the network. This makes programmable governance practical for real-time communication, where blockchain-based governance would introduce unacceptable latency and cost.

This progression matters because as channels grow beyond a single publisher's personal project into shared institutions - media organizations, communities, cooperatives - the governance model becomes the channel's constitution. The protocol must support this without prescribing it.

#### Roles

A channel has three classes of participant, distinguished by their role in the member hierarchy:

- **Owners** (`GROwner`) create the channel, control its identity and profile, manage the member roster, publish content, and hold the authoritative state on their devices. A channel must have at least one owner. Owners hold the private keys needed to sign administrative messages.

- **Relays** (`GRRelay`) are distribution agents. They receive content from owners, cache it, and forward it to subscribers. They accept connection requests from new subscribers and introduce them to the channel. Relays cannot author messages of their own - their role is below `GRObserver` in the permission hierarchy. They hold no authoritative state - only cached copies and delivery queues. A channel must have at least one active relay.

- **Subscribers** (`GRObserver` by default) connect to relays and receive content. They cannot send messages to the channel by default.

Additional roles (`GRModerator`, `GRAdmin`, `GRMember`, `GRAuthor`) exist in the role hierarchy and are inherited from the group protocol. In the current channel implementation, subscribers are assigned `GRObserver` role by default, which permits receiving all messages but not sending.

**Relay model.** Relays are SimpleX Chat clients, not special-purpose servers. They run the same client code as any other participant, with a configuration flag that enables relay behavior (auto-accepting group join requests, forwarding messages, creating relay links). A relay can be operated by anyone - a channel operator, a third-party service provider, or a self-hosted instance.

Each relay holds:

- A **relay link** - a SimpleX contact address that subscribers use to connect to the channel through this relay. The relay link's immutable data includes the channel's entity ID, binding it to a specific channel.

- **Connections to the owner(s)** via SMP queues, over which it receives content to forward.

- **Connections to all subscribers** it serves, over which it forwards cached content.

- A **delivery queue** - persistent task and job records that ensure forwarding can resume after a crash. This is operational state for delivery reliability, not authoritative content storage.

- Optionally, a **content cache** - recent messages retained to serve history to new subscribers. The depth and retention policy of this cache is a relay operational decision, not a protocol requirement.

The relay's relationship with the channel is transient. The owner can add new relays and remove existing ones without changing the channel's identity or its address. When a relay is removed, it forwards the deletion event to its subscribers and shuts down. Subscribers who were connected to the removed relay can restore their connection by retrieving the channel's link data, which lists the current set of relay links, and connecting to a remaining relay.


## Protocol

This section describes the protocol as currently implemented. It builds on the [SimpleX Chat Protocol](./simplex-chat.md), using the same message format and connection model, with extensions for relay-mediated distribution and cryptographic message signing.

#### Channel creation

Creating a channel involves generating cryptographic material, creating the channel link, and connecting relay members:

1. **Key generation.** The owner generates an Ed25519 root key pair. The entity ID is computed as `sha256(rootPubKey)`. A separate member key pair is generated for message signing, and an `OwnerAuth` entry is created, signed by the root key.

2. **Link creation.** The owner calls the agent's `prepareConnectionLink` API with the root key pair and entity ID. This returns a prepared link (including a `ConnShortLink` address) without any network calls. The link address is deterministic, derived from the fixed data hash, so it can be embedded in the group profile immediately.

3. **Link data upload.** The owner calls `createConnectionForLink`, which makes a single network call to create the SMP queue and upload the encrypted link data. The link's fixed data contains the root public key and connection request. The mutable user data contains the `OwnerAuth` array, the channel profile (including the entity ID and the link itself), and the initial subscriber count.

4. **Relay invitation.** For each selected relay, the owner sends a contact request containing an `XGrpRelayInv` message with the channel's short link. The relay retrieves the link data, validates the channel profile, creates its own relay link (with the channel's entity ID in its immutable data), and responds with `XGrpRelayAccept` containing its relay link.

5. **Link update.** As each relay accepts and provides its relay link, the owner validates that the relay link contains the correct entity ID, then adds the relay link to the channel link's mutable data.

6. **Local record.** The channel is stored on the owner's device as a group with `useRelays = True`, `groupKeys` containing the root private key and member private key, and a `PublicGroupProfile` in its profile. This local record is the authoritative state of the channel.

#### Relay addition

The relay addition protocol ensures that relays are cryptographically bound to the correct channel:

1. Owner sends `XGrpRelayInv` to the relay's contact address. This message includes the relay's member ID and role, the owner's profile, and the channel's short link.

2. Relay receives the invitation and creates a relay request record. A relay request worker processes it asynchronously.

3. The worker retrieves the channel's link data from the SMP server, extracts and validates the channel profile and owner authorization.

4. The relay creates its own contact address link (the relay link) with the channel's entity ID as `linkEntityId` in the immutable fixed data.

5. The relay accepts the owner's connection request, sending its relay link in the acceptance.

6. The owner retrieves the relay link data, validates that `linkEntityId` matches the channel's entity ID, and adds the relay link to the channel link's user data.

Once active, the relay periodically retrieves the channel link data to verify that its relay link is still listed, as a monitoring mechanism to detect removal.

#### Subscriber connection

A subscriber joins a channel through the following flow:

1. **Link retrieval.** The subscriber scans or receives the channel's short link. The client retrieves the link data, which contains the channel profile, owner authorization chain, and list of relay links.

2. **Relay link resolution.** For each relay link listed, the client resolves the `ConnectionRequestUri` from the relay's short link.

3. **Async connection.** The client initiates asynchronous connections to all relays. Each connection sends an `XInfo` message with the subscriber's profile (or an incognito profile, created once and shared with all relays).

4. **Relay acceptance.** Each relay accepts the connection, creates a member record for the subscriber with the configured subscriber role (default `GRObserver`), and sends an `XGrpLinkInv` message with the channel profile and group link invitation data.

5. **Introduction.** The relay introduces the new subscriber to the channel's moderators and owners by sending an `XGrpMemNew` message. It also sends moderator/owner profiles to the subscriber.

6. **History.** If the channel has history sharing enabled, the relay sends recent cached history to the new subscriber.

The subscriber is functional (can receive messages) as soon as at least one relay connection succeeds. Additional relay connections provide redundancy and cross-relay consistency checking.

#### Message signing

Messages that alter the channel's roster, profile, or administrative state are cryptographically signed by the sending owner. Content messages are not signed by default; see [Signing scope](#signing-scope-roster-only-content-optional) for the rationale.

**Which messages require signatures:**

| Message | Description | Signed |
|---|---|---|
| `x.grp.del` | Delete channel | Required |
| `x.grp.info` | Update channel profile | Required |
| `x.grp.prefs` | Update channel preferences | Required |
| `x.grp.mem.del` | Remove member | Required |
| `x.grp.mem.role` | Change member role | Required |
| `x.grp.mem.restrict` | Restrict member | Required |
| `x.grp.leave` | Leave channel | Required |
| `x.info` | Update member profile | Required |
| `x.msg.new` | Content message | Not signed |
| `x.msg.update` | Edit message | Not signed |
| `x.msg.del` | Delete message | Not signed |

**Signing process:**

The signing context binds the signature to a specific channel and sender:

```
bindingPrefix = smpEncode(CBGroup) <> smpEncode(publicGroupId, memberId)
signedBytes   = bindingPrefix <> messageBody
signature     = Ed25519.sign(memberPrivKey, signedBytes)
```

The binding prefix includes the chat binding tag (`"G"` for group), the channel's entity ID, and the sender's member ID. This prevents cross-channel and cross-member replay attacks - a signature valid in one channel cannot be reused in another.

**Verification process:**

When a subscriber receives a signed message:

1. The signature is present: reconstruct the binding prefix from the channel's stored entity ID and the sender's member ID. Verify all signatures against the sender's stored public key. If all verify, the message status is `MSSVerified`.

2. The signature is present but the sender's key is unknown: the message is accepted with status `MSSSignedNoKey` only if the event does not require a signature. For `XGrpLeave` and `XInfo` between subscribers whose keys haven't been exchanged yet, `unverifiedAllowed` permits acceptance as a temporary measure.

3. No signature is present: the message is accepted only if the event does not require a signature (i.e., the channel does not use relays, or the event is a content message).

If verification fails for a message that requires a signature, the message is rejected and a `RGEMsgBadSignature` event is shown to the user.

#### Message forwarding

Content originates on the owner's device and flows through relays to subscribers. The forwarding mechanism preserves the original message bytes, including any signature, without re-encoding:

**Owner to Relay:** The owner sends messages directly to each relay over their SMP connection. Messages are encoded in binary batch format.

**Relay processing:** When a relay receives a message from an owner, it:

1. Parses and processes the message locally (updating its cached state, e.g. for roster changes).
2. If the relay is a forwarding relay (`isUserGrpFwdRelay`), creates a **delivery task** for each message that should be forwarded to subscribers. The task records the message ID, the sender's member ID, the broker timestamp, and whether the message was sent as the channel (not attributed to a specific owner).
3. The delivery task is persisted to the database for delivery reliability - ensuring forwarding can resume after a relay crash.

**Relay to Subscribers:** A delivery task worker reads pending tasks, batches them into delivery jobs, and a delivery job worker sends each job to subscribers in paginated batches (using a cursor over group member IDs).

For forwarded messages from subscribers to owners (e.g. support scope messages), the relay wraps the message in a forwarding envelope:

```
forwardEnvelope = ">" <> smpEncode(GrpMsgForward) <> encodeBatchElement(signedMsg, msgBody)
```

This preserves the original message's signature bytes verbatim.

#### Binary batch format

Channels use a binary batch format that preserves exact message bytes for signature verification. This is distinct from the JSON array batching used by regular groups.

```abnf
binaryBatch    = %s"=" elementCount *batchElement
elementCount   = 1*1 OCTET                    ; 1-255 elements
batchElement   = elementLen elementBody
elementLen     = 2*2 OCTET                    ; 16-bit big-endian length

elementBody    = signedElement / forwardElement / plainElement / fileElement

signedElement  = %s"/" chatBinding sigCount *msgSignature jsonBody
forwardElement = %s">" grpMsgForward (signedElement / plainElement)
plainElement   = %s"{" *OCTET                 ; JSON message body
fileElement    = %s"F" *OCTET                 ; binary file chunk

chatBinding    = 1*1 OCTET                    ; "G" (group), "D" (direct), "C" (channel)
sigCount       = 1*1 OCTET                    ; number of signatures (1-255)
msgSignature   = keyRef sigBytes
keyRef         = %s"M"                        ; member key reference
sigBytes       = 64*64 OCTET                  ; Ed25519 signature

grpMsgForward  = fwdSender brokerTs
fwdSender      = memberFwd / channelFwd
memberFwd      = %s"M" memberId memberName    ; attributed to specific member
channelFwd     = %s"C"                        ; attributed to channel as sender
brokerTs       = 8*8 OCTET                    ; UTC system time
```

The parser (`parseChatMessages`) dispatches on the first byte:

- `'='` -> binary batch (new format, used by channels)
- `'X'` -> compressed (decompress, then re-parse)
- `'['` -> JSON array (legacy group format)
- `'{'` -> single JSON message

Forward elements contain the original message bytes verbatim. The relay does not re-encode the inner message. This is what makes signature verification possible after forwarding: the exact bytes that were signed by the owner are preserved through the relay.

Nested forwarding (`>` inside `>`) is explicitly rejected by the parser.

#### Delivery pipeline

The relay's delivery pipeline has two stages, both backed by persistent database tables for delivery reliability (not for authoritative storage - the relay's database is a delivery queue, not a content database):

**Stage 1: Delivery tasks.** When the relay receives a message from an owner that should be forwarded, it creates a `delivery_task` record:

```
delivery_task:
  group_id, worker_scope, job_scope,
  sender_group_member_id, message_id,
  message_from_channel (bool),
  task_status (new -> processed)
```

A **task worker** (one per group per scope) reads pending tasks, batches multiple tasks into a single binary batch body, and creates a delivery job.

**Stage 2: Delivery jobs.** A delivery job contains the pre-encoded batch body and a cursor for paginated delivery:

```
delivery_job:
  group_id, worker_scope, job_scope,
  body (pre-encoded binary batch),
  cursor_group_member_id,
  job_status (pending -> complete)
```

A **job worker** reads the body and delivers it to subscribers in paginated batches. For each page, it loads a bucket of subscribers by cursor position, sends the body to all of them, advances the cursor, and continues until all subscribers have been served. This avoids loading all subscribers into memory at once.

For subsequent subscribers in a batch, the agent uses `VRRef` (value reference) to reference the first subscriber's message body, avoiding redundant data transmission to the SMP server.

#### Message deduplication

When multiple relays serve the same channel, each subscriber receives the same message from each relay independently. Deduplication is performed at the subscriber's client level using the message's `SharedMsgId`:

- When saving a received message, the client checks whether a message with the same `SharedMsgId` already exists for this group.
- If a duplicate is found, the message is silently dropped (in channels with relays). The `SEDuplicateGroupMessage` error is caught and suppressed.
- In non-relay groups, duplicate detection triggers a `XGrpMemCon` notification to the forwarding member.

This is essentially cache coherence verification - comparing what was received from one cache node against another. TODO: Currently, deduplication only detects the presence of duplicates. The protocol design includes provisions for detecting differences between relay-delivered copies of the same message (hash comparison, UI indicators for discrepancies). This is described in the [channels forwarding RFC](../rfcs/2025-08-11-channels-forwarding.md) and is not yet implemented.

#### Channel-as-sender messages

Owners can send messages attributed to the channel rather than to themselves. When `asGroup = True` is set in the message container, the relay forwards the message with `FwdChannel` instead of `FwdMember memberId memberName`. On the subscriber side, such messages are displayed as coming from the channel (using the channel's profile image and name) rather than from a specific owner.

This is useful for channels with multiple owners where the identity of the specific sender should not be visible to subscribers. The relay must respect this directive; ignoring it and revealing the sending owner's identity is a threat vector (detectable out-of-band by members communicating with the owner).

The forwarding binding prefix for channel-as-sender messages uses `CBChannel` instead of `CBGroup`, and includes only the channel's entity ID (not the sender's member ID):

```
channelBinding = smpEncode(CBChannel) <> smpEncode(publicGroupId)
```

#### Member support scope

Channels support a **member support scope** - a private side-channel between a subscriber and the channel's moderators/owners. Messages sent in the support scope are delivered only to moderators and the scoped subscriber, not to all subscribers.

This is implemented through the `MsgScope` type:

```
MSMember { memberId :: MemberId }
```

When a message has a support scope, the delivery pipeline uses a `DJSMemberSupport` job scope instead of `DJSGroup`. The job worker loads only the scoped member and moderators (members with role >= `GRModerator`), rather than all subscribers.

Support scope messages are visible only to the subscriber who initiated the support conversation and to the channel's moderators. Other subscribers cannot see them. This allows subscribers to report issues, appeal moderation decisions, or communicate with administrators without revealing their identity to other subscribers.


## Security

#### Design objectives

The channel protocol is designed to achieve the following security objectives:

1. **Stable message delivery** between channel participants, resilient to individual relay failures.
2. **No possibility for a relay to substitute the channel** - the channel's identity is cryptographically bound to the link and profile.
3. **No possibility for a relay to impersonate an owner** - administrative messages require valid signatures.
4. **Prevention of relay-initiated roster manipulation** - member removal, role changes, and other roster modifications require valid owner signatures.
5. **Relay transience** - the owner can add and remove relays, including the last relay, without permanently losing the channel. Subscribers can restore connectivity by retrieving updated link data.
6. **Sender anonymity within multi-owner channels** - owners can publish as the channel, hiding which specific owner authored a message.
7. **Participant privacy** - relay operators cannot determine subscriber identity or network address, and subscribers cannot determine each other's identity. This is inherited from the SMP transport layer.

#### Signing scope: roster only, content optional

By default, only roster-modifying and administrative messages are signed. Content messages (`XMsgNew`, `XMsgUpdate`, `XMsgDel`, etc.) are not signed. This is a deliberate design choice for two reasons:

1. **Deniability.** Signing content creates non-repudiable proof of authorship. Anyone with the message bytes could prove who wrote a specific message. This is antithetical to SimpleX's privacy model, where communications should be deniable.

2. **Proportional defense.** Roster and profile changes are disruptive and irreversible - a member removed, a role changed, a channel deleted. By the time a relay forgery is detected, the damage is done. These must be authenticated at processing time. Content manipulation by a relay is detectable post-hoc through cross-relay consistency (when multiple independent relays forward the same content). Content delivery is not irreversible - a forged message can be flagged and corrected, and the authoritative record on the owner's device is unaffected.

A future protocol extension will allow owners to opt into signing all messages, including content. This will be a per-channel choice - some publishers want non-repudiable authorship (similar to Nostr), others prefer deniability. The protocol supports this by making `requiresSignature` a function of the event tag and the channel's configuration.

#### Threat model

This threat model assumes the SimpleX network threat model (see [SimpleX Network Security](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md)) and addresses threats specific to the channel layer.

**A single compromised relay (or all relays colluding)**

*can:*

- Serve fabricated content to subscribers by injecting arbitrary unsigned messages (regular content, reactions, edits) that do not require signature verification. The channel identity and signed profile remain intact, but the content stream as seen by subscribers can be fabricated. The authoritative content on the owner's device is unaffected.

- Selectively drop messages, both content and administrative events, for some or all subscribers. This includes dropping signed roster changes, effectively blocking administrative actions from reaching subscribers. The owner's authoritative state still reflects the change, but subscribers don't see it until they receive it through another relay or out-of-band.

- Selectively drop messages for specific subscribers while delivering correctly to others.

- Ignore the "message from channel" directive from the owner, revealing which specific owner sent a message to subscribers. This is detectable out-of-band.

- Fabricate new subscriber connections, inflating subscriber counts and potentially increasing costs for the owner.

*cannot:*

- Alter the channel's authoritative state - the owner's device holds the truth, and the relay has no access to it.

- Substitute the channel profile - profile changes require a valid owner signature that the relay cannot produce.

- Impersonate an owner for any message that requires a signature (roster changes, profile updates, channel deletion).

- Redirect a joining subscriber to a different channel - the entity ID in the relay link's immutable data is validated against the channel link's entity ID.

- Determine the real-world identity or network address of subscribers - subscriber connections pass through the SMP transport layer, which provides 2-hop onion routing and carries no user identifiers.

- Correlate a subscriber's participation across multiple channels, even channels served by the same relay - each channel connection uses independent SMP queues with separate cryptographic credentials.

**A compromised relay when other relays are not compromised**

*can:*

- Serve different content to its own subscribers than what other relays deliver, but this discrepancy is detectable by subscribers connected to multiple relays. If the owner is reachable through an honest relay, the owner's version is authoritative. TODO: Difference detection and highlighting are designed but not yet implemented.

- If the number of compromised relays equals the number of honest relays, create ambiguity about which relays are compromised - subscribers cannot determine which version of a message is genuine without checking against the owner (through a trusted relay) or using future transcript integrity mechanisms.

*cannot:*

- Forge signed administrative messages - subscribers verify these against the owner's public key regardless of which relay delivered them.

**Loss of all owner devices**

*causes:*

- Permanent loss of the channel's private keys. No new `OwnerAuth` entries can be created, no administrative messages can be signed, no new content can be published.

- Relay caches become orphaned - they can continue serving cached content to existing subscribers, but cannot receive new content or authoritative administrative updates.

- The channel is effectively frozen: existing content remains accessible through relays as long as the relays continue operating, but the channel cannot be updated, and its governance is permanently lost.

This is the genuinely catastrophic failure mode - equivalent to losing the origin server with no backup. Mitigation requires owner-side backup of key material, which is an application-level concern outside the channel protocol.

**A subscriber**

*can:*

- Infer which specific owner sent a "message from channel" if the channel has only a single owner. Client-side mitigation: the UI should prevent the channel-as-sender option when there is a single owner.

- Join the channel multiple times with different profiles, inflating subscriber counts.

- See all public content in the channel, by design.

*cannot:*

- Determine the identity of other subscribers - subscribers do not connect to each other, and the relay does not reveal subscriber identities.

- Send messages to the channel (unless their role is elevated above `GRObserver`).

- Forge messages that appear to come from the channel or from an owner.

**A passive network observer**

*can:*

- Observe that a device is communicating with an SMP router, but cannot determine whether the traffic is channel-related, direct messaging, or any other SimpleX protocol use.

*cannot:*

- Determine which channel a subscriber is connected to, due to SMP's 2-hop onion routing and fixed-size transport blocks.

- Correlate a subscriber's channel activity with their other SimpleX activity, because different connections use independent SMP queues with no shared identifiers.

- Determine that an SMP endpoint is a channel relay rather than an ordinary user, because relays use the same SMP client protocol as all other endpoints.

#### Current gaps

The following security properties are designed but not yet fully implemented:

1. **Cross-relay consistency detection.** Subscribers connected to multiple relays should compare message hashes and highlight discrepancies. Currently, duplicate messages from multiple relays are silently deduplicated without comparison.

2. **Link entity ID validation.** Joiners do not yet validate that `linkEntityId == sha256(rootKey)` from the channel link's fixed data. This is deferred to allow forward compatibility with future key rotation. See [group identity binding RFC](../rfcs/2026-03-28-group-identity-binding.md).

3. **Relay profile validation.** When a relay processes a relay request, it should validate the channel profile and verify the owner's signature over the profile. Currently, profile validation is a stub (`validateGroupProfile` is a no-op).

4. **Multi-relay subscriber connection.** Subscribers should connect to multiple relays for redundancy and consistency checking. The protocol supports this, but the current implementation connects to relays listed in the channel link without specific UX for monitoring relay-level delivery health.

5. **Owner-signed member profile vectors.** Relays track which subscriber has received which member profiles, but do not yet prevent targeted withholding of profiles - a relay could selectively not forward certain member profiles to certain subscribers.


## Future work

This section describes planned extensions to the channel protocol. These are at various stages of design, from well-specified to directional.

#### Stateful access and history navigation

SMP queues are stateless - once a message is delivered and expired, it is gone. Channels add state: content persists on the owner's device, and relay caches retain recent history. The current implementation sends recent cached history to new subscribers on join, but does not support navigating or searching older content.

Planned extensions:

- **History pagination.** Subscribers can request older messages from relays via an RPC-style protocol extension, paginated by timestamp or message ID. This is a cache query - the relay returns what it has cached, which may be incomplete depending on its retention policy.
- **Remote search.** Subscribers can query relays for messages matching criteria. The relay executes the search against its cached messages and returns results. This requires trust in the relay to return complete results - cross-relay search comparison can mitigate this, and cache misses could in principle trigger requests through the relay back to the owner.
- **Selective history retrieval.** Subscribers can request specific messages or ranges, rather than receiving the full cache. This reduces bandwidth for subscribers who join an established channel.

These extensions transform channels from a delivery mechanism into an information management system. Relay operators can differentiate on cache depth and search capabilities - offering longer history retention or richer search as a service.

#### Transcript integrity

The current protocol signs administrative messages but not content. Cross-relay consistency provides detection of content manipulation, but only for subscribers connected to multiple relays.

Planned mitigations:

- **Opt-in owner content signing.** Owners can enable signing of all messages, including content. This makes all content non-repudiable (like Nostr), which is appropriate for some use cases (official announcements, legal publications) and inappropriate for others (informal discussion). The protocol already supports this - `requiresSignature` can be made configurable per channel.

- **Subscriber transcript acknowledgment.** Subscribers periodically sign a digest of their received message history - an "I've seen it" signature rather than an "I've authored it" signature. This allows detection of relay manipulation even without opt-in content signing: if relays deliver different content to different subscribers, their transcript digests will diverge.

- **Merkle tree signing.** The owner periodically publishes a signed Merkle root of the message history. Subscribers can verify that their received copies are consistent with the owner's authoritative record. Relays cannot fabricate this root without the owner's key.

#### End-to-end encrypted side conversations

The current support scope provides private messaging between a subscriber and moderators, but the relay can see these messages (as it can see all channel content). Planned extensions:

- **E2E encrypted support scope.** Support conversations are encrypted end-to-end between the subscriber and the moderator/owner. The relay forwards the encrypted messages without being able to read them.

- **E2E encrypted DMs between members.** Where the channel's settings permit it, members can establish direct encrypted connections with each other through the relay, without the relay being able to read the content. This uses the standard SimpleX connection establishment protocol, initiated through the channel context.

- **Private channels.** For channels where content itself should be encrypted, the entire content stream is encrypted to a key shared among authorized subscribers. The relay forwards encrypted content it cannot read. This converts the relay's role from "sees content, doesn't see identity" to "sees neither content nor identity" - a pure distribution conduit.

#### Relay addition and removal

The current protocol supports adding relays at channel creation. Planned extensions:

- **Dynamic relay addition.** Adding new relays to an existing channel, with automatic subscriber redistribution. New relays populate their cache from existing relays or from the owner.

- **Relay removal.** Removing a relay with subscriber migration. The removed relay forwards the deletion event and new relay information to its subscribers. Subscribers reconnect to remaining relays. If the last relay is removed, the owner must add a new one to restore channel functionality.

- **Relay rotation.** Replacing a relay while maintaining service continuity. The new relay connects and populates its cache before the old relay is removed, minimizing disruption.

#### Governance evolution

The governance progression described in [Channel governance](#governance-how-decisions-are-made) requires protocol extensions at each stage:

- **Multiple owners (v7):** Protocol support for multiple `OwnerAuth` entries with concurrent administrative authority. Any owner can act independently. Conflict resolution follows last-writer-wins semantics.

- **Multisig:** Administrative messages carry multiple signatures and are processed only when the required quorum is met. Different action types may require different quorums, and different owners may hold different voting weight.

- **Code-based channel articles:** The channel definition includes executable governance rules - code that determines which actions require which approvals. This generalizes multisig into arbitrary governance: shareholding structures, hierarchical approval chains, role-specific executive powers, or any other management model that can be expressed as rules over the authorization chain.

#### Pre-moderation

For channels with strict content policies, pre-moderation allows all subscriber messages (in channels where subscribers can send, such as comment threads) to be reviewed by moderators before becoming visible to all subscribers.

#### Scheduled delivery

Owners can schedule messages for future delivery. The relay caches the message and delivers it at the scheduled time. This is a common requirement for publication channels - scheduling a week's worth of daily updates in a single session.

#### Link preview proxying

When a message contains a URL, a link preview (title, image, description) is typically generated by fetching the URL. This creates a privacy trade-off:

- If the **sender** loads the preview, they leak their IP address to the URL's server.
- If the **subscriber** loads the preview, they are exposed to tracking by the URL's server.
- If an **external proxy** loads the preview (as Signal and WhatsApp do), the proxy, which is typically the messaging service operator, learns what URLs are being shared, undermining end-to-end encryption promises.

The current implementation has the sender load previews on an opt-in basis, as the least harmful default.

In channels, a better alternative is available: the **relay** loads the preview. The relay already sees the message content (including URLs), so it learns nothing new by fetching the preview. But unlike the sender, the relay's IP address is not linked to any real identity. Unlike an external proxy operated by a messaging service, the relay is not undermining an encryption promise - the content is already visible to it by design. This is the entity that has access to content but not to participant identity, making it the natural choice for preview generation.

This approach gives senders previews before their messages are published, without exposing their IP address or creating a centralized proxy that correlates URL access with user identity.
