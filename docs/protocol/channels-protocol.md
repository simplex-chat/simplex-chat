Revision 1, 2026-04-28

# SimpleX Channels Protocol

For architecture, design rationale, security properties, and threat model, see [SimpleX Channels Overview](./channels-overview.md).

## Table of contents

- [Protocol](#protocol)
  - [Channel creation](#channel-creation)
  - [Relay acceptance](#relay-acceptance)
  - [Relay addition](#relay-addition)
  - [Subscriber connection](#subscriber-connection)
  - [Message signing](#message-signing)
  - [Message forwarding](#message-forwarding)
  - [Binary batch format](#binary-batch-format)
  - [Delivery pipeline](#delivery-pipeline)
  - [Message deduplication](#message-deduplication)
  - [Channel-as-sender messages](#channel-as-sender-messages)
  - [Member support scope](#member-support-scope)


## Protocol

This document describes the channel protocol as currently implemented. It builds on the [SimpleX Chat Protocol](./simplex-chat.md), using the same message format and connection model, with extensions for relay-mediated distribution and cryptographic message signing.

### Channel creation

Creating a channel involves generating cryptographic material, creating the channel link, and connecting relay members:

1. **Key generation.** The owner generates an Ed25519 root key pair. The entity ID is computed as `sha256(rootPubKey)`. A separate member key pair is generated for message signing, and an `OwnerAuth` entry is created, signed by the root key.

2. **Link creation.** The owner calls the agent's `prepareConnectionLink` API with the root key pair and entity ID. This returns a prepared link (including a `ConnShortLink` address) without any network calls. The link address is deterministic, derived from the fixed data hash, so it can be embedded in the group profile immediately.

3. **Link data upload.** The owner calls `createConnectionForLink`, which makes a single network call to create the SMP queue and upload the encrypted link data. The link's fixed data contains the root public key and connection request. The mutable user data contains the `OwnerAuth` array, the channel profile (including the entity ID and the link itself), and the initial subscriber count.

4. **Relay invitation.** For each selected relay, the owner sends a contact request containing an `x.grp.relay.inv` message with the channel's short link. The relay retrieves the link data, validates the channel profile, creates its own relay link (with the channel's entity ID in its immutable data), and responds with `x.grp.relay.acpt` containing its relay link.

5. **Link update.** As each relay accepts and provides its relay link, the owner validates that the relay link contains the correct entity ID, then adds the relay link to the channel link's mutable data.

6. **Local record.** The channel is stored on the owner's device with the root private key, member private key, and channel profile. This local record is the authoritative state of the channel.

### Relay acceptance

When a relay receives an invitation to serve a channel, it validates the channel and creates its own relay link. This flow is currently part of channel creation; adding relays to an existing channel is planned but not yet implemented.

1. Owner sends `x.grp.relay.inv` to the relay's contact address. This message includes the relay's member ID and role, the owner's profile, and the channel's short link.

2. Relay receives the invitation and creates a relay request record. A relay request worker processes it asynchronously.

3. The worker retrieves the channel's link data from the SMP server, extracts and validates the channel profile and owner authorization.

4. The relay creates its own contact address link (the relay link) with the channel's entity ID in the immutable fixed data.

5. The relay accepts the owner's connection request, sending its relay link in the acceptance.

6. The owner retrieves the relay link data, validates that the entity ID in the relay link matches the channel's entity ID, and adds the relay link to the channel link's user data.

TODO: Periodic monitoring where the relay retrieves channel link data to verify its relay link is still listed is planned but not yet implemented.

### Relay addition

When the owner adds a relay to an existing channel:

1. **Acceptance.** The new relay accepts the invitation following the [Relay acceptance](#relay-acceptance) flow. The owner promotes the relay to active when the channel link's updated relay list is confirmed.

2. **Announce.** If the channel has at least one subscriber, the owner sends `x.grp.relay.new` (carrying the new relay's short link) to every other currently-connected relay of the channel.

3. **Forward.** Each relay forwards `x.grp.relay.new` to its subscribers. The relay does not create a member record for the announced relay — relays do not connect to other relays of the same channel.

4. **Connect.** On receipt, the subscriber resolves the announced short link and connects to the new relay asynchronously.

The announce is an optimisation. When it does not reach a subscriber — because the channel had no subscribers at announce time, because an older client or relay sits in the path, or because of a transient network failure — the subscriber reaches the same end state on the next channel open via its relay sync against the channel's link data.

### Relay rejection

When a relay operator removes the relay from a channel, the relay marks the channel as rejected and refuses future invitations from the same channel link:

1. **Leave.** The relay operator runs `/leave #channel`. The relay marks the channel as rejected locally, keyed by the channel's short link.

2. **Refuse.** When the owner later sends `x.grp.relay.inv` for the same channel link — typically from a re-invitation — the relay does not accept the invitation as a relay. Instead it replies with `x.grp.relay.reject` over the owner-relay direct contact channel, carrying a rejection reason. The current reason is `rejoin_rejected`; older relays or future reasons fall through to an unknown reason for forward compatibility.

3. **Owner handling.** The owner marks the corresponding relay as rejected and notifies the operator UI. The owner also sets the relay member's status to `GSMemLeft` so the UI treats the rejected relay identically to one that ran `/leave`. The owner's next user-initiated relay addition for the same channel creates a fresh invitation, which the relay rejects again unless the rejection has been cleared.

4. **Clear.** The relay operator runs `/group allow <groupId>` to clear the rejection for the channel. After the next user-initiated relay addition, the relay accepts the invitation and rejoins as a relay.

An older owner client that does not recognise `x.grp.relay.reject` ignores the message and leaves the relay invitation in an invited state indefinitely — the same end state as a relay that does not respond. An older relay binary does not enforce rejection; in mixed-version deployments the operator can re-run `/leave` under the new binary to re-establish rejection.

### Subscriber connection

A subscriber joins a channel through the following flow:

1. **Link retrieval.** The subscriber scans or receives the channel's short link. The client retrieves the link data, which contains the channel profile, owner authorization chain, and list of relay links.

2. **Relay link resolution.** For each relay link listed, the client resolves the `ConnectionRequestUri` from the relay's short link.

3. **Connection.** The client connects to relays - the first synchronously, the rest asynchronously. Each connection sends an `x.member` message with the subscriber's profile (or an incognito profile, created once and shared with all relays), member ID, and member signing key.

4. **Relay acceptance.** Each relay accepts the connection, creates a member record for the subscriber with the configured subscriber role (default `observer`), and sends an `x.grp.link.inv` message with the channel profile and group link invitation data.

5. **Introduction.** The relay introduces the new subscriber to the channel's moderators and owners by sending an `x.grp.mem.new` message. It also sends moderator/owner profiles to the subscriber.

6. **History.** If the channel has history sharing enabled, the relay sends recent cached history to the new subscriber.

The subscriber is functional (can receive messages) as soon as at least one relay connection succeeds. Additional relay connections provide redundancy and cross-relay consistency checking.

### Message signing

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
| `x.grp.relay.new` | Announce new relay to subscribers | Required |
| `x.grp.leave` | Leave channel | Required (unverified allowed between subscribers) |
| `x.info` | Update member profile | Required (unverified allowed between subscribers) |
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

1. The signature is present: reconstruct the binding prefix from the channel's stored entity ID and the sender's member ID. Verify all signatures against the sender's stored public key. If all verify, the message is accepted as verified.

2. The signature is present but the sender's key is unknown: the message is accepted as signed-but-unverified only if the event does not require a signature. For `x.grp.leave` and `x.info` between subscribers whose keys haven't been exchanged yet, unverified signatures are permitted as a temporary measure.

3. No signature is present: the message is accepted only if the event does not require a signature (i.e., the channel does not use relays, or the event is a content message).

If verification fails for a message that requires a signature, the message is rejected and a bad signature event is shown to the user.

### Message forwarding

Content originates on the owner's device and flows through relays to subscribers. The forwarding mechanism preserves the original message bytes, including any signature, without re-encoding:

**Owner to Relay:** The owner sends messages directly to each relay over their SMP connection. Messages are encoded in binary batch format.

**Relay processing:** When a relay receives a message from an owner, it:

1. Parses and processes the message locally (updating its cached state, e.g. for roster changes).
2. If the relay is configured to forward for this channel, creates a **delivery task** for each message that should be forwarded to subscribers. The task records the message ID, the sender's member ID, the broker timestamp, and whether the message was sent as the channel (not attributed to a specific owner).
3. The delivery task is persisted to the database for delivery reliability - ensuring forwarding can resume after a relay crash.

**Relay to Subscribers:** A delivery task worker reads pending tasks, batches them into delivery jobs, and a delivery job worker sends each job to subscribers in paginated batches (using a cursor over group member IDs).

For forwarded messages from subscribers to owners (e.g. support scope messages), the relay wraps the message in a forwarding envelope:

```
forwardEnvelope = ">" <> smpEncode(GrpMsgForward) <> encodeBatchElement(signedMsg, msgBody)
```

This preserves the original message's signature bytes verbatim.

### Binary batch format

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

### Delivery pipeline

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

For subsequent subscribers in a batch, the agent uses a value reference to the first subscriber's message body, avoiding redundant data transmission to the SMP server.

### Message deduplication

When multiple relays serve the same channel, each subscriber receives the same message from each relay independently. Deduplication is performed at the subscriber's client level using the message's shared message ID:

- When saving a received message, the client checks whether a message with the same shared ID already exists for this group.
- If a duplicate is found, the message is silently dropped (in channels with relays).
- In non-relay groups, duplicate detection triggers a `x.grp.mem.con` notification to the forwarding member.

This is essentially cache coherence verification - comparing what was received from one cache node against another. TODO: Currently, deduplication only detects the presence of duplicates. The protocol design includes provisions for detecting differences between relay-delivered copies of the same message (hash comparison, UI indicators for discrepancies). This is described in the [channels forwarding RFC](../rfcs/2025-08-11-channels-forwarding.md) and is not yet implemented.

### Channel-as-sender messages

Owners can send messages attributed to the channel rather than to themselves. When `asGroup = True` is set in the message container, the relay forwards the message with a channel-as-sender tag instead of attributing it to a specific member. On the subscriber side, such messages are displayed as coming from the channel (using the channel's profile image and name) rather than from a specific owner.

This will be useful for channels with multiple owners (not yet implemented at application level) where the identity of the specific sender should not be visible to subscribers. The relay must respect this directive; ignoring it and revealing the sending owner's identity is a threat vector (detectable out-of-band by members communicating with the owner).

The forwarding binding prefix for channel-as-sender messages uses `CBChannel` instead of `CBGroup`, and includes only the channel's entity ID (not the sender's member ID):

```
channelBinding = smpEncode(CBChannel) <> smpEncode(publicGroupId)
```

### Member support scope

Channels support a **member support scope** - a private side-channel between a subscriber and the channel's moderators/owners. Messages sent in the support scope are delivered only to moderators and the scoped subscriber, not to all subscribers.

A support-scoped message includes the target member's ID. The delivery pipeline uses a separate job scope for support messages, loading only the scoped member and moderators rather than all subscribers.

Support scope messages are visible only to the subscriber who initiated the support conversation and to the channel's moderators. Other subscribers cannot see them. This allows subscribers to report issues, appeal moderation decisions, or communicate with administrators without revealing their identity to other subscribers.
