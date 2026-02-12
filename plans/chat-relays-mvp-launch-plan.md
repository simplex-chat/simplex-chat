# Chat Relays MVP — Launch Plan

## Contents
- [Executive Summary](#executive-summary)
- [What's Done](#whats-done)
- [What's Remaining](#whats-remaining): Protocol & Crypto | Relay Protocol | Member Connection | UI | Testing | Polish | Directory
- [Dependency Summary](#dependency-summary)
- [Risk Register](#risk-register)
- [Decisions Made](#decisions-made)
- [Post-MVP Backlog](#post-mvp-backlog)

---

## Executive Summary

Chat Relays enable large public channels where messages flow owner → relay → members, replacing N-to-N connections. This plan covers what remains for MVP launch.

**Current state**: Core backend ~75% done (delivery system, forwarding, deduplication, relay invitation/acceptance, group creation with relays all working). UI ~15%. Key remaining work: member key signatures, relay identity validation, forward envelope protocol, UI on both platforms.

**MVP delivers**: Owners create channels with preset relays. Relays validate and serve groups. Members join via links, receive relay-forwarded messages signed by owners. UI differentiates channels from groups.

**Out of scope**: Relay removal/recovery, periodic relay health monitoring, relay-to-relay sync, history navigation, e2e encryption in support chats, multi-owner support, reaction/comment batching. See [Post-MVP](#post-mvp-backlog).

---

## What's Done

- Single-roundtrip group creation with relays (`APINewPublicGroup` → `prepareConnectionLink` → `createConnectionForLink` — Agent API complete)
- Relay invitation/acceptance protocol (`XGrpRelayInv`, `XGrpRelayAcpt`) and relay request worker
- Async delivery task/job system with cursor-paginated member delivery
- `FwdChannel` / `FwdMember` forwarding modes, `ShowGroupAsSender` through full pipeline
- Message deduplication on member side
- Binary batch encoding (`=` prefix) in `Messages/Batch.hs` and `Protocol.hs`
- DB schema: `chat_relays`, `group_relays`, `group_members.relay_link`, key columns on `groups`/`group_members`
- Preset relay configuration framework (3 placeholder relays in `Presets.hs`)
- `CIChannelRcv` chat item direction in backend
- Observer role UI already works on both platforms (compose bar hidden, reactions only)

## What's Remaining

Organized by architecture layer, not work streams. Items within each section are roughly ordered by dependency.

---

### 1. Protocol & Cryptography

#### 1.1 Binary Forward Envelope (`F` prefix)
New top-level binary format replacing `XGrpMsgForward` for relay groups. Wraps original sender bytes verbatim — preserves signatures through relay forwarding without re-encoding.

Format: `F<memberId><memberName><brokerTs><original-bytes>` (see member-keys-plan.md §8).

Old groups keep `XGrpMsgForward` (JSON). New relay groups use `F` envelope. Parser accepts both.

**Files**: `Protocol.hs` (parse/encode), `Batch.hs` (batching), `Subscriber.hs` (forwarding handler replacement)

#### 1.2 Key Generation & Storage
Generate Ed25519 key pairs on group creation/join. Populate existing DB columns: `root_priv_key`/`root_pub_key` on `groups`, `member_priv_key` on `groups`, `member_pub_key` on `group_members`.

Consider adding to current `M20260222_chat_relays` migration (unreleased) rather than creating a new one.

**Files**: `Store/Groups.hs`, `Store/Profiles.hs`, `Commands.hs` (creation flow)

#### 1.3 Message Signing
Sign roster-modifying messages (`XGrpRelayInv`, `XGrpMemNew`, `XGrpMemRole`, `XGrpMemDel`, `XGrpInfo`, `XGrpPrefs`, `XGrpDel`) with owner's member key.

**Files**: `Internal.hs` (signChatMessage), `Commands.hs` (sendGroupMessage integration)

#### 1.4 Signature Verification
Verify signatures on received roster messages. Hard fail for missing/invalid signatures in new-version groups.

**Files**: `Internal.hs` (verifyChatMessage), `Subscriber.hs` (reception)

#### 1.5 OwnerAuth Chain
Owner authorization signed by root key, stored in group link's `UserContactData.owners`. Members verify owner identity via chain. Type exists; integration TODO.

**Files**: `Protocol.hs`, `Commands.hs`, `Subscriber.hs`

#### 1.6 Version Gating
Chat relays is a new feature — relay groups only joinable by clients of the new version. Add `chatRelaysVersion` to version range. No backward compat needed for relay groups themselves (they don't exist in older versions).

**Files**: `Types.hs` (version constant), `Commands.hs` (gating)

---

### 2. Relay Protocol

#### 2.1 Relay Address Link Data
On relay address creation, set link data: relay identity (profile, certificate, relay identity key). Members validate this when connecting.

**Files**: `Commands.hs` (relay address creation), `Protocol.hs` (relay link data structure)

#### 2.2 Group Profile Validation by Relay
Before accepting to serve group, relay validates group profile, verifies owner's signature, and checks `shared_group_id` in immutable link data (prevents redirect to wrong group).

**Files**: `Subscriber.hs` (`runRelayRequestWorker` — stub exists, validation logic TODO)

#### 2.3 Relay Link Data on Acceptance
When accepting, relay sets: relay identity, relay key for group, group ID in immutable part of relay link data.

**Files**: `Subscriber.hs` (relay link creation)

#### 2.4 Relay Key/Identity Validation by Members
When member connects to relay, validate relay link data (identity, key, group ID) matches group link data. This is part of the same signature/identity verification work as §1.4.

**Files**: `Commands.hs` (`connectToRelay`), `Subscriber.hs`

#### 2.5 Test Chat Relay Command
`APITestChatRelay` / `TestChatRelay` — channel owners need to verify relay connectivity before creating channels.

**Files**: `Commands.hs` (new command)

#### 2.6 Real Relay Addresses in Presets
Replace placeholder URLs in `simplexChatRelays`. Depends on relay server deployment.

**Files**: `Operators/Presets.hs`

#### 2.7 Channel-Only Behavior Enforcement
In channel groups (`useRelays = True`), the API supports sending both as channel (`asGroup=True`) and as member. For MVP, UI always passes `asGroup=True`. Backend does not enforce — owners retain the API option to send as member for future use. Non-owner/non-admin members can only send reactions (observer role enforced by existing role system).

**Files**: UI-only enforcement for MVP (both platforms pass `asGroup=True` in compose)

---

### 3. Member Connection Flow

#### 3.1 Support `/c` API for Relay Groups
Automate `APIPrepareGroup` → `APIConnectPreparedGroup` flow when using `/c` command with a relay group link. Currently requires manual two-step call.

**Files**: `Commands.hs` (`connectWithPlan`)

#### 3.2 Relay Connection State Response Type
New response type/events showing per-relay connection state (connecting, connected, temporary error, permanent error). Needed for both member join and owner creation UX.

**Files**: `Controller.hs` (new ChatResponse variants), `Commands.hs` (emit events)

#### 3.3 Member Count for Channels
Existing member count display uses loaded member list — won't work for channels, where members only have records for owners and relays. Relays must communicate real member counts (excluding relays themselves) to members and owners. Needs protocol extension for relay → member count communication.

**Files**: `Protocol.hs` (new event or extension), `Subscriber.hs` (relay reporting), UI (display)

---

### 4. UI — Both Platforms (iOS + Android/Desktop)

All UI items must be completed on both platforms for MVP.

#### 4.1 Channel Visual Distinction
Different icon/badge for channels in chat list. "Channel" label. Key off `useRelays` flag in `GroupInfo`.

No backend dependency — can start immediately.

#### 4.2 "Message from Channel" Display
`CIChannelRcv` direction NOT yet handled in either platform's UI. Must add to message rendering pipeline. `showGroupAsSender` message rendering.

Backend complete. No backend dependency.

#### 4.3 Channel Creation Flow
"Create Channel" button in new chat menu → name/description → relay selection → creation with relay status feedback (invited → accepted → active). Backend `APINewPublicGroup` exists.

Depends on: §3.2 (relay connection state type)

#### 4.4 Relay Management (User Settings)
List of configured relays; add/remove/edit; test connectivity. Follow existing SMP server management pattern.

Depends on: §2.5 (`APITestChatRelay`)

#### 4.5 Show Relays in Channel Info
Relay list with status and identity in channel info screen.

#### 4.6 Relay Connection State During Join
Progress feedback when joining: "Connecting to relays..." → per-relay status → "Connected".

Depends on: §3.2 (relay connection state type)

#### 4.7 Owner Posting UI
Compose mode always sends as channel (`asGroup=True`). No toggle for MVP.

#### 4.8 API Type Updates
- **iOS**: Add `apiNewPublicGroup` to `ChatCommand` enum; add `ChatRelay`, `RelayStatus`, `GroupRelay`, `CIChannelRcv` types
- **Android**: Add corresponding types to Kotlin model layer
- Both: relay connection state event types

---

### 5. Testing

- Delivery loop restored after restart
- Delivery in support scopes inside channels
- Connect plans for relay groups
- Cancellation on failure to create relay group
- Async retry connecting to relay (members)
- Relay privileges
- Binary forward envelope encode/decode round-trips
- Message signing and verification flow
- Relay signature validation in invitation flow
- Backward compat: old clients cannot join relay groups (version gated)

---

### 6. Polish & Edge Cases

- Create missing service chat items ("relays updated" for owner, "group invite accepted" for relay)
- Disable link data output in CLI (`View.hs` — currently enabled for manual testing, cleanup)
- When deleting chat relay from user config, check `group_relays` references and mark as deleted instead
- Single file description for all recipients (performance)

---

### 7. Directory Service Verification

Directory service currently has no channel/relay awareness — it only lists regular groups. Needs verification how channels should appear in directory and what integration work is required. Some adaptation may be needed.

---

## Dependency Summary

```
Can start immediately (no dependencies):
  §1.2 Key Storage, §1.3-1.5 Signing/Verification, §1.6 Version Gating
  §2.1 Relay Address Data, §2.7 Channel Enforcement (UI-only)
  §4.1 Channel Visual Distinction, §4.2 "Message from Channel" Display

Needs §1.3-1.5 (signing):
  §2.2 Group Profile Validation, §2.3 Relay Link Data

Needs §2.1+2.3:
  §2.4 Relay Key Validation by Members

Needs §3.2 (relay state type):
  §4.3 Channel Creation UI, §4.6 Join State UI

Needs §2.5 (test command):
  §4.4 Relay Management UI

Late phase:
  §5 Testing (needs most backend complete)
  §2.6 Real Relay Addresses (needs server deployment)
  §7 Directory Verification
```

**Critical path**: §1.1 (Forward Envelope) + §1.2-1.5 (Keys/Signing) → §2.2-2.3 (Relay Validation) → §5 (Testing) → Launch

**Early UI wins**: §4.1, §4.2 can start in Phase 1.

---

## Risk Register

| Risk | Impact | Mitigation |
|------|--------|------------|
| Forward envelope (`F`) version mismatch relay↔member | High | Version gating — relay groups require new version on all participants |
| Relay server instability under load | High | Load test early; multi-relay redundancy |
| UI on 2 platforms takes longer than expected | Medium | Both required for MVP; start UI early (§4.1, §4.2 have no backend deps) |
| Member count protocol extension complexity | Medium | Can ship without count initially; add in fast-follow |
| Stale relay "Active" status (no health monitoring) | Low | Multi-relay redundancy; manual `APITestChatRelay`; monitoring post-MVP |

---

## Decisions Made

- **Single-owner channels**: Allowed without warning (sender identity is clear for "messages from channel"). Single-owner is the main MVP case; "from channel" UX is valuable regardless. Revisit with multi-owner support.
- **Channel-only enforcement**: UI-only for MVP (`asGroup=True` always passed). Backend retains API flexibility for future "send as member" option.
- **Default member role**: Observer by default for channels. No additional owner→relay communication of role/rejection rules for MVP.
- **Contact connection refactoring**: Deferred to post-MVP. Current flow works.
- **Member rejection by relay**: Deferred. MemberId clash unlikely; rejection rules postponed.
- **Relay profiles**: Consider for MVP vs post-MVP. Members and owners see relay profiles in group already; linking to single per-config profile is nice-to-have.
- **Chat relay user filtering**: Post-MVP. Relay user will be visible in client for now.

---

## Post-MVP Backlog

1. Relay removal and group recovery — owner removes relay, members reconnect via updated link
2. Periodic relay health checks — relay verifies link presence in group link data
3. Relay-to-relay synchronization
4. Managing relays in existing group — add/remove relays post-creation
5. Default member role and rejection rules communication owner→relay
6. Member rejection by relay (duplicate member ID, rule violations)
7. Contact connection flow refactoring (`connectViaContact` simplification)
8. Deduplication highlighting — show differences between relay-forwarded messages
9. History navigation — request older messages from channel
10. E2E encryption in admin/support chats
11. Reaction/comment count batching
12. Priority connections — separate queues for messages vs admin requests
13. Member profile delivery optimization
14. Private relays with password
15. Channel content moderation
16. Indefinite file storage for relays
17. Message revocation from history
18. Channel discovery/directory integration (verify and extend)
19. Advanced forwarding envelope — include channel link in forwarded message metadata for distribution
20. Relay profiles linked to single per-config record
21. Chat relay user filtering/separate UI
