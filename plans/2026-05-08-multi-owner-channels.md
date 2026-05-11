# Multi-owner Channels — implementation plan

Revision 5, 2026-05-11 · Target: SimpleX Channels v7 (full-trust, any-owner-decides)

> **DESIGN DECISION REQUIRED — discuss with team before implementation.**
> Maximum number of owners per channel: proposed cap of 8 (default
> `ownerChainDepth`). Each `OwnerAuth` is ~189 B; 8 owners ~1.5 KB of the
> 13.4 KB user-data padded budget; O(N²) ≈ 64 Ed25519 verifies on decode.
> One-line constant change — confirm before implementing.

> **Constraint up front.** Promoting a subscriber to owner via a
> relay-mediated offer (no direct connection between existing owner and
> candidate) is not supported in this delivery. Promotion always uses a
> fresh, channel-scoped direct mesh connection.

### Changelog (since Revision 4)

- **N1** — Removed `x.grp.owner.invite` from the Phase 2.4.1 mesh
  receive allowlist (it is sent over A↔B's personal contact, before
  the mesh edge exists). Added a clarifying sentence.
- **N2** — Replaced the vague "thin wrapper over the existing
  rcv-queue read" description of `getChannelLinkRcvPubKey` with a
  precise one (Phase 4.4).
- **N3** — Replaced "handler takes a mesh-scoped variant" with an
  explicit implementation shape: early-branch inside the existing
  `xGrpMemIntro` / `Inv` / `Fwd` handlers (Phase 2.4).

### Changelog (since Revision 3)

- **I1** — Split G7 creator-row backfill across DBs: SQL migration
  fills chain fields only (`owner_auth_sig`, `owner_position`);
  `owner_rcv_pub_key` is filled by a one-time startup backfill in
  `Simplex.Chat.Library.Owners` that queries the agent for this
  device's rcv pubkey on each channel link queue. Phase 2.1, 4.4. Test
  `testCreatorRcvPubKeyBackfilledAtStartup`.
- **I2** — Phase 4.5 no longer broadcasts `XGrpLinkSync` carrying an
  rcv pubkey. Dropped candidate emits a chat-layer event prompting any
  current owner to re-run the Risk #12 pre-flight + `setQueueRecipientKeys`;
  other owners already know every rcv pubkey via path-2 mesh bootstrap.
- **I3** — `XGrpOwnerInvite` / `XGrpOwnerAccept` defined in Phase 2.4;
  added to the mesh allowlist (2.4.1).
- **I4** — `ownerRcvPubKey` added to `XGrpMemFwd` (in addition to
  `Intro` / `Inv`); Phase 2.4 #4 lists per-edge flow on all three.
- **I5** — Verified `Subscriber.hs:2953-3070`: existing `xGrpMemIntro`
  accepts only `GCHostMember` and rejects / merges on duplicate
  member. Phase 3 Step 6 uses a **mesh-scoped variant** keyed on the
  `ownerRcvPubKey` field + introducer-is-owner: skips category check
  and member-creation paths; creates only the direct mesh connection
  + `channel_owner_mesh` row. Phase 2.4 #4 and Phase 3 Step 6.
- **I6** — Phase 1 renumbered 1.4 → 1.5 → 1.6 → 1.7 (no gap).
- **I7** — `applyChannelOwnerRoster` parameter is now a named
  `WireOwner` record (not a positional tuple). Phase 2.2.
- **I8** — Dropped the test-name catalog at end of Phase 6; names
  remain inline per phase and in the Risk register.

### Changelog (since Revision 2)

G1 `owner_rcv_pub_key` propagation via bundle field (path 1) +
optional `ownerRcvPubKey` on `XGrpMemIntro`/`Inv`/`Fwd` (path 2);
mesh-scope acceptance rule. G2 `groupLinkData` becomes IO. G3 SMP has
no read primitive for `recipientKeys` (verified `Protocol.hs:540-575`);
`owner_rcv_pub_key` is canonical. G4 Risk #12 (stale-mesh-view RKEY).
G5 ingest rule for unknown / key-mismatched entries. G6
`OwnerAuth.ownerId == unMemberId memberId`. G7 eager creator-row
backfill (split across DBs in I1). G8 `rcv_queues` PK verified; no
schema change. G9 mesh-vs-relay receive rule on
`channel_owner_mesh.direct_conn_id`. G10 CI grep replaces "static
logInfo lint".

---

## 1. Summary

Extend SimpleX Channels from single-owner to "any-owner-decides".
Owners share an SMP recipient queue, each holds an Ed25519 owner
private key, each can independently push mutable-blob updates (LSET)
and rotate the recipient-key set (RKEY). A channel-scoped owner mesh
(modeled on `x.grp.mem.intro`) propagates link-data changes;
last-writer-wins on the SMP server resolves contention. Out of scope:
multisig, programmable governance, public-group migration over relays,
root-creator transfer, owner-only chat UI surface.

## 2. Phase ordering

| # | Phase | Depends on | Parallelizable |
|---|---|---|---|
| 1 | Agent: depth cap, RKEY wrapper, co-owner bundle, tests | — | no |
| 2 | Chat: migrations + owner-roster helpers + mesh transport | 1 | no |
| 3 | Chat: promote-to-owner orchestrator | 2 | no |
| 4 | Chat: link-sync, LWW reconciliation, owner removal, RKEY recovery | 3 | no |
| 5 | UI (iOS + Kotlin multiplatform) | 4 | yes (iOS ⫼ Kotlin) |
| 6 | E2E tests + threat-model regressions | 5 | no |

---

## 3. Phases

### Phase 1 — Agent (simplexmq, at master)

Most verification is upstream already: `validateLinkOwners` (prefix-
only chain, duplicate detection), `decryptLinkData` (chain-aware),
`encodeSignUserData` (signer-agnostic), server-side any-of-N on
`recipientKeys`, `RKEY` restricted to `QMContact`.

**1.1 Chain depth cap.** New `ownerChainDepth = 8` in
`Simplex.Messaging.Agent.Protocol`; `validateLinkOwners` rejects
`length owners > ownerChainDepth`; chat-side mirrors at encode (2.3).
Wire `sig64 || md_bytes` carries no signer ID — observers cannot
identify which owner pushed an update (preserves objective #6). Tests:
`testChainCycleStructurallyImpossible`, `testChainTooLong`.

**1.2 Signer selection (no agent change).** `encodeSignUserData` takes
any `PrivateKeyEd25519`. Channel link queue's signing key lives in
`ShortLinkCreds.linkPrivSigKey` (set at creation for creator, at
intake 1.5 for co-owner), sourced chat-side from
`GroupKeys.groupRootKey` (`GRKPrivate rootPrivKey` ⇒ creator;
`memberPrivKey` ⇒ co-owner). Existing `setConnShortLink` already signs
correctly once `linkPrivSigKey` carries the right key — no code change
at that call site. Upstream `linkRootSigKey` TODO at
`AgentStore.hs:2514` untouched.

**1.3 Co-owner RcvQueue (G8).** Each device holds one `rcv_queues`
row for the shared channel link queue with its own `rcv_private_key`
+ `ShortLinkCreds`. Schema unchanged: `PRIMARY KEY (host, port,
rcv_id)` (`agent_schema.sql:66`) is per-database — two profiles on one
device cannot both co-own a channel via that device (same pre-existing
constraint as the subscribe case). Chat-layer gate prevents `DEL` from
a co-owner (only `GRKPrivate _`). Test: `testCoOwnerCannotDeleteQueue`.

**1.4 RKEY agent wrapper.** Single primitive — full key list per call.

```haskell
setQueueRecipientKeys
  :: AgentClient -> NetworkRequestMode -> ConnId
  -> NonEmpty SMP.RcvPublicAuthKey -> AE ()
```

Concurrent races recovered by Phase 4.5 + Risk #12 pre-flight.

**1.5 Co-owner credential bundle.** New `CoOwnerCredsBundle` in
`Simplex.Messaging.Agent.Protocol`. Fields: `server`, `rcvId`,
`rcvDhSecret`, `shortLinkId`, `shortLinkKey`, `rootPubKey`,
`linkEncFixedData`, `agentVRange`, **`ownerRcvPubKey ::
SMP.RcvPublicAuthKey`** (A's own SMP rcv pubkey — G1 path 1; chat
writes it to `group_members.owner_rcv_pub_key` for A on B's device
at intake). B's `rcvPrivateKey` is generated locally
(`C.generateAuthKeyPair`); queue-specific; never derived from
`member_pub_key`. Intake API:

```haskell
acceptCoOwnerCreds
  :: AgentClient -> NetworkRequestMode -> UserId
  -> CoOwnerCredsBundle
  -> RcvPrivateAuthKey       -- B's local rcv auth key
  -> C.PrivateKeyEd25519     -- B's owner = member signing key
  -> AE ConnId
```

Chat also writes `rootPubKey` into `groups.root_pub_key`.

**1.6 Mutable-blob version.** Add `linkDataVersion :: Maybe Word64` to
chat-layer `GroupShortLinkData` JSON. Unknown-field tolerant; absent
⇒ 0. Agent-layer link blob structurally unchanged.

**1.7 Version constants.**

```haskell
-- Simplex.Messaging.Agent.Protocol
multiOwnerSMPAgentVersion = VersionSMPA 8
currentSMPAgentVersion    = multiOwnerSMPAgentVersion

-- Simplex.Chat.Protocol  (implementer reads chatVersionRange at commit)
multiOwnerChatVersion = VersionChat <current+1>
currentChatVersion    = multiOwnerChatVersion
```

Hard incompatibility: pre-v7 clients reject blobs signed by a chained
owner. Acceptable. Release note: "Channels with multiple owners
require SimpleX Chat v7 or later to read." Tests:
`testOldClientRejectsChainedOwnerBlob`,
`testOldClientReadsRootSignedBlob`.

---

### Phase 2 — Chat foundation

Reads: `Library/Internal.hs:1313-1399, 2474-2477`,
`Library/Commands.hs:2496-2527, 4042-4238`,
`Store/Groups.hs:1860-1900, 2999-3020`,
`Library/Subscriber.hs:2953-3070`, `Protocol.hs:422-470, 980-1320`,
`Store/SQLite/Migrations.hs`.

**2.1 Schema migrations.** Additive; SQLite + Postgres mirrors;
`M<DATE>_<name>.hs` (current head is `M20260507_relay_inactive_at`).

- `M<DATE>_group_members_owner_fields` — `group_members` += `owner_auth_sig BLOB`,
  `owner_position INTEGER`, `owner_rcv_pub_key BLOB` (nullable);
  `groups` += `link_data_version`, `link_data_remote_version`
  (`INTEGER NOT NULL DEFAULT 0`). **Eager chain-fields backfill (G7,
  I1):** for each group with non-null `member_priv_key` and no owner
  row with `owner_auth_sig` populated, synthesize the creator's chain
  fields in-migration — `owner_auth_sig = sign(rootPrivKey, memberId
  ‖ encodePubKey(publicKey memberPrivKey))`, `owner_position = 0`.
  `owner_rcv_pub_key` stays NULL here; the agent DB is not reachable
  from a chat-DB migration. It is filled by the one-time startup
  backfill in 4.4.
- `M<DATE>_owner_mesh` — `channel_owner_mesh` (`channel_owner_mesh_id`,
  `group_id`, `peer_group_member_id`, `direct_conn_id`, `status TEXT`,
  timestamps; `UNIQUE(group_id, peer_group_member_id)`; FKs to
  groups/group_members/connections).
- `M<DATE>_promotion_in_progress` — `channel_promotion_in_progress`
  (`promotion_id`, `group_id`, `candidate_member_id`,
  `candidate_pub_key`, `candidate_rcv_pub_key`, `step TEXT`,
  `direct_conn_id`, `last_error`, timestamps; `UNIQUE(group_id,
  candidate_member_id)`). Orchestrator journal.

**2.2 Owner-roster helpers.** Representation IS `group_members` with
`member_role = 'owner'` + owner_* columns. New helpers in
`Simplex.Chat.Store.Groups` (I7 — `WireOwner` record):

```haskell
data WireOwner = WireOwner
  { woMemberId :: MemberId
  , woOwnerKey :: C.PublicKeyEd25519
  , woAuthSig  :: C.Signature 'C.Ed25519
  , woRcvKey   :: Maybe C.PublicKeyEd25519
  }

getChannelOwnerAuths     :: DB.Connection -> GroupId -> IO [OwnerAuth]
applyChannelOwnerRoster  :: DB.Connection -> GroupId -> [WireOwner] -> IO ()
markMemberAsOwner        :: DB.Connection -> GroupId -> MemberId
                         -> C.Signature 'C.Ed25519 -> Int
                         -> Maybe C.PublicKeyEd25519 -> IO ()
demoteOwner              :: DB.Connection -> GroupId -> MemberId -> IO ()
incrementLinkDataVersion :: DB.Connection -> GroupId -> IO Word64
setLinkDataRemoteVersion :: DB.Connection -> GroupId -> Word64 -> IO ()
getLinkDataVersions      :: DB.Connection -> GroupId -> IO (Word64, Word64)
reconstructOwnerAuthorizers
  :: C.PublicKeyEd25519
  -> [(MemberId, C.PublicKeyEd25519, C.Signature 'C.Ed25519)]
  -> [(MemberId, Maybe MemberId)]    -- pure; Nothing = root-signed
```

`getChannelOwnerAuths` materializes `OwnerAuth { ownerId = unMemberId
memberId, ownerKey = memberPubKey, authOwnerSig = ownerAuthSig }`
ordered by `owner_position`. **G6 invariant:** `OwnerAuth.ownerId` is
the raw bytes of `MemberId`; encode and decode assert this.

**G5 — `applyChannelOwnerRoster` ingest rule.** Unknown `woMemberId`
or local `member_pub_key` ≠ `woOwnerKey` → record as a **pending-member
row** (placeholder status, `member_role = 'owner'`, owner_* columns
populated verbatim from the wire). Do NOT coerce or drop — the chain
entry stays so the blob still verifies on re-encode; standard
member-info gossip reconciles later. Known `woMemberId` with matching
`woOwnerKey` → update `member_role`, `owner_auth_sig`, `owner_position`,
`owner_rcv_pub_key` (latter only when `woRcvKey` is `Just`; never
overwrite non-null with NULL). Local owner rows not in the wire list →
demote, clear owner_* columns.

**2.3 `groupLinkData` becomes IO (G2).**

```haskell
groupLinkData
  :: DB.Connection
  -> GroupInfo -> GroupLink -> [GroupRelay]
  -> IO (UserConnLinkData 'CMContact, CRClientData)
```

Reads owners via `getChannelOwnerAuths`; calls
`incrementLinkDataVersion`; embeds version in `GroupShortLinkData`
JSON. Signing key stays on the agent's `ShortLinkCreds` — not threaded
through. Enforce `length owners ≤ ownerChainDepth` before encode. Call
sites (both already inside a DB action): `setGroupLinkData`
(`Internal.hs:1306-1314`), `setGroupLinkDataAsync`
(`Internal.hs:1316-1322`).

**2.4 Owner-mesh transport.** Channel-scoped, fully-connected sub-graph
among owners. Reuses `x.grp.mem.intro` / `inv` / `fwd` at
`Subscriber.hs:2953-3070`. Five additions:

1. **`x.grp.owner.invite`** (I3) — A → B before the mesh edge exists,
   over A↔B's personal contact (or one-time link). `XGrpOwnerInvite {
   groupId :: B64UrlByteString, channelLink :: ConnReqContact }`. On
   accept, B establishes the channel-scoped direct mesh ContactConnection.
2. **`x.grp.owner.accept`** (I3) — B → A over the now-established
   mesh edge. `XGrpOwnerAccept { groupId :: B64UrlByteString,
   ownerPubKey :: C.PublicKeyEd25519, rcvPubKey ::
   SMP.RcvPublicAuthKey }`. A records both for Step 3.
3. **`x.grp.owner.creds`** — `XGrpOwnerCreds (CoOwnerCredsBundleEnvelope
   { groupId, bundle })`. A → B direct mesh only; never relay-forwarded.
   Receiver rejects unless `groupId` matches.
4. **`ownerRcvPubKey :: Maybe SMP.RcvPublicAuthKey`** — optional JSON
   field on `XGrpMemIntro`, `XGrpMemInv`, **and** `XGrpMemFwd` (I4; G1
   path 2; unknown-field tolerant). Populated **only** when the
   introducer is invoking Phase 3 Step 6; standard intros leave it
   `Nothing`. Per-edge flow for A introducing already-promoted B to
   existing owner X: intro A→X carries B's rcv pubkey (introduced
   peer's); inv X→A carries X's rcv pubkey (responder's); fwd A→B
   carries X's rcv pubkey (forwarded to introduced peer).
5. **`XGrpLinkSync`** for link-blob propagation (4.1). Does NOT carry
   rcv pubkeys — SMP rcv keys are fixed for the channel lifetime
   (mirrors 4.4 invariant); they travel only via path 2.

**Mesh-scoping acceptance rule (G1 (5), I5).** Receiver of
`XGrpMemIntro` / `Inv` / `Fwd` MUST verify both that `ownerRcvPubKey`
is present AND that the introducer is a current owner of that group
locally. If either fails, downgrade to a standard intro (drop the
field; existing handler applies). In the existing `xGrpMemIntro` /
`Inv` / `Fwd` handlers, add an early-branch gated by (`ownerRcvPubKey`
present ∧ introducer is owner). The branch creates the direct
connection and `channel_owner_mesh` row only; the standard flow runs
on the else-branch. B is already a known member; the intro creates a
new direct mesh edge between B and X without re-creating B's
chat-side member state. Test: `testMeshScopeRequiresOwnerIntroducer`.

`sendOwnerMeshMessage :: User -> GroupInfo -> [ChannelOwnerMesh] ->
ChatMsgEvent 'Json -> CM ()` walks connected mesh rows and reuses
`sendDirectMemberMessage`; skipped peers fall back to LGET-on-startup.

**2.4.1 Mesh receive allowlist (G9, I3).** A `Connection` referenced
by `channel_owner_mesh.direct_conn_id` (joined at message-receive
time) is mesh; otherwise standard chat allowlist applies. Allowed
events on mesh: `x.grp.owner.accept`, `x.grp.owner.creds`,
`x.grp.link.sync`, `x.grp.mem.role`, `x.grp.mem.intro`,
`x.grp.mem.inv`, `x.grp.mem.fwd`. Anything else over a mesh connection
is logged and rejected (visible in tests; includes `x.msg.new` — not
in this delivery). `XGrpOwnerInvite` is the only new chat-protocol
event delivered over the standard personal-contact path (pre-mesh);
it is registered in the chat-protocol parser like any other `x.*`
event but does not appear in the mesh allowlist. Test: roster
round-trip, mesh-allowlist rejection of `x.msg.new`.

---

### Phase 3 — Promote-to-owner orchestrator

A = existing owner; B = member to promote. Orchestrator owns one
`channel_promotion_in_progress` row per `(group, candidate)`. Each step
is idempotent; restart resumes via `resumePromotions`.

| Step | Marker | Action |
|---|---|---|
| 1 | `invitation_sent` | A creates a fresh channel-scoped direct ContactConnection, encodes `XGrpOwnerInvite { groupId, channelLink }`, sends over A↔B's personal contact (or one-time link). The `channelLink` is the new direct mesh link — not the channel link. |
| 2 | `creds_received` | B accepts, establishes the mesh edge, generates `rcvKeyPair` locally, reuses `memberPrivKey` as `ownerPrivKey`, returns `XGrpOwnerAccept { groupId, ownerPubKey, rcvPubKey }` over the mesh connection. A records both. |
| 3 | `rkey_done` | Run **Risk #12 LWW pre-flight** (re-LGET; refresh roster; map ownerIds to local `owner_rcv_pub_key`; wait for next mesh sync rather than push on missing mapping). Compute desired `recipientKeys` = current owners' rcv pubkeys ∪ {B.rcvPubKey}; `setQueueRecipientKeys`. |
| 4 | `lset_done` | Construct `OwnerAuth_B = (B.memberId, ownerPubKey, sign(A.ownerPrivKey, memberId ‖ encodePubKey ownerPubKey))`; `markMemberAsOwner`; call `groupLinkData` (bumps version); `setGroupLinkData`. |
| 5 | `bundle_sent` | `XGrpOwnerCreds(bundle)` over the mesh connection. Bundle carries A's `ownerRcvPubKey` (G1 path 1). |
| 6 | `mesh_introduced` | For each existing owner X (≠ A, ≠ B), drive `x.grp.mem.intro` / `inv` / `fwd` with `ownerRcvPubKey` populated on all three messages (G1 path 2). Receivers apply the **mesh-scoped variant** (I5): B is already a known member; the intro creates only the new direct mesh edge between B and X plus a `channel_owner_mesh` row, without touching B's chat-side member state. Each completed edge → `status = 'connected'`. |
| 7 | `role_announced` | Sign + broadcast `x.grp.mem.role` via relays; broadcast `XGrpLinkSync` over the mesh. |

**Idempotency.** Step 3 skips if B's rcv key is already in the desired
set after pre-flight; Step 4 LGETs and skips if B's owner row already
chain-verifies; Step 5 keys on the journal step (+ at-least-once
delivery + receiver dedup); Step 6 keys on per-peer mesh state; Step 7
relies on `x.grp.mem.role` idempotence.

**Failure modes.** 3→4 fail: stale rcv key sits in queue's set; Step 3
retry detects, Step 4 retries cleanly. 4→5 fail: B is on-server owner
without credentials — "owner credentials missing — request resend"
surfaces via next LGET. 6 partial: retry per-peer at next app start. 7
fail: subscribers see B as non-owner temporarily.

```haskell
-- Simplex.Chat.Library.Owners  (new module)
promoteToOwner   :: User -> GroupInfo -> GroupMember -> CM ()
resumePromotions :: CM ()     -- wired into Simplex.Chat.Core startup
```

Tests: `testPromoteOwner`, `testPromoteOwnerResumeStep<N>`,
`testPromoteIdempotentRetry`.

---

### Phase 4 — Cross-owner sync, conflict resolution, owner removal

**4.1 `x.grp.link.sync`** — mesh-only.

```haskell
XGrpLinkSync :: GrpLinkSync -> ChatMsgEvent 'Json
data GrpLinkSync = GrpLinkSync
  { groupId :: B64UrlByteString, linkDataVersion :: Word64
  , encMutableData :: B64UrlByteString }            -- raw md_bytes
```

Receiver verifies `groupId`; decodes `md_bytes`; runs
`validateLinkOwners` against `groupRootPubKey (groupRootKey
groupKeys)`; if `linkDataVersion > local`, applies the roster and sets
`link_data_remote_version`. No rcv pubkeys (G1 (3)).

**4.2 LWW reconciliation.** Per device: `link_data_version` (highest
written), `link_data_remote_version` (highest seen). On roster mutation:
LGET; if `serverV > remote`, swap local roster for the server's and
re-apply in-memory pending mutations; `newV := max(local, serverV) + 1`;
encode + LSET; broadcast `XGrpLinkSync(newV)`; bounded retry (max 3)
on contention. Non-roster mutations may use optimistic-without-LGET.
Test: `testLWWConflictDetection`, `testRosterRaceRetry`,
`testStaleBlobIgnored`.

**4.3 Owner removal.** Pure planner

```haskell
planOwnerRemoval
  :: C.PublicKeyEd25519
  -> [(MemberId, C.PublicKeyEd25519, C.Signature 'C.Ed25519)]
  -> MemberId -> MemberId
  -> Either String OwnerRemoval     -- Left when remover's chain depends on removee
```

Forward pass over the prefix-ordered list: any owner whose authorizer
is in the removed-set is itself removed; remover in cascade ⇒ `Left`.

Steps: compute `OwnerRemoval` → bump version, LSET (cascade pruned
writer-side) → recompute `recipientKeys` from `owner_rcv_pub_key`
minus removed → `setQueueRecipientKeys` (preceded by Risk #12
pre-flight) → broadcast `x.grp.mem.role` via relays for each removed
owner → broadcast `XGrpLinkSync` over mesh → close
`channel_owner_mesh` rows for removed peers and delete their direct
connections via `deleteAgentConnectionsAsync'`. D3 surfaces blockage
with "You cannot remove the channel creator — your owner role was
authorized by them." Single-owner channels: the sole root-signed owner
cannot be removed. Tests: `testCascadeRemoval`,
`testRemoverCascadeBlocked`, `testRootCannotBeRemovedSoloOwner`,
`testCascadeUiPreviewShowsAll`.

**4.4 Single-owner → multi-owner upgrade (I1).** Two-part backfill.
Chain fields (`owner_auth_sig`, `owner_position`) are filled by the
eager SQL migration in 2.1 — once per chat DB at upgrade.
`owner_rcv_pub_key` for the creator's own row is filled by a one-time
startup backfill in `Simplex.Chat.Library.Owners`, wired into
`Simplex.Chat.Core` startup: iterate groups whose creator owner row
has `owner_auth_sig IS NOT NULL` and `owner_rcv_pub_key IS NULL`;
query the agent via a new `getChannelLinkRcvPubKey :: AgentClient ->
ConnId -> AE SMP.RcvPublicAuthKey` (derives the pubkey from
`rcvPrivateKey` stored in the `RcvQueue` row, accessible via the
agent's existing connection-by-connId lookup; no new SMP command);
write it. Idempotent; one pass per device.
Post-upgrade the **member-key = owner-key invariant** holds and must
continue to hold; member signing keys are fixed for the channel's
lifetime. Tests: `testUpgradeCreatorOwnerRowBackfilled`,
`testCreatorRcvPubKeyBackfilledAtStartup`, `testMemberKeyEqualsOwnerKey`.

**4.5 Concurrent RKEY recovery (I2).** Dropped candidate's first LSET
returns `ERR AUTH`; their device emits a chat-layer event prompting
any current owner to re-run the Risk #12 LWW pre-flight (which
recomputes the desired `recipientKeys` from the current `OwnerAuth`
chain plus locally-known `owner_rcv_pub_key` mappings) and call
`setQueueRecipientKeys`. Other owners already learned every owner's
rcv pubkey via the path-2 mesh-bootstrap exchange; **no new key
transport is required**, and `XGrpLinkSync` continues to carry no rcv
pubkeys. Converges within one mesh round-trip. Test:
`testConcurrentRKEYConvergence`, `testStaleMeshRKEYWaitsForMeshSync`
(Risk #12).

---

### Phase 5 — UI (iOS + Kotlin multiplatform)

| ID | Screen | iOS file | Kotlin file |
|---|---|---|---|
| D1 | Channel settings → Owners section | `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift` (edit) | `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt` (edit) |
| D1' | Owners list screen | `…/Group/Owners/ChannelOwnersView.swift` (new) | `…/group/owners/ChannelOwnersView.kt` (new) |
| D2 | Add owner — member picker + invite | `…/Group/Owners/AddChannelOwnerView.swift` (new) | `…/group/owners/AddChannelOwnerView.kt` (new) |
| D3 | Remove owner — cascade preview | `…/Group/Owners/RemoveChannelOwnerSheet.swift` (new) | `…/group/owners/RemoveChannelOwnerSheet.kt` (new) |
| D4 | Sync indicator banner | inline in `GroupChatInfoView.swift` | inline in `GroupChatInfoView.kt` |
| D5 | Single-owner upgrade prompt | silent — Add owner always visible to owners | silent |
| D6 | Owner-chat groundwork | `// TODO: surface owner-chat UI` in `ChannelOwnersView.swift` | same marker in `ChannelOwnersView.kt` |

Chat-side RPCs: `apiPromoteToOwner`, `apiRemoveOwner`,
`apiPlanRemoveOwner` (runs `planOwnerRemoval` without committing),
`apiCancelPromotion`, `apiChannelOwners`. UI-shaped `ChannelOwner` on
each side: `memberId`, `displayName`, `ownerPosition`,
`authorizedByMemberId :: Maybe MemberId` (computed from
`reconstructOwnerAuthorizers`; `Nothing` ⇒ root-signed).

Strings: iOS `Localizable.strings` (+ each `SimpleX Localizations`
mirror); Kotlin `MR/base/strings.xml`. ~14 new strings; translations
in a follow-up. Layout per `layout-swift.md` / `layout-compose.md`.

---

### Phase 6 — Tests + threat-model regressions

Files: `tests/ChatTests/ChannelTests.hs` (extended);
`simplexmq/tests/AgentTests/{ShortLinkTests.hs, FunctionalAPITests.hs}`
(extended); new `tests/ChatTests/MultiOwnerTests.hs`. UI snapshot
(iOS) + screenshot (Compose) tests for `ChannelOwnersView` and
`RemoveChannelOwnerSheet`. Test names listed inline per phase and in
the Risk register.

---

## 4. Risk register

| # | Risk | Mitigation | Test |
|---|---|---|---|
| 1 | Hard-break for older clients on multi-owner channels. | agentVRange bump (1.7); release notes; channels without promotions remain readable. | `testOldClientRejectsChainedOwnerBlob`; `testOldClientReadsRootSignedBlob`. |
| 2 | Promotion atomicity — mid-flow process death leaves partial state. | `channel_promotion_in_progress` journal + idempotent steps + `resumePromotions` on startup. | `testPromoteOwnerResumeStep<N>`. |
| 3 | LWW data loss in concurrent roster edits. | Optimistic retry on roster mutations (4.2); D4 banner; bounded retries error to UI on persistent contention. | `testRosterRaceRetry`. |
| 4 | Cascade-removal UX clarity. | D3 explicit list; remove disabled when remover's chain depends on removee. | `testCascadeUiPreviewShowsAll`. |
| 5 | Co-owner credential bundle confidentiality. | Bundle sent only over the channel-scoped direct mesh connection (E2E ratchet); cross-channel replay rejected by `groupId` check at receive. **CI lint (G10):** grep step that fails if any `logInfo`/`logDebug`/`putStrLn`/`hPutStrLn` site references `CoOwnerCredsBundle` or `XGrpOwnerCreds`. | `testBundleCrossChannelReject`; `testBundleReplayRejected`; CI lint. |
| 6 | Member-key = owner-key invariant violated by future refactor. | `testMemberKeyEqualsOwnerKey` runs on every channel-creation and promotion. | `testMemberKeyEqualsOwnerKey`. |
| 7 | Chain-cycle attempts. | `validateLinkOwners` is prefix-only — structurally cycle-free; depth cap of 8. | `testChainCycleStructurallyImpossible`; `testChainTooLong`. |
| 8 | Co-owner DELs the queue. | Chat-layer gate: `DEL` allowed only when this device holds `GRKPrivate _`. | `testCoOwnerCannotDeleteQueue`. |
| 9 | Server returns stale blob during reconciliation. | Treat `linkDataVersion` as authoritative if signed; if monotonicity violated, surface "channel state inconsistent" and skip the write. | `testStaleBlobIgnored`. |
| 10 | Concurrent RKEY race drops a candidate. | Eventual consistency via mesh recovery (4.5). | `testConcurrentRKEYConvergence`. |
| 11 | Member-pub-key rotation breaks owner status. | Invariant test + documentation (4.4); future rotation feature must rewrite the roster in-transaction. | `testMemberKeyEqualsOwnerKey`. |
| 12 | **Stale-mesh-view RKEY race (G4).** Owner pushes RKEY based on a roster diverged from the server's blob; either a freshly-promoted peer's rcv pubkey is missing locally, or a removed peer's lingers. | Before every `setQueueRecipientKeys`, run LWW reconciliation on the link blob first (re-LGET; refresh roster from blob's `OwnerAuth` list; map ownerIds to local `owner_rcv_pub_key`; retry on mismatch). If a mapping is missing locally for a freshly chain-validated owner, **wait for the next mesh sync rather than push** — bounded backoff; surface to UI on persistent failure. | `testStaleMeshRKEYWaitsForMeshSync`. |

### Threat-model alignment

Channels-overview.md objectives 1-7 preserved. Objective 6 (sender
anonymity within multi-owner channels) is *strengthened* — observers
with the link key see only that some chain-valid owner pushed an
update, not which one. New attack surface (owner mesh) is direct,
E2E-encrypted (double-ratchet), channel-scoped, invisible to
subscribers and relays. Threat-model regressions:
`testRelayCannotForgeOwners`, `testBundleCrossChannelReject`,
`testBundleReplayRejected`, `testSignatureRequired` (extended with
chained-owner cases), `testMeshScopeRequiresOwnerIntroducer`.

---

## 5. Out of scope

- Legacy private (P2P) groups.
- Multisig and programmable governance.
- Relay-mediated promotion.
- Pre-signing N≥2 root-signed owners at channel creation for creator
  anonymity.
- Transfer of root creator.
- Owner-only chat UI surface; mesh content (`x.msg.new` etc.) is NOT
  persisted.
- Owner-mesh push catchup for offline owners (LGET-on-startup is MVP).
- Public-groups-over-relays migration.
- Member-pub-key rotation while a member is an owner.
- Agent-side `linkRootSigKey` persistence; TODO at `AgentStore.hs:2514`
  stays untouched.
