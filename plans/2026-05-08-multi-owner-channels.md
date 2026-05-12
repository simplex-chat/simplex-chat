# Multi-owner Channels — implementation plan

2026-05-12 · Target: SimpleX Channels v7 (full-trust, any-owner-decides)

> **DESIGN DECISION REQUIRED — discuss with team before implementation.**
> Maximum number of owners per channel: proposed cap of 8 (default
> `ownerChainDepth`). Each `OwnerAuth` is ~189 B; 8 owners ~1.5 KB of the
> 13.4 KB user-data padded budget; O(N²) ≈ 64 Ed25519 verifies on decode.
> One-line constant change — confirm before implementing.

> **Constraint up front.** Promoting a subscriber to owner via a
> relay-mediated offer (no direct connection between existing owner and
> candidate) is not supported in this delivery. Promotion always uses a
> fresh, channel-scoped direct mesh connection.

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

**1.0 Preflight audit.** Audit chat-side write sites of
`group_members.member_pub_key` (`grep "member_pub_key.*=" /
"updateMember.*key"`). Add a runtime guard or comment at each
confirming the row is NOT a `member_role = 'owner'` row at write
time. Preserves the member-key = owner-key invariant (Risks #6, #11)
at runtime, not just at creation/promotion.

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

**1.3 Co-owner RcvQueue.** Each device holds one `rcv_queues`
row for the shared channel link queue with its own `rcv_private_key`
+ `ShortLinkCreds`. Schema unchanged: `PRIMARY KEY (host, port,
rcv_id)` (`agent_schema.sql:66`) is per-database — two profiles on
one device cannot both co-own a channel via that device. Chat-layer
gate: `DEL`, `OFF`, `NKEY`, `NDEL` allowed only when this device
holds `GRKPrivate _`. All `SRecipient` commands authorize via any-of-N
(`Server.hs:1248-1249`), so the gate must apply uniformly. Test:
`testCoOwnerCannotDeleteQueue`.

**1.4 RKEY agent wrapper.** Single primitive — full key list per call.

```haskell
setQueueRecipientKeys
  :: AgentClient -> NetworkRequestMode -> ConnId
  -> NonEmpty SMP.RcvPublicAuthKey -> AE ()
```

Concurrent races recovered by Phase 4.5 + Risk #12 pre-flight.

**1.5 Co-owner credential bundle.** New `CoOwnerCredsBundle` in
`Simplex.Messaging.Agent.Protocol`. Fields: `server`, `rcvId`,
`sndId :: SMP.SenderId` (queue-level, shared across all co-owners;
satisfies `rcv_queues.snd_id NOT NULL` at `agent_schema.sql:41`), `rcvDhSecret`, `shortLinkId`, `shortLinkKey`,
`rootPubKey`, `linkEncFixedData`, `agentVRange`, **`ownerRcvPubKey ::
SMP.RcvPublicAuthKey`** (A's own SMP rcv pubkey, delivered one-off
to B at first-promotion bootstrap; chat writes it to
`group_members.owner_rcv_pub_key` for A on B's device at intake). B's `rcvPrivateKey` is generated locally
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

`acceptCoOwnerCreds` generates B's `e2e_priv_key` locally via
`C.generateKeyPair` (link queues are LGET-only; a fresh local key
satisfies the NOT-NULL at `agent_schema.sql:39` without leaking A's
secret), then constructs B's `rcv_queues` row: `queue_mode =
QMContact`; `link_priv_sig_key` = B's owner private key (=
`memberPrivKey`); `link_id`/`link_key`/`link_enc_fixed_data`/`rcv_dh_secret`/`snd_id`
from the bundle; B's locally-generated keys for remaining NOT-NULL
columns. **Intake idempotency:** if B's local `rcv_queues` row
+ chat-side co-owner state already exist for this `groupId`, intake
is a no-op and re-emits `XGrpOwnerCredsAck`. This handles A's retry
when an earlier ACK was lost.

**Bundle integrity.** Before persisting, B decrypts
`linkEncFixedData` with `shortLinkKey`, decodes `FixedLinkData`, and
verifies `fixedData.rootKey == bundle.rootPubKey`. Mismatch ⇒ reject.
Test: `testBundleRootKeyMismatchRejected`.

**Subscription policy.** Co-owners do NOT SUB the channel link
queue; updates pulled via LGET on demand and at startup. Avoids the
multi-recipient DH-secret problem (one `rcv_dh_secret` shared across
N owners would let each decrypt the others' SUB traffic).
`rcvDhSecret` carried for forward compat with a future SUB-based path;
SUB-based catchup is post-MVP.

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

- `M<DATE>_group_members_owner_fields` — DDL only: `group_members`
  += `owner_auth_sig BLOB`, `owner_position INTEGER`, `owner_rcv_pub_key
  BLOB` (nullable); `groups` += `link_data_version`,
  `link_data_remote_version` (`INTEGER NOT NULL DEFAULT 0`). Creator-row
  backfill (chain fields **and** `owner_rcv_pub_key`) runs Haskell-side
  at startup (4.4); SQL migration is DDL only because the framework
  type is `Migration { up :: Text }` (`Agent/Store/Shared.hs:30-32`) and
  cannot compute Ed25519 signatures or read the agent DB.
- `M<DATE>_owner_mesh` — `channel_owner_mesh` (`channel_owner_mesh_id`,
  `group_id`, `peer_group_member_id`, `direct_conn_id`, `status TEXT`,
  timestamps; `UNIQUE(group_id, peer_group_member_id)`; FKs to
  groups/group_members/connections). `status` ∈ {`'pending'`,
  `'connected'`, `'closed'`}. Rows are **not deleted** on owner removal;
  status transitions to `'closed'` and the referenced `direct_conn_id`
  is invalidated via `deleteAgentConnectionsAsync'`. Preserved for
  audit and to avoid race-window re-establishment from a stale intro.
- `M<DATE>_promotion_in_progress` — `channel_promotion_in_progress`
  (`promotion_id`, `group_id`, `candidate_member_id`,
  `candidate_pub_key`, `candidate_rcv_pub_key`, `step TEXT`,
  `direct_conn_id`, `last_error`, timestamps; `UNIQUE(group_id,
  candidate_member_id)`). Orchestrator journal.

**2.2 Owner-roster helpers.** Representation IS `group_members` with
`member_role = 'owner'` + owner_* columns. New helpers in
`Simplex.Chat.Store.Groups`:

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

**2.3 `groupLinkData` becomes IO.**

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
`Subscriber.hs:2953-3070`. All wire `groupId :: B64UrlByteString`
fields below (`XGrpOwnerInvite`, `XGrpOwnerAccept`, `XGrpOwnerCreds`,
`XGrpOwnerCredsAck`, `XGrpLinkSync`) carry the channel's
`publicGroupId` — group identity = sha256(genesis root key),
immutable (per `Types.hs:790`). Six additions:

1. **`x.grp.owner.invite`** — A → B before the mesh edge
   exists, over A↔B's personal contact (or one-time link).
   `XGrpOwnerInvite { groupId :: B64UrlByteString, meshConnReq ::
   ConnReqContact }`. On accept, B establishes the channel-scoped
   direct mesh ContactConnection from `meshConnReq` (renamed from
   `channelLink`, which was misleading — this is the new direct mesh
   link, not the channel link).
2. **`x.grp.owner.accept`** — B → A over the now-established
   mesh edge. `XGrpOwnerAccept { groupId :: B64UrlByteString,
   ownerPubKey :: C.PublicKeyEd25519, rcvPubKey ::
   SMP.RcvPublicAuthKey }`. A records both for Step 3.
3. **`x.grp.owner.creds`** — `XGrpOwnerCreds (CoOwnerCredsBundleEnvelope
   { groupId, bundle })`. A → B direct mesh only; never relay-forwarded.
   Receiver rejects unless `groupId` matches. **Sender verification:**
   receiver additionally verifies the mesh sender is the
   orchestrator A — i.e. the device with the matching
   `channel_promotion_in_progress` row identifying that sender as the
   promoter. `XGrpOwnerCreds` from any other mesh peer is rejected even
   when `groupId` matches; prevents same-channel cross-peer bundle
   replay or impersonation.
4. **`x.grp.owner.creds.ack`** — B → A over the mesh edge,
   after `acceptCoOwnerCreds` succeeds and local state is persisted.
   `XGrpOwnerCredsAck { groupId :: B64UrlByteString }`. A's
   orchestrator advances `step = bundle_sent` only on receipt;
   re-emitted by B on intake idempotency (1.5).
5. **`ownerRcvPubKey :: Maybe SMP.RcvPublicAuthKey`** — optional JSON
   field on `XGrpMemIntro`, `XGrpMemInv`, **and** `XGrpMemFwd`
   (unknown-field tolerant). Populated **only** when the
   introducer is invoking Phase 3 Step 6; standard intros leave it
   `Nothing`. Per-edge flow for A introducing already-promoted B to
   existing owner X: intro A→X carries B's rcv pubkey (introduced
   peer's); inv X→A carries X's rcv pubkey (responder's); fwd A→B
   carries X's rcv pubkey (forwarded to introduced peer).
6. **`XGrpLinkSync`** for link-blob propagation (4.1). Does NOT carry
   rcv pubkeys — SMP rcv keys are fixed for the channel lifetime
   (mirrors 4.4 invariant); they travel only via path 2.

**Mesh-scoping acceptance rule.** Receiver of
`XGrpMemIntro` / `Inv` / `Fwd` MUST verify both that `ownerRcvPubKey`
is present AND that the introducer is a current owner of that group
locally. If either fails, downgrade to the standard intro path
(drop the field; existing handler applies). When both hold, each
handler takes an early-branch (the mesh edge IS the chat-layer
ratchet connection that modern group members already share;
`directConnReq` stays `Nothing` — legacy, unused in modern channels):

- **`xGrpMemIntro`** — skip the `GCHostMember` check
  (`Subscriber.hs:2956, 2986`) and the chat-side member-creation writes
  (`createIntroReMember`, `updatePreparedChannelMember`,
  `createNewGroupMember`); reuse `groupConnReq` for the connection;
  add a `channel_owner_mesh` row.
- **`xGrpMemInv`** — the existing handler requires `GCInviteeMember`
  (`Subscriber.hs:3004`); skip this check; forward the
  `IntroInvitation` to B via `XGrpMemFwd` carrying X's `ownerRcvPubKey`.
- **`xGrpMemFwd`** — reuse `joinAgentConnectionAsync` on `groupConnReq`;
  skip both `createNewGroupMember` (missing-member branch) and
  `createIntroToMemberContact` (normal-group chat-side state); add the
  `channel_owner_mesh` row.

Else-branch runs the standard flow. B is already a known member; the
intro creates a new direct mesh edge between B and X without
re-creating B's chat-side state. Test:
`testMeshScopeRequiresOwnerIntroducer`.

**Transient roster lag.** If the introducer is not yet visible
as an owner in the receiver's local roster (transient lag after a
promotion), the receiver downgrades to the standard intro path,
which then rejects (sender is not `GCHostMember`). The introducer's
chat-protocol retry mechanism re-attempts the intro after the
receiver's next LGET refresh propagates the updated roster. No
additional mechanism required.

`sendOwnerMeshMessage :: User -> GroupInfo -> [ChannelOwnerMesh] ->
ChatMsgEvent 'Json -> CM ()` walks connected mesh rows and reuses
`sendDirectMemberMessage`; skipped peers fall back to LGET-on-startup.

**2.4.1 Mesh receive allowlist.** A `Connection` referenced
by `channel_owner_mesh.direct_conn_id` (joined at message-receive
time) is mesh; otherwise standard chat allowlist applies. Allowed
events on mesh: `x.grp.owner.accept`, `x.grp.owner.creds`,
`x.grp.owner.creds.ack`, `x.grp.link.sync`, `x.grp.mem.role`,
`x.grp.mem.intro`, `x.grp.mem.inv`, `x.grp.mem.fwd`. Anything else
over a mesh connection is logged and rejected (visible in tests;
includes `x.msg.new` — not
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
| 1 | `invitation_sent` | A creates a fresh channel-scoped direct ContactConnection, encodes `XGrpOwnerInvite { groupId, meshConnReq }`, sends over A↔B's personal contact (or one-time link). `meshConnReq` is the new direct mesh link — not the channel link. |
| 2 | `creds_received` | B accepts, establishes the mesh edge, generates `rcvKeyPair` locally, reuses `memberPrivKey` as `ownerPrivKey`, returns `XGrpOwnerAccept { groupId, ownerPubKey, rcvPubKey }` over the mesh connection. A records both. |
| 3 | `rkey_done` | Run **Risk #12 LWW pre-flight** (re-LGET; refresh roster; map ownerIds to local `owner_rcv_pub_key`; wait for next mesh sync rather than push on missing mapping). Compute desired `recipientKeys` = current owners' rcv pubkeys ∪ {B.rcvPubKey}; `setQueueRecipientKeys`. |
| 4 | `lset_done` | Construct `OwnerAuth_B = (B.memberId, ownerPubKey, sign(A.ownerPrivKey, memberId ‖ encodePubKey ownerPubKey))`; `markMemberAsOwner`; call `groupLinkData` (bumps version); `setGroupLinkData`. |
| 5 | `bundle_sent` | `XGrpOwnerCreds(bundle)` over the mesh connection. Bundle carries A's `ownerRcvPubKey` (first-promotion delivery to B). **Marker idempotency:** the `bundle_sent` journal marker is written ONLY after `XGrpOwnerCredsAck` is received from B (a small mesh event B sends on successful intake), not after the agent's send-queue submission. Prevents the case where the bundle is lost in transit but the journal advances, leaving B permanently uncredentialed across orchestrator restarts. |
| 6 | `mesh_introduced` | For each existing owner X (≠ A, ≠ B), drive `x.grp.mem.intro` / `inv` / `fwd` with `ownerRcvPubKey` on all three. Each of X's three handlers takes its mesh-scoped early-branch (see Phase 2.4 for the per-handler skip list). The standard `groupConnReq` ratchet path establishes the mesh edge; `directConnReq` stays `Nothing`. Completed edge → `channel_owner_mesh.status = 'connected'`. |
| 7 | `role_announced` | Sign + broadcast `x.grp.mem.role` via relays; broadcast `XGrpLinkSync` over the mesh. |

**Idempotency.** Step 3 skips if B's rcv key is already in the desired
set after pre-flight; Step 4 LGETs and skips if B's owner row already
chain-verifies; Step 5 advances its marker only on
`XGrpOwnerCredsAck` so a lost bundle re-sends rather than
silently dropping; Step 6 keys on per-peer mesh state; Step 7 relies
on `x.grp.mem.role` idempotence.

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
`link_data_remote_version`. Does not carry rcv pubkeys.

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
owner → broadcast `XGrpLinkSync` over mesh → transition
`channel_owner_mesh.status` to `'closed'` for removed peers (rows
preserved) and invalidate their `direct_conn_id` via
`deleteAgentConnectionsAsync'`. D3 surfaces blockage
with "You cannot remove the channel creator — your owner role was
authorized by them." Single-owner channels: the sole root-signed owner
cannot be removed. Tests: `testCascadeRemoval`,
`testRemoverCascadeBlocked`, `testRootCannotBeRemovedSoloOwner`,
`testCascadeUiPreviewShowsAll`.

**4.4 Single-owner → multi-owner upgrade.** Single
unified startup pass in `Simplex.Chat.Library.Owners`, wired into
`Simplex.Chat.Core` startup. SQL migration in 2.1 adds columns only;
all backfill is Haskell-side. The pass iterates groups with non-null
`member_priv_key` and a creator row lacking `owner_auth_sig`, and
for each: sets `member_role = 'owner'`; writes `owner_auth_sig =
sign(rootPrivKey, memberId ‖ encodePubKey(publicKey memberPrivKey))`;
writes `owner_position = 0`; queries the agent via
`getChannelLinkRcvPubKey :: AgentClient -> ConnId -> AE
SMP.RcvPublicAuthKey` (derives the pubkey from `rcvPrivateKey` stored
in the `RcvQueue` row via the existing connection-by-connId lookup;
no new SMP command) and writes `owner_rcv_pub_key`. Idempotent; one
pass per device.

**Synchronicity.** The pass runs **synchronously** during
`Simplex.Chat.Core` startup, completing before any chat-side RPC
touching a channel link blob (`apiPromoteToOwner`, `apiRemoveOwner`,
`setGroupLinkData`, …) can execute. Fallback if a build makes it
unavoidably async: when the missing-mapping owner is the **local
device's own row**, the Risk #12 pre-flight derives on-the-fly via
`getChannelLinkRcvPubKey` rather than waiting for mesh sync.

Post-upgrade the **member-key = owner-key invariant** holds and must
continue to hold; member signing keys are fixed for the channel's
lifetime. Tests: `testUpgradeCreatorOwnerRowBackfilled`,
`testCreatorRcvPubKeyBackfilledAtStartup`, `testMemberKeyEqualsOwnerKey`.

**4.5 Concurrent-promotion race recovery.** Owners A, A'
may race-promote candidates B, B'. Chain (LSET) and recipientKeys
(RKEY) are independent server writes with no atomicity, giving four
outcomes — two clean (A/A or A'/A') and two mixed (chain by one,
RKEY by the other):

- **Case 3, RKEY race lost** (chain by A, RKEY by A'). B's first
  LSET returns `ERR AUTH`; B's device emits a chat-layer event
  prompting any current owner to re-run the Risk #12 pre-flight
  (recomputes desired `recipientKeys` from the current `OwnerAuth`
  chain ∪ locally-known `owner_rcv_pub_key`) and call
  `setQueueRecipientKeys`. Other owners already know every rcv
  pubkey via path-2 bootstrap; no new key transport, `XGrpLinkSync`
  continues to carry no rcv pubkeys. Converges in one mesh round.
- **Case 4, chain race lost** (chain by A', RKEY by A). During any
  current owner's next pre-flight, the chain is re-derived from the
  fresh blob; any rcv pubkey in `recipientKeys` not matching a
  chain-validated owner is dropped, and `setQueueRecipientKeys`
  writes the corrected list. The dropped party (B) is informed via
  `XGrpLinkSync` over the A↔B mesh edge (the only mesh edge B
  holds — Step 6 hasn't introduced B to other owners yet). On
  receipt B applies the roster, finds itself absent, demotes
  locally (clears its own `owner_*` columns and chat-side co-owner
  role) and tears down its local channel-link `rcv_queues` row via
  the agent. **Cooperative assumption:** recovery here assumes
  B is cooperative on receipt of `XGrpLinkSync`. A misbehaving B
  holding SMP write access until another owner's pre-flight could
  re-LSET a blob restoring itself to the chain (its `OwnerAuth_B`
  remains valid as long as A is in the chain). Acceptable under the
  full-trust model; the next pre-flight by any other honest owner
  resolves it. See Risk #10.

Tests: `testConcurrentRKEYConvergence` (case 3),
`testChainRaceLoserDemotes` (case 4),
`testStaleMeshRKEYWaitsForMeshSync` (Risk #12).

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
| 5 | Co-owner credential bundle confidentiality. | Bundle sent only over the channel-scoped direct mesh connection (E2E ratchet); cross-channel replay rejected by `groupId` check at receive. **CI lint:** grep step that fails if any `logInfo`/`logDebug`/`putStrLn`/`hPutStrLn` site references `CoOwnerCredsBundle` or `XGrpOwnerCreds`. | `testBundleCrossChannelReject`; `testBundleReplayRejected`; CI lint. |
| 6 | Member-key = owner-key invariant violated by future refactor. | `testMemberKeyEqualsOwnerKey` runs on every channel-creation and promotion. | `testMemberKeyEqualsOwnerKey`. |
| 7 | Chain-cycle attempts. | `validateLinkOwners` is prefix-only — structurally cycle-free; depth cap of 8. | `testChainCycleStructurallyImpossible`; `testChainTooLong`. |
| 8 | Co-owner disrupts queue via `DEL`/`OFF`/`NKEY`/`NDEL` (any-of-N at `Server.hs:1248-1249`). | Chat-layer gate: each command allowed only when this device holds `GRKPrivate _`. | `testCoOwnerCannotDeleteQueue`. |
| 9 | Server returns stale blob during reconciliation. | Treat `linkDataVersion` as authoritative if signed; if monotonicity violated, surface "channel state inconsistent" and skip the write. | `testStaleBlobIgnored`. |
| 10 | Concurrent RKEY race drops a candidate. | Eventual consistency via mesh recovery (4.5). Case 4 relies on the chain-loser's cooperation; persistent misbehavior is bounded by repeated pre-flight by any honest owner. | `testConcurrentRKEYConvergence`. |
| 11 | Member-pub-key rotation breaks owner status. | Invariant test + documentation (4.4); future rotation feature must rewrite the roster in-transaction. | `testMemberKeyEqualsOwnerKey`. |
| 12 | **Stale-mesh-view RKEY race.** Owner pushes RKEY based on a roster diverged from the server's blob; either a freshly-promoted peer's rcv pubkey is missing locally, or a removed peer's lingers. | Before every `setQueueRecipientKeys`, run LWW reconciliation on the link blob first (re-LGET; refresh roster from blob's `OwnerAuth` list; map ownerIds to local `owner_rcv_pub_key`; retry on mismatch). If a mapping is missing locally for a freshly chain-validated owner, **wait for the next mesh sync rather than push** — bounded backoff; surface to UI on persistent failure. | `testStaleMeshRKEYWaitsForMeshSync`. |

### Threat-model alignment

Channels-overview.md objectives 1-7 preserved. Objective 6 (sender
anonymity within multi-owner channels) is *strengthened* — observers
with the link key see only that some chain-valid owner pushed an
update, not which one. New attack surface (owner mesh) is direct,
E2E-encrypted (double-ratchet), channel-scoped, invisible to
subscribers and relays. **Convergence:** guaranteed in
O(mesh round-trip) under cooperative owners and eventual mesh
connectivity; adversarial or persistently offline owners can
prolong divergence indefinitely (out of scope under the full-trust
model). Threat-model regressions:
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
- Multi-device co-ownership for one profile:
  `group_members.owner_rcv_pub_key` is single-valued per
  `(group_id, member_id)`; multiple devices per profile would each
  hold a different rcv pubkey. Out of scope.
- Agent-side `linkRootSigKey` persistence; TODO at `AgentStore.hs:2514`
  stays untouched.
