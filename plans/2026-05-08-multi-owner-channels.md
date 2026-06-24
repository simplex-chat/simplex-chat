# Multi-owner Channels — implementation plan

2026-05-12 · Target: SimpleX Channels v7 (full-trust, any-owner-decides)

> **DESIGN DECISION REQUIRED — discuss with team before implementation.** Maximum number of owners per channel: proposed cap of 8 (default `ownerChainDepth`). Each `OwnerAuth` is ~189 B; 8 owners ~1.5 KB of the 13.4 KB user-data padded budget; O(N²) ≈ 64 Ed25519 verifies on decode. One-line constant change — confirm before implementing.

> **Constraint up front.** Promoting a subscriber to owner via a relay-mediated offer (no direct connection between existing owner and candidate) is not supported in this delivery. Promotion always uses a fresh, channel-scoped direct mesh connection.

---

## 1. Summary

Extend SimpleX Channels from single-owner to "any-owner-decides". Owners share an SMP recipient queue, each holds an Ed25519 owner private key, each can independently push mutable-blob updates (LSET) and rotate the recipient-key set (RKEY). A channel-scoped owner mesh (modeled on `x.grp.mem.intro`) propagates link-data changes; last-writer-wins on the SMP server resolves contention. Out of scope: multisig, programmable governance, public-group migration over relays, root-creator transfer, owner-only chat UI surface.

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

Most verification is upstream already: `validateLinkOwners` (prefix-only chain, duplicate detection), `decryptLinkData` (chain-aware), `encodeSignUserData` (signer-agnostic), server-side any-of-N on `recipientKeys`, `RKEY` restricted to `QMContact`.

**1.0 Preflight audit.** Audit chat-side write sites of `group_members.member_pub_key`. The invariant is `member_pub_key == chain-known ownerKey` for owner rows. Guard against writes that DIVERGE the two, not against all writes to owner rows. The relay-side ingest at `Store/Groups.hs:1893-1899` (`updateRelayGroupKeys`) legitimately writes `member_pub_key = ownerKey` from received link data and is correct as written; it needs no guard. Preserves Risks #6, #11 at runtime, not just at creation/promotion.

**1.1 Chain depth cap.** New `ownerChainDepth = 8` in `Simplex.Messaging.Agent.Protocol`; `validateLinkOwners` rejects `length owners > ownerChainDepth`; chat-side mirrors at encode (2.3). Wire `sig64 || md_bytes` carries no signer ID — observers cannot identify which owner pushed an update (preserves objective #6). Tests: `testChainCycleStructurallyImpossible`, `testChainTooLong`.

**1.2 Signer selection (no signer-path change).** `encodeSignUserData` takes any `PrivateKeyEd25519`. Channel link queue's signing key lives in `ShortLinkCreds.linkPrivSigKey` (set at creation for creator, at intake 1.5 for co-owner), sourced chat-side from `GroupKeys.groupRootKey` (`GRKPrivate rootPrivKey` ⇒ creator; `memberPrivKey` ⇒ co-owner). Existing `setConnShortLink` already signs correctly once `linkPrivSigKey` carries the right key — no code change at that call site. Upstream `linkRootSigKey` TODO at `AgentStore.hs:2514` resolved by 1.4a (its persistence is required for co-owner LSET to validate against the chain root).

**1.3 Co-owner RcvQueue.** Each device holds one `rcv_queues` row for the shared channel link queue with its own `rcv_private_key` + `ShortLinkCreds`. Schema unchanged: `PRIMARY KEY (host, port, rcv_id)` (`agent_schema.sql:66`) is per-database — two profiles on one device cannot both co-own a channel via that device. Chat-layer gate: `DEL`, `OFF`, `NKEY`, `NDEL` allowed only when this device holds `GRKPrivate _`. All `SRecipient` commands authorize via any-of-N (`Server.hs:1248-1249`), so the gate must apply uniformly. Test: `testCoOwnerCannotDeleteQueue`.

**1.4 RKEY agent wrapper.** Single primitive — full key list per call.

```haskell
setQueueRecipientKeys
  :: AgentClient -> NetworkRequestMode -> ConnId
  -> NonEmpty SMP.RcvPublicAuthKey -> AE ()
```

Concurrent races recovered by Phase 4.5 + Risk #12 pre-flight.

**1.4a Upstream simplexmq: persist `linkRootSigKey`.** Sequenced before 1.5 because the chat-side `acceptCoOwnerCreds` depends on the agent persistence layer accepting a non-`Nothing` `linkRootSigKey`.

`ShortLinkCreds.linkRootSigKey` is currently in-memory only: `toRcvQueue` (`AgentStore.hs:2513-2514`) hardcodes `Nothing` on every reload, so for any co-owner B the next `setConnShortLink` calls `validateOwners` (`Agent/Protocol.hs:1809-1819`) with B's own pubkey instead of the chain root — `validateLinkOwners` rejects the chain's first entry → `CMD PROHIBITED` before LSET reaches the wire, breaking promote, remove, link-data update, and the Phase 4.4 startup pre-flight for every co-owner across every session. Fix is purely persistence; do NOT modify `validateOwners` or `validateLinkOwners`.

**1.4a.1 Schema migration.** Additive nullable column on `rcv_queues` (`STRICT`, `agent_schema.sql:32-70`). Migration `M20260512_rcv_queues_link_root_sig_key` (matches `M<DATE>_<name>` convention; head is `M20260410_receive_attempts`):

```haskell
m20260512_rcv_queues_link_root_sig_key :: Query
m20260512_rcv_queues_link_root_sig_key =
  [sql|ALTER TABLE rcv_queues ADD COLUMN link_root_sig_key BLOB;|]
down_m20260512_rcv_queues_link_root_sig_key =
  [sql|ALTER TABLE rcv_queues DROP COLUMN link_root_sig_key;|]
```

Register at `Migrations/App.hs:98`. Postgres mirror with `BYTEA` in `Postgres/Migrations/M20260512_rcv_queues_link_root_sig_key.hs`, registered at `Postgres/Migrations/App.hs:26` per existing convention (Postgres registry uses bare date; SQLite uses `m`-prefix). Update `agent_schema.sql` to add `link_root_sig_key BLOB,` after `link_enc_fixed_data BLOB,` (line 61). No backfill — pre-fix rows are creator queues whose `linkRootSigKey` is `Nothing` by design.

**1.4a.2 Loader + writers.** Four sites:

- `rcvQueueQuery` (`AgentStore.hs:2483-2494`): append `q.link_root_sig_key` to the SELECT.
- `toRcvQueue` (`AgentStore.hs:2496-2518`): widen the final tuple segment with `Maybe C.PublicKeyEd25519`; bind as `linkRootSigKey_`; replace the hardcoded `linkRootSigKey = Nothing` in the `ShortLinkCreds` constructor and delete the TODO.
- `updateShortLinkCreds` (`AgentStore.hs:862-871`): destructure `linkRootSigKey`, extend the SET list with `link_root_sig_key = ?`, thread into the parameter tuple. Existing creator call site (`Agent.hs:1072-1073`) passes `Nothing`, which now writes SQL `NULL` (preserving creator behavior).
- `insertRcvQueue_` (`AgentStore.hs:2077-2099`): add `link_root_sig_key` to the INSERT column list, extend `VALUES` placeholders 25 → 26, bind `linkRootSigKey =<< shortLink` (collapses `Maybe (Maybe k)` → `Maybe k`).

**1.4a.3 Intake API.** Chat-side `acceptCoOwnerCreds` (§1.5) is the only intake supplying a non-`Nothing` `linkRootSigKey`; it constructs `ShortLinkCreds` with `linkRootSigKey = Just (bundle.rootPubKey)` and persists via `insertRcvQueue_`. After intake, every `getConn` reload returns `linkRootSigKey = Just rootPubKey` and B's LSET validates against the chain root.

**1.4a.4 Tests.** In `simplexmq/tests/AgentTests/`:

- `testLinkRootSigKeyRoundTrip` — round-trip insert/reload for both `Just k` and `Nothing` (creator regression).
- `testCoOwnerLSETAfterReload` — after `acceptCoOwnerCreds` and agent restart, B's `setConnShortLink` succeeds (pre-fix: `CMD PROHIBITED`).
- `testUpdateShortLinkCredsPreservesRootSigKey` — guards against future drift between in-memory type and writer.

Unblocks: 1.5; Phase 3 Step 4; 4.1 / 4.3 / 4.4.

**1.5 Co-owner credential bundle.** New `CoOwnerCredsBundle` in `Simplex.Messaging.Agent.Protocol`. Fields:

- `server`
- `rcvId`
- `sndId :: SMP.SenderId` — queue-level, shared across all co-owners; satisfies `rcv_queues.snd_id NOT NULL` at `agent_schema.sql:41`.
- `rcvDhSecret`
- `shortLinkId`
- `shortLinkKey`
- `rootPubKey`
- `linkEncFixedData`
- `agentVRange`
- **`ownerRcvPubKey :: SMP.RcvPublicAuthKey`** — A's own SMP rcv pubkey, delivered one-off to B at first-promotion bootstrap; chat writes it to `group_members.owner_rcv_pub_key` for A on B's device at intake.

B's `rcvPrivateKey` is generated locally (`C.generateAuthKeyPair`); queue-specific; never derived from `member_pub_key`. Intake API:

```haskell
acceptCoOwnerCreds
  :: AgentClient -> NetworkRequestMode -> UserId
  -> CoOwnerCredsBundle
  -> RcvPrivateAuthKey       -- B's local rcv auth key
  -> C.PrivateKeyEd25519     -- B's owner = member signing key
  -> AE ConnId
```

`acceptCoOwnerCreds` generates B's `e2e_priv_key` locally via `C.generateKeyPair` (link queues are LGET-only; a fresh local key satisfies the NOT-NULL at `agent_schema.sql:39` without leaking A's secret), then constructs B's `rcv_queues` row: `queue_mode = QMContact`; `link_priv_sig_key` = B's owner private key (= `memberPrivKey`); `link_id`/`link_key`/`link_enc_fixed_data`/ `rcv_dh_secret`/`snd_id` from the bundle; B's locally-generated keys for remaining NOT-NULL columns. **Intake idempotency:** if B's local `rcv_queues` row + chat-side co-owner state already exist for this `groupId`, intake is a no-op and re-emits `XGrpOwnerCredsAck` (handles A's retry when an earlier ACK was lost).

**Bundle integrity.** Before persisting, B decrypts `linkEncFixedData` with `shortLinkKey`, decodes `FixedLinkData`, and verifies `fixedData.rootKey == bundle.rootPubKey`. Mismatch ⇒ reject. Test: `testBundleRootKeyMismatchRejected`.

**Subscription policy.** Co-owners do NOT SUB the channel link queue; updates pulled via LGET on demand and at startup. Avoids the multi-recipient DH-secret problem (one `rcv_dh_secret` shared across N owners would let each decrypt the others' SUB traffic). `rcvDhSecret` is carried for forward compat with a future SUB-based co-owner path; **it is the queue-level shared secret originally negotiated by A with the SMP server.** Sharing it with B does not affect MVP behavior (LGET-only). Post-MVP, SUB-based catchup must solve the multi-recipient DH problem before this secret is operationally used.

Chat also writes `rootPubKey` into `groups.root_pub_key`.

**1.6 Mutable-blob version.** Add `linkDataVersion :: Maybe Word64` to chat-layer `GroupShortLinkData` JSON. Unknown-field tolerant; absent ⇒ 0. Agent-layer link blob structurally unchanged.

**1.7 Version constants.**

```haskell
-- Simplex.Messaging.Agent.Protocol
multiOwnerSMPAgentVersion = VersionSMPA 8
currentSMPAgentVersion    = multiOwnerSMPAgentVersion

-- Simplex.Chat.Protocol  (implementer reads chatVersionRange at commit)
multiOwnerChatVersion = VersionChat <current+1>
currentChatVersion    = multiOwnerChatVersion
```

Read compatibility: pre-v7 clients (v6.5+) can read chained-owner blobs. The simplexmq verifier `decryptLinkData` (`Crypto/ShortLink.hs:100-115`) accepts `signedBy rootKey || any (signedBy . ownerKey) owners`, and `validateLinkOwners` (`Agent/Protocol.hs:1821`) walks the prefix chain — both already upstream and shipping in v6.5+ chat builds. `agentVRange` in `FixedLinkData` is encoded metadata only; `decryptLinkData` does not gate on it. The depth cap added in 1.1 is more lenient on pre-v7 (no cap), and we never produce blobs exceeding the cap, so the asymmetry is benign.

What pre-v7 cannot do: produce chained-owner blobs (no orchestrator in Phase 3) or accept the new chat-protocol mesh events in Phase 2.4 (`x.grp.owner.invite`, `x.grp.owner.creds`, …). Release note: "Channels with multiple owners can be read by SimpleX Chat v6.5+, but only edited (promotion, removal, mesh sync) by v7+." Tests: `testOldClientReadsChainedOwnerBlob`, `testOldClientReadsRootSignedBlob`, `testOldClientCannotPromote`.

---

### Phase 2 — Chat foundation

Reads: `Library/Internal.hs:1313-1399, 2474-2477`, `Library/Commands.hs:2496-2527, 4042-4238`, `Store/Groups.hs:1860-1900, 2999-3020`, `Library/Subscriber.hs:2953-3070`, `Protocol.hs:422-470, 980-1320`, `Store/SQLite/Migrations.hs`.

**2.1 Schema migration.** Single chat-side migration `M<DATE>_multi_owner_channels` — additive; SQLite + Postgres mirrors; current head is `M20260507_relay_inactive_at`. SQL is DDL only because the framework type is `Migration { up :: Text }` (`Agent/Store/Shared.hs:30-32`) and cannot compute Ed25519 signatures or read the agent DB; Haskell-side backfill of creator rows (chain fields **and** `owner_rcv_pub_key`) runs at startup per §4.4.

`ALTER TABLE group_members` adds (all nullable):

- `owner_auth_sig BLOB`
- `owner_position INTEGER`
- `owner_rcv_pub_key BLOB`
- `wire_owner_key BLOB` — wire-asserted ownerKey when it diverges from local `member_pub_key`; NULL when they agree.

`ALTER TABLE groups` adds:

- `link_data_version INTEGER NOT NULL DEFAULT 0`
- `link_data_remote_version INTEGER NOT NULL DEFAULT 0`

`CREATE TABLE channel_owner_mesh`:

- Columns: `channel_owner_mesh_id` (PK), `group_id`, `peer_group_member_id`, `direct_conn_id`, `status TEXT`, `pending_inv_conn_req BLOB NULL`, `promoter_member_id INTEGER NULL`, timestamps.
- `UNIQUE(group_id, peer_group_member_id)`.
- FK `group_id → groups ON DELETE CASCADE`.
- FK `peer_group_member_id → group_members ON DELETE CASCADE`.
- FK `direct_conn_id → connections ON DELETE SET NULL` — when `deleteAgentConnectionsAsync'` deletes the connection per §4.3 closure, the FK clears while the row is preserved with `status = 'closed'`.
- FK `promoter_member_id → group_members ON DELETE SET NULL`.
- `status` ∈ {`'pending'`, `'connected'`, `'closed'`}.
- `pending_inv_conn_req` carries the X-side `IntroInvitation` `groupConnReq` while `status = 'pending'` so retried `xGrpMemIntro` events re-send the same invitation without re-creating the agent connection (per §2.4); cleared to NULL on transition to `'connected'` or `'closed'`.
- `promoter_member_id` is populated only on B's local row to A (the row B creates when accepting `XGrpOwnerInvite`); the basis for B's `XGrpOwnerCreds` sender verification (per §2.4 item 3). A's rows leave it NULL.
- Rows are **not deleted** on owner removal — preserved for audit and to avoid race-window re-establishment from a stale intro.

`CREATE TABLE channel_promotion_in_progress` (orchestrator journal):

- Columns: `promotion_id` (PK), `group_id`, `candidate_member_id`, `candidate_pub_key`, `candidate_rcv_pub_key`, `step TEXT`, `direct_conn_id`, `last_error`, `mesh_intros_completed TEXT NOT NULL DEFAULT '[]'`, timestamps.
- `UNIQUE(group_id, candidate_member_id)`.
- FK `group_id → groups ON DELETE CASCADE`.
- FK `direct_conn_id → connections ON DELETE SET NULL` — cancellation may delete the mesh connection while the journal row still exists in a terminal state.
- `mesh_intros_completed` is a JSON array of MemberId — A's per-promotion record of existing-owner peers (X) whose Step 6 `XGrpMemFwd` has been delivered to B; consulted by A's `xGrpMemInv` mesh handler to avoid re-forwarding on retry (per §2.4).

**2.2 Owner-roster helpers.** Representation IS `group_members` with `member_role = 'owner'` + owner_* columns. New helpers in `Simplex.Chat.Store.Groups`:

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

`getChannelOwnerAuths` materializes `OwnerAuth { ownerId = unMemberId memberId, ownerKey = coalesce(wire_owner_key, member_pub_key), authOwnerSig = ownerAuthSig }` ordered by `owner_position`. The `coalesce` reproduces the wire byte-for-byte even when local view diverges from the wire-asserted key (key-mismatch case in G5 below), so `validateLinkOwners` accepts the re-encoded chain. **G6 invariant:** `OwnerAuth.ownerId` is the raw bytes of `MemberId`; encode and decode assert this.

**G5 — `applyChannelOwnerRoster` ingest rule.** Three cases:

- Known `woMemberId` with matching `woOwnerKey` (local `member_pub_key == woOwnerKey`) → update `member_role`, `owner_auth_sig`, `owner_position`, `owner_rcv_pub_key` (latter only when `woRcvKey` is `Just`; never overwrite non-null with NULL); set `wire_owner_key = NULL`.
- Known `woMemberId` with **mismatching** `woOwnerKey` → store the wire's `woOwnerKey` in `wire_owner_key`; update other owner_* columns from the wire; do NOT overwrite local `member_pub_key`. Chain entry stays so the blob still verifies on re-encode (via the `coalesce`); standard member-info gossip reconciles later.
- Unknown `woMemberId` → record as a **pending-member row** (placeholder status, `member_role = 'owner'`); store `wire_owner_key = woOwnerKey` AND set `member_pub_key = woOwnerKey`; populate other owner_* columns verbatim from the wire.

Local owner rows not in the wire list → demote, clear owner_* columns and `wire_owner_key`.

**2.3 `groupLinkData` becomes IO.**

```haskell
groupLinkData
  :: DB.Connection
  -> GroupInfo -> GroupLink -> [GroupRelay]
  -> IO (UserConnLinkData 'CMContact, CRClientData)
```

Reads owners via `getChannelOwnerAuths`; calls `incrementLinkDataVersion`; embeds version in `GroupShortLinkData` JSON. Signing key stays on the agent's `ShortLinkCreds`. Enforce `length owners ≤ ownerChainDepth` before encode.

Both call sites today (`Internal.hs:1306-1322`) call the **pure** `groupLinkData` outside the surrounding `withFastStore`/`withStore` block (e.g., `Internal.hs:1311`). Once IO, both calls move INTO the existing transaction (single transaction, no new `withFastStore` — minimal diff). The IO operations (`getChannelOwnerAuths`, `incrementLinkDataVersion`, `getConnectedGroupRelays`) compose via `liftIO` inside the `ExceptT StoreError IO` wrapper (`Controller.hs:1630`). `setGroupLinkData` (`Internal.hs:1306-1314`) returns `(conn, userLinkData, crClientData)` from the block; `setGroupLinkDataAsync` (`Internal.hs:1316-1322`) takes the same shape using `withStore`.

`incrementLinkDataVersion` may run ahead of successful `setGroupLinkData` on transient failure (`setConnShortLink` runs after the block); acceptable — LWW `max(local, serverV) + 1` reconciliation handles drift.

**2.4 Owner-mesh transport.** Channel-scoped, fully-connected sub-graph among owners. Reuses `x.grp.mem.intro` / `inv` / `fwd` at `Subscriber.hs:2953-3070`. All wire `groupId :: B64UrlByteString` fields below (`XGrpOwnerInvite`, `XGrpOwnerAccept`, `XGrpOwnerCreds`, `XGrpOwnerCredsAck`, `XGrpLinkSync`) carry the channel's `publicGroupId` — group identity = sha256(genesis root key), immutable (per `Types.hs:790`). Six additions:

1. **`x.grp.owner.invite`** — A → B before the mesh edge exists, over A↔B's personal contact (or one-time link). `XGrpOwnerInvite { groupId :: B64UrlByteString, meshConnReq :: ConnReqContact }`. On accept, B establishes the channel-scoped direct mesh ContactConnection from `meshConnReq`.
2. **`x.grp.owner.accept`** — B → A over the now-established mesh edge. `XGrpOwnerAccept { groupId, ownerPubKey :: C.PublicKeyEd25519, rcvPubKey :: SMP.RcvPublicAuthKey }`. A records both for Step 3.
3. **`x.grp.owner.creds`** — `XGrpOwnerCreds (CoOwnerCredsBundleEnvelope { groupId, bundle })`. A → B direct mesh only; never relay-forwarded. Receiver rejects unless `groupId` matches.

    Sender verification on B's side via `channel_owner_mesh.promoter_member_id` (per §2.1):

    - Write site: B's `xGrpOwnerInvite` handler populates `promoter_member_id` from the delivering personal-contact connection's peer member.
    - Read site: B's `xGrpOwnerCreds` handler rejects unless the sending mesh peer's `groupMemberId` equals the recorded `promoter_member_id`.

    Test: `testBundleSenderMustBePromoter`.
4. **`x.grp.owner.creds.ack`** — B → A over the mesh edge, after `acceptCoOwnerCreds` succeeds and local state is persisted. `XGrpOwnerCredsAck { groupId }`. A's orchestrator advances `step = bundle_sent` only on receipt; re-emitted by B on intake idempotency (1.5).
5. **`ownerRcvPubKey :: Maybe SMP.RcvPublicAuthKey`** — optional JSON field on `XGrpMemIntro`, `XGrpMemInv`, **and** `XGrpMemFwd` (unknown-field tolerant). Populated **only** when the introducer is invoking Phase 3 Step 6; standard intros leave it `Nothing`. Per-edge flow for A introducing already-promoted B to existing owner X: intro A→X carries B's rcv pubkey; inv X→A carries X's rcv pubkey; fwd A→B carries X's rcv pubkey.
6. **`XGrpLinkSync`** for link-blob propagation (4.1). Does NOT carry rcv pubkeys — SMP rcv keys are fixed for the channel lifetime (mirrors 4.4 invariant); they travel only via path 2.

**Mesh-scoping acceptance rule.** Receiver of `XGrpMemIntro` / `Inv` / `Fwd` MUST verify both that `ownerRcvPubKey` is present AND that the introducer is a current owner of that group locally. If either fails, downgrade to the standard intro path (drop the field). When both hold, take the mesh-scoped early-branch.

**Mesh edges are NEW connections, not pre-existing ratchets.** In `useRelays'` channels members do NOT share direct ratchets — they communicate via relays. The mesh-scoped branch must call `createAgentConnectionAsync` (X side, mirrors `Subscriber.hs:2988`) or `joinAgentConnectionAsync` (B side, mirrors `Subscriber.hs:3032`) explicitly, then record the resulting `connId` in `channel_owner_mesh.direct_conn_id`. `directConnReq` in `IntroInvitation` stays `Nothing`.

**Dedup contract.** Each mesh-scoped handler queries `channel_owner_mesh` by `(group_id, peer_group_member_id)` keyed on the **introduced peer** (resolved via `getGroupMemberByMemberId` from the wire's `MemberInfo.memId`); A's `xGrpMemInv` is the exception — its dedup key is the orchestrator journal's `mesh_intros_completed` JSON array (per §2.1), since A's mesh row to the candidate exists since Step 1-2 and to X from prior promotions, neither indicating Step 6 fwd-completion.

| Handler            | No row / absent                                                                                                       | `pending`                                       | `connected` | `closed` |
|---|---|---|---|---|
| `xGrpMemIntro` (X) | INSERT row `'pending'`; `createAgentConnectionAsync user CFCreateConnGrpMemInv (chatHasNtfs chatSettings) SCMInvitation subMode`; store `connId` + `groupConnReq` in `pending_inv_conn_req`; emit `XGrpMemInv` | re-emit `XGrpMemInv` from stored `pending_inv_conn_req` (no second `createAgentConnectionAsync`) | no-op       | reject   |
| `xGrpMemInv`  (A)  | (X memberId absent from `mesh_intros_completed`) emit `XGrpMemFwd` to B carrying X's `ownerRcvPubKey`; on send success append X to JSON | row state irrelevant; dedup is via JSON array  | n/a         | reject   |
| `xGrpMemFwd`  (B)  | INSERT row `'pending'`; `joinAgentConnectionAsync user Nothing (chatHasNtfs chatSettings) groupConnReq dm subMode`; store `connId` | no-op | no-op       | reject   |

Per-handler skip lists (chat-side state not written in the mesh branch):

- `xGrpMemIntro` (X): skip `GCHostMember` check (`Subscriber.hs:2956`, with the `messageError` fall-through at `Subscriber.hs:2986`) and `createIntroReMember` / `updatePreparedChannelMember` / `createNewGroupMember`.
- `xGrpMemInv` (A): skip `GCInviteeMember` check (`Subscriber.hs:3004`).
- `xGrpMemFwd` (B): skip `createNewGroupMember` (missing-member branch) and `createIntroToMemberContact`.

`'closed'` rejection is load-bearing: the row was preserved per §2.1 because the peer was previously removed; do not re-establish.

Else-branch (downgrade) runs the standard flow. Tests: `testMeshScopeRequiresOwnerIntroducer`, `testMeshIntroIdempotentOnRetry`, `testMeshInvNotReforwarded`, `testMeshFwdIdempotentOnRetry`.

**Transient roster lag.** If the introducer is not yet visible as an owner in the receiver's local roster (transient lag after a promotion), the receiver downgrades to the standard intro path, which then rejects (sender is not `GCHostMember`). The orchestrator re-emits the intro per the Step 6 retry cadence in Phase 3 (the chat-protocol layer does not retry `messageError` rejections at `Subscriber.hs:1746`), eventually succeeding once the receiver's next LGET refresh propagates the updated roster.

`sendOwnerMeshMessage :: User -> GroupInfo -> [ChannelOwnerMesh] -> ChatMsgEvent 'Json -> CM ()` walks connected mesh rows and reuses `sendDirectMemberMessage`; skipped peers fall back to LGET-on-startup.

**2.4.1 Mesh receive allowlist.** A `Connection` referenced by `channel_owner_mesh.direct_conn_id` (joined at message-receive time) is mesh; otherwise the standard chat allowlist applies. Allowed events on a mesh connection:

- `x.grp.owner.accept`
- `x.grp.owner.creds`
- `x.grp.owner.creds.ack`
- `x.grp.link.sync`
- `x.grp.mem.role`
- `x.grp.mem.intro`
- `x.grp.mem.inv`
- `x.grp.mem.fwd`

Anything else over a mesh connection is logged and rejected (`x.msg.new` is NOT in this delivery). `XGrpOwnerInvite` is the only new chat-protocol event delivered over the standard personal-contact path (pre-mesh); registered in the chat-protocol parser but not in the mesh allowlist. Test: roster round-trip, mesh-allowlist rejection of `x.msg.new`.

---

### Phase 3 — Promote-to-owner orchestrator

A = existing owner; B = member to promote. Orchestrator owns one `channel_promotion_in_progress` row per `(group, candidate)`. Each step is idempotent; restart resumes via `resumePromotions`.

| Step | Marker | Action |
|---|---|---|
| 1 | `invitation_sent` | A creates a fresh channel-scoped direct ContactConnection, encodes `XGrpOwnerInvite { groupId, meshConnReq }`, sends over A↔B's personal contact (or one-time link). |
| 2 | `creds_received` | B accepts, establishes the mesh edge, generates `rcvKeyPair` locally, reuses `memberPrivKey` as `ownerPrivKey`, returns `XGrpOwnerAccept { groupId, ownerPubKey, rcvPubKey }` over the mesh connection. A records both. |
| 3 | `rkey_done` | Run **Risk #12 LWW pre-flight** (re-LGET; refresh roster; map ownerIds to local `owner_rcv_pub_key`; wait for next mesh sync rather than push on missing mapping). Compute desired `recipientKeys` = current owners' rcv pubkeys ∪ {B.rcvPubKey}; `setQueueRecipientKeys`. |
| 4 | `lset_done` | Construct `OwnerAuth_B = (B.memberId, ownerPubKey, sign(A.ownerPrivKey, memberId ‖ encodePubKey ownerPubKey))`; `markMemberAsOwner`; call `groupLinkData` (bumps version); `setGroupLinkData`. |
| 5 | `bundle_sent` | `XGrpOwnerCreds(bundle)` over the mesh connection. Bundle carries A's `ownerRcvPubKey` (first-promotion delivery to B). **Marker idempotency:** `bundle_sent` is written ONLY after `XGrpOwnerCredsAck` from B, not after the agent's send-queue submission — prevents bundle loss in transit advancing the journal and leaving B permanently uncredentialed. |
| 6 | `mesh_introduced` | For each existing owner X (≠ A, ≠ B), drive `x.grp.mem.intro` / `inv` / `fwd` with `ownerRcvPubKey` on all three. Each of X's three handlers takes its mesh-scoped early-branch (§2.4). Completed edge → `channel_owner_mesh.status = 'connected'`. |
| 7 | `role_announced` | Sign + broadcast `x.grp.mem.role` via relays; broadcast `XGrpLinkSync` over the mesh. |

**B's consent (Steps 1 → 2).** B's UI surfaces `XGrpOwnerInvite` as a notification ("A is offering to make you an owner of channel <name>") with Accept / Decline. On Accept, B joins the mesh `meshConnReq` ContactConnection and proceeds to Step 2. On Decline, B's client does not join; A's orchestrator times out per the Step 6 cadence below, then surfaces "B declined or did not respond" to A's UI after the hour-long bound. A may then `apiCancelPromotion`.

**Idempotency.** Step 3 skips if B's rcv key is already in the desired set after pre-flight; Step 4 LGETs and skips if B's owner row already chain-verifies; Step 5 advances its marker only on `XGrpOwnerCredsAck`; Step 6 keys on per-peer mesh state; Step 7 relies on `x.grp.mem.role` idempotence.

**Step 6 retry cadence.** Within an app session the orchestrator polls `channel_owner_mesh` per (group, peer) every 30s while any row's `status` is `'pending'`; for each still-pending row, re-emit `x.grp.mem.intro` with the same `ownerRcvPubKey`. Backoff to 5min after 5 consecutive failures. Surface to UI on hour-long persistent failure. Retries are non-destructive because §2.4 handlers short-circuit on receiver-side state.

**Failure modes.** 3→4 fail: stale rcv key sits in queue's set; Step 3 retry detects, Step 4 retries cleanly. 4→5 fail: B is on-server owner without credentials — "owner credentials missing — request resend" surfaces via next LGET. 6 partial: retry per-peer at next app start. 7 fail: subscribers see B as non-owner temporarily.

**Cancellation (`apiCancelPromotion`).** Semantics by step:

- Cancel from `invitation_sent` or `creds_received`: tear down the channel-scoped direct mesh ContactConnection; delete the `channel_promotion_in_progress` row. No server-side effect.
- Cancel from `rkey_done` or `lset_done`: drive the **removal flow (4.3) targeted at B** as rollback — `setQueueRecipientKeys` first, then LSET, broadcasts, mesh closure. Then delete journal row.
- Cancel from `bundle_sent` onward: same removal-flow rollback. B may receive `XGrpLinkSync` removing itself and self-demote per 4.5 Case 4 logic.

```haskell
-- Simplex.Chat.Library.Owners  (new module)
promoteToOwner   :: User -> GroupInfo -> GroupMember -> CM ()
resumePromotions :: CM ()     -- wired into Simplex.Chat.Core startup
```

Tests: `testPromoteOwner`, `testPromoteOwnerResumeStep<N>`, `testPromoteIdempotentRetry`, `testCandidateDeclinePromotion`, `testStep6RetriesOnRosterLag`, `testCancelPromotionFromEachStep`.

---

### Phase 4 — Cross-owner sync, conflict resolution, owner removal

**4.1 `x.grp.link.sync`** — mesh-only.

```haskell
XGrpLinkSync :: GrpLinkSync -> ChatMsgEvent 'Json
data GrpLinkSync = GrpLinkSync
  { groupId :: B64UrlByteString, linkDataVersion :: Word64
  , encMutableData :: B64UrlByteString }            -- raw md_bytes
```

Receiver verifies `groupId`; decodes `md_bytes`; runs `validateLinkOwners` against `groupRootPubKey (groupRootKey groupKeys)`; if `linkDataVersion > local`, applies the roster and sets `link_data_remote_version`. Does not carry rcv pubkeys.

**4.2 LWW reconciliation.** Per device: `link_data_version` (highest written), `link_data_remote_version` (highest seen). On roster mutation: LGET; if `serverV > remote`, swap local roster for the server's and re-apply in-memory pending mutations; `newV := max(local, serverV) + 1`; encode + LSET; broadcast `XGrpLinkSync(newV)`; bounded retry (max 3) on contention. Non-roster mutations may use optimistic-without-LGET. Tests: `testLWWConflictDetection`, `testRosterRaceRetry`, `testStaleBlobIgnored`.

**4.3 Owner removal.** Pure planner

```haskell
planOwnerRemoval
  :: C.PublicKeyEd25519
  -> [(MemberId, C.PublicKeyEd25519, C.Signature 'C.Ed25519)]
  -> MemberId -> MemberId
  -> Either String OwnerRemoval     -- Left when remover's chain depends on removee
```

Forward pass over the prefix-ordered list: any owner whose authorizer is in the removed-set is itself removed; remover in cascade ⇒ `Left`.

Steps: compute `OwnerRemoval` → recompute `recipientKeys` from `owner_rcv_pub_key` minus removed → `setQueueRecipientKeys` (preceded by Risk #12 pre-flight) → bump version, LSET (cascade pruned writer-side) → broadcast `x.grp.mem.role` via relays for each removed owner → broadcast `XGrpLinkSync` over mesh → transition `channel_owner_mesh.status` to `'closed'` for removed peers (rows preserved) and invalidate their `direct_conn_id` via `deleteAgentConnectionsAsync'`.

Order matters: `setQueueRecipientKeys` runs before LSET to revoke SMP write access first. If LSET ran first and `setQueueRecipientKeys` failed (or was delayed), a misbehaving removed owner could observe their removal and re-LSET themselves back into the chain before access was revoked (their `OwnerAuth_X` remains valid as long as their authorizer is still in the chain). With keys revoked first, the removed owner's LSET attempts return `ERR AUTH` regardless of chain-blob state.

D3 surfaces blockage: "You cannot remove the channel creator — your owner role was authorized by them." Single-owner channels: the sole root-signed owner cannot be removed.

Sole-owner channel destruction: the existing channel-deletion flow (`deleteGroupChat` / `apiDeleteChat`) is unchanged for channels; permitted for the sole owner via the existing creator-only chat-layer gate. Multi-owner: any owner can run `deleteGroupChat` locally (removing their own view), but `LDEL` of the link queue requires `GRKPrivate _` (creator-only, per 1.3 gate); chained owners attempting deletion get `CEPermissionDenied` and leave locally while the channel persists on the server until the creator issues `LDEL`. No new tests; covered by existing channel-deletion tests + `testCoOwnerCannotDeleteQueue`.

Tests: `testCascadeRemoval`, `testRemoverCascadeBlocked`, `testRootCannotBeRemovedSoloOwner`, `testCascadeUiPreviewShowsAll`.

**4.4 Single-owner → multi-owner upgrade and convergence pre-flight.** Single unified startup pass in `Simplex.Chat.Library.Owners`, wired into `Simplex.Chat.Core` startup. SQL migration in 2.1 adds columns only; all backfill is Haskell-side. Two parts:

*Backfill.* Iterate groups with non-null `member_priv_key` and a creator row lacking `owner_auth_sig`. For each:

- Set `member_role = 'owner'`.
- Write `owner_auth_sig = sign(rootPrivKey, memberId ‖ encodePubKey(publicKey memberPrivKey))`.
- Write `owner_position = 0`.
- Query the agent via `getChannelLinkRcvPubKey :: AgentClient -> ConnId -> AE SMP.RcvPublicAuthKey` (derives the pubkey from `rcvPrivateKey` stored in the `RcvQueue` row; no new SMP command) and write `owner_rcv_pub_key`.

Idempotent; one pass per device. If `getChannelLinkRcvPubKey` fails for a specific channel, skip with a logged warning and surface to UI; do not block chat startup.

*Convergence pre-flight.* For each multi-owner channel on this device, run one Risk #12 pre-flight pass at startup: compute the desired `recipientKeys` from the chain (via `getChannelOwnerAuths`) ∪ locally-known `owner_rcv_pub_key` mappings; compare against the previous desired set persisted from this device's last `setQueueRecipientKeys`; if they differ, push the new desired set (bounded retry as 4.2). Idempotent. Resolves both 4.5 Case 3 (re-adds a chain-valid owner whose key was dropped) and 4.5 Case 4 (drops an orphaned key + emits a demotion `XGrpLinkSync` over the existing A↔B mesh edge if reachable) without waiting for a manual trigger. Note: server-side `recipientKeys` is unreadable per G3, so the comparison is against the device's last-known desired set.

**Synchronicity.** The pass runs **synchronously** during `Simplex.Chat.Core` startup, completing before any chat-side RPC touching a channel link blob (`apiPromoteToOwner`, `apiRemoveOwner`, `setGroupLinkData`, …) can execute. Fallback if a build makes it unavoidably async: when the missing-mapping owner is the **local device's own row**, the Risk #12 pre-flight derives on-the-fly via `getChannelLinkRcvPubKey` rather than waiting for mesh sync.

Post-upgrade the **member-key = owner-key invariant** holds and must continue to hold; member signing keys are fixed for the channel's lifetime. Tests: `testUpgradeCreatorOwnerRowBackfilled`, `testCreatorRcvPubKeyBackfilledAtStartup`, `testMemberKeyEqualsOwnerKey`, `testStartupPreflightConvergesRaces`.

**4.5 Concurrent-promotion race recovery.** Owners A, A' may race-promote candidates B, B'. Chain (LSET) and recipientKeys (RKEY) are independent server writes with no atomicity, giving four outcomes — two clean (A/A or A'/A') and two mixed:

- **Case 3, RKEY race lost** (chain by A, RKEY by A'). B's first LSET returns `ERR AUTH`; B's device emits a chat-layer event prompting any current owner to re-run the Risk #12 pre-flight and call `setQueueRecipientKeys`. Other owners already know every rcv pubkey via path-2 bootstrap; no new key transport. Converges in one mesh round.
- **Case 4, chain race lost** (chain by A', RKEY by A). During any current owner's next pre-flight, the chain is re-derived from the fresh blob; any rcv pubkey in `recipientKeys` not matching a chain-validated owner is dropped, and `setQueueRecipientKeys` writes the corrected list. The dropped party (B) is informed via `XGrpLinkSync` over the A↔B mesh edge (the only mesh edge B holds — Step 6 hasn't introduced B to other owners yet). On receipt B applies the roster, finds itself absent, demotes locally and tears down its local channel-link `rcv_queues` row. **Cooperative assumption:** recovery here assumes B is cooperative on receipt. A misbehaving B holding SMP write access could re-LSET to restore itself (its `OwnerAuth_B` remains valid as long as A is in the chain). Acceptable under the full-trust model; the next pre-flight by any other honest owner resolves it. See Risk #10.

Tests: `testConcurrentRKEYConvergence` (case 3), `testChainRaceLoserDemotes` (case 4), `testStaleMeshRKEYWaitsForMeshSync` (Risk #12).

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

Chat-side RPCs: `apiPromoteToOwner`, `apiRemoveOwner`, `apiPlanRemoveOwner` (runs `planOwnerRemoval` without committing), `apiCancelPromotion`, `apiChannelOwners`. UI-shaped `ChannelOwner`: `memberId`, `displayName`, `ownerPosition`, `authorizedByMemberId :: Maybe MemberId` (computed from `reconstructOwnerAuthorizers`; `Nothing` ⇒ root-signed).

Strings: iOS `Localizable.strings` (+ each `SimpleX Localizations` mirror); Kotlin `MR/base/strings.xml`. ~14 new strings; translations in a follow-up. Layout per `layout-swift.md` / `layout-compose.md`.

---

### Phase 6 — Tests + threat-model regressions

Files: `tests/ChatTests/ChannelTests.hs` (extended); `simplexmq/tests/AgentTests/{ShortLinkTests.hs, FunctionalAPITests.hs}` (extended); new `tests/ChatTests/MultiOwnerTests.hs`. UI snapshot (iOS) + screenshot (Compose) tests for `ChannelOwnersView` and `RemoveChannelOwnerSheet`. Test names listed inline per phase and in the Risk register.

---

## 4. Risk register

| # | Risk | Mitigation | Test |
|---|---|---|---|
| 1 | Cross-version compatibility on multi-owner channels. | v6.5+ readers verify chained-owner blobs correctly via the existing `validateLinkOwners` (per 1.7 verification); they cannot promote, accept promotion, or sync via mesh. agentVRange bump + chat-protocol gate restrict editing to v7+; release notes call out the read/edit asymmetry. | `testOldClientReadsChainedOwnerBlob`; `testOldClientReadsRootSignedBlob`; `testOldClientCannotPromote`. |
| 2 | Promotion atomicity — mid-flow process death leaves partial state. | `channel_promotion_in_progress` journal + idempotent steps + `resumePromotions` on startup. | `testPromoteOwnerResumeStep<N>`. |
| 3 | LWW data loss in concurrent roster edits. | Optimistic retry on roster mutations (4.2); D4 banner; bounded retries error to UI on persistent contention. | `testRosterRaceRetry`. |
| 4 | Cascade-removal UX clarity. | D3 explicit list; remove disabled when remover's chain depends on removee. | `testCascadeUiPreviewShowsAll`. |
| 5 | Co-owner credential bundle confidentiality. | Bundle sent only over the channel-scoped direct mesh connection (E2E ratchet); cross-channel replay rejected by `groupId` check at receive. **CI lint:** grep step that fails if any `logInfo`/`logDebug`/`putStrLn`/`hPutStrLn` site references `CoOwnerCredsBundle` or `XGrpOwnerCreds`. | `testBundleCrossChannelReject`; `testBundleReplayRejected`; CI lint. |
| 6 | Member-key = owner-key invariant violated by future refactor. | `testMemberKeyEqualsOwnerKey` runs on every channel-creation and promotion. | `testMemberKeyEqualsOwnerKey`. |
| 7 | Chain-cycle attempts. | `validateLinkOwners` is prefix-only — structurally cycle-free; depth cap of 8. | `testChainCycleStructurallyImpossible`; `testChainTooLong`. |
| 8 | Co-owner disrupts queue via `DEL`/`OFF`/`NKEY`/`NDEL` (any-of-N at `Server.hs:1248-1249`). | Chat-layer gate: each command allowed only when this device holds `GRKPrivate _`. | `testCoOwnerCannotDeleteQueue`. |
| 9 | Server returns stale blob during reconciliation. | Treat `linkDataVersion` as authoritative if signed; if monotonicity violated, surface "channel state inconsistent" and skip the write. | `testStaleBlobIgnored`. |
| 10 | Concurrent RKEY race drops a candidate. | Eventual consistency via mesh recovery (4.5) and the 4.4 startup pre-flight. The cooperative assumption is **strictly limited** to 4.5 Case 4 (chain race lost by a freshly-promoted candidate B); removal (4.3) revokes SMP write access via `setQueueRecipientKeys` before LSET, so removed owners cannot re-LSET. Persistent misbehavior in Case 4 is bounded by repeated pre-flight by any honest owner. | `testConcurrentRKEYConvergence`; `testChainRaceLoserDemotes`; `testStartupPreflightConvergesRaces`. |
| 11 | Member-pub-key rotation breaks owner status. | Invariant test + documentation (4.4); future rotation feature must rewrite the roster in-transaction. | `testMemberKeyEqualsOwnerKey`. |
| 12 | **Stale-mesh-view RKEY race.** Owner pushes RKEY based on a roster diverged from the server's blob; either a freshly-promoted peer's rcv pubkey is missing locally, or a removed peer's lingers. | Before every `setQueueRecipientKeys`, run LWW reconciliation on the link blob first (re-LGET; refresh roster from blob's `OwnerAuth` list; map ownerIds to local `owner_rcv_pub_key`; retry on mismatch). If a mapping is missing locally for a freshly chain-validated owner, **wait for the next mesh sync rather than push** — bounded backoff; surface to UI on persistent failure. | `testStaleMeshRKEYWaitsForMeshSync`. |

### Threat-model alignment

Channels-overview.md objectives 1-7 preserved. Objective 6 (sender anonymity within multi-owner channels) is *strengthened* — observers with the link key see only that some chain-valid owner pushed an update, not which one. New attack surface (owner mesh) is direct, E2E-encrypted (double-ratchet), channel-scoped, invisible to subscribers and relays. **Convergence:** guaranteed in O(mesh round-trip) under cooperative owners and eventual mesh connectivity; adversarial or persistently offline owners can prolong divergence indefinitely (out of scope under the full-trust model). Threat-model regressions: `testRelayCannotForgeOwners`, `testBundleCrossChannelReject`, `testBundleReplayRejected`, `testSignatureRequired` (extended with chained-owner cases), `testMeshScopeRequiresOwnerIntroducer`.

---

## 5. Out of scope

- Legacy private (P2P) groups.
- Multisig and programmable governance.
- Relay-mediated promotion.
- Pre-signing N≥2 root-signed owners at channel creation for creator anonymity.
- Transfer of root creator.
- Owner-only chat UI surface; mesh content (`x.msg.new` etc.) is NOT persisted.
- Owner-mesh push catchup for offline owners (LGET-on-startup is MVP).
- Public-groups-over-relays migration.
- Member-pub-key rotation while a member is an owner.
- Multi-device co-ownership for one profile: `group_members.owner_rcv_pub_key` is single-valued per `(group_id, member_id)`; multiple devices per profile would each hold a different rcv pubkey. Out of scope.
- Device migration for a single profile. After restoring a chat profile to a new device, the agent DB is fresh; the new device's `rcv_private_key` for the channel link queue does not match `group_members.owner_rcv_pub_key` carried in the link blob from the old device. The migrated user is silently locked out of channel-owner write access until a separate device-migration flow is added (post-MVP).
