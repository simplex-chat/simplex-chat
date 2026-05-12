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

**1.2 Signer selection (no signer-path change).** `encodeSignUserData`
takes any `PrivateKeyEd25519`. Channel link queue's signing key lives
in `ShortLinkCreds.linkPrivSigKey` (set at creation for creator, at
intake 1.5 for co-owner), sourced chat-side from
`GroupKeys.groupRootKey` (`GRKPrivate rootPrivKey` ⇒ creator;
`memberPrivKey` ⇒ co-owner). Existing `setConnShortLink` already signs
correctly once `linkPrivSigKey` carries the right key — no code change
at that call site. Upstream `linkRootSigKey` TODO at
`AgentStore.hs:2514` resolved by 1.4a (its persistence is required
for co-owner LSET to validate against the chain root).

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

**1.4a Upstream simplexmq: persist `linkRootSigKey`.** Numbered
`1.4a` to avoid renumbering existing references to 1.5/1.6/1.7
throughout the plan; sequenced before 1.5 because the chat-side
intake in 1.5 (`acceptCoOwnerCreds`) depends on the agent's
persistence layer accepting a non-`Nothing` `linkRootSigKey`.

**Bug.** `ShortLinkCreds.linkRootSigKey` is in-memory only. On every
load from `rcv_queues`, `toRcvQueue` (`AgentStore.hs:2513-2514`)
hardcodes `linkRootSigKey = Nothing` (`-- TODO linkRootSigKey
should be stored in a separate field`). For a co-owner B,
`validateOwners` (`Agent/Protocol.hs:1809-1819`) at every
`setConnShortLink'` (`Agent.hs:1042-1055`) then reduces to
`validateLinkOwners (publicKey linkPrivSigKey) owners` — i.e., it
treats B's own pubkey as the root. The chain's first entry was
signed by the actual root, not by B → `signedBy` fails → `Left` →
`CMD PROHIBITED` before the LSET reaches the wire. **Co-owners ≠
creator cannot LSET, ever, across all sessions.** This breaks
promote, remove, link-data update, LWW reconciliation, and the
Phase 4.4 startup pre-flight. Fix is purely persistence; do NOT
modify `validateOwners` or `validateLinkOwners`.

**1.4a.1 Schema migration.** Additive, nullable column on
`rcv_queues`. Migration name `M20260512_rcv_queues_link_root_sig_key`
(matches simplexmq's `M<DATE>_<name>` convention; current head is
`M20260410_receive_attempts`).

- *SQLite* (`Migrations/M20260512_rcv_queues_link_root_sig_key.hs`):

  ```haskell
  m20260512_rcv_queues_link_root_sig_key :: Query
  m20260512_rcv_queues_link_root_sig_key =
    [sql|ALTER TABLE rcv_queues ADD COLUMN link_root_sig_key BLOB;|]

  down_m20260512_rcv_queues_link_root_sig_key :: Query
  down_m20260512_rcv_queues_link_root_sig_key =
    [sql|ALTER TABLE rcv_queues DROP COLUMN link_root_sig_key;|]
  ```

  Register at `Migrations/App.hs:98` (after the
  `m20260410_receive_attempts` row):
  `("m20260512_rcv_queues_link_root_sig_key",
  m20260512_rcv_queues_link_root_sig_key,
  Just down_m20260512_rcv_queues_link_root_sig_key)`. The
  `rcv_queues` table is `STRICT` (`agent_schema.sql:32-70`); `BLOB`
  is a permitted STRICT type, no further annotation required.

- *Postgres* (`Postgres/Migrations/M20260512_rcv_queues_link_root_sig_key.hs`):

  ```haskell
  m20260512_rcv_queues_link_root_sig_key :: Text
  m20260512_rcv_queues_link_root_sig_key =
    [r|ALTER TABLE rcv_queues ADD COLUMN link_root_sig_key BYTEA;|]

  down_m20260512_rcv_queues_link_root_sig_key :: Text
  down_m20260512_rcv_queues_link_root_sig_key =
    [r|ALTER TABLE rcv_queues DROP COLUMN link_root_sig_key;|]
  ```

  Register at `Postgres/Migrations/App.hs:26` (after
  `m20260410_receive_attempts`):
  `("20260512_rcv_queues_link_root_sig_key",
  m20260512_rcv_queues_link_root_sig_key,
  Just down_m20260512_rcv_queues_link_root_sig_key)`. (Postgres
  registry uses the bare date prefix without the leading `m`,
  matching the existing `"20260410_receive_attempts"` entry; SQLite
  registry uses the `m`-prefixed identifier.)

  Update `agent_schema.sql` (the canonical-state mirror of all
  applied SQLite migrations) to add `link_root_sig_key BLOB,`
  immediately after `link_enc_fixed_data BLOB,` (currently line 61),
  to keep the schema dump in sync with applied migrations.

- *Backfill.* None. The column is nullable; existing rows
  retain `link_root_sig_key = NULL`, which corresponds to the
  pre-fix in-memory behavior of `linkRootSigKey = Nothing` for the
  creator's own queue (creator has no chained-owner authorizer).
  Co-owners did not exist before this change, so no co-owner row
  needs backfill.

**1.4a.2 Loader update — `toRcvQueue`.** Extend the SQL projection,
the tuple type, and the destructuring; populate
`ShortLinkCreds.linkRootSigKey` from the new column; remove the
TODO.

- `rcvQueueQuery` (`AgentStore.hs:2483-2494`): append
  `q.link_root_sig_key` to the SELECT — last column of the last
  `(... link_id, link_key, link_priv_sig_key, link_enc_fixed_data)`
  group.
- `toRcvQueue` (`AgentStore.hs:2496-2518`): widen the final tuple
  segment from
  `(Maybe SMP.LinkId, Maybe LinkKey, Maybe C.PrivateKeyEd25519, Maybe EncDataBytes)`
  to
  `(Maybe SMP.LinkId, Maybe LinkKey, Maybe C.PrivateKeyEd25519, Maybe EncDataBytes, Maybe C.PublicKeyEd25519)`.
  Bind the new value as `linkRootSigKey_` and replace the
  hardcoded `linkRootSigKey = Nothing` in the `ShortLinkCreds`
  construction with `linkRootSigKey = linkRootSigKey_`. Delete the
  trailing `-- TODO linkRootSigKey should be stored in a separate
  field` comment.
- The semantics for `shortLink` reconstruction stay
  all-or-nothing on the four NOT-NULL-when-set fields
  (`shortLinkId`, `shortLinkKey`, `linkPrivSigKey`,
  `linkEncFixedData`); `linkRootSigKey_` is consumed inside the
  `Just` branch and may be `Nothing` (creator) or `Just rootPubKey`
  (co-owner). Pre-fix rows decode to `linkRootSigKey = Nothing`,
  preserving prior behavior for creators.

**1.4a.3 Writers — `updateShortLinkCreds` + `insertRcvQueue_`.**
Both write sites must thread the new field. Smaller and clearer
than adding a separate setter — `linkRootSigKey` is already a
field of `ShortLinkCreds`, and there are exactly two writers.

- `updateShortLinkCreds` (`AgentStore.hs:862-871`): destructure
  `linkRootSigKey` alongside the other four `ShortLinkCreds`
  fields; extend the `UPDATE` SET list to write
  `link_root_sig_key = ?`; thread `linkRootSigKey` into the
  parameter tuple. Existing creator call site
  (`Agent.hs:1072-1073`) constructs
  `ShortLinkCreds linkId linkKey privSigKey Nothing (fst srvData)`
  — it passes `Nothing`, which now writes a SQL `NULL` (preserving
  prior behavior for creators).
- `insertRcvQueue_` (`AgentStore.hs:2077-2099`): add
  `link_root_sig_key` to the INSERT column list (after
  `link_enc_fixed_data`); extend the `VALUES` placeholder count
  from 25 to 26; bind `linkRootSigKey =<< shortLink` in the
  parameter tuple alongside the existing
  `linkPrivSigKey <$> shortLink` etc. (`=<<` collapses
  `Maybe (Maybe k)` → `Maybe k`, since `linkRootSigKey` is itself
  `Maybe`.)

No other writers touch `link_priv_sig_key` (verified via
`grep -n "link_priv_sig_key" AgentStore.hs`); the new column is
covered by exactly the two sites above.

**1.4a.4 Intake API for co-owner persistence.** The chat-side
plan's 1.5 declares
`acceptCoOwnerCreds :: AgentClient -> NetworkRequestMode -> UserId
-> CoOwnerCredsBundle -> RcvPrivateAuthKey ->
C.PrivateKeyEd25519 -> AE ConnId`. It constructs a `ShortLinkCreds`
populated from the bundle:

```haskell
ShortLinkCreds
  { shortLinkId       = bundle.shortLinkId
  , shortLinkKey      = bundle.shortLinkKey
  , linkPrivSigKey    = ownerPrivKey  -- B's owner = member signing key
  , linkRootSigKey    = Just (bundle.rootPubKey)
  , linkEncFixedData  = bundle.linkEncFixedData
  }
```

The `Just (bundle.rootPubKey)` is the load-bearing change — the
chat-side `CoOwnerCredsBundle.rootPubKey` already carries the
value; only the persistence side was missing. `acceptCoOwnerCreds`
constructs `NewRcvQueue { ..., shortLink = Just slCreds, ... }` and
persists via `insertRcvQueue_` (1.4a.3 above), which now writes the
fifth `link_root_sig_key` column. After this, every subsequent
`getConn`/`toRcvQueue` reload returns `linkRootSigKey = Just
rootPubKey`, and `validateOwners` at the next
`setConnShortLink` calls `validateLinkOwners rootPubKey owners` —
the chain's first entry verifies, B's LSET is permitted.

`acceptCoOwnerCreds` is the only intake API that ever supplies a
non-`Nothing` `linkRootSigKey`. The creator path
(`prepareContactLinkData` at `Agent.hs:1065-1074`, which calls
`updateShortLinkCreds` with `linkRootSigKey = Nothing`) is
unchanged in semantics — it now writes a SQL `NULL` instead of
relying on the loader's hardcoded `Nothing`.

**1.4a.5 Tests.** New tests in
`simplexmq/tests/AgentTests/ShortLinkTests.hs` (round-trip) and
`tests/AgentTests/FunctionalAPITests.hs` (LSET regression).

- `testLinkRootSigKeyRoundTrip` — insert an `rcv_queues` row via
  `insertRcvQueue_` with
  `shortLink = Just ShortLinkCreds { linkRootSigKey = Just k, ... }`
  for a randomly-generated Ed25519 pubkey `k`; reload via
  `getRcvQueuesByConnId_`; assert
  `linkRootSigKey (fromJust (shortLink rq)) == Just k`. Repeat
  with `linkRootSigKey = Nothing` and assert the round-trip is
  also `Nothing` (creator regression).
- `testCoOwnerLSETAfterReload` — full loop:
  1. Set up A as a creator via the existing short-link creation
     path; LSET initial blob with one `OwnerAuth` for B
     (signed by A's root key).
  2. On B's `AgentClient`, call `acceptCoOwnerCreds` with a
     `CoOwnerCredsBundle` carrying `rootPubKey = A_rootPub`,
     B's locally-generated rcv private key, and B's owner
     (= member) signing private key.
  3. Restart B's agent or otherwise force a fresh `getConn` on
     the channel link queue.
  4. Have B call `setConnShortLink` with new
     `UserContactLinkData`.
  5. Assert success — pre-fix this fails with `CMD PROHIBITED`
     (`validateOwners` rejects); post-fix it reaches
     `addQueueLink` and the wire LSET succeeds.
- `testUpdateShortLinkCredsPreservesRootSigKey` — call
  `updateShortLinkCreds` with a `ShortLinkCreds` whose
  `linkRootSigKey = Just k`; reload and assert preservation.
  Then call again with `linkRootSigKey = Nothing` and assert the
  column is now SQL `NULL`. Guards against future drift between
  the in-memory type and the writer.

This section unblocks: 1.5 (`acceptCoOwnerCreds` can now persist
`linkRootSigKey`); Phase 3 Step 4 (B's first LSET as part of
subsequent promotions); 4.1 / 4.3 / 4.4 (any LSET driven by a
co-owner).

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
`rcvDhSecret` is carried for forward compat with a future SUB-based
co-owner path; **it is the queue-level shared secret originally
negotiated by A with the SMP server.** Sharing it with B does not
affect MVP behavior (LGET-only). When SUB-based catchup is added
post-MVP, the multi-recipient DH problem must be solved before this
secret is operationally used — co-owners cannot share one secret
without each being able to decrypt the others' SUB traffic.

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

Read compatibility: pre-v7 clients (v6.5+) can read chained-owner
blobs. The simplexmq verifier `decryptLinkData`
(`Crypto/ShortLink.hs:100-115`) accepts `signedBy rootKey || any
(signedBy . ownerKey) owners`, and `validateLinkOwners`
(`Agent/Protocol.hs:1821`) walks the prefix chain — both already
upstream and shipping in v6.5+ chat builds. `agentVRange` in
`FixedLinkData` is encoded metadata only; `decryptLinkData` does
not gate on it. The depth cap added in 1.1 is more lenient on
pre-v7 (no cap = accept any length), and we never produce blobs
exceeding the cap, so this asymmetry is benign.

What pre-v7 cannot do: produce chained-owner blobs (no orchestrator
in Phase 3) and accept the new chat-protocol mesh events in Phase
2.4 (`x.grp.owner.invite`, `x.grp.owner.creds`, …). v6.5 readers
see a multi-owner channel correctly but cannot promote, accept
promotion, or sync changes via the mesh.

Release note: "Channels with multiple owners can be read by SimpleX
Chat v6.5 and later, but only edited (promotion, removal, mesh
sync) by v7 and later." Tests:
`testOldClientReadsChainedOwnerBlob`,
`testOldClientReadsRootSignedBlob`,
`testOldClientCannotPromote`.

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
  += `owner_auth_sig BLOB`, `owner_position INTEGER`,
  `owner_rcv_pub_key BLOB`, `wire_owner_key BLOB` (all nullable);
  `groups` += `link_data_version`, `link_data_remote_version`
  (`INTEGER NOT NULL DEFAULT 0`). `wire_owner_key` carries the
  wire-asserted ownerKey when it diverges from local
  `member_pub_key`; NULL when they agree (the common case). Creator-row
  backfill (chain fields **and** `owner_rcv_pub_key`) runs Haskell-side
  at startup (4.4); SQL migration is DDL only because the framework
  type is `Migration { up :: Text }` (`Agent/Store/Shared.hs:30-32`) and
  cannot compute Ed25519 signatures or read the agent DB.
- `M<DATE>_owner_mesh` — `channel_owner_mesh` (`channel_owner_mesh_id`,
  `group_id`, `peer_group_member_id`, `direct_conn_id`, `status TEXT`,
  `pending_inv_conn_req BLOB NULL`, `promoter_member_id INTEGER NULL`,
  timestamps; `UNIQUE(group_id, peer_group_member_id)`). FKs:
  `group_id REFERENCES groups ON DELETE CASCADE`,
  `peer_group_member_id REFERENCES group_members ON DELETE CASCADE`
  (standard cascade on parent removal),
  `direct_conn_id REFERENCES connections ON DELETE SET NULL` —
  when `deleteAgentConnectionsAsync'` deletes the connection (per
  4.3 closure), the FK transitions to NULL while the mesh row is
  preserved with `status = 'closed'`,
  `promoter_member_id REFERENCES group_members ON DELETE SET NULL`.
  `status` ∈ {`'pending'`, `'connected'`, `'closed'`}.
  `pending_inv_conn_req` carries the X-side `IntroInvitation`
  `groupConnReq` while `status = 'pending'` so retried
  `xGrpMemIntro` events can re-send the same invitation without
  re-creating the agent connection (per §2.4); cleared to NULL on
  transition to `'connected'` or `'closed'`.
  `promoter_member_id` is populated only on B's local row to A
  (the row B creates when accepting `XGrpOwnerInvite`) and is the
  basis for B's `XGrpOwnerCreds` sender verification (per §2.4
  item 3); A's rows leave it NULL. Rows are **not deleted** on
  owner removal; status transitions to `'closed'` and the
  referenced `direct_conn_id` is invalidated via
  `deleteAgentConnectionsAsync'`. Preserved for audit and to avoid
  race-window re-establishment from a stale intro.
- `M<DATE>_promotion_in_progress` — `channel_promotion_in_progress`
  (`promotion_id`, `group_id`, `candidate_member_id`,
  `candidate_pub_key`, `candidate_rcv_pub_key`, `step TEXT`,
  `direct_conn_id`, `last_error`,
  `mesh_intros_completed TEXT NOT NULL DEFAULT '[]'`, timestamps;
  `UNIQUE(group_id, candidate_member_id)`). FKs:
  `group_id REFERENCES groups ON DELETE CASCADE`,
  `direct_conn_id REFERENCES connections ON DELETE SET NULL`
  (cancellation may delete the mesh connection while the journal
  row still exists in a terminal state). `mesh_intros_completed`
  is a JSON array of MemberId — A's per-promotion record of
  existing-owner peers (X) whose Step 6 `XGrpMemFwd` has been
  delivered to B; consulted by A's `xGrpMemInv` mesh handler to
  avoid re-forwarding on Step 6 retry (per §2.4). Orchestrator
  journal.

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
memberId, ownerKey = coalesce(wire_owner_key, member_pub_key),
authOwnerSig = ownerAuthSig }` ordered by `owner_position`. The
`coalesce` reproduces the wire byte-for-byte even when the local
view diverges from the wire-asserted key (key-mismatch case in G5
below), so `validateLinkOwners` accepts the re-encoded chain.
**G6 invariant:** `OwnerAuth.ownerId` is the raw bytes of
`MemberId`; encode and decode assert this.

**G5 — `applyChannelOwnerRoster` ingest rule.** Three cases:

- Known `woMemberId` with matching `woOwnerKey` (local
  `member_pub_key == woOwnerKey`) → update `member_role`,
  `owner_auth_sig`, `owner_position`, `owner_rcv_pub_key` (latter
  only when `woRcvKey` is `Just`; never overwrite non-null with
  NULL); set `wire_owner_key = NULL` (clear any prior divergence).
- Known `woMemberId` with **mismatching** `woOwnerKey` (local
  `member_pub_key ≠ woOwnerKey`) → store the wire's `woOwnerKey`
  in `wire_owner_key`; update other owner_* columns from the wire.
  Do NOT overwrite local `member_pub_key`. The chain entry stays
  so the blob still verifies on re-encode (via the `coalesce` in
  `getChannelOwnerAuths`); standard member-info gossip reconciles
  the divergence later.
- Unknown `woMemberId` → record as a **pending-member row**
  (placeholder status, `member_role = 'owner'`); store
  `wire_owner_key = woOwnerKey` AND set `member_pub_key =
  woOwnerKey` (no local view exists to disagree with); populate
  other owner_* columns verbatim from the wire.

Local owner rows not in the wire list → demote, clear owner_*
columns and `wire_owner_key`.

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
through. Enforce `length owners ≤ ownerChainDepth` before encode.

**Call-site refactor.** Both call sites today
(`Internal.hs:1306-1322`) fetch `(conn, groupRelays)` inside
`withFastStore` / `withStore` and then call the **pure**
`groupLinkData` outside the transaction (e.g., `Internal.hs:1311`,
`Internal.hs:1321`). Once `groupLinkData` is `IO`, both calls must
be moved **into** the existing transaction — same `DB.Connection`,
no second `withFastStore`. The IO-only operations
(`getChannelOwnerAuths`, `incrementLinkDataVersion`,
`getConnectedGroupRelays`) are composed via `liftIO` inside the
`ExceptT StoreError IO` wrapper (`Controller.hs:1630`); no new
transaction is opened. The diff is a structural reshape of two
callers, not a new code path.

- `setGroupLinkData` (`Internal.hs:1306-1314`): replace the
  `(conn, groupRelays) <- withFastStore $ \db -> ...` /
  `let (userLinkData, crClientData) = groupLinkData ...` pair with
  one `withFastStore` block that returns
  `(conn, userLinkData, crClientData)`. Inside the block:
  `getGroupLinkConnection` (ExceptT) followed by `liftIO` of
  `getConnectedGroupRelays` and `groupLinkData`. `groupRelays`
  becomes a transaction-local `let`; it does not escape.
- `setGroupLinkDataAsync` (`Internal.hs:1316-1322`): same shape,
  using `withStore` (the existing call already uses `withStore`,
  not `withFastStore`).

Trade-off considered and rejected: option (b) — keep
`groupLinkData` outside and add a second `withFastStore` to wrap
it. That doubles the transaction count per LSET-style update and
offers no clarity gain; the data flow is one straight line from
`getChannelOwnerAuths`/`incrementLinkDataVersion` through encode to
the LSET payload. Option (a) — single transaction — is the minimal
diff and matches the surrounding pattern in `Internal.hs`.

`incrementLinkDataVersion` may run ahead of successful
`setGroupLinkData` on transient failure (the agent call
`setConnShortLink` runs **after** the `withFastStore` block, so
network failure leaves the version bumped but the server unchanged);
this is acceptable — local version is allowed to drift ahead of
server, and the LWW `max(local, serverV) + 1` reconciliation is
unaffected.

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
   { groupId, bundle })`. A → B direct mesh only; never relay-
   forwarded. Receiver rejects unless `groupId` matches. **Sender
   verification on B's side:** B records the promoter at invite-
   acceptance time (Step 1 → 2) and consults this record on bundle
   receipt.

   Storage: `channel_owner_mesh.promoter_member_id INTEGER NULL`
   (per §2.1). The column is meaningful only on B's local mesh row
   to A — the row B creates when accepting `XGrpOwnerInvite`. A's
   mesh rows to its peers leave this column NULL (A's source of
   truth is its `channel_promotion_in_progress` journal). A's
   `channel_promotion_in_progress` row is **not needed** for
   verification — only B's local state matters at receive time,
   since the journal is per-device.

   Write site (B): when B's `xGrpOwnerInvite` handler creates the
   mesh `ContactConnection` from `meshConnReq` and INSERTs the
   `channel_owner_mesh` row for `(group, A)`, populate
   `promoter_member_id` with A's `groupMemberId` (resolvable from
   the personal-contact connection over which `XGrpOwnerInvite`
   arrived; A is a known group member of B since the channel
   pre-dates the promotion).

   Read site (B): in `xGrpOwnerCreds` handler, before invoking
   `acceptCoOwnerCreds` (chat-side §1.5):

   ```sql
   SELECT promoter_member_id
     FROM channel_owner_mesh
     WHERE group_id = ?           -- envelope groupId resolved to local groupId
       AND direct_conn_id = ?     -- the mesh connection delivering this event
   ```

   Reject unless the row exists AND `promoter_member_id` is
   non-NULL AND the sender's groupMemberId (resolved from the mesh
   `Connection`'s peer member) equals `promoter_member_id`. Any
   mismatch ⇒ log + drop; B does not proceed to intake. Prevents
   same-channel cross-peer bundle replay or impersonation: a
   different mesh peer C cannot supply bundle bytes to B even if C
   somehow obtained them, because C's `groupMemberId` won't match
   B's recorded promoter.

   Lifecycle: `promoter_member_id` is NOT cleared after Step 5 ack
   succeeds — mesh rows are preserved for audit per §2.1, and the
   column has no downside in being left in place. If B is later
   re-promoted by a different peer (after a removal cycle), the
   re-acceptance updates `promoter_member_id` in place via B's
   xGrpOwnerInvite handler when it transitions a `'closed'` row
   back to `'pending'`. Test: `testBundleSenderMustBePromoter` (B
   sets up a mesh edge to A as the promoter; a second mesh peer C
   sends `XGrpOwnerCreds` for the same `groupId` over a separate
   mesh edge; assert B rejects without invoking
   `acceptCoOwnerCreds`).
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
handler takes a mesh-scoped early-branch.

**Mesh edges are NEW connections, not pre-existing ratchets.** In
`useRelays'` channels (the only mode this delivery targets), members
do NOT share direct double-ratchet connections — they communicate
via relays. The non-relays code path in `xGrpMemIntro`
(`Subscriber.hs:2972-2984`) creates an agent connection via
`createAgentConnectionAsync` (line 2980, helper `createConn` at
2988); the relays branch at lines 2965-2971 does NOT — for normal
relay-mediated members, no direct connection is needed. Multi-owner
channels reverse that for owners only: the mesh edge IS a fresh
agent ContactConnection created specifically for inter-owner mesh
delivery, regardless of the relays mode. **The mesh-scoped branch
must therefore call `createAgentConnectionAsync` explicitly**, then
record the resulting `connId` in `channel_owner_mesh.direct_conn_id`.
`directConnReq` in the `IntroInvitation` payload stays `Nothing` —
legacy, unused in modern channels.

Per-handler behavior, with explicit receiver-side idempotency to
make the Step 6 retry cadence (Phase 3) safe:

- **`xGrpMemIntro` (mesh path on X, receiving intro about
  introduced peer B from introducer A).** Skip the `GCHostMember`
  check (`Subscriber.hs:2956, 2986`) and the chat-side member-
  creation writes (`createIntroReMember`,
  `updatePreparedChannelMember`, `createNewGroupMember`). De-dup
  query, by `(group_id, peer_group_member_id)` keyed on B's
  locally-known `groupMemberId` (resolved by
  `getGroupMemberByMemberId` using the `MemberInfo.memId` from the
  wire):

  ```sql
  SELECT status, direct_conn_id, pending_inv_conn_req
    FROM channel_owner_mesh
    WHERE group_id = ? AND peer_group_member_id = ?
  ```

  Branches:
  - No row → INSERT row with `status = 'pending'`; call
    `createAgentConnectionAsync user CFCreateConnGrpMemInv
    (chatHasNtfs chatSettings) SCMInvitation subMode` (mirrors
    `Subscriber.hs:2988`); store the resulting `connId` in
    `direct_conn_id` and the agent's invitation `groupConnReq` in
    `pending_inv_conn_req`; emit `XGrpMemInv` to A carrying the
    `groupConnReq`.
  - Row with `status = 'pending'` → **do NOT call
    `createAgentConnectionAsync` again**; re-emit `XGrpMemInv` to
    A using the stored `pending_inv_conn_req` (re-sends the same
    invitation A would have received on the first attempt; safe
    against retry).
  - Row with `status = 'connected'` → no-op (the mesh edge to B is
    established; A's orchestrator will observe completion via
    Step 7's `XGrpLinkSync`).
  - Row with `status = 'closed'` → reject (the peer was previously
    removed — the row preservation per §2.1 is load-bearing here;
    do not re-establish).

- **`xGrpMemInv` (mesh path on A, receiving inv from X about
  introduced peer B).** Skip the `GCInviteeMember` check
  (`Subscriber.hs:3004`). The intro IS A's own (re-)retry, so the
  natural de-dup site is A's orchestrator journal, not the
  `channel_owner_mesh` row (A's row to B exists since Step 1-2 and
  A's row to X exists from prior promotions; neither indicates
  Step 6 fwd-completion). The
  `channel_promotion_in_progress.mesh_intros_completed` JSON-array
  column (per §2.1) holds X member IDs whose fwd has been delivered
  to B. Query:

  ```sql
  SELECT mesh_intros_completed
    FROM channel_promotion_in_progress
    WHERE group_id = ? AND candidate_member_id = ?
  ```

  Branches:
  - X's member ID present in the JSON array → no-op (already
    forwarded; A's prior `XGrpMemFwd` to B succeeded or B is
    handling its own dedup; either way, do not re-forward).
  - X's member ID absent → emit `XGrpMemFwd` to B carrying X's
    `ownerRcvPubKey`; on send success, append X's member ID to the
    JSON array.

- **`xGrpMemFwd` (mesh path on B, receiving fwd from A with X's
  introInvitation).** Skip both `createNewGroupMember`
  (missing-member branch) and `createIntroToMemberContact`
  (normal-group chat-side state). De-dup query, keyed on X's
  `groupMemberId` (resolved via `getGroupMemberByMemberId` from
  the wire's `MemberInfo.memId` — X is an existing owner, B has
  known X for some time):

  ```sql
  SELECT status FROM channel_owner_mesh
    WHERE group_id = ? AND peer_group_member_id = ?
  ```

  Branches:
  - No row → INSERT `status = 'pending'`; call
    `joinAgentConnectionAsync user Nothing (chatHasNtfs
    chatSettings) groupConnReq dm subMode` (mirrors
    `Subscriber.hs:3032`); store the resulting `connId` in
    `direct_conn_id`. Critically: this is the only path on which
    `joinAgentConnectionAsync` runs — preventing the orphan-edge
    accumulation that would otherwise occur on Step 6 retry.
  - Row with `status` ∈ {`'pending'`, `'connected'`} → no-op (the
    mesh edge to X is in flight or established).
  - Row with `status = 'closed'` → reject.

Else-branch (downgrade) runs the standard flow. Tests:
`testMeshScopeRequiresOwnerIntroducer`,
`testMeshIntroIdempotentOnRetry`,
`testMeshInvNotReforwarded`,
`testMeshFwdIdempotentOnRetry`.

**Transient roster lag.** If the introducer is not yet visible
as an owner in the receiver's local roster (transient lag after a
promotion), the receiver downgrades to the standard intro path,
which then rejects (sender is not `GCHostMember`). The orchestrator
re-emits the intro per the Step 6 retry cadence in Phase 3 (the
chat-protocol layer does not retry `messageError` rejections at the
receiver), eventually succeeding once the receiver's next LGET
refresh propagates the updated roster.

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

**B's consent (Steps 1 → 2).** B's UI surfaces the
`XGrpOwnerInvite` as a notification ("A is offering to make you an
owner of channel <name>") with Accept / Decline actions. On Accept,
B's client joins the mesh `meshConnReq` ContactConnection and
proceeds to Step 2 (`XGrpOwnerAccept`). On Decline, B's client does
not join the mesh; A's orchestrator times out per the Step 6 retry
cadence below, then surfaces "B declined or did not respond" to A's
UI after the hour-long bound. A may then `apiCancelPromotion`.

**Idempotency.** Step 3 skips if B's rcv key is already in the desired
set after pre-flight; Step 4 LGETs and skips if B's owner row already
chain-verifies; Step 5 advances its marker only on
`XGrpOwnerCredsAck` so a lost bundle re-sends rather than
silently dropping; Step 6 keys on per-peer mesh state; Step 7 relies
on `x.grp.mem.role` idempotence.

**Step 6 retry cadence.** Within an app session the orchestrator
polls `channel_owner_mesh` per (group, peer) pair every 30s while
any row's `status` is `'pending'`; for each still-pending row,
re-emit the `x.grp.mem.intro` with the same `ownerRcvPubKey`
payload. Backoff to 5min after 5 consecutive failures. Surface to
UI on hour-long persistent failure. The standard chat-protocol
delivery layer does not retry `messageError` rejections at the
receiver (`Subscriber.hs:1746` is fire-and-forget toView), so the
orchestrator drives retry from its own journal.

**Retry safety.** Re-emission is non-destructive because each
mesh-scoped handler in §2.4 short-circuits on receiver-side state:
X's `xGrpMemIntro` re-uses the stored `pending_inv_conn_req` (no
duplicate `createAgentConnectionAsync`); A's `xGrpMemInv` consults
`channel_promotion_in_progress.mesh_intros_completed` (no duplicate
`XGrpMemFwd` emission); B's `xGrpMemFwd` skips if a `(group, X)`
mesh row already exists (no duplicate `joinAgentConnectionAsync`).
The 30s/5min/1h cadence is unchanged; only the receiver-side
guarantees are new. Tests for the safety property:
`testMeshIntroIdempotentOnRetry`, `testMeshInvNotReforwarded`,
`testMeshFwdIdempotentOnRetry` (all defined in §2.4).

**Failure modes.** 3→4 fail: stale rcv key sits in queue's set; Step 3
retry detects, Step 4 retries cleanly. 4→5 fail: B is on-server owner
without credentials — "owner credentials missing — request resend"
surfaces via next LGET. 6 partial: retry per-peer at next app start. 7
fail: subscribers see B as non-owner temporarily.

**Cancellation (`apiCancelPromotion`).** Semantics by step:

- Cancel from `invitation_sent` or `creds_received`: tear down the
  channel-scoped direct mesh ContactConnection; delete the
  `channel_promotion_in_progress` row. No server-side effect to
  undo.
- Cancel from `rkey_done` or `lset_done` (server-side state has
  changed): drive the **removal flow (4.3) targeted at B** as the
  rollback — `setQueueRecipientKeys` first (drop B's rcv key),
  then LSET (drop B from chain), broadcasts, mesh closure. Then
  delete the journal row.
- Cancel from `bundle_sent` onward: rollback is the same removal
  flow targeted at B. B may receive `XGrpLinkSync` removing itself
  and self-demote per 4.5 Case 4 logic (the bundle they hold
  becomes effectively useless because the chain no longer accepts
  them).

```haskell
-- Simplex.Chat.Library.Owners  (new module)
promoteToOwner   :: User -> GroupInfo -> GroupMember -> CM ()
resumePromotions :: CM ()     -- wired into Simplex.Chat.Core startup
```

Tests: `testPromoteOwner`, `testPromoteOwnerResumeStep<N>`,
`testPromoteIdempotentRetry`, `testCandidateDeclinePromotion`,
`testStep6RetriesOnRosterLag`,
`testCancelPromotionFromEachStep`.

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

Steps: compute `OwnerRemoval` → recompute `recipientKeys` from
`owner_rcv_pub_key` minus removed → `setQueueRecipientKeys`
(preceded by Risk #12 pre-flight) → bump version, LSET (cascade
pruned writer-side) → broadcast `x.grp.mem.role` via relays for
each removed owner → broadcast `XGrpLinkSync` over mesh →
transition `channel_owner_mesh.status` to `'closed'` for removed
peers (rows preserved) and invalidate their `direct_conn_id` via
`deleteAgentConnectionsAsync'`.

Order matters: `setQueueRecipientKeys` runs before LSET to revoke
SMP write access first. If LSET ran first and
`setQueueRecipientKeys` failed (or was delayed), a misbehaving
removed owner could observe their removal and re-LSET themselves
back into the chain before their access was revoked (their
`OwnerAuth_X` remains valid as long as their authorizer is still
in the chain). With keys revoked first, the removed owner's LSET
attempts return `ERR AUTH` regardless of the chain-blob state.

D3 surfaces blockage with "You cannot remove the channel creator —
your owner role was authorized by them." Single-owner channels:
the sole root-signed owner cannot be removed.

Sole-owner channel destruction: the existing channel-deletion flow
(chat-side `deleteGroupChat` / `apiDeleteChat`) is unchanged for
channels. It tears down the channel's queues (LDEL on the link
queue, deletion of relay/group queues), removes all local state,
and is permitted for the sole owner via the existing creator-only
chat-layer gate. Multi-owner channels: any owner can run
`deleteGroupChat` locally (removing their own view), but `LDEL` of
the link queue requires `GRKPrivate _` (creator-only, per 1.3
gate); chained owners attempting deletion get `CEPermissionDenied`.
Effect for chained owners: they leave the channel locally but the
channel persists on the server until the creator (or whoever holds
`GRKPrivate _`) issues `LDEL`. No new tests; covered by existing
channel-deletion tests + `testCoOwnerCannotDeleteQueue`.

Tests: `testCascadeRemoval`, `testRemoverCascadeBlocked`,
`testRootCannotBeRemovedSoloOwner`, `testCascadeUiPreviewShowsAll`.

**4.4 Single-owner → multi-owner upgrade and convergence pre-flight.**
Single unified startup pass in `Simplex.Chat.Library.Owners`, wired
into `Simplex.Chat.Core` startup. SQL migration in 2.1 adds columns
only; all backfill is Haskell-side. The pass has two parts:

*Backfill.* Iterate groups with non-null `member_priv_key` and a
creator row lacking `owner_auth_sig`; for each: set `member_role =
'owner'`; write `owner_auth_sig = sign(rootPrivKey, memberId ‖
encodePubKey(publicKey memberPrivKey))`; write `owner_position = 0`;
query the agent via `getChannelLinkRcvPubKey :: AgentClient -> ConnId
-> AE SMP.RcvPublicAuthKey` (derives the pubkey from `rcvPrivateKey`
stored in the `RcvQueue` row via the existing connection-by-connId
lookup; no new SMP command) and write `owner_rcv_pub_key`.
Idempotent; one pass per device. If `getChannelLinkRcvPubKey` fails
for a specific channel (missing connection, agent error), skip that
channel with a logged warning; do not block chat startup; surface to
UI for manual investigation. Other channels' backfills proceed
independently.

*Convergence pre-flight.* For each multi-owner channel on this
device, additionally run one Risk #12 pre-flight pass at startup:
compute the desired `recipientKeys` from the chain (via
`getChannelOwnerAuths`) ∪ locally-known `owner_rcv_pub_key`
mappings; compare against the previous desired set persisted from
this device's last `setQueueRecipientKeys`; if they differ, push the
new desired set via `setQueueRecipientKeys` (bounded retry as 4.2).
Idempotent: if the server is already correct, the call is a no-op
write. This resolves both 4.5 Case 3 (re-adds a chain-valid owner
whose key was dropped) and 4.5 Case 4 (drops an orphaned key + emits
a demotion `XGrpLinkSync` to the affected party over the existing
A↔B mesh edge if reachable) without waiting for a manual
promotion/removal trigger. Note: server-side `recipientKeys` is
unreadable per G3, so the comparison is against the device's
last-known desired set, not against the server.

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
`testCreatorRcvPubKeyBackfilledAtStartup`,
`testMemberKeyEqualsOwnerKey`, `testStartupPreflightConvergesRaces`.

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
- Device migration for a single profile. After restoring a chat
  profile to a new device, the agent DB is fresh; the new device's
  `rcv_private_key` for the channel link queue does not match
  `group_members.owner_rcv_pub_key` carried in the link blob from
  the old device. The migrated user is silently locked out of
  channel-owner write access until a separate device-migration flow
  is added (post-MVP).
