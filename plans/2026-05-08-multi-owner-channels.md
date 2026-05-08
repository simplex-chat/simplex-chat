# Multi-owner Channels — implementation plan

Revision 2, 2026-05-08 · Target: SimpleX Channels v7 (full-trust, any-owner-decides)

### Changelog (since Revision 1)

- Removed pin-vs-upstream discussion (both repos at master).
- Removed `linkRootSigKey` persistence work; rootKey already covered by
  chat-side `GroupKeys.groupRootKey` and fixed-data extraction at decode
  time.
- Owner roster persisted by extending `group_members` with three nullable
  columns; no separate `channel_owners` or `channel_co_owner_creds` tables.
- Added rationale for putting `linkDataVersion` in chat-layer JSON.
- Promoted owners-cap-of-8 to a top-level "Design decision required"
  callout for team discussion.
- Owner mesh restricted to protocol events only in this delivery; non-
  protocol messages prohibited (with TODO).
- Added Concurrent-RKEY-recovery sub-section under Phase 4.
- Added member-pub-key fixed-for-channel-lifetime invariant.
- Risk register entries 10 and 11 added.
- Open questions resolved; depth cap is the only remaining team-level call.

> **Important constraint up front.** Promoting a subscriber to owner via a
> relay-mediated offer (no direct connection between the existing owner and
> the candidate) is **not** supported in this delivery. Promotion always
> requires a fresh, channel-scoped direct mesh connection between an existing
> owner and the candidate. Relay-mediated promotion may be reconsidered as a
> follow-up.

---

## 1. Summary

This plan extends the SimpleX Channels MVP from single-owner to "any-owner-
decides" multi-owner. Owners share an SMP recipient queue (the channel link
queue), each holds an Ed25519 owner private key, each can independently push
mutable-blob updates (LSET) and rotate the recipient-key set (RKEY) without
coordination. A dedicated owner mesh (channel-scoped, direct, P2P, modeled on
the existing `x.grp.mem.intro` flow) propagates link-data changes between
owners, with last-writer-wins on the SMP server as the authoritative resolver.

Out of scope for this delivery: multisig, programmable governance, public-
group migration over relays, root-creator transfer, owner-only chat UI
surface (data plumbing is included; the tab/thread is not). See section 9.

---

> **DESIGN DECISION REQUIRED — discuss with team before implementation.**
>
> Maximum number of owners per channel: proposed cap of 8.
>
> Rationale: each `OwnerAuth` encodes to ~189 bytes; 8 owners cost ~1.5 KB
> of the 13.4 KB user-data padded budget. Cap is enforced in
> `validateLinkOwners` on both encode and decode paths. At ≤8, prefix
> chain validation is cheap (O(N²) ≈ 64 Ed25519 verifies maximum).
>
> If the team needs higher (e.g., 16, 32), the cap is a one-line constant
> change; the cost is a larger blob and modestly more expensive validation.
> If the team needs lower (e.g., 4), simpler still.
>
> **Implementation must not begin until this is resolved.** Plan defaults
> to 8.

---

## 2. Phase ordering

Phases run mostly in sequence; the iOS and Kotlin UI work in phase 5 can
parallelize after phase 4 lands.

| # | Phase | Depends on | Parallelizable |
|---|---|---|---|
| 1 | Agent: chain depth cap, RKEY wrapper, co-owner credential bundle, tests | — | no |
| 2 | Chat: schema migrations + owner-roster helpers + owner mesh (data model + transport) | 1 | no |
| 3 | Chat: promote-to-owner flow (orchestrator, persistent state, idempotent steps) | 2 | no |
| 4 | Chat: cross-owner link-sync + LWW reconciliation + owner-removal + concurrent-RKEY recovery | 3 | no |
| 5a | iOS UI (Swift) — owners section, add/remove flows, sync indicator | 4 | yes |
| 5b | Kotlin multiplatform UI (Android + desktop) — same scope | 4 | yes |
| 6 | End-to-end tests + threat-model regression suite | 5a + 5b | no |

---

## 3. Per-phase implementation steps

### Phase 1 — Agent layer (simplexmq)

> **Repo:** `/home/builder/code/simplexmq`. Both this repo and the chat
> repo are at master; no pin-vs-upstream coordination is needed.
>
> **Phase 1 is substantially smaller than Revision 1 envisaged.** Most of
> the verification path (chain validation, signer-agnostic encode/sign,
> chain-aware decrypt) is already in place upstream. Phase 1 reduces to:
> (a) chain depth cap, (b) RKEY agent wrapper, (c) co-owner credential
> bundle format and intake API, (d) tests.

**Preflight reads** (no code changes; output is a one-paragraph
confirmation in the PR description quoting the line refs below):

- `/home/builder/code/simplexmq/src/Simplex/Messaging/Crypto/ShortLink.hs:100-115`
  — `decryptLinkData` extracts `rootKey` from FixedLinkData and accepts
  owner-chain signatures via `validateLinkOwners`.
- `/home/builder/code/simplexmq/src/Simplex/Messaging/Agent/Protocol.hs:1792-1835`
  — `OwnerAuth` (3 fields) + `validateLinkOwners` (prefix-only chain
  authorization, duplicate detection).
- `/home/builder/code/simplexmq/src/Simplex/Messaging/Server.hs:1228-1290, 1465-1486`
  — server-side any-of-N for `SRecipient` + `RKEY` restricted to
  `QMContact`.
- `/workspace/src/Simplex/Chat/Library/Internal.hs:1313-1399, 2474-2477`
  — chat-side `groupLinkData`, `setGroupLinkDataAsync`,
  `setAgentConnShortLinkAsync`.
- `/workspace/src/Simplex/Chat/Library/Commands.hs:2496-2527, 4042-4238`
  — channel creation; `verifyLinkOwner` (already chain-aware).
- `/workspace/src/Simplex/Chat/Store/Groups.hs:1860-1900, 2999-3020`
  — `updateGroupMemberKeys`, `updateRelayGroupKeys`,
  `createLinkOwnerMember`.
- `/workspace/src/Simplex/Chat/Library/Subscriber.hs:2950-3070`
  — existing `xGrpMemIntro`/`xGrpMemInv`/`xGrpMemFwd` flow we will reuse
  for the owner mesh.
- `/workspace/src/Simplex/Chat/Types.hs:451-465`
  — `GroupRootKey = GRKPrivate {rootPrivKey} | GRKPublic {rootPubKey}` and
  `GroupKeys`. Loaded from `rootPrivKey_` / `rootPubKey_` columns by
  `Store/Shared.hs:699-701`. The chat layer already has the state needed
  to distinguish creator from co-owner; the agent does not need a parallel
  notion.

#### 1.1 Chain validation: depth cap

The upstream `decryptLinkData`
(`Simplex/Messaging/Crypto/ShortLink.hs:100-115`) already verifies that
the user-data signature is valid against either `rootKey` or any
chain-validated owner's `ownerKey`. The chain itself is checked by
`validateLinkOwners`
(`Simplex/Messaging/Agent/Protocol.hs:1821-1835`), which:
- detects duplicates in the owners list (loop / replay protection),
- requires every owner's `authOwnerSig` to verify against either rootKey
  or against an *earlier* owner in the list,
- enforces "earlier" by only allowing an owner to be authorized by the
  prefix of the list that comes before them — depth bounded by the list
  length and naturally loop-free.

**Deltas to add:**

```haskell
-- Simplex.Messaging.Agent.Protocol
ownerChainDepth :: Int
ownerChainDepth = 8
-- new constant; team decision pending (see top-level callout).

-- existing function, made depth-aware
validateLinkOwners
  :: C.PublicKeyEd25519
  -> [OwnerAuth]
  -> Either String ()
validateLinkOwners rootKey owners
  | length owners > ownerChainDepth = Left "owners list too long"
  | otherwise = ...   -- existing logic
```

Plumb the cap so it is enforced both at encode time (the chat layer's
`groupLinkData` rejects rosters longer than the cap before serializing)
and at decode time (`decryptLinkData` rejects on read).

**Privacy property to preserve.** The wire format is `sig64 || md_bytes`
with no signer ID inside; an observer who has the link key and decrypts
the blob cannot determine which owner pushed the latest write — only
that *some* chain-valid owner did. This satisfies channels-overview.md
objective #6 (sender anonymity within multi-owner channels). Ring
signatures from RFC option 2 (Multiple owners managing queue data) remain
deferred.

#### 1.2 Encode-and-sign: caller-side key selection

The upstream `encodeSignUserData` already accepts an arbitrary
`PrivateKeyEd25519`:

```haskell
-- Simplex.Messaging.Crypto.ShortLink (already exists upstream)
encodeSignUserData
  :: ConnectionModeI c
  => SConnectionMode c
  -> C.PrivateKeyEd25519        -- signing key (root OR co-owner)
  -> VersionRangeSMPA
  -> UserConnLinkData c
  -> ByteString
```

The function is signer-agnostic. **No agent-side persistence change is
required.** Instead, the chat layer chooses which key to put into
`ShortLinkCreds.linkPrivSigKey` at the call site, using the existing
`GroupRootKey` carried in `GroupKeys`:

| Channel role this device plays | `GroupKeys.groupRootKey` | Key passed as `linkPrivSigKey` |
|---|---|---|
| Creator (single-owner today, also a multi-owner case) | `GRKPrivate rootPrivKey` | `rootPrivKey` (today's path) |
| Co-owner promoted later | `GRKPublic rootPubKey` | this owner's `memberPrivKey` (= owner private key, by the invariant in 4.4) |

The chat layer already loads `GroupRootKey` for both cases via
`Store/Shared.hs:699-701`; the integer additional work is at the chat-
side caller of the agent's `setConnShortLink` family.

**`linkRootSigKey` persistence is NOT in scope.** The agent's
`linkRootSigKey :: Maybe C.PublicKeyEd25519` field on `ShortLinkCreds`
exists upstream as forward-compat for a future "trust anchor moved"
scenario unrelated to this work. The TODO at
`Agent/Store/AgentStore.hs:2514` (literal:
`linkRootSigKey = Nothing -- TODO linkRootSigKey should be stored in a
separate field`) stays untouched. Verification on read does not need it
— `decryptLinkData` extracts `rootKey` from the immutable fixed-data
block on every decode (`ShortLink.hs:104`).

#### 1.3 Co-owner queue state

A non-root owner's device must be able to issue `LSET`/`LDEL`/`RKEY`
against the channel's recipient queue. The minimal change is to make
the channel's link queue look — to the agent — like a normal
`ContactConnection` *for the co-owner too*: the co-owner's device gets
its own `RcvQueue` record pointing at the same `(server, rcvId)` but
holding the co-owner's `rcvPrivateKey` and `ShortLinkCreds`.

```haskell
-- StoredRcvQueue gains nothing structural; the co-owner record reuses
-- existing fields:
--   rcvId         = channel link queue's recipient ID
--                   (identical across all owners on the SMP server)
--   rcvPrivateKey = co-owner's recipient auth private key
--                   (its public counterpart is added to recipientKeys via RKEY).
--                   Generated fresh by C.generateAuthKeyPair on the co-owner's
--                   device; queue-specific; NOT the same as member_pub_key.
--   shortLink     = ShortLinkCreds with co-owner's linkPrivSigKey
--                   (= their owner private key);
--                   linkRootSigKey stays Nothing (verification reads rootKey
--                   from fixed data).
--   queueMode     = Just QMContact
--   primary       = True; this is the co-owner's only queue for this channel
```

Decision: **extend the existing `RcvQueue` rather than introduce a
parallel record.** Reasoning: the agent's connection model is "one
RcvQueue per (connId, queueIndex)"; a co-owner's view *is* that — they
have their own connId for the channel link queue, with credentials they
exclusively control. Server-side, the SMP queue is shared via the
`recipientKeys :: NonEmpty RcvPublicAuthKey` list. The agent already
treats SUB/RKEY/LSET/LDEL/QUE/OFF/DEL/NDEL/NKEY as "any recipient key
in the queue's list authorizes" (verified at
`Server.hs:1236-1237` via `verifiedWithKeys`).

A small caveat surfaces: agents call `DEL` to delete the queue. If a
co-owner DEL'd, every other co-owner's queue would suddenly be
orphaned. This plan **prohibits the co-owner from DEL'ing** at the chat
layer: chat-side gating uses `GroupKeys.groupRootKey` — `DEL` is
permitted only when this device holds `GRKPrivate _` (creator role).
No agent-side flag is added.

#### 1.4 RKEY agent wrapper

There is currently no `RKEY` wrapper on the agent. Add:

```haskell
-- Simplex.Messaging.Agent.Client
setQueueRecipientKeys
  :: AgentClient
  -> NetworkRequestMode
  -> RcvQueue
  -> NonEmpty SMP.RcvPublicAuthKey
  -> AM ()
setQueueRecipientKeys c nm rq@RcvQueue{rcvId, rcvPrivateKey} ks =
  withSMPClient c nm rq "RKEY" $ \smp ->
    sendSMPQueueRecipientKeys smp nm rcvPrivateKey rcvId ks
-- restricted server-side to QMContact (Server.hs:1471 already enforces).
```

And a low-level companion in `Simplex.Messaging.Client`:

```haskell
sendSMPQueueRecipientKeys
  :: SMPClient
  -> NetworkRequestMode
  -> RcvPrivateAuthKey
  -> SMP.RecipientId
  -> NonEmpty SMP.RcvPublicAuthKey
  -> ExceptT SMPClientError IO ()
sendSMPQueueRecipientKeys smp nm pk rcvId ks =
  okSMPCommand (Cmd SRecipient (RKEY ks)) smp nm pk rcvId
```

Expose at `Agent.hs`:

```haskell
setQueueRecipientKeys :: AgentClient -> NetworkRequestMode -> ConnId
                     -> NonEmpty SMP.RcvPublicAuthKey -> AE ()
setQueueRecipientKeys c = withAgentEnv c .::. setQueueRecipientKeys' c
```

Single primitive (set the full key list) is preferred over RADD/RDEL —
the SMP server's `RKEY` already takes the full list in one transaction
(no race window between two-step add+remove). Caller responsibility:
read current set, mutate, send. Concurrent races between owners are
recovered by the eventual-consistency loop in Phase 4.5.

#### 1.5 Co-owner setConnShortLink path

The existing `setConnShortLink'`
(`Simplex.Messaging.Agent.hs:840-871`) signs with whatever
`linkPrivSigKey` is on the connection's `ShortLinkCreds`. With 1.2's
chat-side key-selection logic, a co-owner's connection has the co-
owner's owner private key in that field — so **no code change is
required at this call site**. The verifier accepts owner-signed blobs.

**Plan note.** We considered adding a sibling
`setCoOwnerConnShortLink` to make caller intent explicit. Rejected: the
existing entry point already does the right thing once the chat layer
selects the correct key. Adding a parallel entry would duplicate the
call graph for no semantic gain. Caller intent is captured at the chat
layer's `groupLinkData` builder (phase 2), which selects between
`GRKPrivate.rootPrivKey` and the device's `memberPrivKey` based on
`GroupRootKey`.

#### 1.6 Co-owner credential bundle (wire format)

Existing owner A sends to candidate B over the channel-scoped direct
mesh connection (phase 3). Bundle is sent inside the chat-layer
envelope as a new `XGrpOwnerCreds` event (defined in phase 2). Agent
provides the encoded byte format; chat wraps it.

```haskell
-- Simplex.Messaging.Agent.Protocol (new)
data CoOwnerCredsBundle = CoOwnerCredsBundle
  { server         :: SMPServer
  , rcvId          :: SMP.RecipientId
  , rcvDhSecret    :: RcvDhSecret      -- channel link queues do not carry
                                       -- messages, but include for forward
                                       -- compat
  , shortLinkId    :: SMP.LinkId
  , shortLinkKey   :: LinkKey
  , rootPubKey     :: C.PublicKeyEd25519
                                       -- chat-layer destination:
                                       -- groups.root_pub_key on B's device
                                       -- (= GRKPublic rootPubKey).
                                       -- Not used by the agent; the agent
                                       -- reads rootKey from fixed data on
                                       -- every decode.
  , linkEncFixedData :: SMP.EncFixedDataBytes
  , agentVRange    :: VersionRangeSMPA
  }
  deriving (Eq, Show)

instance Encoding CoOwnerCredsBundle where
  smpEncode CoOwnerCredsBundle{..} =
    smpEncode (server, rcvId, rcvDhSecret, shortLinkId, shortLinkKey,
               rootPubKey, linkEncFixedData, agentVRange)
  smpP = ... -- mirror
```

B's *own* `rcvPrivateKey` (recipient auth) and `ownerPrivKey` are
generated locally on B's device — never in the bundle. B's
`rcvPrivateKey` is queue-specific and unrelated to B's
`member_pub_key`; reusing the chat-layer member key as an SMP rcv auth
key would leak chat identity into the SMP server's `recipientKeys` list
and is not allowed.

Intake API on the agent:

```haskell
-- Simplex.Messaging.Agent
acceptCoOwnerCreds
  :: AgentClient
  -> NetworkRequestMode
  -> UserId
  -> CoOwnerCredsBundle
  -> RcvPrivateAuthKey       -- generated locally on B
  -> C.PrivateKeyEd25519     -- B's owner private key (= member signing key)
  -> AE ConnId               -- newly-created channel-link RcvQueue connId
```

This creates B's local `RcvQueue` + `ShortLinkCreds` row pointing at
the shared `rcvId`, with B's keys. The chat layer separately stores
`rootPubKey` into `groups.root_pub_key` and constructs
`GroupKeys{groupRootKey = GRKPublic rootPubKey}`.

#### 1.7 Mutable-blob version field

Add a `linkDataVersion :: Maybe Word64` field to the chat-layer
`GroupShortLinkData` JSON for last-writer-wins reconciliation.

```haskell
-- Simplex.Chat.Types (or co-located near GroupShortLinkData)
data GroupShortLinkData = GroupShortLinkData
  { groupProfile     :: GroupProfile
  , publicGroupData  :: Maybe PublicGroupData
  , linkDataVersion  :: Maybe Word64    -- new; absent => treat as 0
  }
```

> The version field lives in the chat-layer `GroupShortLinkData` JSON,
> encoded into the agent-layer `userData` ByteString as opaque payload.
> Rationale: the version is a channel-feature concept (last-writer-wins
> reconciliation between concurrent owner edits); putting it in the
> chat-layer JSON keeps the agent-layer link blob structurally
> unchanged, so non-channel uses (1-time invitations, normal contact
> addresses) are untouched. Older clients reading channels with
> `linkDataVersion = Just n` ignore the unknown JSON field; newer
> clients reading older blobs without the field default the version to
> 0 and reconcile from there.

#### 1.8 agentVRange bump

```haskell
-- Simplex.Messaging.Agent.Protocol
multiOwnerSMPAgentVersion :: VersionSMPA
multiOwnerSMPAgentVersion = VersionSMPA 8

currentSMPAgentVersion = multiOwnerSMPAgentVersion
supportedSMPAgentVRange = mkVersionRange minSupportedSMPAgentVersion currentSMPAgentVersion
```

Older clients (`< 8`) fail the chain-aware verification in
`decryptLinkData` for blobs signed by a chained owner; this is a
**hard incompatibility** for older clients reading channels that have
promoted any chained owner. Acceptable per design decision; flag in
release notes (section 5).

#### Tests to add

- `simplexmq/tests/AgentTests/ShortLinkTests.hs` — extend with cases:
  - sign with chained owner pk, verify with root pk → success.
  - sign with non-listed pk → reject.
  - chain depth > `ownerChainDepth` → reject.
  - duplicate ownerId → reject (existing).
  - duplicate ownerKey → reject (existing).
  - cycle attempt → existing prefix-only authorization makes this
    structurally impossible; add a regression test
    (`testChainCycleStructurallyImpossible`).
- `simplexmq/tests/AgentTests/FunctionalAPITests.hs` — co-owner LSET
  end-to-end:
  - A creates queue + uploads root-signed blob.
  - A RKEY-adds B's pubkey.
  - B (with co-owner ShortLinkCreds) LSETs an owner-signed blob.
  - C (subscriber) LGETs and verifies through B's chain.

#### Acceptance

- All ShortLinkTests pass.
- `cabal build && cabal test` from the simplexmq repo green.
- Wire round-trip test demonstrates byte-level identity for a root-
  signed blob (no breakage for v6.5 channels).

### Phase 2 — Chat layer foundation

Files to read first:
- `/workspace/src/Simplex/Chat/Library/Internal.hs:1313-1399`
- `/workspace/src/Simplex/Chat/Library/Commands.hs:2496-2527, 4042-4238`
- `/workspace/src/Simplex/Chat/Store/Groups.hs:1850-1920, 2990-3025`
- `/workspace/src/Simplex/Chat/Library/Subscriber.hs:2953-3070`
- `/workspace/src/Simplex/Chat/Protocol.hs:440-460, 980-1100, 1100-1320`
- `/workspace/src/Simplex/Chat/Store/SQLite/Migrations.hs` (registry)

#### 2.1 Schema migrations (chat-side)

Three migrations (Postgres mirrors). All additive; existing single-
owner channels need no data move.

The owner roster lives **on `group_members`**, not in a separate
`channel_owners` table. Members already carry a role; owners are simply
members with `member_role = 'owner'`. Owner-specific fields hang off
the same row. Co-owner credential storage is implicit: this device's
own `rcv_queues` row and the chat-side `groups.root_pub_key` together
encode the co-owner state — no new chat-side table needed.

```sql
-- M20260508_group_members_owner_fields.hs
-- (Implementer note: `M20260508` matches today's date. The current head
-- of the SQLite migrations registry is `M20260507_relay_inactive_at`;
-- verify the actual head at the time of work and bump if a later
-- migration has landed.)
ALTER TABLE group_members ADD COLUMN owner_auth_sig BLOB;
ALTER TABLE group_members ADD COLUMN owner_position INTEGER;
ALTER TABLE group_members ADD COLUMN owner_rcv_pub_key BLOB;
-- All three NULL when member_role <> 'owner'.
-- owner_auth_sig    = OwnerAuth.authOwnerSig (signature authorizing this
--                     member as an owner)
-- owner_position    = position in the prefix-ordered OwnerAuth list
-- owner_rcv_pub_key = the SMP recipient auth pubkey used by this owner's
--                     device on the channel link queue (gossiped over mesh
--                     on promotion; required for owner-removal RKEY)

-- Mutable-blob version cursor (LWW reconciliation reads/writes this).
ALTER TABLE groups ADD COLUMN link_data_version INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN link_data_remote_version INTEGER NOT NULL DEFAULT 0;
```

```sql
-- M20260509_owner_mesh.hs (or whatever the next available date is)
-- Owner-only direct mesh connections, scoped per channel.
-- Mirrors group_member_intros for semantics; reuses x.grp.mem.intro plumbing.
CREATE TABLE channel_owner_mesh (
  channel_owner_mesh_id  INTEGER PRIMARY KEY,
  group_id               INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  peer_group_member_id   INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  direct_conn_id         INTEGER REFERENCES connections ON DELETE SET NULL,
                                  -- the direct E2EE connection to this peer
  status                 TEXT NOT NULL,
                                  -- 'pending' | 'connected' | 'closed'
  created_at             TEXT NOT NULL,
  updated_at             TEXT NOT NULL,
  UNIQUE(group_id, peer_group_member_id)
) STRICT;
```

```sql
-- M20260510_promotion_in_progress.hs
-- Phase-3 promotion-in-progress journal, used by the orchestrator
-- (resume-from-step on app restart).
CREATE TABLE channel_promotion_in_progress (
  promotion_id          INTEGER PRIMARY KEY,
  group_id              INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  candidate_member_id   BLOB NOT NULL,
  candidate_pub_key     BLOB,                 -- B's owner public key (post-step-2)
  candidate_rcv_pub_key BLOB,                 -- B's recipient pub key (post-step-2)
  step                  TEXT NOT NULL,
                                  -- 'invitation_sent' | 'creds_received' |
                                  -- 'rkey_done' | 'lset_done' | 'bundle_sent' |
                                  -- 'mesh_introduced' | 'role_announced'
  direct_conn_id        INTEGER REFERENCES connections ON DELETE SET NULL,
  last_error            TEXT,
  created_at            TEXT NOT NULL,
  updated_at            TEXT NOT NULL,
  UNIQUE(group_id, candidate_member_id)
) STRICT;
```

Place migrations in
`/workspace/src/Simplex/Chat/Store/SQLite/Migrations/`, named by
today's date (the implementer must verify the actual current head and
bump if a later migration has landed). Mirror in
`/workspace/src/Simplex/Chat/Store/Postgres/Migrations/`. Register in
`Simplex/Chat/Store/SQLite/Migrations.hs` after the actual current
head.

#### 2.2 Owner-roster helpers (built on `group_members`)

No new Haskell record type for "channel owner" — the on-disk
representation is `group_members` with `member_role = 'owner'` plus
the three new owner_* columns. Helpers:

```haskell
-- Simplex.Chat.Store.Groups (new helpers, alongside existing group-member
-- accessors)

-- materialize the OwnerAuth list for an LSET; ordered by owner_position.
getChannelOwnerAuths
  :: DB.Connection
  -> GroupId
  -> IO [OwnerAuth]

-- shape returned by getChannelOwnerAuths, derived directly:
--   SELECT member_id, member_pub_key, owner_auth_sig
--   FROM group_members
--   WHERE group_id = ? AND member_role = 'owner'
--   ORDER BY owner_position
-- producing OwnerAuth { ownerId = unMemberId memberId
--                     , ownerKey = memberPubKey
--                     , authOwnerSig = ownerAuthSig }

-- apply a fresh roster received from the wire (LGET response or mesh sync):
-- for each entry set member_role = 'owner' and update
-- (owner_auth_sig, owner_position, owner_rcv_pub_key); demote any existing
-- owner not in the received list to its previous role (or 'member').
applyChannelOwnerRoster
  :: DB.Connection
  -> GroupId
  -> [(MemberId, C.Signature 'C.Ed25519, Maybe C.PublicKeyEd25519 {- rcvPubKey, when known -})]
  -> IO ()

-- promote a single member (used by the orchestrator at step 4).
markMemberAsOwner
  :: DB.Connection
  -> GroupId
  -> MemberId
  -> C.Signature 'C.Ed25519     -- authOwnerSig
  -> Int                         -- owner_position (append to end)
  -> Maybe C.PublicKeyEd25519    -- owner_rcv_pub_key (Just on this device for self;
                                 -- Nothing initially for peers, filled in via mesh sync)
  -> IO ()

-- demote on owner removal; clears the three owner_* columns.
demoteOwner
  :: DB.Connection
  -> GroupId
  -> MemberId
  -> IO ()

-- LWW version cursor accessors.
incrementLinkDataVersion :: DB.Connection -> GroupId -> IO Word64
setLinkDataRemoteVersion :: DB.Connection -> GroupId -> Word64 -> IO ()
getLinkDataVersions      :: DB.Connection -> GroupId -> IO (Word64, Word64)
                                          -- (local, remote)
```

`applyChannelOwnerRoster` is invoked whenever the chat layer ingests a
fresh roster from the wire (LGET decoded, or mesh sync); it overwrites
the in-DB owner state atomically (the link blob is the source of
truth).

Cascade-removal preview (D3) does not need a stored "who authorized
whom" column — it's reconstructable on demand from the prefix-ordered
list using the same loop `validateLinkOwners` runs internally:

```haskell
-- pure function, no IO; called when rendering the cascade preview
reconstructOwnerAuthorizers
  :: C.PublicKeyEd25519                                     -- rootKey
  -> [(MemberId, C.PublicKeyEd25519, C.Signature 'C.Ed25519)]
                                                            -- prefix-ordered owners
  -> [(MemberId, Maybe MemberId)]
                                                            -- (owner, authorizer ; Nothing = root)
reconstructOwnerAuthorizers rootKey owners =
  -- For each owner o in order, check whether sig(o.memberId || o.pubKey)
  -- verifies against rootKey; if yes -> (o.memberId, Nothing). Otherwise,
  -- find the earliest prior owner whose pubkey verifies the sig; emit
  -- (o.memberId, Just that earlier memberId).
  ...
```

#### 2.3 `groupLinkData` rewrite

Change `groupLinkData` in `Internal.hs:1355-1370` to source the owners
list from `group_members` via `getChannelOwnerAuths`, and to select the
signing key from `GroupRootKey`:

```haskell
groupLinkData
  :: DB.Connection
  -> GroupInfo -> GroupLink -> [GroupRelay]
  -> IO (UserConnLinkData 'CMContact, CRClientData, C.PrivateKeyEd25519)
                                  -- last component is the signing key the
                                  -- caller passes to the agent's setConnShortLink
groupLinkData db gInfo gLink groupRelays = do
  owners <- getChannelOwnerAuths db (groupId' gInfo)
  version <- incrementLinkDataVersion db (groupId' gInfo)
  let direct = not (useRelays' gInfo)
      relays = mapMaybe (\GroupRelay{relayLink} -> relayLink) groupRelays
      groupProfile = (groupProfile gInfo)
      userData = encodeShortLinkData $
        GroupShortLinkData { groupProfile
                           , publicGroupData = ...
                           , linkDataVersion = Just version }
      userLinkData = UserContactLinkData UserContactData
        { direct, owners, relays, userData }
      crClientData = encodeJSON $ CRDataGroup (groupLinkId gLink)
      signingKey = case groupKeys gInfo of
        Just GroupKeys { groupRootKey = GRKPrivate rootPriv } -> rootPriv
        Just GroupKeys { groupRootKey = GRKPublic _,   memberPrivKey } -> memberPrivKey
        Nothing -> error "groupLinkData: missing groupKeys"
  pure (userLinkData, crClientData, signingKey)
```

The upstream `OwnerAuth` is the 3-field shape
(`ownerId, ownerKey, authOwnerSig`). The chain is reconstructed purely
from signatures (`validateLinkOwners` walks the prefix). We do NOT
persist a "who authorized this owner" column; the cascade UI uses
`reconstructOwnerAuthorizers` on demand.

Callers of `groupLinkData`
(`setGroupLinkDataAsync`, `updatePublicGroupData` —
`Internal.hs:1316-1334`) become DB-aware. Caller convention: increment
`link_data_version` first (already done by `groupLinkData` via
`incrementLinkDataVersion`), then pass `signingKey` into the agent's
`setConnShortLink` family. If LSET fails, version is still bumped — the
local cache will correct on the next LGET (still LWW-safe).

#### 2.4 Owner mesh data model + transport

The owner mesh is a fully-connected sub-graph among channel owners,
**channel-scoped** (per-channel `MemberId` identity) so removing X
from owners closes their mesh edges without touching unrelated
personal contacts between the same humans.

We **reuse**, not duplicate, the existing `x.grp.mem.intro` /
`x.grp.mem.inv` / `x.grp.mem.fwd` flow at
`Subscriber.hs:2953-3070`. Two additions:

1. A new chat message scoped to owners only:

```haskell
-- Simplex.Chat.Protocol  (new constructor + tag + JSON)
XGrpOwnerCreds :: CoOwnerCredsBundleEnvelope -> ChatMsgEvent 'Json
XGrpOwnerCreds_ :: CMEventTag 'Json
"x.grp.owner.creds"

data CoOwnerCredsBundleEnvelope = CoOwnerCredsBundleEnvelope
  { groupId    :: B64UrlByteString    -- channel publicGroupId
  , bundle     :: B64UrlByteString    -- smpEncoded CoOwnerCredsBundle
  } deriving (Eq, Show)
```

`XGrpOwnerCreds` is **only** sent over the direct mesh connection
between A and B (never relay-forwarded). Verifier on B's side: confirm
the `groupId` matches the channel B was invited to; reject otherwise.

2. The link-sync message in 4.1 (`x.grp.link.sync`).

The mesh's `direct_conn_id` is a regular `Connection` row that points
at a `ContactConnection` agent connection (no group queue, no relay).
Existing primitives like `sendDirectMemberMessage`
(`Subscriber.hs:2994`) carry the payload; we reuse them with a small
helper:

```haskell
-- Simplex.Chat.Library.Internal  (new)
sendOwnerMeshMessage
  :: User -> GroupInfo -> [ChannelOwnerMesh]
  -> ChatMsgEvent 'Json
  -> CM ()
sendOwnerMeshMessage user gInfo mesh msg =
  forM_ mesh $ \ChannelOwnerMesh{directConnId = Just cid} -> do
    conn <- withStore $ \db -> getConnectionById db vr user cid
    void $ sendDirectMemberMessage conn msg (groupId' gInfo)
```

Skipped peers (status ≠ `'connected'`) silently fall back to the
LGET-on-startup safety net (4.2).

##### 2.4.1 Owner-mesh content scope

> **Owner-mesh content scope, this delivery.** The mesh accepts only the
> following events; any other event arriving over a mesh connection is
> logged and rejected (visible during testing, not silently dropped):
>
> - `x.grp.owner.creds` — co-owner credential bundle handover.
> - `x.grp.link.sync` — link-data sync (4.1).
> - `x.grp.mem.role` — owner-role-change broadcast among owners.
> - `x.grp.mem.intro` / `x.grp.mem.inv` / `x.grp.mem.fwd` — standard
>   member introduction flow used to bootstrap mesh edges.
>
> No `x.msg.new` or any other content message is accepted on the mesh in
> this delivery.
>
> **TODO (post-MVP).** Enabling mesh content would unlock: a private
> "channel owners" chat thread for governance discussion, file sharing
> between owners, draft-content review before publishing, and out-of-band
> coordination invisible to subscribers and relays. The data plumbing is
> deferred until the UI ships — there is no benefit to persisting messages
> that no UI can display.

D6 (UI groundwork): keep the `// TODO surface owner-chat UI` markers at
the natural insertion points in `GroupChatInfoView.swift`,
`GroupChatInfoView.kt`, and the new Owners views. No persistence of
owner-mesh content in this delivery.

#### Tests

- `tests/ChatTests/ChannelTests.hs` — extend with:
  - LWW: two simulated owners, concurrent edits, only the higher
    `linkDataVersion` survives on the third subscriber's view.
  - Roster round-trip: owners persisted to `group_members` round-trip
    via `getChannelOwnerAuths → groupLinkData → encrypt → decrypt →
    validateLinkOwners → applyChannelOwnerRoster`.
  - Mesh-event allowlist: `x.msg.new` received over a mesh connection
    is logged and rejected.

#### Acceptance

- Single-owner channels create and operate identically to today (the
  creator's row in `group_members` carries the `owner_*` fields populated
  lazily on first multi-owner action — see 4.4).
- Schema migrations are idempotent and reversible.
- Mesh connection records exist after a simulated promotion (phase 3).

### Phase 3 — Promote-to-owner orchestrator

#### 3.1 Top-level handshake (existing-owner A, candidate B)

A is an owner; B is an existing member/subscriber of the channel. A
selects B in the UI (D2). The orchestrator owns the
`channel_promotion_in_progress` row from start to finish. Each step is
idempotent and detected before re-execution.

```
Step 1  invitation_sent   A creates a fresh, channel-scoped direct
                          ContactConnection, encodes the invitation
                          link as XGrpOwnerInvite, sends it via the
                          regular A↔B direct connection (their
                          existing personal contact, OR a one-time
                          link if no contact). The invitation is the
                          *new direct mesh link* — not the channel
                          link.
Step 2  creds_received    B accepts; B generates rcvKeyPair locally
                          (queue-specific; via C.generateAuthKeyPair)
                          and reuses memberPrivKey as ownerPrivKey.
                          B sends back rcvPubKey + ownerPubKey via
                          XGrpOwnerAccept on the new direct mesh
                          connection. A records both pubkeys.
Step 3  rkey_done         A reads current recipientKeys (from a local
                          cache populated by setQueueRecipientKeys
                          and by mesh sync; fallback recompute from
                          group_members.owner_rcv_pub_key over all
                          current owners), appends rcvPubKey,
                          calls setQueueRecipientKeys.
Step 4  lset_done         A constructs OwnerAuth_B = (B.memberId,
                          ownerPubKey, sign(A.ownerPrivKey,
                          memberId || ownerPubKey)), promotes B in
                          group_members via markMemberAsOwner,
                          increments link_data_version, rebuilds blob,
                          LSET.
Step 5  bundle_sent       A sends XGrpOwnerCreds(bundle) over the new
                          direct mesh connection.
Step 6  mesh_introduced   For each existing owner X (other than A and
                          B), A initiates the standard
                          x.grp.mem.intro / x.grp.mem.inv flow,
                          parameterized to mark the resulting
                          connection as `channel_owner_mesh.status =
                          'pending' → 'connected'`.
Step 7  role_announced    A signs and broadcasts an x.grp.mem.role
                          message via the channel's relays
                          (subscribers learn that B is now an owner).
                          Also broadcast x.grp.link.sync over the
                          owner mesh so peers update their cached
                          link blob and roster (and learn B's
                          owner_rcv_pub_key for future RKEY ops).
```

#### 3.2 Idempotency

Each step has a "did this already happen?" check before execution:

- Step 3: read `recipientKeys` cached locally (we begin caching this
  on the existing-owner's device starting at phase 2; loaded by the
  agent on startup via a new `getQueueRecipientKeys` helper that
  parses the most-recent server LGET). If `rcvPubKey` is already in
  the set, skip.
- Step 4: read current owners list from server via LGET, check whether
  a row for `B.memberId` with `member_role = 'owner'` and matching
  `member_pub_key == ownerPubKey` is present; if yes, skip.
- Step 5: track via `channel_promotion_in_progress.step`; the bundle
  is delivered exactly-once via the underlying agent's
  `sendDirectMemberMessage` (already at-least-once + receiver-side
  dedup by message ID).
- Step 6: per-peer state in `channel_owner_mesh`; skip peers already
  in `'connected'`.
- Step 7: chat-protocol `x.grp.mem.role` is already idempotent (latest
  role wins).

#### 3.3 Failure modes

| Failure | State left | Recovery |
|---|---|---|
| Step 3 succeeds, Step 4 fails | Unused recipient key in queue's set | On retry, Step 3 detects key already present, Step 4 retries cleanly. Stale `rcvPubKey` is harmless until Step 4 either succeeds or the candidate gives up — in that case an explicit cleanup RKEY removes it. |
| Step 4 succeeds, Step 5 fails | B is an authorized owner on-server but lacks credentials | Retry Step 5 from the journal. If A's device is destroyed first, B can detect via the next LGET on the channel (their owner row exists in the blob, but they have no credentials to use it) — we surface "owner credentials missing — request resend" in the UI. |
| Step 5 succeeds, Step 6 partial | Owner mesh missing some edges | Retry per-peer at next app start. |
| Step 6 succeeds, Step 7 fails | Subscribers don't see the new owner's role | Retry Step 7. New owner may temporarily appear as a non-owner member in subscribers' views; harmless — they cannot post anyway until the role updates. |

#### 3.4 Orchestrator entry point

```haskell
-- Simplex.Chat.Library.Owners (new module)
promoteToOwner
  :: User -> GroupInfo -> GroupMember -> CM ()
promoteToOwner user gInfo memberB = do
  -- finds or creates a channel_promotion_in_progress row;
  -- runs the next pending step; persists; returns.
  ...

resumePromotions :: CM ()
resumePromotions = do
  -- called on app start; iterates rows in channel_promotion_in_progress,
  -- runs `promoteToOwner` to advance each.
  ...
```

Wire `resumePromotions` into the chat startup sequence in
`Simplex/Chat/Core.hs` next to other recovery paths.

#### Tests

- `tests/ChatTests/ChannelTests.hs::testPromoteOwner`: full happy path.
- `tests/ChatTests/ChannelTests.hs::testPromoteOwnerResumeStep4`:
  simulate process death between steps 3 and 4; restart;
  `resumePromotions` completes cleanly.
- `tests/ChatTests/ChannelTests.hs::testPromoteIdempotentRetry`: rerun
  the orchestrator end-to-end on an already-completed promotion;
  no-op outcome.

#### Acceptance

- B can edit the channel link blob (LSET) after Step 4.
- All existing owners and B share a fully-connected owner mesh after
  Step 6.
- Subscribers see B with `memberRole = .owner` after Step 7.

### Phase 4 — Cross-owner sync, conflict resolution, owner removal

#### 4.1 `x.grp.link.sync`

```haskell
XGrpLinkSync :: GrpLinkSync -> ChatMsgEvent 'Json
"x.grp.link.sync"

data GrpLinkSync = GrpLinkSync
  { groupId        :: B64UrlByteString
  , linkDataVersion :: Word64
  , encMutableData :: B64UrlByteString  -- raw md_bytes (sig64 || smpEncode...)
  } deriving (Eq, Show)
```

Sender: any owner that just completed an LSET. Receivers (owners only,
mesh):
1. Verify the `groupId` matches the channel.
2. Decode `md_bytes`, run `validateLinkOwners` against the rootKey
   extracted from the channel's stored fixed-data (or, equivalently,
   `groupRootPubKey (groupRootKey groupKeys)`).
3. If `linkDataVersion > local`, replace the local cached blob and
   roster (`applyChannelOwnerRoster`); set
   `link_data_remote_version := linkDataVersion`.
4. If `linkDataVersion ≤ local`, ignore (already seen or this owner
   has a newer pending edit; D4 banner will handle UX).

The tag `x.grp.link.sync` is reserved for this delivery; the parallel
"link-data passing" workstream will use a different name.

#### 4.2 LWW reconciliation

State on each owner's device:
- `groups.link_data_version` — the highest version this device has
  *written* (the optimistic local copy).
- `groups.link_data_remote_version` — the highest version this device
  has *seen* on the server (or via mesh sync).

On every roster mutation:
1. Read current `(local, remote) = getLinkDataVersions`.
2. LGET the server's blob; parse its version `serverV`.
3. If `serverV > remote`:
   - Replace local roster with server's (this owner's edits, if any,
     are not yet persisted to local — we kept them in memory until
     LSET succeeded).
   - Re-apply pending mutations on top of fresh state.
   - `remote := serverV`.
4. `newV := max(local, serverV) + 1`.
5. Encode + LSET. If LSET succeeds, `local := newV; remote := newV`.
6. Broadcast `XGrpLinkSync(newV)` over mesh.
7. Bounded retry (max 3) if a concurrent owner beats us in step 5
   (we'd see `serverV ≥ newV` on the next LGET) — go back to step 2.

For the simpler, non-roster mutations (e.g., relay set updates),
optimistic-without-LGET is acceptable — collisions are rare and
self-heal next sync.

#### 4.3 Owner-removal flow

Existing owner R removes owner X. R must verify *before* attempting:
- X.authOwnerSig chain does not pass through R (otherwise removing X
  cascades through R, invalidating R themselves).
- If X is a root-signed owner whose chain authorizes R, removal is
  blocked entirely (D3 will explain).

```haskell
-- Simplex.Chat.Library.Owners
data OwnerRemoval = OwnerRemoval
  { directlyRemoved :: MemberId
  , cascadeRemoved  :: [MemberId]   -- transitively invalidated
  }

planOwnerRemoval
  :: C.PublicKeyEd25519                                       -- rootKey
  -> [(MemberId, C.PublicKeyEd25519, C.Signature 'C.Ed25519)] -- prefix-ordered owners
  -> MemberId                                                 -- remover
  -> MemberId                                                 -- removee
  -> Either String OwnerRemoval
-- returns Left when the remover's chain depends on the removee.
```

`planOwnerRemoval` performs a forward pass over the prefix-ordered
list using the same logic as `reconstructOwnerAuthorizers` (2.2): any
owner whose authorizer is in the removed-set is itself removed.

Removal steps:

1. Compute `OwnerRemoval`. If any cascade member is the remover, fail.
2. Build new owners list (= old list minus directlyRemoved minus
   cascadeRemoved). Bump version. LSET.
3. Compute new `recipientKeys` set, executable directly against
   `group_members`:

   ```sql
   SELECT owner_rcv_pub_key
   FROM group_members
   WHERE group_id = ?
     AND member_role = 'owner'
     AND member_id NOT IN (?, ?, ...)  -- removed and cascade-removed
   ```

   Then call `setQueueRecipientKeys` with the resulting list.
4. Broadcast `x.grp.mem.role` (member or relay) for each removed
   owner via relays.
5. Broadcast `x.grp.link.sync` over mesh.
6. Close `channel_owner_mesh` rows where the peer was removed; delete
   their direct connections via `deleteAgentConnectionsAsync'`.

If the remover IS being demoted (self-removal): same flow, except
they stop seeding their own mesh edges and clear their own row's
`owner_*` columns last (via `demoteOwner`).

##### 4.3.1 Chain-cascade semantics, recursive validator

`validateLinkOwners` (in agent layer) already rejects an owners list
whose chain doesn't terminate in rootKey. After a removal, if the
remover forgets to prune cascade-invalidated entries, the next
LGET-on-startup by a subscriber would simply drop the chained owners
on verification — they'd be invisible. This is correctness but messy.
**Plan: prune the cascade on the LSET writer's side.** Rationale:
keeps the wire blob clean, makes mesh sync deterministic, allows the
UI's roster view to match what's on the server byte-for-byte.

##### 4.3.2 Removing a root-signed owner

Constraints:
- Root-signed owners are those whose `authOwnerSig` verifies directly
  against `rootKey` (i.e., `reconstructOwnerAuthorizers` returns
  `Nothing`).
- A root-signed owner can only be removed by another owner whose chain
  does NOT pass through them.
- Single-owner channels: the sole root-signed owner cannot be removed.
- Multi-owner channels with one root-signed owner who authorized all
  chained owners: chained owners cannot remove the root-signed owner
  without cascade-invalidating themselves.

Surface in D3 (UI): when remover's chain depends on removee, the
remove action is disabled with the explanation
`"You cannot remove the channel creator — your owner role was
authorized by them."`.

##### 4.3.3 Out of scope: transferring root creator

Pre-signing additional `OwnerAuth` entries with `authOwnerId = null`
at channel creation is a future feature (channels-overview.md notes
this preserves creator anonymity). Not part of this delivery.

#### 4.4 Single-owner → multi-owner upgrade

Verified: existing channels at `Internal.hs:1361-1366` build exactly
one root-signed `OwnerAuth` whose `ownerKey = C.publicKey memberPrivKey`
(same key reused as member signing key). Phase 2's
`applyChannelOwnerRoster` lazy-migrates this on the first multi-owner
action: when A's device first calls `groupLinkData`, if no row in
`group_members` has the `owner_*` columns populated AND
`groups.root_pub_key`/`member_priv_key` are populated, we synthesize
the creator's owner row in-place by setting
`(owner_auth_sig, owner_position, owner_rcv_pub_key)` from the
device's local state.

No on-disk migration of channel data is required.

After upgrade:
- `groups.root_pub_key` (private side: `groups.member_priv_key` is
  reused as the owner private key) stays on A's device.
- A's `member_priv_key` IS A's owner private key. The "member-key =
  owner-key" invariant holds and must continue to hold.
- The OwnerAuth chain for any owner B added later reveals
  "creator authorized newcomer". Acceptable per the design decision
  in channels-overview.md objective #6 (a future feature could
  pre-sign N≥2 root owners at creation to preserve creator
  anonymity — out of scope here).

> **Invariant: `OwnerAuth.ownerKey == group_members.member_pub_key`.**
>
> Member signing keys are fixed for the channel's lifetime. Independent
> rotation of `member_pub_key` while the member is an owner is not
> supported in this delivery — it would invalidate the stored
> `OwnerAuth.ownerKey` in the link blob. Add an invariant test
> (`testMemberKeyEqualsOwnerKey`) that runs on every channel-creation
> and promotion. If a future feature introduces member-key rotation,
> the owner roster must be rewritten in the same transaction.

#### 4.5 Concurrent RKEY recovery

> **Concurrent RKEY race — eventual consistency.**
>
> `RKEY` replaces the entire `recipientKeys` set on the SMP server in one
> transaction. If owners A and B concurrently RKEY (e.g., each promoting
> a different candidate), the second write clobbers the first; the
> loser's candidate has no SMP write access until reconciliation.
>
> Recovery loop:
> 1. The dropped candidate's first `LSET` returns `ERR AUTH`.
> 2. Their device emits an event to the chat layer that bubbles to other
>    owners over the mesh (re-broadcast `x.grp.link.sync` with their
>    current `owner_rcv_pub_key`).
> 3. Any current owner reconstructs the desired `recipientKeys` set from
>    `group_members` (filter `member_role = 'owner'`, take
>    `owner_rcv_pub_key` for each), and calls `setQueueRecipientKeys`
>    with the merged list.
> 4. Convergence within one mesh round-trip.
>
> Acceptable per the full-trust model. No write-fence, no coordination
> protocol. Test: `testConcurrentRKEYConvergence`.

#### Tests

- `testLWWConflictDetection` — A and B race-edit the profile; final
  state is whichever wrote last; the loser detects via version mismatch
  on next LGET.
- `testCascadeRemoval` — root signs A and B; A signs C and D; B
  removes A; C and D cascade-removed.
- `testRemoverCascadeBlocked` — D tries to remove the root-signed
  owner whose chain authorized D; rejected.
- `testRootCannotBeRemovedSoloOwner`.
- `testUpgradeSingleOwnerLazy` — synthesize creator's owner row
  fields from groups.root_pub_key/member_priv_key on first read.
- `testConcurrentRKEYConvergence` — A and B race-RKEY; recovery loop
  converges within one mesh round-trip.
- `testMemberKeyEqualsOwnerKey` — invariant on creation and promotion.

#### Acceptance

- Three-owner channel: any owner can edit profile; all three converge.
- Cascade UX correctly previews removed set in the orchestrator.
- Existing v6.5 channels open without migration; first multi-owner
  action lazy-populates the creator's owner row.

### Phase 5a — iOS UI

Files to edit:
- `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift` — add the
  Owners section to the channel-settings list (between the link
  section and the preferences section, lines 110-180).
- `apps/ios/Shared/Views/Chat/Group/ChannelMembersView.swift` — adapt
  the existing "Owners" section header to show authorizer info per
  chained owner.
- New file: `apps/ios/Shared/Views/Chat/Group/Owners/ChannelOwnersView.swift`
  — owners management screen, owner-only.
- New file: `apps/ios/Shared/Views/Chat/Group/Owners/AddChannelOwnerView.swift`
  — picker over existing channel members + invitation generation.
- New file: `apps/ios/Shared/Views/Chat/Group/Owners/RemoveChannelOwnerSheet.swift`
  — confirmation with cascade preview.
- `apps/ios/Shared/Model/SimpleXAPI.swift` (or wherever group/channel
  API calls live) — add bindings for `apiPromoteToOwner`,
  `apiRemoveOwner`, `apiPlanRemoveOwner`, `apiCancelPromotion`,
  `apiChannelOwners`.
- Strings: append to `apps/ios/en.lproj/Localizable.strings` and
  `SimpleX Localizations/*/.../Localizable.strings`.

Define a small chat-side type returned by `apiChannelOwners` — a UI-
shaped record mirroring the columns we read from `group_members`:

```swift
struct ChannelOwner: Codable, Identifiable {
    let memberId: MemberId
    let displayName: String
    let ownerPosition: Int
    let authorizedByMemberId: MemberId?    // Nothing => root
    var id: MemberId { memberId }
}
```

(`authorizedByMemberId` is computed by the chat-side RPC using
`reconstructOwnerAuthorizers`, not stored on disk.)

#### D1 — Owners section in channel settings

Add a new `Section { ... } header: "Owners"` in
`GroupChatInfoView.swift` after the channel-link section (after line
141 in the `if groupInfo.useRelays` branch). Visible to all members;
read-only for non-owners.

```swift
// GroupChatInfoView.swift, inside the useRelays branch
Section(header: Text("Owners").foregroundColor(theme.colors.secondary)) {
    NavigationLink {
        ChannelOwnersView(chat: chat, groupInfo: groupInfo)
    } label: {
        let count = chatModel.channelOwners(forGroupId: groupInfo.groupId).count
        HStack {
            Image(systemName: "person.2.crop.square.stack")
                .frame(width: 24, height: 24)
            Text(String.localizedStringWithFormat(
                NSLocalizedString("Owners (%lld)", comment: "channel owners section"),
                count
            ))
            Spacer()
            Image(systemName: "chevron.right").font(.footnote).foregroundColor(theme.colors.secondary)
        }
    }
}
```

`ChannelOwnersView` (new):

```swift
struct ChannelOwnersView: View {
    @ObservedObject var chat: Chat
    let groupInfo: GroupInfo
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        let owners = chatModel.channelOwners(forGroupId: groupInfo.groupId)
        List {
            Section {
                ForEach(owners, id: \.memberId) { o in
                    NavigationLink {
                        if let m = chatModel.member(by: o.memberId, groupId: groupInfo.groupId) {
                            GroupMemberInfoView(
                                groupInfo: groupInfo, chat: chat,
                                groupMember: GMember(m),
                                scrollToItemId: Binding.constant(nil)
                            )
                        }
                    } label: {
                        OwnerRow(owner: o, owners: owners)
                    }
                }
            } header: {
                Text("Owners")
            } footer: {
                if groupInfo.isOwner {
                    Text("Any owner can change channel info, manage relays, and add or remove other owners.")
                        .foregroundColor(theme.colors.secondary)
                }
            }
            if groupInfo.isOwner {
                Section {
                    NavigationLink {
                        AddChannelOwnerView(groupInfo: groupInfo, chat: chat)
                    } label: {
                        Label("Add owner", systemImage: "person.badge.plus")
                    }
                }
            }
        }
        .navigationTitle("Channel owners")
    }
}
```

`OwnerRow` shows display name + small "creator" tag when
`authorizedByMemberId == nil`, otherwise "Authorized by <Alice>" as a
caption. Layout per `layout-swift.md`: System Image needs no
`.resizable()` (SF Symbols size correctly); avatar uses the existing
`MemberProfileImage` pattern with `.frame(width: 38, height: 38)`.

#### D2 — Add owner flow

`AddChannelOwnerView`:

1. List of channel members eligible for promotion (filter out
   relays, the user themselves, and existing owners). Show member
   row similar to `ChannelMembersView.memberRow`. On tap, present
   confirmation: "Promote Bob to owner?".
2. On confirm, call `apiPromoteToOwner(groupId, memberId)` which
   creates the `channel_promotion_in_progress` row and starts step 1.
3. Show pending state (spinner with "Sending invitation to Bob…");
   the chat layer streams `CEvtPromotionStep` events that update the
   UI through the chat model.
4. On full completion (`step = 'role_announced'`): toast
   `"Bob is now a channel owner"` and dismiss.
5. On failure: error sheet with retry / cancel.

Out of scope for MVP (note in code, not UI): inviting a non-member
directly to become an owner. Today's flow is "promote existing
member".

#### D3 — Remove owner flow

`RemoveChannelOwnerSheet`:

```swift
struct RemoveChannelOwnerSheet: View {
    let groupInfo: GroupInfo
    let owner: ChannelOwner
    @State private var cascadePreview: [ChannelOwner] = []
    @State private var blocked: String? = nil

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Remove \(displayName(owner))?")
                .font(.title3)
            if let blocked {
                Label(blocked, systemImage: "exclamationmark.triangle")
                    .foregroundColor(theme.colors.secondary)
                Button("OK") { dismiss() }
            } else {
                if !cascadePreview.isEmpty {
                    Text("Removing \(displayName(owner)) will also remove:")
                    ForEach(cascadePreview, id: \.memberId) { o in
                        Text("• \(displayName(o)) (authorized by \(displayName(owner)))")
                    }
                }
                HStack {
                    Button("Cancel") { dismiss() }
                    Spacer()
                    Button("Remove", role: .destructive) {
                        Task { await apiRemoveOwner(groupInfo.groupId, owner.memberId) ; dismiss() }
                    }
                }
            }
        }
        .padding()
        .task {
            let plan = await apiPlanRemoveOwner(groupInfo.groupId, owner.memberId)
            switch plan {
            case .ok(let cascade):  cascadePreview = cascade
            case .blocked(let msg): blocked = msg
            }
        }
    }
}
```

`apiPlanRemoveOwner` is a chat-side preview RPC that runs
`planOwnerRemoval` (4.3) without committing.

#### D4 — Sync indicator

When the chat model receives `CEvtChannelLinkRemoteUpdate(by:
otherOwner)` while the local user has uncommitted edits in flight,
overlay a non-blocking banner:

```swift
// In GroupChatInfoView.swift — overlay above the form
if let pending = chatModel.pendingLinkUpdate(forGroupId: groupInfo.groupId),
   pending.remoteWriterMemberId != nil {
    HStack {
        Image(systemName: "arrow.triangle.2.circlepath")
        Text("Channel info updated by \(displayName(pending.remoteWriter)) — your edits will be reapplied.")
            .font(.callout)
        Spacer()
    }
    .padding(.horizontal)
    .background(Color.yellow.opacity(0.18))
}
```

The banner clears when the LWW retry loop succeeds (4.2 step 7) or
the user dismisses.

#### D5 — Single-owner upgrade prompt

None proactive. The "Add owner" entry is silently visible in
`ChannelOwnersView` for single-owner channels. (Owner role is
required to see the section's edit affordances; pre-existing single
owners always satisfy that.)

#### D6 — Owner-only chat UI groundwork

Add `// TODO: surface owner-chat UI` in:
- `GroupChatInfoView.swift`, immediately after the new Owners section
  and before the preferences section.
- `ChannelOwnersView.swift`, near the bottom — would become a
  "Owners chat" nav link.

No nav link, no view, no entry in the chat list this delivery. No
mesh content is persisted.

#### Strings (en.lproj/Localizable.strings)

```
"Owners" = "Owners";
"Owners (%lld)" = "Owners (%lld)";
"Channel owners" = "Channel owners";
"Add owner" = "Add owner";
"Remove" = "Remove";
"Promote %@ to owner?" = "Promote %@ to owner?";
"Sending invitation to %@…" = "Sending invitation to %@…";
"%@ is now a channel owner" = "%@ is now a channel owner";
"Removing %@ will also remove:" = "Removing %@ will also remove:";
"Authorized by %@" = "Authorized by %@";
"creator" = "creator";
"You cannot remove the channel creator — your owner role was authorized by them." =
  "You cannot remove the channel creator — your owner role was authorized by them.";
"Channel info updated by %@ — your edits will be reapplied." =
  "Channel info updated by %@ — your edits will be reapplied.";
"Any owner can change channel info, manage relays, and add or remove other owners." =
  "Any owner can change channel info, manage relays, and add or remove other owners.";
```

Replicate (without translation; English text) into each
`SimpleX Localizations/*/Source Contents/en.lproj/Localizable.strings`
to keep build parity. Translations land in a follow-up.

#### Image checklist

All new screens use SF Symbols; no asset additions required. Avatars
reuse `MemberProfileImage` (already `.resizable().scaledToFit()` with
`.frame(width: 38, height: 38)`).

### Phase 5b — Kotlin multiplatform UI

Files to edit:
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt`
  (insert Owners section in the `useRelays` branch around line 615).
- New file: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/owners/ChannelOwnersView.kt`.
- New file: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/owners/AddChannelOwnerView.kt`.
- New file: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/owners/RemoveChannelOwnerSheet.kt`.
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimplexAPI.kt`
  — add `apiPromoteToOwner`, `apiRemoveOwner`, `apiPlanRemoveOwner`,
  `apiChannelOwners`.
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`
  — strings.

Define the analogous Kotlin UI-shaped type:

```kotlin
@Serializable
data class ChannelOwner(
  val memberId: MemberId,
  val displayName: String,
  val ownerPosition: Int,
  val authorizedByMemberId: MemberId? = null,   // null => root
)
```

#### D1 — Owners section

```kotlin
// GroupChatInfoView.kt, inside the useRelays branch (after the
// ChannelLinkButton/ChannelMembersButton section, around line 627)
SectionDividerSpaced(maxBottomPadding = false)
SectionView(stringResource(MR.strings.channel_owners_section)) {
  ChannelOwnersButton(chat.remoteHostId, groupInfo) {
    ModalManager.end.showModalCloseable { close ->
      ChannelOwnersView(chat = chat, groupInfo = groupInfo, close = close)
    }
  }
}
```

`ChannelOwnersView` (new):

```kotlin
@Composable
fun ChannelOwnersView(chat: Chat, groupInfo: GroupInfo, close: () -> Unit) {
  val owners = remember { mutableStateOf<List<ChannelOwner>>(emptyList()) }
  LaunchedEffect(groupInfo.groupId) {
    owners.value = controller.apiChannelOwners(chat.remoteHostId, groupInfo.groupId)
  }
  ColumnWithScrollBar(modifier = Modifier.fillMaxSize()) {
    AppBarTitle(stringResource(MR.strings.channel_owners_title))
    SectionView {
      owners.value.forEach { o ->
        OwnerRow(owner = o, owners = owners.value, onClick = {
          // navigate to GroupMemberInfoView
        })
      }
    }
    if (groupInfo.isOwner) {
      SectionDividerSpaced(maxBottomPadding = false)
      SectionView {
        SectionItemView(click = {
          ModalManager.end.showModalCloseable { close ->
            AddChannelOwnerView(chat = chat, groupInfo = groupInfo, close = close)
          }
        }) {
          Icon(painterResource(MR.images.ic_person_add), null,
               Modifier.size(24.dp), tint = MaterialTheme.colors.primary)
          Spacer(Modifier.width(DEFAULT_PADDING))
          Text(stringResource(MR.strings.channel_owners_add))
        }
      }
    }
    SectionTextFooter(
      stringResource(MR.strings.channel_owners_footer)
    )
  }
}
```

Layout per `layout-compose.md`:
- `Column` defaults to `Alignment.Start` — fine, owner rows are
  left-aligned.
- `Image` for SF-equivalents → use `MR.images.ic_person_add` icon
  with `Modifier.size(24.dp)`; no need for `ContentScale.Fit`
  (icons render correctly).
- `ColumnWithScrollBar` already wraps `LazyColumn` semantics in this
  codebase — see `ScrollableColumn.android.kt`.

`OwnerRow` design:

```kotlin
@Composable
fun OwnerRow(owner: ChannelOwner, owners: List<ChannelOwner>, onClick: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .clickable(onClick = onClick)
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    verticalAlignment = Alignment.CenterVertically
  ) {
    MemberProfileImage(owner.memberId, size = 38.dp)
    Spacer(Modifier.width(DEFAULT_PADDING_HALF))
    Column(Modifier.weight(1f)) {
      Text(owner.displayName, maxLines = 1, fontSize = 16.sp)
      val caption = if (owner.authorizedByMemberId == null) {
        stringResource(MR.strings.channel_owners_creator_tag)
      } else {
        val authorizer = owners.firstOrNull { it.memberId == owner.authorizedByMemberId }
        stringResource(MR.strings.channel_owners_authorized_by, authorizer?.displayName ?: "—")
      }
      Text(caption, fontSize = 13.sp, color = MaterialTheme.colors.onBackground.copy(alpha = 0.6f))
    }
  }
}
```

#### D2/D3/D4 — analogous screens

Mirror iOS structure. `RemoveChannelOwnerSheet` shows cascade preview
in a Compose `AlertDialog` with custom content; `AddChannelOwnerView`
opens member picker; D4 banner attaches above the form in
`GroupChatInfoView.kt` (use a fixed-height composable `Surface` with
warning color).

#### D5 / D6

Same as iOS: silent availability, `// TODO: surface owner-chat UI`
markers. No mesh content persistence.

#### Strings additions in
`apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

```xml
<string name="channel_owners_section">Owners</string>
<string name="channel_owners_title">Channel owners</string>
<string name="channel_owners_button">Owners (%1$d)</string>
<string name="channel_owners_add">Add owner</string>
<string name="channel_owners_remove">Remove</string>
<string name="channel_owners_promote_confirm">Promote %1$s to owner?</string>
<string name="channel_owners_sending_invitation">Sending invitation to %1$s…</string>
<string name="channel_owners_added_toast">%1$s is now a channel owner</string>
<string name="channel_owners_remove_cascade_intro">Removing %1$s will also remove:</string>
<string name="channel_owners_authorized_by">Authorized by %1$s</string>
<string name="channel_owners_creator_tag">creator</string>
<string name="channel_owners_blocked_creator">You cannot remove the channel creator — your owner role was authorized by them.</string>
<string name="channel_owners_remote_update_banner">Channel info updated by %1$s — your edits will be reapplied.</string>
<string name="channel_owners_footer">Any owner can change channel info, manage relays, and add or remove other owners.</string>
```

### Phase 6 — Tests + threat-model regressions

#### Haskell tests

- `tests/ChatTests/ChannelTests.hs` — phases 2-4 add new test groups
  (sections referenced in each phase).
- `simplexmq/tests/AgentTests/ShortLinkTests.hs` — phase 1 cases.
- `simplexmq/tests/AgentTests/FunctionalAPITests.hs` — co-owner LSET
  end-to-end.
- New file: `tests/ChatTests/MultiOwnerTests.hs` — orchestrator
  resume tests (3.2 idempotency cases) and concurrent-RKEY recovery.

Test names called out elsewhere in this plan:
`testLWWConflictDetection`, `testCascadeRemoval`,
`testRemoverCascadeBlocked`, `testRootCannotBeRemovedSoloOwner`,
`testUpgradeSingleOwnerLazy`, `testConcurrentRKEYConvergence`,
`testMemberKeyEqualsOwnerKey`, `testChainCycleStructurallyImpossible`,
`testChainTooLong`, `testCoOwnerCannotDeleteQueue`,
`testStaleBlobIgnored`, `testBundleCrossChannelReject`,
`testBundleReplayRejected`,
`testOldClientRejectsChainedOwnerBlob`,
`testOldClientReadsRootSignedBlob`, `testRosterRaceRetry`,
`testRelayCannotForgeOwners`, `testCascadeUiPreviewShowsAll`,
`testPromoteOwner`, `testPromoteOwnerResumeStep<N>`,
`testPromoteIdempotentRetry`.

#### Threat-model regression suite

Encode each "cannot" from channels-overview.md threat model as a
property test:

| Property | Test |
|---|---|
| Subscriber cannot impersonate owner (signed admin msg rejected) | existing `testSignatureRequired` extended with chained-owner cases |
| Compromised relay cannot manipulate roster | new `testRelayCannotForgeOwners` |
| Co-owner B cannot remove root-signed creator A whose chain authorized B | `testRemoverCascadeBlocked` |
| Co-owner credentials bundle cannot be replayed across channels | `testBundleCrossChannelReject` |
| Member-key = owner-key invariant | `testMemberKeyEqualsOwnerKey` |

#### UI smoke tests

- iOS: snapshot tests for `ChannelOwnersView` (single-owner +
  3-owner) and `RemoveChannelOwnerSheet` (with and without cascade).
- Compose: screenshot tests via the same harness used for other
  group screens.

---

## 4. Schema migrations (sketch)

### Chat-side (SQLite)

```sql
-- M20260508_group_members_owner_fields.hs   (date matches today; bump to
-- the actual current registry head at implementation time)
ALTER TABLE group_members ADD COLUMN owner_auth_sig BLOB;
ALTER TABLE group_members ADD COLUMN owner_position INTEGER;
ALTER TABLE group_members ADD COLUMN owner_rcv_pub_key BLOB;
ALTER TABLE groups ADD COLUMN link_data_version INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN link_data_remote_version INTEGER NOT NULL DEFAULT 0;

-- M20260509_owner_mesh.hs
CREATE TABLE channel_owner_mesh ( ... );

-- M20260510_promotion_in_progress.hs
CREATE TABLE channel_promotion_in_progress ( ... );
```

Up/down-migration files in
`/workspace/src/Simplex/Chat/Store/SQLite/Migrations/` and mirrors in
`/workspace/src/Simplex/Chat/Store/Postgres/Migrations/`. Register in
`Migrations.hs` after the actual current head (currently
`M20260507_relay_inactive_at`; verify at implementation time).

### Agent-side (simplexmq)

**No agent-side schema migrations.** `linkRootSigKey` persistence is
not required for this work (see 1.2). All other agent changes are
in-process only.

---

## 5. Wire-format and version gating

### Wire format

- **SMP queue layer:** unchanged. `recipientKeys :: NonEmpty
  RcvPublicAuthKey` already supports any-of-N; `RKEY` already mutates
  it. Verified at `simplexmq/src/Simplex/Messaging/Server.hs:1228-1290,
  1471, 1483`.
- **Agent link blob layer:** structurally unchanged. `OwnerAuth`
  already encodes 3 fields with extension headroom (length-prefixed
  inner ByteString — see
  `simplexmq/src/Simplex/Messaging/Agent/Protocol.hs:1801-1807`);
  `validateLinkOwners` already chain-walks. Cap depth at 8 (subject to
  the top-level design-decision callout).
- **Mutable user-data payload (chat layer):** add
  `linkDataVersion :: Maybe Word64` to `GroupShortLinkData` JSON.
  JSON unknown-field tolerant on older clients; absence interpreted
  as 0 by newer.

### Version constants

```haskell
-- Simplex.Messaging.Agent.Protocol
multiOwnerSMPAgentVersion :: VersionSMPA
multiOwnerSMPAgentVersion = VersionSMPA 8

currentSMPAgentVersion :: VersionSMPA
currentSMPAgentVersion = multiOwnerSMPAgentVersion

supportedSMPAgentVRange :: VersionRangeSMPA
supportedSMPAgentVRange = mkVersionRange minSupportedSMPAgentVersion currentSMPAgentVersion
```

```haskell
-- Simplex.Chat.Protocol  (chat protocol version range)
multiOwnerChatVersion :: VersionChat
multiOwnerChatVersion = VersionChat <chat-version-current+1>

currentChatVersion = multiOwnerChatVersion
```

(`<chat-version-current+1>` to be filled in by the implementing agent
after re-reading `chatVersionRange` at the time of work.)

### Hard incompatibility

Older clients (pre-`multiOwnerSMPAgentVersion`) call the older
`decryptLinkData` that hard-requires `rootKey` for the user-data
signature. They will reject blobs signed by a chained owner. This
breaks read access to channels that have promoted at least one
chained owner. **Acceptable per design decision.** Surface in release
notes:

> Channels with multiple owners require SimpleX Chat v7 or later to
> read. Earlier versions will see the channel link as invalid until
> upgrade.

---

## 6. UI deliverables (consolidated)

| ID | Screen | iOS file | Kotlin file |
|---|---|---|---|
| D1 | Channel settings → Owners section | `Shared/Views/Chat/Group/GroupChatInfoView.swift` | `views/chat/group/GroupChatInfoView.kt` |
| D1' | Owners list screen | `Shared/Views/Chat/Group/Owners/ChannelOwnersView.swift` (new) | `views/chat/group/owners/ChannelOwnersView.kt` (new) |
| D2 | Add owner — member picker + invite | `Shared/Views/Chat/Group/Owners/AddChannelOwnerView.swift` (new) | `views/chat/group/owners/AddChannelOwnerView.kt` (new) |
| D3 | Remove owner — cascade preview | `Shared/Views/Chat/Group/Owners/RemoveChannelOwnerSheet.swift` (new) | `views/chat/group/owners/RemoveChannelOwnerSheet.kt` (new) |
| D4 | Sync indicator banner | inline in `GroupChatInfoView.swift` | inline in `GroupChatInfoView.kt` |
| D5 | Single-owner upgrade prompt | (none — silent) | (none — silent) |
| D6 | Owner-chat groundwork (TODO markers only) | `Shared/Views/Chat/Group/Owners/ChannelOwnersView.swift` | `views/chat/group/owners/ChannelOwnersView.kt` |

### String files

- iOS: `apps/ios/en.lproj/Localizable.strings` plus
  `apps/ios/SimpleX Localizations/*/Source Contents/en.lproj/Localizable.strings`.
- Kotlin: `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`.

### Layout-prompt compliance

iOS: every new `Image` inside `ChannelOwnersView` /
`AddChannelOwnerView` is either an `SF Symbol` (no `.resizable()`
needed) or a `MemberProfileImage` (which already chains
`.resizable().scaledToFit()` internally). All sections follow the
project's `Section { ... } header: { } footer: { }` idiom — no row
clipping issues because content rows host plain text + symbols.

Compose: `Column` calls explicitly state alignment when non-default;
`Image`/`Icon` always have explicit `Modifier.size(...)`;
`ContentScale.Fit` set on raster images. Owner-row uses
`Modifier.weight(1f)` on the inner Column with non-weighted siblings
on either side, avoiding the "only weighted children" trap from
`layout-compose.md`.

---

## 7. Risk register

| # | Risk | Mitigation | Test |
|---|---|---|---|
| 1 | **Hard-break for older clients on multi-owner channels.** Older clients reject owner-signed mutable blobs. | agentVRange bump (5.); release notes; channels that haven't promoted remain readable by older clients. | `testOldClientRejectsChainedOwnerBlob` (negative); `testOldClientReadsRootSignedBlob` (positive). |
| 2 | **Promotion atomicity.** Mid-flow process death leaves partial state. | `channel_promotion_in_progress` journal + idempotent steps + `resumePromotions` on startup. | `testPromoteOwnerResumeStep<N>` for each step. |
| 3 | **LWW data loss in concurrent roster edits.** Two owners adding/removing simultaneously can lose one mutation. | Optimistic-concurrency retry on roster mutations (4.2 step 7); D4 banner alerts user; bounded retries return error to UI on persistent contention. | `testRosterRaceRetry`. |
| 4 | **Cascade-removal UX clarity.** User unaware that removing X also removes Y, Z. | D3 explicit list; remove disabled when remover's chain depends on removee. | UI snapshot + `testCascadeUiPreviewShowsAll`. |
| 5 | **Co-owner credential bundle confidentiality.** Bundle leaks → impersonation. | Bundle sent only over the channel-scoped direct mesh connection (E2E ratchet, not relay-mediated); never logged; bundle replays detected by version check + roster lookup at receive time. | `testBundleCrossChannelReject`; `testBundleReplayRejected`; static check that no `logInfo`/`putStrLn` paths touch the bundle. |
| 6 | **Member-key = owner-key invariant.** A future refactor accidentally separates them, breaking signature verification of chat messages by owners. | `testMemberKeyEqualsOwnerKey` invariant test running on every channel-creation and promotion. | `testMemberKeyEqualsOwnerKey`. |
| 7 | **Chain-cycle attempts.** Adversarial blob includes self-referential `authOwnerSig`. | `validateLinkOwners` is prefix-only — structurally cycle-free. Depth cap of 8 (pending team confirmation) prevents arbitrary list inflation. | `testChainCycleStructurallyImpossible`; `testChainTooLong`. |
| 8 | **Co-owner DELs the queue.** | Chat-layer gate: `DEL` allowed only when this device holds `GRKPrivate _` (creator role) in `GroupKeys.groupRootKey`. Co-owner path raises `CEPermissionDenied`. | `testCoOwnerCannotDeleteQueue`. |
| 9 | **Server returns stale blob during reconciliation.** Owner reads version N, server returns N-1. | Treat returned `linkDataVersion` as authoritative if it includes a valid signature; if version monotonicity is violated, surface a "channel state inconsistent" warning and skip the write (do not LSET an older version). | `testStaleBlobIgnored`. |
| 10 | **RKEY race — concurrent owner promotions can drop one another's candidate.** | Eventual consistency via mesh-driven recovery (Phase 4.5); affected owner re-RKEYs on AUTH failure. | `testConcurrentRKEYConvergence`. |
| 11 | **Member-pub-key rotation breaks owner status.** | Invariant test (4.4) plus documentation: member signing keys are fixed for the channel's lifetime; if a future feature rotates them, the owner roster must be rewritten in the same transaction. | `testMemberKeyEqualsOwnerKey`. |

### Threat-model alignment

The plan preserves channels-overview.md objectives 1-7 unchanged; in
particular:

- **Objective 6 (sender anonymity within multi-owner channels)** is
  *strengthened* — a passive observer with the link key sees only
  "some chain-valid owner pushed an update", not which one.
- **Objective 3 (no relay impersonation)** unchanged — admin
  signatures still require a chain-valid owner key, and the link blob
  binds the chain to the rootKey.
- The new attack surface is the owner mesh. It is direct, E2E-
  encrypted (double-ratchet inherited from agent layer), channel-
  scoped, and invisible to subscribers and relays. A compromise of
  one owner mesh edge does not compromise the channel beyond what
  compromise of an owner's keys already implies.

---

## 8. Decisions and remaining open questions

### Resolved decisions

| Topic | Decision | Where it shows up |
|---|---|---|
| simplexmq pin vs. upstream | Both repos at master; no coordination needed | Phase 1 preflight reads, section 4 |
| `linkRootSigKey` persistence | Not required; rootKey extracted from fixed data on every decode; chat already has `GroupRootKey` for both creator and co-owner cases | Phase 1.2; section 4 (no agent migration) |
| Owner roster storage | Extend `group_members` with three nullable columns (`owner_auth_sig`, `owner_position`, `owner_rcv_pub_key`); no separate tables | Phase 2.1, 2.2 |
| Co-owner credential storage on chat side | Implicit in agent's `rcv_queues` row + chat-side `GroupKeys.groupRootKey = GRKPublic _` + `groups.root_pub_key` | Phase 1.6, 2.1 |
| Mesh content scope | Protocol events only (`x.grp.owner.creds`, `x.grp.link.sync`, `x.grp.mem.role`, `x.grp.mem.intro/inv/fwd`); other events logged and rejected; no `x.msg.new` persistence | Phase 2.4.1 |
| `linkDataVersion` placement | Chat-layer `GroupShortLinkData` JSON (not agent-layer record) | Phase 1.7 |
| `x.grp.link.sync` tag | Reserved for this delivery; the parallel link-data-passing workstream picks a different name | Phase 4.1 |

### Remaining open questions

1. **Owner cap.** Plan defaults to `ownerChainDepth = 8` (~1.5 KB blob
   cost; O(N²) ≈ 64 verifies on decode). The cap is a one-line
   constant change. Promoted to a top-level "Design decision required"
   callout above. Implementation must not begin until the team
   confirms the value.

---

## 9. Out of scope

- Legacy private (P2P) groups — unaffected.
- Multisig and programmable governance.
- Promoting a subscriber to owner via relay-mediated offer (no direct
  connection between existing owner and candidate). Possible follow-up.
- Pre-signing N≥2 root-signed owners at channel creation for creator-
  anonymity preservation. Future feature.
- Transfer of root creator. Future feature.
- Owner-only chat UI (thread surface in chat list, message
  composition view). UI is post-MVP. Mesh content (`x.msg.new` and
  the like) is **not persisted** in this delivery; persisting it now
  would store data that no UI can display.
- Owner-mesh push-style catchup for offline owners. LGET-on-startup
  is the catchup mechanism for MVP.
- Public-groups-over-relays migration (orthogonal workstream; may
  reuse owner-mesh primitives later).
- UX for monitoring relay-level delivery health
  (channels-overview.md "Current gap #3"; tracked separately).
- Member-pub-key rotation while the member is an owner (would
  invalidate stored `OwnerAuth.ownerKey`; future feature must rewrite
  the roster in the same transaction).
- Agent-side `linkRootSigKey` persistence. The TODO at
  `Agent/Store/AgentStore.hs:2514` stays untouched.
