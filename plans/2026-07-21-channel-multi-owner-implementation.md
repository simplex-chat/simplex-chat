# Channel multi-owner — implementation plan

Companion to the design overview (`2026-07-15-channel-multi-owner.md`) — read that first for *why*. This covers *what to build*: data model, migrations, types, protocol events, commands, and sequencing. It is a map for implementing agents, not an exhaustive edit list; trivial wiring (JSON instances, `CMEventTag` cases, view strings) is implied.

The sequencer frame (overview §1) drives everything: link data and owner keys are sequenced by the SMP server via **hash-CAS** (the server keys on a content hash, not a version counter); the roster has no sequencer and converges by a deterministic blob tie-break plus owner-side auto-retry; the subscriber count is cosmetic and any owner corrects it on channel open.

## Sequencing

Two repos, in order. **simplexmq first** (the relay version bump, hash-CAS on the SMP link queue, the foreign-link write path, `linkRootSigKey`), then **simplex-chat** (data model, events, logic, apps). Multi-owner is gated on the link queue's server negotiating the new SMP relay version; below it, promotion is blocked, not degraded.

---

## Part 1 — simplexmq

### 1.1 SMP server — hash-CAS on the link queue

- **Version bump.** New `VersionSMP` `linkCasSMPVersion = 19`; bump **both** `currentClientSMPRelayVersion` and `currentServerSMPRelayVersion` 18 → 19 (`Transport.hs:228`/`:234`). The negotiated session version is the *min* of the two peers' relay ranges, so the client constant must also advance for a client to perceive and exercise v19. This is the SMP *relay* version (`VersionSMP`, gating `Command`/`BrokerMsg` encoding) — **not** `currentSMPClientVersion`, which rides the client↔client envelope and gates nothing server-facing.
- **The CAS token is a content hash, not a counter.** For link data it is the hash of the stored *encrypted* user-data blob (the only thing the server can key on, since it cannot read the content); for `recipientKeys` it is the hash of the canonically sorted key set. No server version counters.
- **Where the CAS lives — inside the queue store, not the command handler.** In the Postgres store the mutable link blob is not loaded into the in-memory `QueueRec` on the normal path (`rowToQueueRec` uses empty placeholders), so link-data hashing cannot be done in the handler. Add a persisted `user_data_hash` column maintained on every link-data write, and CAS via `UPDATE … WHERE user_data_hash = expected`, returning the current blob on a 0-row update. `recipientKeys` *is* loaded normally in both stores, so the key-set hash-CAS compares in memory — an asymmetry to respect.
- **Two new recipient commands** (`Command Recipient`, gated ≥ v19; new tags rather than extending `LSET`/`RKEY`, so version-gating is clean):
  - **`LSETH expectedHash linkId d`** — hash-CAS `LSET`. If `hash(stored) == expectedHash`: store `d`, reply the **new** hash. Else reject, replying `(currentHash, currentUserData)`. The check sits beside the existing `lnkId' /= lnkId -> err AUTH` in the `LSET` handler (`Server.hs:1484`).
  - **`RKEYH expectedHash keys`** — hash-CAS `RKEY`. Same pattern on the sorted-key-set hash (`Server.hs:1483`, `updateKeys` `STM.hs:194`); reject replies `(currentHash, currentKeys)`.
- **New `BrokerMsg` responses:** success replies the new hash (small); a conflict replies `(hash, payload)` — `LSETH` returns the user-data blob, `RKEYH` the key set. (`LNK` already carries a `QueueLinkData`, so returning a blob is routine.) Returning current state on rejection is what removes the writer's pre-read.
- **Auth unchanged:** both are recipient commands, so any key in `recipientKeys` authorises them (`Server.hs:1248-1249`) — this is what lets a promoted owner write. `recipientKeys` stays `NonEmpty` (the parser rejects a zero-length set), so an `RKEYH` can never empty the set; the server has no notion of owner/revocation, so "≥ 1 *live* owner" is a client-side concern (Part 6).

### 1.2 SMP agent — foreign-link write, hash-CAS APIs, `linkRootSigKey`

- **`rcv_queues` columns** (both SQLite and Postgres agent-store migration trees, selected by `-fclient_postgres`): `link_root_sig_key BLOB` — a real durable column that fixes `AgentStore.hs:2514` (it currently hardcodes `linkRootSigKey = Nothing`, a `TODO`; without it `validateOwners` derives the root as the owner's *own* key). Populate it on read and set it when a promoted owner provisions link creds. Also cache `link_data_hash BLOB` and `key_set_hash BLOB` (last-known hashes) beside the link credentials, replacing any version counter.
- **Hash-CAS agent APIs** over the CAS commands: `setConnShortLinkCAS` (takes the expected hash; on success returns the **new hash the agent computes from the exact ciphertext it uploaded** — never re-encrypting, since encryption uses a fresh random nonce, so chat cannot reproduce the bytes; on conflict returns the decrypted current data + its hash) and `updateRcvKeysCAS` (RKEY hash-CAS, same shape). On conflict the agent hands chat the current state + hash; it never re-merges (merge policy is chat's).
- **Foreign-link path** — a promoted owner does not own the link queue. Add `setForeignLinkData` / `getForeignLink` / `updateForeignRcvKeys` taking **explicit creds** — `SMPServer`, the link-queue recipient id (`linkRcvId`, delivered in the promotion `inv`; it is *not* derivable from the public short link, which yields only the sender-side `linkId`), the owner's `linkRcvKey` private half, the `LinkKey`, `rootPubKey`, and the expected hash. Prefer this over materialising a fake `RcvQueue`/`ContactConnection`, which would need an `rcvDhSecret` the owner must not have and risks it subscribing to the creator's queue.
- **`Crypto/ShortLink.hs`:** extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth` (the signer's private key); redefine `newOwnerAuth` on it; export (kills the formula duplicated in chat at `Internal.hs:1554`). The creator signs a new owner with the **root** private key (as `Commands.hs:2690` already does for itself), keeping the chain flat; a non-creator owner signs with its **own member** key (delegation). `validateLinkOwners` is **unchanged** — it accepts "signed by root or an earlier entry", which holds as long as chat writes the owners list in insertion (signer) order (Part 2), so no order-independence change is needed.

---

## Part 2 — simplex-chat: data model & migration

One migration `M20260721_channel_multi_owner` (register in `Migrations.hs` + `.cabal`, both SQLite and Postgres; `chat_schema.sql` regenerates from tests). All new columns nullable / defaulted, so `ALTER TABLE ADD COLUMN` is safe on the STRICT tables and remote-desktop JSON stays forward-compatible.

### `group_members` (columns)
| column | on which rows | purpose |
|---|---|---|
| `owner_auth_sig BLOB` | owners | chain entry signature (with existing `member_pub_key` = key, this is the full `OwnerAuth`) |
| `owner_auth_index INTEGER` | owners | **insertion (signer) order** in the chain — a new entry is appended after the entry that signed it; load-bearing for `validateLinkOwners`, *not* a memberId sort |
| `owner_revoked INTEGER DEFAULT 0` | owners | revoked owner: kept in chain as a past signer, not a current owner (mirrors `revokedOwners` in the blob) |
| `link_rcv_key BLOB` | owners | the owner's link-write **public** key — the local `memberId → linkRcvKey` map |
| `relay_subscriber_count INTEGER` | relays | latest absolute count that relay reported |
| `relay_subscriber_count_ts TEXT` | relays | that reading's **broker** timestamp (the freshness token; cleared with the count on `GSMemRemoved`/`GSMemLeft`) |
| `promote_invitation_id BLOB` | the promotee (invitee row on O1; own membership on M) | pending promotion |
| `promote_role TEXT` | ″ | offered role |
| `promote_status TEXT` | ″ | invited / accepted / rejected / cancelled |
| `promote_link_rcv_id BLOB` | M's own row | link-queue recipient id (from `inv`) |
| `promote_link_rcv_priv_key BLOB` | M's own row | M's generated link-write private key (one key, reused across concurrent invites) |

`MemberRoleProposal` (Part 3) is *derived* from `promote_*`, not a stored column.

### `groups` (columns)
| column | purpose |
|---|---|
| `link_data_hash BLOB` | last hash this device knows for the published link data (from events/accepted writes) — the value it CASes on |
| `key_set_hash BLOB` | last known key-set hash |
| `stored_roster_owner_id BLOB` | cross-device author `memberId` of the **stored roster blob**, written beside `stored_roster_version` only by `setGroupLiveRoster`; the tie-break key (supersedes the device-local `roster_sending_owner_gm_id` for comparison) |
| link-write creds (a small group) | for *this user as a non-creator owner*: its `linkRcvKey` private half + the link-queue coordinates (`SMPServer`, `linkRcvId`, `LinkKey`, `rootPubKey`) so its agent can write the foreign link queue (the creator keeps `GroupKeys` unchanged) |

Do **not** add a `roster_version_owner_id` on the gate `roster_version` — the delta path (`setGroupRosterVersion`) must advance the gate *without* writing any blob author, or a delta would corrupt the blob tie-break (Part 6).

### `group_pending_role_changes` (new table)
`(group_id, group_member_id, target_role, pre_role, roster_version, created_at)` — an owner's role changes not yet confirmed converged. Drives the roster auto-retry (overview §4): re-apply a row only while it is unsatisfied **and** the member still sits at `pre_role`; **clear** the row as soon as an observed roster shows the member at `target_role` (this is what stops an old satisfied delta from resurrecting a value and ping-ponging with a legitimate later change).

### Whole-value link-data pending edit
For a whole-value link field (channel profile/prefs) that the owner has edited but not yet had accepted, keep a durable `(base_hash, pending_value)` (a small `groups` column group or side table), cleared on an accepted `LSETH`. Reconcile diffs local against published relative to `base_hash` to classify adopt / keep-pending / surface-conflict. Set fields (relays, owners, revoked-owners) need no such record — they merge as an idempotent union keyed by identity.

### Published link data (not DB — the `UserContactData` user-data blob)
Gains an in-content `linkDataVersion` (advisory ordering — recipients drop older events; correctness rests on the server hash-CAS and reconcile, so a corrupted value only degrades live-event ordering, never stored state), a `revokedOwners :: [MemberId]` list, and `(subscriberCount, subscriberCountTs)`, alongside the existing `owners`/`relays`/profile. `owner_revoked` locally mirrors `revokedOwners`.

---

## Part 3 — new Haskell types

- **`MemberRoleProposal = MRProposed GroupMemberRole | MRRejected GroupMemberRole`** (`Types.hs`); `GroupMember` gains `memberRoleProposal :: Maybe MemberRoleProposal` (derived from `promote_*`) and the app-facing `promotionPending :: Bool`.
- **`PromotionRoleData`** for the wire: a role-tagged sum with an opaque fallback — `PRDOwnerInv { linkRcvId }` / `PRDOwnerAcpt { linkRcvKey }` / `PRDUnknown` (decode an unrecognised tag to `PRDUnknown`, do **not** error — `omittedField` only covers an *absent* field, not a present unknown one).
- **`ForeignLinkKeys`** for a promoted owner — a record holding `SMPServer`/`linkRcvId`/`LinkKey`/`rootPubKey`/`linkRcvKey` priv + the last-known link-data and key-set hashes; the input to the Part 1.2 foreign-link APIs.
- Reuse: `OwnerAuth` (simplexmq), `MemberId`, `MemberKey`, `VersionRoster`.

---

## Part 4 — new protocol events (`ChatMsgEvent`, `Protocol.hs`)

All `requiresSignature`. Wire is JSON with optional-field forward-compat (`.=?` / `opt`).

| event | dir | fields | delivery |
|---|---|---|---|
| `x.grp.promote.inv` | O1→M | `invitationId, memberRole, roleData?` (`roleData` = `PRDOwnerInv{linkRcvId}`) | M's support scope |
| `x.grp.promote.acpt` | M→O1 | `invitationId, memberKey, roleData?` (`roleData` = `PRDOwnerAcpt{linkRcvKey}`) | support scope |
| `x.grp.promote.reject` | M→O1 | `invitationId` | support scope |
| `x.grp.promote.cancel` | O1→M | `invitationId` | support scope |
| `x.grp.owner.key` | owner→owners | `memberId, linkRcvKey` | **`DJSOwners`** (new) |
| `x.grp.relay.count` | relay→owners | `count` (+ the batch's broker ts) | batched into the `DJSGroup` delivery already carrying the member join/leave/removal event (owners receive it as members) |

`memberKey` in `acpt` is M's existing key; O1 checks it **equals the key it already holds for M** (a consistency check, distinct from the advisory OOB verification in Part 7). The `linkRcvKey` in `acpt` is one key M generates once and reuses across concurrent invites — the memberId-keyed owners merge already collapses two concurrent adds of M to one `OwnerAuth`, but reuse additionally keeps `recipientKeys`, the surviving `OwnerAuth.ownerKey`, and M's materialised `linkPrivSigKey` all referencing the same key even when different owners win the `RKEYH` and `LSETH` races.

Additive: `linkDataVersion` and the **server-confirmed** `link_data_hash` become optional fields on the events that change link data (`XGrpInfo`, `XGrpRelayNew`, the owner add/remove/revoke events), so owners track both without a pre-read. Order: write first (`LSETH`), then broadcast the event carrying the hash of what was written.

The roster tie-break needs **no wire field** — the author is the message signer (`withAuthor`), already available at the completion gate; store its cross-device `memberId` in `stored_roster_owner_id` at completion and compare there.

New delivery scope **`DJSOwners`** (`Delivery.hs` + the worker branch near `Subscriber.hs:4276`): recipients = `getGroupOwners` (owner-only, mirrors the existing `DJSMemberSupport` shape; no schema column needed). The owner-key map must not be public or on the link server — a joining SMP operator could otherwise correlate a link-queue key to an owner identity — so it travels owner-scoped and is cached by relays (Part 6).

---

## Part 5 — commands (`ChatCommand`, `Controller.hs`)

- **`APIAcceptRolePromotion GroupId`** — M generates its `linkRcvKey` (once), sends `acpt`, marks accepted; the async chain (overview §6) then runs on O1.
- **`APIRejectRolePromotion GroupId`** — sends `reject`, clears.
- **`APICancelRolePromotion GroupId GroupMemberId`** — O1 sends `cancel`, clears the proposal.
- Route **`APIMembersRole … GROwner`** on a channel into the invite flow — single-target (reject a multi-member owner promotion, mirroring the existing admin-batch guard in the batch-role handler).
- Route owner **removal** (`APIRemoveMembers` of an owner) into the removal flow (overview §7): an async command that writes the revocation **first** (`LSETH` adding the target to `revokedOwners`), **then** drops the key (`RKEYH`), each `RKEYH`/`LSETH` payload built by applying the delta to the server's *current* set/blob (the believed one, or the one returned on a conflict) — never rebuilt from the local map, which could drop a concurrently-added owner's key. A crash between the two heals via reconcile (Part 6). Refuse removing/leaving the last *live* owner (local `owners` minus `revokedOwners`); a stale view under concurrent leave+removal or mutual removal is an accepted, disclosed residual, not auto-healed.

The promotion and removal chains are async commands resumable from their durable records (the `promote_*` columns / the removal command's own state) — no new durable-worker type.

---

## Part 6 — key logic changes (anchors)

- **`groupLinkData`** (`Internal.hs:1543`, self-only owners at `:1550-1554`) → build the write from local state via the **reconcile** routine; delete the `GRKPrivate` singleton / `[]` fallback; read the stored chain (insertion order).
- **Reconcile** = merge published link data into local, per field: set fields (relays, owners, revoked-owners) merge as an idempotent union keyed by identity (memberId for owners), so an owner's own not-yet-published additions survive without a stored delta; a whole-value field (profile/prefs) uses the durable `(base_hash, pending_value)` (Part 2) to classify adopt / keep-pending / surface-conflict. Run it **on channel open** (owners run `APIGetUpdatedGroupLinkData` like subscribers — remove the owner-exclusion around `syncSubscriberRelays`, `Commands.hs:1913`, and the app-side `else if`), **on a CAS conflict** (adopt the returned blob, rebuild), and as the **missed-event backstop**.
- **Removal heal** (runs during reconcile-on-open, and it is what makes the revoke-first ordering crash-safe): for each `memberId` in the decrypted `revokedOwners` whose `linkRcvKey` the owner still holds in its local map, issue one `RKEYH` dropping that key from the server-*returned* current set (CAS-retry inline; an already-absent key is a harmless no-op). This is the sole heal direction, because `revokedOwners` is readable from the blob while `recipientKeys` is write-only — so a crash between `LSETH` (revocation) and `RKEYH` (key drop) is recoverable, but the reverse order would not be. Merging `revokedOwners` into local state alone does *not* evict a still-live key; this explicit server-side `RKEYH` is required.
- **Relay activation off an accepted write**, not the `LINK` echo (`Agent.hs:1814` notifies `LINK link userLinkData` — the client's own submitted data, not the server's stored truth).
- **Roster** (overview §4): the event gate stays **version-only, author-blind** (the `fresh` check, `Subscriber.hs:3304`, `version >= held`), so concurrent *different-member* deltas both apply — the union. The `(version, authorMemberId)` tie-break is added **only at the completion gate** (`Subscriber.hs:3462-3468`): keep the existing downgrade reject (`pendingVer < roster_version` gate) and, at the equal-version case that today overwrites by arrival order, accept iff `pendingVer > stored_roster_version` **or** (`pendingVer == stored_roster_version` **and** `author > storedAuthor`) — comparing the incoming blob's signer against `stored_roster_owner_id`, never the gate. Store the cross-device author with the blob in `setGroupLiveRoster` (`:3468`). **Auto-retry is event-driven**: an owner holding an unsatisfied pending role-delta that sees another owner's concurrent (same-version) `x.grp.mem.role` re-broadcasts its now-union local state at a higher version; both owners do this, so the served blob converges without waiting on any one owner. Existing members converge from the re-emitted `x.grp.mem.role` (`DJSGroup` → all members). No leader, no relay-to-owner blob forwarding.
- **Count** (overview §5): `x.grp.relay.count` produced by the relay branch of `updatePublicGroupData` (`Internal.hs:1491`) and batched with the join/leave/removal delivery; owners store `(count, ts)` per relay, estimate = **max** count over current relays carrying **that max-holder's own `ts`**. On channel open (during reconcile) any owner publishes `(estimate, that ts)` to link data iff the estimate differs from the published count **and** the max-holder's `ts` is newer than `subscriberCountTs` — a freshness gate, no leader, no threshold. Clear a relay's `(count, ts)` on `GSMemRemoved`/`GSMemLeft` (`Types.hs:1357-1358`).
- **Owner materialisation** threads the full `OwnerAuth` (sig + append index): `createLinkOwnerMember` (`Groups.hs:3508`), `updateRelayGroupKeys` (`Groups.hs:2299`, also create the row if missing). Root-sign when the adder is the creator (holds the root private key), else delegate.
- **Relay caches** the signed `memberId → linkRcvKey` map (announced over `DJSOwners`) and serves it on request — the source an owner uses to *identify* a removal target's key; the `RKEYH` payload is still built from the server-returned set. Each entry is owner-signed, so a relay cannot forge one, and a relay is not the link-queue operator, so it cannot correlate link-queue writes.
- **Promoted owner commit:** on receiving `x.grp.mem.role … GROwner` (the commit point), M materialises its `ForeignLinkKeys` (rcvId = `linkRcvId` from the inv, `linkPrivSigKey` = its generated key, `linkRootSigKey` = the channel root public key from `FixedLinkData`, `LinkKey`/server from the channel link) so its agent can issue foreign `LSETH`/`RKEYH`.
- **Creator's root key is unrevocable:** "removing the creator" evicts its link-write key and marks its owner entry revoked, but it retains root signing authority (the immutable trust anchor in `FixedLinkData`); true creator eviction would need a root-key rotation, out of scope.

---

## Part 7 — apps (iOS + Kotlin, mirror both)

- `GroupInfo`: decode **`canManageLink`** (new backend field); gate link/relay *management* on it (an owner mid-handover cannot write yet), keep relay *status display* on `isOwner`.
- `GroupMember`: decode `memberRoleProposal` + `promotionPending` (optional/nullable).
- **New `RcvGroupEvent` case** for the promotion service item (both enums are exhaustive — a missing case throws) + strings.
- `canChangeRoleTo` (`ChatTypes.swift:3092`, `ChatModel.kt:2668`): add `.owner` for channels; selecting it starts the async invite and shows the proposed-role row (proposed + Cancel / rejected + re-invite). Warn if the invitee's key is unverified and route to the verification screen (`verifyChannelMemberCode`, `Commands.hs:3702`, called at `:2018`, hashes both keys sorted) — advisory, matching the existing channel model; the consequence is the channel's trust chain, not one conversation.
- Invitee: a **new** support-chat accept/reject banner (the existing pending-member bar is gated on a non-null scope member; M's own support scope has none, so this is new code reading the proposal from `membership`) + the service-item unread; accept goes through a confirmation stating any owner can delete the channel.
- Conflict surfaces: "profile edit superseded", "your role change to X was overridden" — plain, re-apply via normal UI.
- Fix the chat-list leave guard to honour `hasOtherOwner` at **both** occurrences per file (`ChatListNavLink.swift:247`+`:272`, `ChatListNavLinkView.kt:319`+`:341`) — the guard `!(useRelays && isOwner)` appears in two member-status branches; patching one leaves the other inconsistent.

---

## Part 8 — version gating

Detect the link queue's negotiated server version (the relay-handshake `thVersion`); **require ≥ v19 to promote a second owner** (the hash-CAS `LSETH`/`RKEYH` all need it). On an older link server, block promotion with a clear error rather than degrade — a blind `RKEY` there evicts other owners (replace with no CAS). Because the negotiated version is the min of the two peers' relay ranges, both relay constants (Part 1.1) must reach 19 before a client perceives v19.

---

## Part 9 — tests (scope)

Under `describe "channels"` (`tests/ChatTests/Groups.hs`), ceiling 2 owners + 1 relay + 3 subscribers (profiles run out at `frank`). Priorities:
- promote O2 (verified-then-accepted), and its admin message accepted by a subscriber that only saw O1;
- owner-2 adds owner-3 (delegated entry validates in insertion order);
- concurrent link writes by two owners (hash-CAS: loser re-merges onto the returned blob, both survive);
- concurrent roster edits — different members (event-gate union + auto-retry → served blob converges to both) and same member (tie-break winner, loser surfaces conflict);
- count across a newly-added relay (no crash; max ignores the catching-up relay) and publish-on-open updating a small channel 1 → 2 → 3;
- owner removal (revoke-first + key drop, a new joiner sees the removed owner as non-owner); crash between `LSETH` and `RKEYH` heals on another owner's reconcile;
- adversarial — replayed `acpt` (invitation-bound, no double-add), a moderator-injected `x.grp.promote.inv` (inert: no owner holds a matching pending record, M rejects a non-owner-signed inv), a chain published out of order / on a pre-v19 link server (promotion fails closed), promotion on an unverified key (advisory warning only).

The count/roster races need forced interleaving (`deliveryWorkerDelay`).
