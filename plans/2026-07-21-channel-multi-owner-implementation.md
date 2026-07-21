# Channel multi-owner — implementation plan

Companion to the design overview (`2026-07-15-channel-multi-owner.md`) — read that first for *why*. This covers *what to build*: data model, migrations, types, protocol events, commands, and sequencing. It is a map for implementing agents, not an exhaustive edit list; trivial wiring (JSON instances, `CMEventTag` cases, view strings) is implied.

## Sequencing

Two repos, in order. **simplexmq first** (versions + CAS on the SMP queue, the foreign-link write path, `linkRootSigKey`), then **simplex-chat** (data model, events, logic, apps). Multi-owner is gated on the link queue's server being ≥ the new SMP relay version; below it, promotion is blocked, not degraded.

---

## Part 1 — simplexmq

### 1.1 SMP server — versioned CAS on the link queue

- **Version bump.** New `VersionSMP` `linkCasSMPVersion = 19`; `currentServerSMPRelayVersion` 18 → 19. (This is the SMP *relay* version — `VersionSMP`, gating `Command`/`BrokerMsg` encoding in `Protocol.hs` — **not** `currentSMPClientVersion`, which rides the client↔client envelope and gates nothing server-facing.)
- **Queue record** (`Server/QueueStore`, both STM and Postgres stores): add `linkDataVersion :: Int64` and `keySetVersion :: Int64` — server-owned counters, initialised at 0.
- **Two new recipient commands** (`Command Recipient`, gated ≥ v19; new tags rather than extending `LSET`/`RKEY`, so version-gating is clean):
  - **`LSETV expectedVersion linkId d`** — CAS `LSET`. If `linkDataVersion == expectedVersion`: store `d`, increment, reply the new version. Else reply a conflict carrying `(linkDataVersion, currentUserData)`. The check sits beside the existing `lnkId' /= lnkId -> err AUTH` in the `LSET` handler (`Server.hs:1484`); the bytes are already in `queueData qr`, so no new server storage beyond the counter.
  - **`RKEYV expectedVersion keys`** — CAS `RKEY`. Same pattern on `keySetVersion`; conflict carries `(keySetVersion, currentKeys)`.
- **New `BrokerMsg` responses:** success replies the new version (small); conflict replies `(version, payload)` — `LSETV` conflict returns the user-data blob, `RKEYV` conflict returns the key set. (`LNK` already carries a `QueueLinkData`, so returning a blob is routine.)
- **Auth unchanged:** both are recipient commands, so any key in `recipientKeys` authorises them (`Server.hs:1249`) — this is what lets a promoted owner write.

### 1.2 SMP agent — foreign-link write, versioned APIs, `linkRootSigKey`

- **`rcv_queues` columns** (both SQLite and Postgres agent-store migration trees, selected by `-fclient_postgres`): `link_root_sig_key BLOB` (fixes `AgentStore.hs:2514`); `link_data_version`, `key_set_version` cached for the *creator's own* link queue.
- **Versioned agent APIs** over the CAS commands: `setConnShortLinkCAS` (returns new version, or a conflict with the decrypted data + version); `updateRcvKeysCAS` (RKEY-CAS, same). On conflict the agent decrypts and hands chat the current state + version; it never re-merges (merge policy is chat's).
- **Foreign-link path** — a promoted owner does not own the link queue. Add `setForeignLinkData` / `getForeignLink` / `updateForeignRcvKeys` taking **explicit creds** (`SMPServer`, `linkId`, the owner's `linkRcvKey` private half, the `LinkKey`, `rootPubKey`, expected version). Prefer this over materialising a fake `RcvQueue`/`ContactConnection`, which would need an `rcvDhSecret` the owner must not have and risks it subscribing to the creator's queue.
- **`Crypto/ShortLink.hs`:** extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`; redefine `newOwnerAuth` on it; export (kills the formula duplicated in chat at `Internal.hs:1519`).

---

## Part 2 — simplex-chat: data model & migration

One migration `M20260721_channel_multi_owner` (register in `Migrations.hs` + `.cabal`, both SQLite and Postgres; `chat_schema.sql` regenerates from tests). All new columns nullable / defaulted, so `ALTER TABLE ADD COLUMN` is safe on the STRICT tables and remote-desktop JSON stays forward-compatible.

### `group_members` (columns)
| column | on which rows | purpose |
|---|---|---|
| `owner_auth_sig BLOB` | owners | chain entry signature (with existing `member_pub_key` = key, this is the full `OwnerAuth`) |
| `owner_auth_index INTEGER` | owners | chain order (load-bearing for `validateLinkOwners`) |
| `owner_revoked INTEGER DEFAULT 0` | owners | revoked owner: kept in chain as a past signer, not a current owner |
| `link_rcv_key BLOB` | owners | the owner's link-write **public** key — the `memberId → linkRcvKey` map |
| `relay_subscriber_count INTEGER` | relays | latest absolute count that relay reported |
| `promote_invitation_id BLOB` | the promotee (invitee row on O1; own membership on M) | pending promotion |
| `promote_role TEXT` | ″ | offered role |
| `promote_status TEXT` | ″ | invited / accepted / rejected / cancelled |
| `promote_link_rcv_id BLOB` | M's own row | link-queue recipient id (from `inv`) |
| `promote_link_rcv_priv_key BLOB` | M's own row | M's generated link-write private key |

`MemberRoleProposal` (below) is *derived* from `promote_*`, not a stored column.

### `groups` (columns)
| column | purpose |
|---|---|
| `link_data_version INTEGER` | last version this device knows for the published link data (tracked from events/writes) |
| `key_set_version INTEGER` | last known key-set version |
| `roster_version_owner_id BLOB` | author `memberId` of the applied roster version — the cross-device tie-break key |
| link-write creds (a small group) | for *this user as a non-creator owner*: its `linkRcvKey` private half + the link-queue coordinates so its agent can write the foreign link queue (extends the existing per-group key material; the creator keeps `GroupKeys` unchanged) |

### New table `group_pending_role_changes`
`(group_id, group_member_id, role, roster_version, created_at)` — an owner's role changes not yet confirmed converged. Drives the roster auto-retry (§4 overview): on adopting a newer blob, re-apply any row the winner did not touch; clear a row once seen in an adopted roster.

### Published link data (not DB — the `UserContactData` user-data blob)
Gains `linkDataVersion` and a `revokedOwners :: [MemberId]` list alongside the existing `owners`/`relays`/profile. `owner_revoked` locally mirrors the latter.

---

## Part 3 — new Haskell types

- **`MemberRoleProposal = MRProposed GroupMemberRole | MRRejected GroupMemberRole`** (`Types.hs`); `GroupMember` gains `memberRoleProposal :: Maybe MemberRoleProposal` (derived from `promote_*`) and the app-facing `promotionPending :: Bool`.
- **`PromotionRoleData`** for the wire: a role-tagged sum with an opaque fallback — `PRDOwnerInv { linkRcvId }` / `PRDOwnerAcpt { linkRcvKey }` / `PRDUnknown` (decode an unrecognised tag to `PRDUnknown`, do **not** error — `omittedField` only covers an *absent* field, not a present unknown one).
- **Link-write creds** for a promoted owner — a record (`ForeignLinkKeys` or an extension of `GroupKeys`) holding `SMPServer`/`linkId`/`LinkKey`/`rootPubKey`/`linkRcvKey` priv + the two versions.
- Reuse: `OwnerAuth` (simplexmq), `MemberId`, `MemberKey`, `VersionRoster`.

---

## Part 4 — new protocol events (`ChatMsgEvent`, `Protocol.hs`)

All `requiresSignature`. Wire is JSON with optional-field forward-compat (`.=?` / `opt`).

| event | dir | fields | delivery |
|---|---|---|---|
| `x.grp.promote.inv` | O1→M | `invitationId, memberRole, roleData?` | M's support scope |
| `x.grp.promote.acpt` | M→O1 | `invitationId, memberKey, roleData?` | support scope |
| `x.grp.promote.reject` | M→O1 | `invitationId` | support scope |
| `x.grp.promote.cancel` | O1→M | `invitationId` | support scope |
| `x.grp.owner.key` | owner→owners | `memberId, linkRcvKey, keySetVersion` | **`DJSOwners`** (new) |
| `x.grp.relay.count` | relay→owners | `count` | batched with the join/leave/removal delivery to owners |

Additive: `link_data_version` becomes an optional field on the events that change link data (`XGrpInfo`, `XGrpRelayNew`, owner add/remove/revoke), carrying the **server-confirmed** version so owners track it without a pre-read.

The roster tie-break needs **no wire field** — the author is the message signer (`withAuthor`), already available at the three gates; store its `memberId` in `roster_version_owner_id` and compare there.

New delivery scope **`DJSOwners`** (`Delivery.hs` + the worker branch at `Subscriber.hs:4276`): recipients = `getGroupOwners` (owner-only, mirrors the existing `DJSMemberSupport` shape; no schema column needed).

---

## Part 5 — commands (`ChatCommand`, `Controller.hs`)

- **`APIAcceptRolePromotion GroupId`** — M generates `linkRcvKey`, sends `acpt`, marks accepted; the async chain (§6 overview) then runs.
- **`APIRejectRolePromotion GroupId`** — sends `reject`, clears.
- **`APICancelRolePromotion GroupId GroupMemberId`** — O1 sends `cancel`, clears the proposal.
- Route **`APIMembersRole … GROwner`** on a channel into the invite flow — single-target (reject a multi-member owner promotion, mirroring the existing admin-batch guard `Commands.hs:2879`).
- Route owner **removal** (`APIRemoveMembers` of an owner) into the removal flow (§7 overview): async command, `RKEYV`-out + `LSETV` the revocation, reconcile heals a crash.

---

## Part 6 — key logic changes (anchors)

- `groupLinkData` (`Internal.hs:1509`) → build the write from local state via the **reconcile** routine; delete the `GRKPrivate` singleton / `[]` fallback; read the stored chain.
- **Reconcile** = merge published link data into local (per-field: sets merge + keep pending intent, whole-value surfaces conflict). Run on channel open (remove owner-exclusion at `Commands.hs:1918` and the app `else if`), on a CAS conflict, and as missed-event backstop.
- Relay activation off an **accepted write**, not the `LINK` echo (`Subscriber.hs:1411-1458`, `Agent.hs:1813`).
- Roster: tie-break at all three gates (`Subscriber.hs:3302`, `:3430`, `:3461`) using `roster_version_owner_id`; pending-delta auto-retry; existing-member healing via re-emitted `x.grp.mem.role` (verified reaches members).
- Count: `x.grp.relay.count` produced by the relay branch of `updatePublicGroupData` (`Internal.hs:1457`), stored per-relay, `max` across relays; leading owner (lowest `owner_auth_index`) publishes to link data.
- Owner materialisation threads the full `OwnerAuth` (sig + index): `createLinkOwnerMember` (`Groups.hs:3508`), `updateRelayGroupKeys` (`Groups.hs:2320`, also create the row if missing).
- Relay caches the signed key-map and serves it on request (for removal).

---

## Part 7 — apps (iOS + Kotlin, mirror both)

- `GroupInfo`: decode **`canManageLink`** (new backend field); gate link/relay *management* on it, keep relay *status display* on `isOwner`.
- `GroupMember`: decode `memberRoleProposal` + `promotionPending` (optional/nullable).
- **New `RcvGroupEvent` case** for the promotion service item (both enums are exhaustive — a missing case throws) + strings.
- `canChangeRoleTo` (`ChatTypes.swift:3092`, `ChatModel.kt:2668`): add `.owner` for channels; selecting it starts the async invite, shows the proposed-role row (proposed + Cancel / rejected + re-invite).
- Invitee: a **new** support-chat accept/reject banner (the existing pending-member bar is gated on a non-null scope member; M's own scope has none, so this is new code reading the proposal from `membership`) + the service-item unread.
- Conflict surfaces: "profile edit superseded", "your role change to X was overridden" — plain, re-apply via normal UI.
- Fix the chat-list leave guard to honour `hasOtherOwner` (`ChatListNavLink.swift:272`, `ChatListNavLinkView.kt:319`).

---

## Part 8 — version gating

Detect the link queue's negotiated server version; **require ≥ v19 to promote a second owner** (RKEY-CAS + LSET-CAS + the version fields all need it). On an older link server, block promotion with a clear error rather than degrade — a blind `RKEY` there evicts other owners.

---

## Part 9 — tests (scope)

Under `describe "channels"` (`tests/ChatTests/Groups.hs`), ceiling 2 owners + 1 relay + 3 subscribers (profiles run out at `frank`). Priorities: promote O2 verified-then-accepted, and its admin message accepted by a subscriber that only saw O1; owner-2 adds owner-3 (chain order); concurrent link writes by two owners (CAS: both survive); concurrent roster edits, different members (auto-retry → union) and same member (conflict surfaced); count across a newly-added relay (no crash); owner removal (revoke + key drop, joiner sees non-owner); adversarial — replayed `acpt`, chain published out of order (fail closed), promotion on an unverified key. The count/roster races need forced interleaving (`deliveryWorkerDelay`).
