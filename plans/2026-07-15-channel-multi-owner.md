# Channel multi-owner

A channel can have N equal owners, per `docs/protocol/channels-overview.md` §Governance ("v7: any-owner-decides"). Any owner can take any administrative action, including destroying the channel; no coordination between owners. Owner *removal* (handing over ownership, dropping an owner) is in scope — it is the point of the feature: a channel must survive losing any single owner.

Only the creator can do this today. simplexmq already models most of it and leaves it unwired: `UserContactData.owners :: [OwnerAuth]` (a signed chain — each entry signed by root or an earlier owner), `decryptLinkData`/`validateLinkOwners` accept data signed by any listed owner, and `RKEY`/`recipientKeys :: NonEmpty` let a queue have many writers. The single-owner assumption lives almost entirely in `groupLinkData` (`Internal.hs:1509`), which republishes only *self* as the owner.

## 1. The frame: every shared state needs a sequencer

The whole problem is ordering concurrent writes, and the answer is fixed by **where the state physically lives**:

| State | Lives on | Sequencer | How it converges |
|---|---|---|---|
| **Link data** (profile, relay list, owners chain) | one blob on the SMP link queue | the **SMP server** | version-CAS: write-with-expected-version, rejection returns current state, re-merge |
| **Owner queue keys** (`recipientKeys`) | one set on the SMP link queue | the **SMP server** | version-CAS, same shape; + an owner-held key→owner map |
| **Roster** (member/mod/admin roles) | a signed copy on every relay; no trusted central copy | **none** | deterministic tie-break + owner-side auto-retry |
| **Subscriber count** | derived, cosmetic | a **leading owner** (for efficiency, not correctness) | relay-sourced; chain-order handover |

Link data and keys already *have* a sequencer — the server — so they need only optimistic concurrency, no consensus. The roster is the only state with no sequencer, which is exactly why it is the hard part. The count needs a soft leader only to avoid write contention.

## 2. Link data

**What:** the mutable link blob — channel profile, relay list, owners chain, subscriber count. Read by every joiner; written by any owner. The server stores it encrypted and cannot read it.

**Sequencer — a version on the server, server-assigned.** Add `link_data_version` to the queue record. `LSET` carries `expectedVersion` = *the version the writer's blob is merged onto*; the server accepts iff `stored == expectedVersion`, increments, and **returns the new version in the success reply**. A mismatch **rejects and returns `(currentVersion, currentBlob)`**. The owner never picks or assumes a version: it takes the current one from events (below), sends it as `expectedVersion`, and on success echoes the server-confirmed new version into its broadcast. The rejection returns the full state because a rejected owner cannot know whether it missed an event or genuinely raced — it re-merges its one change onto the returned blob and retries.

**The version travels in events, so writers rarely pre-read.** Carry `link_data_version` in the signed events that change link data (`XGrpInfo`, `XGrpRelayNew`, the owner add/remove/revoke events, the count publish), each carrying the version the *server confirmed* for that change. Owners hold current state + version from those events, so a write uses the version it holds and succeeds — the only rejections are genuinely simultaneous writes. **Order: write first (`LSET`-CAS), then broadcast the event with the server-confirmed version** — a rejected-then-retried write would otherwise announce the wrong version. **`LGET` is never needed to write** — a stale writer is simply rejected and gets the current state back.

**One reconcile routine keeps local state in sync; `groupLinkData` always builds from local.** "Reconcile" merges published link data into local state per field: sets (relays, owners, revoked-owners) merge and keep the owner's own pending intent; a whole-value field (profile/prefs) with a genuine concurrent same-field edit surfaces a conflict. Because local state thus stays "published + my pending intents", `groupLinkData` builds the write from local state — no separate delta step. Reconcile runs in three places, all the same code: **on channel open** (owners run `APIGetUpdatedGroupLinkData` like subscribers — the owner-exclusion is removed — to adopt others' changes, connect to newly-added relays, and correct revocations), **on a write rejection** (adopt the returned blob, then rebuild), and as the missed-event backstop. The owners field appends my entry; the revoked-owners field appends (the remover writes it on removal, the leaver on leave, any owner corrects an inconsistency it sees on reconcile); the count comes from relays (§5).

**One more existing defect:** relay activation must key off an *accepted* write, not the `LINK` echo (`Agent.hs:1813` returns the client's own sent data, so today an owner marks a relay active because it asked to — nothing reads back the truth).

## 3. Owner queue keys

**What:** the set of recipient auth public keys allowed to write the link queue = the owners' link-write keys. A promoted owner generates one (`linkRcvKey`) and it is added via `RKEY`.

**Sequencer — the server, version-CAS, same shape as link data.** Give the key set a `key_set_version`; `RKEY` carries the expected version and the full set; success returns the new version; a rejection returns the current set. Two concurrent additions: one wins, the other re-merges its key onto the returned set and retries, so both land. CAS is needed because `RKEY` *replaces* — without it, two concurrent adds each drop the other's key.

**Correlation for removal — a signed map, cached on relays, never on the link server.** To remove owner X you must know *which* key is X's, but the server's set is deliberately **anonymous**: any public or link-server key→owner map lets a joining SMP operator (who reads the OwnerAuth chain and sees which key writes) tie an owner's link-queue activity to their identity. So the `memberId → linkRcvKey` map lives elsewhere: a new owner announces its key (own protocol event, delivered owner-scoped via `getGroupOwners`/a `DJSOwners` scope, carrying the `key_set_version` from its `RKEY` success), and — critically — **relays cache the signed map and serve it on request**. Each entry is owner-signed, so a relay cannot forge one; a relay sees the map but is not the link-queue operator, so it cannot correlate link-queue writes. Because relays are always-on, an owner can always obtain the map without waiting for a peer owner.

**So removal never blocks and self-heals:** it is an **async command** whose steps chain by continuation (fetch the map from a relay if the local copy lacks the target → `RKEY` out the key → `LSET` the revocation), CAS rejections retrying inline — no new durable-worker type. A crash mid-removal is corrected by reconcile-on-open (§2): any owner that sees an `OwnerAuth` whose key is no longer in the set treats it as revoked and completes the marker. A missing *addition* self-heals via CAS (stale `key_set_version` → rejection → returned set repairs it). A relay withholding the map is handled by asking another relay.

## 4. Roster

**What:** the owner-signed set of member/moderator/admin roles (owners are on the link chain, not the roster). It has **two representations**: individual signed `x.grp.mem.role` events sent to members, and a versioned blob sent to relays and served to joiners. These can diverge under concurrency — which is the whole difficulty.

**No sequencer exists** (a copy sits on each relay; relays are untrusted and cannot sign), so convergence is owner-side, in three parts:

1. **Deterministic winner.** Tie-break on `(version, authorMemberId)` — `memberId` is identical on every device, so every node picks the same blob regardless of arrival order. This must be applied at **all three** version gates: the signed-event gate (`Subscriber.hs:3302`), the blob header (`:3430`), and the blob-completion gate (`:3461`, today version-only and storing a device-local id) — the owner↔owner path is the blob, applied at completion. Add `roster_version_owner_id` (the cross-device author) and store it there.

2. **Auto-retry to the union.** An owner keeps its **pending role-deltas** durably. On adopting a newer winning blob that lacks one, it re-applies the delta *if the winner did not also touch that member* — re-broadcasting at a higher version. O1 (X→admin) and O2 (Y→admin) thus converge to *both*, not one. Verified: the re-emitted `x.grp.mem.role` fans out to the full membership (`DJSGroup` → all members, not just relays/joiners), so an already-joined affected member receives it and updates — this is the only channel by which an existing member learns a role change (the blob goes only to relays/joiners).

3. **True conflict.** If both owners changed the *same* member, the tie-break winner stands, that member heals to the winner (both owners emitted its event), and the losing owner surfaces "your change to X was overridden." No silent revert.

**Why no leader:** a leading owner that sequences the roster reintroduces the single-owner dependency multi-owner exists to remove — if it is asleep, all role changes stall. Auto-retry needs no one to be online but the acting owners. Races are rare (concurrent role edits within seconds), so the extra broadcast is acceptable. Bulk changes (one call, one version bump, N events) re-send the bulk on retry.

## 5. Subscriber count

**What:** the "subscribers: N" display — cosmetic, best-effort. Owners have no subscriber connections (subscribers connect to relays), so the count can only come from relays.

**Source — absolute per-relay counts, take the max.** Each relay states its own *current absolute* subscriber count as a distinct per-relay event **batched with the join/leave/removal event it already sends to owners** — so it costs no extra SMP block, and it need only be restated when the count changes, which is exactly on a join/leave/removal (no periodic reporting). The owner stores the latest **per relay** (a field on the relay's member row); the published count is the **max** across relays. Absolute-plus-max makes the hard case correct: a newly added relay that subscribers are still slowly connecting to reports a low number that stays below the max and is ignored until it catches up; a subscriber leaving that relay drops only its entry, not the max; a real departure (from all relays) lowers every entry and so the max. **Deltas fail here** — a leave is forwarded by several relays and *deduplicated*, so an owner cannot attribute a decrease to a relay, and a low new relay's decrease would wrongly crash the total. The count event, being a separate per-relay event, is not deduplicated even though the join/leave it batches with is. (Relays need not talk to each other; the max of independent reports suffices.)

**Sequencer — a leading owner** (the lowest `owner_auth_index` among current owners) publishes the count to link data. Not for correctness (relay-sourcing already fixed the promoted-owner-undercount) but for **efficiency**: the count shares `link_data_version`, so N owners publishing per join would bump it constantly and starve human writes; one publisher is one stream. **Handover follows chain order on a membership change** (tentative): when the leader is removed or leaves, the next owner in `owner_auth_index` order automatically becomes the publisher — no liveness detection. A leader that is merely *offline* is not handed over; the count just goes stale until it returns, which is fine because the count is cosmetic. A new owner seeds from the published value and does not publish until it has its own relay reports.

## 6. Owner promotion (adding an owner)

**Trigger:** `APIMembersRole … GROwner` on a channel starts an async, single-target invite flow instead of a plain role change. The UI warns the promoter to verify the invitee's key first (§8).

**Events** (role-generic, so a future non-owner promotion reuses them), all `requiresSignature`, delivered owner→relay→M in M's support scope:
```
x.grp.promote.inv     { invitationId, memberRole, roleData? }   -- O1 -> M
x.grp.promote.acpt    { invitationId, memberKey, roleData? }    -- M  -> O1
x.grp.promote.reject  { invitationId }                          -- M  -> O1
x.grp.promote.cancel  { invitationId }                          -- O1 -> M
```
`memberRole` is the offered role; `roleData` is an optional role-scoped block (owner: `linkRcvId` in inv, fresh `linkRcvKey` in acpt); `memberKey` is M's existing key, which O1 must check **equals the key it already holds for M** — a consistency check so the acceptance cannot introduce a new key (distinct from the OOB verification in §8, which is advisory). A future role adds a `roleData` variant; the parser must decode an unknown tag to an opaque/ignored value (not error), since `omittedField` only covers an *absent* field.

**Consent:** acceptance is not automatic — M confirms in the UI. A pending record on each side; reject or cancel clears it. On acceptance O1 chains, as an async command resumable from the pending record (no new worker type — the record is the durable state, re-driven on receipt and on startup): `RKEY` (add M's key, §3) → `LSET` (append M's `OwnerAuth`, §2) → `x.grp.mem.role … GROwner` (the **commit point** — M treats itself as owner only on receiving this). O1 also announces M's key to the other owners and sends M the current key-map (§3).

**Transport is not confidential, and that is fine.** Support scope reaches M plus all moderators, so a moderator sees the invitation — but nothing secret is in it (`linkRcvId`/`linkRcvKey` are inert public data), and a moderator that *injects* a fake `x.grp.promote.inv` achieves nothing: completion needs an owner holding a matching pending record, which no one does, so no key or link write ever happens. M additionally verifies the `.inv` is owner-signed and ignores others.

## 7. Owner removal

Drop the leaver's key from `recipientKeys` (`RKEY`, targeting it via the owner-held map §3) and mark the entry **revoked** in link data. **Keep the `OwnerAuth` entry** — the chain is append-only, so removing an entry would break every owner it transitively signed; a revoked entry stays valid as a past signer but is not a current owner (a revoked-id list in the link user-data) and its key is gone, so it can no longer write anything. Enforcement is therefore at the SMP layer (no key = no authority), not in the chain. **Leave** is self-sequenced: an owner removing itself writes the revoked-marker first and drops its own key last; if it crashes, a remaining owner finishes. Removal is not a defence against a malicious owner (any owner can already destroy the channel) — it is administrative hand-off.

## 8. Security

- **Promotion is only as safe as the verified key.** An owner's copy of a subscriber's key is relay-asserted (from unsigned `XGrpMemNew`), so a relay that substituted it could otherwise be promoted. The defence is out-of-band verification, which is **already implemented** for channels (`verifyChannelMemberCode`, `Commands.hs:2021`, hashes both members' keys, sorted, so comparing detects substitution). It is **advisory**: the promoter is warned in the UI to verify before promoting (matching the existing channel model), not hard-blocked in the backend. Residual risk: an owner who ignores the warning and promotes an unverified member could sign a relay-substituted key into the chain, making that relay an owner. The backend still enforces the weaker consistency check (§6) that the accepted key equals the one O1 already holds.
- **Any owner can destroy the channel** (`Server.hs:1249`: any recipient key authorises `DEL`/`LDEL`) and can RKEY the key set down to itself, evicting the others. Both accepted under any-owner-decides; recorded because they exceed any chat-level action.
- **Owner-key map is owner-only** (§3), so an SMP operator cannot tie an owner's link-queue writes to their identity.
- **Creator anonymity** is weaker than the overview claims once owner 2 signs owner 3 with its own key (the chain shows who delegated to whom). Qualify it.

## 9. Data model

- **SMP link queue (simplexmq server):** `link_data_version`, `key_set_version`. These wire changes gate on a new SMP **relay** version (`currentServerSMPRelayVersion`, `VersionSMP` 18 → 19 — *not* `currentSMPClientVersion`, which is the client↔client envelope and gates nothing server-facing). **Multi-owner requires the link queue's server ≥ v19**: below it, blind `LSET` is merely lossy, but blind `RKEY` *evicts* other owners (it replaces the set with no CAS), so promotion must **fail closed** on an old link server, not degrade.
- **Agent store (owner device, both SQLite and Postgres trees):** the CAS version cached next to the link credentials; `linkRootSigKey` persisted (`AgentStore.hs:2514`).
- **Chat DB (owner device):** owners chain (`owner_auth_sig`/`owner_auth_index` on `group_members`, threaded through `createLinkOwnerMember`/`updateRelayGroupKeys`); pending-promotion record (member-row columns); pending role-deltas; `roster_version_owner_id`; per-relay absolute subscriber count (a field on each relay's member row); the `memberId → linkRcvKey` map. A `DJSOwners` owner-only delivery scope for the key announcement.
- **Relay:** a cached signed `memberId → linkRcvKey` map, served on request (for owner removal).
- **Apps (iOS + Kotlin):** `canManageLink` on `GroupInfo`; `MemberRoleProposal` (`MRProposed`/`MRRejected`) + `promotionPending` on `GroupMember`; a new `RcvGroupEvent` case for the promotion service item — all optional/forward-compatible so remote-desktop parsing across versions holds.

## 10. UI/UX

- **Promoter:** keep the role picker, add `.owner`; selecting it sends the invitation and shows a proposed-role row (proposed → invitation-sent + Cancel; rejected → declined + re-invite). Warn if the invitee's key is unverified and route to the verification screen — the consequence is the channel's trust chain, not one conversation.
- **Invitee:** a service chat item (unread) plus an accept/reject banner in M's support chat; accept goes through a confirmation stating any owner can delete the channel. (The banner is new code, not a reuse — the existing pending-member bar is gated on a non-null scope member, and M's own support scope has none.)
- **`canManageLink`** gates link/relay *management* (an owner mid-handover cannot write yet); relay *status display* stays on `isOwner`.
- **Conflict surfaces** (both rare): the losing owner is told "profile edit superseded" (link-data same-field conflict) or "your role change to X was overridden" (roster same-member conflict). No special affordance — the owner re-applies through the normal picker/edit if they still want to.

## 11. Open decisions

- **Count-leader handover** is tentative: chain-order takeover on a membership change (§5). The alternative — staleness-triggered takeover so a merely-offline leader is also replaced — is more available but needs liveness inference; deferred unless cosmetic staleness during a leader's absence proves unacceptable.

Settled this round: multi-owner hard-blocks on a pre-v19 link server (§9, no degraded mode); the roster approach is tie-break + auto-retry with no leader (§4); a same-member conflict is surfaced and re-applied through normal UI (§10).

A detailed change-list (per-file edits, migrations, test cases) follows once this design is locked — deliberately omitted here to keep the design reviewable in one pass.
